'    NRTL Property Package 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports System.Collections.Generic
Imports FileHelpers
Imports System.Threading.Tasks

Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()> _
    Public Class NRTL_IPData

        Implements ICloneable

        <FieldConverter(GetType(ObjectConverter))> Public ID1 As Object = ""
        <FieldConverter(GetType(ObjectConverter))> Public ID2 As Object = ""
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public alpha12 As Double = 0
        Public comment As String = ""
        <FieldHidden()> Public B12 As Double = 0
        <FieldHidden()> Public B21 As Double = 0
        <FieldHidden()> Public C12 As Double = 0
        <FieldHidden()> Public C21 As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New NRTL_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .B21 = Me.B21
                .C12 = Me.C12
                .C21 = Me.C21
                .alpha12 = Me.alpha12
                .comment = Me.comment
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class ObjectConverter

        Inherits ConverterBase

        Public Overrides Function StringToField(ByVal from As String) As Object
            Return [from]
        End Function


        Public Overrides Function FieldToString(ByVal fieldValue As Object) As String

            Return fieldValue.ToString

        End Function

    End Class

    <System.Serializable()> Public Class NRTL

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, NRTL_IPData))

        Public Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, NRTL_IPData))
            Get
                Return _ip
            End Get
            Set(value As Dictionary(Of String, Dictionary(Of String, NRTL_IPData)))
                _ip = value
            End Set
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, NRTL_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim nrtlip As NRTL_IPData
            Dim nrtlipc() As NRTL_IPData
            Dim fh1 As New FileHelperEngine(Of NRTL_IPData)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.nrtl.dat")
                Using t As New IO.StreamReader(filestr)
                    nrtlipc = fh1.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            'load ChemSep database interactions
            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(nrtlip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                End If
            Next

            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(nrtlip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetCSName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetCSName(nrtlip.ID2), nrtlip.Clone)
                    If Not Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).ContainsKey(csdb.GetDWSIMName(nrtlip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(nrtlip.ID1)).Add(csdb.GetDWSIMName(nrtlip.ID2), nrtlip.Clone)
                    End If
                End If
            Next

            'load biodiesel database interactions
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.biod_intparm_nrtl.dat")
                Using t As New IO.StreamReader(filestr)
                    nrtlipc = fh1.ReadStream(t)
                End Using
            End Using

            For Each nrtlip In nrtlipc
                If Me.InteractionParameters.ContainsKey(nrtlip.ID1) Then
                    If Me.InteractionParameters((nrtlip.ID1)).ContainsKey((nrtlip.ID2)) Then
                    Else
                        Me.InteractionParameters((nrtlip.ID1)).Add((nrtlip.ID2), nrtlip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add((nrtlip.ID1), New Dictionary(Of String, NRTL_IPData))
                    Me.InteractionParameters((nrtlip.ID1)).Add((nrtlip.ID2), nrtlip.Clone)
                End If
            Next

            nrtlip = Nothing
            nrtlipc = Nothing
            fh1 = Nothing

        End Sub

        Function GAMMA(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal index As Integer) As Double

            Return GAMMA_MR(T, Vx, Vids)(index)

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String()) As Double()

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism, .TaskScheduler = Settings.AppTaskScheduler}

            Dim n As Integer = Vx.Length - 1

            Dim Gij(n)(), tau_ij(n)(), Gji(n)(), tau_ji(n)(), alpha12(n)() As Double
            Dim S(n), C(n) As Double
            Dim Vg(n), lnVg(n) As Double

            Dim i, j As Integer

            For i = 0 To n
                Array.Resize(Gij(i), n + 1)
                Array.Resize(Gji(i), n + 1)
                Array.Resize(tau_ij(i), n + 1)
                Array.Resize(tau_ji(i), n + 1)
                Array.Resize(alpha12(i), n + 1)
            Next

            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            tau_ij(i)(j) = (Me.InteractionParameters(Vids(i))(Vids(j)).A12 + Me.InteractionParameters(Vids(i))(Vids(j)).B12 * T + Me.InteractionParameters(Vids(i))(Vids(j)).C12 * T ^ 2) / (1.98721 * T)
                            tau_ji(i)(j) = (Me.InteractionParameters(Vids(i))(Vids(j)).A21 + Me.InteractionParameters(Vids(i))(Vids(j)).B21 * T + Me.InteractionParameters(Vids(i))(Vids(j)).C21 * T ^ 2) / (1.98721 * T)
                            alpha12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).alpha12
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    tau_ji(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A12 + Me.InteractionParameters(Vids(j))(Vids(i)).B12 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C12 * T ^ 2) / (1.98721 * T)
                                    tau_ij(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A21 + Me.InteractionParameters(Vids(j))(Vids(i)).B21 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C21 * T ^ 2) / (1.98721 * T)
                                    alpha12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                                Else
                                    tau_ij(i)(j) = 0.0#
                                    tau_ji(i)(j) = 0.0#
                                    alpha12(i)(j) = 0.0#
                                End If
                            Else
                                tau_ij(i)(j) = 0.0#
                                tau_ji(i)(j) = 0.0#
                                alpha12(i)(j) = 0.0#
                            End If
                        End If
                    ElseIf Me.InteractionParameters.ContainsKey(Vids(j)) Then
                        If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                            tau_ji(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A12 + Me.InteractionParameters(Vids(j))(Vids(i)).B12 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C12 * T ^ 2) / (1.98721 * T)
                            tau_ij(i)(j) = (Me.InteractionParameters(Vids(j))(Vids(i)).A21 + Me.InteractionParameters(Vids(j))(Vids(i)).B21 * T + Me.InteractionParameters(Vids(j))(Vids(i)).C21 * T ^ 2) / (1.98721 * T)
                            alpha12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).alpha12
                        Else
                            tau_ij(i)(j) = 0.0#
                            tau_ji(i)(j) = 0.0#
                            alpha12(i)(j) = 0.0#
                        End If
                    Else
                        tau_ij(i)(j) = 0.0#
                        tau_ji(i)(j) = 0.0#
                        alpha12(i)(j) = 0.0#
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'i = 0
            'Do
            '    j = 0
            '    Do
            '        Gij(i, j) = Math.Exp(-alpha12(i, j) * tau_ij(i, j))
            '        Gji(i, j) = Math.Exp(-alpha12(i, j) * tau_ji(i, j))
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     Gij(ip) = alpha12(ip).NegateY.MultiplyY(tau_ij(ip)).ExpY
                                                     Gji(ip) = alpha12(ip).NegateY.MultiplyY(tau_ji(ip)).ExpY
                                                 End Sub)
            Else
                For i = 0 To n
                    Gij(i) = alpha12(i).NegateY.MultiplyY(tau_ij(i)).ExpY
                    Gji(i) = alpha12(i).NegateY.MultiplyY(tau_ji(i)).ExpY
                Next
            End If

            'i = 0
            'Do
            '    S(i) = 0
            '    C(i) = 0
            '    j = 0
            '    Do
            '        S(i) += Vx(j) * Gji(i)(j)
            '        C(i) += Vx(j) * Gji(i)(j) * tau_ji(i)(j)
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     S(ip) = Vx.MultiplyY(Gji(ip)).SumY
                                                     C(ip) = Vx.MultiplyY(Gji(ip)).MultiplyY(tau_ji(ip)).SumY
                                                 End Sub)
            Else
                For i = 0 To n
                    S(i) = Vx.MultiplyY(Gji(i)).SumY
                    C(i) = Vx.MultiplyY(Gji(i)).MultiplyY(tau_ji(i)).SumY
                Next
            End If

            'i = 0
            'Do
            '    lnVg(i) = C(i) / S(i)
            '    j = 0
            '    Do
            '        lnVg(i) += Vx(j) * Gij(i)(j) * (tau_ij(i)(j) - C(j) / S(j)) / S(j)
            '       j = j + 1
            '    Loop Until j = n + 1
            '    Vg(i) = Math.Exp(lnVg(i))
            '    i = i + 1
            'Loop Until i = n + 1

            lnVg = C.DivideY(S)

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     lnVg(ip) += Vx.MultiplyY(Gij(ip)).MultiplyY(tau_ij(ip).SubtractY(C.DivideY(S))).DivideY(S).SumY
                                                 End Sub)
            Else
                For i = 0 To n
                    lnVg(i) += Vx.MultiplyY(Gij(i)).MultiplyY(tau_ij(i).SubtractY(C.DivideY(S))).DivideY(S).SumY
                Next
            End If

            Vg = lnVg.ExpY

            Return Vg

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, Vids)
            gamma2 = GAMMA_MR(T + epsilon, Vx, Vids)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, Vids)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, Vids)
            hex2 = HEX_MIX(T + epsilon, Vx, Vids)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs)

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs)

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs)

        End Function

    End Class

End Namespace
