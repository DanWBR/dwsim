'    UNIQUAC Property Package 
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
    Public Class UNIQUAC_IPData

        Implements ICloneable

        Public ID1 As Integer = -1
        Public ID2 As Integer = -1
        Public A12 As Double = 0
        Public A21 As Double = 0
        Public comment As String = ""
        <FieldIgnored()> Public B12 As Double = 0
        <FieldIgnored()> Public B21 As Double = 0
        <FieldIgnored()> Public C12 As Double = 0
        <FieldIgnored()> Public C21 As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New UNIQUAC_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .A12 = Me.A12
                .A21 = Me.A21
                .B12 = Me.B12
                .B21 = Me.B21
                .C12 = Me.C12
                .C21 = Me.C21
                .comment = Me.comment
            End With
            Return newclass
        End Function

        Public Function CloneToLIQUAC() As LIQUAC2_IPData

            Dim newclass As New LIQUAC2_IPData
            With newclass
                .ID1 = Me.ID1
                .ID2 = Me.ID2
                .Group1 = .ID1
                .Group2 = .ID2
                .A12 = Me.A12
                .A21 = Me.A21
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class UNIQUAC

        Implements IActivityCoefficientBase

        Private _ip As Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))
            Get
                Return _ip
            End Get
        End Property

        Sub New()

            _ip = New Dictionary(Of String, Dictionary(Of String, UNIQUAC_IPData))

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim uniquacip As UNIQUAC_IPData
            Dim uniquacipc() As UNIQUAC_IPData
            Dim uniquacipc2() As UNIQUAC_IPData
            Dim fh1 As New FileHelperEngine(Of UNIQUAC_IPData)
            uniquacipc = fh1.ReadFile(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "uniquac.dat")
            uniquacipc2 = fh1.ReadFile(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "uniquacip.dat")

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            'load UNIQUAC.DAT database interactions
            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next
            For Each uniquacip In uniquacipc
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next

            'load UNIQUACIP.DAT database interactions
            For Each uniquacip In uniquacipc2
                uniquacip.A12 *= 1.98721
                uniquacip.A21 *= 1.98721
            Next

            For Each uniquacip In uniquacipc2
                If Me.InteractionParameters.ContainsKey(csdb.GetDWSIMName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetDWSIMName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetDWSIMName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next
            For Each uniquacip In uniquacipc2
                If Me.InteractionParameters.ContainsKey(csdb.GetCSName(uniquacip.ID1)) Then
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetCSName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    End If
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(csdb.GetCSName(uniquacip.ID1), New Dictionary(Of String, UNIQUAC_IPData))
                    Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetCSName(uniquacip.ID2), uniquacip.Clone)
                    If Not Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).ContainsKey(csdb.GetDWSIMName(uniquacip.ID2)) Then
                        Me.InteractionParameters(csdb.GetCSName(uniquacip.ID1)).Add(csdb.GetDWSIMName(uniquacip.ID2), uniquacip.Clone)
                    End If
                End If
            Next

            uniquacip = Nothing
            uniquacipc = Nothing
            uniquacipc2 = Nothing
            fh1 = Nothing

        End Sub

        Function GAMMA(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String(), ByVal VQ As Double(), ByVal VR As Double(), ByVal index As Integer)

            Return GAMMA_MR(T, Vx, Vids, VQ, VR)(index)

        End Function

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), ByVal Vids As String(), ByVal VQ As Double(), ByVal VR As Double())

            Dim doparallel As Boolean = My.Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = My.Settings.MaxDegreeOfParallelism, .TaskScheduler = App.AppTaskScheduler}

            Dim n As Integer = UBound(Vx)

            Dim tau_ij(n)(), tau_ji(n)(), a12(n)(), a21(n)(), b12(n)(), b21(n)(), c12(n)(), c21(n)() As Double
            Dim teta(n), fi(n), l(n), S(n), lngc(n), lngr(n), lng(n), g(n), sum1(n), sum2 As Double
            Dim z As Double = 10.0#
            Dim r, q As Double

            Dim i, j As Integer

            For i = 0 To n
                Array.Resize(tau_ij(i), n + 1)
                Array.Resize(tau_ji(i), n + 1)
                Array.Resize(a12(i), n + 1)
                Array.Resize(a21(i), n + 1)
                Array.Resize(b12(i), n + 1)
                Array.Resize(b21(i), n + 1)
                Array.Resize(c12(i), n + 1)
                Array.Resize(c21(i), n + 1)
            Next
            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            a12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).A12
                            a21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).A21
                            b12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).B12
                            b21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).B21
                            c12(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).C12
                            c21(i)(j) = Me.InteractionParameters(Vids(i))(Vids(j)).C21
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    a12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A21
                                    a21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).A12
                                    b12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B21
                                    b21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).B12
                                    c12(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C21
                                    c21(i)(j) = Me.InteractionParameters(Vids(j))(Vids(i)).C12
                                End If
                            End If
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            'i = 0
            'Do
            '    j = 0
            '    Do
            '        tau_ij(i)(j) = Math.Exp(-(a12(i)(j) + b12(i)(j) * T + c12(i)(j) * T ^ 2) / (1.98721 * T))
            '        tau_ji(j)(i) = Math.Exp(-(a21(i)(j) + b21(i)(j) * T + c21(i)(j) * T ^ 2) / (1.98721 * T))
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     tau_ij(ip) = a12(ip).NegateY.AddY(b12(ip).MultiplyConstY(T).AddY(c12(ip).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                                                     tau_ji(ip) = a21(ip).NegateY.AddY(b21(ip).MultiplyConstY(T).AddY(c21(ip).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                                                 End Sub)
            Else
                For i = 0 To n
                    tau_ij(i) = a12(i).NegateY.AddY(b12(i).MultiplyConstY(T).AddY(c12(i).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                    tau_ji(i) = a21(i).NegateY.AddY(b21(i).MultiplyConstY(T).AddY(c21(i).MultiplyConstY(T ^ 2))).MultiplyConstY(1 / (1.98721 * T)).ExpY
                Next
            End If

            'r = 0.0#
            'q = 0.0#
            'i = 0
            'Do
            '    r += Vx(i) * VR(i)
            '    q += Vx(i) * VQ(i)
            '    i = i + 1
            'Loop Until i = n + 1

            r = Vx.MultiplyY(VR).SumY
            q = Vx.MultiplyY(VQ).SumY

            'i = 0
            'Do
            '    fi(i) = Vx(i) * VR(i) / r
            '    teta(i) = Vx(i) * VQ(i) / q
            '    l(i) = z / 2 * (VR(i) - VQ(i)) - (VR(i) - 1)
            '    i = i + 1
            'Loop Until i = n + 1

            fi = Vx.MultiplyY(VR).MultiplyConstY(1 / r)
            teta = Vx.MultiplyY(VQ).MultiplyConstY(1 / q)
            l = VR.SubtractY(VQ).MultiplyConstY(z / 2).SubtractY(VR.AddConstY(-1))

            'i = 0
            'Do
            '    S(i) = 0
            '    j = 0
            '    Do
            '        S(i) += teta(j) * tau_ji(j)(i)
            '        j = j + 1
            '    Loop Until j = n + 1
            '    i = i + 1
            'Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     S(ip) = teta.MultiplyY(tau_ij(ip)).SumY
                                                 End Sub)
            Else
                For i = 0 To n
                    S(i) = teta.MultiplyY(tau_ij(i)).SumY
                Next
            End If


            'i = 0
            'Do
            '    sum1(i) = 0
            '    j = 0
            '    Do
            '        sum1(i) += teta(j) * tau_ij(i)(j) / S(j)
            '        j = j + 1
            '    Loop Until j = n + 1
            '    sum2 += Vx(i) * l(i)
            '    i = i + 1
            'Loop Until i = n + 1

            If doparallel And n > 10 Then
                Parallel.For(0, n + 1, poptions, Sub(ip)
                                                     sum1(ip) = teta.MultiplyY(tau_ij(ip).DivideY(S)).SumY
                                                 End Sub)
            Else
                For i = 0 To n
                    sum1(i) = teta.MultiplyY(tau_ij(i).DivideY(S)).SumY
                Next
            End If

            sum2 = Vx.MultiplyY(l).SumY

            'i = 0
            'Do
            '    If Vx(i) <> 0.0# Then
            '        lngc(i) = 1 - VR(i) / r + Math.Log(VR(i) / r) - z / 2 * VQ(i) * (1 - fi(i) / teta(i) + Math.Log(fi(i) / teta(i)))
            '    Else
            '        lngc(i) = 1 - VR(i) / r
            '    End If
            '    lngr(i) = VQ(i) * (1 - Math.Log(S(i)) - sum1(i))
            '    lng(i) = lngc(i) + lngr(i)
            '    g(i) = Math.Exp(lng(i))
            '    i = i + 1
            'Loop Until i = n + 1

            lngc = VR.MultiplyConstY(-1 / r).AddConstY(1).AddY(VR.MultiplyConstY(1 / r).LogY.AddY(VQ.MultiplyConstY(-z / 2).MultiplyY(fi.DivideY(teta).NegateY.AddConstY(1).AddY(fi.DivideY(teta).LogY))))
            lngr = VQ.MultiplyY(S.LogY.NegateY.SubtractY(sum1).AddConstY(1))
            lng = lngc.AddY(lngr)
            g = lng.ExpY

            Return g

        End Function

        Function DLNGAMMA_DT(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Array

            Dim gamma1, gamma2 As Double()

            Dim epsilon As Double = 0.001

            gamma1 = GAMMA_MR(T, Vx, Vids, VQ, VR)
            gamma2 = GAMMA_MR(T + epsilon, Vx, Vids, VQ, VR)

            Dim dgamma(gamma1.Length - 1) As Double

            For i As Integer = 0 To Vx.Length - 1
                dgamma(i) = (gamma2(i) - gamma1(i)) / (epsilon)
            Next

            Return dgamma

        End Function

        Function HEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Double

            Dim dgamma As Double() = DLNGAMMA_DT(T, Vx, Vids, VQ, VR)

            Dim hex As Double = 0.0#

            For i As Integer = 0 To Vx.Length - 1
                hex += -8.314 * T ^ 2 * Vx(i) * dgamma(i)
            Next

            Return hex 'kJ/kmol

        End Function

        Function CPEX_MIX(ByVal T As Double, ByVal Vx As Array, ByVal Vids As Array, ByVal VQ As Array, ByVal VR As Array) As Double

            Dim hex1, hex2, cpex As Double

            Dim epsilon As Double = 0.001

            hex1 = HEX_MIX(T, Vx, Vids, VQ, VR)
            hex2 = HEX_MIX(T + epsilon, Vx, Vids, VQ, VR)

            cpex = (hex2 - hex1) / epsilon

            Return cpex 'kJ/kmol.K

        End Function

        Public Function CalcActivityCoefficients(T As Double, Vx As Array, otherargs As Object) As Array Implements IActivityCoefficientBase.CalcActivityCoefficients

            Return GAMMA_MR(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessEnthalpy(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessEnthalpy

            Return HEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

        Public Function CalcExcessHeatCapacity(T As Double, Vx As Array, otherargs As Object) As Double Implements IActivityCoefficientBase.CalcExcessHeatCapacity

            Return CPEX_MIX(T, Vx, otherargs(0), otherargs(1), otherargs(2))

        End Function

    End Class

End Namespace
