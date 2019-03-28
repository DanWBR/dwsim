'    Extended UNIQUAC Property Package 
'    Copyright 2013 Daniel Wagner O. de Medeiros
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
Imports System.Math
Imports System.Linq


Namespace PropertyPackages.Auxiliary

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class ExUNIQUAC_IPData

        Implements ICloneable

        Public Group1 As String = ""
        Public Group2 As String = ""
        Public uij0 As Double = 0
        Public uijT As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New ExUNIQUAC_IPData
            With newclass
                .Group1 = Me.Group1
                .Group2 = Me.Group2
                .uij0 = Me.uij0
                .uijT = Me.uijT
            End With
            Return newclass
        End Function

    End Class

    <DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()> _
    Public Class ExUNIQUAC_RiQiData

        Implements ICloneable

        Public Formula As String = ""
        Public R As Double = 0
        Public Q As Double = 0
        Public a As Double = 0
        Public b As Double = 0
        Public c As Double = 0

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New ExUNIQUAC_RiQiData
            With newclass
                .Formula = Me.Formula
                .R = Me.R
                .Q = Me.Q
                .a = Me.a
                .b = Me.b
                .c = Me.c
            End With
            Return newclass
        End Function

    End Class

    <System.Serializable()> Public Class ExUNIQUAC

        Private _ipl As Dictionary(Of String, Dictionary(Of String, ExUNIQUAC_IPData))
        Private _rq As Dictionary(Of String, ExUNIQUAC_RiQiData)
        Private _dc As Dictionary(Of String, DielectricConstants)

        Public ReadOnly Property InteractionParameters() As Dictionary(Of String, Dictionary(Of String, ExUNIQUAC_IPData))
            Get
                Return _ipl
            End Get
        End Property

        Public ReadOnly Property ModelData() As Dictionary(Of String, ExUNIQUAC_RiQiData)
            Get
                Return _rq
            End Get
        End Property

        Public ReadOnly Property DielectricConstants() As Dictionary(Of String, DielectricConstants)
            Get
                Return _dc
            End Get
        End Property

        Sub New()

            _ipl = New Dictionary(Of String, Dictionary(Of String, ExUNIQUAC_IPData))
            _rq = New Dictionary(Of String, ExUNIQUAC_RiQiData)
            _dc = New Dictionary(Of String, DielectricConstants)

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim exuniquac As ExUNIQUAC_IPData
            Dim exuniquacc() As ExUNIQUAC_IPData
            Dim exuniquacrq As ExUNIQUAC_RiQiData
            Dim exuniquacrqc() As ExUNIQUAC_RiQiData
            Dim exuniquacdc As DielectricConstants
            Dim exuniquacdcc() As DielectricConstants
            Dim fh2 As New FileHelperEngine(Of ExUNIQUAC_IPData)
            Dim fh3 As New FileHelperEngine(Of ExUNIQUAC_RiQiData)
            Dim fh4 As New FileHelperEngine(Of DielectricConstants)
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.ExUNIQUAC_uij.txt")
                Using t As New IO.StreamReader(filestr)
                    exuniquacc = fh2.ReadStream(t)
                End Using
            End Using
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.ExUNIQUAC_RiQi.txt")
                Using t As New IO.StreamReader(filestr)
                    exuniquacrqc = fh3.ReadStream(t)
                End Using
            End Using
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.dielectricconstants.txt")
                Using t As New IO.StreamReader(filestr)
                    exuniquacdcc = fh4.ReadStream(t)
                End Using
            End Using

            Dim csdb As New ChemSepHelper.ChemSepIDConverter

            For Each exuniquac In exuniquacc
                If Me.InteractionParameters.ContainsKey(exuniquac.Group1) Then
                    If Not Me.InteractionParameters(exuniquac.Group1).ContainsKey(exuniquac.Group2) Then
                        Me.InteractionParameters(exuniquac.Group1).Add((exuniquac.Group2), exuniquac.Clone)
                    End If
                Else
                    Me.InteractionParameters.Add(exuniquac.Group1, New Dictionary(Of String, ExUNIQUAC_IPData))
                    Me.InteractionParameters(exuniquac.Group1).Add(exuniquac.Group2, exuniquac.Clone)
                End If
            Next

            For Each exuniquacrq In exuniquacrqc
                Me.ModelData.Add(exuniquacrq.Formula, exuniquacrq.Clone)
            Next

            For Each exuniquacdc In exuniquacdcc
                Me.DielectricConstants.Add(exuniquacdc.Name, exuniquacdc.Clone)
            Next

            exuniquac = Nothing
            exuniquacc = Nothing
            exuniquacrq = Nothing
            exuniquacrqc = Nothing
            exuniquacdc = Nothing
            exuniquacdcc = Nothing
            fh2 = Nothing
            fh3 = Nothing
            fh4 = Nothing

        End Sub

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Array

            Dim n As Integer = Vx.Length - 1

            Dim tau_ji(n, n), uij0(n, n), uijT(n, n), uii(n, n) As Double
            Dim Vids(n) As String, VQ(n), VR(n), vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A, b, a1(n, n), a2(n, n), Bij(n, n), Bref(n, n), dBdIm(n, n) As Double
            Dim teta(n), fi(n), S(n), lngc(n), lngr(n), lnglr(n), lngsr(n), lng(n), g(n), sum1(n), sum0, sigma As Double

            Dim i, j, k, l, wi As Integer

            i = 0
            Do
                If cprops(i).IsIon Then
                    Vids(i) = cprops(i).Formula
                    charge(i) = cprops(i).Charge
                    If Vx(i) = 0.0# Then Vx(i) = 0.0000000001
                Else
                    Vids(i) = cprops(i).Name
                    charge(i) = 0.0#
                    solvdensity(i) = cprops(i).Molar_Weight / cprops(i).Chao_Seader_Liquid_Molar_Volume * 1000
                End If
                If Me.ModelData.ContainsKey(cprops(i).Formula) Then
                    VQ(i) = Me.ModelData(cprops(i).Formula).Q
                    VR(i) = Me.ModelData(cprops(i).Formula).R
                Else
                    VQ(i) = cprops(i).UNIQUAC_Q
                    VR(i) = cprops(i).UNIQUAC_R
                End If
                i = i + 1
            Loop Until i = n + 1

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    wi = i
                    wtotal += Vx(i) * cprops(i).Molar_Weight / 1000
                End If
                i += 1
            Loop Until i = n + 1

            Xsolv = 1

            i = 0
            Do
                molality(i) = Vx(i) / wtotal
                'salt-free mole fractions
                If cprops(i).Name <> "Water" And cprops(i).Name <> "Methanol" Then
                    Xsolv -= Vx(i)
                End If
                i += 1
            Loop Until i = n + 1

            Dim sumvfrac As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    sumvfrac = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).Name = "Water" Then
                    solvmfrac(i) = Vx(i) / Xsolv
                    solvvfrac(i) = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0 / sumvfrac
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Msolv = 0.0#
            DCsolv = 0.0#
            dsolv = 0.0#
            Do
                If cprops(i).Name = "Water" Then
                    Msolv += Vx(i) / Xsolv * cprops(i).Molar_Weight / 1000
                    With Me.DielectricConstants(cprops(i).Name)
                        DCsolv += solvvfrac(i) * (.a + .b * T + .c * T ^ 2 + .d * T ^ 3 + .e * T ^ 4)
                    End With
                    dsolv += solvvfrac(i) * solvdensity(i)
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Im = 0.0#
            Do
                Im += cprops(i).Charge ^ 2 * molality(i) / 2
                i += 1
            Loop Until i = n + 1

            A = 132775.7 * dsolv ^ 0.5 / (DCsolv * T) ^ (1.5)
            b = 6.359696 * dsolv ^ 0.5 / (DCsolv * T) ^ 0.5

            'long range term (Debye-Huckel)

            For i = 0 To n
                With cprops(i)
                    If .IsIon Then
                        lnglr(i) = -A * .Charge ^ 2 * Im ^ 0.5 / (1 + b * Im ^ 0.5)
                    ElseIf .Name = "Water" Then
                        sigma = 3 / Vx(i) ^ 3 * (1 + Vx(i) - 1 / (1 + Vx(i)) - 2 * Log(1 + Vx(i)))
                        lnglr(i) = 2 / 3 * A * .Molar_Weight / 1000 * Im ^ (3 / 2) * sigma * (b * Im ^ 0.5)
                    Else
                        lnglr(i) = 0.0#
                    End If
                End With
            Next

            'short range term

            i = 0
            Do
                j = 0
                Do
                    If Me.InteractionParameters.ContainsKey(Vids(i)) Then
                        If Me.InteractionParameters(Vids(i)).ContainsKey(Vids(j)) Then
                            uij0(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).uij0
                            uijT(i, j) = Me.InteractionParameters(Vids(i))(Vids(j)).uijT
                        Else
                            If Me.InteractionParameters.ContainsKey(Vids(j)) Then
                                If Me.InteractionParameters(Vids(j)).ContainsKey(Vids(i)) Then
                                    uij0(j, i) = Me.InteractionParameters(Vids(j))(Vids(i)).uij0
                                    uijT(j, i) = Me.InteractionParameters(Vids(j))(Vids(i)).uijT
                                End If
                            End If
                        End If
                    End If
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim t1, t2 As Double

            i = 0
            Do
                j = 0
                Do
                    t1 = uij0(j, i) + uijT(j, i) * (T - 298.15)
                    t2 = uij0(i, i) + uijT(i, i) * (T - 298.15)
                    tau_ji(j, i) = Math.Exp(-(t1 - t2) / T)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            Dim R, Q As Double

            i = 0
            Do
                R += Vx(i) * VR(i)
                Q += Vx(i) * VQ(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                fi(i) = Vx(i) * VR(i) / R
                teta(i) = Vx(i) * VQ(i) / Q
                i = i + 1
            Loop Until i = n + 1

            For i = 0 To n
                S(i) = 0
                sum1(i) = 0
            Next

            For i = 0 To n
                For k = 0 To n
                    S(i) += teta(k) * tau_ji(k, i)
                Next
            Next

            For i = 0 To n
                sum1(i) = 0
                For k = 0 To n
                    sum0 = 0
                    For l = 0 To n
                        sum0 += teta(l) * tau_ji(l, k)
                    Next
                    sum1(i) += teta(k) * tau_ji(i, k) / sum0
                Next
            Next

            i = 0
            Do
                lngc(i) = Log(fi(i) / Vx(i)) + 1 - fi(i) / Vx(i) - 5 * VQ(i) * (Log(fi(i) / teta(i)) + 1 - fi(i) / teta(i))
                lngr(i) = VQ(i) * (1 - Log(S(i)) - sum1(i))
                lngsr(i) = lngc(i) + lngr(i)
                If cprops(i).IsIon Then
                    'reference state normalization
                    lngsr(i) -= Log(VR(i) / VR(wi)) + 1 - VR(i) / VR(wi) - 5 * VQ(i) * (Log(VR(i) * VQ(wi) / (VR(wi) * VQ(i))) + 1 - VR(i) * VQ(wi) / (VR(wi) * VQ(i)))
                    lngsr(i) -= VQ(i) * (1 - Log(tau_ji(wi, i)) - tau_ji(i, wi))
                    lngsr(i) += Log(Vx(wi))
                End If
                i = i + 1
            Loop Until i = n + 1

            'final activity coefficients

            For i = 0 To n
                lng(i) = lngsr(i) + lnglr(i)
            Next

            'calculate mean molal activity coefficient

            Dim plng, nlng As Double

            For i = 0 To n
                With cprops(i)
                    If .IsSalt Or .IsHydratedSalt Then
                        For j = 0 To n
                            If .PositiveIon = Vids(j) Then
                                plng = lng(j)
                            End If
                            If .NegativeIon = Vids(j) Then
                                nlng = lng(j)
                            End If
                        Next
                        lng(i) = .PositiveIonStoichCoeff * plng / .StoichSum + .NegativeIonStoichCoeff * nlng / .StoichSum
                    End If
                End With
            Next

            For i = 0 To n
                g(i) = Math.Exp(lng(i))
            Next

            Return g

        End Function

    End Class

End Namespace

