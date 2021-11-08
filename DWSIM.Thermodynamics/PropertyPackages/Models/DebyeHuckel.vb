'    Debye Huckel Property Package 
'    Copyright 2021 Daniel Wagner O. de Medeiros
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
'
'    Reference: https://www.phasediagram.dk/programming-guide-for-the-extended-uniquac-model/

Imports FileHelpers
Imports System.Math

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class DebyeHuckel

        Private _dc As Dictionary(Of String, DielectricConstants)

        Public ReadOnly Property DielectricConstants() As Dictionary(Of String, DielectricConstants)
            Get
                Return _dc
            End Get
        End Property

        Sub New()

            _dc = New Dictionary(Of String, DielectricConstants)

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim exuniquacdc As DielectricConstants
            Dim exuniquacdcc() As DielectricConstants
            Dim fh4 As New FileHelperEngine(Of DielectricConstants)

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.dielectricconstants.txt")
                Using t As New IO.StreamReader(filestr)
                    exuniquacdcc = fh4.ReadStream(t)
                End Using
            End Using

            For Each exuniquacdc In exuniquacdcc
                Me.DielectricConstants.Add(exuniquacdc.Name, exuniquacdc.Clone)
            Next

        End Sub

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Array

            Dim n As Integer = Vx.Length - 1

            Dim tau_ji(n, n), uij0(n, n), uijT(n, n), uii(n, n) As Double
            Dim Vids(n) As String, VQ(n), VR(n), vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A, b, a1(n, n), a2(n, n), Bij(n, n), Bref(n, n), dBdIm(n, n) As Double
            Dim teta(n), fi(n), S(n), lngc(n), lngr(n), lnglr(n), lngsr(n), lng(n), g(n), sum1(n), sigma As Double

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

            'final activity coefficients

            For i = 0 To n
                lng(i) = lnglr(i)
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

