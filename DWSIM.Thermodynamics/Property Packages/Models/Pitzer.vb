'    Pitzer (Aqueous Electrolytes) Property Package 
'    Copyright 2012-2021 Daniel Wagner O. de Medeiros
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

Imports System.Math
Imports FileHelpers

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class Pitzer

        Private _dc As Dictionary(Of String, DielectricConstants)

        Public ReadOnly Property DielectricConstants() As Dictionary(Of String, DielectricConstants)
            Get
                Return _dc
            End Get
        End Property

        Sub New()

            _dc = New Dictionary(Of String, DielectricConstants)

            Dim dcc() As DielectricConstants
            Dim fh4 As New FileHelperEngine(Of DielectricConstants)
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.dielectricconstants.txt")
                Using t As New IO.StreamReader(filestr)
                    dcc = fh4.ReadStream(t)
                End Using
            End Using

            For Each dc In dcc
                Me.DielectricConstants.Add(dc.Name, dc.Clone)
            Next

        End Sub

        Function GAMMA_MR(ByVal T As Double, ByVal Vx As Double(), cprops As List(Of Interfaces.ICompoundConstantProperties)) As Array

            Dim n As Integer = Vx.Length - 1

            Dim a12(n, n), a21(n, n), b12(n, n), c12(n, n) As Double
            Dim Vids(n) As String, vsolv(n), charge(n), molality(n), solvdensity(n), solvvfrac(n), solvmfrac(n) As Double
            Dim Msolv, DCsolv, dsolv, Xsolv, Im, A, b, a1(n, n), a2(n, n), Bij(n, n), Bref(n, n), dBdIm(n, n), sigma As Double

            Dim i, j, k, l As Integer

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
                i += 1
            Loop Until i = n + 1

            'calculate molality considering 1 mol of mixture.

            Dim wtotal As Double = 0

            i = 0
            Do
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
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
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                    sumvfrac = Vx(i) / Xsolv * cprops(i).Chao_Seader_Liquid_Molar_Volume / 1000000.0
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
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
                If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
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

            Dim teta(n), fi(n), S(n), lngc(n), lngr(n), lngmr(n), lnglr(n), lngsr(n), lng(n), g(n), sum1(n) As Double

            'long range term

            For i = 0 To n
                With cprops(i)
                    If .IsIon Then
                        lnglr(i) = -A * .Charge ^ 2 * Im ^ 0.5 / (1 + b * Im ^ 0.5)
                    ElseIf .Name = "Water" Or .Name = "Methanol" Then
                        sigma = 3 / Vx(i) ^ 3 * (1 + Vx(i) - 1 / (1 + Vx(i)) - 2 * Log(1 + Vx(i)))
                        lnglr(i) = 2 / 3 * A * .Molar_Weight / 1000 * Im ^ (3 / 2) * sigma * (b * Im ^ 0.5)
                    Else
                        lnglr(i) = 0.0#
                    End If
                End With
            Next

            'middle range term

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).IsIon And cprops(j).IsIon Then
                        a1(i, j) = -1
                        a2(i, j) = 0.25 / (Abs(cprops(i).Charge - cprops(j).Charge))
                        If Double.IsInfinity(a2(i, j)) Then a2(i, j) = 0.0#
                    ElseIf Not cprops(i).IsIon And cprops(j).IsIon Then
                        a1(i, j) = -1.2
                        a2(i, j) = 0.25
                    ElseIf cprops(i).IsIon And Not cprops(j).IsIon Then
                        a1(i, j) = -1.2
                        a2(i, j) = 0.25
                    ElseIf Not cprops(i).IsIon And Not cprops(j).IsIon Then
                        a1(i, j) = 0
                        a2(i, j) = 0
                    End If
                Next
            Next

            Dim sum1mr(n), sum2mr, sum3mr, sum4mr(n), sum5mr, sum6mr(n), sum7mr, sum8mr(n) As Double

            For i = 0 To n
                For j = 0 To n
                    Bij(i, j) = b12(i, j) + c12(i, j) * Exp(a1(i, j) * Im ^ 0.5 + a2(i, j) * Im)
                    Bref(i, j) = b12(i, j) + c12(i, j)
                    dBdIm(i, j) = (a1(i, j) / (2 * Im ^ 0.5) + a2(i, j)) * c12(i, j) * Exp(a1(i, j) * Im ^ 0.5 + a2(i, j) * Im)
                Next
            Next

            For i = 0 To n
                sum1mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum1mr(i) += molality(j) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum2mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum2mr += solvmfrac(i) * molality(j) * (Bij(i, j) + Im * dBdIm(i, j))
                        End If
                    End If
                Next
            Next

            sum3mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Charge > 0 And cprops(j).Charge < 0 Then
                        sum3mr += molality(i) * molality(j) * (Bij(i, j) + Im * dBdIm(i, j))
                    End If
                Next
            Next

            For i = 0 To n
                sum4mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum4mr(j) += solvmfrac(i) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum5mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Name = "Water" Or cprops(i).Name = "Methanol" Then
                        If cprops(j).IsIon Then
                            sum5mr += solvmfrac(i) * molality(j) * dBdIm(i, j)
                        End If
                    End If
                Next
            Next


            For i = 0 To n
                sum6mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(i).IsIon Then
                        If cprops(j).IsIon Then
                            sum6mr(j) += molality(i) * Bij(i, j)
                        End If
                    End If
                Next
            Next

            sum7mr = 0
            For i = 0 To n
                For j = 0 To n
                    If cprops(i).Charge > 0 And cprops(j).Charge < 0 Then
                        sum7mr += molality(i) * molality(j) * dBdIm(i, j)
                    End If
                Next
            Next

            'reference state normalization

            For i = 0 To n
                sum8mr(i) = 0
            Next

            For i = 0 To n
                For j = 0 To n
                    If cprops(j).IsIon Then
                        sum8mr(j) += solvmfrac(i) * Bref(i, j)
                    End If
                Next
            Next

            For i = 0 To n
                With cprops(i)
                    If .IsIon Then
                        lngmr(i) = 1 / Msolv * sum4mr(i) + .Charge ^ 2 / (2 * Msolv) * sum5mr + sum6mr(i) + .Charge ^ 2 / 2 * sum7mr - 1 / Msolv * sum8mr(i)
                    ElseIf .Name = "Water" Or .Name = "Methanol" Then
                        lngmr(i) = sum1mr(i) - .Molar_Weight / 1000 / Msolv * sum2mr - Msolv * sum3mr
                    Else
                        lngmr(i) = 0.0#
                    End If
                End With
            Next

            'final activity coefficients

            For i = 0 To n
                lng(i) = lngmr(i) + lnglr(i)
            Next

            'calculate activity of non-dissociated salts

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

