'    Grayson_Streed Property Package 
'    Copyright 2009 Daniel Wagner O. de Medeiros
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
Imports filehelpers
Imports DWSIM.MathOps.MathEx.PolySolve

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class GS

        Sub New()

        End Sub

        Public Function CalcLiqActCoeff(ByVal Vx As Object, ByVal VVL() As Double, ByVal VSP() As Double, ByVal T As Double) As Double()

            Dim n As Integer = UBound(VVL)

            Dim i As Integer
            Dim sumVS, sumV, S_ As Double

            Dim R As Double = 8314470.0

            sumV = 0
            sumVS = 0
            For i = 0 To n
                sumV += Vx(i) * VVL(i)
                sumVS += Vx(i) * VVL(i) * VSP(i)
            Next

            S_ = sumVS / sumV

            Dim lnac(n), ac(n) As Double

            For i = 0 To n
                lnac(i) = VVL(i) * (VSP(i) - S_) ^ 2 / (R * T)
                ac(i) = Math.Exp(lnac(i))
            Next

            Return ac

        End Function

        Public Function CalcNu(ByVal P As Double, ByVal T As Double, ByVal VMW() As Double, ByVal VPc() As Double, ByVal VTc() As Double, ByVal Vw() As Double) As Double()

            Dim n As Integer = UBound(VPc)
            Dim i As Integer

            Dim A0, A1, A2, A3, A4, A5, A6, A7, A8, A9 As Double

            Dim v(n), logv(n), logv0(n), logv1(n) As Double
            Dim Tr, Pr As Double

            For i = 0 To n
                If Convert.ToInt32(VMW(i)) = 2 Then
                    'hydrogen
                    A0 = 1.50709
                    A1 = 2.74283
                    A2 = -0.0211
                    A3 = 0.00011
                    A4 = 0
                    A5 = 0.008585
                    A6 = 0
                    A7 = 0
                    A8 = 0
                    A9 = 0
                ElseIf Convert.ToInt32(VMW(i)) = 16 Then
                    'methane
                    A0 = 1.36822
                    A1 = -1.54831
                    A2 = 0
                    A3 = 0.02889
                    A4 = -0.01076
                    A5 = 0.10486
                    A6 = -0.02529
                    A7 = 0
                    A8 = 0
                    A9 = 0
                Else
                    'simple fluid
                    A0 = 2.05135
                    A1 = -2.10889
                    A2 = 0
                    A3 = -0.19396
                    A4 = 0.02282
                    A5 = 0.08852
                    A6 = 0.0
                    A7 = -0.00872
                    A8 = -0.00353
                    A9 = 0.00203
                End If
                Tr = T / VTc(i)
                Pr = P / VPc(i)
                logv0(i) = A0 + A1 / Tr + A2 * Tr + A3 * Tr ^ 2 + A4 * Tr ^ 3 + (A5 + A6 * Tr + A7 * Tr ^ 2) * Pr + (A8 + A9 * Tr) * Pr ^ 2 - Math.Log10(Pr)
                logv1(i) = -4.23893 + 8.65808 * Tr - 1.2206 / Tr - 3.15224 * Tr ^ 3 - 0.025 * (Pr - 0.6)
                logv(i) = logv0(i) + Vw(i) * logv1(i)
                v(i) = 10 ^ logv(i)
            Next

            Return v

        End Function

        Function CalcVapFugCoeff(ByVal T, ByVal P, ByVal Vx, ByVal VTc, ByVal VPc, ByVal Vw) As Double()

            Dim n, R, coeff(3) As Double
            Dim Vant(0, 4) As Double
            Dim criterioOK As Boolean = False
            Dim ZV As Double
            Dim AG, BG, aml, bml As Double
            Dim t1, t2, t3, t4 As Double

            n = Vx.Length - 1

            Dim ai(n), bi(n), tmp(n + 1), a(n, n), b(n, n)
            Dim LN_CF(n), PHI(n) As Double
            Dim Tc(n), Pc(n), alpha(n), m(n), Tr(n) As Double

            R = 8.314

            Dim i, j As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                ai(i) = 0.42748 * R ^ 2 * Tc(i) ^ 2.5 / (Pc(i) * T ^ 0.5)
                bi(i) = 0.08664 * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1


            i = 0
            aml = 0
            Do
                j = 0
                Do
                    aml = aml + Vx(i) * Vx(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            bml = 0
            Do
                bml = bml + Vx(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            AG = aml * P / (R * T) ^ 2
            BG = bml * P / (R * T)

            Dim u, w As Integer
            u = 1
            w = 0

            coeff(0) = (-AG * BG - w * BG ^ 2 - w * BG ^ 3)
            coeff(1) = AG + w * BG ^ 2 - u * BG - u * BG ^ 2
            coeff(2) = -(1 + BG - u * BG)
            coeff(3) = 1

            Dim temp1 = Poly_Roots(coeff)
            Dim tv = 0.0#
            Dim tv2

            Try

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(1, 1)
                    temp1(1, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(0, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(0, 1)
                    temp1(0, 1) = tv2
                End If
                If temp1(1, 0) > temp1(2, 0) Then
                    tv = temp1(2, 0)
                    temp1(2, 0) = temp1(1, 0)
                    temp1(1, 0) = tv
                    tv2 = temp1(2, 1)
                    temp1(2, 1) = temp1(1, 1)
                    temp1(1, 1) = tv2
                End If

                ZV = temp1(2, 0)
                If temp1(2, 1) <> 0 Then
                    ZV = temp1(1, 0)
                    If temp1(1, 1) <> 0 Then
                        ZV = temp1(0, 0)
                    End If
                End If

                ZV = temp1(2, 0)

            Catch
                Dim findZV
                ZV = 1
                Do
                    findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                    ZV -= 0.00001
                Loop Until Math.Abs(findZV) < 0.0001 Or ZV < 0
            End Try

            i = 0
            Do
                t1 = bi(i) * (ZV - 1) / bml
                t2 = -Math.Log(ZV - BG)
                t3 = AG / (BG * (u ^ 2 - 4 * w) ^ 0.5) * (bi(i) / bml - 2 * (ai(i) / aml) ^ 0.5)
                t4 = Math.Log((2 * ZV + BG * (u + (u ^ 2 - 4 * w) ^ 0.5)) / (2 * ZV + BG * (u - (u ^ 2 - 4 * w) ^ 0.5)))
                PHI(i) = Math.Exp(t1 + t2 + t3 * t4)
                i = i + 1
            Loop Until i = n + 1

            Return PHI

        End Function

    End Class


End Namespace


