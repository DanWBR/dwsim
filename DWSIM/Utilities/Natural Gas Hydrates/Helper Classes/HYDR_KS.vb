'    Hydrate Calculation Routines (Klauda & Sandler)
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports DWSIM.MathOps.MathEx.PolySolve

Namespace DWSIM.Utilities.HYD

    Public Class KlaudaSandler

        Dim am As DWSIM.Utilities.HYD.AuxMethods
        Dim unf As PropertyPackages.Auxiliary.UNIQUAC
        Dim unfPP As PropertyPackages.UNIQUACPropertyPackage

        Sub New(ByVal materialStream As Streams.MaterialStream)

            am = New DWSIM.Utilities.HYD.AuxMethods
            unf = New PropertyPackages.Auxiliary.UNIQUAC
            unfPP = New PropertyPackages.UNIQUACPropertyPackage

            unfPP.CurrentMaterialStream = materialStream

        End Sub

        Function Cml_vdWP(ByVal estrutura As String, ByVal cela As Integer, ByVal id As Integer, ByVal T As Double)

            Dim k As Double = 1.38E-23

            Dim i, j As Integer
            Dim Rcell1, r, a, sigma, epsilon, z1 As Double
            Dim d10_1, d11_1, d4_1, d5_1 As Double
            Dim W(100), W1(100) As Double
            Dim Int1 As Double
            Dim delta1 As Double

            If id = 1 Then i = 17
            If id = 2 Then i = 18
            If id = 3 Then i = 19
            If id = 4 Then i = 20
            If id = 14 Then i = 21
            If id = 16 Then i = 22
            If id = 15 Then i = 23
            If id = 38 Then i = 24
            epsilon = Convert.ToDouble(am.MAT_VDWP_PP(i, 1)) * k
            sigma = Convert.ToDouble(am.MAT_VDWP_PP(i, 2)) * 0.0000000001
            a = Convert.ToDouble(am.MAT_VDWP_PP(i, 3)) * 0.0000000001

            If estrutura = "sI" And cela = 1 Then
                Rcell1 = 7.95 / 2 * 0.0000000001
                z1 = 20
            End If
            If estrutura = "sI" And cela = 2 Then
                Rcell1 = 8.6 / 2 * 0.0000000001
                z1 = 24
            End If
            If estrutura = "sII" And cela = 1 Then
                Rcell1 = 7.82 / 2 * 0.0000000001
                z1 = 20
            End If
            If estrutura = "sII" And cela = 2 Then
                Rcell1 = 9.46 / 2 * 0.0000000001
                z1 = 28
            End If

            delta1 = (Rcell1 - a) / 101

            Dim t1, t2, t3, t4
            j = 0
            r = 1.0E-30 * Rcell1
            Do
                d10_1 = ((1 - (r + a) / Rcell1) ^ -10 - (1 + (r - a) / Rcell1) ^ -10) / 10
                d11_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -11 - (1 + r / Rcell1 - a / Rcell1) ^ -11) / 11
                d4_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -4 - (1 + r / Rcell1 - a / Rcell1) ^ -4) / 4
                d5_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -5 - (1 + r / Rcell1 - a / Rcell1) ^ -5) / 5
                t1 = sigma ^ 12 / (Rcell1 ^ 11 * r)
                t2 = (d10_1 + a / Rcell1 * d11_1)
                t3 = sigma ^ 6 / (Rcell1 ^ 5 * r)
                t4 = (d4_1 + a / Rcell1 * d5_1)
                W1(j) = 2 * z1 * epsilon * (t1 * t2 - t3 * t4)
                W1(j) = Math.Exp(-W1(j) / (k * T)) * r ^ 2
                r = r + delta1
                j = j + 1
            Loop Until j = 101

            Int1 = W1(0)
            i = 1
            Do
                Int1 = Int1 + 2 * W1(i)
                i = i + 1
            Loop Until i = 100
            Int1 = Int1 + W1(100)
            Int1 = Int1 * delta1 / 2

            Cml_vdWP = 4 * Math.PI / (k * T) * Int1

        End Function

        Function Cml_KS(ByVal estrutura As String, ByVal cela As Integer, ByVal id As Integer, ByVal T As Double)

            Dim k As Decimal = 0.0000000000000000000000138D

            Dim i, j As Integer
            Dim Rcell, Rcell1, Rcell2, Rcell3, r, a, sigma, epsilon, z, z1, z2, z3 As Double
            Dim d10_1, d11_1, d4_1, d5_1 As Double
            Dim d10_2, d11_2, d4_2, d5_2 As Double
            Dim d10_3, d11_3, d4_3, d5_3 As Double
            Dim W(100), W1(100), W2(100), W3(100) As Double
            Dim Int1 As Double
            Dim delta1, delta2, delta3 As Double

            If id = 1 Then i = 17
            If id = 2 Then i = 18
            If id = 3 Then i = 19
            If id = 4 Then i = 20
            If id = 14 Then i = 21
            If id = 16 Then i = 22
            If id = 15 Then i = 23
            If id = 38 Then i = 24
            epsilon = (Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 1)) * Convert.ToDouble(am.MAT_KLAUDASANDLER(25, 1))) ^ 0.5 '* k
            sigma = (Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 2)) + Convert.ToDouble(am.MAT_KLAUDASANDLER(25, 2))) / 2 '* 0.0000000001
            a = (Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 3)) + Convert.ToDouble(am.MAT_KLAUDASANDLER(25, 3))) / 2 '* 0.0000000001

            If estrutura = "sI" And cela = 1 Then i = 10
            If estrutura = "sI" And cela = 2 Then i = 11
            If estrutura = "sII" And cela = 1 Then i = 12
            If estrutura = "sII" And cela = 2 Then i = 13
            Rcell1 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 2)) '* 0.0000000001
            Rcell2 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 3)) '* 0.0000000001
            Rcell3 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 4)) '* 0.0000000001
            z1 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 5))
            z2 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 6))
            z3 = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 7))

            delta1 = (Rcell1 - a) / 101
            delta2 = (Rcell2 - a) / 101
            delta3 = (Rcell3 - a) / 101

            Dim t1, t2, t3, t4
            j = 0
            r = 0.0001 * Rcell1
            Do
                d10_1 = ((1 - (r + a) / Rcell1) ^ -10 - (1 + (r - a) / Rcell1) ^ -10) / 10
                d11_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -11 - (1 + r / Rcell1 - a / Rcell1) ^ -11) / 11
                d4_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -4 - (1 + r / Rcell1 - a / Rcell1) ^ -4) / 4
                d5_1 = ((1 - r / Rcell1 - a / Rcell1) ^ -5 - (1 + r / Rcell1 - a / Rcell1) ^ -5) / 5
                t1 = sigma ^ 12 / (Rcell1 ^ 11 * r)
                t2 = (d10_1 + a / Rcell1 * d11_1)
                t3 = sigma ^ 6 / (Rcell1 ^ 5 * r)
                t4 = (d4_1 + a / Rcell1 * d5_1)
                W1(j) = 2 * z1 * epsilon * (t1 * t2 - t3 * t4)
                r = r + delta1
                j = j + 1
            Loop Until j = 101

            j = 0
            r = 0.0001 * Rcell2
            Do
                d10_2 = ((1 - r / Rcell2 - a / Rcell2) ^ -10 - (1 + r / Rcell2 - a / Rcell2) ^ -10) / 10
                d11_2 = ((1 - r / Rcell2 - a / Rcell2) ^ -11 - (1 + r / Rcell2 - a / Rcell2) ^ -11) / 11
                d4_2 = ((1 - r / Rcell2 - a / Rcell2) ^ -4 - (1 + r / Rcell2 - a / Rcell2) ^ -4) / 4
                d5_2 = ((1 - r / Rcell2 - a / Rcell2) ^ -5 - (1 + r / Rcell2 - a / Rcell2) ^ -5) / 5
                W2(j) = 2 * z2 * epsilon * (sigma ^ 12 / (Rcell2 ^ 11 * r) * (d10_2 + a / Rcell2 * d11_2) - sigma ^ 6 / (Rcell2 ^ 5 * r) * (d4_2 + a / Rcell2 * d5_2))
                r = r + delta2
                j = j + 1
            Loop Until j = 101

            j = 0
            r = 0.0001 * Rcell3
            Do
                d10_3 = ((1 - r / Rcell3 - a / Rcell3) ^ -10 - (1 + r / Rcell3 - a / Rcell3) ^ -10) / 10
                d11_3 = ((1 - r / Rcell3 - a / Rcell3) ^ -11 - (1 + r / Rcell3 - a / Rcell3) ^ -11) / 11
                d4_3 = ((1 - r / Rcell3 - a / Rcell3) ^ -4 - (1 + r / Rcell3 - a / Rcell3) ^ -4) / 4
                d5_3 = ((1 - r / Rcell3 - a / Rcell3) ^ -5 - (1 + r / Rcell3 - a / Rcell3) ^ -5) / 5
                W3(j) = 2 * z3 * epsilon * (sigma ^ 12 / (Rcell3 ^ 11 * r) * (d10_3 + a / Rcell3 * d11_3) - sigma ^ 6 / (Rcell3 ^ 5 * r) * (d4_3 + a / Rcell3 * d5_3))
                r = r + delta3
                j = j + 1
            Loop Until j = 101

            If estrutura = "sI" And cela = 1 Then
                Rcell = 3.906 '* 0.0000000001
                z = 20
            End If
            If estrutura = "sI" And cela = 2 Then
                Rcell = 4.326 '* 0.0000000001
                z = 24
            End If
            If estrutura = "sII" And cela = 1 Then
                Rcell = 3.902 '* 0.0000000001
                z = 20
            End If
            If estrutura = "sII" And cela = 2 Then
                Rcell = 4.682 '* 0.0000000001
                z = 28
            End If

            j = 0
            r = 0.0001 * Rcell
            Do
                W(j) = Math.Exp(-(W1(j) + W2(j) + W3(j)) / (T)) * Math.Pow(r * 0.0000000001, 2)
                r = r + delta1
                j = j + 1
            Loop Until j = 101

            Int1 = W(0)
            i = 1
            Do
                Int1 = Int1 + W(i)
                i = i + 1
            Loop Until i = 100
            Int1 = Int1 + W(100)
            Int1 = Int1 * delta1 * 0.0000000001

            Return 4 * Math.PI / (k * T) * Int1

        End Function

        Function GET_HS_KS(ByVal id)

            Dim i, tmp(3)
            i = 0

            If id = 1 Then i = 29
            If id = 2 Then i = 30
            If id = 3 Then i = 31
            If id = 4 Then i = 32
            If id = 14 Then i = 33
            If id = 16 Then i = 34
            If id = 15 Then i = 35
            If id = 38 Then i = 36
            Try
                tmp(0) = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 1))
                tmp(1) = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 2))
                tmp(2) = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 3))
                tmp(3) = Convert.ToDouble(am.MAT_KLAUDASANDLER(i, 4))
            Catch
                tmp(0) = -1.0E+32
                tmp(1) = -1.0E+32
                tmp(2) = -1.0E+32
                tmp(3) = -1.0E+32
            End Try
            GET_HS_KS = tmp

        End Function

        Function OBJ_FUNC_HYD_KS(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim FGsI, FGsII, FGHYD, FGAG As Double
            Dim vm(1, 1), sumvmsI(1), sumvmsII(1)
            Dim AsI, BsI, CsI, DsI
            Dim AsII, BsII, CsII, DsII
            Dim VbsI, VbsII, Nbw
            Dim C1(1, n), C2(1, n)
            Dim DT, Tnfp, DHm, Td
            Dim Vxaq(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV
            Dim act As Double
            Dim H(n), tmp2(3)
            Dim VMG, FG, FL
            Dim TETA1(1, n), TETA2(1, n)
            Dim vi_(n), VLW
            Dim VysI, VysII, VsI(n), VsII(n)
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n)
            Dim Vy(n), pos, sum
            Dim sum2sI, sum2sII
            Dim R = 8.314
            Dim temp1, tv, tv2, tm1
            Dim bg As Double
            Dim i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = Me.unfPP.RET_VTC()(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = Me.unfPP.RET_VPC()(i)
                    W(i) = Me.unfPP.RET_VW()(i)
                    Vp(i) = 0
                End If
                i = i + 1
            Loop Until i = n + 1

            'CALCULAR EQUILIBRIO EM BASE LIVRE DE aGUA

            pos = 0
            i = 0
            Do
                If Vids(i) = 13 Then pos = i
                i = i + 1
            Loop Until i = n + 1
            sum = 0
            i = 0
            Do
                If i <> pos Then sum += Vz(i)
                i = i + 1
            Loop Until (i = n + 1)
            If vaporonly Then
                i = 0
                Do
                    Vy(i) = Vz(i)
                    i = i + 1
                Loop Until i = n + 1
            Else
                i = 0
                Do
                    If i <> pos Then Vy(i) = Vz(i) / sum
                    If i = pos Then Vy(i) = 0
                    i = i + 1
                Loop Until i = n + 1
            End If

            'PRSV

            Dim PHIV(n), LN_CFV(n)
            Dim ai(n), bi(n), tmp(3, n + 1), a(n, n), b(n, n)
            Dim aml2(n), amv2(n)
            Dim alpha(n), m(n)
            Dim b1(n), b2(n), coeff(3) As Double
            Dim j As Integer

            Dim ai_(n)
            Dim KAPPA(n), KAPPA0(n)

            i = 0
            Do
                If Vz(i) <> 0 Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    ai_(i) = ai(i) ^ 0.5
                    bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                End If
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

            'CALCULO DOS VOLUMES PARCIAIS MOLARES a DILUIcaO INFINITA

            VLW = Math.Exp(-10.9241 + 0.00025 * (T - 273.15) - 0.0003532 * (P / 1000000.0 - 0.101325) + 0.0000001559 * (P / 1000000.0 - 0.101325) ^ 2)
            i = 0
            Do
                vi_(i) = (R * T / (VLW - bi(pos)) * (1 + bi(i) / (VLW - bi(pos))) - (2 * ai(pos) - 2 * ai(pos) * bi(i) * (VLW - bi(pos)) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos)))) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos)))) / (R * T / (VLW - bi(pos)) ^ 2 - 2 * ai(pos) * (VLW + bi(pos)) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos))) ^ 2)
                i = i + 1
            Loop Until i = n + 1

            'CALCULO DOS FATORES DE COMPRESSIBILIDADE a DILUIcaO INFINITA

            i = 0
            Do
                ZLinf(i) = P * vi_(i) / (R * T)
                i = i + 1
            Loop Until i = n + 1

            'CALCULO DA CONSTANTE DE HENRY

            i = 0
            Do
                If i <> pos Then
                    tmp2 = GET_HS_KS(Vids(i))
                    H(i) = 101325 * Math.Exp(-tmp2(0) - tmp2(1) / T - tmp2(2) * Math.Log(T) - tmp2(3) * T)
                End If
                i = i + 1
            Loop Until i = n + 1

            ' CALCULO DAS RAIZES PARA A Phase VAPOR

            i = 0
            Do
                amv2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim amv = 0.0#
            Do
                j = 0
                Do
                    amv = amv + Vy(i) * Vy(j) * a(i, j)
                    amv2(i) = amv2(i) + Vy(j) * a(j, i)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bmv = 0.0#
            Do
                bmv = bmv + Vy(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            Dim AG = amv * P / (R * T) ^ 2
            bg = bmv * P / (R * T)

            coeff(0) = -AG * bg + bg ^ 2 + bg ^ 3
            coeff(1) = AG - 3 * bg ^ 2 - 2 * bg
            coeff(2) = bg - 1
            coeff(3) = 1

            temp1 = Poly_Roots(coeff)
            tv = 0

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

            Catch

                Dim findZV
                ZV = 1
                Do
                    findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                    ZV -= 0.00001
                    If ZV < 0 Then ZV = 1
                Loop Until Math.Abs(findZV) < 0.0001

            End Try

            ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase VAPOR

            i = 0
            Do
                t1 = bi(i) * (ZV - 1) / bmv
                t2 = -Math.Log(ZV - bg)
                t3 = amv * (2 * amv2(i) / amv - bi(i) / bmv)
                t4 = Math.Log(ZV + (1 + 2 ^ 0.5) * bg) - Math.Log(ZV + ((1 - 2 ^ 0.5) * bg))
                t5 = 8 ^ 0.5 * bmv * R * T
                LN_CFV(i) = t1 + t2 - (t3 * t4 / t5)
                PHIV(i) = Math.Exp(LN_CFV(i)) * Vy(i) * P
                i = i + 1
            Loop Until i = n + 1

            If vaporonly Then

                Td = 273.15

                FGAG = PHIV(pos)

            Else

                'CALCULO DAS FRAcoES MOLARES DOS COMPONENTES NA Phase AQUOSA

                i = 0
                Do
                    If i <> pos Then Vxaq(i) = PHIV(i) / (H(i) * Math.Exp(ZLinf(i)))
                    If H(i) = 101325.0# Then Vxaq(i) = 0.0#
                    i = i + 1
                Loop Until i = n + 1

                Dim sum_vxaq = 0.0#
                i = 0
                Do
                    If i <> pos Then sum_vxaq += Vxaq(i)
                    i = i + 1
                Loop Until i = n + 1
                Vxaq(pos) = 1 - sum_vxaq

                Dim WAC As Double = unf.GAMMA(T, Vxaq, unfPP.RET_VIDS, unfPP.RET_VQ, unfPP.RET_VR, pos)

                'CALCULO DA DEPRESSaO NO PONTO DE FUSaO DA aGUA
                Tnfp = 273.15
                DHm = 6001700.0 / 1000
                DT = R * Tnfp ^ 2 / DHm * Math.Log(Vxaq(pos) * WAC)
                Td = DT + Tnfp

                'CALCULO DO VOLUME MOLAR DO GELO
                VMG = 0.00001912 + 0.0000000008387 * T + 0.000000000004016 * T ^ 2

                'CALCULO DA FUGACIDADE DA aGUA NO GELO
                FG = Math.Exp(4.6056 * Math.Log(T) - 5501.1243 / T + 2.9446 - 0.0081431 * T) * Math.Exp((VMG / (R * T) * (P - Math.Exp(4.6056 * Math.Log(T) - 5501.1243 / T + 2.9446 - 0.0081431 * T))))

                'CALCULO DA FUGACIDADE DA aGUA NA Phase LiQUIDA
                act = WAC
                FL = Vxaq(pos) * act * Math.Exp(4.1539 * Math.Log(T) - 5500.9332 / T + 7.6537 - 0.0161277 * T) * Math.Exp((VLW / (R * T) * (P - Math.Exp(4.1539 * Math.Log(T) - 5500.9332 / T + 2.9446 - 0.0161266 * T))))

                If T < Td Then FGAG = FG
                If T > Td Then FGAG = FL

            End If

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C1(0, i) = Cml_KS("sI", 1, Vids(i), T)
                            C1(1, i) = Cml_KS("sI", 2, Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Dim soma_CML11 = 0.0#, soma_CML21 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML11 += C1(0, i) * PHIV(i)
                        soma_CML21 += C1(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA1(0, i) = C1(0, i) * PHIV(i) / (1 + soma_CML11)
                        TETA1(1, i) = C1(1, i) * PHIV(i) / (1 + soma_CML21)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sI"
                Nbw = 46
                VbsI = (11.835 + 0.00002217 * T + 0.000002242 * T ^ 2) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                'CALCULAR CONSTANTES DA PRESSaO DE VAPOR DO LaTICE VAZIO

                'CALCULAR SOMAS dos VMs (sI)
                vm(0, 0) = 1 / 23
                vm(0, 1) = 3 / 23
                sumvmsI(0) = 0
                sumvmsI(1) = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then sumvmsI(0) += TETA1(0, i)
                        If i <> pos Then sumvmsI(1) += TETA1(1, i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                sum2sI = vm(0, 0) * Math.Log(1 - sumvmsI(0)) + vm(0, 1) * Math.Log(1 - sumvmsI(1))

                'calcular fracao total dos formadores de sI
                VysI = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysI += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                Dim schsI = 0.0#
                i = 0
                Do
                    If i <> pos Then schsI += (vm(0, 0) * TETA1(0, i) + vm(0, 1) * TETA1(1, i))
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    VsI(i) = (vm(0, 0) * TETA1(0, i) + vm(0, 1) * TETA1(1, i)) / schsI
                    i = i + 1
                Loop Until i = n + 1

                'para hidrato sI
                j = 0
                i = 0
                AsI = 0
                BsI = 0
                CsI = 0
                DsI = 0
                Do
                    If Vids(i) = 1 Then j = 45
                    If Vids(i) = 2 Then j = 46
                    If Vids(i) = 38 Then j = 47
                    If Vids(i) = 15 Then j = 48
                    If Vids(i) = 14 Then j = 49
                    If Vids(i) = 16 Then j = 50
                    If Vids(i) = 3 Then j = 51
                    If Vids(i) = 4 Then j = 52
                    If Vids(i) = 38 Then j = 53
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        AsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 2))
                        BsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 3))
                        CsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 4))
                        DsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 5)) * 0.001
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR FUGACIDADE DA aGUA NO HIDRATO sI
                tm1 = Math.Exp(sum2sI)
                FGsI = Math.Exp(AsI * Math.Log(T) + BsI / T + 2.7789 + DsI * T) * Math.Exp(VbsI / (R * T) * (P - Math.Exp(AsI * Math.Log(T) + BsI / T + 2.7789 + DsI * T))) * tm1
                FGHYD = FGsI

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C2(0, i) = Cml_KS("sII", 1, Vids(i), T)
                            C2(1, i) = Cml_KS("sII", 2, Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1
                Dim soma_CML12 = 0.0#, soma_CML22 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML12 += C2(0, i) * PHIV(i)
                        soma_CML22 += C2(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA2(0, i) = C2(0, i) * PHIV(i) / (1 + soma_CML12)
                        TETA2(1, i) = C2(1, i) * PHIV(i) / (1 + soma_CML22)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sII"
                Nbw = 136
                VbsII = (17.13 + 0.0002249 * T + 0.000002013 * T ^ 2 + 0.000000001009 * T ^ 3) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                'CALCULAR SOMAS dos VMs (sII)
                vm(1, 0) = 2 / 17
                vm(1, 1) = 1 / 17
                sumvmsII(0) = 0
                sumvmsII(1) = 0
                i = 0
                Do
                    If i <> pos Then sumvmsII(0) += TETA2(0, i)
                    If i <> pos Then sumvmsII(1) += TETA2(1, i)
                    i = i + 1
                Loop Until i = n + 1
                sum2sII = vm(1, 0) * Math.Log(1 - sumvmsII(0)) + vm(1, 1) * Math.Log(1 - sumvmsII(1))

                'CALCULAR CONSTANTES DA PRESSaO DE VAPOR DO LaTICE VAZIO

                'calcular fracao total dos formadores de sII
                VysII = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysII += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                Dim schsII = 0.0#
                i = 0
                Do
                    If i <> pos Then schsII += (vm(1, 0) * TETA2(0, i) + vm(1, 1) * TETA2(1, i))
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    VsII(i) = (vm(1, 0) * TETA2(0, i) + vm(1, 1) * TETA2(1, i)) / schsII
                    i = i + 1
                Loop Until i = n + 1

                'para hidrato sII
                j = 0
                i = 0
                AsII = 0
                BsII = 0
                CsII = 0
                DsII = 0
                Do
                    If Vids(i) = 1 Then j = 45
                    If Vids(i) = 2 Then j = 46
                    If Vids(i) = 38 Then j = 47
                    If Vids(i) = 15 Then j = 48
                    If Vids(i) = 14 Then j = 49
                    If Vids(i) = 16 Then j = 50
                    If Vids(i) = 3 Then j = 51
                    If Vids(i) = 4 Then j = 52
                    If Vids(i) = 38 Then j = 53
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        AsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 2))
                        BsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 3))
                        CsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 4))
                        DsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 5)) * 0.001
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR FUGACIDADE DA aGUA NO HIDRATO sII
                FGsII = Math.Exp(AsII * Math.Log(T) + BsII / T + 2.7789 + DsII * T) * Math.Exp(VbsII / (R * T) * (P - Math.Exp(AsII * Math.Log(T) + BsII / T + 2.7789 + DsII * T))) * Math.Exp(sum2sII)
                FGHYD = FGsII

            End If

            OBJ_FUNC_HYD_KS = FGAG - FGHYD

        End Function

        Function HYD_KS2(ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim TIPO_HIDRATO As String = "sI"
            Dim sI_formers As Boolean = False
            Dim sII_formers As Boolean = False

            Dim P, PsI, PsII, Pinf, Psup As Double

            Dim i As Integer
            Dim n = UBound(Vz)

            'CHECAR PRESENcA DE FORMADORES DE HIDRATO sI E/OU sII
            i = 0
            Do
                If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 38 Or Vids(i) = 15 Or Vids(i) = 14 Then sI_formers = True
                If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 38 Then sII_formers = True
                i = i + 1
            Loop Until i = n + 1

            Dim fP, fP_inf, nsub, delta_P As Double

            If sI_formers = False Then TIPO_HIDRATO = "sII"

START_LOOP:

            Pinf = 1000
            Psup = 600 * 101325

            nsub = 10

            delta_P = (Psup - Pinf) / nsub

            Do
                fP = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
                Pinf = Pinf + delta_P
                fP_inf = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
            Loop Until fP * fP_inf < 0 Or Pinf > Psup
            If Pinf > Psup Then GoTo Final4
            Psup = Pinf
            Pinf = Pinf - delta_P


            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 50
            Dim iter2 As Integer

            aaa = Pinf
            bbb = Psup
            ccc = Psup

            faa = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, aaa, T, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:     P = bbb
            GoTo STEP2
Final4:     P = 1000 * 101325

STEP2:

            If sI_formers = True And sII_formers = True And TIPO_HIDRATO = "sI" Then
                TIPO_HIDRATO = "sII"
                PsI = P
                GoTo START_LOOP
            ElseIf sI_formers = True And sII_formers = True And TIPO_HIDRATO = "sII" Then
                PsII = P
            ElseIf sI_formers = False And sII_formers = True And TIPO_HIDRATO = "sI" Then
                TIPO_HIDRATO = "sII"
                GoTo START_LOOP
            ElseIf sI_formers = True And sII_formers = False And TIPO_HIDRATO = "sI" Then
                PsI = P
                PsII = P
            ElseIf sI_formers = False And sII_formers = True Then
                PsII = P
                PsI = P
            End If

            Dim tmpx(1)
            tmpx(0) = PsI
            tmpx(1) = PsII

            HYD_KS2 = tmpx

        End Function

        Function HYD_KS2T(ByVal P As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim TIPO_HIDRATO As String = "sI"
            Dim sI_formers As Boolean = False
            Dim sII_formers As Boolean = False

            Dim T, TsI, TsII, Tinf, Tsup As Double

            Dim i As Integer
            Dim n = UBound(Vz)

            'CHECAR PRESENcA DE FORMADORES DE HIDRATO sI E/OU sII
            i = 0
            Do
                If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then sI_formers = True
                If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Then sII_formers = True
                i = i + 1
            Loop Until i = n + 1

            Dim fT, fT_inf, nsub, delta_T As Double

            If sI_formers = False Then TIPO_HIDRATO = "sII"

START_LOOP:

            Tinf = 100
            Tsup = 350

            nsub = 25

            delta_T = (Tsup - Tinf) / nsub

            Do
                fT = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
                Tinf = Tinf + delta_T
                fT_inf = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
            Loop Until fT * fT_inf < 0 Or Tinf > Tsup
            If Tinf > Tsup Then GoTo Final4
            Tinf = Tinf - delta_T
            Tsup = Tinf + delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 50
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, P, aaa, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
            fcc = fbb

            iter2 = 0
            Do
                If (fbb > 0 And fcc > 0) Or (fbb < 0 And fcc < 0) Then
                    ccc = aaa
                    fcc = faa
                    ddd = bbb - aaa
                    eee = ddd
                End If
                If Math.Abs(fcc) < Math.Abs(fbb) Then
                    aaa = bbb
                    bbb = ccc
                    ccc = aaa
                    faa = fbb
                    fbb = fcc
                    fcc = faa
                End If
                tol11 = 0.000001
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If (Math.Abs(eee) >= tol11) And (Math.Abs(faa) > Math.Abs(fbb)) Then
                    sss = fbb / faa
                    If aaa = ccc Then
                        ppp = 2 * xmm * sss
                        qqq = 1 - sss
                    Else
                        qqq = faa / fcc
                        rrr = fbb / fcc
                        ppp = sss * (2 * xmm * qqq * (qqq - rrr) - (bbb - aaa) * (rrr - 1))
                        qqq = (qqq - 1) * (rrr - 1) * (sss - 1)
                    End If
                    If ppp > 0 Then qqq = -qqq
                    ppp = Math.Abs(ppp)
                    min11 = 3 * xmm * qqq - Math.Abs(tol11 * qqq)
                    min22 = Math.Abs(eee * qqq)
                    Dim tvar2 As Double
                    If min11 < min22 Then tvar2 = min11
                    If min11 > min22 Then tvar2 = min22
                    If 2 * ppp < tvar2 Then
                        eee = ddd
                        ddd = ppp / qqq
                    Else
                        ddd = xmm
                        eee = ddd
                    End If
                Else
                    ddd = xmm
                    eee = ddd
                End If
                aaa = bbb
                faa = fbb
                If (Math.Abs(ddd) > tol11) Then
                    bbb += ddd
                Else
                    bbb += Math.Sign(xmm) * tol11
                End If
                fbb = OBJ_FUNC_HYD_KS(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:     T = bbb
            GoTo STEP2
Final4:     T = -100

STEP2:

            If sI_formers = True And sII_formers = True And TIPO_HIDRATO = "sI" Then
                TIPO_HIDRATO = "sII"
                TsI = T
                GoTo START_LOOP
            ElseIf sI_formers = True And sII_formers = True And TIPO_HIDRATO = "sII" Then
                TsII = T
            ElseIf sI_formers = False And sII_formers = True And TIPO_HIDRATO = "sI" Then
                TIPO_HIDRATO = "sII"
                GoTo START_LOOP
            ElseIf sI_formers = True And sII_formers = False And TIPO_HIDRATO = "sI" Then
                TsI = T
                TsII = T
            ElseIf sI_formers = False And sII_formers = True Then
                TsII = T
                TsI = T
            End If

            Dim tmpx(1)
            tmpx(0) = TsI
            tmpx(1) = TsII

            HYD_KS2T = tmpx

        End Function

        Function DET_HYD_KS(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim FGsI, FGsII, FGHYD, FGAG As Double
            Dim vm(1, 1), sumvmsI(1), sumvmsII(1)
            Dim AsI, BsI, CsI, DsI
            Dim AsII, BsII, CsII, DsII
            Dim VbsI, VbsII, Nbw
            Dim C1(1, n), C2(1, n)
            Dim DT, Tnfp, DHm, Td
            Dim Vxaq(n), VxHC(n), Ki(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV
            Dim act As Double
            Dim H(n), tmp2(3)
            Dim VMG, FG, FL
            Dim TETA1(1, n), TETA2(1, n)
            Dim vi_(n), VLW
            Dim VysI, VysII, VsI(n), VsII(n), Vh(n)
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n)
            Dim Vy(n) As Double, pos, sum
            Dim sum2sI, sum2sII
            Dim R = 8.314
            Dim temp1, tv, tv2, tm1
            Dim bg As Double
            Dim i = 0
            Do
                If Vz(i) <> 0 Then
                    Tc(i) = Me.unfPP.RET_VTC()(i)
                    Tr(i) = T / Tc(i)
                    Pc(i) = Me.unfPP.RET_VPC()(i)
                    W(i) = Me.unfPP.RET_VW()(i)
                    Vp(i) = 0
                End If
                i = i + 1
            Loop Until i = n + 1

            'CALCULAR EQUILIBRIO EM BASE LIVRE DE aGUA

            pos = 0
            i = 0
            Do
                If Vids(i) = 13 Then pos = i
                i = i + 1
            Loop Until i = n + 1
            sum = 0
            i = 0
            Do
                If i <> pos Then sum += Vz(i)
                i = i + 1
            Loop Until (i = n + 1)
            If vaporonly Then
                i = 0
                Do
                    Vy(i) = Vz(i)
                    i = i + 1
                Loop Until i = n + 1
            Else
                i = 0
                Do
                    If i <> pos Then Vy(i) = Vz(i) / sum
                    If i = pos Then Vy(i) = 0
                    i = i + 1
                Loop Until i = n + 1
            End If

            'PRSV

            Dim PHIV(n), LN_CFV(n)
            Dim ai(n), bi(n), tmp(3, n + 1), a(n, n), b(n, n)
            Dim aml2(n), amv2(n)
            Dim alpha(n), m(n)
            Dim b1(n), b2(n), coeff(3) As Double
            Dim j As Integer

            Dim ai_(n)
            Dim KAPPA(n), KAPPA0(n)

            i = 0
            Do
                If Vz(i) <> 0 Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * W(i) - 0.26992 * W(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    ai_(i) = ai(i) ^ 0.5
                    bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                End If
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

            'CALCULO DOS VOLUMES PARCIAIS MOLARES a DILUIcaO INFINITA

            VLW = Math.Exp(-10.9241 + 0.00025 * (T - 273.15) - 0.0003532 * (P / 1000000.0 - 0.101325) + 0.0000001559 * (P / 1000000.0 - 0.101325) ^ 2)
            i = 0
            Do
                vi_(i) = (R * T / (VLW - bi(pos)) * (1 + bi(i) / (VLW - bi(pos))) - (2 * ai(pos) - 2 * ai(pos) * bi(i) * (VLW - bi(pos)) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos)))) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos)))) / (R * T / (VLW - bi(pos)) ^ 2 - 2 * ai(pos) * (VLW + bi(pos)) / (VLW * (VLW + bi(pos)) + bi(pos) * (VLW - bi(pos))) ^ 2)
                i = i + 1
            Loop Until i = n + 1

            'CALCULO DOS FATORES DE COMPRESSIBILIDADE a DILUIcaO INFINITA

            i = 0
            Do
                ZLinf(i) = P * vi_(i) / (R * T)
                i = i + 1
            Loop Until i = n + 1

            'CALCULO DA CONSTANTE DE HENRY

            i = 0
            Do
                If i <> pos Then
                    tmp2 = GET_HS_KS(Vids(i))
                    H(i) = 101325 * Math.Exp(-tmp2(0) - tmp2(1) / T - tmp2(2) * Math.Log(T) - tmp2(3) * T)
                End If
                i = i + 1
            Loop Until i = n + 1

            ' CALCULO DAS RAIZES PARA A Phase VAPOR

            i = 0
            Do
                amv2(i) = 0
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim amv = 0.0#
            Do
                j = 0
                Do
                    amv = amv + Vy(i) * Vy(j) * a(i, j)
                    amv2(i) = amv2(i) + Vy(j) * a(j, i)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bmv = 0.0#
            Do
                bmv = bmv + Vy(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            Dim AG = amv * P / (R * T) ^ 2
            bg = bmv * P / (R * T)

            coeff(0) = -AG * bg + bg ^ 2 + bg ^ 3
            coeff(1) = AG - 3 * bg ^ 2 - 2 * bg
            coeff(2) = bg - 1
            coeff(3) = 1

            temp1 = Poly_Roots(coeff)
            tv = 0

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

            Catch

                Dim findZV
                ZV = 1
                Do
                    findZV = coeff(3) * ZV ^ 3 + coeff(2) * ZV ^ 2 + coeff(1) * ZV + coeff(0)
                    ZV -= 0.00001
                    If ZV < 0 Then ZV = 1
                Loop Until Math.Abs(findZV) < 0.0001

            End Try

            ' CALCULO DO COEFICIENTE DE FUGACIDADE DA Phase VAPOR

            i = 0
            Do
                t1 = bi(i) * (ZV - 1) / bmv
                t2 = -Math.Log(ZV - bg)
                t3 = amv * (2 * amv2(i) / amv - bi(i) / bmv)
                t4 = Math.Log(ZV + (1 + 2 ^ 0.5) * bg) - Math.Log(ZV + ((1 - 2 ^ 0.5) * bg))
                t5 = 8 ^ 0.5 * bmv * R * T
                LN_CFV(i) = t1 + t2 - (t3 * t4 / t5)
                PHIV(i) = Math.Exp(LN_CFV(i)) * Vy(i) * P
                i = i + 1
            Loop Until i = n + 1

            If vaporonly Then

                Td = 273.15

                FGAG = PHIV(pos)

            Else

                'CALCULO DAS FRAcoES MOLARES DOS COMPONENTES NA Phase AQUOSA

                i = 0
                Do
                    If i <> pos Then Vxaq(i) = PHIV(i) / (H(i) * Math.Exp(ZLinf(i)))
                    If H(i) = 101325.0# Then Vxaq(i) = 0.0#
                    i = i + 1
                Loop Until i = n + 1

                Dim sum_vxaq = 0.0#
                i = 0
                Do
                    If i <> pos Then sum_vxaq += Vxaq(i)
                    i = i + 1
                Loop Until i = n + 1
                Vxaq(pos) = 1 - sum_vxaq

                Dim WAC As Double = unf.GAMMA(T, Vxaq, unfPP.RET_VIDS, unfPP.RET_VQ, unfPP.RET_VR, pos)

                'CALCULO DA DEPRESSaO NO PONTO DE FUSaO DA aGUA
                Tnfp = 273.15
                DHm = 6001700.0 / 1000
                DT = R * Tnfp ^ 2 / DHm * Math.Log(Vxaq(pos) * WAC)
                Td = DT + Tnfp

                'CALCULO DO VOLUME MOLAR DO GELO
                VMG = 0.00001912 + 0.0000000008387 * T + 0.000000000004016 * T ^ 2

                'CALCULO DA FUGACIDADE DA aGUA NO GELO
                FG = Math.Exp(4.6056 * Math.Log(T) - 5501.1243 / T + 2.9446 - 0.0081431 * T) * Math.Exp((VMG / (R * T) * (P - Math.Exp(4.6056 * Math.Log(T) - 5501.1243 / T + 2.9446 - 0.0081431 * T))))

                'CALCULO DA FUGACIDADE DA aGUA NA Phase LiQUIDA
                act = WAC
                FL = Vxaq(pos) * act * Math.Exp(4.1539 * Math.Log(T) - 5500.9332 / T + 7.6537 - 0.0161277 * T) * Math.Exp((VLW / (R * T) * (P - Math.Exp(4.1539 * Math.Log(T) - 5500.9332 / T + 2.9446 - 0.0161266 * T))))

                If T < Td Then FGAG = FG
                If T > Td Then FGAG = FL

            End If

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C1(0, i) = Cml_KS("sI", 1, Vids(i), T)
                            C1(1, i) = Cml_KS("sI", 2, Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Dim soma_CML11 = 0.0#, soma_CML21 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML11 += C1(0, i) * PHIV(i)
                        soma_CML21 += C1(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA1(0, i) = C1(0, i) * PHIV(i) / (1 + soma_CML11)
                        TETA1(1, i) = C1(1, i) * PHIV(i) / (1 + soma_CML21)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sI"
                Nbw = 46
                VbsI = (11.835 + 0.00002217 * T + 0.000002242 * T ^ 2) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                'CALCULAR CONSTANTES DA PRESSaO DE VAPOR DO LaTICE VAZIO

                'CALCULAR SOMAS dos VMs (sI)
                vm(0, 0) = 1 / 23
                vm(0, 1) = 3 / 23
                sumvmsI(0) = 0
                sumvmsI(1) = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then sumvmsI(0) += TETA1(0, i)
                        If i <> pos Then sumvmsI(1) += TETA1(1, i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                sum2sI = vm(0, 0) * Math.Log(1 - sumvmsI(0)) + vm(0, 1) * Math.Log(1 - sumvmsI(1))

                'calcular fracao total dos formadores de sI
                VysI = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysI += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                Dim schsI = 0.0#
                i = 0
                Do
                    If i <> pos Then schsI += (vm(0, 0) * TETA1(0, i) + vm(0, 1) * TETA1(1, i))
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    VsI(i) = (vm(0, 0) * TETA1(0, i) + vm(0, 1) * TETA1(1, i)) / schsI
                    i = i + 1
                Loop Until i = n + 1

                'para hidrato sI
                j = 0
                i = 0
                AsI = 0
                BsI = 0
                CsI = 0
                DsI = 0
                Do
                    If Vids(i) = 1 Then j = 45
                    If Vids(i) = 2 Then j = 46
                    If Vids(i) = 38 Then j = 47
                    If Vids(i) = 15 Then j = 48
                    If Vids(i) = 14 Then j = 49
                    If Vids(i) = 16 Then j = 50
                    If Vids(i) = 3 Then j = 51
                    If Vids(i) = 4 Then j = 52
                    If Vids(i) = 38 Then j = 53
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        AsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 2))
                        BsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 3))
                        CsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 4))
                        DsI += VsI(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 5)) * 0.001
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Vh = VsI

                'CALCULAR FUGACIDADE DA aGUA NO HIDRATO sI
                tm1 = Math.Exp(sum2sI)
                FGsI = Math.Exp(AsI * Math.Log(T) + BsI / T + 2.7789 + DsI * T) * Math.Exp(VbsI / (R * T) * (P - Math.Exp(AsI * Math.Log(T) + BsI / T + 2.7789 + DsI * T))) * tm1
                FGHYD = FGsI

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C2(0, i) = Cml_KS("sII", 1, Vids(i), T)
                            C2(1, i) = Cml_KS("sII", 2, Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1
                Dim soma_CML12 = 0.0#, soma_CML22 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML12 += C2(0, i) * PHIV(i)
                        soma_CML22 += C2(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA2(0, i) = C2(0, i) * PHIV(i) / (1 + soma_CML12)
                        TETA2(1, i) = C2(1, i) * PHIV(i) / (1 + soma_CML22)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sII"
                Nbw = 136
                VbsII = (17.13 + 0.0002249 * T + 0.000002013 * T ^ 2 + 0.000000001009 * T ^ 3) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                'CALCULAR SOMAS dos VMs (sII)
                vm(1, 0) = 2 / 17
                vm(1, 1) = 1 / 17
                sumvmsII(0) = 0
                sumvmsII(1) = 0
                i = 0
                Do
                    If i <> pos Then sumvmsII(0) += TETA2(0, i)
                    If i <> pos Then sumvmsII(1) += TETA2(1, i)
                    i = i + 1
                Loop Until i = n + 1
                sum2sII = vm(1, 0) * Math.Log(1 - sumvmsII(0)) + vm(1, 1) * Math.Log(1 - sumvmsII(1))

                'CALCULAR CONSTANTES DA PRESSaO DE VAPOR DO LaTICE VAZIO

                'calcular fracao total dos formadores de sII
                VysII = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysII += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                Dim schsII = 0.0#
                i = 0
                Do
                    If i <> pos Then schsII += (vm(1, 0) * TETA2(0, i) + vm(1, 1) * TETA2(1, i))
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    VsII(i) = (vm(1, 0) * TETA2(0, i) + vm(1, 1) * TETA2(1, i)) / schsII
                    i = i + 1
                Loop Until i = n + 1

                'para hidrato sII
                j = 0
                i = 0
                AsII = 0
                BsII = 0
                CsII = 0
                DsII = 0
                Do
                    If Vids(i) = 1 Then j = 45
                    If Vids(i) = 2 Then j = 46
                    If Vids(i) = 38 Then j = 47
                    If Vids(i) = 15 Then j = 48
                    If Vids(i) = 14 Then j = 49
                    If Vids(i) = 16 Then j = 50
                    If Vids(i) = 3 Then j = 51
                    If Vids(i) = 4 Then j = 52
                    If Vids(i) = 38 Then j = 53
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        AsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 2))
                        BsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 3))
                        CsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 4))
                        DsII += VsII(i) * Convert.ToDouble(am.MAT_KLAUDASANDLER(j, 5)) * 0.001
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Vh = VsII

                'CALCULAR FUGACIDADE DA aGUA NO HIDRATO sII
                FGsII = Math.Exp(AsII * Math.Log(T) + BsII / T + 2.7789 + DsII * T) * Math.Exp(VbsII / (R * T) * (P - Math.Exp(AsII * Math.Log(T) + BsII / T + 2.7789 + DsII * T))) * Math.Exp(sum2sII)
                FGHYD = FGsII

            End If

            Dim res As Object = New Object() {Td, act, FGAG, FGHYD, Vxaq, Vy, Vh, VxHC}

            DET_HYD_KS = res

        End Function

    End Class

End Namespace
