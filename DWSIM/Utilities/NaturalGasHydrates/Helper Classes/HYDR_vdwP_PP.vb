'    Hydrate Calculation Routines (Pharrish & Prausnitz)
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

    Public Class vdwP_PP

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

            Dim k As Decimal = 0.0000000000000000000000138D
            Dim i, j As Integer
            Dim Rcell1, r, a, sigma, epsilon, z1 As Double
            Dim d10_1, d11_1, d4_1, d5_1 As Double
            Dim W(100), W1(100) As Double
            Dim Int1 As Double
            Dim delta1 As Double

            Try

                If id = 1 Then i = 17
                If id = 2 Then i = 18
                If id = 3 Then i = 19
                If id = 4 Then i = 20
                If id = 14 Then i = 21
                If id = 16 Then i = 22
                If id = 15 Then i = 23
                If id = 38 Then i = 24
                epsilon = Convert.ToDouble(am.MAT_VDWP_PP(i, 1)) '* k
                sigma = Convert.ToDouble(am.MAT_VDWP_PP(i, 2)) '* 0.0000000001
                a = Convert.ToDouble(am.MAT_VDWP_PP(i, 3)) '* 0.0000000001

                If estrutura = "sI" And cela = 1 Then
                    Rcell1 = 7.95 / 2 '* 0.0000000001
                    z1 = 20
                End If
                If estrutura = "sI" And cela = 2 Then
                    Rcell1 = 8.6 / 2 '* 0.0000000001
                    z1 = 24
                End If
                If estrutura = "sII" And cela = 1 Then
                    Rcell1 = 7.82 / 2 '* 0.0000000001
                    z1 = 20
                End If
                If estrutura = "sII" And cela = 2 Then
                    Rcell1 = 9.46 / 2 '* 0.0000000001
                    z1 = 28
                End If

                delta1 = (Rcell1 - a) / 101

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
                    'MsgBox(W1(j) & vbCrLf & z1 & vbCrLf & epsilon & vbCrLf & t1 & vbCrLf & t2 & vbCrLf & t3 & vbCrLf & t4 & vbCrLf & vbCrLf & T & vbCrLf & r)
                    W(j) = Math.Exp(-W1(j) / (T)) * Math.Pow(r * 0.0000000001, 2)
                    r = r + delta1
                    j = j + 1
                Loop Until j = 101

                Int1 = W(0)
                i = 1
                Do
                    If Double.IsNaN(W(i)) Or Double.IsNegativeInfinity(W(i)) Or Double.IsPositiveInfinity(W(i)) Then
                        'MsgBox(W1(i))
                    Else
                        Int1 = Int1 + 1 * W(i)
                    End If
                    i = i + 1
                Loop Until i = 101
                Int1 = Int1 * delta1 * 0.0000000001 / 1

            Catch ex As Exception
                MsgBox(ex.ToString)
            End Try

            Cml_vdWP = Convert.ToDouble(4 * Math.PI / (k * T) * Int1)


        End Function

        Function OBJ_FUNC_HYD_vdwP(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim PQHYD, PQAG, PQRL, PQRG As Double
            Dim vm(1, 1), sumvmsI(1), sumvmsII(1)
            Dim C1(1, n), C2(1, n)
            Dim DT, Tnfp, DHm, Td, Nbw, VbsI, VbsII, FUGH, PWSAT, FUGW As Double
            Dim Vxaq(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV
            Dim act As Double
            Dim H(n), tmp2(3)
            Dim PQG, PQL, DH
            Dim TETA1(1, n), TETA2(1, n)
            Dim vi_(n), VLW
            Dim VysI, VysII, VsI(n), VsII(n)
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n)
            Dim Vy(n), pos, sum
            Dim sum2sI, sum2sII
            Dim R = 8.314, T0 = 273.15
            Dim temp1, tv, tv2
            Dim PR, dPdT, bg As Double
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

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DA PRESSaO DE FORMAcaO DO HIDRATO DE REFEReNCIA
                If T >= 211 And T <= 273 Then
                    PR = Math.Exp(23.0439 - 3357.57 / T - 1.85 * Math.Log(T)) * 101325
                    dPdT = PR * (3357.57 / T ^ 2 - 1.85 / T)
                End If
                If T > 273 And T <= 300 Then
                    PR = Math.Exp(-1212.2 + 44344 / T + 187.719 * Math.Log(T)) * 101325
                    dPdT = PR * (-44344 / T ^ 2 + 187.719 / T)
                End If

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DA PRESSaO DE FORMAcaO DO HIDRATO DE REFEReNCIA
                If T >= 253 And T <= 273 Then
                    PR = Math.Exp(11.5115 - 4092.37 / T + 0.316033 * Math.Log(T)) * 101325
                    dPdT = PR * (4092.37 / T ^ 2 + 0.316033 / T)
                End If
                If T > 273 And T <= 291 Then
                    PR = Math.Exp(-1023.14 + 34984.3 / T + 159.923 * Math.Log(T)) * 101325
                    dPdT = PR * (-34984.3 / T ^ 2 + 159.923 / T)
                End If
                If T > 291 And T <= 303 Then
                    PR = Math.Exp(4071.64 - 193428.8 / T - 599.755 * Math.Log(T)) * 101325
                    dPdT = PR * (193428.8 / T ^ 2 - 599.755 / T)
                End If

            End If

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
                    tmp2 = am.GET_HS_KS(Vids(i))
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

                FUGW = PHIV(pos)

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

                'CALCULO DO POTENCIAL QUiMICO DA aGUA NO GELO
                DH = -4.1868 * (275 + (9.11 + 0.0336 * 273.1) * (T - T0) - 0.0168 * (T ^ 2 - T0 ^ 2))
                PQRG = 302 * 4.1868 / (R * T0) + DH / R * (1 / T - 1 / T0) + 3 * 0.000001 / R * dPdT * Math.Log(T / T0)
                PQG = PQRG * R * T + 3 * 0.000001 * (P - PR)

                'CALCULO DO POTENCIAL QUiMICO DA aGUA NA Phase LiQUIDA
                act = WAC
                DH = -4.1868 * (1463.3 + 275 + (9.11 + 0.0336 * 273.1) * (T - T0) - 0.0168 * (T ^ 2 - T0 ^ 2))
                PQRL = 302 * 4.1868 / (R * T0) + DH / R * (1 / T - 1 / T0) + (3 * 0.000001 + Math.Exp(-10.9241) - 0.00001912) / R * dPdT * Math.Log(T / T0)
                PQL = PQRL * R * T + (3 * 0.000001 + Math.Exp(-10.9241) - 0.00001912) * (P - PR)

                If T < Td Then PQAG = PQG
                If T > Td Then PQAG = PQL

            End If

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C1(0, i) = Cml_vdWP("sI", 1, Vids(i), T)
                            C1(1, i) = Cml_vdWP("sI", 2, Vids(i), T)
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
                        TETA1(0, i) = C1(0, i) * PHIV(i)
                        TETA1(1, i) = C1(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'calcular fracao total dos formadores de sI
                VysI = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysI += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

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
                sum2sI = vm(0, 0) * Math.Log(1 + sumvmsI(0)) + vm(0, 1) * Math.Log(1 + sumvmsI(1))

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

                If T < Td Or vaporonly Then
                    PQHYD = R * T * sum2sI
                Else
                    PQHYD = R * T * sum2sI + R * T * Math.Log(Vxaq(pos) * act)
                End If

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sI"
                Nbw = 46
                VbsI = (11.835 + 0.00002217 * T + 0.000002242 * T ^ 2) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                PWSAT = Math.Exp(17.44 - 6003.9 / T) * 1000

                FUGH = PWSAT * 1.0# * Math.Exp(VbsI * (P - PWSAT) / (R * T)) * Math.Exp(sum2sI)

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C2(0, i) = Cml_vdWP("sII", 1, Vids(i), T)
                            C2(1, i) = Cml_vdWP("sII", 2, Vids(i), T)
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
                        TETA2(0, i) = C2(0, i) * PHIV(i)
                        TETA2(1, i) = C2(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'calcular fracao total dos formadores de sII
                VysII = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysII += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sII)
                vm(1, 0) = 2 / 17
                vm(1, 1) = 1 / 17
                sumvmsII(0) = 0
                sumvmsII(1) = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then sumvmsII(0) += TETA2(0, i)
                        If i <> pos Then sumvmsII(1) += TETA2(1, i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                sum2sII = vm(1, 0) * Math.Log(1 + sumvmsII(0)) + vm(1, 1) * Math.Log(1 + sumvmsII(1))

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

                If T < Td Or vaporonly Then
                    PQHYD = R * T * sum2sII
                Else
                    PQHYD = R * T * sum2sII + R * T * Math.Log(Vxaq(pos) * act)
                End If

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sII"
                Nbw = 136
                VbsII = (17.13 + 0.0002249 * T + 0.000002013 * T ^ 2 + 0.000000001009 * T ^ 3) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                PWSAT = Math.Exp(17.332 - 6017.6 / T) * 1000

                FUGH = PWSAT * 1.0# * Math.Exp(VbsII * (P - PWSAT) / (R * T)) * Math.Exp(sum2sII)

            End If

            If vaporonly Then
                OBJ_FUNC_HYD_vdwP = FUGW - FUGH
            Else
                OBJ_FUNC_HYD_vdwP = -PQAG + PQHYD
            End If

        End Function

        Function HYD_vdwP2(ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

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
                'MessageBox.Show(Pinf & " " & T & " " & Vz(0) & " " & Vids(0), MsgBoxStyle.Critical)
                fP = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
                Pinf = Pinf + delta_P
                fP_inf = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
                'If Pinf > Psup Then
                '    Throw New Exception(DWSIM.App.GetLocalString("Noforamencontradasco"))
                'End If
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

            faa = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, aaa, T, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
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
                fbb = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
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

            HYD_vdwP2 = tmpx

        End Function

        Function HYD_vdwP2T(ByVal P As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

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

            Tinf = 350
            Tsup = 100

            nsub = 25

            delta_T = (Tsup - Tinf) / nsub

            Do
                fT = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
                Tinf = Tinf + delta_T
                fT_inf = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
                'If Tinf > Tsup Then
                '    Throw New Exception(DWSIM.App.GetLocalString("Noforamencontradasco") & vbCrLf & fT & " " & fT_inf & " " & Tinf & " " & Tsup)
                'End If
            Loop Until fT * fT_inf < 0 Or Tinf < Tsup
            If Tinf < Tsup Then GoTo Final4
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 50
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, P, aaa, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
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
                fbb = OBJ_FUNC_HYD_vdwP(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
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

            HYD_vdwP2T = tmpx

        End Function

        Function DET_HYD_vdwP(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim PQHYD, PQAG, PQRL, PQRG As Double
            Dim vm(1, 1), sumvmsI(1), sumvmsII(1)
            Dim C1(1, n), C2(1, n)
            Dim DT, Tnfp, DHm, Td, VbsI, VbsII, PWSAT, Nbw As Double
            Dim Vxaq(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV
            Dim act As Double
            Dim H(n), tmp2(3)
            Dim PQG, PQL, DH
            Dim TETA1(1, n), TETA2(1, n)
            Dim vi_(n), VLW
            Dim VysI, VysII, VsI(n), VsII(n), Vh(n), VxHC(n), Ki(n) As Double
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n)
            Dim Vy(n) As Double, pos, sum
            Dim sum2sI, sum2sII
            Dim R = 8.314, T0 = 273.15
            Dim temp1, tv, tv2
            Dim PR, dPdT, bg As Double
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

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DA PRESSaO DE FORMAcaO DO HIDRATO DE REFEReNCIA
                If T >= 211 And T <= 273 Then
                    PR = Math.Exp(23.0439 - 3357.57 / T - 1.85 * Math.Log(T)) * 101325
                    dPdT = PR * (3357.57 / T ^ 2 - 1.85 / T)
                End If
                If T > 273 And T <= 300 Then
                    PR = Math.Exp(-1212.2 + 44344 / T + 187.719 * Math.Log(T)) * 101325
                    dPdT = PR * (-44344 / T ^ 2 + 187.719 / T)
                End If

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DA PRESSaO DE FORMAcaO DO HIDRATO DE REFEReNCIA
                If T >= 253 And T <= 273 Then
                    PR = Math.Exp(11.5115 - 4092.37 / T + 0.316033 * Math.Log(T)) * 101325
                    dPdT = PR * (4092.37 / T ^ 2 + 0.316033 / T)
                End If
                If T > 273 And T <= 291 Then
                    PR = Math.Exp(-1023.14 + 34984.3 / T + 159.923 * Math.Log(T)) * 101325
                    dPdT = PR * (-34984.3 / T ^ 2 + 159.923 / T)
                End If
                If T > 291 And T <= 303 Then
                    PR = Math.Exp(4071.64 - 193428.8 / T - 599.755 * Math.Log(T)) * 101325
                    dPdT = PR * (193428.8 / T ^ 2 - 599.755 / T)
                End If

            End If

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
                    tmp2 = am.GET_HS_KS(Vids(i))
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

                PQAG = PHIV(pos)

                i = 0
                Do
                    Vxaq(i) = 0.0#
                    VxHC(i) = 0.0#
                    i += 1
                Loop Until i = n + 1

            Else

                'CALCULO DAS FRAcoES MOLARES DOS COMPONENTES NA Phase AQUOSA

                Dim fresult As Object = unfPP.FlashBase.Flash_PT(Vy, P, T, unfPP)
                If fresult(0) > 0.0# Then
                    For i = 0 To n
                        Ki(i) = fresult(3)(i) / fresult(2)(i)
                        If Double.IsNaN(Ki(i)) Then Ki(i) = Double.PositiveInfinity
                    Next
                Else
                    For i = 0 To n
                        Ki(i) = Double.PositiveInfinity
                    Next
                End If

                i = 0
                Do
                    If i <> pos Then Vxaq(i) = PHIV(i) / (H(i) * Math.Exp(ZLinf(i)))
                    If H(i) = 101325.0# Then
                        Vxaq(i) = 0.0#
                    End If
                    VxHC(i) = Vy(i) / Ki(i)
                    i = i + 1
                Loop Until i = n + 1

                Dim sum_vxaq = 0.0#
                Dim sum_vxhc = 0.0#
                i = 0
                Do
                    If i <> pos Then sum_vxaq += Vxaq(i)
                    sum_vxhc += VxHC(i)
                    i = i + 1
                Loop Until i = n + 1
                Vxaq(pos) = 1 - sum_vxaq

                i = 0
                Do
                    If sum_vxhc <> 0.0# Then VxHC(i) = VxHC(i) / sum_vxhc
                    i = i + 1
                Loop Until i = n + 1

                Dim WAC As Double = unf.GAMMA(T, Vxaq, unfPP.RET_VIDS, unfPP.RET_VQ, unfPP.RET_VR, pos)

                'CALCULO DA DEPRESSaO NO PONTO DE FUSaO DA aGUA
                Tnfp = 273.15
                DHm = 6001700.0 / 1000
                DT = R * Tnfp ^ 2 / DHm * Math.Log(Vxaq(pos) * WAC)
                Td = DT + Tnfp

                'CALCULO DO POTENCIAL QUiMICO DA aGUA NO GELO
                DH = -4.1868 * (275 + (9.11 + 0.0336 * 273.1) * (T - T0) - 0.0168 * (T ^ 2 - T0 ^ 2))
                PQRG = 302 * 4.1868 / (R * T0) + DH / R * (1 / T - 1 / T0) + 3 * 0.000001 / R * dPdT * Math.Log(T / T0)
                PQG = PQRG * R * T + 3 * 0.000001 * (P - PR)

                'CALCULO DO POTENCIAL QUiMICO DA aGUA NA Phase LiQUIDA
                act = WAC
                DH = -4.1868 * (1463.3 + 275 + (9.11 + 0.0336 * 273.1) * (T - T0) - 0.0168 * (T ^ 2 - T0 ^ 2))
                PQRL = 302 * 4.1868 / (R * T0) + DH / R * (1 / T - 1 / T0) + (3 * 0.000001 + Math.Exp(-10.9241) - 0.00001912) / R * dPdT * Math.Log(T / T0)
                PQL = PQRL * R * T + (3 * 0.000001 + Math.Exp(-10.9241) - 0.00001912) * (P - PR)

                If T <= Td Then PQAG = PQG
                If T > Td Then PQAG = PQL

            End If

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C1(0, i) = Cml_vdWP("sI", 1, Vids(i), T)
                            C1(1, i) = Cml_vdWP("sI", 2, Vids(i), T)
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
                        TETA1(0, i) = C1(0, i) * PHIV(i)
                        TETA1(1, i) = C1(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'calcular fracao total dos formadores de sI
                VysI = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysI += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

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
                sum2sI = vm(0, 0) * Math.Log(1 + sumvmsI(0)) + vm(0, 1) * Math.Log(1 + sumvmsI(1))

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

                Vh = VsI

                If T < Td Or vaporonly Then
                    PQHYD = R * T * sum2sI
                Else
                    PQHYD = R * T * sum2sI + R * T * Math.Log(Vxaq(pos) * act)
                End If

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sI"
                Nbw = 46
                VbsI = (11.835 + 0.00002217 * T + 0.000002242 * T ^ 2) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                PWSAT = Math.Exp(17.44 - 6003.9 / T) * 1000

                PQHYD = PWSAT * 1.0# * Math.Exp(VbsI * (P - PWSAT) / (R * T)) * Math.Exp(sum2sI)

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then
                            C2(0, i) = Cml_vdWP("sII", 1, Vids(i), T)
                            C2(1, i) = Cml_vdWP("sII", 2, Vids(i), T)
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
                        TETA2(0, i) = C2(0, i) * PHIV(i)
                        TETA2(1, i) = C2(1, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'calcular fracao total dos formadores de sII
                VysII = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then VysII += Vy(i)
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sII)
                vm(1, 0) = 2 / 17
                vm(1, 1) = 1 / 17
                sumvmsII(0) = 0
                sumvmsII(1) = 0
                i = 0
                Do
                    If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then
                        If i <> pos Then sumvmsII(0) += TETA2(0, i)
                        If i <> pos Then sumvmsII(1) += TETA2(1, i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                sum2sII = vm(1, 0) * Math.Log(1 + sumvmsII(0)) + vm(1, 1) * Math.Log(1 + sumvmsII(1))

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

                Vh = VsII

                If T < Td Or vaporonly Then
                    PQHYD = R * T * sum2sII
                Else
                    PQHYD = R * T * sum2sII + R * T * Math.Log(Vxaq(pos) * act)
                End If

                'CALCULO DO VOLUME MOLAR DO HIDRATO "sII"
                Nbw = 136
                VbsII = (17.13 + 0.0002249 * T + 0.000002013 * T ^ 2 + 0.000000001009 * T ^ 3) * 1.0E-30 * 6.02E+23 / Nbw - 0.000000008006 * P / 1000000.0 + 0.000000000005448 * P ^ 2 / 1000000000000.0

                PWSAT = Math.Exp(17.332 - 6017.6 / T) * 1000

                PQHYD = PWSAT * 1.0# * Math.Exp(VbsII * (P - PWSAT) / (R * T)) * Math.Exp(sum2sII)

            End If

            Dim res As Object = New Object() {Td, act, PQAG, PQHYD, Vxaq, Vy, Vh, VxHC}

            DET_HYD_vdwP = res

        End Function

    End Class

End Namespace
