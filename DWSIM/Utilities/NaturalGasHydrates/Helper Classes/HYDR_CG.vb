'    Hydrate Calculation Routines (Chen & Guo)
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
Imports DWSIM.Thermodynamics.PropertyPackages

Namespace DWSIM.Utilities.HYD

    Public Class ChenGuo

        Dim am As DWSIM.Utilities.HYD.AuxMethods
        Dim unf As PropertyPackages.Auxiliary.UNIQUAC
        Dim unfPP As PropertyPackages.UNIQUACPropertyPackage
        Dim prPP As PropertyPackages.PengRobinsonPropertyPackage

        Sub New(ByVal materialStream As Streams.MaterialStream)

            am = New DWSIM.Utilities.HYD.AuxMethods
            prPP = New PropertyPackages.PengRobinsonPropertyPackage
            unf = New PropertyPackages.Auxiliary.UNIQUAC
            unfPP = New PropertyPackages.UNIQUACPropertyPackage

            unfPP.CurrentMaterialStream = materialStream
            prPP.CurrentMaterialStream = materialStream

        End Sub

        Function Cml_CG(ByVal id As Integer, ByVal T As Double)

            Dim i As Integer

            If id = 16 Then i = 23
            If id = 15 Then i = 25
            If id = 14 Then i = 26
            If id = 1 Then i = 27

            Dim X = am.MAT_CHENGUO(i, 1) * 0.000001
            Dim Y = am.MAT_CHENGUO(i, 2)
            Dim Z = am.MAT_CHENGUO(i, 3)

            Cml_CG = X * Math.Exp(Y / (T - Z)) / 100000

        End Function

        Function F0Pconst(ByVal id As Integer, ByVal estrut As String)

            Dim l As Integer
            l = 17
            If id = 1 Then l = 10
            If id = 2 Then l = 11
            If id = 3 Then l = 12
            If id = 4 Then l = 15
            If id = 5 Then l = 16
            If id = 38 Then l = 14
            If id = 16 Then l = 6
            If id = 17 Then l = 7
            If id = 15 Then l = 8
            If id = 14 Then l = 9

            Dim tmp(2) As Double
            If estrut = "sI" Then
                tmp(0) = am.MAT_CHENGUO(l, 1) * 10000000000.0
                tmp(1) = am.MAT_CHENGUO(l, 2)
                tmp(2) = am.MAT_CHENGUO(l, 3)
            Else
                tmp(0) = am.MAT_CHENGUO(l, 4) * 1.0E+23
                tmp(1) = am.MAT_CHENGUO(l, 5)
                tmp(2) = am.MAT_CHENGUO(l, 6)
            End If

            F0Pconst = tmp

        End Function

        Function AIJ_CG(ByVal i As Integer, ByVal j As Integer)

            AIJ_CG = 0
            If i = 1 And j = 2 Then AIJ_CG = 154
            If i = 16 And j = 3 Then AIJ_CG = 292
            If i = 15 And j = 5 Then AIJ_CG = 530
            If i = 14 And j = 4 Then AIJ_CG = 100
            If i = 1 And j = 2 Then AIJ_CG = 50
            If i = 16 And j = 3 Then AIJ_CG = 155
            If i = 15 And j = 5 Then AIJ_CG = 297
            If i = 14 And j = 4 Then AIJ_CG = 67
            If i = 1 And j = 2 Then AIJ_CG = 165
            If i = 16 And j = 3 Then AIJ_CG = 352
            If i = 15 And j = 5 Then AIJ_CG = 560
            If i = 14 And j = 4 Then AIJ_CG = 100
            If i = 1 And j = 2 Then AIJ_CG = 450
            If i = 16 And j = 3 Then AIJ_CG = 790
            If i = 15 And j = 5 Then AIJ_CG = 1500
            If i = 14 And j = 4 Then AIJ_CG = 879

        End Function

        Function OBJ_FUNC_HYD_CG(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim F0Ts1(n), F0Ps1(n), F0As1(n), F0sI(n) As Double
            Dim F0Ts2(n), F0Ps2(n), F0As2(n), F0sII(n) As Double
            Dim consts, consts2 As Double()
            Dim objfunc As Double
            Dim vm(1, 1), sumTETAsI, sumTETAsII As Double
            Dim C1(1, n), C2(1, n) As Double
            Dim DT, Tnfp, DHm, Td As Double
            Dim Vx(n), Vx2(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV As Double
            Dim H(n), tmp2(3) As Double
            Dim TETA1(1, n), TETA2(1, n) As Double
            Dim vi_(n) As Double
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n) As Double
            Dim Vy(n) As Double, pos
            Dim R = 8.314
            Dim temp1, tv, tv2
            Dim bg As Double

            Dim i = 0
            Do
                Tc(i) = Me.unfPP.RET_VTC()(i)
                Tr(i) = T / Tc(i)
                Pc(i) = Me.unfPP.RET_VPC()(i)
                W(i) = Me.unfPP.RET_VW()(i)
                i = i + 1
            Loop Until i = n + 1

            'CALCULAR EQUILIBRIO EM BASE LIVRE DE aGUA

            pos = 0
            i = 0
            Do
                If Vids(i) = 13 Then pos = i
                i = i + 1
            Loop Until i = n + 1
            Dim sum = 0.0#
            i = 0
            Do
                If i <> pos Then sum += Vz(i)
                i = i + 1
            Loop Until (i = n + 1)
            i = 0
            Do
                If i <> pos Then Vy(i) = Vz(i) / sum
                If i = pos Then Vy(i) = 0
                i = i + 1
            Loop Until i = n + 1

            'CALCULAR EQUILIBRIO L-V

            Dim eqtmp = prPP.FlashBase.Flash_PT(Vz, P, T, prPP)

            Dim L = eqtmp(0)
            Dim V = eqtmp(1)
            Vx = eqtmp(2)
            Vy = eqtmp(3)
            Vx2 = eqtmp(6)

            'PR

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

            'CALCULO DOS VOLUMES PARCIAIS MOLARES a DILUIcaO INFINITA

            Dim VLW = Math.Exp(-10.9241 + 0.00025 * (T - 273.15) - 0.0003532 * (P / 1000000.0 - 0.101325) + 0.0000001559 * (P / 1000000.0 - 0.101325) ^ 2)
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

            'CALCULO DAS FRAcoES MOLARES DOS COMPONENTES NA Phase AQUOSA

            i = 0
            Do
                If i <> pos Then Vx(i) = PHIV(i) / (H(i) * Math.Exp(ZLinf(i)))
                If H(i) = 101325.0# Then Vx(i) = 0.0#
                i = i + 1
            Loop Until i = n + 1

            Dim sum_vxaq = 0.0#
            i = 0
            Do
                If i <> pos Then sum_vxaq += Vx(i)
                i = i + 1
            Loop Until i = n + 1
            Vx(pos) = 1 - sum_vxaq

            Dim WAC As Double = unf.GAMMA(T, Vx, unfPP.RET_VIDS, unfPP.RET_VQ, unfPP.RET_VR, pos)

            'CALCULO DA DEPRESSaO NO PONTO DE FUSaO DA aGUA
            Tnfp = 273.15
            DHm = 6001700.0 / 1000
            DT = R * Tnfp ^ 2 / DHm * Math.Log(Vx(pos) * WAC)
            Td = DT + Tnfp

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Then
                        If i <> pos Then
                            C1(0, i) = Cml_CG(Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Dim soma_CML11 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML11 += C1(0, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA1(0, i) = C1(0, i) * PHIV(i) / (1 + soma_CML11)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sI)
                vm(0, 0) = 1 / 23
                sumTETAsI = 0
                i = 0
                Do
                    If i <> pos Then sumTETAsI += TETA1(0, i)
                    i = i + 1
                Loop Until i = n + 1


                i = 0
                Do
                    F0Ps1(i) = Math.Exp(0.4242 / 100000 * P / T)
                    consts = F0Pconst(Vids(i), "sI")
                    F0Ts1(i) = consts(0) * Math.Exp(consts(1) / (T - consts(2))) * 100000
                    F0As1(i) = (Vx(pos) * WAC) ^ (-23 / 3)
                    F0sI(i) = F0Ps1(i) * F0Ts1(i) * F0As1(i)
                    If T < Td Then F0sI(i) = F0sI(i) * Math.Exp(22.5 * (T - 273.15) / T)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                objfunc = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 3 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Or Vids(i) = 17 Or Vids(i) = 38 Then
                        objfunc += PHIV(i) / (F0sI(i) * (1 - sumTETAsI) ^ (1 / 3))
                    End If
                    i = i + 1
                Loop Until i = n + 1

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Then
                        If i <> pos Then
                            C2(0, i) = Cml_CG(Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1
                Dim soma_CML12 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML12 += C2(0, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA2(0, i) = C2(0, i) * PHIV(i) / (1 + soma_CML12)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sII)
                sumTETAsII = 0
                i = 0
                Do
                    If i <> pos Then sumTETAsII += TETA2(0, i)
                    i = i + 1
                Loop Until i = n + 1

                Dim k As Integer = 0
                Dim sumAIJ(n)
                i = 0
                Do
                    sumAIJ(i) = 0
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    k = 0
                    Do
                        sumAIJ(i) += AIJ_CG(Vids(i), Vids(k)) * TETA2(0, k)
                        k = k + 1
                    Loop Until k = n + 1
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    F0Ps2(i) = Math.Exp(1.0224 / 100000 * P / T)
                    consts2 = F0Pconst(Vids(i), "sII")
                    F0Ts2(i) = Math.Exp(-sumAIJ(i) / T) * consts2(0) * Math.Exp(consts2(1) / (T - consts2(2))) * 100000
                    F0As2(i) = (Vx(pos) * WAC) ^ (-17)
                    F0sII(i) = F0Ps2(i) * F0Ts2(i) * F0As2(i)
                    If T < Td Then F0sII(i) = F0sII(i) * Math.Exp(49.5 * (T - 273.15) / T)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                objfunc = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 5 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Or Vids(i) = 17 Or Vids(i) = 38 Then
                        objfunc += PHIV(i) / (F0sII(i) * (1 - sumTETAsII) ^ (2))
                    End If
                    i = i + 1
                Loop Until i = n + 1

            End If

            OBJ_FUNC_HYD_CG = 1 - objfunc

        End Function

        Function HYD_CG2(ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim TIPO_HIDRATO As String = "sI"
            Dim sI_formers As Boolean = False
            Dim sII_formers As Boolean = False

            Dim P, PsI, PsII, Pinf, Psup As Double

            Dim i As Integer
            Dim n = UBound(Vz)

            'CHECAR PRESENcA DE FORMADORES DE HIDRATO sI E/OU sII
            i = 0
            Do
                If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 15 Or Vids(i) = 14 Then sI_formers = True
                If Vids(i) = 16 Or Vids(i) = 3 Or Vids(i) = 4 Then sII_formers = True
                i = i + 1
            Loop Until i = n + 1

            Dim fP, fP_inf, nsub, delta_P As Double

            If sI_formers = False Then TIPO_HIDRATO = "sII"

START_LOOP:

            Pinf = 1000
            Psup = 600 * 101325

            nsub = 5

            delta_P = (Psup - Pinf) / nsub

            Do
                fP = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
                Pinf = Pinf + delta_P
                fP_inf = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, Pinf, T, Vz, Vids, vaporonly)
            Loop Until fP * fP_inf < 0 Or Pinf > Psup
            If Pinf > Psup Then GoTo Final4
            Psup = Pinf
            Pinf = Pinf - delta_P

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 100
            Dim iter2 As Integer

            aaa = Pinf
            bbb = Psup
            ccc = Psup

            faa = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, aaa, T, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
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
                fbb = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, bbb, T, Vz, Vids, vaporonly)
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

            HYD_CG2 = tmpx

        End Function

        Function HYD_CG2T(ByVal P As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

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
                fT = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
                Tinf = Tinf + delta_T
                fT_inf = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, P, Tinf, Vz, Vids, vaporonly)
            Loop Until fT * fT_inf < 0 Or Tinf < Tsup
            If Tinf < Tsup Then GoTo Final4
            Tsup = Tinf
            Tinf = Tinf - delta_T

            'metodo de Brent para encontrar Vc

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 10000
            Dim iter2 As Integer

            aaa = Tinf
            bbb = Tsup
            ccc = Tsup

            faa = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, P, aaa, Vz, Vids, vaporonly)
            fbb = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
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
                fbb = OBJ_FUNC_HYD_CG(TIPO_HIDRATO, P, bbb, Vz, Vids, vaporonly)
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

            HYD_CG2T = tmpx

        End Function

        Function DET_HYD_CG(ByVal TIPO_HIDRATO As String, ByVal P As Double, ByVal T As Double, ByVal Vz As Object, ByVal Vids As Object, Optional ByVal vaporonly As Boolean = False) As Object

            Dim n = UBound(Vz)
            Dim F0Ts1(n), F0Ps1(n), F0As1(n), F0sI(n) As Double
            Dim F0Ts2(n), F0Ps2(n), F0As2(n), F0sII(n) As Double
            Dim consts, consts2 As Double()
            Dim objfunc As Double
            Dim vm(1, 1), sumTETAsI, sumTETAsII As Double
            Dim C1(1, n), C2(1, n) As Double
            Dim DT, Tnfp, DHm, Td As Double
            Dim Vx(n), VxHC(n), Vh(n), Ki(n), t1, t2, t3, t4, t5 As Double
            Dim ZLinf(n), ZV As Double
            Dim H(n), tmp2(3) As Double
            Dim TETA1(1, n), TETA2(1, n) As Double
            Dim vi_(n) As Double
            Dim Vp(n), Tc(n), Tb(n), Pc(n), Vc(n), Zc(n), W(n), Tr(n) As Double
            Dim Vy(n) As Double, pos
            Dim R = 8.314
            Dim temp1, tv, tv2
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

            'CALCULAR EQUILIBRIO L-V

            Dim eqtmp = prPP.FlashBase.Flash_PT(Vz, P, T, prPP)

            Dim L = eqtmp(0)
            Dim V = eqtmp(1)
            Vx = eqtmp(2)
            Vy = eqtmp(3)

            'PR

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

            'CALCULO DOS VOLUMES PARCIAIS MOLARES a DILUIcaO INFINITA

            Dim VLW = Math.Exp(-10.9241 + 0.00025 * (T - 273.15) - 0.0003532 * (P / 1000000.0 - 0.101325) + 0.0000001559 * (P / 1000000.0 - 0.101325) ^ 2)
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

            'CALCULO DAS FRAcoES MOLARES DOS COMPONENTES NA Phase AQUOSA

            Dim VyNW(n) As Double
            For i = 0 To n
                If i = pos Then VyNW(i) = 0.0# Else VyNW(i) = Vy(i) / (1 - Vy(pos))
            Next

            Dim fresult As Object = unfPP.FlashBase.Flash_PT(VyNW, P, T, unfPP)
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
                If i <> pos Then Vx(i) = PHIV(i) / (H(i) * Math.Exp(ZLinf(i)))
                If H(i) = 101325.0# Then
                    Vx(i) = 0.0#
                End If
                If i <> pos Then VxHC(i) = Vy(i) / Ki(i)
                i = i + 1
            Loop Until i = n + 1

            Dim sum_vxaq = 0.0#
            Dim sum_vxhc = 0.0#
            i = 0
            Do
                If i <> pos Then sum_vxaq += Vx(i)
                sum_vxhc += VxHC(i)
                i = i + 1
            Loop Until i = n + 1
            Vx(pos) = 1 - sum_vxaq

            i = 0
            Do
                If sum_vxhc <> 0.0# Then VxHC(i) = VxHC(i) / sum_vxhc
                i = i + 1
            Loop Until i = n + 1

            Dim WAC As Double = unf.GAMMA(T, Vx, unfPP.RET_VIDS, unfPP.RET_VQ, unfPP.RET_VR, pos)

            'CALCULO DA DEPRESSaO NO PONTO DE FUSaO DA aGUA
            Tnfp = 273.15
            DHm = 6001700.0 / 1000
            DT = R * Tnfp ^ 2 / DHm * Math.Log(Vx(pos) * WAC)
            Td = DT + Tnfp

            If TIPO_HIDRATO = "sI" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SI"
                i = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Then
                        If i <> pos Then
                            C1(0, i) = Cml_CG(Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1

                Dim soma_CML11 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML11 += C1(0, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA1(0, i) = C1(0, i) * PHIV(i) / (1 + soma_CML11)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sI)
                vm(0, 0) = 1 / 23
                sumTETAsI = 0
                i = 0
                Do
                    If i <> pos Then sumTETAsI += TETA1(0, i)
                    i = i + 1
                Loop Until i = n + 1


                i = 0
                Do
                    F0Ps1(i) = Math.Exp(0.4242 / 100000 * P / T)
                    consts = F0Pconst(Vids(i), "sI")
                    F0Ts1(i) = consts(0) * Math.Exp(consts(1) / (T - consts(2))) * 100000
                    F0As1(i) = (Vx(pos) * WAC) ^ (-23 / 3)
                    F0sI(i) = F0Ps1(i) * F0Ts1(i) * F0As1(i)
                    If T < Td Then F0sI(i) = F0sI(i) * Math.Exp(22.5 * (T - 273.15) / T)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                objfunc = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 3 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Or Vids(i) = 17 Or Vids(i) = 38 Then
                        objfunc += PHIV(i) / (F0sI(i) * (1 - sumTETAsI) ^ (1 / 3))
                        Vh(i) = PHIV(i) / (F0sI(i) * (1 - sumTETAsI) ^ (1 / 3))
                    End If
                    i = i + 1
                Loop Until i = n + 1

            ElseIf TIPO_HIDRATO = "sII" Then

                'CALCULO DAS CONSTANTES DE LANGMUIR PARA HIDRATO TIPO "SII"
                i = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Then
                        If i <> pos Then
                            C2(0, i) = Cml_CG(Vids(i), T)
                        End If
                    End If
                    i = i + 1
                Loop Until i = n + 1
                Dim soma_CML12 = 0.0#
                i = 0
                Do
                    If i <> pos Then
                        soma_CML12 += C2(0, i) * PHIV(i)
                    End If
                    i = i + 1
                Loop Until i = n + 1
                i = 0
                Do
                    If i <> pos Then
                        TETA2(0, i) = C2(0, i) * PHIV(i) / (1 + soma_CML12)
                    End If
                    i = i + 1
                Loop Until i = n + 1

                'CALCULAR SOMAS dos VMs (sII)
                sumTETAsII = 0
                i = 0
                Do
                    If i <> pos Then sumTETAsII += TETA2(0, i)
                    i = i + 1
                Loop Until i = n + 1

                Dim k As Integer = 0
                Dim sumAIJ(n)
                i = 0
                Do
                    sumAIJ(i) = 0
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    k = 0
                    Do
                        sumAIJ(i) += AIJ_CG(Vids(i), Vids(k)) * TETA2(0, k)
                        k = k + 1
                    Loop Until k = n + 1
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                Do
                    F0Ps2(i) = Math.Exp(1.0224 / 100000 * P / T)
                    consts2 = F0Pconst(Vids(i), "sII")
                    F0Ts2(i) = Math.Exp(-sumAIJ(i) / T) * consts2(0) * Math.Exp(consts2(1) / (T - consts2(2))) * 100000
                    F0As2(i) = (Vx(pos) * WAC) ^ (-17)
                    F0sII(i) = F0Ps2(i) * F0Ts2(i) * F0As2(i)
                    If T < Td Then F0sII(i) = F0sII(i) * Math.Exp(49.5 * (T - 273.15) / T)
                    i = i + 1
                Loop Until i = n + 1

                i = 0
                objfunc = 0
                Do
                    If Vids(i) = 1 Or Vids(i) = 2 Or Vids(i) = 3 Or Vids(i) = 4 Or Vids(i) = 5 Or Vids(i) = 14 Or Vids(i) = 15 Or Vids(i) = 16 Or Vids(i) = 17 Or Vids(i) = 38 Then
                        objfunc += PHIV(i) / (F0sII(i) * (1 - sumTETAsII) ^ (2))
                        Vh(i) = PHIV(i) / (F0sII(i) * (1 - sumTETAsII) ^ (2))
                    End If
                    i = i + 1
                Loop Until i = n + 1

            End If

            Dim res As Object = New Object() {Td, WAC, 0, 0, Vx, Vy, Vh, VxHC}
            DET_HYD_CG = res

        End Function

    End Class

End Namespace
