'    Property Package Auxiliary Calculations Base Classes 
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

Imports DWSIM.MathOps.MathEx.PolySolve

Namespace PropertyPackages.Auxiliary

    <System.Serializable()> Public Class PROPS

        Shared Function Cpl_rb(cpig As Double, T As Double, Tc As Double, w As Double, MW As Double) As Double

            'liquid heat capacity by Rowlinson/Bondi correlation

            'cpig = ideal gas heat capacity, kJ/kg.K
            'T = temperature in K
            'Tc = critical temperature in K
            'w = acentric factor
            'MW = mole weight, kg/kmol

            Dim cpl, Tr, R As Double

            R = 8.314
            Tr = T / Tc

            cpl = cpig * MW + 1.45 * R + 0.45 * R * (1 - Tr) ^ -1 + 0.25 * w * R * (17.11 + 25.2 * (1 - Tr) ^ (1 / 3) * Tr ^ -1 + 1.742 * (1 - Tr) ^ -1)

            Return cpl / MW 'kJ/kg.K


        End Function

        Shared Function Cpig_lk(ByVal WK As Double, ByVal w As Double, ByVal T As Double) As Double

            'Ideal gas state heat capacity for petroleum fractions (pseudos) by Lee-Kesler

            Dim A0, A1, A2, B0, B1, B2, C, Cp As Double

            A0 = -1.41779 + 0.11828 * WK
            A1 = -(6.99724 - 8.69326 * WK + 0.27715 * WK ^ 2) * 0.0001
            A2 = -0.0000022582
            B0 = 1.09223 - 2.48245 * w
            B1 = -(3.434 - 7.14 * w) * 0.0001
            B2 = -(7.2661 - 9.2561 * w) * 0.0000001
            C = ((12.8 - WK) * (10 - WK) / (10 * w)) ^ 2

            Cp = A0 + A1 * T + A2 * T ^ 2 - C * (B0 + B1 * T + B2 * T ^ 2) 'kJ/kg

            Return Cp

        End Function

        Shared Function sigma_bb(ByVal T As Double, ByVal Tb As Double, ByVal Tc As Double, ByVal Pc As Double) As Double

            'Estimativa da tensao interfacial pelo metodo de Brock e Bird

            Dim Tr, Tbr, Q, tmp, Pc_ As Double

            Pc_ = Pc / 100000 'bar

            Tr = T / Tc

            Tbr = Tb / Tc

            Q = 0.1196 * (1 + Tbr * Math.Log(Pc_ / 1.01325) / (1 - Tbr)) - 0.279

            tmp = Pc_ ^ (2 / 3) * Tc ^ (1 / 3) * Q * (1 - Tr) ^ (11 / 9)

            tmp = tmp / 1000 'N/m

            sigma_bb = tmp

        End Function

        Shared Function viscl_letsti(ByVal T As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double, ByVal MM As Double) As Double

            'Estimativa da viscosidade de liquidos pelo metodo de Letsou e Stiel

            Dim Tr As Double = T / Tc

            Pc = Pc / 100000

            Dim e0 As Double = (2.648 - 3.725 * Tr + 1.309 * Tr ^ 2) * 0.001

            Dim e1 As Double = (7.425 - 13.39 * Tr + 5.933 * Tr ^ 2) * 0.001

            Dim e As Double = 0.176 * (Tc / (MM ^ 3 * Pc ^ 4)) ^ (1 / 6)

            viscl_letsti = (e0 + e1) / e / 1000 'Pa.s

        End Function

        Shared Function viscg_lucas(ByVal T As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double, ByVal MM As Double) As Double

            'Estimativa da viscosidade de gases pelo metodo de Lucas

            Dim Tr As Double = T / Tc

            Pc = Pc / 100000

            Dim e As Double = 0.176 * (Tc / (MM ^ 3 * Pc ^ 4)) ^ (1 / 6)

            Dim tmp As Double = (1 / e) * (0.807 * Tr ^ 0.618 - 0.357 * Math.Exp(-0.449 * Tr) + 0.34 * Math.Exp(-4.058 * Tr) + 0.018)

            viscg_lucas = tmp / 1000000.0 * 100 / 1000 'Pa.s

        End Function

        Shared Function liq_dens_rackett(ByVal T As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double, ByVal MM As Double, Optional ByVal ZRa As Double = 0, Optional ByVal P As Double = 0, Optional ByVal Pvp As Double = 0) As Double

            'Calculo da densidade do liquido utilizando o metodo de Rackett

            If Double.IsNaN(Pvp) Then Pvp = 0.0#

            Dim R, Tr As Double

            R = 83.14

            Pc = Pc / 100000

            Tr = T / Tc
            If Tr > 0.99 Then Tr = 0.5 'estimation for supercritical gases solved in liquid phase

            If ZRa = 0.0# Then ZRa = 0.29056 - 0.08775 * w

            Dim tmp As Double = R * Tc / Pc * ZRa ^ (1 + (1 - Tr) ^ (2 / 7))

            If Pvp <> 0.0# And T < Tc Then

                'Modified HBT method - Thomson (para liquidos comprimidos)
                Dim a, b, c, d, e, f, g, h, j, k As Double

                a = -9.070217
                b = 62.45326
                d = -135.1102
                f = 4.79594
                g = 0.250047
                h = 1.14188
                j = 0.0861488
                k = 0.0344483

                e = Math.Exp(f + g * w + h * w ^ 2)
                c = j + k * w

                Dim beta As Double = Pc * 100000 * (-1 + a * (1 - Tr) ^ (1 / 3) + b * (1 - Tr) ^ (2 / 3) + d * (1 - Tr) + e * (1 - Tr) ^ (4 / 3))

                Dim tmpcor As Double

                tmpcor = tmp * (1 - c * Math.Log((beta + P) / (beta + Pvp)))

                liq_dens_rackett = 0.001 * MM / (tmpcor * 0.000001) 'kg/m3 ''''m3/mol

            Else

                liq_dens_rackett = 0.001 * MM / (tmp * 0.000001) 'kg/m3 ''''m3/mol

            End If

        End Function

        Shared Function JT_Goldzberg(ByVal T As Double, ByVal Tpc As Double, ByVal Ppc As Double, ByVal Cp As Double, ByVal fluido As Double, ByVal SG As Double)

            'T em °F
            T = 5 / 9 * (T + 459.7)
            'Tpc em °F
            Tpc = 5 / 9 * (Tpc + 459.7)
            'Ppc em lbf/in.2
            Ppc = Ppc / 6894.76
            'Cp em J/kg°F
            Cp = Cp * 1.8 / 1000
            'tipo como "V" ou "L"
            'SG = densidade absoluta ou densidade do liquido em lbm/ft3

            'JT em °F in.2/lbf

            'para gases

            Dim Tpr As Double = T / Tpc
            Dim eta As Double

            If fluido = "V" Then eta = 0.0048823 * Tpc * (18 / Tpr ^ 2 - 1) / (Ppc * Cp * SG)

            'para liquidos

            If fluido = "L" Then eta = -1 / (SG / 16.0185 * Cp)

            JT_Goldzberg = eta / 1.8 / 6894.76 '(K/Pa)

        End Function

        Shared Function condl_latini(ByVal T As Double, ByVal Tb As Double, ByVal Tc As Double, ByVal M As Double, ByVal Tipo As String) As Double

            Dim Tr, A, A_, alpha, beta, lambda, gamma As Double

            Tr = T / Tc

            'Hidrocarbonetos saturados (Tipo = "H")

            A_ = 0.0035
            alpha = 1.2
            beta = 0.5
            gamma = 0.167

            If Tipo = "O" Then
                'Oleofinas

                A_ = 0.0361
                alpha = 1.2
                beta = 1
                gamma = 0.167

            ElseIf Tipo = "C" Then
                'Cicloparafinas

                A_ = 0.031
                alpha = 1.2
                beta = 1.0
                gamma = 0.167

            ElseIf Tipo = "A" Then
                'Aromaticos

                A_ = 0.0346
                alpha = 1.2
                beta = 1.0
                gamma = 0.167

            ElseIf Tipo = "X" Then
                'Outros (ex. agua)

                A_ = 0.494
                alpha = 0
                beta = 0.5
                gamma = -0.167

            End If

            A = A_ * Tb ^ alpha / (M ^ beta * Tc ^ gamma)
            lambda = A * (1 - Tr) ^ 0.38 / Tr ^ (1 / 6)

            If T / Tc > 0.98 Then lambda = 0.0#

            condl_latini = lambda   'W/(m.K)

        End Function

        Shared Function condtg_elyhanley(ByVal T As Double, ByVal Tc As Double, ByVal Vc As Double, ByVal Zc As Double, ByVal w As Double, ByVal M As Double, ByVal Cv As Double)

            Dim Tr, Tplus, teta, omega, f, h, T0, eta0, lambda0, C(8), lambda_, eta_, Hgrande

            Tr = T / Tc

            M = M / 1000

            If Tr <= 2 Then

                Tplus = Tr

            Else

                Tplus = 2

            End If

            teta = 1 + (w - 0.011) * (0.56553 - 0.86276 * Math.Log(Tplus) - 0.69852 / Tplus)

            omega = (1 + (w - 0.011) * (0.3856 - 1.1617 * Math.Log(Tplus))) * 0.288 / Zc

            f = Tc * teta / 190.4

            h = Vc * omega / 99.2 * 1000000.0

            T0 = T / f

            C(0) = 2907740.0
            C(1) = -3312870.0
            C(2) = 1608100.0
            C(3) = -433190.0
            C(4) = 70624.8
            C(5) = -7116.62
            C(6) = 432.517
            C(7) = -14.4591
            C(8) = 0.203712

            Dim i = 0

            eta0 = 0.0#

            Do

                eta0 = eta0 + 0.0000001 * C(i) * T0 ^ ((i - 3) / 3)

                i = i + 1

            Loop Until i = 9

            'eta0 = viscg_lucas(T, 190.6, 45.4 * 101325, 0.012, 16)

            lambda0 = 1944 * eta0

            Hgrande = (0.01604 / M) ^ 0.5 * f ^ 0.5 / h ^ (2 / 3)

            lambda_ = lambda0 * Hgrande

            eta_ = eta0 * Hgrande * M / (0.01604)

            condtg_elyhanley = lambda_ + (eta_ / M) * 1.32 * (Cv - 3 * 8.314 / 2) 'W/(m.K)

        End Function

        Shared Function Tcm(ByVal Vz, ByVal VTc) As Double

            Dim i, n As Integer

            n = UBound(Vz)

            i = 0

            Tcm = 0.0#

            Do

                Tcm += Vz(i) * VTc(i)

                i = i + 1

            Loop Until i = n + 1

        End Function

        Shared Function Pcm(ByVal Vz, ByVal VPc) As Double

            Dim i, n As Integer

            n = UBound(Vz)

            i = 0

            Pcm = 0.0#

            Do

                Pcm += Vz(i) * VPc(i)

                i = i + 1

            Loop Until i = n + 1

        End Function

        Shared Function Vcm(ByVal Vz, ByVal VVc) As Double

            Dim i, j, n As Integer

            n = UBound(Vz)

            i = 0
            Vcm = 0.0#
            Do
                j = 0
                Do
                    Vcm += Vz(i) * Vz(j) * (0.5 * VVc(i) ^ (1 / 3) + 0.5 * VVc(j) ^ (1 / 3)) ^ 3
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

        End Function

        Shared Function Zcm(ByVal Vz, ByVal VZc) As Double

            Dim i, n As Integer

            n = UBound(Vz)

            i = 0

            Zcm = 0.0#

            Do

                Zcm += Vz(i) * VZc(i)

                i = i + 1

            Loop Until i = n + 1

        End Function

        Shared Function wm(ByVal Vz, ByVal Vw) As Double

            Dim i, n As Integer

            n = UBound(Vz)

            i = 0

            wm = 0.0#

            Do

                wm += Vz(i) * Vw(i)

                i = i + 1

            Loop Until i = n + 1

        End Function

        Shared Function MMm(ByVal Vz, ByVal VMM) As Double

            Dim i, n As Integer

            n = UBound(Vz)

            i = 0

            MMm = 0.0#

            Do

                MMm += Vz(i) * VMM(i)

                i = i + 1

            Loop Until i = n + 1

        End Function

        Shared Function viscg_jossi_stiel_thodos(ByVal eta0 As Double, ByVal T As Double, ByVal V As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal Vc As Double, ByVal MM As Double) As Double

            'Estimativa da viscosidade de gases em pressoes elevadas pelo 
            'metodo de Jossi-Stiel-Thodos

            Dim Tr As Double = T / Tc

            Pc = Pc / 101325

            Dim rho_r As Double = Vc / V

            Dim xit As Double = (Tc / MM ^ 3 / Pc ^ 4) ^ (1 / 6)

            eta0 = eta0 * 1000000.0 / 100 * 1000

            Dim tmp As Double = ((1.023 + 0.23364 * rho_r + 0.58533 * rho_r ^ 2 - 0.40758 * rho_r ^ 3 + 0.093324 * rho_r ^ 4) ^ 4 - 1) / xit + eta0

            viscg_jossi_stiel_thodos = tmp / 10000 / 1000 'Pa.s

        End Function

        Shared Function Vc(ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double) As Double

            If Pc > 0.0# Then Vc = 8.314 * (0.291 - 0.08 * w) * Tc / Pc * 1000 Else Vc = 0.0# 'm3/kmol

        End Function

        Shared Function Vc(ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double, ByVal Zc As Double) As Double

            Vc = 8.314 * Zc * Tc / Pc * 1000 'm3/kmol

        End Function

        Shared Function Zc1(ByVal w As Double) As Double

            Zc1 = 0.291 - 0.08 * w

        End Function

        Shared Function condlm_li(ByVal VVl, ByVal Vcondl, ByVal Vz) As Double

            Dim n = UBound(Vz)

            Dim phi(n), somaz As Double, i, j As Integer

            somaz = 0.0#
            i = 0
            Do
                If Vz(i) <> 0 And Not Double.IsNaN(VVl(i)) Then somaz = somaz + Vz(i) * VVl(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                If Vz(i) <> 0 And Not Double.IsNaN(VVl(i)) Then phi(i) = Vz(i) * VVl(i) / somaz
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim condlm = 0.0#
            Do
                j = 0
                Do
                    If Vz(i) <> 0.0# And Vz(j) <> 0.0# Then condlm += phi(i) * phi(j) * (2 * (Vcondl(i) ^ -1 + Vcondl(j) ^ -1) ^ -1)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            condlm_li = condlm    'W/(m.K)

        End Function

        Shared Function CPig_pol(ByVal VCOEFF, ByVal T)

            Dim tmp2 As Double = VCOEFF(0) + VCOEFF(1) * T + VCOEFF(2) * T ^ 2 + VCOEFF(3) * T ^ 3 + VCOEFF(4) * T ^ 4

            CPig_pol = tmp2

        End Function

        Shared Function Vc(ByVal T, ByVal Tc, ByVal Tb, ByVal Pc)

            Pc = Pc / 101325
            Dim R As Double = 82.0575
            Dim s As Double = Tb / Tc * Math.Log(Pc) / (1 - Tb / Tc)
            Dim tmp As Double = (R * Tc / Pc) * (0.3883 - 0.0179 * s)

            Vc = tmp * 0.000001

        End Function

        Shared Function oilvisc_twu(ByVal T, ByVal T1, ByVal T2, ByVal v1, ByVal v2)

            'v = m2/s, T = K

            Dim Z, Z1, Z2, B, vk1, vk2 As Double

            vk1 = v1 * 1000000.0
            vk2 = v2 * 1000000.0

            T = 1.8 * T
            T1 = 1.8 * T1
            T2 = 1.8 * T2

            Z1 = vk1 + 0.7 + Math.Exp(-1.47 - 1.84 * vk1 - 0.51 * vk1 ^ 2)
            Z2 = vk2 + 0.7 + Math.Exp(-1.47 - 1.84 * vk2 - 0.51 * vk2 ^ 2)

            Dim var1 As Double = (Math.Log(Math.Log(Z1)) + Math.Log(Math.Log(Z2)))
            Dim var2 As Double = (Math.Log(T1) + Math.Log(T2))
            B = var1 / var2

            Z = Math.Exp(Math.Exp(Math.Log(Math.Log(Z1)) + B * (Math.Log(T) - Math.Log(T2))))

            Dim tmp = Z - 0.7 - Math.Exp(-0.7487 - 3.295 * (Z - 0.7) + 0.6119 * (Z - 0.7) ^ 2 - 0.3193 * (Z - 0.7) ^ 3)

            oilvisc_twu = tmp * 0.000001 'm2/s

        End Function

        Shared Function CpCvR(ByVal TIPO As String, ByVal T As Double, ByVal P As Double, ByVal Vz As Double(), ByVal VKij As Double(,), ByVal Vzmass As Double(), ByVal VTc As Double(), ByVal VPc As Double(), ByVal VCpig As Double(), ByVal VMM As Double(), ByVal Vw As Double(), ByVal VZRa As Double()) As Double()

            Dim dadt, n, R, Z As Double
            Dim i, j As Integer

            n = UBound(Vz)

            Dim ai(n), bi(n), ci(n), a(n, n), b(n, n) As Double
            Dim Tc(n), Pc(n), Vc(n), Zc(n), w(n), alpha(n), m(n), Tr(n) As Double

            R = 8.314

            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                w(i) = Vw(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim MMm = 0.0#
            Do
                MMm += Vz(i) * VMM(i)
                i += 1
            Loop Until i = n + 1

            i = 0
            Do
                If Tc(i) > 0 Then
                    alpha(i) = (1 + (0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2) * (1 - (T / Tc(i)) ^ 0.5)) ^ 2
                    ai(i) = 0.45724 * alpha(i) * R ^ 2 * Tc(i) ^ 2 / Pc(i)
                    bi(i) = 0.0778 * R * Tc(i) / Pc(i)
                    ci(i) = 0.37464 + 1.54226 * w(i) - 0.26992 * w(i) ^ 2
                End If
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Do
                j = 0
                Do
                    a(i, j) = (ai(i) * ai(j)) ^ 0.5 * (1 - VKij(i, j))
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim am = 0.0#
            Do
                j = 0
                Do
                    am = am + Vz(i) * Vz(j) * a(i, j)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim bm = 0.0#
            Do
                bm = bm + Vz(i) * bi(i)
                i = i + 1
            Loop Until i = n + 1

            'Dim dadT = 

            Dim AG1 = am * P / (R * T) ^ 2
            Dim BG1 = bm * P / (R * T)

            Dim coeff(3) As Double

            coeff(0) = -AG1 * BG1 + BG1 ^ 2 + BG1 ^ 3
            coeff(1) = AG1 - 3 * BG1 ^ 2 - 2 * BG1
            coeff(2) = BG1 - 1
            coeff(3) = 1

            Dim temp1 As Double(,) = Poly_Roots(coeff)
            Dim tv, tv2 As Double

            If Not IsNumeric(temp1) Then

                If temp1(0, 0) > temp1(1, 0) Then
                    tv = temp1(1, 0)
                    tv2 = temp1(1, 1)
                    temp1(1, 0) = temp1(0, 0)
                    temp1(0, 0) = tv
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

                If TIPO = "L" Then
                    Z = temp1(0, 0)
                    If temp1(0, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(2, 0)
                        End If
                    End If
                    If Z < 0 Then Z = temp1(1, 0)
                ElseIf TIPO = "V" Then
                    Z = temp1(2, 0)
                    If temp1(2, 1) <> 0 Then
                        Z = temp1(1, 0)
                        If temp1(1, 1) <> 0 Then
                            Z = temp1(0, 0)
                        End If
                    End If
                End If

            Else

                Dim findZV, dfdz, zant As Double
                If TIPO = "V" Then Z = 1 Else Z = 0.05
                Do
                    findZV = coeff(3) * Z ^ 3 + coeff(2) * Z ^ 2 + coeff(1) * Z + coeff(0)
                    dfdz = 3 * coeff(3) * Z ^ 2 + 2 * coeff(2) * Z + coeff(1)
                    zant = Z
                    Z = Z - findZV / dfdz
                    If Z < 0 Then Z = 1
                Loop Until Math.Abs(findZV) < 0.0001 Or Double.IsNaN(Z)


            End If

            Dim V As Double = (Z * R * T / P) ' m3/mol

            Dim tmp1 As Double = MMm / V / 1000

            Dim aux1 As Double = -R / 2 * (0.45724 / T) ^ 0.5
            i = 0
            Dim aux2 As Double = 0.0#
            Do
                j = 0
                Do
                    If Vz(i) > 0.0# And Vz(j) > 0 And Tc(i) > 0.0# And Tc(j) > 0.0# Then aux2 += Vz(i) * Vz(j) * (1 - VKij(i, j)) * (ci(j) * (ai(i) * Tc(j) / Pc(j)) ^ 0.5 + ci(i) * (ai(j) * Tc(i) / Pc(i)) ^ 0.5)
                    j = j + 1
                Loop Until j = n + 1
                i = i + 1
            Loop Until i = n + 1

            dadt = aux1 * aux2

            Dim d2adt2 As Double = R / 4 * (0.45724 / T) ^ 0.5 * (1 / T) * aux2
            'Dim d2adt2 = 0.169049 * R / (T ^ (3 / 2))

            Dim dP_dT_V As Double = R / (V - bm) - dadt / (V ^ 2 + 2 * bm * V - bm ^ 2)

            Dim dV_dT_P As Double = dP_dT_V / (R * T / (V - bm) ^ 2 - am * (2 * V + 2 * bm) / (V * (V + bm) + bm * (V - bm)) ^ 2)

            Dim dP_dV_T As Double = -R * T * (V - bm) ^ -2 - am * (V ^ 2 + 2 * bm * V - bm ^ 2) ^ -2 * (2 * V + 2 * bm)

            Dim d2P_dT2 As Double = -1 / (V ^ 2 + 2 * bm * V - bm ^ 2) * d2adt2

            Dim var As Double = (bm + V) / (2 ^ 0.5 * bm)

            Dim Int_d2P_dT2_V_dV As Double = -d2adt2 * Math.Log((-(2 ^ 0.5) * bm + bm + V) / ((2 ^ 0.5) * bm + bm + V)) / (8 ^ 0.5 * bm)

            Dim Cpm_ig As Double = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i) * MMm
                i += 1
            Loop Until i = n + 1

            Dim Cv As Double = T * Int_d2P_dT2_V_dV + Cpm_ig - 2 * R - T * dP_dT_V ^ 2 / dP_dV_T
            'Dim Cp = Cpm_ig + T * Int_d2P_dT2_V_dV - T * dP_dT_V ^ 2 / dP_dV_T - R
            Dim Cp As Double = Cpm_ig - R + T * dP_dT_V * dV_dT_P - T * d2adt2 / (8 ^ 0.5 * bm) * Math.Log((V + (1 - 2 ^ 0.5) * bm) / (V + (1 + 2 ^ 0.5) * bm))

            Dim Cp_Cv2 As Double = Cp / Cv

            Dim Cp_Cv As Double = 1 - (T * dP_dT_V ^ 2 / dP_dV_T) / (Cpm_ig - R + T * Int_d2P_dT2_V_dV)
            'Cv = Cp / Cp_Cv

            Dim tmp(2) As Double
            tmp(0) = Cp_Cv2
            tmp(1) = Cp / MMm
            tmp(2) = Cv / MMm

            CpCvR = tmp

        End Function

        Shared Function JT_PR(ByVal TIPO, ByVal T, ByVal P, ByVal Vz, ByVal Vzmass, ByVal VTc, ByVal VPc, ByVal VCpig, ByVal VMM, ByVal Vw, ByVal VZRa)

            Dim n, R, Cpm_ig As Double
            Dim vetor(8) As Double
            Dim Tc(), Pc(), Vc(), W(), Zc(), a, b, c, Tr() As Double

            n = UBound(Vz)

            ReDim Zc(n), Tc(n), Pc(n), Vc(n), W(n), Tr(n)

            R = 8.314

            Dim i, j As Integer
            i = 0
            Do
                Tc(i) = VTc(i)
                Tr(i) = T / Tc(i)
                Pc(i) = VPc(i)
                W(i) = Vw(i)
                Vc(i) = Zc1(W(i)) * R * Tc(i) / Pc(i)
                i = i + 1
            Loop Until i = n + 1

            i = 0
            Dim Vcm = 0.0#
            Dim wm = 0.0#
            Dim Zcm = 0.0#
            Dim MMm = 0.0#
            Dim ZRam = 0.0#
            Do
                If Vz(i) <> 0 Then
                    Vcm += Vz(i) * Vc(i)
                    wm += Vz(i) * W(i)
                    Zcm += Vz(i) * Zc1(W(i))
                    MMm += Vz(i) * VMM(i)
                    ZRam = Vz(i) * VZRa(i)
                End If
                i += 1
            Loop Until i = n + 1

            i = 0
            Dim Tcm = 0.0#
            Do
                j = 0
                Do
                    If Vz(i) <> 0 And Vz(j) <> 0 Then Tcm += Vz(i) * Vz(j) * (Tc(i) * Tc(j)) ^ 0.5
                    j += 1
                Loop Until j = n + 1
                i += 1
            Loop Until i = n + 1

            Dim Pcm = Zcm * R * Tcm / (Vcm)

            Dim V = 0.0#
            If TIPO = "L" Then

                'V = (Z_PR(T, P, Vz, VTc, VPc, Vw, "L") * R * T / P) * 1000 ' m3/kgmol

            ElseIf TIPO = "V" Then

                'V = (Z_PR(T, P, Vz, VTc, VPc, Vw, "V") * R * T / P) * 1000 ' m3/kgmol

            End If

            a = 0.45724 * R ^ 2 * Tcm ^ 2 / Pcm
            b = 0.0778 * R * Tcm / Pcm
            c = 0.37464 + 1.54226 * wm - 0.26992 * wm ^ 2

            Dim Trm = T / Tcm
            Dim AG = -b * R * T
            Dim BG = -2 * a * T * (1 + c - c * Trm ^ 0.5) * (-0.5 * c * Trm ^ 0.5)
            Dim CG = a * (1 + c - c * Trm ^ 0.5) ^ 2

            Dim dP_dT_V = R / (V - b) - (a * (1 + c) * c * Tcm ^ -0.5 * T ^ -0.5 + a * c ^ 2 * Tcm ^ -1) / (V ^ 2 + 2 * b * V - b ^ 2)

            Dim dP_dV_T = -R * T / (V - b) ^ 2 + (2 * b + 2 * V) * (a * (1 + c) ^ 2 + 2 * a * (1 + c) * c * Tcm ^ -0.5 * T ^ 0.5 + a * c ^ 2 * Tcm ^ -1 * T) / (V ^ 2 + 2 * b * V - b ^ 2) ^ 2

            Cpm_ig = 0.0#
            i = 0
            Do
                Cpm_ig += Vzmass(i) * VCpig(i)
                i += 1
            Loop Until i = n + 1

            Dim JT = -(T * dP_dT_V / dP_dV_T + V) / (Cpm_ig * MMm)

            JT_PR = JT

        End Function

        Shared Function Pvp_leekesler(ByVal T As Double, ByVal Tc As Double, ByVal Pc As Double, ByVal w As Double)

            Dim tmp, f0, f1 As Double

            Dim Tr As Double = T / Tc

            f0 = 5.92714 - 6.09648 / Tr - 1.28862 * Math.Log(Tr) + 0.169347 * Tr ^ 6
            f1 = 15.2518 - 15.6875 / Tr - 13.4721 * Math.Log(Tr) + 0.43577 * Tr ^ 6

            tmp = Pc * Math.Exp(f0 + w * f1)

            Pvp_leekesler = tmp

        End Function

        Shared Function mu_ml(ByVal mu_aq As Double, ByVal mu_ol As Double, ByVal xv_ol As Double) As Double

            mu_ml = 0.0#

            If xv_ol >= 0.5 Then

                mu_ml = mu_ol * Math.Exp(3.6 * (1 - xv_ol))

            ElseIf xv_ol < 0.33 Then

                mu_ml = (1 + 2.5 * xv_ol * (mu_ol + 0.4 * mu_aq) / (mu_ol + mu_aq)) * mu_aq

            Else

                mu_ml = xv_ol * mu_ol * Math.Exp(3.6 * (1 - xv_ol)) + (1 - xv_ol) * (1 + 2.5 * xv_ol * (mu_ol + 0.4 * mu_aq) / (mu_ol + mu_aq)) * mu_aq

            End If

        End Function

    End Class

End Namespace
