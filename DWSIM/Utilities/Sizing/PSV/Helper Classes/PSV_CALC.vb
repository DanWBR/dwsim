Namespace DWSIM.Utilities.PSV

    <System.Serializable()> Public Class Sizing

        Sub New()

        End Sub

        Function PSV_LNCC_D(ByVal Q, ByVal P, ByVal BP, ByVal G, ByVal visc, ByVal Kd, ByVal Kc, ByVal OP_P)

            Dim A, R, A_API526, tmp, Kp, Kv

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'Q em m3/d
            'visc em cP
            'G em kg/m3
            'OP_P em % (sobrepressao em %)

            P = (P + 1.033) * 101.325 / 1.033 - 101.325
            BP = (BP + 1.033) * 101.325 / 1.033 - 101.325

            Q = 16.6667 / 24 * Q
            G = G / 1000

            If OP_P < 25 Then

                Kp = 0.00009 * OP_P ^ 3 - 0.006 * OP_P ^ 2 + 0.1457 * OP_P - 0.35

            Else

                Kp = 0.004 * OP_P + 0.9

            End If


            Kv = 1.0#
            A = 11.78 * Q / (Kd * Kc * Kv * Kp) * (G / (1.25 * P - BP)) ^ 0.5
            A = A * 0.00155

            Do

                tmp = ORIF_API(A)
                A_API526 = tmp(2)

                R = Q * (18800 * G) / (visc * (A_API526 / 0.00155) ^ 0.5)

                Kv = (0.9935 + 2.878 / R ^ 0.5 + 342.75 / R ^ 1.5) ^ -1

                A = 11.78 * Q / (Kd * Kc * Kv * Kp) * (G / (1.25 * P - BP)) ^ 0.5
                A = A * 0.00155

            Loop Until A <= A_API526

            PSV_LNCC_D = A

        End Function

        Function PSV_LCC_D(ByVal Q, ByVal P, ByVal BP, ByVal G, ByVal visc, ByVal Kd, ByVal Kc)

            Dim A, R, A_API526, tmp, Kv

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'Q em m3/d
            'visc em cP
            'G em kg/m3

            P = (P + 1.033) * 101.325 / 1.033 - 101.325
            BP = (BP + 1.033) * 101.325 / 1.033 - 101.325

            Q = 16.6667 / 24 * Q
            G = G / 1000

            Kv = 1.0#
            A = 11.78 * Q / (Kd * Kc * Kv) * (G / (P - BP)) ^ 0.5
            A = A * 0.00155

            Do

                tmp = ORIF_API(A)
                A_API526 = tmp(2)

                R = Q * (18800 * G) / (visc * (A_API526 / 0.00155) ^ 0.5)

                Kv = (0.9935 + 2.878 / R ^ 0.5 + 342.75 / R ^ 1.5) ^ -1

                A = 11.78 * Q / (Kd * Kc * Kv) * (G / (P - BP)) ^ 0.5
                A = A * 0.00155

            Loop Until A <= A_API526

            PSV_LCC_D = A

        End Function

        Function PSV_G_D(ByVal P, ByVal BP, ByVal T, ByVal W, ByVal Z, ByVal M, ByVal k, ByVal Kd, ByVal Kb, ByVal Kc)

            Dim Pc, c, A, F2, R As Double

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'T em K
            'W em kg/h

            P = (P + 1.033) * 101.325 / 1.033
            BP = (BP + 1.033) * 101.325 / 1.033

            Pc = P * (2 / (k + 1)) ^ (k / (k - 1))



            If BP <= Pc Then

                c = 520 * (k * (2 / (k + 1)) ^ ((k + 1) / (k - 1))) ^ 0.5
                A = 13160 * W * (T * Z / M) ^ 0.5 / (c * Kd * P * Kb * Kc)

            Else

                R = BP / P
                F2 = ((k / (k - 1)) * (R) ^ (2 / k) * ((1 - (R) ^ ((k - 1) / k)) / (1 - (R)))) ^ 0.5
                A = 17.9 * W / (F2 * Kd * Kc) * (Z * T / (M * P * (P - BP))) ^ 0.5

            End If

            A = A * 0.00155

            PSV_G_D = A

        End Function

        Function PSV_GL_D23_D(ByVal x, ByVal rhog, ByVal rhom, ByVal rhom90, ByVal P, ByVal BP, ByVal Q, ByVal Kd, ByVal Kb, ByVal Kc)

            Dim Pc, G, W, vg0, v0, v9, alpha0, eta_c, A

            'x = fracao massica de gas
            'rhog = densidade do gas em kg/m3
            'rhom = densidade da mistura em kg/m3
            'rhom90 = densidade da mistura em kg/m3 a 90% de P
            'P = pressao de alivio em kgf/cm²_g
            'BP = contrapressao em kgf/cm²_g
            'W = vazao da mistura em kg/h

            vg0 = (1 / rhog) * 16.0185
            v0 = (1 / rhom) * 16.0185
            v9 = (1 / rhom90) * 16.0185
            P = (P + 1.033) * 14.22
            BP = (BP + 1.033) * 14.22
            Q = Q * 2.20462

            alpha0 = x * vg0 / v0
            W = 9 * (v9 / v0 - 1)

            'metodo de Brent para encontrar eta_c

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = 10000
            Dim iter2 As Integer

            aaa = 0
            bbb = 10
            ccc = 10

            faa = aaa ^ 2 + (W ^ 2 - 2 * W) * (1 - aaa) ^ 2 + 2 * W ^ 2 * Math.Log(aaa) + 2 * W ^ 2 * (1 - aaa)
            fbb = bbb ^ 2 + (W ^ 2 - 2 * W) * (1 - bbb) ^ 2 + 2 * W ^ 2 * Math.Log(bbb) + 2 * W ^ 2 * (1 - bbb)
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
                tol11 = 0.00000001
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
                fbb = bbb ^ 2 + (W ^ 2 - 2 * W) * (1 - bbb) ^ 2 + 2 * W ^ 2 * Math.Log(bbb) + 2 * W ^ 2 * (1 - bbb)
                iter2 += 1
            Loop Until iter2 = ITMAX2

Final3:     eta_c = bbb

            Pc = eta_c * P

            If Pc >= BP Then

                G = 68.09 * eta_c * (P / (v0 * W)) ^ 0.5

            Else

                G = 68.09 * (-2 * (W / Math.Log(BP / P) + (W - 1) * (1 - BP / P))) ^ 0.5 * (P / v0) ^ 0.5 / (W * (P / BP - 1) + 1)

            End If

            A = 0.04 * Q / (Kd * Kb * Kc * G)

            Dim tmp(7)

            tmp(0) = A
            tmp(1) = x
            tmp(2) = vg0
            tmp(3) = v0
            tmp(4) = v9
            tmp(5) = alpha0
            tmp(6) = W
            tmp(7) = G

            PSV_GL_D23_D = tmp

        End Function

        Function ORIF_API(ByVal A)

            Dim an As Double = 0
            Dim L As String = ""

            If A <= 0.11 Then
                L = "D"
                an = 0.11
            ElseIf A <= 0.196 Then
                L = "E"
                an = 0.196
            ElseIf A <= 0.307 Then
                L = "F"
                an = 0.307
            ElseIf A <= 0.503 Then
                L = "G"
                an = 0.503
            ElseIf A <= 0.785 Then
                L = "H"
                an = 0.785
            ElseIf A <= 1.287 Then
                L = "J"
                an = 1.287
            ElseIf A <= 1.838 Then
                L = "K"
                an = 1.838
            ElseIf A <= 2.853 Then
                L = "L"
                an = 2.853
            ElseIf A <= 3.6 Then
                L = "M"
                an = 3.6
            ElseIf A <= 4.34 Then
                L = DWSIM.App.GetLocalString("N")
                an = 4.34
            ElseIf A <= 6.38 Then
                L = "P"
                an = 6.38
            ElseIf A <= 11.05 Then
                L = "Q"
                an = 11.05
            ElseIf A <= 16 Then
                L = "R"
                an = 16
            ElseIf A <= 26 Then
                L = "T"
                an = 26
            End If

            Dim tmp(2)

            tmp(1) = L
            tmp(2) = an

            ORIF_API = tmp

        End Function

    End Class

End Namespace
