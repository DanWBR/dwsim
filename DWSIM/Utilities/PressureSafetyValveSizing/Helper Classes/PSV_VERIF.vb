Namespace DWSIM.Utilities.PSV

    <System.Serializable()> Public Class Evaluation

        Function VERIF_PSV(ByVal TIPO As Object, ByVal L As Object, ByVal P As Object, ByVal BP As Object, ByVal G As Object, ByVal visc As Object, ByVal Kd As Object, ByVal Kc As Object, ByVal OP_P As Object, ByVal T As Object, ByVal Z As Object, ByVal M As Object, ByVal k As Object, ByVal Kb As Object, ByVal x As Object, ByVal xo As Object, ByVal rhog As Object, ByVal rhom As Object, ByVal rhom90 As Object) As Object
            Dim Ql As Object = 0.0#
            Dim Qa As Object = 0.0#
            Dim Qo As Object = 0.0#
            Dim Qg As Object = 0.0#
            Dim A As Object = 0.0#

            Dim tmp As Object

            'TIPO = Fluido aliviado pela PSV
            'L = Letra do orificio da PSV
            'visc em cP
            'G em kg/m3
            'OP_P em % (sobrepressao em %)
            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'A em in.2
            'visc em cP
            'G em kg/m3
            'T em K
            'x = fracao massica de gas
            'rhog = densidade do gas em kg/m3
            'rhom = densidade da mistura em kg/m3
            'rhom90 = densidade da mistura em kg/m3 a 90% de P
            'xo = fracao volumetrica de oleo na Phase liquida

            A = AREA_API(L)

            If TIPO = "G" Then

                Qg = PSV_G_V(P, BP, T, A, Z, M, k, Kd, Kb, Kc)
                Qo = 0
                Qa = 0

            ElseIf TIPO = "LCC" Then

                Ql = PSV_LCC_V(A, P, BP, G, visc, Kd, Kc)
                Qg = 0
                Qo = xo * Ql
                Qa = (1 - xo) * Ql

            ElseIf TIPO = "LNCC" Then

                Ql = PSV_LNCC_V(A, P, BP, G, visc, Kd, Kc, OP_P)
                Qg = 0
                Qo = xo * Ql
                Qa = (1 - xo) * Ql

            ElseIf TIPO = "GL" Then

                tmp = PSV_GL_D23_V(x, xo, rhog, rhom, rhom90, P, BP, A, Kd, Kb, Kc)
                Qg = tmp(0)
                Qo = tmp(1)
                Qa = tmp(2)

            End If

            VERIF_PSV = New Object() {Qg, Qo, Qa}

        End Function

        Function PSV_LNCC_V(ByVal A As Object, ByVal P As Object, ByVal BP As Object, ByVal G As Object, ByVal visc As Object, ByVal Kd As Object, ByVal Kc As Object, ByVal OP_P As Object) As Object

            Dim Qant As Object
            Dim Kv As Object

            Dim Q, R, Kp As Object

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'A em in.2
            'visc em cP
            'G em kg/m3
            'OP_P em % (sobrepressao em %)

            P = (P + 1.033) * 101.325 / 1.033 - 101.325
            BP = (BP + 1.033) * 101.325 / 1.033 - 101.325

            G = G / 1000

            If OP_P < 25 Then

                Kp = 0.00009 * OP_P ^ 3 - 0.006 * OP_P ^ 2 + 0.1457 * OP_P - 0.35

            Else

                Kp = 0.004 * OP_P + 0.9

            End If

            Kv = 1.0#
            Q = (A / 0.00155 / 11.78) * (Kd * Kc * Kv * Kp) / (G / (1.25 * P - BP)) ^ 0.5

            Do

                R = Q * (18800 * G) / (visc * (A / 0.00155) ^ 0.5)
                Kv = (0.9935 + 2.878 / R ^ 0.5 + 342.75 / R ^ 1.5) ^ -1
                Qant = Q

            Loop Until System.Math.Abs(Q - Qant) < 0.000001

            Q = 24 / 16.6667 * Q

            PSV_LNCC_V = Q

        End Function

        Function PSV_LCC_V(ByVal A As Object, ByVal P As Object, ByVal BP As Object, ByVal G As Object, ByVal visc As Object, ByVal Kd As Object, ByVal Kc As Object) As Object
            Dim Qant As Object
            Dim Kv As Object

            Dim Q, R As Object

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'A em in.2
            'visc em cP
            'G em kg/m3

            P = (P + 1.033) * 101.325 / 1.033 - 101.325
            BP = (BP + 1.033) * 101.325 / 1.033 - 101.325

            G = G / 1000

            Kv = 1.0#
            Q = (A / 0.00155 / 11.78) * (Kd * Kc * Kv) / (G / (1.25 * P - BP)) ^ 0.5

            Do

                R = Q * (18800 * G) / (visc * (A / 0.00155) ^ 0.5)

                Kv = (0.9935 + 2.878 / R ^ 0.5 + 342.75 / R ^ 1.5) ^ -1

                Qant = Q
                Q = (A / 0.00155 / 11.78) * (Kd * Kc * Kv) / (G / (P - BP)) ^ 0.5

            Loop Until System.Math.Abs(Q - Qant) < 0.000001

            Q = 24 / 16.6667 * Q

            PSV_LCC_V = Q

        End Function

        Function PSV_G_V(ByVal P As Object, ByVal BP As Object, ByVal T As Object, ByVal A As Object, ByVal Z As Object, ByVal M As Object, ByVal k As Object, ByVal Kd As Object, ByVal Kb As Object, ByVal Kc As Object) As Object
            Dim Q As Object
            Dim RHO As Object

            Dim W, Pc, c, F2 As Object
            Dim R As Double

            'P em kgf/cm²_g
            'BP em kgf/cm²_g
            'T em K
            'A em in.2

            P = (P + 1.033) * 101.325 / 1.033
            BP = (BP + 1.033) * 101.325 / 1.033

            Pc = P * (2 / (k + 1)) ^ (k / (k - 1))

            If BP <= Pc Then

                c = 520 * (k * (2 / (k + 1)) ^ ((k + 1) / (k - 1))) ^ 0.5
                W = A / 0.00155 / 13160 / (T * Z / M) ^ 0.5 * (c * Kd * P * Kb * Kc)

            Else

                R = BP / P
                F2 = ((k / (k - 1)) * (R) ^ (2 / k) * ((1 - (R) ^ ((k - 1) / k)) / (1 - (R)))) ^ 0.5
                W = A / 0.00155 / 17.9 * (F2 * Kd * Kc) / (Z * T / (M * P * (P - BP))) ^ 0.5

            End If

            RHO = M / (8314 * Z * T / P) * 1000
            Q = (W / RHO) * 24

            PSV_G_V = Q

        End Function

        Function PSV_GL_D23_V(ByVal x As Object, ByVal xo As Object, ByVal rhog As Object, ByVal rhom As Object, ByVal rhom90 As Object, ByVal P As Object, ByVal BP As Object, ByVal A As Object, ByVal Kd As Object, ByVal Kb As Object, ByVal Kc As Object) As Object
            Dim Qa_vol As Object
            Dim Qo_vol As Object
            Dim Qg_vol As Object
            Dim Q As Object
            Dim eta_c As Object
            Dim alpha0 As Object
            Dim v9 As Object
            Dim v0 As Object
            Dim vg0 As Object

            Dim G, Pc, W As Object

            'x = fracao massica de gas
            'rhog = densidade do gas em kg/m3
            'rhom = densidade da mistura em kg/m3
            'rhom90 = densidade da mistura em kg/m3 a 90% de P
            'P = pressao de alivio em kgf/cm²_g
            'BP = contrapressao em kgf/cm²_g
            'Q = vazao da mistura em kg/h
            'A = area em in.2
            'xo = fracao volumetrica de oleo na Phase liquida

            vg0 = (1 / rhog) * 16.0185
            v0 = (1 / rhom) * 16.0185
            v9 = (1 / rhom90) * 16.0185
            P = (P + 1.033) * 14.22
            BP = (BP + 1.033) * 14.22

            alpha0 = x * vg0 / v0
            W = 9 * (v9 / v0 - 1)

            eta_c = 0
            Do
                eta_c = eta_c + 0.00001
            Loop Until System.Math.Abs(eta_c ^ 2 + (W ^ 2 - 2 * W) * (1 - eta_c) ^ 2 + 2 * W ^ 2 * System.Math.Log(eta_c) + 2 * W ^ 2 * (1 - eta_c)) < 0.00001

            Pc = eta_c * P

            If Pc >= BP Then

                G = 68.09 * eta_c * (P / (v0 * W)) ^ 0.5

            Else

                G = 68.09 * (-2 * (W / System.Math.Log(BP / P) + (W - 1) * (1 - BP / P))) ^ 0.5 * (P / v0) ^ 0.5 / (W * (P / BP - 1) + 1)

            End If

            Q = A / 0.04 * (Kd * Kb * Kc * G)
            Q = Q / 2.20462
            Qg_vol = x * Q / rhog * 24
            Qo_vol = xo * (Q / rhom * 24 - Qg_vol)
            Qa_vol = (1 - xo) * (Q / rhom * 24 - Qg_vol)

            PSV_GL_D23_V = New Object() {Qg_vol, Qo_vol, Qa_vol}

        End Function

        Function AREA_API(ByVal L As String) As Double

            Dim an As Object = ""

            If L <> "" Then

                If L = "D" Then
                    an = 0.11
                ElseIf L = "E" Then
                    an = 0.196
                ElseIf L = "F" Then
                    an = 0.307
                ElseIf L = "G" Then
                    an = 0.503
                ElseIf L = "H" Then
                    an = 0.785
                ElseIf L = "J" Then
                    an = 1.287
                ElseIf L = "K" Then
                    an = 1.838
                ElseIf L = "L" Then
                    an = 2.853
                ElseIf L = "M" Then
                    an = 3.6
                ElseIf L = DWSIM.App.GetLocalString("N") Then
                    an = 4.34
                ElseIf L = "P" Then
                    an = 6.38
                ElseIf L = "Q" Then
                    an = 11.05
                ElseIf L = "R" Then
                    an = 16
                ElseIf L = "T" Then
                    an = 26
                End If

            End If

            If L = "X" Then an = 0

            AREA_API = an

        End Function

    End Class

End Namespace
