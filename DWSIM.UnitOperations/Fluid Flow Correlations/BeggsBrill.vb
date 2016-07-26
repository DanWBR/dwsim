'    Beggs and Brill Pressure Drop Calculation Routine
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

Namespace FlowPackages

    <Serializable()> Public Class BeggsBrill

        Inherits FPBaseClass

        Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double

            'mu = mu * 0.001
            NRe = rho * v * D / mu

        End Function

        Public Overrides Function CalculateDeltaP(ByVal D As Object, ByVal L As Object, ByVal deltaz As Object, ByVal k As Object, ByVal qv As Object, ByVal ql As Object, ByVal muv As Object, ByVal mul As Object, ByVal rhov As Object, ByVal rhol As Object, ByVal surft As Object) As Object
            'Function PA_DP(ByVal D, ByVal L, ByVal Z, ByVal epsilon, ByVal QG, ByVal QL, ByVal mu_g, ByVal mu_l, ByVal rho_g, ByVal rho_l, ByVal sigma)

            CalculateDeltaP = Nothing

            Dim ResVector(4) As Object

            If qv = 0 Then

                ql = ql / 3600 / 24
                Dim vlo = ql / (Math.PI * D ^ 2 / 4)
                mul = 0.001 * mul
                Dim Re_fit = NRe(rhol, vlo, D, mul)
                Dim fric = 0.0#
                If Re_fit > 3250 Then
                    Dim a1 = Math.Log(((k / D) ^ 1.1096) / 2.8257 + (7.149 / Re_fit) ^ 0.8961) / Math.Log(10.0#)
                    Dim b1 = -2 * Math.Log((k / D) / 3.7065 - 5.0452 * a1 / Re_fit) / Math.Log(10.0#)
                    fric = (1 / b1) ^ 2
                Else
                    fric = 64 / Re_fit
                End If

                Dim dPl = fric * L / D * vlo ^ 2 / 2 * rhol
                Dim dPh = rhol * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                ResVector(0) = "Liquid Only"
                ResVector(1) = 1
                ResVector(2) = dPl
                ResVector(3) = dPh
                ResVector(4) = dPl + dPh

                CalculateDeltaP = ResVector

            ElseIf ql = 0 Then

                qv = qv / 3600 / 24
                Dim vgo = qv / (Math.PI * D ^ 2 / 4)
                muv = 0.001 * muv
                Dim Re_fit = NRe(rhov, vgo, D, muv)
                Dim fric = 0.0#
                If Re_fit > 3250 Then
                    Dim a1 = Math.Log(((k / D) ^ 1.1096) / 2.8257 + (7.149 / Re_fit) ^ 0.8961) / Math.Log(10.0#)
                    Dim b1 = -2 * Math.Log((k / D) / 3.7065 - 5.0452 * a1 / Re_fit) / Math.Log(10.0#)
                    fric = (1 / b1) ^ 2
                Else
                    fric = 64 / Re_fit
                End If

                Dim dPl = fric * L / D * vgo ^ 2 / 2 * rhov
                Dim dPh = rhov * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                ResVector(0) = "Vapor Only"
                ResVector(1) = 0
                ResVector(2) = dPl
                ResVector(3) = dPh
                ResVector(4) = dPl + dPh

                CalculateDeltaP = ResVector

            Else


                'qv =  vazao de gas em m3/d reais
                'ql = vazao de liquido em m3/d reais
                'muv = viscosidade do gas em cP
                'mul = viscosidade do liquido em cP
                'rhov = densidade do gas em kg/m3
                'rhol = densidade do liquido em kg/m3
                'd = diametro da tubulacao em metros
                'l = comprimento da tubulacao em metros
                'deltaz = elevacao da tubulacao em metros
                'surft = tensao superficial em N/m
                'k = rugosidade do duto

                'diametro de metros para ft
                D = D * 3.28084

                'vazoes de m3/d para ft3/s
                qv = qv / 24 * 0.00980963
                ql = ql / 24 * 0.00980963

                'massas especificas de kg/m3 para lb/ft3
                rhov = rhov * 0.062428
                rhol = rhol * 0.062428

                'tensao superficial de N/m para dyn/cm
                surft = surft * 1000

                'distancias de metros para ft
                L = L * 3.28084
                deltaz = deltaz * 3.28084

                'inclinacao da tubulacao (graus)
                Dim teta = Math.Atan(deltaz / (L ^ 2 - deltaz ^ 2) ^ 0.5) * 180 / Math.PI

                'area da secao transversal (ft2)
                Dim ap = Math.PI * D ^ 2 / 4

                'velocidade da mistura (ft/s)
                Dim vm = (qv + ql) / ap

                'fracao volumetrica de liquido
                Dim Cl = ql / (qv + ql)

                'calculo do regime de fluxo

                Dim L1 = 316 * Cl ^ 0.302
                Dim L2 = 0.0009252 * Cl ^ -2.4684
                Dim L3 = 0.1 * Cl ^ -1.4516
                Dim L4 = 0.5 * Cl ^ -6.738

                'Numero de Froude p/ a mistura
                Dim Frm = vm ^ 2 / (32.2 * D)

                Dim fluxo As String
                fluxo = ("NA")
                If Cl < 0.01 And Frm < L1 Then fluxo = "Segregated"
                If Cl >= 0.01 And Frm < L2 Then fluxo = "Segregated"
                If Cl >= 0.01 And Cl < 0.4 And Frm > L3 And Frm <= L1 Then fluxo = "Intermittent"
                If Cl >= 0.4 And Frm > L3 And Frm <= L4 Then fluxo = "Intermittent"
                If Cl < 0.4 And Frm >= L1 Then fluxo = "Distributed"
                If Cl >= 0.4 And Frm > L4 Then fluxo = "Distributed"
                If Cl >= 0.01 And Frm > L2 And Frm < L3 Then fluxo = "Transition"

                'calculo do ("liquidholdup") horizontal
                Dim El_0, El_teta As Double
                If fluxo = "Segregated" Then
                    El_0 = 0.98 * Cl ^ 0.4846 / (Frm ^ 0.0868)
                ElseIf fluxo = "Intermittent" Then
                    El_0 = 0.845 * Cl ^ 0.5351 / (Frm ^ 0.0173)
                ElseIf fluxo = "Distributed" Then
                    El_0 = 1.065 * Cl ^ 0.5824 / (Frm ^ 0.0609)
                ElseIf fluxo = "Transition" Then
                    Dim AI = (L3 - Frm) / (L3 - L2)
                    Dim BI = 1 - AI
                    El_0 = AI * 0.98 * Cl ^ 0.4846 / (Frm ^ 0.0868) + BI * 0.845 * Cl ^ 0.5351 / (Frm ^ 0.0173)
                End If

                'calculo do fator de inclinacao
                Dim beta As Double
                Dim vsl = ql / ap
                Dim Nvl = 1.938 * vsl * (rhol / (32.2 * surft)) ^ 0.25

                If deltaz > 0 Then
                    If fluxo = "Segregated" Then
                        beta = (1 - Cl) * Math.Log(0.011 * Nvl ^ 3.539 / (Cl ^ 3.768 * Frm ^ 1.614))
                    ElseIf fluxo = "Intermittent" Then
                        beta = (1 - Cl) * Math.Log(2.96 * Cl ^ 0.305 * Frm ^ 0.0978 / Nvl ^ 0.4473)
                    ElseIf fluxo = "Distributed" Then
                        beta = 0
                    End If
                Else
                    beta = (1 - Cl) * Math.Log(4.7 * Nvl ^ 0.1244 / (Cl ^ 0.3692 * Frm ^ 0.5056))
                End If
                If beta < 0 Then beta = 0

                Dim B_teta = 1 + beta * (Math.Sin(1.8 * teta * Math.PI / 180) - 0.3333 * (Math.Sin(1.8 * teta * Math.PI / 180)) ^ 3)

                El_teta = El_0 * B_teta

                'calculo da densidade da mistura
                Dim rhom = rhol * El_teta + rhov * (1 - El_teta)

                'calculo do delta P em funcao da carga hidrostatica em lbf/ft2
                Dim dP_hh = rhom * deltaz

                'calculo do delta P em funcao da friccao
                Dim y = Math.Log(Cl / El_teta ^ 2)

                Dim S = 0.0#
                If y > 1 And y < 1.2 Then
                    S = Math.Log(2.2 * Math.Exp(y) - 1.2)
                Else
                    S = y / (-0.0523 + 3.182 * y - 0.8725 * y ^ 2 + 0.01853 * y ^ 4)
                End If

                'calculo do numero de Reynolds
                Dim rho_ns = Cl * rhol + (1 - Cl) * rhov
                Dim mu_ns = Cl * mul + (1 - Cl) * muv
                Dim NRe_ns = rho_ns * vm * D / (mu_ns * 0.00067197)

                'Dim k = 0.0018 ' Rugosidade do duto - aco carbono

                'calculo do fator de friccao
                Dim f_ns = 0.0#
                If NRe_ns > 3250 Then
                    Dim a = Math.Log(((k * 3.2808 / D) ^ 1.1096) / 2.8257 + (7.149 / NRe_ns) ^ 0.8961) / Math.Log(10.0#)
                    Dim b = -2 * Math.Log((k * 3.2808 / D) / 3.7065 - 5.0452 * a / NRe_ns) / Math.Log(10.0#)
                    f_ns = (1 / b) ^ 2
                Else
                    f_ns = 64 / NRe_ns
                End If

                'calculo do fator de friccao bifasico
                Dim f_tp = f_ns * Math.Exp(S)

                ' delta P devido a friccao em lbf/ft2
                'Dim dP_fr = 2 * f_tp * vm ^ 2 * rho_ns * L / (32.2 * D)
                Dim dP_fr = f_tp * vm ^ 2 / 2 * rho_ns * L / (32.2 * D)

                ResVector(0) = fluxo
                ResVector(1) = El_teta
                ResVector(2) = ((dP_fr) * 47.88)
                ResVector(3) = ((dP_hh) * 47.88)
                ResVector(4) = ((dP_hh + dP_fr) * 47.88)

                CalculateDeltaP = ResVector

            End If
        End Function

    End Class

End Namespace

