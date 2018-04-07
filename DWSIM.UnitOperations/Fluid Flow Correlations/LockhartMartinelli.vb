'    Lockhart and Martinelli Pressure Drop Calculation Routine
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
    <Serializable()> Public Class LockhartMartinelli

        Inherits FPBaseClass

      Public Overrides Function CalculateDeltaP(ByVal D As Double, ByVal L As Double, ByVal deltaz As Double, ByVal k As Double, ByVal qv As Double, ByVal ql As Double, ByVal muv As Double, ByVal mul As Double, ByVal rhov As Double, ByVal rhol As Double, ByVal surft As Double) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalculateDeltaP", "Lockhart and Martinelli Pressure Drop", "Lockhart and Martinelli Multiphase Pressure Drop Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("Pressure drop for each of the phases are calculated explicitly assuming that either liquid or gas is flowing through the pipe based on procedure provided for <a href='https://cheguide.com/2015/08/single-phase-fluid-flow-pressure-drop/'>single phase flow</a>.</p>
                                <pre><code>(?P/L)<sub>G</sub> = function(Q<sub>G</sub>, D, µ<sub>G</sub>, ?<sub>G</sub>, e)</code>
                                <code>(?P/L)<sub>L</sub> = function(Q<sub>L</sub>, D, µ<sub>L</sub>, ?<sub>L</sub>, e)</code></pre>
                                <p>A pressure ratio is calculated.</p>
                                <pre><code>X² = (?P/L)<sub>L</sub> / (?P/L)<sub>G</sub></code></pre>
                                <p><img src='https//cheguide.com/wp-content/uploads/2015/08/ff_Lockhart_Martinelli.gif' alt='Lockhart - Martinelli Correlation' /></p>
                                <p>A separate pressure drop is calculated for each phase.</p>
                                <pre><code>(?P/L)<sub>L1</sub> = f²<sub>L</sub> (?P/L)<sub>L</sub></code>
                                <code>(?P/L)<sub>G1</sub> = f²<sub>G</sub> (?P/L)<sub>G</sub></code></pre>
                                <p>Estimated two phase pressure drop is maximum of these</p>
                                <pre><code>(?P/L)<sub>TP</sub> = Max((?P/L)<sub>L1</sub>,(?P/L)<sub>G1</sub>)</code></pre><p>")

            IObj?.Paragraphs.Add("Reference: <a href='https://cheguide.com/tag/lockhart-martinelli/'>https://cheguide.com/tag/lockhart-martinelli/</a>")

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")

            IObj?.Paragraphs.Add("<mi>D</mi> = " & D & " m")
            IObj?.Paragraphs.Add("<mi>L</mi> = " & L & " m")
            IObj?.Paragraphs.Add("<mi>H</mi> = " & deltaz & " m")
            IObj?.Paragraphs.Add("<mi>k</mi> = " & k & " m")
            IObj?.Paragraphs.Add("<mi>Q_V</mi> = " & qv & " m3/d actual")
            IObj?.Paragraphs.Add("<mi>Q_L</mi> = " & ql & " m3/d actual")
            IObj?.Paragraphs.Add("<mi>\mu _V</mi> = " & muv & " cP")
            IObj?.Paragraphs.Add("<mi>\mu _L</mi> = " & mul & " cP")
            IObj?.Paragraphs.Add("<mi>\rho _V</mi> = " & rhov & " kg/m3")
            IObj?.Paragraphs.Add("<mi>\rho _L</mi> = " & rhol & " kg/m3")
            IObj?.Paragraphs.Add("<mi>\sigma</mi> = " & surft & " N/m")

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            CalculateDeltaP = Nothing

            Dim resvect(4) As Object

            If qv = 0.0# Then

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

                IObj?.Paragraphs.Add("<mi>Re</mi> = " & Re_fit)
                IObj?.Paragraphs.Add("<mi>f</mi> = " & fric)
                IObj?.Paragraphs.Add("<mi>v_L</mi> = " & vlo & " m/s")

                Dim dPl = fric * L / D * vlo ^ 2 / 2 * rhol
                Dim dPh = rhol * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                resvect(0) = "Liquid Only"
                resvect(1) = 1
                resvect(2) = dPl
                resvect(3) = dPh
                resvect(4) = dPl + dPh

                CalculateDeltaP = resvect

            ElseIf ql = 0.0# Then

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

                IObj?.Paragraphs.Add("<mi>Re</mi> = " & Re_fit)
                IObj?.Paragraphs.Add("<mi>f</mi> = " & fric)
                IObj?.Paragraphs.Add("<mi>v_V</mi> = " & vgo & " m/s")

                Dim dPl = fric * L / D * vgo ^ 2 / 2 * rhov
                Dim dPh = rhov * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                resvect(0) = "Vapor Only"
                resvect(1) = 0
                resvect(2) = dPl
                resvect(3) = dPh
                resvect(4) = dPl + dPh

                CalculateDeltaP = resvect

            Else

                'D em m
                'L em m
                'k em m
                'qv e QL em m3/d
                'muv e mul em cP
                'rhov e rhol em kg/m3
                'sigma em N/m

                'LM_DP - Calculo da perda de carga utilizando o metodo de Lockhart-Martinelli (1949)

                Dim g, teta, Cg, Cl, A, Vm, Vsl, Vsg As Double
                Dim Re_SL, Re_SG, fsl, fsg, a1, b1, dP_SL, dP_SG

                g = 9.8

                teta = Math.Atan(deltaz / (L ^ 2 - deltaz ^ 2) ^ 0.5)

                qv = qv / 24 / 60 / 60
                ql = ql / 24 / 60 / 60
                muv = muv * 0.001
                mul = mul * 0.001

                A = Math.PI * D ^ 2 / 4
                Vsl = ql / A
                Vsg = qv / A
                Vm = Vsl + Vsg
                Cg = Vsg / Vm
                Cl = 1 - Cg

                Re_SL = rhol * Vsl * D / mul
                Re_SG = rhov * Vsg * D / muv

                If Re_SL > 3250 Then
                    a1 = Math.Log(((k / D) ^ 1.1096) / 2.8257 + (7.149 / Re_SL) ^ 0.8961) / Math.Log(10.0#)
                    b1 = -2 * Math.Log((k / D) / 3.7065 - 5.0452 * a1 / Re_SL) / Math.Log(10.0#)
                    fsl = (1 / b1) ^ 2
                Else
                    fsl = 64 / Re_SL
                End If

                If Re_SG > 3250 Then
                    a1 = Math.Log(((k / D) ^ 1.1096) / 2.8257 + (7.149 / Re_SG) ^ 0.8961) / Math.Log(10.0#)
                    b1 = -2 * Math.Log((k / D) / 3.7065 - 5.0452 * a1 / Re_SG) / Math.Log(10.0#)
                    fsg = (1 / b1) ^ 2
                Else
                    fsg = 64 / Re_SG
                End If

                dP_SL = fsl * Vsl ^ 2 * L * rhol / (D * 2) ' em Pa
                dP_SG = fsg * Vsg ^ 2 * L * rhov / (D * 2) ' em Pa

                Dim Xtt, fi_Ltt, fi_Gtt, dPf, dPg

                Xtt = ((1 - Cg) / Cg) ^ 0.9 * (rhov / rhol) ^ 0.5 * (mul / muv) ^ 0.1

                IObj?.Paragraphs.Add("<mi>Re_{sg}</mi> = " & Re_SG)
                IObj?.Paragraphs.Add("<mi>Re_{sl}</mi> = " & Re_SL)
                IObj?.Paragraphs.Add("<mi>f_{sg}</mi> = " & fsg)
                IObj?.Paragraphs.Add("<mi>f_{sl}</mi> = " & fsl)
                IObj?.Paragraphs.Add("<mi>X</mi> = " & Xtt)

                If Vsl > Vsg Then
                    fi_Ltt = 1 + 20 / Xtt + 1 / Xtt ^ 2
                    dPf = fi_Ltt * dP_SL
                Else
                    fi_Gtt = 1 + 20 * Xtt + Xtt ^ 2
                    dPf = fi_Gtt * dP_SG
                End If

                dPg = (Cg * rhov + Cl * rhol) * g * Math.Sin(teta) * L

                resvect(0) = "Homogeneous"
                resvect(1) = Cl
                resvect(2) = dPf
                resvect(3) = dPg
                resvect(4) = (dPf + dPg)

                CalculateDeltaP = resvect

            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add("Flow Regime: " & resvect(0))
            IObj?.Paragraphs.Add("<mi>e_L</mi> = " & resvect(1))
            IObj?.Paragraphs.Add("<mi>\Delta P_{friction}</mi> = " & resvect(2) & " Pa")
            IObj?.Paragraphs.Add("<mi>\Delta P_{elevation}</mi> = " & resvect(3) & " Pa")
            IObj?.Paragraphs.Add("<mi>\Delta P_{total}</mi> = " & resvect(4) & " Pa")

            IObj?.Close()

        End Function

        Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double

            'mu = mu * 0.001
            NRe = rho * v * D / mu

        End Function

    End Class

End Namespace
