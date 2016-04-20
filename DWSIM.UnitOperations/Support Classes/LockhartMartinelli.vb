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

        Public Overrides Function CalculateDeltaP(ByVal D As Object, ByVal L As Object, _
                                ByVal deltaz As Object, ByVal k As Object, ByVal qv As Object, _
                                ByVal ql As Object, ByVal muv As Object, ByVal mul As Object, _
                                ByVal rhov As Object, ByVal rhol As Object, ByVal surft As Object) As Object

            CalculateDeltaP = Nothing

            Dim resvect(4) As Object

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

                resvect(0) = ("Lquidoapenas")
                resvect(1) = 1
                resvect(2) = dPl
                resvect(3) = dPh
                resvect(4) = dPl + dPh

                CalculateDeltaP = resvect

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

                resvect(0) = ("Gsapenas")
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

                teta = Math.Asin(deltaz / L) ' * 180 / Math.PI

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

                If Vsl > Vsg Then
                    fi_Ltt = 1 + 20 / Xtt + 1 / Xtt ^ 2
                    dPf = fi_Ltt * dP_SL
                Else
                    fi_Gtt = 1 + 20 * Xtt + Xtt ^ 2
                    dPf = fi_Gtt * dP_SG
                End If

                dPg = (Cg * rhov + Cl * rhol) * g * Math.Sin(teta) * L

                resvect(0) = ("Homogneo")
                resvect(1) = Cl
                resvect(2) = dPf
                resvect(3) = dPg
                resvect(4) = (dPf + dPg)

                CalculateDeltaP = resvect

            End If

        End Function

        Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double

            'mu = mu * 0.001
            NRe = rho * v * D / mu

        End Function

    End Class

End Namespace
