Imports System.Runtime.InteropServices

'    Petalas-Aziz Pressure Drop Calculation Routine
'    Copyright 2012 Daniel Wagner O. de Medeiros
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

    <Serializable()> Public Class PetalasAziz

        Inherits FPBaseClass

        'INPUT Variables
        Protected DensL As Single  'Liquid density (lb/ft³)
        Protected DensG As Single  'Gas density (lb/ft³)
        Protected Sigma As Single  'Gas/Liquid interfacial tension (dyne/cm)
        Protected VsL As Single    'Liquid superficial velocity (ft/sec)
        Protected VsG As Single    'Gas superficial velocity (ft/sec)
        Protected Dia As Single    'Pipe diameter (inch)
        Protected Theta As Single  'Pipe inclination (degrees)
        Protected Rough As Single  'Absolute pipe roughness (ft)

        'OUTPUT Variables
        Protected dPfr As Single   'Frictional Pressure Gradient (psi/ft)
        Protected dPhh As Single   'Hydrostatic Pressure Gradient (psi/ft)
        Protected eL As Single 'Volume Fraction Liquid
        Protected Region As Integer    'Code designating predicted flow regime
        Public FlowRegime As String 'Text description of predicted flow regime

        <DllImport("PetAz", CallingConvention:=CallingConvention.Cdecl, EntryPoint:="calcpdrop")> _
        Public Shared Sub calcpdrop(ByRef DensL As Single, ByRef DensG As Single, ByRef MuL As Single, ByRef MuG As Single,
                                                               ByRef Sigma As Single, ByRef Dia As Single, ByRef Rough As Single, ByRef Theta As Single,
                                                               ByRef VsL As Single, ByRef VsG As Single, ByRef Region As Integer, ByRef dPfr As Single,
                                                               ByRef dPhh As Single, ByRef eL As Single)
        End Sub

        Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double

            NRe = rho * v * D / mu

        End Function

        Public Overrides Function CalculateDeltaP(ByVal D As Double, ByVal L As Double, ByVal deltaz As Double, ByVal k As Double, ByVal qv As Double, ByVal ql As Double, ByVal muv As Double, ByVal mul As Double, ByVal rhov As Double, ByVal rhol As Double, ByVal surft As Double) As Object

            CalculateDeltaP = Nothing

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "CalculateDeltaP", "Petalas and Aziz Pressure Drop", "Petalas and Aziz Multiphase Pressure Drop Calculation Routine", True)

            IObj?.SetCurrent()

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

            Dim ResVector(4) As Object

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

                Dim dPl = fric * L / D * vlo ^ 2 / 2 * rhol
                Dim dPh = rhol * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                ResVector(0) = "Liquid Only"
                ResVector(1) = 1
                ResVector(2) = dPl
                ResVector(3) = dPh
                ResVector(4) = dPl + dPh

                CalculateDeltaP = ResVector

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

                Dim dPl = fric * L / D * vgo ^ 2 / 2 * rhov
                Dim dPh = rhov * 9.8 * Math.Sin(Math.Asin(deltaz / L)) * L

                ResVector(0) = "Vapor Only"
                ResVector(1) = 0
                ResVector(2) = dPl
                ResVector(3) = dPh
                ResVector(4) = dPl + dPh

                CalculateDeltaP = ResVector

            Else

                'INPUT Variables
                'Liquid density (lb/ft³)
                'Gas density (lb/ft³)
                'Gas/Liquid interfacial tension (dyne/cm)
                'Liquid superficial velocity (ft/sec)
                'Gas superficial velocity (ft/sec)
                'Pipe diameter (inch)
                'Pipe inclination (degrees)
                'Absolute pipe roughness (ft)

                'OUTPUT Variables
                'Frictional Pressure Gradient (psi/ft)
                'Hydrostatic Pressure Gradient (psi/ft)
                'Volume Fraction Liquid
                'Code designating predicted flow regime
                'Text description of predicted flow regime

                DensL = rhol / 16.0185
                DensG = rhov / 16.0185
                Sigma = surft / 0.001
                VsG = qv / 24 / 3600 / (Math.PI * (D ^ 2) / 4) * 3.28084
                VsL = ql / 24 / 3600 / (Math.PI * (D ^ 2) / 4) * 3.28084
                Dia = D * 39.37
                Theta = Math.Atan(deltaz / (L ^ 2 - deltaz ^ 2) ^ 0.5) * 180 / Math.PI
                Rough = k * 3.28084
                FlowRegime = "                    "

                calcpdrop(DensL, DensG, mul, muv, Sigma, Dia, Rough, Theta, VsL, VsG, Region, dPfr, dPhh, eL)

                Select Case Region
                    Case 1
                        FlowRegime = "Elongated Bubbles"
                    Case 2
                        FlowRegime = "Bubbles"
                    Case 3
                        FlowRegime = "Stratified Smooth"
                    Case 4
                        FlowRegime = "Stratified Waves"
                    Case 5
                        FlowRegime = "Slug"
                    Case 6
                        FlowRegime = "Annular Mist"
                    Case 7
                        FlowRegime = "Dispersed Bubbles"
                    Case 8
                        FlowRegime = "Froth I (DB/AM transition)"
                    Case 9
                        FlowRegime = "Homogeneous"
                    Case 10
                        FlowRegime = "Froth"
                    Case 11
                        FlowRegime = "Stratified"
                    Case 12
                        FlowRegime = "Segregated"
                    Case 13
                        FlowRegime = "Transition"
                    Case 14
                        FlowRegime = "Intermittent"
                    Case 15
                        FlowRegime = "Distributed"
                    Case 16
                        FlowRegime = "Single Phase"
                End Select

                CalculateDeltaP = New Object() {FlowRegime, eL, dPfr * 6894.76 * 3.28084 * L, dPhh * 6894.76 * 3.28084 * L, (dPfr + dPhh) * 6894.76 * 3.28084 * L}

            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add("Flow Regime: " & FlowRegime)
            IObj?.Paragraphs.Add("<mi>e_L</mi> = " & eL)
            IObj?.Paragraphs.Add("<mi>\Delta P_{friction}</mi> = " & dPfr * 6894.76 * 3.28084 * L & " Pa")
            IObj?.Paragraphs.Add("<mi>\Delta P_{elevation}</mi> = " & dPhh * 6894.76 * 3.28084 * L & " Pa")
            IObj?.Paragraphs.Add("<mi>\Delta P_{total}</mi> = " & (dPfr + dPhh) * 6894.76 * 3.28084 * L & " Pa")

            IObj?.Close()

        End Function

    End Class

End Namespace


