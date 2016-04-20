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

        Declare Sub DLL_PetAz Lib "PetAz.dll" Alias "_PETAZ" (ByRef DnsL As Single, _
        ByRef DnsG As Single, ByRef MuL As Single, ByRef MuG As Single, ByRef Sgma As Single, _
        ByRef D As Single, ByRef aK As Single, ByRef Theta As Single, ByRef VsL As Single, _
        ByRef VsG As Single, ByRef Reg As Integer, ByRef dPf As Single, ByRef dPh As Single, _
        ByRef eLx As Single, ByVal xReg As String)

        'Call DLL_PetAz(DensL, DensG, MuL, MuG, Sigma, Dia, Rough, Theta, VsL,_ 
        'VsG, Region, dPfr, dPhh, eL, RegionText)

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
        Public RegionText As String 'Text description of predicted flow regime

        Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double

            NRe = rho * v * D / mu

        End Function

        Public Overrides Function CalculateDeltaP(ByVal D As Object, ByVal L As Object, ByVal deltaz As Object, ByVal k As Object, ByVal qv As Object, ByVal ql As Object, ByVal muv As Object, ByVal mul As Object, ByVal rhov As Object, ByVal rhol As Object, ByVal surft As Object) As Object

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

                ResVector(0) = ("Lquidoapenas")
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

                ResVector(0) = ("Gsapenas")
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
                Theta = Math.Asin(deltaz / L) * 180 / Math.PI
                Rough = k * 3.28084
                RegionText = "                    "

                DLL_PetAz(DensL, DensG, mul, muv, Sigma, Dia, Rough, Theta, VsL, VsG, Region, dPfr, dPhh, eL, RegionText)

                CalculateDeltaP = New Object() {RegionText.TrimEnd(" "), eL, dPfr * 6894.76 * 3.28084 * L, dPhh * 6894.76 * 3.28084 * L, (dPfr + dPhh) * 6894.76 * 3.28084 * L}

            End If

        End Function

    End Class

End Namespace


