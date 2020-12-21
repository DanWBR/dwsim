'    Flow Package Base Class
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

    Public MustInherit Class FPBaseClass

        Sub New()

        End Sub

        Public MustOverride Function CalculateDeltaP(ByVal D As Double, ByVal L As Double, ByVal deltaz As Double, ByVal k As Double, ByVal qv As Double, ByVal ql As Double, ByVal muv As Double, ByVal mul As Double, ByVal rhov As Double, ByVal rhol As Double, ByVal surft As Double) As Object

        Public Function FrictionFactor(ByVal Re As Double, ByVal D As Double, ByVal k As Double) As Double
            Dim f, a1, b1, a, b, c As Double
            If Re > 4000 Then
                a1 = Math.Log10(((k / D) ^ 1.1096) / 2.8257 + (5.8506 / Re) ^ 0.8961)
                b1 = -2 * Math.Log10((k / D) / 3.7065 - 5.0452 * a1 / Re)
                f = (1 / b1) ^ 2
            ElseIf Re < 2100 Then
                f = 64 / Re
            Else
                a = (8 / Re) ^ 12
                b = (2.457 * Math.Log(1.0 / ((7.0 / Re) ^ 0.9 + 0.27 * k / D))) ^ 16
                c = (37530 / Re) ^ 16
                f = 8 * (a + 1 / (b + c) ^ 1.5) ^ (1.0 / 12.0)
            End If
            Return f
        End Function

        Public Function NRe(ByVal rho As Double, ByVal v As Double, ByVal D As Double, ByVal mu As Double) As Double
            NRe = rho * v * D / mu
        End Function
    End Class

End Namespace