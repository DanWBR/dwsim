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

Namespace DWSIM.MathEx

    Public Class PolySolve

        Shared Function Poly_Roots(ByVal Coeff) As Double(,)

            Poly_Roots = CalcRoots(Coeff(3), Coeff(2), Coeff(1), Coeff(0))

        End Function

        Shared Function CalcRoots(ByVal a As Double, ByVal b As Double, ByVal c As Double, ByVal d As Double) As Double(,)

            Dim cnt As Integer = 0
            Dim r, rant, rant2, fi, fi_ant, fi_ant2, dfidr As Double

            fi_ant2 = 0.0#
            fi_ant = 0.0#
            fi = 0.0#

            r = 0.01
            rant = r
            Do
                fi_ant2 = fi_ant
                fi_ant = fi
                fi = a * r ^ 3 + b * r ^ 2 + c * r + d
                dfidr = 3 * a * r ^ 2 + 2 * b * r + c
                rant2 = rant
                rant = r
                r = r - fi / dfidr
                If Math.Abs(fi - fi_ant2) = 0.0# Then r = rant * 1.01
                cnt += 1
            Loop Until Math.Abs(fi) < 0.00000001 Or cnt >= 1000

            Dim r1, i1, r2, i2, r3, i3 As Double

            If cnt >= 1000 Then
                r1 = r
                i1 = -1
            Else
                r1 = r
                i1 = 0
            End If

            fi_ant2 = 0
            fi_ant = 0
            fi = 0

            cnt = 0

            r = 0.99999999
            rant = r
            Do
                fi_ant2 = fi_ant
                fi_ant = fi
                fi = a * r ^ 3 + b * r ^ 2 + c * r + d
                dfidr = 3 * a * r ^ 2 + 2 * b * r + c
                rant2 = rant
                rant = r
                r = r - fi / dfidr
                If Math.Abs(fi - fi_ant2) = 0 Then r = rant * 0.999
                cnt += 1
            Loop Until Math.Abs(fi) < 0.00000001 Or cnt >= 1000

            If cnt >= 1000 Then
                r2 = r
                i2 = -1
            Else
                r2 = r
                i2 = 0
            End If

            fi_ant2 = 0
            fi_ant = 0
            fi = 0

            cnt = 0

            r = 0.5
            rant = r
            Do
                fi_ant2 = fi_ant
                fi_ant = fi
                fi = a * r ^ 3 + b * r ^ 2 + c * r + d
                dfidr = 3 * a * r ^ 2 + 2 * b * r + c
                rant2 = rant
                rant = r
                r = r - fi / dfidr
                If Math.Abs(fi - fi_ant2) = 0 Then r = rant * 0.999
                cnt += 1
            Loop Until Math.Abs(fi) < 0.00000001 Or cnt >= 1000

            If cnt >= 1000 Then
                r3 = r
                i3 = -1
            Else
                r3 = r
                i3 = 0
            End If

            Dim roots(2, 1) As Double

            roots(0, 0) = r1
            roots(0, 1) = i1
            roots(1, 0) = r2
            roots(1, 1) = i2
            roots(2, 0) = r3
            roots(2, 1) = i3

            Return roots

        End Function

    End Class

End Namespace