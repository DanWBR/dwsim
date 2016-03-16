'    Miscelaneous Math Functions for DWSIM
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

Namespace DWSIM.MathEx.BrentOpt

    Public Class Brent

        Public fc As funcdelegate
        Delegate Function funcdelegate(ByVal x As Double, ByVal otherargs As Object) As Double

        Sub New()

        End Sub

        Sub DefineFuncDelegate(ByVal fg As funcdelegate)
            Me.fc = fg
        End Sub

        Function func(ByVal x As Double, ByVal otherargs As Object) As Double
            Return fc.Invoke(x, otherargs)
        End Function

        Function BrentOpt(ByVal minval As Double, ByVal maxval As Double, ByVal n As Integer, ByVal tol As Double, ByVal itmax As Integer, ByVal otherargs As Object) As Double

            Dim x_inf, x_sup, y_inf, y, delta_x As Double

            x_inf = minval
            x_sup = maxval

            delta_x = (x_sup - x_inf) / n

            Do
                y = func(x_inf, otherargs)
                x_inf = x_inf + delta_x
                y_inf = func(x_inf, otherargs)
            Loop Until y * y_inf < 0 Or x_inf > x_sup
            x_sup = x_inf - delta_x

            Dim aaa, bbb, ccc, ddd, eee, min11, min22, faa, fbb, fcc, ppp, qqq, rrr, sss, tol11, xmm As Double
            Dim ITMAX2 As Integer = itmax
            Dim iter2 As Integer

            aaa = x_inf
            bbb = x_sup
            ccc = x_sup

            faa = func(x_inf, otherargs)
            fbb = func(x_sup, otherargs)
            fcc = func(x_sup, otherargs)

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
                tol11 = tol
                xmm = 0.5 * (ccc - bbb)
                If (Math.Abs(xmm) <= tol11) Or (fbb = 0) Then GoTo Final3
                If Math.Abs(fbb) < tol11 Then GoTo Final3
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
                fbb = func(bbb, otherargs)
                iter2 += 1
            Loop Until iter2 = ITMAX2

            Return bbb

Final3:

            Return bbb

        End Function


    End Class

End Namespace
