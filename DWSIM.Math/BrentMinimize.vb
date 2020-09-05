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

Namespace MathEx.BrentOpt

    Public Class BrentMinimize

        Public fc As funcdelegate
        Delegate Function funcdelegate(ByVal x As Double) As Double

        Sub New()

        End Sub

        Sub DefineFuncDelegate(ByVal fg As funcdelegate)
            Me.fc = fg
        End Sub

        Function f(ByVal x As Double) As Double
            Return fc.Invoke(x)
        End Function

        '************************************************************************
        '    Minimization by the Brent method
        '
        '    Input parameters:
        '        a - left boundary of an interval to search minimum in.
        '        b - right boundary of an interval to search minimum in.
        '        Epsilon – absolute error of the value of the function minimum.
        '
        '    Output parameters:
        '        XMin - point of minimum.
        '
        '    The result:
        '        function value at the point of minimum.
        '    ************************************************************************

        Public Function brentoptimize(ByVal a As Double, ByVal b As Double, ByVal epsilon As Double, ByRef xmin As Double) As Double
            Dim result As Double = 0
            Dim ia As Double = 0
            Dim ib As Double = 0
            Dim bx As Double = 0
            Dim d As Double = 0
            Dim e As Double = 0
            Dim etemp As Double = 0
            Dim fu As Double = 0
            Dim fv As Double = 0
            Dim fw As Double = 0
            Dim fx As Double = 0
            Dim iter As Integer = 0
            Dim p As Double = 0
            Dim q As Double = 0
            Dim r As Double = 0
            Dim u As Double = 0
            Dim v As Double = 0
            Dim w As Double = 0
            Dim x As Double = 0
            Dim xm As Double = 0
            Dim cgold As Double = 0

            cgold = 0.381966
            bx = 0.5 * (a + b)
            If a < b Then
                ia = a
            Else
                ia = b
            End If
            If a > b Then
                ib = a
            Else
                ib = b
            End If
            v = bx
            w = v
            x = v
            e = 0.0R
            fx = f(x)
            fv = fx
            fw = fx
            For iter = 1 To 100
                xm = 0.5 * (ia + ib)
                If Math.Abs(x - xm) <= epsilon * 2 - 0.5 * (ib - ia) Then
                    Exit For
                End If
                If Math.Abs(e) > epsilon Then
                    r = (x - w) * (fx - fv)
                    q = (x - v) * (fx - fw)
                    p = (x - v) * q - (x - w) * r
                    q = 2 * (q - r)
                    If q > 0 Then
                        p = -p
                    End If
                    q = Math.Abs(q)
                    etemp = e
                    e = d
                    If Not (Math.Abs(p) >= Math.Abs(0.5 * q * etemp) Or p <= q * (ia - x) Or p >= q * (ib - x)) Then
                        d = p / q
                        u = x + d
                        If u - ia < epsilon * 2 Or ib - u < epsilon * 2 Then
                            d = mysign(epsilon, xm - x)
                        End If
                    Else
                        If x >= xm Then
                            e = ia - x
                        Else
                            e = ib - x
                        End If
                        d = cgold * e
                    End If
                Else
                    If x >= xm Then
                        e = ia - x
                    Else
                        e = ib - x
                    End If
                    d = cgold * e
                End If
                If Math.Abs(d) >= epsilon Then
                    u = x + d
                Else
                    u = x + mysign(epsilon, d)
                End If
                fu = f(u)
                If Double.IsNaN(fu) Then Exit For
                If fu <= fx Then
                    If u >= x Then
                        ia = x
                    Else
                        ib = x
                    End If
                    v = w
                    fv = fw
                    w = x
                    fw = fx
                    x = u
                    fx = fu
                Else
                    If u < x Then
                        ia = u
                    Else
                        ib = u
                    End If
                    If fu <= fw Or w = x Then
                        v = w
                        fv = fw
                        w = u
                        fw = fu
                    Else
                        If fu <= fv Or v = x Or v = 2 Then
                            v = u
                            fv = fu
                        End If
                    End If
                End If
            Next
            xmin = x
            result = fx
            Return result
        End Function

        Public Function brentoptimize2(ByVal a As Double, ByVal b As Double, ByVal epsilon As Double, ByVal func As Func(Of Double, Double)) As Double
            Dim result As Double = 0
            Dim ia As Double = 0
            Dim ib As Double = 0
            Dim bx As Double = 0
            Dim d As Double = 0
            Dim e As Double = 0
            Dim etemp As Double = 0
            Dim fu As Double = 0
            Dim fv As Double = 0
            Dim fw As Double = 0
            Dim fx As Double = 0
            Dim iter As Integer = 0
            Dim p As Double = 0
            Dim q As Double = 0
            Dim r As Double = 0
            Dim u As Double = 0
            Dim v As Double = 0
            Dim w As Double = 0
            Dim x As Double = 0
            Dim xm As Double = 0
            Dim cgold As Double = 0

            cgold = 0.381966
            bx = 0.5 * (a + b)
            If a < b Then
                ia = a
            Else
                ia = b
            End If
            If a > b Then
                ib = a
            Else
                ib = b
            End If
            v = bx
            w = v
            x = v
            e = 0.0R
            fx = func(x)
            fv = fx
            fw = fx
            For iter = 1 To 100
                xm = 0.5 * (ia + ib)
                If Math.Abs(x - xm) <= epsilon * 2 - 0.5 * (ib - ia) Then
                    Exit For
                End If
                If Math.Abs(e) > epsilon Then
                    r = (x - w) * (fx - fv)
                    q = (x - v) * (fx - fw)
                    p = (x - v) * q - (x - w) * r
                    q = 2 * (q - r)
                    If q > 0 Then
                        p = -p
                    End If
                    q = Math.Abs(q)
                    etemp = e
                    e = d
                    If Not (Math.Abs(p) >= Math.Abs(0.5 * q * etemp) Or p <= q * (ia - x) Or p >= q * (ib - x)) Then
                        d = p / q
                        u = x + d
                        If u - ia < epsilon * 2 Or ib - u < epsilon * 2 Then
                            d = mysign(epsilon, xm - x)
                        End If
                    Else
                        If x >= xm Then
                            e = ia - x
                        Else
                            e = ib - x
                        End If
                        d = cgold * e
                    End If
                Else
                    If x >= xm Then
                        e = ia - x
                    Else
                        e = ib - x
                    End If
                    d = cgold * e
                End If
                If Math.Abs(d) >= epsilon Then
                    u = x + d
                Else
                    u = x + mysign(epsilon, d)
                End If
                fu = func(u)
                If Double.IsNaN(fu) Then Exit For
                If fu <= fx Then
                    If u >= x Then
                        ia = x
                    Else
                        ib = x
                    End If
                    v = w
                    fv = fw
                    w = x
                    fw = fx
                    x = u
                    fx = fu
                Else
                    If u < x Then
                        ia = u
                    Else
                        ib = u
                    End If
                    If fu <= fw Or w = x Then
                        v = w
                        fv = fw
                        w = u
                        fw = fu
                    Else
                        If fu <= fv Or v = x Or v = 2 Then
                            v = u
                            fv = fu
                        End If
                    End If
                End If
            Next
            Return x
        End Function


        Private Shared Function mysign(ByVal a As Double, ByVal b As Double) As Double
            Dim result As Double = 0

            If b > 0 Then
                result = Math.Abs(a)
            Else
                result = -Math.Abs(a)
            End If
            Return result
        End Function

    End Class

End Namespace
