Imports System

Namespace MathEx.Interpolation

    '************************************************************************
    'Copyright (c) 2007, Sergey Bochkanov (ALGLIB project).
    '
    'Redistribution and use in source and binary forms, with or without
    'modification, are permitted provided that the following conditions are
    'met:
    '
    '- Redistributions of source code must retain the above copyright
    '  notice, this list of conditions and the following disclaimer.
    '
    '- Redistributions in binary form must reproduce the above copyright
    '  notice, this list of conditions and the following disclaimer listed
    '  in this license in the documentation and/or other materials
    '  provided with the distribution.
    '
    '- Neither the name of the copyright holders nor the names of its
    '  contributors may be used to endorse or promote products derived from
    '  this software without specific prior written permission.
    '
    'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    '"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    'A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    'OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    'LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    'DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    'THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    '(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    '************************************************************************

    Public Class Interpolation

        Public Shared Function Interpolate(x() As Double, y() As Double, xint As Double) As Double

            If x.Count = 1 Then
                Return xint / x(0) * y(0)
            ElseIf x.Count = 0 Then
                Return 0.0
            Else
                Dim w(x.Count) As Double
                ratinterpolation.buildfloaterhormannrationalinterpolant(x, x.Count, 0.5, w)
                Return polinterpolation.barycentricinterpolation(x, y, w, x.Count, xint)
            End If

        End Function

    End Class

    Public Class polinterpolation

        '************************************************************************
        '    Interpolation using barycentric formula
        '
        '    F(t) = SUM(i=0,n-1,w[i]*f[i]/(t-x[i])) / SUM(i=0,n-1,w[i]/(t-x[i]))
        '
        '    Input parameters:
        '        X   -   interpolation nodes, array[0..N-1]
        '        F   -   function values, array[0..N-1]
        '        W   -   barycentric weights, array[0..N-1]
        '        N   -   nodes count, N>0
        '        T   -   interpolation point
        '        
        '    Result:
        '        barycentric interpolant F(t)
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Function barycentricinterpolation(x As Double(), f As Double(), w As Double(), n As Integer, t As Double) As Double
            Dim result As Double = 0
            Dim s1 As Double = 0
            Dim s2 As Double = 0
            Dim v As Double = 0
            Dim threshold As Double = 0
            Dim s As Double = 0
            Dim i As Integer = 0
            Dim j As Integer = 0

            System.Diagnostics.Debug.Assert(n > 0, "barycentricinterpolation: N<=0!")
            threshold = Math.Sqrt(AP.MathEx.MinRealNumber)

            '
            ' First, decide: should we use "safe" formula (guarded
            ' against overflow) or fast one?
            '
            j = 0
            s = t - x(0)
            For i = 1 To n - 1
                If Math.Abs(t - x(i)) < Math.Abs(s) Then
                    s = t - x(i)
                    j = i
                End If
            Next
            If s = 0 Then
                result = f(j)
                Return result
            End If
            If Math.Abs(s) > threshold Then

                '
                ' use fast formula
                '
                j = -1
                s = 1.0R
            End If

            '
            ' Calculate using safe or fast barycentric formula
            '
            s1 = 0
            s2 = 0
            For i = 0 To n - 1
                If i <> j Then
                    v = s * w(i) / (t - x(i))
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                Else
                    v = w(i)
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                End If
            Next
            result = s1 / s2
            Return result
        End Function


        '************************************************************************
        '    Polynomial interpolation on the equidistant nodes using barycentric
        '    formula. O(N) complexity.
        '
        '    Input parameters:
        '        A,B -   interpolation interval [A,B]
        '        F   -   function values, array[0..N-1].
        '                F[i] = F(A+(B-A)*i/(N-1))
        '        N   -   nodes count
        '        T   -   interpolation point
        '
        '    Result:
        '        the value of the interpolation polynomial F(t)
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Function equidistantpolynomialinterpolation(ByVal a As Double, ByVal b As Double, ByRef f As Double(), ByVal n As Integer, ByVal t As Double) As Double
            Dim result As Double = 0
            Dim s1 As Double = 0
            Dim s2 As Double = 0
            Dim v As Double = 0
            Dim threshold As Double = 0
            Dim s As Double = 0
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim w As Double = 0
            Dim x As Double = 0

            System.Diagnostics.Debug.Assert(n > 0, "barycentricinterpolation: N<=0!")
            threshold = Math.Sqrt(AP.MathEx.MinRealNumber)

            '
            ' Special case: N=1
            '
            If n = 1 Then
                result = f(0)
                Return result
            End If

            '
            ' First, decide: should we use "safe" formula (guarded
            ' against overflow) or fast one?
            '
            j = 0
            s = t - a
            For i = 1 To n - 1
                x = a + Convert.ToDouble((i)) / Convert.ToDouble((n - 1)) * (b - a)
                If Math.Abs(t - x) < Math.Abs(s) Then
                    s = t - x
                    j = i
                End If
            Next
            If s = 0 Then
                result = f(j)
                Return result
            End If
            If Math.Abs(s) > threshold Then

                '
                ' use fast formula
                '
                j = -1
                s = 1.0R
            End If

            '
            ' Calculate using safe or fast barycentric formula
            '
            s1 = 0
            s2 = 0
            w = 1.0R
            For i = 0 To n - 1
                If i <> j Then
                    v = s * w / (t - (a + Convert.ToDouble((i)) / Convert.ToDouble((n - 1)) * (b - a)))
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                Else
                    v = w
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                End If
                w = -(w * (n - 1 - i))
                w = w / (i + 1)
            Next
            result = s1 / s2
            Return result
        End Function


        '************************************************************************
        '    Polynomial interpolation on the Chebyshev nodes (first kind) using
        '    barycentric formula. O(N) complexity.
        '
        '    Input parameters:
        '        A,B -   interpolation interval [A,B]
        '        F   -   function values, array[0..N-1].
        '                F[i] = F(0.5*(B+A) + 0.5*(B-A)*Cos(PI*(2*i+1)/(2*n)))
        '        N   -   nodes count
        '        T   -   interpolation point
        '
        '    Result:
        '        the value of the interpolation polynomial F(t)
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Function chebyshev1interpolation(ByVal a As Double, ByVal b As Double, ByRef f As Double(), ByVal n As Integer, ByVal t As Double) As Double
            Dim result As Double = 0
            Dim s1 As Double = 0
            Dim s2 As Double = 0
            Dim v As Double = 0
            Dim threshold As Double = 0
            Dim s As Double = 0
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim a0 As Double = 0
            Dim delta As Double = 0
            Dim alpha As Double = 0
            Dim beta As Double = 0
            Dim ca As Double = 0
            Dim sa As Double = 0
            Dim tempc As Double = 0
            Dim temps As Double = 0
            Dim x As Double = 0
            Dim w As Double = 0
            Dim p1 As Double = 0

            System.Diagnostics.Debug.Assert(n > 0, "Chebyshev1Interpolation: N<=0!")
            threshold = Math.Sqrt(AP.MathEx.MinRealNumber)
            t = (t - 0.5 * (a + b)) / (0.5 * (b - a))

            '
            ' Prepare information for the recurrence formula
            ' used to calculate sin(pi*(2j+1)/(2n+2)) and
            ' cos(pi*(2j+1)/(2n+2)):
            '
            ' A0    = pi/(2n+2)
            ' Delta = pi/(n+1)
            ' Alpha = 2 sin^2 (Delta/2)
            ' Beta  = sin(Delta)
            '
            ' so that sin(..) = sin(A0+j*delta) and cos(..) = cos(A0+j*delta).
            ' Then we use
            '
            ' sin(x+delta) = sin(x) - (alpha*sin(x) - beta*cos(x))
            ' cos(x+delta) = cos(x) - (alpha*cos(x) - beta*sin(x))
            '
            ' to repeatedly calculate sin(..) and cos(..).
            '
            a0 = Math.PI / (2 * (n - 1) + 2)
            delta = 2 * Math.PI / (2 * (n - 1) + 2)
            alpha = 2 * AP.MathEx.Sqr(Math.Sin(delta / 2))
            beta = Math.Sin(delta)

            '
            ' First, decide: should we use "safe" formula (guarded
            ' against overflow) or fast one?
            '
            ca = Math.Cos(a0)
            sa = Math.Sin(a0)
            j = 0
            x = ca
            s = t - x
            For i = 1 To n - 1

                '
                ' Next X[i]
                '
                temps = sa - (alpha * sa - beta * ca)
                tempc = ca - (alpha * ca + beta * sa)
                sa = temps
                ca = tempc
                x = ca

                '
                ' Use X[i]
                '
                If Math.Abs(t - x) < Math.Abs(s) Then
                    s = t - x
                    j = i
                End If
            Next
            If s = 0 Then
                result = f(j)
                Return result
            End If
            If Math.Abs(s) > threshold Then

                '
                ' use fast formula
                '
                j = -1
                s = 1.0R
            End If

            '
            ' Calculate using safe or fast barycentric formula
            '
            s1 = 0
            s2 = 0
            ca = Math.Cos(a0)
            sa = Math.Sin(a0)
            p1 = 1.0R
            For i = 0 To n - 1

                '
                ' Calculate X[i], W[i]
                '
                x = ca
                w = p1 * sa

                '
                ' Proceed
                '
                If i <> j Then
                    v = s * w / (t - x)
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                Else
                    v = w
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                End If

                '
                ' Next CA, SA, P1
                '
                temps = sa - (alpha * sa - beta * ca)
                tempc = ca - (alpha * ca + beta * sa)
                sa = temps
                ca = tempc
                p1 = -p1
            Next
            result = s1 / s2
            Return result
        End Function


        '************************************************************************
        '    Polynomial interpolation on the Chebyshev nodes (second kind) using
        '    barycentric formula. O(N) complexity.
        '
        '    Input parameters:
        '        A,B -   interpolation interval [A,B]
        '        F   -   function values, array[0..N-1].
        '                F[i] = F(0.5*(B+A) + 0.5*(B-A)*Cos(PI*i/(n-1)))
        '        N   -   nodes count
        '        T   -   interpolation point
        '
        '    Result:
        '        the value of the interpolation polynomial F(t)
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Function chebyshev2interpolation(ByVal a As Double, ByVal b As Double, ByRef f As Double(), ByVal n As Integer, ByVal t As Double) As Double
            Dim result As Double = 0
            Dim s1 As Double = 0
            Dim s2 As Double = 0
            Dim v As Double = 0
            Dim threshold As Double = 0
            Dim s As Double = 0
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim a0 As Double = 0
            Dim delta As Double = 0
            Dim alpha As Double = 0
            Dim beta As Double = 0
            Dim ca As Double = 0
            Dim sa As Double = 0
            Dim tempc As Double = 0
            Dim temps As Double = 0
            Dim x As Double = 0
            Dim w As Double = 0
            Dim p1 As Double = 0

            System.Diagnostics.Debug.Assert(n > 1, "Chebyshev2Interpolation: N<=1!")
            threshold = Math.Sqrt(AP.MathEx.MinRealNumber)
            t = (t - 0.5 * (a + b)) / (0.5 * (b - a))

            '
            ' Prepare information for the recurrence formula
            ' used to calculate sin(pi*i/n) and
            ' cos(pi*i/n):
            '
            ' A0    = 0
            ' Delta = pi/n
            ' Alpha = 2 sin^2 (Delta/2)
            ' Beta  = sin(Delta)
            '
            ' so that sin(..) = sin(A0+j*delta) and cos(..) = cos(A0+j*delta).
            ' Then we use
            '
            ' sin(x+delta) = sin(x) - (alpha*sin(x) - beta*cos(x))
            ' cos(x+delta) = cos(x) - (alpha*cos(x) - beta*sin(x))
            '
            ' to repeatedly calculate sin(..) and cos(..).
            '
            a0 = 0.0R
            delta = Math.PI / (n - 1)
            alpha = 2 * AP.MathEx.Sqr(Math.Sin(delta / 2))
            beta = Math.Sin(delta)

            '
            ' First, decide: should we use "safe" formula (guarded
            ' against overflow) or fast one?
            '
            ca = Math.Cos(a0)
            sa = Math.Sin(a0)
            j = 0
            x = ca
            s = t - x
            For i = 1 To n - 1

                '
                ' Next X[i]
                '
                temps = sa - (alpha * sa - beta * ca)
                tempc = ca - (alpha * ca + beta * sa)
                sa = temps
                ca = tempc
                x = ca

                '
                ' Use X[i]
                '
                If Math.Abs(t - x) < Math.Abs(s) Then
                    s = t - x
                    j = i
                End If
            Next
            If s = 0 Then
                result = f(j)
                Return result
            End If
            If Math.Abs(s) > threshold Then

                '
                ' use fast formula
                '
                j = -1
                s = 1.0R
            End If

            '
            ' Calculate using safe or fast barycentric formula
            '
            s1 = 0
            s2 = 0
            ca = Math.Cos(a0)
            sa = Math.Sin(a0)
            p1 = 1.0R
            For i = 0 To n - 1

                '
                ' Calculate X[i], W[i]
                '
                x = ca
                If i = 0 Or i = n - 1 Then
                    w = 0.5 * p1
                Else
                    w = 1.0R * p1
                End If

                '
                ' Proceed
                '
                If i <> j Then
                    v = s * w / (t - x)
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                Else
                    v = w
                    s1 = s1 + v * f(i)
                    s2 = s2 + v
                End If

                '
                ' Next CA, SA, P1
                '
                temps = sa - (alpha * sa - beta * ca)
                tempc = ca - (alpha * ca + beta * sa)
                sa = temps
                ca = tempc
                p1 = -p1
            Next
            result = s1 / s2
            Return result
        End Function


        '************************************************************************
        '    Polynomial interpolation on the arbitrary nodes using Neville's algorithm.
        '    O(N^2) complexity.
        '
        '    Input parameters:
        '        X   -   interpolation nodes, array[0..N-1].
        '        F   -   function values, array[0..N-1].
        '        N   -   nodes count
        '        T   -   interpolation point
        '
        '    Result:
        '        the value of the interpolation polynomial F(t)
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Function nevilleinterpolation(ByRef x As Double(), ByVal f As Double(), ByVal n As Integer, ByVal t As Double) As Double
            Dim result As Double = 0
            Dim m As Integer = 0
            Dim i As Integer = 0

            f = DirectCast(f.Clone(), Double())

            n = n - 1
            For m = 1 To n
                For i = 0 To n - m
                    f(i) = ((t - x(i + m)) * f(i) + (x(i) - t) * f(i + 1)) / (x(i) - x(i + m))
                Next
            Next
            result = f(0)
            Return result
        End Function


        '************************************************************************
        '    Polynomial interpolation on the arbitrary nodes using Neville's algorithm.
        '    O(N^2) complexity. Subroutine  returns  the  value  of  the  interpolation
        '    polynomial, the first and the second derivative.
        '
        '    Input parameters:
        '        X   -   interpolation nodes, array[0..N-1].
        '        F   -   function values, array[0..N-1].
        '        N   -   nodes count
        '        T   -   interpolation point
        '
        '    Output parameters:
        '        P   -   the value of the interpolation polynomial F(t)
        '        DP  -   the first derivative of the interpolation polynomial dF(t)/dt
        '        D2P -   the second derivative of the interpolation polynomial d2F(t)/dt2
        '
        '      -- ALGLIB --
        '         Copyright 28.05.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Sub nevilledifferentiation(ByRef x As Double(), ByVal f As Double(), ByVal n As Integer, ByVal t As Double, ByRef p As Double, ByRef dp As Double, _
         ByRef d2p As Double)
            Dim m As Integer = 0
            Dim i As Integer = 0
            Dim df As Double() = New Double(-1) {}
            Dim d2f As Double() = New Double(-1) {}

            f = DirectCast(f.Clone(), Double())

            n = n - 1
            df = New Double(n) {}
            d2f = New Double(n) {}
            For i = 0 To n
                d2f(i) = 0
                df(i) = 0
            Next
            For m = 1 To n
                For i = 0 To n - m
                    d2f(i) = ((t - x(i + m)) * d2f(i) + (x(i) - t) * d2f(i + 1) + 2 * df(i) - 2 * df(i + 1)) / (x(i) - x(i + m))
                    df(i) = ((t - x(i + m)) * df(i) + f(i) + (x(i) - t) * df(i + 1) - f(i + 1)) / (x(i) - x(i + m))
                    f(i) = ((t - x(i + m)) * f(i) + (x(i) - t) * f(i + 1)) / (x(i) - x(i + m))
                Next
            Next
            p = f(0)
            dp = df(0)
            d2p = d2f(0)
        End Sub


        '************************************************************************
        '    Obsolete algorithm, replaced by NevilleInterpolation.
        '    ************************************************************************

        Public Shared Function lagrangeinterpolate(ByVal n As Integer, ByRef x As Double(), ByVal f As Double(), ByVal t As Double) As Double
            Dim result As Double = 0

            f = DirectCast(f.Clone(), Double())

            result = nevilleinterpolation(x, f, n, t)
            Return result
        End Function


        '************************************************************************
        '    Obsolete algorithm, replaced by NevilleInterpolationWithDerivative
        '    ************************************************************************

        Public Shared Sub lagrangederivative(ByVal n As Integer, ByRef x As Double(), ByVal f As Double(), ByVal t As Double, ByRef p As Double, ByRef dp As Double)
            Dim d2p As Double = 0

            f = DirectCast(f.Clone(), Double())

            nevilledifferentiation(x, f, n, t, p, dp, d2p)

        End Sub
    End Class

    '************************************************************************
    'Copyright (c) 2007, Sergey Bochkanov (ALGLIB project).
    '
    'Redistribution and use in source and binary forms, with or without
    'modification, are permitted provided that the following conditions are
    'met:
    '
    '- Redistributions of source code must retain the above copyright
    '  notice, this list of conditions and the following disclaimer.
    '
    '- Redistributions in binary form must reproduce the above copyright
    '  notice, this list of conditions and the following disclaimer listed
    '  in this license in the documentation and/or other materials
    '  provided with the distribution.
    '
    '- Neither the name of the copyright holders nor the names of its
    '  contributors may be used to endorse or promote products derived from
    '  this software without specific prior written permission.
    '
    'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    '"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    'A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    'OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    'LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    'DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    'THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    '(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    '************************************************************************

    Public Class ratinterpolation

        '************************************************************************
        '    Rational barycentric interpolation without poles
        '
        '    The subroutine constructs the rational interpolating function without real
        '    poles. It should be noted that the barycentric weights of the  interpolant
        '    constructed are independent of the values of the given function.
        '
        '    Input parameters:
        '        X   -   interpolation nodes, array[0..N-1].
        '        N   -   number of nodes, N>0.
        '        D   -   order of the interpolation scheme, 0 <= D <= N-1.
        '
        '    Output parameters:
        '        W   -   array of the barycentric weights which  can  be  used  in  the
        '                BarycentriConvert.ToInt32erpolate subroutine. Array[0..N-1]
        '
        '    Note:
        '        this algorithm always succeeds and calculates the weights  with  close
        '        to machine precision.
        '
        '      -- ALGLIB PROJECT --
        '         Copyright 17.06.2007 by Bochkanov Sergey
        '    ************************************************************************

        Public Shared Sub buildfloaterhormannrationalinterpolant(ByVal x As Double(), ByVal n As Integer, ByVal d As Integer, ByRef w As Double())
            Dim s0 As Double = 0
            Dim s As Double = 0
            Dim v As Double = 0
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim k As Integer = 0
            Dim perm As Integer() = New Integer(-1) {}
            Dim wtemp As Double() = New Double(-1) {}
            Dim i_ As Integer = 0

            x = DirectCast(x.Clone(), Double())

            System.Diagnostics.Debug.Assert(n > 0, "BuildRationalInterpolantWithoutPoles: N<=0!")
            System.Diagnostics.Debug.Assert(d >= 0 And d <= n, "BuildRationalInterpolantWithoutPoles: incorrect D!")

            '
            ' Prepare
            '
            w = New Double(n - 1) {}
            s0 = 1
            For k = 1 To d
                s0 = -s0
            Next
            perm = New Integer(n - 1) {}
            For i = 0 To n - 1
                perm(i) = i
            Next
            For i = 0 To n - 2
                For j = i + 1 To n - 1
                    If x(j) < x(i) Then
                        s = x(i)
                        x(i) = x(j)
                        x(j) = s
                        k = perm(i)
                        perm(i) = perm(j)
                        perm(j) = k
                    End If
                Next
            Next

            '
            ' Calculate Wk
            '
            For k = 0 To n - 1

                '
                ' Wk
                '
                s = 0
                For i = Math.Max(k - d, 0) To Math.Min(k, n - 1 - d)
                    v = 1
                    For j = i To i + d
                        If j <> k Then
                            v = v / Math.Abs(x(k) - x(j))
                        End If
                    Next
                    s = s + v
                Next
                w(k) = s0 * s

                '
                ' Next S0
                '
                s0 = -s0
            Next

            '
            ' Reorder W
            '
            wtemp = New Double(n - 1) {}
            For i_ = 0 To n - 1
                wtemp(i_) = w(i_)
            Next
            For i = 0 To n - 1
                w(perm(i)) = wtemp(i)
            Next
        End Sub
    End Class

End Namespace
