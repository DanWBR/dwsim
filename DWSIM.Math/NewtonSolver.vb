'    Copyright 2020 Daniel Wagner O. de Medeiros
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

Namespace MathEx.Optimization

    Public Class NewtonSolver

        Public Property Tolerance As Double = 0.0001

        Public Property MaxIterations As Integer = 100

        Public Property EnableDamping As Boolean = True

        Public Property UseBroydenApproximation As Boolean = False

        Public Property ExpandFactor As Double = 1.5

        Public Property MaximumDelta As Double = 0.5

        Public Property Epsilon As Double = Double.NaN

        Private _Iterations As Integer = 0

        Private fxb As Func(Of Double(), Double())

        Private broydengrad As Double(,)

        Private brentsolver As New BrentOpt.BrentMinimize

        Private tmpx As Double(), tmpdx As Double()

        Private _jacobian As Boolean

        Private dfdx As Func(Of Double(), Double(,))

        Private _error As Double

        Private _jac As Double(,)

        Public ReadOnly Property Jacobian As Double(,)
            Get
                Return _jac
            End Get
        End Property

        Public ReadOnly Property BuildingJacobian As Boolean
            Get
                Return _jacobian
            End Get
        End Property

        Public ReadOnly Property Iterations As Integer
            Get
                Return _Iterations
            End Get
        End Property

        Sub New()

            brentsolver.DefineFuncDelegate(AddressOf minimizeerror)

        End Sub

        Public Sub Reset()

            _Iterations = 0
            _error = 0.0

        End Sub

        Public Shared Function FindRoots(functionbody As Func(Of Double(), Double()), vars As Double(),
                                         maxits As Integer, tol As Double) As Double()

            Dim newton As New NewtonSolver
            newton.Tolerance = tol
            newton.MaxIterations = maxits

            Return newton.Solve(functionbody, vars)

        End Function

        ''' <summary>
        ''' Solves a system of non-linear equations [f(x) = 0] using newton's method.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of double, returns the error values for each x</param>
        ''' <param name="vars">initial values for x</param>
        ''' <returns>vector of variables which solve the equations according to the minimum allowable error value (tolerance).</returns>
        Function Solve(functionbody As Func(Of Double(), Double()), vars As Double()) As Double()

            Dim dfacs As Double() = New Double() {0.1, 0.2, 0.4, 0.6, 0.8, 1.0}
            Dim epsilons As Double() = New Double() {0.000000000001, 0.00000001, 0.0001, 0.001, 0.01, 0.1}

            Dim leave As Boolean = False
            Dim finalx As Double() = vars

            dfdx = Nothing

            If Not Double.IsNaN(Epsilon) Then epsilons = New Double() {Epsilon}

            If EnableDamping Then
                For Each d In dfacs
                    If leave Then Exit For
                    For Each eps In epsilons
                        If leave Then Exit For
                        Try
                            finalx = solve_internal(d, eps, functionbody, vars)
                            leave = True
                        Catch ex As ArgumentException
                            'try next parameters
                        End Try
                    Next
                Next
            Else
                For Each eps In epsilons
                    If leave Then Exit For
                    Try
                        finalx = solve_internal(1.0, eps, functionbody, vars)
                        leave = True
                    Catch ex As ArgumentException
                        'try next parameters
                    End Try
                Next
            End If

            If Not leave Then Throw New Exception("Newton Convergence Error")

            Return finalx

        End Function

        ''' <summary>
        ''' Solves a system of non-linear equations [f(x) = 0] using newton's method.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of double, returns the error values for each x</param>
        ''' <param name="vars">initial values for x</param>
        ''' <returns>vector of variables which solve the equations according to the minimum allowable error value (tolerance).</returns>
        Function Solve(functionbody As Func(Of Double(), Double()), functiongradient As Func(Of Double(), Double(,)), vars As Double()) As Double()

            Dim dfacs As Double() = New Double() {0.1, 0.2, 0.4, 0.6, 0.8, 1.0}
            Dim epsilons As Double() = New Double() {0.000000000001, 0.00000001, 0.0001, 0.001, 0.01, 0.1}

            Dim leave As Boolean = False
            Dim finalx As Double() = vars

            dfdx = functiongradient

            If EnableDamping Then
                For Each d In dfacs
                    If leave Then Exit For
                    For Each eps In epsilons
                        If leave Then Exit For
                        Try
                            finalx = solve_internal(d, eps, functionbody, vars)
                            leave = True
                        Catch ex As ArgumentException
                            'try next parameters
                        End Try
                    Next
                Next
            Else
                For Each eps In epsilons
                    If leave Then Exit For
                    Try
                        finalx = solve_internal(1.0, eps, functionbody, vars)
                        leave = True
                    Catch ex As ArgumentException
                        'try next parameters
                    End Try
                Next
            End If

            If Not leave Then Throw New Exception("Newton Convergence Error")

            Return finalx

        End Function


        Private Function solve_internal(mindamp As Double, epsilon As Double, functionbody As Func(Of Double(), Double()), vars As Double()) As Double()

            fxb = functionbody

            Dim fx(), x(), dx(), dfdx(,), df, fxsum, fxsum0 As Double
            Dim success As Boolean = False

            x = vars.Clone

            dx = x.Clone

            _Iterations = 0

            Do

                If _Iterations = 0 Then
                    fxsum0 = 1.0E+20
                Else
                    fxsum0 = MathEx.Common.SumSqr(fx)
                End If

                _jacobian = False

                fx = fxb.Invoke(x)

                _error = MathEx.Common.SumSqr(fx)
                fxsum = _error

                If fxsum < Tolerance Then
                    Exit Do
                End If

                _jacobian = True

                dfdx = gradient(epsilon, x, fx)

                Dim A = MathNet.Numerics.LinearAlgebra.Matrix(Of Double).Build.DenseOfArray(dfdx)
                Dim B = MathNet.Numerics.LinearAlgebra.Vector(Of Double).Build.DenseOfArray(fx)

                dx = A.Solve(B).ToArray()

                'SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)

                'If success Then

                If Common.SumSqr(dx) < Tolerance And _Iterations > MaxIterations / 2 Then
                    Exit Do
                End If

                If EnableDamping Then
                    If _Iterations > 5 Then
                        df = df * ExpandFactor
                        If df > 1.0 Then df = 1.0
                    Else
                        df = mindamp
                    End If
                Else
                    df = 1.0#
                End If

                For i = 0 To x.Length - 1
                    If Math.Abs(x(i)) < 1.0E-20 Then
                        x(i) -= dx(i) * df
                    Else
                        If Math.Abs(dx(i) / x(i)) > MaximumDelta Then
                            dx(i) = Math.Sign(dx(i)) * Math.Abs(x(i)) * MaximumDelta
                        End If
                        x(i) -= dx(i) * df
                    End If
                Next

                'Else

                '    For i = 0 To x.Length - 1
                '        x(i) *= 0.999
                '    Next

                'End If

                _Iterations += 1

                If _Iterations > 50 And fxsum > fxsum0 Then
                    Throw New ArgumentException("not converging")
                End If

                If Double.IsNaN(fxsum) Then
                    Throw New ArgumentException("not converging")
                End If

            Loop Until _Iterations > MaxIterations

            If _Iterations > MaxIterations Then
                Throw New ArgumentException("not converged")
            End If

            If dfdx Is Nothing Then dfdx = gradient(epsilon, x, fx)

            _jac = dfdx

            Return x

        End Function

        Private Function gradient(epsilon As Double, ByVal x() As Double, fx() As Double) As Double(,)

            Dim f1(), f2() As Double
            Dim g(x.Length - 1, x.Length - 1), x1(x.Length - 1), x2(x.Length - 1), dx(x.Length - 1), xbr(x.Length - 1), fbr(x.Length - 1) As Double
            Dim i, j, k, n As Integer

            n = x.Length - 1

            If UseBroydenApproximation Then

                If broydengrad Is Nothing Then broydengrad = g.Clone()

                If _Iterations = 0 Then
                    For i = 0 To n
                        For j = 0 To n
                            If i = j Then broydengrad(i, j) = 1.0 Else broydengrad(i, j) = 0.0
                        Next
                    Next
                    Broyden.broydn(n, x, fx, dx, xbr, fbr, broydengrad, 0)
                Else
                    Broyden.broydn(n, x, fx, dx, xbr, fbr, broydengrad, 1)
                End If

                Return broydengrad

            Else

                If dfdx IsNot Nothing Then

                    g = dfdx.Invoke(x)

                Else

                    For i = 0 To x.Length - 1
                        For j = 0 To x.Length - 1
                            If i <> j Then
                                x1(j) = x(j)
                                x2(j) = x(j)
                            Else
                                If x(j) = 0.0# Then
                                    x1(j) = epsilon
                                    x2(j) = 2 * epsilon
                                Else
                                    x1(j) = x(j) * (1 - epsilon)
                                    x2(j) = x(j) * (1 + epsilon)
                                End If
                            End If
                        Next
                        f1 = fxb.Invoke(x1)
                        f2 = fxb.Invoke(x2)
                        For k = 0 To x.Length - 1
                            g(k, i) = (f2(k) - f1(k)) / (x2(i) - x1(i))
                        Next
                    Next

                End If

            End If

            Return g

        End Function

        Public Function minimizeerror(ByVal t As Double) As Double

            Dim tmpx0 As Double() = tmpx.Clone

            For i = 0 To tmpx.Length - 1
                tmpx0(i) -= tmpdx(i) * t
            Next

            Dim abssum0 = MathEx.Common.SumSqr(fxb.Invoke(tmpx0))
            If Double.IsNaN(abssum0) Then abssum0 = 1.0E+20
            Return abssum0

        End Function

    End Class

    Public Class NewtonSolver_Old

        Public Property Tolerance As Double = 0.0001

        Public Property MaxIterations As Integer = 1000

        Public Property EnableDamping As Boolean = True

        Private _Iterations As Integer = 0

        Private fxb As Func(Of Double(), Double())

        Private brentsolver As New BrentOpt.BrentMinimize

        Private tmpx As Double(), tmpdx As Double()

        Private _error As Double

        Public ReadOnly Property Iterations
            Get
                Return _Iterations
            End Get
        End Property

        Sub New()

            brentsolver.DefineFuncDelegate(AddressOf minimizeerror)

        End Sub

        ''' <summary>
        ''' Solves a system of non-linear equations [f(x) = 0] using newton's method.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of double, returns the error values for each x</param>
        ''' <param name="vars">initial values for x</param>
        ''' <returns>vector of variables which solve the equations according to the minimum allowable error value (tolerance).</returns>
        Function Solve(functionbody As Func(Of Double(), Double()), vars As Double()) As Double()

            Dim minimaldampings As Double() = New Double() {1.0E-20, 0.000000000000001, 0.0000000001, 0.00001, 0.0001, 0.001, 0.01, 0.1}
            Dim epsilons As Double() = New Double() {0.0000000001, 0.000000001, 0.00000001, 0.0000001, 0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1}

            Dim leave As Boolean = False
            Dim finalx As Double() = vars

            If EnableDamping Then
                For Each mindamp In minimaldampings
                    If leave Then Exit For
                    For Each eps In epsilons
                        If leave Then Exit For
                        Try
                            finalx = solve_internal(mindamp, eps, functionbody, vars)
                            leave = True
                        Catch ex As ArgumentException
                            'try next parameters
                        End Try
                    Next
                Next
            Else
                For Each eps In epsilons
                    If leave Then Exit For
                    Try
                        finalx = solve_internal(1.0, eps, functionbody, vars)
                        leave = True
                    Catch ex As ArgumentException
                        'try next parameters
                    End Try
                Next
            End If

            If Not leave Then Throw New Exception("newton convergence error")

            Return finalx

        End Function

        Private Function solve_internal(mindamp As Double, epsilon As Double, functionbody As Func(Of Double(), Double()), vars As Double()) As Double()

            fxb = functionbody

            Dim fx(), x(), dx(), dfdx(,), df, fxsum, fxsum0 As Double
            Dim success As Boolean = False

            x = vars.Clone

            dx = x.Clone

            _Iterations = 0

            Do

                If _Iterations = 0 Then
                    fxsum0 = 1.0E+20
                Else
                    fxsum0 = MathEx.Common.SumSqr(fx)
                End If

                fx = fxb.Invoke(x)

                _error = MathEx.Common.SumSqr(fx)
                fxsum = _error

                If Common.SumSqr(fx) < Tolerance Then Exit Do

                dfdx = gradient(epsilon, x)

                success = SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)

                If success Then

                    'this call to the brent solver calculates the damping factor which minimizes the error (fval).

                    If EnableDamping Then

                        tmpx = x.Clone
                        tmpdx = dx.Clone
                        brentsolver.brentoptimize(mindamp, 1.0, mindamp / 10.0#, df)

                    Else

                        df = 1.0#

                    End If

                    For i = 0 To x.Length - 1
                        x(i) -= dx(i) * df
                    Next

                Else

                    For i = 0 To x.Length - 1
                        x(i) *= 0.999
                    Next

                End If

                _Iterations += 1

                If _Iterations > 50 And fxsum > fxsum0 Then
                    Throw New ArgumentException("not converging")
                End If

                If Double.IsNaN(fxsum) Then
                    Throw New ArgumentException("not converging")
                End If

            Loop Until _Iterations > MaxIterations

            If _Iterations > MaxIterations Then
                Throw New ArgumentException("not converged")
            End If

            Return x

        End Function

        Private Function gradient(epsilon As Double, ByVal x() As Double) As Double(,)

            Dim f1(), f2() As Double
            Dim g(x.Length - 1, x.Length - 1), x2(x.Length - 1) As Double
            Dim i, j, k As Integer

            f1 = fxb.Invoke(x)
            For i = 0 To x.Length - 1
                For j = 0 To x.Length - 1
                    If i <> j Then
                        x2(j) = x(j)
                    Else
                        If x(j) = 0.0# Then
                            x2(j) = epsilon
                        Else
                            x2(j) = x(j) * (1 + epsilon)
                        End If
                    End If
                Next
                f2 = fxb.Invoke(x2)
                For k = 0 To x.Length - 1
                    g(k, i) = (f2(k) - f1(k)) / (x2(i) - x(i))
                Next
            Next

            Return g

        End Function

        Public Function minimizeerror(ByVal t As Double) As Double

            Dim tmpx0 As Double() = tmpx.Clone

            For i = 0 To tmpx.Length - 1
                tmpx0(i) -= tmpdx(i) * t
            Next

            Dim abssum0 = MathEx.Common.SumSqr(fxb.Invoke(tmpx0))
            If Double.IsNaN(abssum0) Then abssum0 = 1.0E+20
            Return abssum0

        End Function

    End Class


End Namespace