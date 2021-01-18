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

Namespace MathEx.Newton

    Public Class NewtonSolver

        Public Property Tolerance As Double = 0.0001

        Public Property MaxIterations As Integer = 1000

        Public Property MaxPercentChange As Double = 10

        Public Property DampingFactor As Double = 1.0

        Public Property DerivativePerturbation = 0.01

        Private _Iterations As Integer = 0

        Private fxb As Func(Of Double(), Double())

        Public ReadOnly Property Iterations
            Get
                Return _Iterations
            End Get
        End Property

        Sub New()

        End Sub

        ''' <summary>
        ''' Solves a system of non-linear equations [f(x) = 0] using newton's method.
        ''' </summary>
        ''' <param name="functionbody">f(x) where x is a vector of double, returns the error values for each x</param>
        ''' <param name="vars">initial values for x</param>
        ''' <returns>vector of variables which solve the equations according to the minimum allowable error value (tolerance).</returns>
        Function Solve(functionbody As Func(Of Double(), Double()), vars As Double()) As Double()

            fxb = functionbody

            Dim fx(), x(), dx(), dfdx(,) As Double
            Dim success As Boolean = False

            x = vars

            dx = x.Clone

            _Iterations = 0

            Do

                fx = fxb.Invoke(x)

                If Common.SumSqr(fx) < Tolerance Then Exit Do

                dfdx = gradient(x)

                success = SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)

                If success Then

                    Dim dontupdate As Boolean = False

                    For i = 0 To x.Length - 1
                        If Math.Abs(dx(i) / x(i) * 100) > MaxPercentChange And x(i) <> 0.0 Then
                            dontupdate = True
                            Exit For
                        End If
                    Next

                    If dontupdate Then
                        For i = 0 To x.Length - 1
                            x(i) -= Math.Sign(dx(i)) * x(i) * MaxPercentChange / 100
                        Next
                    Else
                        For i = 0 To x.Length - 1
                            x(i) -= dx(i) * DampingFactor
                        Next
                    End If

                Else

                    For i = 0 To x.Length - 1
                        x(i) *= 0.999
                    Next

                End If

                _Iterations += 1

            Loop Until _Iterations > MaxIterations

            Return x

        End Function

        Private Function gradient(ByVal x() As Double) As Double(,)

            Dim epsilon As Double = DerivativePerturbation

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

    End Class


End Namespace

