Public Class BFGSBMinimizer

    Public Property Tolerance As Double = 0.0001

    Public Property MaxIterations As Integer = 1000

    Public Property ReturnLowestObjFuncValue As Boolean = True

    Private _Iterations As Integer = 0

    Private fxb As Func(Of Double(), Double)

    Private fxg As Func(Of Double(), Double())

    Private _error As Double

    Private objval, objval0 As Double

    Public ReadOnly Property Iterations As Integer
        Get
            Return _Iterations
        End Get
    End Property

    Sub New()

    End Sub

    Public Shared Function FindRoots(functionbody As Func(Of Double(), Double), vars As Double(), maxits As Integer, tol As Double,
                                     Optional lbounds As Double() = Nothing, Optional ubounds As Double() = Nothing) As Double()

        Dim bfgsb As New BFGSBMinimizer
        bfgsb.Tolerance = tol
        bfgsb.MaxIterations = maxits

        Return bfgsb.Solve(functionbody, Nothing, vars, lbounds, ubounds)

    End Function


    ''' <summary>
    ''' Minimizes a function value using IPOPT solver.
    ''' </summary>
    ''' <param name="functionbody">f(x) where x is a vector of doubles, returns the value of the function.</param>
    ''' <param name="functiongradient">Optional. g(x) where x is a vector of doubles, returns the value of the gradient of the function with respect to each variable.</param>
    ''' <param name="vars">initial values for x</param>
    ''' <param name="lbounds">lower bounds for x</param>
    ''' <param name="ubounds">upper bounds for x</param>
    ''' <returns>vector of variables corresponding to the function's minimum value.</returns>
    Public Function Solve(functionbody As Func(Of Double(), Double), functiongradient As Func(Of Double(), Double()), vars As Double(), Optional lbounds As Double() = Nothing, Optional ubounds As Double() = Nothing) As Double()

        _Iterations = 0

        Dim obj As Double = 0.0#

        fxb = functionbody
        fxg = functiongradient

        If functiongradient Is Nothing Then
            fxg = Function(xv)
                      Return FunctionGradientInternal(xv)
                  End Function
        Else
            fxg = functiongradient
        End If

        If lbounds Is Nothing Then
            lbounds = vars.Clone()
            For i As Integer = 0 To lbounds.Length - 1
                lbounds(i) = -1.0E+19
            Next
        End If

        If ubounds Is Nothing Then
            ubounds = vars.Clone()
            For i As Integer = 0 To ubounds.Length - 1
                ubounds(i) = 1.0E+19
            Next
        End If

        Dim slv As New MathNet.Numerics.Optimization.BfgsBMinimizer(Tolerance, Tolerance, Tolerance, MaxIterations)

        Dim objf = MathNet.Numerics.Optimization.ObjectiveFunction.Gradient(Function(xvec)
                                                                                Return fxb.Invoke(xvec.ToArray())
                                                                            End Function, Function(xvec)
                                                                                              Return New MathNet.Numerics.LinearAlgebra.[Double].DenseVector(fxg.Invoke(xvec.ToArray()))
                                                                                          End Function)

        Dim solution = slv.FindMinimum(objf, New MathNet.Numerics.LinearAlgebra.[Double].DenseVector(lbounds),
                                                           New MathNet.Numerics.LinearAlgebra.[Double].DenseVector(ubounds),
                                                           New MathNet.Numerics.LinearAlgebra.[Double].DenseVector(vars))

        vars = solution.MinimizingPoint.ToArray()

        Return vars

    End Function

    Private Function FunctionGradientInternal(ByVal x() As Double) As Double()

        Dim epsilon As Double = 0.001

        Dim f1, f2 As Double
        Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
        Dim j, k As Integer

        For j = 0 To x.Length - 1
            For k = 0 To x.Length - 1
                x1(k) = x(k)
                x2(k) = x(k)
            Next
            If x(j) <> 0.0# Then
                x1(j) = x(j) * (1.0# + epsilon)
                x2(j) = x(j) * (1.0# - epsilon)
            Else
                x1(j) = x(j) + epsilon
                x2(j) = x(j) - epsilon
            End If
            f1 = fxb.Invoke(x1)
            f2 = fxb.Invoke(x2)
            g(j) = (f2 - f1) / (x2(j) - x1(j))
        Next

        Return g

    End Function

End Class

