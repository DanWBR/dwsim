
Public Class ObjectiveFunction

    Inherits LibOptimization.Optimization.absObjectiveFunction

    Private fxb As Func(Of Double(), Double)

    Private fxg As Func(Of Double(), Double())

    Private epsilon As Double

    Private n As Integer

    Public Sub New(func1 As Func(Of Double(), Double), deriv1 As Func(Of Double(), Double()), eps As Double, nvars As Integer)

        fxb = func1
        fxg = deriv1
        epsilon = eps
        n = nvars

    End Sub

    Public Overrides Function F(ByVal a As List(Of Double)) As Double

        Return fxb.Invoke(a.ToArray())

    End Function

    Public Overrides Function Gradient(x As List(Of Double)) As List(Of Double)

        If fxg IsNot Nothing Then
            Return fxg.Invoke(x.ToArray()).ToList()
        Else
            Return FunctionGradient(x.ToArray()).ToList()
        End If

    End Function

    Public Overrides Function Hessian(x As List(Of Double)) As List(Of List(Of Double))

        Return Nothing

    End Function

    Public Overrides Function NumberOfVariable() As Integer

        Return n

    End Function

    Private Function FunctionGradient(ByVal x() As Double) As Double()

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
