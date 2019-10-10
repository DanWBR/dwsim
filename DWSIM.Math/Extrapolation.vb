Imports DWSIM.MathOps.MathEx

Public Class Extrapolation

    Public Shared Function Extrapolate(equation As Func(Of Double, Double), xpoints() As Double, x_extrap As Double) As Double

        Dim y As New List(Of Double)

        For Each x In xpoints
            y.Add(equation.Invoke(x))
        Next

        Dim y_extrap = Interpolation.polinterpolation.nevilleinterpolation(xpoints, y.ToArray, 5, x_extrap)

        Return y_extrap

    End Function

End Class
