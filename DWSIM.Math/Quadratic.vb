Imports System.Threading

Public Class Quadratic
    Public Shared Function quadForm(ByVal a As Double, ByVal b As Double, ByVal c As Double) As Tuple(Of Double, Double)
        Dim preRoot = ((b * b) - (4 * (a * c)))
        If (preRoot < 0) Then
            Return New Tuple(Of Double, Double)(Double.NaN, Double.NaN)
        Else
            Dim r1 = ((Math.Sqrt(preRoot)) - b) / (2 * a)
            Dim r2 = ((-Math.Sqrt(preRoot)) - b) / (2 * a)
            Return New Tuple(Of Double, Double)(r1, r2)
        End If
    End Function

End Class
