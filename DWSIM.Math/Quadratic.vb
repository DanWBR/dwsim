Imports System.Threading

Public Class Quadratic
    Public Shared Function quadForm(ByVal a As Double, ByVal b As Double, ByVal c As Double, ByVal pos As Boolean) As Double
        Dim preRoot = ((b * b) - (4 * (a * c)))
        If (preRoot < 0) Then
            Return Double.NaN
        Else
            Dim sgn As Double = 1.0
            If Not pos Then sgn = -1.0
            Return ((sgn * Math.Sqrt(preRoot)) - b) / (2 * a)
        End If
    End Function

End Class
