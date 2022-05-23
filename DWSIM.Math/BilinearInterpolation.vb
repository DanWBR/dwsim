Imports DWSIM.DrawingTools.Point

Public Class BilinearInterpolation

    Public Shared Function Interpolate(p As Point, points As List(Of Tuple(Of Point, Double))) As Double

        Dim distances As New List(Of Double)
        Dim d As Double
        For Each item In points
            d = Math.Sqrt(Math.Pow(item.Item1.X - p.X, 2) + Math.Pow(item.Item1.Y - p.Y, 2))
            distances.Add(d)
        Next
        Dim val As Double = 0
        For i As Integer = 0 To points.Count - 1
            val += distances(i) / distances.Sum * points(i).Item2
        Next
        Return val

    End Function

End Class
