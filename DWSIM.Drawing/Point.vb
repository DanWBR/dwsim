<System.Serializable> Public Class Point

    Implements Interfaces.IPoint

    Sub New()

    End Sub

    Sub New(x As Double, y As Double)
        Me.X = x
        Me.Y = y
    End Sub

    Function ToSDPoint() As System.Drawing.Point
        Return New System.Drawing.Point(X, Y)
    End Function

    Function ToSDPointF() As System.Drawing.PointF
        Return New System.Drawing.PointF(X, Y)
    End Function

    Public Property X As Double Implements Interfaces.IPoint.X

    Public Property Y As Double Implements Interfaces.IPoint.Y

    Sub Offset(p1 As Double, p2 As Double)
        X += p1
        Y += p2
    End Sub

End Class
