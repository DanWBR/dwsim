Public Class Point

    Implements DWSIM.Interfaces.IPoint

    'Private _position As SkiaSharp.SKPoint

    'Sub New(position As SkiaSharp.SKPoint)
    '    ' TODO: Complete member initialization 
    '    _position = position
    'End Sub

    Public Property X As Double Implements DWSIM.Interfaces.IPoint.X

    Public Property Y As Double Implements DWSIM.Interfaces.IPoint.Y

    'Public Sub FromSKPoint(p As SKPoint)
    '    X = p.X
    '    Y = p.Y
    'End Sub

    'Public Function ToSKPoint() As SKPoint
    '    Return New SKPoint(X, Y)
    'End Function

    'Sub New(p As SKPoint)
    '    X = p.X
    '    Y = p.Y
    'End Sub

    Sub New([x] As Single, [y] As Single)
        Me.X = [x]
        Me.Y = [y]
    End Sub

    Sub New()

    End Sub

End Class
