Imports SkiaSharp.Views.Desktop
Imports SkiaSharp
Imports SkiaSharp.Extended
Imports DWSIM

Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub SkControl1_PaintSurface(sender As Object, e As SKPaintSurfaceEventArgs) Handles SkControl1.PaintSurface

        Dim canvas = e.Surface.Canvas

        canvas.Clear(SKColors.White)

        Dim w = 20
        Dim h = 70

        Dim x = 100.0
        Dim y = 100.0

        Dim origin = x + w * 0.4

        Dim minvalue = 0.0
        Dim maxvalue = 400.0
        Dim currentvalue = 254.0

        Dim minvaltext = minvalue.ToString("N0")
        Dim maxvaltext = maxvalue.ToString("N0")
        Dim valtext = currentvalue.ToString("N0")

        Using paint2 As New SKPaint With {.Color = SKColors.LightGray, .IsStroke = False, .IsAntialias = True}
            canvas.DrawRect(x + origin, y, w / 2, h, paint2)
        End Using

        Dim currentlevel = (currentvalue - minvalue) / (maxvalue - minvalue) * h

        Dim clx = x + origin + w / 2 + 6
        Dim cly = y + (h - currentlevel)

        Using paint2 As New SKPaint With {.Color = SKColors.Blue, .IsStroke = False, .IsAntialias = True}
            canvas.DrawRect(x + origin + 2, cly + 2, w / 2 - 4, currentlevel - 4, paint2)
        End Using

        canvas.Save()

        canvas.RotateDegrees(-90, clx, cly)

        Using paint2 As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
            canvas.DrawTriangle(x + origin + w / 2 + 6, cly, 4, paint2)
        End Using

        canvas.Restore()

        Using paint As New SKPaint With {.TextSize = 10.0, .Color = SKColors.Black, .IsAntialias = True}
            Dim trect As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
            Dim stry As Single = cly + trect.Height / 2
            canvas.DrawText(minvaltext, x + origin + w / 2 + 10, y + h, paint)
            canvas.DrawText(maxvaltext, x + origin + w / 2 + 10, y + trect.Height / 2, paint)
            canvas.DrawText(valtext, x + origin + w / 2 + 10, stry, paint)
        End Using

    End Sub

End Class
