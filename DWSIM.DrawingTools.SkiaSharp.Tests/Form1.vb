Imports SkiaSharp.Views.Desktop
Imports SkiaSharp
Imports SkiaSharp.Extended

Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub SkControl1_PaintSurface(sender As Object, e As SKPaintSurfaceEventArgs) Handles SkControl1.PaintSurface

        Dim canvas = e.Surface.Canvas

        canvas.Clear(SKColors.White)

        Dim w = 100.0
        Dim h = 100.0

        Dim x = 100.0
        Dim y = 100.0

        Dim center = New SKPoint(x + w / 2, y + h / 2)

        Dim radius = w / 2

        'draw circle
        Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = True, .StrokeWidth = 5, .IsAntialias = True}
            canvas.DrawCircle(center.X, center.Y, radius, paint)
        End Using

        Using paint As New SKPaint With {.Color = SKColors.Green.WithAlpha(150), .IsStroke = True, .StrokeWidth = 20, .IsAntialias = True}
            Dim p As New SKPath()
            p.AddArc(New SKRect(x + 10.0, y + 10.0, x + w - 10.0, y + h - 10.0), -225, 90)
            canvas.DrawPath(p, paint)
        End Using

        Using paint As New SKPaint With {.Color = SKColors.Yellow.WithAlpha(150), .IsStroke = True, .StrokeWidth = 20, .IsAntialias = True}
            Dim p As New SKPath()
            p.AddArc(New SKRect(x + 10.0, y + 10.0, x + w - 10.0, y + h - 10.0), -135, 90)
            canvas.DrawPath(p, paint)
        End Using

        Using paint As New SKPaint With {.Color = SKColors.Red.WithAlpha(150), .IsStroke = True, .StrokeWidth = 20, .IsAntialias = True}
            Dim p As New SKPath()
            p.AddArc(New SKRect(x + 10.0, y + 10.0, x + w - 10.0, y + h - 10.0), -45, 90)
            canvas.DrawPath(p, paint)
        End Using

        Dim needle = SKPath.ParseSvgPathData("M13.886,84.243L2.83,83.875c0,0,3.648-70.77,3.956-74.981C7.104,4.562,7.832,0,8.528,0c0.695,0,1.752,4.268,2.053,8.894C10.883,13.521,13.886,84.243,13.886,84.243z")
        needle.AddPath(SKPath.ParseSvgPathData("M16.721,85.475c0,4.615-3.743,8.359-8.36,8.359S0,90.09,0,85.475c0-4.62,3.743-8.363,8.36-8.363S16.721,80.855,16.721,85.475z"))
        Dim bounds As SKRect = SKRect.Empty
        needle.GetBounds(bounds)
        Dim sf = radius / bounds.Height * 0.9
        needle.Transform(SKMatrix.MakeScale(sf, sf))
        needle.GetBounds(bounds)
        needle.Offset(center)
        needle.Offset(-bounds.Width / 2, -bounds.Height + 5)

        Dim mintick = -135.0
        Dim maxtick = 135.0
        Dim nstep = 10

        Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = True, .StrokeWidth = 2, .IsAntialias = True}
            For i As Integer = mintick To maxtick Step nstep
                Dim angle = (i - 90) * Math.PI / 180
                Dim p1 = New SKPoint(center.X + radius * Math.Cos(angle), center.Y + radius * Math.Sin(angle))
                Dim p2 = New SKPoint(center.X + (radius - 10.0) * Math.Cos(angle), center.Y + (radius - 10.0) * Math.Sin(angle))
                canvas.DrawLine(p1, p2, paint)
            Next
        End Using

        Dim minvalue = -300
        Dim maxvalue = 400
        Dim currentvalue = -300

        Dim currenttick = mintick + (currentvalue - minvalue) / (maxvalue - minvalue) * (maxtick - mintick)

        needle.Transform(SKMatrix.MakeRotationDegrees(currenttick, center.X, center.Y))

        Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
            canvas.DrawPath(needle, paint)
        End Using

        Dim valtext = currentvalue.ToString("G")

        Using paint As New SKPaint With {.TextSize = 13.0, .Color = SKColors.Black, .IsAntialias = True}
            paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
            Dim trect As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
            Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)
            Dim strx As Single = (w - paint.MeasureText(valtext)) / 2
            Dim stry As Single = h - tsize.Height - 20.0
            canvas.DrawText(valtext, x + strx, y + stry, paint)
        End Using


    End Sub

End Class
