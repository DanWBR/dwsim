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

        Dim x = 100
        Dim y = 100

        Dim w = 100
        Dim h = 100

        Dim tll = "LL"
        Dim tl = "L"
        Dim th = "H"
        Dim thh = "HH"

        Using paint As New SKPaint With {.TextSize = 8, .Color = SKColors.White, .IsAntialias = True}
            Dim trll As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(tll, 0, 0).GetBounds(trll)
            Using bpaint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawOval(x, y - trll.Height / 2, trll.Height, trll.Height, bpaint)
            End Using
            canvas.DrawText(tll, x - trll.Width / 2, y, paint)
        End Using

        Using paint As New SKPaint With {.TextSize = 8, .Color = SKColors.White, .IsAntialias = True}
            Dim trl As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(tl, 0, 0).GetBounds(trl)
            Using bpaint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawOval(x + 0.33 * w, y - trl.Height / 2, trl.Height, trl.Height, bpaint)
            End Using
            canvas.DrawText(tl, x + 0.33 * w - trl.Width / 2, y, paint)
        End Using

        Using paint As New SKPaint With {.TextSize = 8, .Color = SKColors.White, .IsAntialias = True}
            Dim trh As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(th, 0, 0).GetBounds(trh)
            Using bpaint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawOval(x + 0.66 * w, y - trh.Height / 2, trh.Height, trh.Height, bpaint)
            End Using
            canvas.DrawText(th, x + 0.66 * w - trh.Width / 2, y, paint)
        End Using

        Using paint As New SKPaint With {.TextSize = 8, .Color = SKColors.White, .IsAntialias = True}
            Dim trhh As New SKRect(0, 0, 2, 2)
            paint.GetTextPath(th, 0, 0).GetBounds(trhh)
            Using bpaint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawOval(x + w, y - trhh.Height / 2, trhh.Height, trhh.Height, bpaint)
            End Using
            canvas.DrawText(thh, x + w - trhh.Width / 2, y, paint)
        End Using

    End Sub

End Class
