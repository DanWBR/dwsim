Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    Public Class AnalogGaugeGraphic

        Inherits ShapeGraphic


#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal w As Double, h As Double)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.SetSize(New SKSize(w, h))
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal w As Double, h As Double)
            Me.New()
            Me.SetPosition(New SKPoint(posX, posY))
            Me.SetSize(New SKSize(w, h))
        End Sub

#End Region

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim w = Width
            Dim h = Height

            Dim center = New SKPoint(X + w / 2, Y + h / 2)

            Dim radius = w / 2

            Dim f = Width / 100.0

            'draw circle
            Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = True, .StrokeWidth = 5 * f, .IsAntialias = True}
                canvas.DrawCircle(center.X, center.Y, radius, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Green.WithAlpha(75), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -225, 90)
                canvas.DrawPath(p, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Yellow.WithAlpha(75), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -135, 90)
                canvas.DrawPath(p, paint)
            End Using

            Using paint As New SKPaint With {.Color = SKColors.Red.WithAlpha(75), .IsStroke = True, .StrokeWidth = 20 * f, .IsAntialias = True}
                Dim p As New SKPath()
                p.AddArc(New SKRect(X + 10.0 * f, Y + 10.0 * f, X + w - 10.0 * f, Y + h - 10.0 * f), -45, 90)
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
            needle.Offset(-bounds.Width / 2, -bounds.Height + 5 * f)

            Dim mintick = -135.0
            Dim maxtick = 135.0
            Dim nstep = 10

            Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = True, .StrokeWidth = 2 * f, .IsAntialias = True}
                For i As Integer = mintick To maxtick Step nstep
                    Dim angle = (i - 90) * Math.PI / 180
                    Dim p1 = New SKPoint(center.X + radius * Math.Cos(angle), center.Y + radius * Math.Sin(angle))
                    Dim p2 = New SKPoint(center.X + (radius - 10.0 * f) * Math.Cos(angle), center.Y + (radius - 10.0 * f) * Math.Sin(angle))
                    canvas.DrawLine(p1, p2, paint)
                Next
            End Using

            Dim owneri = DirectCast(Owner, IIndicator)

            Dim minvalue = owneri.MinimumValue
            Dim maxvalue = owneri.MaximumValue
            Dim currentvalue As Double
            If owneri.SelectedObject IsNot Nothing Then
                currentvalue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(owneri.SelectedPropertyUnits, owneri.SelectedObject.GetPropertyValue(owneri.SelectedProperty))
            End If

            Dim currenttick = mintick + (currentvalue - minvalue) / (maxvalue - minvalue) * (maxtick - mintick)

            needle.Transform(SKMatrix.MakeRotationDegrees(currenttick, center.X, center.Y))

            Using paint As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawPath(needle, paint)
            End Using

            Dim valtext = currentvalue.ToString("G")

            Using paint As New SKPaint With {.TextSize = 13.0 * f, .Color = SKColors.Black, .IsAntialias = True}
                Select Case GlobalSettings.Settings.RunningPlatform
                    Case GlobalSettings.Settings.Platform.Windows
                        paint.Typeface = SKTypeface.FromFamilyName("Consolas", SKTypefaceStyle.Bold)
                    Case GlobalSettings.Settings.Platform.Linux
                        paint.Typeface = SKTypeface.FromFamilyName("Courier New", SKTypefaceStyle.Bold)
                    Case GlobalSettings.Settings.Platform.Mac
                        paint.Typeface = SKTypeface.FromFamilyName("Menlo", SKTypefaceStyle.Bold)
                End Select
                Dim trect As New SKRect(0, 0, 2, 2)
                paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
                Dim tsize As New SKSize(trect.Right - trect.Left, trect.Top - trect.Bottom)
                Dim strx As Single = (w - paint.MeasureText(valtext)) / 2
                Dim stry As Single = h - tsize.Height - 20.0 * f
                canvas.DrawText(valtext, X + strx, Y + stry, paint)
            End Using

        End Sub

    End Class

End Namespace
