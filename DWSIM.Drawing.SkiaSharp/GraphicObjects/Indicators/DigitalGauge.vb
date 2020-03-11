Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    Public Class DigitalGaugeGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.DigitalGauge

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

            Dim f = Height / 40.0

            Dim owneri = DirectCast(Owner, IIndicator)

            Dim currentvalue As Double

            If owneri.SelectedObject IsNot Nothing Then
                currentvalue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(owneri.SelectedPropertyUnits, owneri.SelectedObject.GetPropertyValue(owneri.SelectedProperty))
            End If

            Dim formatstring = ""

            For i = 0 To owneri.IntegralDigits
                formatstring += "0"
            Next
            formatstring += "."
            For i = 0 To owneri.DecimalDigits
                formatstring += "0"
            Next

            If owneri.SelectedObject IsNot Nothing Then
                currentvalue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(owneri.SelectedPropertyUnits, owneri.SelectedObject.GetPropertyValue(owneri.SelectedProperty))
            End If

            Dim valtext = currentvalue.ToString(formatstring)

            Using paint As New SKPaint With {.TextSize = 29.0 * f, .Color = SKColors.LightGreen, .IsAntialias = True}
                Dim assm = Me.GetType.Assembly
                Using filestr As IO.Stream = assm.GetManifestResourceStream("DWSIM.Drawing.SkiaSharp.digital7_mono_italic.ttf")
                    paint.Typeface = SKTypeface.FromStream(filestr)
                    Dim trect As New SKRect(0, 0, 2, 2)
                    paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
                    Dim strx As Single = (w - trect.Width) / 2
                    Dim stry As Single = h - trect.Height / 2
                    If trect.Width > w Then
                        w = trect.Width + 30 * f
                        strx = (w - trect.Width) / 2
                        Width = w
                    End If
                    Using paint2 As New SKPaint With {.Color = SKColors.Gray, .IsStroke = False, .IsAntialias = True}
                        canvas.DrawRect(X - 5 * f, Y - 5 * f, w + 10 * f, h + 10 * f, paint2)
                    End Using
                    Using paint2 As New SKPaint With {.Color = SKColors.LightGray, .IsStroke = False, .IsAntialias = True}
                        canvas.DrawRect(X - 2 * f, Y - 2 * f, w + 4 * f, h + 4 * f, paint2)
                    End Using
                    Using paint2 As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                        canvas.DrawRect(X, Y, w, h, paint2)
                    End Using
                    canvas.DrawText(valtext, X + strx, Y + stry, paint)
                End Using
            End Using

        End Sub

    End Class

End Namespace
