Imports DWSIM.Interfaces
Imports SkiaSharp.Extended

Namespace GraphicObjects

    Public Class LevelGaugeGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.LevelGauge

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

            Dim f = Height / 70.0

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

            Dim minvaltext = owneri.MinimumValue.ToString(formatstring)
            Dim maxvaltext = owneri.MaximumValue.ToString(formatstring)
            Dim valtext = currentvalue.ToString(formatstring)

            Dim origin = X + w * 0.3

            Using paint2 As New SKPaint With {.Color = SKColors.LightGray, .IsStroke = False, .IsAntialias = True}
                canvas.DrawRect(origin, Y, w / 2, h, paint2)
            End Using

            Dim currentlevel = (currentvalue - owneri.MinimumValue) / (owneri.MaximumValue - owneri.MinimumValue) * h

            Dim clx = origin + w / 2 + 6 * f
            Dim cly = Y + (h - currentlevel)

            Using paint2 As New SKPaint With {.Color = SKColors.Blue, .IsStroke = False, .IsAntialias = True}
                canvas.DrawRect(origin + 2 * f, cly + 2 * f, w / 2 - 4 * f, currentlevel - 4 * f, paint2)
            End Using

            canvas.Save()

            canvas.RotateDegrees(-90, clx, cly)

            Using paint2 As New SKPaint With {.Color = SKColors.Black, .IsStroke = False, .IsAntialias = True}
                canvas.DrawTriangle(origin + w / 2 + 6 * f, cly, 4 * f, paint2)
            End Using

            canvas.Restore()

            Using paint As New SKPaint With {.TextSize = 10.0 * f, .Color = SKColors.Black, .IsAntialias = True}
                Dim trect As New SKRect(0, 0, 2, 2)
                paint.GetTextPath(valtext, 0, 0).GetBounds(trect)
                Dim stry As Single = cly + trect.Height / 2
                canvas.DrawText(minvaltext, origin + w / 2 + 10 * f, Y + h, paint)
                canvas.DrawText(maxvaltext, origin + w / 2 + 10 * f, Y + trect.Height / 2, paint)
                canvas.DrawText(valtext, origin + w / 2 + 10 * f, stry, paint)
            End Using

        End Sub

    End Class

End Namespace
