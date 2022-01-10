Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects

    Public Class TextGraphic

        Inherits GraphicObject

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Text
            Me.Height = 20
            Me.Width = 50

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal text As String)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Text = text
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal text As String)
            Me.New(New SKPoint(posX, posY), text)
        End Sub

#End Region

        Public Property Flowsheet As IFlowsheet

        Public Property Text As String = "TEXT (double-click to edit)"

        Public Property Size As Double = 14.0#

        Public Property Color As SKColor = SKColors.Black

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            Dim tpaint As New SKPaint()

            If DrawMode = 0 Then
                With tpaint
                    .TextSize = Size
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = If(GlobalSettings.Settings.DarkMode, SKColors.LightSteelBlue, Color)
                    .IsStroke = False
                    .Typeface = GetFont()
                End With
            Else
                With tpaint
                    .TextSize = Size
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = SKColors.Black
                    .IsStroke = False
                    .Typeface = GetFont()
                End With
            End If

            Dim lines = Text.Split(vbLf)

            Dim newy As Integer = Y

            Try
                Height = 0
                Width = 0
                For Each l As String In lines
                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath(l, 0, 0).GetBounds(trect)
                    newy += trect.Height + 2
                    Height += trect.Height + 2
                    Width = Math.Max(Width, trect.Width)
                    canvas.DrawText(l, X, newy, tpaint)
                Next
            Catch ex As Exception
                Dim trect As New SKRect(0, 0, 2, 2)
                tpaint.GetTextPath(Text.Replace("\n", vbCrLf), 0, 0).GetBounds(trect)
                Height = trect.Height
                Width = trect.Width
                canvas.DrawText(Text, X, Y + MeasureString(Text, tpaint).Height, tpaint)
            End Try

        End Sub

    End Class

End Namespace