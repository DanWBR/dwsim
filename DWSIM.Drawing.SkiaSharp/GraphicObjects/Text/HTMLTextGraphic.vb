Imports Interfaces = DWSIM.Interfaces
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports SkiaSharp.Views.Desktop.Extensions
Imports TheArtOfDev.HtmlRenderer.WinForms

Namespace GraphicObjects

    Public Class HTMLTextGraphic

        Inherits GraphicObject

#Region "Constructors"

        Public Sub New()

            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_HTMLText

            Me.Height = 100
            Me.Width = 200

        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
            Me.Text = Text
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

#End Region

        Public Property Text As String = "<html><body><p>Double-click to edit this text.</p></body></html>"

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

            If Flowsheet IsNot Nothing Then
                If Text.Contains("{") And Text.Contains("}") Then

                End If
            End If

            Dim p As New SKPaint
            With p
                p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                p.FilterQuality = SKFilterQuality.High
            End With

            Using img = HtmlRender.RenderToImage(Text, Width, Height, System.Drawing.Color.White)
                Using bmp = New System.Drawing.Bitmap(img)
                    Using skbmp = bmp.ToSKBitmap()
                        canvas.DrawBitmap(skbmp, X, Y, p)
                    End Using
                End Using
            End Using

        End Sub

    End Class

End Namespace