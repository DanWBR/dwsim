Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class DummyGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.CustomUO
            Me.Description = "Dummy Block"
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer)
            Me.New(New SKPoint(posX, posY))
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint, ByVal graphicSize As SKSize)
            Me.New(graphicPosition)
            Me.SetSize(graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As SKSize)
            Me.New(New SKPoint(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer)
            Me.New(New SKPoint(posX, posY), New SKSize(width, height))
        End Sub

#End Region
        Public Overrides Sub PositionConnectors()

            CreateConnectors(0, 0)

        End Sub

        Public Overrides Sub CreateConnectors(InCount As Integer, OutCount As Integer)

            'Creates all the connection points.

            If InputConnectors.Count = 0 Then

                For i As Integer = 1 To 20

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConIn
                    InputConnectors.Add(Con)

                Next

            End If

            If OutputConnectors.Count = 0 Then

                For i As Integer = 1 To 20

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConOut
                    OutputConnectors.Add(Con)

                Next

            End If

            With InputConnectors
                For i As Integer = 0 To InputConnectors.Count - 1
                    .Item(i).Position = New Point(X, Y + (i + 1) / 20 * Height)
                    .Item(i).ConnectorName = "#" & (i + 1)
                Next
            End With

            With OutputConnectors
                For i As Integer = 0 To OutputConnectors.Count - 1
                    .Item(i).Position = New Point(X + Width, Y + (i + 1) / 20 * Height)
                    .Item(i).ConnectorName = "#" & (i - 1)
                Next
            End With

            EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            If Owner IsNot Nothing AndAlso Owner.UseEmbeddedImage = True AndAlso Owner.EmbeddedImageData <> "" Then

                Dim p As New SKPaint
                With p
                    p.IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    p.FilterQuality = SKFilterQuality.High
                End With

                Using image As SKImage = EmbeddedImageGraphic.Base64ToImage(Owner.EmbeddedImageData)
                    canvas.DrawImage(image, New SKRect(X, Y, X + Width, Y + Height), p)
                End Using

            Else

                Select Case DrawMode

                    Case 0

                        'default
                        Dim myPen2 As New SKPaint()
                        With myPen2
                            .Color = LineColor
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .IsStroke = True
                            .StrokeWidth = LineWidth
                        End With

                        Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)

                        Dim gradPen As New SKPaint()
                        With gradPen
                            .Color = LineColor.WithAlpha(50)
                            .StrokeWidth = LineWidth
                            .IsStroke = False
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        End With

                        canvas.DrawRoundRect(rect1, 2, 2, gradPen)

                        canvas.DrawRoundRect(rect1, 2, 2, myPen2)

                        Dim tpaint As New SKPaint()

                        With tpaint
                            .TextSize = 10.0#
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .Color = LineColor
                            .IsStroke = False
                            .Typeface = BoldTypeFace
                        End With

                    Case 1

                        'b/w
                        Dim myPen2 As New SKPaint()
                        With myPen2
                            .Color = SKColors.Black
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .IsStroke = True
                            .StrokeWidth = LineWidth
                        End With

                        Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)

                        canvas.DrawRoundRect(rect1, 2, 2, myPen2)

                        Dim tpaint As New SKPaint()

                        With tpaint
                            .TextSize = 10.0#
                            .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                            .Color = SKColors.Black
                            .IsStroke = False
                            .Typeface = BoldTypeFace
                        End With

                    Case 2

                        DrawIcon(canvas)

                End Select

            End If

        End Sub

    End Class

End Namespace