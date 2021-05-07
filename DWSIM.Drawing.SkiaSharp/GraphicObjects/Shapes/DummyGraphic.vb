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

                Dim myPen2 As New SKPaint()
                With myPen2
                    .Color = LineColor
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = True
                    .StrokeWidth = LineWidth
                End With

                Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)

                If GradientMode Then

                    Dim r0 As New SKRect(X, Y, X + Width, Y + Height)

                    Dim radius2 = 0.8F * Math.Min(Width, Height)
                    Dim center = New SKPoint(r0.MidX, r0.MidY)
                    Dim offCenter = center - New SKPoint(radius2 / 2, radius2 / 2)

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = LineColor
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Shader = SKShader.CreateTwoPointConicalGradient(
                                        offCenter, 1, center, radius2,
                                        New SKColor() {SKColors.White, LineColor},
                                        Nothing, SKShaderTileMode.Clamp)
                    End With

                    canvas.DrawRoundRect(rect1, 2, 2, gradPen)

                End If

                canvas.DrawRoundRect(rect1, 2, 2, myPen2)

                Dim tpaint As New SKPaint()

                With tpaint
                    .TextSize = 10.0#
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = LineColor
                    .IsStroke = False
                    .Typeface = DefaultTypeFace
                End With

                Dim trect As New SKRect(0, 0, 2, 2)
                tpaint.GetTextPath("DO", 0, 0).GetBounds(trect)

                Dim ax, ay As Integer
                ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                If FlippedH Or FlippedV Or Rotation <> 0 Then

                    Dim currmat = canvas.TotalMatrix

                    canvas.Save()

                    If FlippedV And Not FlippedH Then
                        canvas.Scale(1, -1, (X + Width / 2), (Y + Height / 2))
                    ElseIf FlippedH And Not FlippedV Then
                        canvas.Scale(-1, 1, (X + Width / 2), (Y + Height / 2))
                    ElseIf FlippedH And FlippedV Then
                        canvas.Scale(-1, -1, (X + Width / 2), (Y + Height / 2))
                    End If

                    If Rotation <> 0.0 Then canvas.RotateDegrees(Rotation, X + Width / 2, Y + Height / 2)

                    canvas.DrawText("DO", ax, ay, tpaint)

                    canvas.SetMatrix(currmat)

                Else

                    canvas.DrawText("DO", ax, ay, tpaint)

                End If

            End If

        End Sub

    End Class

End Namespace