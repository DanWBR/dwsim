Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class ScriptGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.CustomUO
            Me.Description = "Python Script Block"
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

            Dim myIC1 As New ConnectionPoint
            myIC1.Position = New Point(X, Y)
            myIC1.Type = ConType.ConIn
            myIC1.ConnectorName = "Inlet Port #1"

            Dim myIC2 As New ConnectionPoint
            myIC2.Position = New Point(X, Y + 1 * 0.13 * Height)
            myIC2.Type = ConType.ConIn
            myIC2.ConnectorName = "Inlet Port #2"

            Dim myIC3 As New ConnectionPoint
            myIC3.Position = New Point(X, Y + 2 * 0.13 * Height)
            myIC3.Type = ConType.ConIn
            myIC3.ConnectorName = "Inlet Port #3"

            Dim myIC4 As New ConnectionPoint
            myIC4.Position = New Point(X, Y)
            myIC4.Type = ConType.ConEn
            myIC4.Direction = ConDir.Up
            myIC4.ConnectorName = "Inlet Energy Port"

            Dim myIC5 As New ConnectionPoint
            myIC5.Position = New Point(X, Y + 3 * 0.13 * Height)
            myIC5.Type = ConType.ConIn
            myIC5.ConnectorName = "Inlet Port #4"

            Dim myIC6 As New ConnectionPoint
            myIC6.Position = New Point(X, Y + 4 * 0.13 * Height)
            myIC6.Type = ConType.ConIn
            myIC6.ConnectorName = "Inlet Port #5"

            Dim myIC7 As New ConnectionPoint
            myIC7.Position = New Point(X, Y + 5 * 0.13 * Height)
            myIC7.Type = ConType.ConIn
            myIC7.ConnectorName = "Inlet Port #6"

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y)
            myOC1.Type = ConType.ConOut
            myOC1.ConnectorName = "Outlet Port #1"

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(X + Width, Y + 1 * 0.13 * Height)
            myOC2.Type = ConType.ConOut
            myOC2.ConnectorName = "Outlet Port #2"

            Dim myOC3 As New ConnectionPoint
            myOC3.Position = New Point(X + Width, Y + 2 * 0.13 * Height)
            myOC3.Type = ConType.ConOut
            myOC3.ConnectorName = "Outlet Port #3"

            Dim myOC4 As New ConnectionPoint
            myOC4.Position = New Point(X + Width, Y + Height)
            myOC4.Type = ConType.ConEn
            myOC4.Direction = ConDir.Down
            myOC4.ConnectorName = "Outlet Energy Port"

            Dim myOC5 As New ConnectionPoint
            myOC5.Position = New Point(X + Width, Y + 3 * 0.13 * Height)
            myOC5.Type = ConType.ConIn
            myOC5.ConnectorName = "Outlet Port #4"

            Dim myOC6 As New ConnectionPoint
            myOC6.Position = New Point(X + Width, Y + 4 * 0.13 * Height)
            myOC6.Type = ConType.ConIn
            myOC6.ConnectorName = "Outlet Port #5"

            Dim myOC7 As New ConnectionPoint
            myOC7.Position = New Point(X + Width, Y + 5 * 0.13 * Height)
            myOC7.Type = ConType.ConIn
            myOC7.ConnectorName = "Outlet Port #6"

            With InputConnectors

                If .Count <> 0 Then
                    If .Count = 3 Then
                        .Add(myIC4)
                    ElseIf .Count = 4 Then
                        .Add(myIC5)
                        .Add(myIC6)
                        .Add(myIC7)
                    Else
                        .Item(0).Position = New Point(X, Y)
                        .Item(1).Position = New Point(X, Y + 1 * 0.13 * Height)
                        .Item(2).Position = New Point(X, Y + 2 * 0.13 * Height)
                        .Item(3).Position = New Point(X, Y + Height)
                        .Item(4).Position = New Point(X, Y + 3 * 0.13 * Height)
                        .Item(5).Position = New Point(X, Y + 4 * 0.13 * Height)
                        .Item(6).Position = New Point(X, Y + 5 * 0.13 * Height)
                    End If
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                    .Add(myIC3)
                    .Add(myIC4)
                    .Add(myIC5)
                    .Add(myIC6)
                    .Add(myIC7)
                End If

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    If .Count = 3 Then
                        .Add(myOC4)
                    ElseIf .Count = 4 Then
                        .Add(myOC5)
                        .Add(myOC6)
                        .Add(myOC7)
                    Else
                        .Item(0).Position = New Point(X + Width, Y)
                        .Item(1).Position = New Point(X + Width, Y + 1 * 0.13 * Height)
                        .Item(2).Position = New Point(X + Width, Y + 2 * 0.13 * Height)
                        .Item(3).Position = New Point(X + Width, Y + Height)
                        .Item(4).Position = New Point(X + Width, Y + 3 * 0.13 * Height)
                        .Item(5).Position = New Point(X + Width, Y + 4 * 0.13 * Height)
                        .Item(6).Position = New Point(X + Width, Y + 5 * 0.13 * Height)
                    End If
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                    .Add(myOC3)
                    .Add(myOC4)
                    .Add(myOC5)
                    .Add(myOC6)
                    .Add(myOC7)
                End If

            End With

            Me.EnergyConnector.Active = False

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
                tpaint.GetTextPath("PS", 0, 0).GetBounds(trect)

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

                    canvas.DrawText("PS", ax, ay, tpaint)

                    canvas.SetMatrix(currmat)

                Else

                    canvas.DrawText("PS", ax, ay, tpaint)

                End If

            End If

        End Sub

    End Class

End Namespace