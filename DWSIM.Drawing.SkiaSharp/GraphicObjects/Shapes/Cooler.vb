Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class CoolerGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.Cooler
            Me.Description = "Material Stream Cooler"
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
            myIC1.Position = New Point(X, Y + 0.5 * Height)
            myIC1.Type = ConType.ConIn

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Direction = ConDir.Down
            Me.EnergyConnector.ConnectorName = "Energy Stream"
            Me.EnergyConnector.Active = True

            With InputConnectors

                If .Count = 1 Then
                    .Item(0).Position = New Point(X, Y + 0.5 * Height)
                Else
                    .Add(myIC1)
                End If

                .Item(0).ConnectorName = "Inlet"

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                Else
                    .Add(myOC1)
                End If

                .Item(0).ConnectorName = "Outlet"

            End With

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)


            Dim myPen As New SKPaint()
            With myPen
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect As New SKRect(X, Y, X + Width, X + Height)

            Dim gp As New SKPath()
            gp.MoveTo((X), (Y + 0.5 * Height))

            gp.LineTo((X + 0.5 * Width), (Y))
            gp.LineTo((X + Width), (Y + 0.5 * Height))
            gp.LineTo((X + 0.5 * Width), (Y + Height))
            gp.LineTo((X), (Y + 0.5 * Height))

            gp.Close()

            canvas.DrawPath(gp, myPen)

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

                canvas.DrawPath(gp, gradPen)

            End If

            Dim tpaint As New SKPaint()

            With tpaint
                .TextSize = 14.0#
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .Color = LineColor
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath("C", 0, 0).GetBounds(trect)

            Dim ax, ay As Double
            ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2 - 1.0
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

                canvas.DrawText("C", ax, ay, tpaint)

                canvas.SetMatrix(currmat)

            Else

                canvas.DrawText("C", ax, ay, tpaint)

            End If

        End Sub

    End Class

End Namespace