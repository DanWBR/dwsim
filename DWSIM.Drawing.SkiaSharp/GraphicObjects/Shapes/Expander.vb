Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class TurbineGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.Expander
            Me.Description = "Adiabatic Expander"
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
            Me.EnergyConnector.Active = True
            Me.EnergyConnector.ConnectorName = "Energy Stream"

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

            Dim myPen, myPen2 As New SKPaint()
            With myPen
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect As New SKRect(X, Y, X + Width, X + Height)

            Dim gp2 As New SKPath()

            gp2.MoveTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.3 * Height))
            gp2.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.7 * Height))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + Height))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.Close()

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

                canvas.DrawPath(gp2, gradPen)

            End If

            canvas.DrawPath(gp2, myPen)

        End Sub

    End Class

End Namespace