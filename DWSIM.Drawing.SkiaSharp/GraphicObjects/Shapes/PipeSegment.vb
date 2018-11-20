Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class PipeSegmentGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.Pipe
            Me.Description = "Pipe Segment"
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

            With InputConnectors

                If .Count <> 0 Then
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

            Dim myPen2 As New SKPaint()
            With myPen2
                .Color = LineColor
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .IsStroke = True
                .StrokeWidth = LineWidth
            End With

            Dim rect1 As New SKRect(X + 0.05 * Width, Y, X + 0.95 * Width, Y + Height)
            Dim rect0 As New SKRect(X, Y, X + 0.1 * Width, Y + Height)
            Dim rect2 As New SKRect(X + 0.9 * Width, Y, X + Width, Y + Height)

            canvas.DrawRect(rect1, myPen2)
            canvas.DrawOval(rect0, myPen2)
            canvas.DrawOval(rect2, myPen2)

            If GradientMode Then

                Dim gradPen As New SKPaint()
                With gradPen
                    .Color = LineColor
                    .StrokeWidth = LineWidth
                    .IsStroke = False
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Shader = SKShader.CreateLinearGradient(New SKPoint(X, Y), New SKPoint(X, Y + Height),
                                    New SKColor() {SKColors.White, LineColor},
                                    New Single() {0.0, 1.0}, SKShaderTileMode.Clamp)
                End With

                canvas.DrawRect(rect1, gradPen)
                canvas.DrawOval(rect0, gradPen)
                canvas.DrawOval(rect2, gradPen)
                canvas.DrawOval(rect0, myPen2)

            End If

        End Sub

    End Class

End Namespace
