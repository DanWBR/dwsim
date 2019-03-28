Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class OrificePlateGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.OrificePlate
            Me.Description = "Orifice Plate"
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
            myIC1.ConnectorName = "Inlet Stream"

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut
            myOC1.ConnectorName = "Outlet Stream"

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Active = False

            With InputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X, Y + 0.5 * Height)
                Else
                    .Add(myIC1)
                End If

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                Else
                    .Add(myOC1)
                End If

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

            Dim rect As New SKRect(X, Y, X + Width, Y + Height)
            Dim rect2 As New SKRect(X + 0.3 * Width, Y + 0.3 * Height, X + 0.7 * Width, Y + 0.7 * Height)
            Dim rect3 As New SKRect(X + 0.4 * Width, Y - 0.3 * Height, X + 0.6 * Width, Y)

            canvas.DrawOval(rect, myPen2)
            canvas.DrawOval(rect2, myPen2)
            canvas.DrawRect(rect3, myPen2)

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

                canvas.DrawOval(rect, gradPen)
                canvas.DrawRect(rect3, gradPen)

                Dim bgpen As New SKPaint()
                With bgpen
                    .Color = If(GlobalSettings.Settings.DarkMode, SKColors.Black, SKColors.White)
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .IsStroke = False
                End With

                canvas.DrawOval(rect2, bgpen)
                canvas.DrawOval(rect2, myPen2)

            End If

        End Sub

    End Class

End Namespace