Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class SplitterGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.NodeOut
            Me.Description = "Material Stream Splitter"
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
            myOC1.Position = New Point(X + Width, Y)
            myOC1.Type = ConType.ConOut

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC2.Type = ConType.ConOut

            Dim myOC3 As New ConnectionPoint
            myOC3.Position = New Point(X + Width, Y + Height)
            myOC3.Type = ConType.ConOut

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
                    .Item(0).Position = New Point(X + Width, Y)
                    .Item(1).Position = New Point(X + Width, Y + 0.5 * Height)
                    .Item(2).Position = New Point(X + Width, Y + Height)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                    .Add(myOC3)
                End If
                .Item(0).ConnectorName = "Outlet 1"
                .Item(1).ConnectorName = "Outlet 2"
                .Item(2).ConnectorName = "Outlet 3"

            End With

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            Dim rect As New SKRect(X, Y, X + Width, X + Height)

            Dim gp As New SKPath()
            gp.MoveTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.5 * Height))
            gp.LineTo(Convert.ToInt32(X + 0.5 * Width), Convert.ToInt32(Y))
            gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + Height))
            gp.LineTo(Convert.ToInt32(X + 0.5 * Width), Convert.ToInt32(Y + Height))
            gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.5 * Height))

            gp.Close()

            Select Case DrawMode

                Case 0

                    'default

                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = LineColor
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, gradPen)

                    canvas.DrawPath(gp, myPen)

                Case 1

                    'b/w

                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)

                Case 2

                    'Gas/Liquid Flows

                Case 3

                    'Temperature Gradients

                Case 4

                    'Pressure Gradients

                Case 5

                    'Temperature/Pressure Gradients

            End Select

        End Sub

    End Class

End Namespace