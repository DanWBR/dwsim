Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class HeatExchangerGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.HeatExchanger
            Me.Description = "Heat Exchanger"
            CreateConnectors(2, 2)
            EmbeddedResourceIconName = "heat_exchanger.png"
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

                For I As Integer = 1 To 2

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConIn
                    InputConnectors.Add(Con)

                Next

            End If

            If OutputConnectors.Count = 0 Then

                For I As Integer = 1 To 2

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConOut
                    OutputConnectors.Add(Con)

                Next

            End If

            With InputConnectors
                .Item(0).Position = New Point(X, Y + 0.5 * Height)
                If DrawMode = 2 Then
                    .Item(1).Position = New Point(X + 0.257 * Width, Y + 0.234 * Height)
                Else
                    .Item(1).Position = New Point(X + 0.5 * Width, Y)
                End If
                .Item(1).Direction = ConDir.Down
                .Item(0).ConnectorName = "Inlet Stream 1"
                .Item(1).ConnectorName = "Inlet Stream 2"
            End With

            With OutputConnectors
                .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                If DrawMode = 2 Then
                    .Item(1).Position = New Point(X + 0.765 * Width, Y + 0.7825 * Height)
                Else
                    .Item(1).Position = New Point(X + 0.5 * Width, Y + Height)
                End If
                .Item(1).Direction = ConDir.Down
                .Item(0).ConnectorName = "Outlet Stream 1"
                .Item(1).ConnectorName = "Outlet Stream 2"
            End With

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

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

                    Dim gp As New SKPath()

                    gp.MoveTo(Convert.ToInt32(X), Convert.ToInt32(Y + Height))

                    gp.LineTo(Convert.ToInt32(X + (2 / 8) * Width), Convert.ToInt32(Y + (3 / 8) * Height))
                    gp.LineTo(Convert.ToInt32(X + (6 / 8) * Width), Convert.ToInt32(Y + (5 / 8) * Height))
                    gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))

                    Dim myrect As New SKRect(X, Y, X + Width, Y + Height)

                    Dim gradPen As New SKPaint()
                        With gradPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                            .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                        canvas.DrawOval(myrect, gradPen)

                    canvas.DrawOval(myrect, myPen)
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

                    Dim gp As New SKPath()

                    gp.MoveTo(Convert.ToInt32(X), Convert.ToInt32(Y + Height))

                    gp.LineTo(Convert.ToInt32(X + (2 / 8) * Width), Convert.ToInt32(Y + (3 / 8) * Height))
                    gp.LineTo(Convert.ToInt32(X + (6 / 8) * Width), Convert.ToInt32(Y + (5 / 8) * Height))
                    gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))

                    Dim myrect As New SKRect(X, Y, X + Width, Y + Height)

                    canvas.DrawOval(myrect, myPen)
                    canvas.DrawPath(gp, myPen)

                Case 2

                    DrawIcon(canvas)

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