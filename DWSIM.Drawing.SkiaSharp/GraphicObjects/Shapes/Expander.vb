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
            EmbeddedResourceIconName = "expander.png"
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

            Me.EnergyConnector.Position = New Point(X + Width, Y + 0.5 * Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Direction = ConDir.Right
            Me.EnergyConnector.Active = True
            Me.EnergyConnector.ConnectorName = "Energy Stream"

            Dim myIC1, myOC1 As New ConnectionPoint

            If DrawMode = 2 Then

                myIC1.Position = New Point(X + 0.094 * Width, Y + 0.374 * Height)
                myIC1.Type = ConType.ConIn
                myIC1.Direction = ConDir.Down

                myOC1.Position = New Point(X + 0.612 * Width, Y + 0.28 * Height)
                myOC1.Type = ConType.ConOut
                myOC1.Direction = ConDir.Up

            Else

                myIC1.Position = New Point(X, Y)
                myIC1.Type = ConType.ConIn
                myIC1.Direction = ConDir.Down

                myOC1.Position = New Point(X + Width, Y + 0.3 * Height)
                myOC1.Type = ConType.ConOut
                myOC1.Direction = ConDir.Up

            End If

            With InputConnectors

                If .Count = 1 Then
                    If DrawMode = 2 Then
                        .Item(0).Position = New Point(X + 0.094 * Width, Y + 0.374 * Height)
                        .Item(0).Direction = ConDir.Down
                    Else
                        .Item(0).Position = New Point(X, Y + 0.3 * Height)
                        .Item(0).Direction = ConDir.Down
                    End If
                Else
                    .Add(myIC1)
                End If

                .Item(0).ConnectorName = "Inlet"

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    If DrawMode = 2 Then
                        .Item(0).Position = New Point(X + 0.612 * Width, Y + 0.28 * Height)
                        .Item(0).Direction = ConDir.Up
                    Else
                        .Item(0).Position = New Point(X + Width, Y)
                        .Item(0).Direction = ConDir.Up
                    End If
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

            Dim rect As New SKRect(X, Y, X + Width, X + Height)

            Dim gp2 As New SKPath()

            gp2.MoveTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.3 * Height))
            gp2.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.7 * Height))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + Height))
            gp2.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y))
            gp2.Close()

            Select Case DrawMode

                Case 0

                    'default
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

                    canvas.DrawPath(gp2, gradPen)

                    canvas.DrawPath(gp2, myPen)

                Case 1

                    'b/w
                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp2, myPen)

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