Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class PumpGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.Pump
            Me.Description = "Adiabatic Pump"
            EmbeddedResourceIconName = "pump.png"
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

            Dim myIC1, myIC2, myOC1 As New ConnectionPoint

            If DrawMode = 2 Then

                myIC1.Position = New Point(X, Y + 0.625 * Height)
                myIC1.Type = ConType.ConIn
                myIC1.Direction = ConDir.Right

                myIC2.Position = New Point(X + Width, Y + 0.51 * Height)
                myIC2.Type = ConType.ConEn
                myIC2.Direction = ConDir.Left

                myOC1.Position = New Point(X + 0.129 * Width, Y + 0.433 * Height)
                myOC1.Type = ConType.ConOut

                With InputConnectors

                    If .Count = 2 Then
                        .Item(0).Position = New Point(X, Y + 0.625 * Height)
                        .Item(1).Position = New Point(X + Width, Y + 0.51 * Height)
                    Else
                        .Add(myIC1)
                        .Add(myIC2)
                    End If

                    .Item(0).ConnectorName = "Inlet"
                    .Item(1).ConnectorName = "Energy Stream"

                    .Item(0).Direction = ConDir.Right
                    .Item(1).Direction = ConDir.Left

                End With

                With OutputConnectors

                    If .Count <> 0 Then
                        .Item(0).Position = New Point(X + 0.129 * Width, Y + 0.433 * Height)
                    Else
                        .Add(myOC1)
                    End If

                    .Item(0).ConnectorName = "Outlet"
                    .Item(0).Direction = ConDir.Up

                End With

            Else

                myIC1.Position = New Point(X, Y + 0.5 * Height)
                myIC1.Type = ConType.ConIn

                myIC2.Position = New Point(X + 0.5 * Width, Y + Height)
                myIC2.Type = ConType.ConEn

                myOC1.Position = New Point(X + Width, Y + 0.1 * Height)
                myOC1.Type = ConType.ConOut

                With InputConnectors

                    If .Count = 2 Then
                        .Item(0).Position = New Point(X, Y + 0.5 * Height)
                        .Item(1).Position = New Point(X + 0.5 * Width, Y + Height)
                    ElseIf .Count = 1 Then
                        .Item(0).Position = New Point(X, Y + 0.5 * Height)
                        .Add(myIC2)
                    Else
                        .Add(myIC1)
                        .Add(myIC2)
                    End If

                    .Item(0).ConnectorName = "Inlet"
                    .Item(1).ConnectorName = "Energy Stream"

                End With

                With OutputConnectors

                    If .Count <> 0 Then
                        .Item(0).Position = New Point(X + Width, Y + 0.1 * Height)
                    Else
                        .Add(myOC1)
                    End If

                    .Item(0).ConnectorName = "Outlet"
                    .Item(0).Direction = ConDir.Right

                End With

            End If

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)

            UpdateStatus()
            MyBase.Draw(g)


            Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.8 * Width, Y + 0.8 * Height)

            Dim pt0 As New Point(X + 0.5 * Width, Y + Height)
            Dim pt0b As New Point(X + 0.7 * Width, Y + 0.98 * Height)
            Dim pt0a As New Point(X + 0.3 * Width, Y + 0.98 * Height)

            Dim pt3 As New Point(X + 0.1 * Width, Y + Height)
            Dim pt4 As New Point(X + 0.2 * Width, Y + 0.9 * Height)

            Dim pt5 As New Point(X + 0.9 * Width, Y + Height)
            Dim pt6 As New Point(X + 0.8 * Width, Y + 0.9 * Height)

            Dim pt7 As New Point(X + 0.1 * Width, Y + Height)
            Dim pt8 As New Point(X + 0.9 * Width, Y + Height)

            Dim pt9 As New Point(X + 0.5 * Width, Y)
            Dim pt10 As New Point(X + Width, Y)
            Dim pt11 As New Point(X + Width, Y + 0.25 * Height)
            Dim pt12 As New Point(X + 0.93 * Width, Y + 0.25 * Height)

            Dim gp As New SKPath()

            gp.MoveTo(pt0.X, pt0.Y)
            gp.LineTo(pt0b.X, pt0b.Y)
            gp.LineTo(pt6.X, pt6.Y)
            gp.LineTo(pt5.X, pt5.Y)
            gp.LineTo(pt3.X, pt3.Y)
            gp.LineTo(pt4.X, pt4.Y)
            gp.LineTo(pt0a.X, pt0a.Y)

            gp.Close()

            Dim gp2 As New SKPath()

            gp2.MoveTo(pt9.X, pt9.Y)

            gp2.LineTo(pt10.X, pt10.Y)
            gp2.LineTo(pt11.X, pt11.Y)
            gp2.LineTo(pt12.X, pt12.Y)

            gp.Close()

            Dim rect As New SKRect(X, Y, X + Width, Y + Height)

            Dim rectin As New SKRect(X - 0.05 * Width, Y + 0.35 * Height, X + 0.5 * Width, Y + 0.65 * Height)

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
                    Dim myPen2 As New SKPaint()
                    With myPen2
                        .Color = GraphicsSurface.BackgroundColor
                        .IsStroke = False
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
                    canvas.DrawPath(gp2, gradPen)
                    canvas.DrawOval(rect, gradPen)
                    canvas.DrawRect(rectin, gradPen)

                    canvas.DrawOval(rect, myPen)
                    canvas.DrawPath(gp, myPen)
                    canvas.DrawPath(gp2, myPen)
                    canvas.DrawRect(rectin, myPen2)
                    canvas.DrawRect(rectin, gradPen)
                    canvas.DrawRect(rectin, myPen)

                Case 1

                    'b/w
                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With
                    Dim myPen2 As New SKPaint()
                    With myPen2
                        .Color = SKColors.White
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)
                    canvas.DrawPath(gp2, myPen)
                    canvas.DrawOval(rect, myPen2)
                    canvas.DrawOval(rect, myPen)
                    canvas.DrawRect(rectin, myPen)

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