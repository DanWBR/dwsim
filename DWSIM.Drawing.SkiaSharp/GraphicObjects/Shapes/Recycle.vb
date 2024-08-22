Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class RecycleGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.OT_Recycle
            Me.Description = "Recycle Logical Op"
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

            Me.EnergyConnector.Active = False

            Dim myIC1 As New ConnectionPoint
            myIC1.Position = New Point(X, Y + 0.5 * Height)
            myIC1.Type = ConType.ConIn

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn

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

            Select Case DrawMode

                Case 0, 2

                    'default
                    Dim myPen As New SKPaint()
                    With myPen
                        If Active Then
                            .Color = SKColors.LightGreen
                        Else
                            .Color = SKColors.LightGray
                        End If
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawOval(New SKRect(X, Y, X + Width, Y + Height), myPen)

                    Dim myPen2 As New SKPaint()
                    With myPen2
                        If Active Then
                            .Color = SKColors.Green
                        Else
                            .Color = SKColors.Gray
                        End If
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawOval(New SKRect(X, Y, X + Width, Y + Height), myPen2)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 12.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        If Active Then
                            .Color = SKColors.Green
                        Else
                            .Color = SKColors.Gray
                        End If
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("R", 0, 0).GetBounds(trect)

                    Dim ax, ay As Integer
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    Using New SKAutoCanvasRestore(canvas)
                        StraightCanvas(canvas)
                        canvas.DrawText("R", ax, ay, tpaint)
                    End Using

                Case 1

                    'b/w

                    Dim myPen2 As New SKPaint()
                    With myPen2
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateDash(New Single() {2, 2}, 4)
                    End With

                    canvas.DrawOval(New SKRect(X, Y, X + Width, Y + Height), myPen2)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 12.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = SKColors.Black
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("R", 0, 0).GetBounds(trect)

                    Dim ax, ay As Integer
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    Using New SKAutoCanvasRestore(canvas)
                        StraightCanvas(canvas)
                        canvas.DrawText("R", ax, ay, tpaint)
                    End Using

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