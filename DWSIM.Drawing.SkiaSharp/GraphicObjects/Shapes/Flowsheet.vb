Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class FlowsheetGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.FlowsheetUO
            Me.Description = "Sub-Flowsheet Block"
            EmbeddedResourceIconName = "flowsheet_block.png"
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

            Dim myIC1 As New ConnectionPoint With {.ConnectorName = "Inlet Port #1", .Position = New Point(X, Y + 0.1 * Height), .Type = ConType.ConIn}
            Dim myIC2 As New ConnectionPoint With {.ConnectorName = "Inlet Port #2", .Position = New Point(X, Y + 0.2 * Height), .Type = ConType.ConIn}
            Dim myIC3 As New ConnectionPoint With {.ConnectorName = "Inlet Port #3", .Position = New Point(X, Y + 0.3 * Height), .Type = ConType.ConIn}
            Dim myIC4 As New ConnectionPoint With {.ConnectorName = "Inlet Port #4", .Position = New Point(X, Y + 0.4 * Height), .Type = ConType.ConIn}
            Dim myIC5 As New ConnectionPoint With {.ConnectorName = "Inlet Port #5", .Position = New Point(X, Y + 0.5 * Height), .Type = ConType.ConIn}
            Dim myIC6 As New ConnectionPoint With {.ConnectorName = "Inlet Port #6", .Position = New Point(X, Y + 0.6 * Height), .Type = ConType.ConIn}
            Dim myIC7 As New ConnectionPoint With {.ConnectorName = "Inlet Port #7", .Position = New Point(X, Y + 0.7 * Height), .Type = ConType.ConIn}
            Dim myIC8 As New ConnectionPoint With {.ConnectorName = "Inlet Port #8", .Position = New Point(X, Y + 0.8 * Height), .Type = ConType.ConIn}
            Dim myIC9 As New ConnectionPoint With {.ConnectorName = "Inlet Port #9", .Position = New Point(X, Y + 0.9 * Height), .Type = ConType.ConIn}
            Dim myIC10 As New ConnectionPoint With {.ConnectorName = "Inlet Port #10", .Position = New Point(X, Y + 1.0 * Height), .Type = ConType.ConIn}

            Dim myOC1 As New ConnectionPoint With {.ConnectorName = "Outlet Port #1", .Position = New Point(X + Width, Y + 0.1 * Height), .Type = ConType.ConOut}
            Dim myOC2 As New ConnectionPoint With {.ConnectorName = "Outlet Port #2", .Position = New Point(X + Width, Y + 0.2 * Height), .Type = ConType.ConOut}
            Dim myOC3 As New ConnectionPoint With {.ConnectorName = "Outlet Port #3", .Position = New Point(X + Width, Y + 0.3 * Height), .Type = ConType.ConOut}
            Dim myOC4 As New ConnectionPoint With {.ConnectorName = "Outlet Port #4", .Position = New Point(X + Width, Y + 0.4 * Height), .Type = ConType.ConOut}
            Dim myOC5 As New ConnectionPoint With {.ConnectorName = "Outlet Port #5", .Position = New Point(X + Width, Y + 0.5 * Height), .Type = ConType.ConOut}
            Dim myOC6 As New ConnectionPoint With {.ConnectorName = "Outlet Port #6", .Position = New Point(X + Width, Y + 0.6 * Height), .Type = ConType.ConOut}
            Dim myOC7 As New ConnectionPoint With {.ConnectorName = "Outlet Port #7", .Position = New Point(X + Width, Y + 0.7 * Height), .Type = ConType.ConOut}
            Dim myOC8 As New ConnectionPoint With {.ConnectorName = "Outlet Port #8", .Position = New Point(X + Width, Y + 0.8 * Height), .Type = ConType.ConOut}
            Dim myOC9 As New ConnectionPoint With {.ConnectorName = "Outlet Port #9", .Position = New Point(X + Width, Y + 0.9 * Height), .Type = ConType.ConOut}
            Dim myOC10 As New ConnectionPoint With {.ConnectorName = "Outlet Port #10", .Position = New Point(X + Width, Y + 1.0 * Height), .Type = ConType.ConOut}

            With InputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X, Y + 0.1 * Height)
                    .Item(1).Position = New Point(X, Y + 0.2 * Height)
                    .Item(2).Position = New Point(X, Y + 0.3 * Height)
                    .Item(3).Position = New Point(X, Y + 0.4 * Height)
                    .Item(4).Position = New Point(X, Y + 0.5 * Height)
                    .Item(5).Position = New Point(X, Y + 0.6 * Height)
                    .Item(6).Position = New Point(X, Y + 0.7 * Height)
                    .Item(7).Position = New Point(X, Y + 0.8 * Height)
                    .Item(8).Position = New Point(X, Y + 0.9 * Height)
                    .Item(9).Position = New Point(X, Y + 1.0 * Height)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                    .Add(myIC3)
                    .Add(myIC4)
                    .Add(myIC5)
                    .Add(myIC6)
                    .Add(myIC7)
                    .Add(myIC8)
                    .Add(myIC9)
                    .Add(myIC10)
                End If

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.1 * Height)
                    .Item(1).Position = New Point(X + Width, Y + 0.2 * Height)
                    .Item(2).Position = New Point(X + Width, Y + 0.3 * Height)
                    .Item(3).Position = New Point(X + Width, Y + 0.4 * Height)
                    .Item(4).Position = New Point(X + Width, Y + 0.5 * Height)
                    .Item(5).Position = New Point(X + Width, Y + 0.6 * Height)
                    .Item(6).Position = New Point(X + Width, Y + 0.7 * Height)
                    .Item(7).Position = New Point(X + Width, Y + 0.8 * Height)
                    .Item(8).Position = New Point(X + Width, Y + 0.9 * Height)
                    .Item(9).Position = New Point(X + Width, Y + 1.0 * Height)
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                    .Add(myOC3)
                    .Add(myOC4)
                    .Add(myOC5)
                    .Add(myOC6)
                    .Add(myOC7)
                    .Add(myOC8)
                    .Add(myOC9)
                    .Add(myOC10)
                End If

            End With

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            Dim myPen2 As New SKPaint()

            Select Case DrawMode

                Case 0

                    'default
                    With myPen2
                        .Color = LineColor
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .IsStroke = True
                        .StrokeWidth = LineWidth
                    End With
                    Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)

                    Dim r0 As New SKRect(X, Y, X + Width, Y + Height)

                        Dim radius2 = 0.8F * Math.Min(Width, Height)
                        Dim center = New SKPoint(r0.MidX, r0.MidY)
                        Dim offCenter = center - New SKPoint(radius2 / 2, radius2 / 2)

                        Dim gradPen As New SKPaint()
                        With gradPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                            .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                        canvas.DrawRoundRect(rect1, 2, 2, gradPen)

                    canvas.DrawRoundRect(rect1, 2, 2, myPen2)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 10.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = LineColor
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("FS", 0, 0).GetBounds(trect)

                    Dim ax, ay As Integer
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    Using New SKAutoCanvasRestore(canvas)
                        StraightCanvas(canvas)
                        canvas.DrawText("FS", ax, ay, tpaint)
                    End Using

                Case 1

                    'b/w

                    With myPen2
                        .Color = SKColors.Black
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .IsStroke = True
                        .StrokeWidth = LineWidth
                    End With

                    Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + Height)
                    canvas.DrawRoundRect(rect1, 2, 2, myPen2)

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 10.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = SKColors.Black
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("FS", 0, 0).GetBounds(trect)

                    Dim ax, ay As Integer
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    Using New SKAutoCanvasRestore(canvas)
                        StraightCanvas(canvas)
                        canvas.DrawText("FS", ax, ay, tpaint)
                    End Using

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