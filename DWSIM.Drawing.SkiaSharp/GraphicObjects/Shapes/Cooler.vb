﻿Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
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

            Dim myIC2 As New ConnectionPoint
            myIC2.Position = New Point(X + 0.5 * Width, Y)
            myIC2.Type = ConType.ConEn
            myIC2.Direction = ConDir.Up

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Direction = ConDir.Down
            Me.EnergyConnector.ConnectorName = "Energy Stream (Primary)"
            Me.EnergyConnector.Active = True

            With InputConnectors

                If .Count = 1 Or .Count = 2 Then
                    .Item(0).Position = New Point(X, Y + 0.5 * Height)
                    .Item(1).Position = New Point(X + 0.5 * Width, Y)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If

                .Item(0).ConnectorName = "Inlet"
                .Item(1).ConnectorName = "Energy Stream (Secondary)"

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

                Case 0

                    'default
                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = LineColor
                        .StrokeWidth = LineWidth
                        .IsStroke = True
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

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, gradPen)

                    canvas.DrawPath(gp, myPen)


                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 14.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = LineColor
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("C", 0, 0).GetBounds(trect)

                    Dim ax, ay As Double
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2 - 1.0
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    canvas.DrawText("C", ax, ay, tpaint)

                Case 1

                    'b/w
                    Dim myPen As New SKPaint()
                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
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

                    Dim tpaint As New SKPaint()

                    With tpaint
                        .TextSize = 14.0#
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .Color = SKColors.Black
                        .IsStroke = False
                        .Typeface = BoldTypeFace
                    End With

                    Dim trect As New SKRect(0, 0, 2, 2)
                    tpaint.GetTextPath("C", 0, 0).GetBounds(trect)

                    Dim ax, ay As Double
                    ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2 - 1.0
                    ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

                    canvas.DrawText("C", ax, ay, tpaint)

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