Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point

Namespace GraphicObjects.Shapes

    Public Class RigorousColumnGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.DistillationColumn
            CreateConnectors(11, 11)
            Me.Description = "Distillation Column"
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

            CreateConnectors(11, 11)

        End Sub

        Public Overrides Sub CreateConnectors(InCount As Integer, OutCount As Integer)

            'Creates all the connection points.

            If InputConnectors.Count = 0 Then

                For i As Integer = 1 To InCount

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConIn
                    InputConnectors.Add(Con)

                Next

            End If

            If OutputConnectors.Count = 0 Then

                For i As Integer = 1 To OutCount

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConOut
                    OutputConnectors.Add(Con)

                Next

            End If

            With InputConnectors
                For i As Integer = 0 To InputConnectors.Count - 2
                    .Item(i).Position = New Point(X + 0.05 * 1.25 * Width, Y + Height * 0.2 + (i + 1) / InCount * Height * 0.6)
                    .Item(i).ConnectorName = "Column Feed Port #" & (i + 1)
                    .Item(i).Direction = ConDir.Right
                Next
                .Item(10).Position = New Point(X + Width, Y + 0.825 * Height)
                .Item(10).ConnectorName = "Reboiler Duty"
                .Item(10).Direction = ConDir.Left
                .Item(10).Type = ConType.ConEn
            End With

            With OutputConnectors
                If Me.Shape = 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.3 * Height)
                Else
                    .Item(0).Position = New Point(X + Width, Y + 0.02 * Height)
                End If
                .Item(1).Position = New Point(X + Width, Y + 0.98 * Height)
                .Item(0).ConnectorName = "Distillate"
                .Item(1).ConnectorName = "Bottoms"
                For i As Integer = 2 To OutputConnectors.Count - 3
                    .Item(i).Position = New Point(X + 0.05 * 1.25 * Width + 0.2 * 1.25 * Width, Y + Height * 0.2 + (i + 1) / OutCount * Height * 0.6)
                    .Item(i).ConnectorName = "Side Draw #" & (i - 1)
                Next
                .Item(9).Position = New Point(X + Width, Y + 0.02 * Height)
                .Item(9).ConnectorName = "Overhead Vapor"
                .Item(9).Direction = ConDir.Right
                .Item(9).Type = ConType.ConOut
                .Item(10).Position = New Point(X + Width, Y + 0.175 * Height)
                .Item(10).ConnectorName = "Condenser Duty"
                .Item(10).Direction = ConDir.Right
                .Item(10).Type = ConType.ConEn
            End With

            EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(11, 11)
            UpdateStatus()

            MyBase.Draw(g)

            Dim myPen As New SKPaint()

            Select Case DrawMode
                Case 0

                    'default

                    Dim r0 As New SKRect(X, Y, X + Width, Y + Height)

                    Dim radius2 = 0.9F * Math.Min(Width, Height)
                    Dim center = New SKPoint(r0.MidX, r0.MidY)
                    Dim offCenter = center - New SKPoint(radius2 / 2, radius2 / 2)

                    With myPen
                        .Color = LineColor.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    DrawGP(canvas, myPen)

                    Dim myPen2 As New SKPaint()
                    With myPen2
                        .Color = LineColor
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    DrawGP(canvas, myPen2)

                Case 1

                    'b/w

                    With myPen
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    DrawGP(canvas, myPen)

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

        Private Sub DrawGP(canvas As SKCanvas, myPen As SKPaint)

            If myPen.IsStroke Then

                canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.1 * Height), New SKPoint(X + 0.175 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.1 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.9 * Height), New SKPoint(X + 0.175 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.9 * Height)}, myPen)

                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.25 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.3 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.75 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.7 * Height)}, myPen)

                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.7 * Height), myPen)

                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.98 * Height), (X + Width), (Y + 0.98 * Height), myPen)

                If Owner.ReboiledAbsorber Then
                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
                Else
                    If Owner.CondenserType = 0 Then
                        'total
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                    ElseIf Owner.CondenserType = 1 Then
                        'partial
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                    Else
                        'full reflux
                        canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
                    End If
                End If

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.125 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.225 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.175 * Height)}, myPen)
                canvas.DrawLine((X + 0.65 * 1.25 * Width), (Y + 0.175 * Height), (X + Width), (Y + 0.175 * Height), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.875 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.775 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + Width, Y + 0.825 * Height)}, myPen)
                canvas.DrawLine((X + 0.5 * 1.25 * Width), (Y + 0.825 * Height), (X + 0.4 * 1.25 * Width), (Y + 0.825 * Height), myPen)

                'canvas.DrawLine((X), (Y + 0.5 * Height), (X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), myPen)

                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.2 * Height), (X + 0.31 * Width), (Y + 0.2 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.31 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.4 * Height), (X + 0.31 * Width), (Y + 0.4 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), (X + 0.31 * Width), (Y + 0.5 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.6 * Height), (X + 0.31 * Width), (Y + 0.6 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.31 * Width), (Y + 0.7 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.8 * Height), (X + 0.31 * Width), (Y + 0.8 * Height), myPen)

            Else

                canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)

                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

            End If



        End Sub

    End Class

End Namespace