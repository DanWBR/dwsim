Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects.Shapes

    Public Class ShortcutColumnGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.ShortcutColumn
            CreateConnectors(2, 2)
            Me.Description = "Shortcut Column"
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

            'Creates all the connection points.

            If InputConnectors.Count = 0 Then

                For I As Integer = 1 To InCount

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConIn
                    InputConnectors.Add(Con)

                Next

            End If

            If OutputConnectors.Count = 0 Then

                For I As Integer = 1 To OutCount

                    Dim Con As New ConnectionPoint
                    Con.Type = ConType.ConOut
                    OutputConnectors.Add(Con)

                Next

            End If

            Dim myIC1 As New ConnectionPoint

            With InputConnectors
                If FlippedH Then
                    .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                    .Item(1).Position = New Point(X, Y + 0.825 * Height)
                Else
                    .Item(0).Position = New Point(X, Y + 0.5 * Height)
                    .Item(1).Position = New Point(X + Width, Y + 0.825 * Height)
                End If
                .Item(1).Direction = ConDir.Left
                .Item(0).ConnectorName = "Inlet"
                .Item(1).ConnectorName = "Reboiler Duty"
                .Item(1).Type = ConType.ConEn
             End With

            With OutputConnectors
                If FlippedH Then
                    If Me.Shape = 0 Then
                        .Item(0).Position = New Point(X, Y + 0.3 * Height)
                    Else
                        .Item(0).Position = New Point(X, Y + 0.02 * Height)
                    End If
                    .Item(1).Position = New Point(X, Y + 0.98 * Height)
                Else
                    If Me.Shape = 0 Then
                        .Item(0).Position = New Point(X + Width, Y + 0.3 * Height)
                    Else
                        .Item(0).Position = New Point(X + Width, Y + 0.02 * Height)
                    End If
                    .Item(1).Position = New Point(X + Width, Y + 0.98 * Height)
                End If
                .Item(0).ConnectorName = "Distillate"
                .Item(1).ConnectorName = "Bottoms"
            End With

            With Me.EnergyConnector
                If FlippedH Then
                    .Position = New Point(X, Y + 0.175 * Height)
                Else
                    .Position = New Point(X + Width, Y + 0.175 * Height)
                End If
                .ConnectorName = "Condenser Duty"
            End With

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

      
            Dim myPen As New SKPaint()
            With myPen
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            If Me.FlippedH = True Then
                canvas.DrawRoundRect(New SKRect(X + (1 - 0.1 - 0.2) * Width, Y + 0.1 * Height, X + (1 - 0.1 - 0.2) * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)
            Else
                canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)
            End If

            If Me.FlippedH = True Then

                'Me.DrawRoundRect(g, X + (1 - 0.1 - 0.2) * Width, Y + 0.1 * Height, X + (1 - 0.1 - 0.2) * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height, 20, myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.825 * Width, Y + 0.1 * Height), New SKPoint(X + 0.825 * Width, Y + 0.02 * Height), New SKPoint(X + 0.4 * Width, Y + 0.02 * Height), New SKPoint(X + 0.4 * Width, Y + 0.1 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.825 * Width, Y + 0.9 * Height), New SKPoint(X + 0.825 * Width, Y + 0.98 * Height), New SKPoint(X + 0.4 * Width, Y + 0.98 * Height), New SKPoint(X + 0.4 * Width, Y + 0.9 * Height)}, myPen)

                canvas.DrawOval(New SKRect(X + (1 - 0.525 - 0.15) * Width, (Y + 0.1 * Height), (X + (1 - 0.525 - 0.15) * Width + 0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
                canvas.DrawOval(New SKRect((X + (1 - 0.525 - 0.15) * Width), (Y + 0.75 * Height), X + (1 - 0.525 - 0.15) * Width + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * Width, Y + 0.25 * Height), New SKPoint(X + 0.4 * Width, Y + 0.3 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * Width, Y + 0.75 * Height), New SKPoint(X + 0.4 * Width, Y + 0.7 * Height)}, myPen)

                canvas.DrawLine((X + 0.4 * Width), (Y + 0.3 * Height), (X + 0.7 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.4 * Width), (Y + 0.7 * Height), (X + 0.7 * Width), (Y + 0.7 * Height), myPen)

                canvas.DrawLine((X + 0.4 * Width), (Y + 0.98 * Height), (X), (Y + 0.98 * Height), myPen)

                If Me.Shape = 1 Then
                    canvas.DrawLine((X + 0.4 * Width), (Y + 0.02 * Height), (X), (Y + 0.02 * Height), myPen)
                    canvas.DrawLine((X + 0.4 * Width), (Y + 0.3 * Height), (X), (Y + 0.3 * Height), myPen)
                Else
                    canvas.DrawLine((X + 0.4 * Width), (Y + 0.3 * Height), (X), (Y + 0.3 * Height), myPen)
                End If

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * Width, Y + 0.175 * Height), New SKPoint(X + 0.45 * Width, Y + 0.175 * Height), New SKPoint(X + 0.425 * Width, Y + 0.125 * Height), New SKPoint(X + 0.375 * Width, Y + 0.225 * Height), New SKPoint(X + 0.35 * Width, Y + 0.175 * Height)}, myPen)
                canvas.DrawLine((X + 0.35 * Width), (Y + 0.175 * Height), (X), (Y + 0.175 * Height), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * Width, Y + 0.825 * Height), New SKPoint(X + 0.45 * Width, Y + 0.825 * Height), New SKPoint(X + 0.425 * Width, Y + 0.875 * Height), New SKPoint(X + 0.375 * Width, Y + 0.775 * Height), New SKPoint(X + 0.35 * Width, Y + 0.825 * Height), New SKPoint(X, Y + 0.825 * Height)}, myPen)
                canvas.DrawLine((X + 0.5 * Width), (Y + 0.825 * Height), (X + 0.6 * Width), (Y + 0.825 * Height), myPen)

                canvas.DrawLine((X + Width), (Y + 0.5 * Height), (X + 0.95 * Width), (Y + 0.5 * Height), myPen)

                canvas.DrawLine((X + 0.7 * Width), (Y + 0.2 * Height), (X + 0.95 * Width), (Y + 0.2 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.3 * Height), (X + 0.95 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.4 * Height), (X + 0.95 * Width), (Y + 0.4 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.5 * Height), (X + 0.95 * Width), (Y + 0.5 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.6 * Height), (X + 0.95 * Width), (Y + 0.6 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.7 * Height), (X + 0.95 * Width), (Y + 0.7 * Height), myPen)
                canvas.DrawLine((X + 0.7 * Width), (Y + 0.8 * Height), (X + 0.95 * Width), (Y + 0.8 * Height), myPen)

            Else

                'Me.DrawRoundRect(g, X + 0.05 * 1.25 * Width, Y + 0.1 * Height, X + 0.05 * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height, 20, myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.1 * Height), New SKPoint(X + 0.175 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.02 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.1 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.9 * Height), New SKPoint(X + 0.175 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.98 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.9 * Height)}, myPen)

                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
                canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.25 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.3 * Height)}, myPen)
                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.75 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.7 * Height)}, myPen)

                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.7 * Height), myPen)

                canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.98 * Height), (X + Width), (Y + 0.98 * Height), myPen)

                If Me.Shape = 1 Then
                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                Else
                    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
                End If

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.125 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.225 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.175 * Height)}, myPen)
                canvas.DrawLine((X + 0.65 * 1.25 * Width), (Y + 0.175 * Height), (X + Width), (Y + 0.175 * Height), myPen)

                canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.875 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.775 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + Width, Y + 0.825 * Height)}, myPen)
                canvas.DrawLine((X + 0.5 * 1.25 * Width), (Y + 0.825 * Height), (X + 0.4 * 1.25 * Width), (Y + 0.825 * Height), myPen)

                canvas.DrawLine((X), (Y + 0.5 * Height), (X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), myPen)

                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.2 * Height), (X + 0.31 * Width), (Y + 0.2 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.31 * Width), (Y + 0.3 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.4 * Height), (X + 0.31 * Width), (Y + 0.4 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), (X + 0.31 * Width), (Y + 0.5 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.6 * Height), (X + 0.31 * Width), (Y + 0.6 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.31 * Width), (Y + 0.7 * Height), myPen)
                canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.8 * Height), (X + 0.31 * Width), (Y + 0.8 * Height), myPen)

                Dim tpaint As New SKPaint()
                With tpaint
                    .TextSize = 14.0#
                    .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    .Color = LineColor
                    .IsStroke = False
                    .Typeface = DefaultTypeFace
                End With

                canvas.DrawText("SC", X + 0.5 * Width, Y + 0.5 * Height, tpaint)

            End If

        End Sub

    End Class

End Namespace