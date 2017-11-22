Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects.Shapes

    Public Class AbsorptionColumnGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.AbsorptionColumn
            CreateConnectors(10, 10)
            Me.Description = "Absorption Column"
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
                    For i As Integer = 0 To InCount - 1
                        .Item(i).Position = New Point(X, Y + (i + 1) / InCount * Height)
                        .Item(i).Direction = ConDir.Right
                        .Item(i).ConnectorName = "Column Feed Port #" & (i + 1)
                    Next
            End With

            With OutputConnectors
                If Me.Shape = 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.3 * Height)
                Else
                    .Item(0).Position = New Point(X + Width, Y + 0.02 * Height)
                End If
                .Item(1).Position = New Point(X + Width, Y + 0.98 * Height)
                .Item(0).ConnectorName = "Top Product"
                .Item(1).ConnectorName = "Bottoms Product"
                For i As Integer = 2 To OutCount - 1
                    .Item(i).Position = New Point(X, Y + (i + 1) / InCount * Height)
                    .Item(i).Direction = ConDir.Left
                    .Item(i).ConnectorName = "Side Draw #" & (i - 1)
                Next
            End With

            EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(10, 10)
            UpdateStatus()

            MyBase.Draw(g)

            Dim myPen As New SKPaint()
            With myPen
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = Not Fill
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

           canvas.DrawRoundRect(New SKRect(X + (0.05) * 1.25 * Width, Y + 0.1 * Height, X + (0.05) * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height), 10, 10, myPen)
           

            'Me.DrawRoundRect(g, X + 0.05 * 1.25 * Width, Y + 0.1 * Height, X + 0.05 * 1.25 * Width + 0.2 * 1.25 * Width, Y + 0.1 * Height + 0.8 * Height, 20, myPen)

            canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.1 * Height), New SKPoint(X + 0.175 * Width, Y + 0.02 * Height), New SKPoint(X + Width, Y + 0.02 * Height)}, myPen)
            canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.175 * Width, Y + 0.9 * Height), New SKPoint(X + 0.175 * Width, Y + 0.98 * Height), New SKPoint(X + Width, Y + 0.98 * Height)}, myPen)

            'canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.1 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.1 * Height) + (0.15 * Height)), myPen)
            'canvas.DrawOval(New SKRect((X + 0.525 * 1.25 * Width), (Y + 0.75 * Height), (X + 0.525 * 1.25 * Width) + (0.15 * 1.25 * Width), (Y + 0.75 * Height) + (0.15 * Height)), myPen)

            'canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.25 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.3 * Height)}, myPen)
            'canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.75 * Height), New SKPoint(X + 0.6 * 1.25 * Width, Y + 0.7 * Height)}, myPen)

            'canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.3 * Height), myPen)
            'canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.25 * 1.25 * Width), (Y + 0.7 * Height), myPen)

            'canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.98 * Height), (X + Width), (Y + 0.98 * Height), myPen)

            'If Me.Shape = 1 Then
            '    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.02 * Height), (X + Width), (Y + 0.02 * Height), myPen)
            '    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
            'Else
            '    canvas.DrawLine((X + 0.6 * 1.25 * Width), (Y + 0.3 * Height), (X + Width), (Y + 0.3 * Height), myPen)
            'End If

            'canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.4 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.175 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.125 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.225 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.175 * Height)}, myPen)
            'canvas.DrawLine((X + 0.65 * 1.25 * Width), (Y + 0.175 * Height), (X + Width), (Y + 0.175 * Height), myPen)

            'canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.55 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + 0.575 * 1.25 * Width, Y + 0.875 * Height), New SKPoint(X + 0.625 * 1.25 * Width, Y + 0.775 * Height), New SKPoint(X + 0.65 * 1.25 * Width, Y + 0.825 * Height), New SKPoint(X + Width, Y + 0.825 * Height)}, myPen)
            'canvas.DrawLine((X + 0.5 * 1.25 * Width), (Y + 0.825 * Height), (X + 0.4 * 1.25 * Width), (Y + 0.825 * Height), myPen)

            'canvas.DrawLine((X), (Y + 0.5 * Height), (X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), myPen)

            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.2 * Height), (X + 0.31 * Width), (Y + 0.2 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.3 * Height), (X + 0.31 * Width), (Y + 0.3 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.4 * Height), (X + 0.31 * Width), (Y + 0.4 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.5 * Height), (X + 0.31 * Width), (Y + 0.5 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.6 * Height), (X + 0.31 * Width), (Y + 0.6 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.7 * Height), (X + 0.31 * Width), (Y + 0.7 * Height), myPen)
            canvas.DrawLine((X + 0.05 * 1.25 * Width), (Y + 0.8 * Height), (X + 0.31 * Width), (Y + 0.8 * Height), myPen)

        End Sub

    End Class

End Namespace