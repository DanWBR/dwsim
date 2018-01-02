Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects.Shapes

    Public Class CSTRGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.RCT_CSTR
            Me.Description = "CSTR"
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

            Dim myIC1 As New ConnectionPoint
            myIC1.Position = New Point(X, Y + 0.5 * Height)
            myIC1.Type = ConType.ConIn

            Dim myIC2 As New ConnectionPoint
            myIC2.Position = New Point(X + 0.125 * Width, Y + 0.7 * Height)
            myIC2.Type = ConType.ConEn

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Dim myOC2 As New ConnectionPoint
            myOC2.Position = New Point(X + 0.5 * Width, Y)
            myOC2.Type = ConType.ConOut
            myOC2.Direction = ConDir.Up

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Direction = ConDir.Up
            Me.EnergyConnector.Active = False

            With InputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X, Y + 0.5 * Height)
                    .Item(1).Position = New Point(X + 0.125 * Width, Y + 0.7 * Height)
                Else
                    .Add(myIC1)
                    .Add(myIC2)
                End If

                .Item(0).ConnectorName = "Inlet"
                .Item(1).ConnectorName = "Energy Stream"

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    If Me.FlippedH Then
                        .Item(0).Position = New Point(X, Y + 0.5 * Height)
                        .Item(1).Position = New Point(X + 0.5 * Width, Y)
                    Else
                        .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                        .Item(1).Position = New Point(X + 0.5 * Width, Y)
                    End If
                Else
                    .Add(myOC1)
                    .Add(myOC2)
                End If

                .Item(0).ConnectorName = "Liquid Outlet"
                .Item(1).ConnectorName = "Vapor Outlet (Optional)"

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
            Dim myPen2 As New SKPaint()
            With myPen2
                .Color = SKColors.White
                .StrokeWidth = LineWidth
                .IsStroke = False
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect1 As New SKRect(X + 0.1 * Width, Y + 0.1 * Height, X + 0.9 * Width, Y + 0.9 * Height)
            Dim rect2 As New SKRect(X + 0.1 * Width, Y, X + 0.9 * Width, Y + 0.2 * Height)
            Dim rect3 As New SKRect(X + 0.1 * Width, Y + 0.8 * Height, X + 0.9 * Width, Y + Height)

            canvas.DrawOval(rect3, myPen)
            canvas.DrawRect(rect1, myPen)
            canvas.DrawRect(rect1, myPen2)
            canvas.DrawOval(rect2, myPen2)
            canvas.DrawOval(rect2, myPen)
            canvas.DrawOval(rect3, myPen2)

            canvas.DrawPoints(SKPointMode.Polygon, New SKPoint() {New SKPoint(X + 0.5 * Width, Y - 0.1 * Height), New SKPoint(X + 0.5 * Width, Y + 0.7 * Height)}, myPen)
            canvas.DrawOval(New SKRect(X + 0.2 * Width, Y + 0.6 * Height, X + 0.5 * Width, Y + 0.7 * Height), myPen)
            canvas.DrawOval(New SKRect(X + 0.5 * Width, Y + 0.6 * Height, X + 0.8 * Width, Y + 0.7 * Height), myPen)


        End Sub

    End Class

End Namespace
