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
            myIC2.Position = New Point(X + 0.5 * Width, Y + Height)
            myIC2.Type = ConType.ConEn

            Dim myOC1 As New ConnectionPoint
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

            End With

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Direction = ConDir.Up
            Me.EnergyConnector.ConnectorName = "Energy Stream"
            Me.EnergyConnector.Active = False

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
                .Color = If(GlobalSettings.Settings.DarkMode, SKColors.Black, SKColors.White)
                .IsStroke = False
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
            End With

            Dim rect1 As New SKRect(X + 0.1 * Width, Y, X + 0.8 * Width, Y + 0.8 * Height)

            Dim pt3 As New Point(X + 0.1 * Width, Y + Height)
            Dim pt4 As New Point(X + 0.2 * Width, Y + 0.65 * Height)

            Dim pt5 As New Point(X + 0.9 * Width, Y + Height)
            Dim pt6 As New Point(X + 0.8 * Width, Y + 0.65 * Height)

            Dim pt7 As New Point(X + 0.1 * Width, Y + Height)
            Dim pt8 As New Point(X + 0.9 * Width, Y + Height)

            Dim pt9 As New Point(X + 0.5 * Width, Y)
            Dim pt10 As New Point(X + Width, Y)
            Dim pt11 As New Point(X + Width, Y + 0.25 * Height)
            Dim pt12 As New Point(X + 0.88 * Width, Y + 0.25 * Height)

            Dim gp As New SKPath()

            gp.MoveTo(pt3.X, pt3.Y)

            gp.LineTo(pt4.X, pt4.Y)
            gp.LineTo(pt6.X, pt6.Y)
            gp.LineTo(pt5.X, pt5.Y)

            gp.Close()

            canvas.DrawPath(gp, myPen)

            Dim gp2 As New SKPath()

            gp2.MoveTo(pt9.X, pt9.Y)

            gp2.LineTo(pt10.X, pt10.Y)
            gp2.LineTo(pt11.X, pt11.Y)
            gp2.LineTo(pt12.X, pt12.Y)

            gp.Close()

            canvas.DrawPath(gp2, myPen)

            Dim rect As New SKRect(X, Y, X + Width, Y + Height)

            canvas.DrawOval(rect, myPen2)
            canvas.DrawOval(rect, myPen)

        End Sub

    End Class

End Namespace