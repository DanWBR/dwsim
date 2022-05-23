Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces

Namespace GraphicObjects.Shapes

    Public Class EnergyStreamGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream
            Me.Description = "Energy Stream"
            Me.IsEnergyStream = True
        End Sub

        Public Sub New(ByVal graphicPosition As SKPoint)
            Me.New()
            CreateConnectors(1, 1)
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

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                Else
                    .Add(myOC1)
                End If

            End With

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            CreateConnectors(0, 0)
            UpdateStatus()

            MyBase.Draw(g)

            Dim myPen As New SKPaint()

            Dim gp As New SKPath()

            gp.MoveTo((X), (Y + 0.35 * Height))
            gp.LineTo((X + 0.65 * Width), (Y + 0.35 * Height))
            gp.LineTo((X + 0.65 * Width), (Y + 0.25 * Height))
            gp.LineTo((X + Width), (Y + 0.5 * Height))
            gp.LineTo((X + 0.65 * Width), (Y + 0.75 * Height))
            gp.LineTo((X + 0.65 * Width), (Y + 0.65 * Height))
            gp.LineTo((X), (Y + 0.65 * Height))
            gp.LineTo((X), (Y + 0.35 * Height))

            gp.Close()

            Select Case DrawMode

                Case 0

                    'default

                    Dim gradPen As New SKPaint()
                    With gradPen
                        .Color = SKColors.Yellow.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateCorner(0.1F)
                    End With

                    canvas.DrawPath(gp, gradPen)

                    With myPen
                        .Color = LineColor
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                        .PathEffect = SKPathEffect.CreateCorner(0.1F)
                    End With

                    canvas.DrawPath(gp, myPen)

                Case 1

                    'b/w

                    With myPen
                        .Color = SKColors.Yellow.WithAlpha(50)
                        .StrokeWidth = LineWidth
                        .IsStroke = False
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)

                    Dim myPen2 As New SKPaint()
                    With myPen2
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen2)

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

        Public Overrides Function GetPointValue(type As PointValueType, X As Integer, Y As Integer, args As List(Of Object)) As Double

            If X >= 0 And X <= Width And Y >= 0 And Y <= Height Then
                If type = PointValueType.EnergyFlow Then
                    Return DirectCast(Owner, IEnergyStream).GetEnergyFlow()
                Else
                    Return Double.NaN
                End If
            Else
                Return Double.NaN
            End If

        End Function

    End Class

End Namespace