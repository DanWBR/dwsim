Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.DrawingTools.Point
Imports DWSIM.Interfaces

Namespace GraphicObjects.Shapes

    Public Class MaterialStreamGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream
            Me.Description = "Material Stream"
            EmbeddedResourceIconName = "material_stream.png"
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

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Active = False

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
                        .Color = LineColor.WithAlpha(50)
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
                        .Color = SKColors.Black
                        .StrokeWidth = LineWidth
                        .IsStroke = True
                        .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                    End With

                    canvas.DrawPath(gp, myPen)

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

        Public Overrides Function GetPointValue(type As PointValueType, X As Integer, Y As Integer, args As List(Of Object)) As Double

            If X >= 0 And X <= Width And Y >= 0 And Y <= Height Then
                Dim ms = DirectCast(Owner, IMaterialStream)
                Select Case type
                    Case PointValueType.Temperature
                        Return ms.GetTemperature()
                    Case PointValueType.Pressure
                        Return ms.GetPressure()
                    Case PointValueType.Flow
                        Return ms.GetMassFlow()
                    Case PointValueType.EnergyFlow
                        Return ms.GetEnergyFlow()
                    Case PointValueType.Concentration
                        Return ms.GetCompoundMassConcentration(args(0))
                    Case PointValueType.CompoundMassFlow
                        Return ms.Phases(0).Compounds(args(0)).MassFlow.GetValueOrDefault()
                    Case PointValueType.CompoundMolarFlow
                        Return ms.Phases(0).Compounds(args(0)).MolarFlow.GetValueOrDefault()
                    Case PointValueType.CompoundMassFraction
                        Return ms.Phases(0).Compounds(args(0)).MassFraction.GetValueOrDefault()
                    Case PointValueType.CompoundMolarFraction
                        Return ms.Phases(0).Compounds(args(0)).MoleFraction.GetValueOrDefault()
                    Case PointValueType.MeanSolidParticleSize
                        Return ms.Phases(7).Properties.particleSize_Mean.GetValueOrDefault()
                    Case Else
                        Return Double.NaN
                End Select
            Else
                Return Double.NaN
            End If

        End Function

    End Class

End Namespace