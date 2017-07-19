Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects.Shapes

    Public Class MaterialStreamGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream
            Me.Description = "Material Stream"
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

            Dim myOC1 As New ConnectionPoint
            myOC1.Position = New Point(X + Width, Y + 0.5 * Height)
            myOC1.Type = ConType.ConOut

            Me.EnergyConnector.Position = New Point(X + 0.5 * Width, Y + Height)
            Me.EnergyConnector.Type = ConType.ConEn
            Me.EnergyConnector.Active = False

            With InputConnectors

                If .Count <> 0 Then
                    If Me.FlippedH Then
                        .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                    Else
                        .Item(0).Position = New Point(X, Y + 0.5 * Height)
                    End If
                Else
                    .Add(myIC1)
                End If

            End With

            With OutputConnectors

                If .Count <> 0 Then
                    If Me.FlippedH Then
                        .Item(0).Position = New Point(X, Y + 0.5 * Height)
                    Else
                        .Item(0).Position = New Point(X + Width, Y + 0.5 * Height)
                    End If
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
            With myPen
                .Color = LineColor
                .StrokeWidth = LineWidth
                .IsStroke = False
                .IsAntialias = GlobalSettings.Settings.DrawingAntiAlias
                .PathEffect = SKPathEffect.CreateCorner(1.0F)
            End With

            Dim gp As New SKPath()

           
            If Me.FlippedH = False Then
                gp.MoveTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.35 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.75 * Width), Convert.ToInt32(Y + 0.35 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.75 * Width), Convert.ToInt32(Y + 0.25 * Height))
                gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.5 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.75 * Width), Convert.ToInt32(Y + 0.75 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.75 * Width), Convert.ToInt32(Y + 0.65 * Height))
                gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.65 * Height))
                gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.35 * Height))
            Else
                gp.MoveTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.35 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.25 * Width), Convert.ToInt32(Y + 0.35 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.25 * Width), Convert.ToInt32(Y + 0.25 * Height))
                gp.LineTo(Convert.ToInt32(X), Convert.ToInt32(Y + 0.5 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.25 * Width), Convert.ToInt32(Y + 0.75 * Height))
                gp.LineTo(Convert.ToInt32(X + 0.25 * Width), Convert.ToInt32(Y + 0.65 * Height))
                gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.65 * Height))
                gp.LineTo(Convert.ToInt32(X + Width), Convert.ToInt32(Y + 0.35 * Height))
            End If

            gp.Close()

            canvas.DrawPath(gp, myPen)

        End Sub

    End Class

End Namespace