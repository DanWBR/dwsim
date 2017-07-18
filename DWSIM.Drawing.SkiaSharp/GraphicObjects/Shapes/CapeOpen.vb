Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects

Namespace GraphicObjects.Shapes

    Public Class CAPEOPENGraphic

        Inherits ShapeGraphic

#Region "Constructors"

        Public Sub New()
            Me.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.CapeOpenUO
            Me.Description = "CAPE-OPEN Unit Operation Block"
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

            If Not Me.AdditionalInfo Is Nothing Then

                Dim obj1(Me.InputConnectors.Count), obj2(Me.InputConnectors.Count) As Double
                Dim obj3(Me.OutputConnectors.Count), obj4(Me.OutputConnectors.Count) As Double
                obj1 = Me.AdditionalInfo(0)
                obj2 = Me.AdditionalInfo(1)
                obj3 = Me.AdditionalInfo(2)
                obj4 = Me.AdditionalInfo(3)

                Try
                    Dim i As Integer = 0
                    For Each ic As ConnectionPoint In Me.InputConnectors
                        ic.Position = New Point(Me.X + obj1(i), Me.Y + obj2(i))
                        i = i + 1
                    Next
                    i = 0
                    For Each oc As ConnectionPoint In Me.OutputConnectors
                        oc.Position = New Point(Me.X + obj3(i), Me.Y + obj4(i))
                        i = i + 1
                    Next
                Catch ex As Exception

                End Try

            Else

                Try
                    Dim i As Integer = 0
                    For Each ic As ConnectionPoint In Me.InputConnectors
                        ic.Position = New Point(Me.X, Me.Y + (i + 1) / InputConnectors.Count * Height)
                        i = i + 1
                    Next
                    i = 0
                    For Each oc As ConnectionPoint In Me.OutputConnectors
                        oc.Position = New Point(Me.X + Width, Me.Y + (i + 1) / OutputConnectors.Count * Height)
                        i = i + 1
                    Next
                Catch ex As Exception

                End Try

            End If

            Me.EnergyConnector.Active = False

        End Sub

        Public Overrides Sub Draw(ByVal g As Object)

            Dim canvas As SKCanvas = DirectCast(g, SKCanvas)

            MyBase.Draw(g)

            PositionConnectors()
            UpdateStatus()

            Dim myPen2 As New SKPaint()
            With myPen2
                .Color = LineColor
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
                .Color = LineColor
                .IsStroke = False
                .Typeface = DefaultTypeFace
            End With

            Dim trect As New SKRect(0, 0, 2, 2)
            tpaint.GetTextPath("CO", 0, 0).GetBounds(trect)

            Dim ax, ay As Integer
            ax = Me.X + (Me.Width - (trect.Right - trect.Left)) / 2
            ay = Me.Y + (Me.Height - (trect.Top - trect.Bottom)) / 2

            canvas.DrawText("CO", ax, ay, tpaint)

        End Sub

    End Class

End Namespace