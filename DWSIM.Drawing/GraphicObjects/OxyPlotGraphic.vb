Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports OxyPlot

Namespace GraphicObjects

    <Serializable()> _
    Public Class OxyPlotGraphic

        Inherits GraphicObject

        Private renderer As OxyPlot.WindowsForms.GraphicsRenderContext

        Public Sub New()
            MyBase.New()
            Me.ObjectType = Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart
            Description = "Chart Object"
        End Sub

        Public Sub New(ByVal graphicPosition As Point, ByVal graphicSize As Size)
            Me.New()
            Me.SetSize(graphicSize)
            Me.AutoSize = False
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal graphicSize As Size)
            Me.New(New Point(posX, posY), graphicSize)
        End Sub

        Public Sub New(ByVal graphicPosition As Point, ByVal text As String)
            Me.New()
            Me.SetPosition(graphicPosition)
        End Sub

        Public Sub New(ByVal posX As Integer, ByVal posY As Integer, ByVal width As Integer, ByVal height As Integer)
            Me.New(New Point(posX, posY), New Size(width, height))
        End Sub

        Public Property Scale As Double = 1.0

        Public Property ModelName As String = ""

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            Dim model = DirectCast(Owner.GetChartModel(ModelName), IPlotModel)

            If model IsNot Nothing Then

                If renderer Is Nothing Then renderer = New WindowsForms.GraphicsRenderContext(g)
                renderer.SetGraphicsTarget(g)
                model.Render(renderer, Width / Scale, Height / Scale)

            End If

        End Sub

    End Class

End Namespace