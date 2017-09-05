Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports OxyPlot
Imports DWSIM.Interfaces

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

        Public Property Flowsheet As IFlowsheet

        Public Property OwnerID As String = ""

        Public Property ModelName As String = ""

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            If Flowsheet IsNot Nothing AndAlso Flowsheet.SimulationObjects.ContainsKey(OwnerID) Then
                Try
                    Dim model = Flowsheet.SimulationObjects(OwnerID).GetChartModel(ModelName)
                    If model IsNot Nothing Then
                        Using bm = New Bitmap(Width, Height, Imaging.PixelFormat.Format32bppArgb)
                            Using gr = Graphics.FromImage(bm)
                                gr.CompositingQuality = CompositingQuality.Default
                                gr.InterpolationMode = InterpolationMode.Bicubic
                                gr.SmoothingMode = SmoothingMode.Default
                                gr.TextRenderingHint = Text.TextRenderingHint.SystemDefault
                                If renderer Is Nothing Then renderer = New WindowsForms.GraphicsRenderContext(g)
                                renderer.SetGraphicsTarget(gr)
                                DirectCast(model, IPlotModel).Update(True)
                                DirectCast(model, IPlotModel).Render(renderer, Width, Height)
                                g.DrawImage(bm, X, Y)
                            End Using
                        End Using
                    Else
                        DrawText(g, "Chart model not found.")
                    End If
                Catch ex As Exception
                    DrawText(g, "Error: " + ex.Message)
                End Try
            Else
                DrawText(g, "Referenced flowsheet object not found.")
            End If

        End Sub

        Private Sub DrawText(g As Graphics, txt As String)

            Dim f As New Font("Arial", 10)

            Dim format10 As New StringFormat(StringFormatFlags.NoClip)
            Dim Size = g.MeasureString(Flowsheet.GetTranslatedString("DoubleClickToEdit"), f, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            g.DrawString(Flowsheet.GetTranslatedString("DoubleClickToEdit"), f, New SolidBrush(Color.Black), X + (Width - Size.Width) / 2, Y + (Height - Size.Height) / 2, format10)
            g.DrawRectangle(New Pen(Brushes.Black), New Rectangle(X, Y, Width, Height))

        End Sub


    End Class

End Namespace