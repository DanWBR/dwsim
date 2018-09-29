Imports System.Drawing
Imports System.Drawing.Drawing2D
Imports OxyPlot
Imports DWSIM.Interfaces
Imports OxyPlot.Series

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

        Private Function GetClipboardData(model As PlotModel) As String

            Dim sb As New System.Text.StringBuilder

            sb.AppendLine(model.Title)
            sb.AppendLine(model.Subtitle)

            sb.AppendLine()

            For Each ls As LineSeries In model.Series.Where(Function(x) TypeOf x Is LineSeries)
                sb.AppendLine(ls.Title)
                sb.AppendLine(model.Axes(0).Title & vbTab & model.Axes(1).Title)
                For Each p In ls.Points
                    sb.AppendLine(p.X & vbTab & p.Y)
                Next
                sb.AppendLine()
            Next

            Return sb.ToString

        End Function

        Public Function CopyToClipboard() As String
            Dim model = Flowsheet.SimulationObjects(OwnerID).GetChartModel(ModelName)
            If model IsNot Nothing Then
                Return GetClipboardData(model)
            Else
                Return ""
            End If
        End Function

        Public Overrides Sub Draw(ByVal g As System.Drawing.Graphics)

            If Flowsheet IsNot Nothing AndAlso Flowsheet.SimulationObjects.ContainsKey(OwnerID) Then
                Try
                    Dim model = Flowsheet.SimulationObjects(OwnerID).GetChartModel(ModelName)
                    If model IsNot Nothing Then
                        Using bm = New Bitmap(Width * 3, Height * 3, Imaging.PixelFormat.Format32bppArgb)
                            Using gr = Graphics.FromImage(bm)
                                gr.CompositingQuality = CompositingQuality.HighQuality
                                gr.InterpolationMode = InterpolationMode.HighQualityBicubic
                                gr.SmoothingMode = SmoothingMode.AntiAlias
                                gr.ScaleTransform(3, 3)
                                If renderer Is Nothing Then renderer = New WindowsForms.GraphicsRenderContext(gr)
                                renderer.SetGraphicsTarget(gr)
                                DirectCast(model, IPlotModel).Update(True)
                                DirectCast(model, IPlotModel).Render(renderer, Width, Height)
                                g.DrawImage(bm, New Rectangle(X, Y, Width, Height))
                            End Using
                        End Using
                    Else
                        DrawText(g, "Chart model not found.")
                    End If
                Catch ex As Exception
                    DrawText(g, "Error: " + ex.Message)
                End Try
            Else
                If Flowsheet IsNot Nothing Then DrawText(g, Flowsheet.GetTranslatedString("DoubleClickToEdit"))
            End If

        End Sub

        Private Sub DrawText(g As Graphics, txt As String)

            Dim f As New Font("Arial", 10)

            Dim format10 As New StringFormat(StringFormatFlags.NoClip)
            Dim Size = g.MeasureString(txt, f, New PointF(0, 0), New StringFormat(StringFormatFlags.NoClip, 0))
            g.DrawString(txt, f, New SolidBrush(Color.Black), X + (Width - Size.Width) / 2, Y + (Height - Size.Height) / 2, format10)
            g.DrawRectangle(New Pen(Brushes.Black), New Rectangle(X, Y, Width, Height))

        End Sub


    End Class

End Namespace