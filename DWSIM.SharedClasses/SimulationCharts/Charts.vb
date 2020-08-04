Imports DWSIM.Interfaces.Enums
Imports OxyPlot
Imports OxyPlot.Series
Imports System.Xml.Serialization

Namespace Charts

    Public Class Chart

        Implements IChart, ICustomXMLSerialization

        Public Property ID As String Implements IChart.ID

        Public Property DisplayName As String = "MyChart" Implements IChart.DisplayName

        Public Property ChartType As ChartType = ChartType.TwoDimensionalScatter Implements IChart.ChartType

        Public Property ChartSource As ChartSource = ChartSource.SpreadsheetRange Implements IChart.ChartSource

        <XmlIgnore>
        Public Property PlotModel As Object Implements IChart.PlotModel

        Public Property ChartSourceObjectID As String = "" Implements IChart.ChartSourceObjectID

        Public Property ChartSourceObjectChartID As String = "" Implements IChart.ChartSourceObjectChartID

        Public Property ChartSourceObjectAutoUpdate As Boolean = True Implements IChart.ChartSourceObjectAutoUpdate

        Public Property SpreadsheetDataSourcesX As List(Of String) = New List(Of String) Implements IChart.SpreadsheetDataSourcesX

        Public Property SpreadsheetDataSourcesY As List(Of String) = New List(Of String) Implements IChart.SpreadsheetDataSourcesY

        Public Sub New()

            ID = Guid.NewGuid().ToString()

            DisplayName = "MyChart"

            PlotModel = CreatePlotModel(New Double() {}, New Double() {}, "Title", "Subtitle", "XTitle", "YTitle")

        End Sub

        Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData

            Dim elements = XMLSerializer.XMLSerializer.Serialize(Me)

            If PlotModel Is Nothing Then Return elements

            Dim el_model = XMLSerializer.XMLSerializer.Serialize(PlotModel)
            elements.Add(New XElement("PlotModel", el_model))

            Dim series As New List(Of XElement)
            For Each s In DirectCast(PlotModel, PlotModel).Series
                series.Add(New XElement("LineSeries", XMLSerializer.XMLSerializer.Serialize(s)))
            Next

            elements.Add(New XElement("Series", series))

            Dim axes As New List(Of XElement)
            For Each s In DirectCast(PlotModel, PlotModel).Axes
                s.AbsoluteMaximum = 1.0E+100
                s.AbsoluteMinimum = -1.0E+100
                s.FilterMaxValue = 1.0E+100
                s.FilterMinValue = -1.0E+100
                axes.Add(New XElement("Axis", XMLSerializer.XMLSerializer.Serialize(s)))
            Next

            elements.Add(New XElement("Axes", axes))

            Return elements

        End Function

        Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Try
                XMLSerializer.XMLSerializer.Deserialize(PlotModel, data.Where(Function(e) e.Name = "PlotModel").Elements.ToList())

                DirectCast(PlotModel, PlotModel).Series.Clear()

                For Each el In data.Where(Function(e) e.Name = "Series").Elements.ToList
                    Dim ls As New OxyPlot.Series.LineSeries()
                    ls.Title = ""
                    XMLSerializer.XMLSerializer.Deserialize(ls, el.Elements.ToList)
                    DirectCast(PlotModel, PlotModel).Series.Add(ls)
                Next

                DirectCast(PlotModel, PlotModel).Axes.Clear()

                For Each el In data.Where(Function(e) e.Name = "Axes").Elements.ToList
                    Dim a As New OxyPlot.Axes.LinearAxis()
                    a.Title = ""
                    XMLSerializer.XMLSerializer.Deserialize(a, el.Elements.ToList)
                    DirectCast(PlotModel, PlotModel).Axes.Add(a)
                Next
            Catch ex As Exception

            End Try

            Return True

        End Function

        Public Function CreatePlotModel(ByVal x() As Double, ByVal y() As Double, ByVal title As String, ByVal subtitle As String, ByVal xtitle As String, ByVal ytitle As String) As OxyPlot.PlotModel

            Dim model = New OxyPlot.PlotModel
            model.Background = OxyPlot.OxyColors.White
            model.TitleFontSize = 14
            model.SubtitleFontSize = 12
            model.Axes.Add(New OxyPlot.Axes.LinearAxis With {.MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                .MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                .Position = OxyPlot.Axes.AxisPosition.Bottom,
                .FontSize = 12,
                .Title = xtitle,
                .Key = "x"})
            model.Axes.Add(New OxyPlot.Axes.LinearAxis With {
                .MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                .MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                .Position = OxyPlot.Axes.AxisPosition.Left,
                .FontSize = 12,
                .Title = ytitle
            })
            model.LegendFontSize = 11
            model.LegendPlacement = OxyPlot.LegendPlacement.Outside
            model.LegendOrientation = OxyPlot.LegendOrientation.Vertical
            model.LegendPosition = OxyPlot.LegendPosition.BottomCenter
            model.TitleHorizontalAlignment = OxyPlot.TitleHorizontalAlignment.CenteredWithinView
            model.Title = ""
            model.Subtitle = ""
            model.SubtitleFont = "Arial"
            model.TitleFont = "Arial"

            Dim lineSeries As LineSeries = New LineSeries With {.Title = ytitle, .Color = OxyPlot.OxyColors.Black}

            For i As Integer = 0 To x.Count - 1
                lineSeries.Points.Add(New DataPoint(x(i), y(i)))
            Next

            model.Series.Add(lineSeries)

            Return model

        End Function

    End Class

End Namespace


