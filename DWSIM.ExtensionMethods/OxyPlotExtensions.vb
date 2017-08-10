Imports OxyPlot
Imports OxyPlot.Axes
Imports OxyPlot.Series

Public Module OxyPlot

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddScatterSeries(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double))
        model.AddScatterSeries(xSeries, ySeries, OxyColors.Automatic)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddScatterSeries(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), color As OxyColor)

        Dim scatterSeries As ScatterSeries = New ScatterSeries() With {.MarkerFill = color, .MarkerSize = 1}

        For i As Integer = 0 To xSeries.Count - 1
            scatterSeries.Points.Add(New ScatterPoint(xSeries(i), ySeries(i)))
        Next

        model.Series.Add(scatterSeries)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddHighlightedPoint(model As PlotModel, x As Double, y As Double)
        model.AddHighlightedPoint(x, y, OxyColors.Automatic)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddHighlightedPoint(model As PlotModel, x As Double, y As Double, color As OxyColor)

        Dim scatterSeries As ScatterSeries = New ScatterSeries() With {
             .MarkerFill = color,
             .MarkerType = MarkerType.Triangle,
             .MarkerSize = 5
        }

        scatterSeries.Points.Add(New ScatterPoint(x, y))

        model.Series.Add(scatterSeries)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddLineSeries(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), Optional ByVal title As String = Nothing)
        model.AddLineSeries(xSeries, ySeries, OxyColors.Automatic, title)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddLineSeries(model As PlotModel, xSeries As IEnumerable(Of DateTime), ySeries As IEnumerable(Of Double))
        model.AddLineSeries(xSeries, ySeries, OxyColors.Automatic)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddLineSeries(model As PlotModel, xSeries As IEnumerable(Of DateTime), ySeries As IEnumerable(Of Double), color As OxyColor)
        model.Axes.Add(New DateTimeAxis())
        model.AddLineSeries(xSeries.[Select](Function(x) DateTimeAxis.ToDouble(x)), ySeries)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddLineSeries(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), color As OxyColor, Optional ByVal title As String = Nothing)

        Dim lineSeries As LineSeries = New LineSeries With {.Title = title}

        For i As Integer = 0 To xSeries.Count - 1
            LineSeries.Points.Add(New DataPoint(xSeries(i), ySeries(i)))
        Next

        model.Series.Add(LineSeries)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddColumnSeries(model As PlotModel, xLabels As IEnumerable(Of String), ySeries As IEnumerable(Of Double))
        model.AddColumnSeries(xLabels, ySeries, OxyColors.Automatic)
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddColumnSeries(model As PlotModel, xLabels As IEnumerable(Of String), ySeries As IEnumerable(Of Double), color As OxyColor)
        Dim axis As CategoryAxis = New CategoryAxis() With {
             .Position = AxisPosition.Bottom
        }
        axis.Labels.AddRange(xLabels)
        model.Axes.Add(axis)

        Dim columnSeries As ColumnSeries = New ColumnSeries() With {
             .FillColor = color
        }

        columnSeries.Items.AddRange(ySeries.[Select](Function(y) New ColumnItem(y)))

        model.Series.Add(columnSeries)
    End Sub

End Module