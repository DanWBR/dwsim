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
    Public Sub AddYAxis(model As PlotModel, title As String, key As String, position As AxisPosition, positiontier As Integer)
        model.Axes.Add(New LinearAxis() With { _
            .MajorGridlineStyle = LineStyle.Dash, _
            .MinorGridlineStyle = LineStyle.Dot, _
            .Position = position, _
            .FontSize = 12, _
            .Title = title, _
            .Key = key, _
            .PositionTier = positiontier _
        })
    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddLineSeriesWithKeys(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), title As String, yaxiskey As String, xaxiskey As String)
        model.AddLineSeries(xSeries, ySeries, OxyColors.Automatic, title)
        DirectCast(model.Series(model.Series.Count - 1), LineSeries).YAxisKey = yaxiskey
        DirectCast(model.Series(model.Series.Count - 1), LineSeries).XAxisKey = xaxiskey
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
    Public Sub AddAreaSeries(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), ySeries2 As IEnumerable(Of Double), color As OxyColor, ByVal title As String, ByVal displaylabel As Boolean)

        Dim areaseries As AreaSeries = New AreaSeries

        With areaseries
            .Fill = color
            .StrokeThickness = 0.0
            .Title = title
            .LineStyle = LineStyle.None
            '.InterpolationAlgorithm = InterpolationAlgorithms.UniformCatmullRomSpline
            .Color = OxyColors.Transparent
            .Color2 = OxyColors.Transparent
            .MinimumSegmentLength = 0.01
        End With

        For i As Integer = 0 To xSeries.Count - 1
            areaseries.Points.Add(New DataPoint(xSeries(i), ySeries(i)))
            areaseries.Points2.Add(New DataPoint(xSeries(i), ySeries2(i)))
        Next

        model.Series.Add(areaseries)

        If displaylabel Then
            model.Annotations.Add(New Annotations.TextAnnotation() With {.TextPosition = New DataPoint(0.5, (ySeries.Average + ySeries2.Average) / 2), .Text = title, .TextColor = OxyColors.Black, .FontSize = 10, .Background = OxyColors.Transparent, .StrokeThickness = 0.0})
        End If

    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddAreaSeriesBeyond(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), color As OxyColor, ByVal title As String, ByVal displaylabel As Boolean)

        Dim areaseries As AreaSeries = New AreaSeries

        With areaseries
            .Fill = color
            .StrokeThickness = 0.0
            .Title = title
            .LineStyle = LineStyle.None
            '.InterpolationAlgorithm = InterpolationAlgorithms.UniformCatmullRomSpline
            .Color = OxyColors.Transparent
            .Color2 = OxyColors.Transparent
            .MinimumSegmentLength = 0.01
        End With

        For i As Integer = 0 To xSeries.Count - 1
            areaseries.Points.Add(New DataPoint(xSeries(i), 0.0))
            areaseries.Points2.Add(New DataPoint(xSeries(i), ySeries(i)))
        Next

        model.Series.Add(areaseries)

        If displaylabel Then
            model.Annotations.Add(New Annotations.TextAnnotation() With {.TextPosition = New DataPoint(0.5, ySeries.Average / 2), .Text = title, .TextColor = OxyColors.Black, .FontSize = 10, .Background = OxyColors.Transparent, .StrokeThickness = 0.0})
        End If

    End Sub

    <System.Runtime.CompilerServices.Extension>
    Public Sub AddAreaSeriesAbove(model As PlotModel, xSeries As IEnumerable(Of Double), ySeries As IEnumerable(Of Double), maxvalue As Double, color As OxyColor, ByVal title As String, ByVal displaylabel As Boolean)

        Dim areaseries As AreaSeries = New AreaSeries

        With areaseries
            .Fill = color
            .StrokeThickness = 0.0
            .Title = title
            .LineStyle = LineStyle.None
            '.InterpolationAlgorithm = InterpolationAlgorithms.UniformCatmullRomSpline
            .Color = OxyColors.Transparent
            .Color2 = OxyColors.Transparent
            .MinimumSegmentLength = 0.01
        End With

        For i As Integer = 0 To xSeries.Count - 1
            areaseries.Points.Add(New DataPoint(xSeries(i), ySeries(i)))
            areaseries.Points2.Add(New DataPoint(xSeries(i), maxvalue))
        Next

        model.Series.Add(areaseries)

        If displaylabel Then
            model.Annotations.Add(New Annotations.TextAnnotation() With {.TextPosition = New DataPoint(0.5, (maxvalue + ySeries.Average) / 2), .Text = title, .TextColor = OxyColors.Black, .FontSize = 10, .Background = OxyColors.Transparent, .StrokeThickness = 0.0})
        End If

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

    <System.Runtime.CompilerServices.Extension>
    Public Function GetClipboardData(model As PlotModel) As String

        Dim sb As New Text.StringBuilder

        For Each ls As LineSeries In model.Series.Where(Function(x) TypeOf x Is LineSeries)
            sb.AppendLine(ls.Title)
            sb.AppendLine()
            sb.AppendLine(ls.XAxis.Title & vbTab & ls.YAxis.Title)
            For Each p In ls.Points
                sb.AppendLine(p.X & vbTab & p.Y)
            Next
            sb.AppendLine()
        Next

        Return sb.ToString

    End Function

End Module