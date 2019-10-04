<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IChart

    Property ID As String

    Property DisplayName As String

    Property ChartType As Enums.ChartType

    Property ChartSource As Enums.ChartSource

    Property PlotModel As Object 'OxyPlot PlotModel, ALWAYS

    Property ChartSourceObjectID As String

    Property ChartSourceObjectChartID As String

    Property SpreadsheetDataSourcesX As List(Of String)

    Property SpreadsheetDataSourcesY As List(Of String)


End Interface