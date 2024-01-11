Imports Aga.Controls
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
#If LINUX Then
Imports DWSIM.CrossPlatform.UI.Controls.ReoGrid
#End If
Imports OxyPlot
Imports OxyPlot.Series
Imports System.Linq

Public Class TwoDimChartControl

    Public Property Chart As SharedClasses.Charts.Chart

    Public Property Flowsheet As FormFlowsheet

#If LINUX Then
    Public Property Spreadsheet As ReoGridControl
#Else
    Public Property Spreadsheet As unvell.ReoGrid.ReoGridControl
#End If

    Private ColorChoices As New List(Of String)

    Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.

        Dim colors = GetType(OxyColors).GetFields()

        For Each c In colors
            ColorChoices.Add(c.Name)
        Next

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Sub UpdatePlotModelData()

        If Chart.PlotModel IsNot Nothing Then

            If Chart.ChartSource = ChartSource.FlowsheetObject Then

                If Chart.ChartSourceObjectAutoUpdate Then

                    Chart.PlotModel = Flowsheet.SimulationObjects(Chart.ChartSourceObjectID).GetChartModel(Chart.ChartSourceObjectChartID)

                End If

            Else

                Dim pm As OxyPlot.PlotModel = Chart.PlotModel

                If pm.Series.Count <> Chart.SpreadsheetDataSourcesX.Count And
                    pm.Series.Count <> Chart.SpreadsheetDataSourcesY.Count Then

                    pm.Series.Clear()

                    Dim xnumbers As New List(Of List(Of Double))
                    Dim ynumbers As New List(Of List(Of Double))

                    For Each item In Chart.SpreadsheetDataSourcesX
                        Dim xlist As New List(Of Double)
                        Dim sheet = Spreadsheet.GetWorksheetByName(item.Split("!")(0))
#If LINUX Then
                        Dim data As Object(,) = sheet.GetRangeData(New RangePosition(item.Split("!")(1)))
#Else
                        Dim data As Object(,) = sheet.GetRangeData(New unvell.ReoGrid.RangePosition(item.Split("!")(1)))
#End If
                        If data.GetLength(0) > 1 Then
                            Dim j As Integer = 0
                            For j = 0 To data.GetLength(0) - 1
                                Dim d As Double = 0.0
                                Double.TryParse(data(j, 0).ToString, d)
                                xlist.Add(d)
                            Next
                        ElseIf data.GetLength(1) > 1 Then
                            Dim j As Integer = 0
                            For j = 0 To data.GetLength(1) - 1
                                Dim d As Double = 0.0
                                Double.TryParse(data(0, j).ToString, d)
                                xlist.Add(d)
                            Next
                        End If
                        xnumbers.Add(xlist)
                    Next

                    For Each item In Chart.SpreadsheetDataSourcesY
                        Dim ylist As New List(Of Double)
                        Dim sheet = Spreadsheet.GetWorksheetByName(item.Split("!")(0))
#If LINUX Then
                        Dim data As Object(,) = sheet.GetRangeData(New RangePosition(item.Split("!")(1)))
#Else
                        Dim data As Object(,) = sheet.GetRangeData(New unvell.ReoGrid.RangePosition(item.Split("!")(1)))
#End If
                        If data.GetLength(0) > 1 Then
                            Dim j As Integer = 0
                            For j = 0 To data.GetLength(0) - 1
                                Dim d As Double = 0.0
                                Double.TryParse(data(j, 0).ToString, d)
                                ylist.Add(d)
                            Next
                        ElseIf data.GetLength(1) > 1 Then
                            Dim j As Integer = 0
                            For j = 0 To data.GetLength(1) - 1
                                Dim d As Double = 0.0
                                Double.TryParse(data(0, j).ToString, d)
                                ylist.Add(d)
                            Next
                        End If
                        ynumbers.Add(ylist)
                    Next

                    For i As Integer = 0 To xnumbers.Count - 1

                        pm.AddLineSeries(xnumbers(i), ynumbers(i), "Series" & (i + 1).ToString)

                    Next

                Else

                    Dim xnumbers As New List(Of List(Of Double))
                    Dim ynumbers As New List(Of List(Of Double))

                    For Each item In Chart.SpreadsheetDataSourcesX
                        Dim xlist As New List(Of Double)
                        Dim sheet = Spreadsheet.GetWorksheetByName(item.Split("!")(0))
                        If Not sheet Is Nothing Then
#If LINUX Then
                            Dim data As Object(,) = sheet.GetRangeData(New RangePosition(item.Split("!")(1)))
#Else
                            Dim data As Object(,) = sheet.GetRangeData(New unvell.ReoGrid.RangePosition(item.Split("!")(1)))
#End If
                            If data.GetLength(0) > 1 Then
                                Dim j As Integer = 0
                                For j = 0 To data.GetLength(0) - 1
                                    Dim d As Double = 0.0
                                    Double.TryParse(data(j, 0).ToString, d)
                                    xlist.Add(d)
                                Next
                            ElseIf data.GetLength(1) > 1 Then
                                Dim j As Integer = 0
                                For j = 0 To data.GetLength(1) - 1
                                    Dim d As Double = 0.0
                                    Double.TryParse(data(0, j).ToString, d)
                                    xlist.Add(d)
                                Next
                            End If
                            xnumbers.Add(xlist)
                        End If
                    Next

                    For Each item In Chart.SpreadsheetDataSourcesY
                        Dim ylist As New List(Of Double)
                        Dim sheet = Spreadsheet.GetWorksheetByName(item.Split("!")(0))
                        If Not sheet Is Nothing Then
#If LINUX Then
                        Dim data As Object(,) = sheet.GetRangeData(New RangePosition(item.Split("!")(1)))
#Else
                            Dim data As Object(,) = sheet.GetRangeData(New unvell.ReoGrid.RangePosition(item.Split("!")(1)))
#End If
                            If data.GetLength(0) > 1 Then
                                Dim j As Integer = 0
                                For j = 0 To data.GetLength(0) - 1
                                    Dim d As Double = 0.0
                                    Double.TryParse(data(j, 0).ToString, d)
                                    ylist.Add(d)
                                Next
                            ElseIf data.GetLength(1) > 1 Then
                                Dim j As Integer = 0
                                For j = 0 To data.GetLength(1) - 1
                                    Dim d As Double = 0.0
                                    Double.TryParse(data(0, j).ToString, d)
                                    ylist.Add(d)
                                Next
                            End If
                            ynumbers.Add(ylist)
                        End If
                    Next

                    For i As Integer = 0 To xnumbers.Count - 1

                        Dim lineSeries As LineSeries = pm.Series(i)

                        lineSeries.Points.Clear()

                        For j As Integer = 0 To xnumbers(i).Count - 1
                            lineSeries.Points.Add(New DataPoint(xnumbers(i)(j), ynumbers(i)(j)))
                        Next

                    Next

                End If

            End If

        End If

    End Sub

    Sub UpdatePropertyGrid()

        Dim pm As OxyPlot.PlotModel = Chart.PlotModel

        If pm IsNot Nothing Then
            If pm.Series.Count = Chart.SpreadsheetDataSourcesX.Count & PGrid1.Item.Count > 0 Then
                PGrid1.Refresh()
                Exit Sub
            End If
        End If

        With PGrid1

            .Item.Clear()

            .Item.Add("Name", Chart, "DisplayName", False, "1. General", "")
            .Item.Add("Type", Chart, "ChartType", True, "1. General", "")
            .Item.Add("Data Source", Chart, "ChartSource", False, "1. General", "")

            If Chart.ChartSource = ChartSource.FlowsheetObject Then
                If Flowsheet.SimulationObjects.ContainsKey(Chart.ChartSourceObjectID) Then
                    Dim sourceobj = Flowsheet.SimulationObjects(Chart.ChartSourceObjectID)
                    .Item.Add("Source Object", sourceobj.GraphicObject.Tag, False, "2. Data Source")
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(Flowsheet.SimulationObjects.Values.Select(Function(x) x.GraphicObject.Tag).ToArray())
                    .Item.Add("Chart Type", Chart, "ChartSourceObjectChartID", False, "2. Data Source")
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(sourceobj.GetChartModelNames().ToArray())
                    .Item.Add("Auto Update Chart from Object", Chart, "ChartSourceObjectAutoUpdate", False, "2. Data Source")
                Else
                    .Item.Add("Source Object", "", False, "2. Data Source")
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(Flowsheet.SimulationObjects.Values.Select(Function(x) x.GraphicObject.Tag).ToArray())
                End If
            Else
                .Item.Add("X Axis Values (Ranges)", Chart, "SpreadsheetDataSourcesX", False, "2. Data Source")
                .Item(.Item.Count - 1).CustomEditor = New StringCollectionEditor(GetType(System.String))
                .Item.Add("Y Axis Values (Ranges)", Chart, "SpreadsheetDataSourcesY", False, "2. Data Source")
                .Item(.Item.Count - 1).CustomEditor = New StringCollectionEditor(GetType(System.String))
            End If

            If pm IsNot Nothing Then

                If pm.Title Is Nothing Then pm.Title = Chart.DisplayName
                .Item.Add("Title", pm, "Title", False, "3. Plot")
                .Item(.Item.Count - 1).DefaultValue = ""

                .Item.Add("Title Position", pm, "TitleHorizontalAlignment", False, "3. Plot")

                .Item.Add("Title Font Size", pm, "TitleFontSize", False, "3. Plot")

                If pm.Subtitle Is Nothing Then pm.Subtitle = ""
                .Item.Add("Subtitle", pm, "Subtitle", False, "3. Plot")

                .Item.Add("Subtitle Font Size", pm, "SubtitleFontSize", False, "3. Plot")

                .Item.Add("Display Legend", pm, "IsLegendVisible", False, "4. Legend")

                If pm.IsLegendVisible Then

                    .Item.Add("Legend Position", pm, "LegendPosition", False, "4. Legend")

                    .Item.Add("Legend Placement", pm, "LegendPlacement", False, "4. Legend")

                    .Item.Add("Legend Orientation", pm, "LegendOrientation", False, "4. Legend")

                    If pm.LegendTitle Is Nothing Then pm.LegendTitle = ""
                    .Item.Add("Legend Title", pm, "LegendTitle", False, "4. Legend")

                    .Item.Add("Legend Title Font Size", pm, "LegendTitleFontSize", False, "4. Legend")

                    .Item.Add("Legend Item Alignment", pm, "LegendItemAlignment", False, "4. Legend")

                    .Item.Add("Legend Item Order", pm, "LegendItemOrder", False, "4. Legend")

                    .Item.Add("Legend Item Spacing", pm, "LegendItemSpacing", False, "4. Legend")

                    .Item.Add("Legend Line Spacing", pm, "LegendLineSpacing", False, "4. Legend")

                End If

                If pm.Axes(0).Title Is Nothing Then pm.Axes(0).Title = ""
                .Item.Add("X Axis Title", pm.Axes(0), "Title", False, "5. X Axis")

                .Item.Add("X Axis Font Size", pm.Axes(0), "FontSize", False, "5. X Axis")

                .Item.Add("X Axis Title Font Size", pm.Axes(0), "TitleFontSize", False, "5. X Axis")

                .Item.Add("X Axis Title Position", pm.Axes(0), "TitlePosition", False, "5. X Axis")

                If pm.Axes(1).Title Is Nothing Then pm.Axes(1).Title = ""
                .Item.Add("Y Axis Title", pm.Axes(1), "Title", False, "6. Y Axis")

                .Item.Add("Y Axis Font Size", pm.Axes(1), "FontSize", False, "6. Y Axis")

                .Item.Add("Y Axis Title Font Size", pm.Axes(1), "TitleFontSize", False, "6. Y Axis")

                .Item.Add("Y Axis Title Position", pm.Axes(0), "TitlePosition", False, "6. Y Axis")

                Dim i As Integer = 0

                For Each series As LineSeries In pm.Series

                    If series.Title Is Nothing Then series.Title = ""
                    .Item.Add("Title", series, "Title", False, String.Format("Line Series #{0}", i))

                    .Item.Add("Line Type", series, "LineStyle", False, String.Format("Line Series #{0}", i))

                    .Item.Add("Line Color", series.Color, False, String.Format("Line Series #{0}", i))
                    .Item(.Item.Count - 1).Tag = i
                    .Item(.Item.Count - 1).DefaultType = GetType(OxyColor)
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(ColorChoices.ToArray)

                    .Item.Add("Line Width", series, "StrokeThickness", False, String.Format("Line Series #{0}", i))

                    .Item.Add("Marker Type", series, "MarkerType", False, String.Format("Line Series #{0}", i))

                    .Item.Add("Marker Size", series, "MarkerSize", False, String.Format("Line Series #{0}", i))

                    .Item.Add("Marker Fill Color", series.MarkerFill, False, String.Format("Line Series #{0}", i))
                    .Item(.Item.Count - 1).Tag = i
                    .Item(.Item.Count - 1).DefaultType = GetType(OxyColor)
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(ColorChoices.ToArray)

                    .Item.Add("Marker Stroke Color", series.MarkerStroke, False, String.Format("Line Series #{0}", i))
                    .Item(.Item.Count - 1).Tag = i
                    .Item(.Item.Count - 1).DefaultType = GetType(OxyColor)
                    .Item(.Item.Count - 1).Choices = New Controls.PropertyGridEx.CustomChoices(ColorChoices.ToArray)

                    i += 1

                Next

            End If

            .PropertySort = PropertySort.Categorized

            .ShowCustomProperties = True

            .Refresh()

            PlotView1.InvalidatePlot(True)

        End With

    End Sub


    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnUpdate.Click

        Try
            UpdatePlotModelData()
            If Chart IsNot Nothing Then PlotView1.Model = Chart.PlotModel
            PlotView1.InvalidatePlot(True)
        Catch ex As Exception
            Flowsheet.ShowMessage("Chart update error: " & ex.Message, Interfaces.IFlowsheet.MessageType.GeneralError)
        End Try

    End Sub

    Private Sub TwoDimChartControl_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        Try
            UpdatePropertyGrid()
            If Chart IsNot Nothing Then PlotView1.Model = Chart.PlotModel
            UpdatePlotModelData()
            PlotView1.InvalidatePlot(True)
        Catch ex As Exception
            Flowsheet.ShowMessage("Chart update error: " & ex.Message, Interfaces.IFlowsheet.MessageType.GeneralError)
        End Try

    End Sub

    Private Sub PGrid1_PropertyValueChanged(s As Object, e As PropertyValueChangedEventArgs) Handles PGrid1.PropertyValueChanged

        Try
            If e.ChangedItem.Label.Equals("Source Object") Then
                Chart.ChartSourceObjectID = Flowsheet.GetFlowsheetSimulationObject(e.ChangedItem.Value).Name
            ElseIf e.ChangedItem.Label.Equals("Name") Then
                Parent.Text = Chart.DisplayName
            ElseIf e.ChangedItem.Label.Equals("Line Color") Then
                DirectCast(Chart.PlotModel.Series(e.ChangedItem.Tag), LineSeries).Color = GetType(OxyColors).GetField(e.ChangedItem.Value).GetValue(Nothing)
            ElseIf e.ChangedItem.Label.Equals("Marker Fill Color") Then
                DirectCast(Chart.PlotModel.Series(e.ChangedItem.Tag), LineSeries).MarkerFill = GetType(OxyColors).GetField(e.ChangedItem.Value).GetValue(Nothing)
            ElseIf e.ChangedItem.Label.Equals("Marker Stroke Color") Then
                DirectCast(Chart.PlotModel.Series(e.ChangedItem.Tag), LineSeries).MarkerStroke = GetType(OxyColors).GetField(e.ChangedItem.Value).GetValue(Nothing)
            End If
        Catch ex As Exception
            Flowsheet.ShowMessage("Chart property update error: " & ex.Message, Interfaces.IFlowsheet.MessageType.GeneralError)
        End Try

        UpdatePropertyGrid()

    End Sub

    Private Sub BtnExportPNG_Click(sender As Object, e As EventArgs) Handles btnExportPNG.Click

        SavePlot("PNG")

    End Sub

    Private Sub BtnExportSVG_Click(sender As Object, e As EventArgs) Handles btnExportSVG.Click

        SavePlot("SVG")

    End Sub

    Sub SavePlot(extension As String)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType(extension + " File", "*." + extension)})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                If extension = "PNG" Then
                    Dim exporter = New OxyPlot.WindowsForms.PngExporter() With {.Background = OxyPlot.OxyColors.White, .Width = PlotView1.Width, .Height = PlotView1.Height}
                    exporter.Export(Chart.PlotModel, stream)
                    Flowsheet.ShowMessage(String.Format("Chart '{0}' saved to '{1}'.", Chart.DisplayName, handler.FullPath), Interfaces.IFlowsheet.MessageType.Information)
                Else
                    Dim exporter As New OxyPlot.SvgExporter() With {.Width = PlotView1.Width, .Height = PlotView1.Height}
                    exporter.Export(Chart.PlotModel, stream)
                    Flowsheet.ShowMessage(String.Format("Chart '{0}' saved to '{1}'.", Chart.DisplayName, handler.FullPath), Interfaces.IFlowsheet.MessageType.Information)
                End If
                handler.Write(stream)
            End Using
        End If

    End Sub

End Class
