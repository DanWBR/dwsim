Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Drawing

Public Class EditingForm_HeatExchanger_ViewProfile

    Public hx As UnitOperations.HeatExchanger

    Dim su As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Dim loaded As Boolean = False

    Private Sub EditingForm_HeatExchanger_ViewProfile_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        nf = hx.FlowSheet.FlowsheetOptions.NumberFormat
        su = hx.FlowSheet.FlowsheetOptions.SelectedUnitSystem

        Me.Text = hx.GraphicObject.Tag & " - " & Me.Text

        loaded = True

        Grid1.Columns(0).HeaderText += " (" & su.heatflow & ")"
        Grid1.Columns(1).HeaderText += " (" & su.temperature & ")"
        Grid1.Columns(2).HeaderText += " (" & su.temperature & ")"

        Grid1.Columns(0).DefaultCellStyle.Format = nf
        Grid1.Columns(1).DefaultCellStyle.Format = nf
        Grid1.Columns(2).DefaultCellStyle.Format = nf


        ChangeDefaultFont()

        FillTables()
        FillGraphs()

    End Sub

    Private Sub FillTables()

        Grid1.Rows.Clear()
        For i = 0 To hx.HeatProfile.Count - 1
            Grid1.Rows.Add(New Object() {cv.ConvertFromSI(su.heatflow, hx.HeatProfile(i)), cv.ConvertFromSI(su.temperature, hx.TemperatureProfileCold(i)), cv.ConvertFromSI(su.temperature, hx.TemperatureProfileHot(i))})
        Next

    End Sub

    Private Sub FillGraphs()

        With Me.GraphControl.GraphPane
            .CurveList.Clear()
            With .AddCurve("Cold Fluid Temperature", cv.ConvertArrayFromSI(su.heatflow, hx.HeatProfile), cv.ConvertArrayFromSI(su.temperature, hx.TemperatureProfileCold), Color.Blue, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            With .AddCurve("Hot Fluid Temperature", cv.ConvertArrayFromSI(su.heatflow, hx.HeatProfile), cv.ConvertArrayFromSI(su.temperature, hx.TemperatureProfileHot), Color.Red, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            Dim maxT = Math.Max(hx.TemperatureProfileHot.Max, hx.TemperatureProfileCold.Max)
            Dim minT = Math.Min(hx.TemperatureProfileHot.Min, hx.TemperatureProfileCold.Min)
            With .AddCurve("Operating Point", cv.ConvertArrayFromSI(su.heatflow, New Double() {hx.Q.GetValueOrDefault(), hx.Q.GetValueOrDefault()}), cv.ConvertArrayFromSI(su.temperature, New Double() {minT, maxT}), Color.Red, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = True
                .Symbol.IsVisible = False
                .Line.IsVisible = True
                .Line.Color = Color.Green
                .Line.Width = 3
            End With
            .XAxis.Title.Text = "Q (" & su.heatflow & ")"
            .YAxis.Title.Text = "T (" & su.temperature & ")"
            .Title.IsVisible = True
            .Title.Text = "Heat Exchange Profile: " + hx.GraphicObject.Tag
            .Legend.IsVisible = True
            .Legend.Border.IsVisible = False
            .Legend.Position = ZedGraph.LegendPos.BottomCenter
            .AxisChange(Me.CreateGraphics)
        End With

        Me.GraphControl.Invalidate()

    End Sub


End Class