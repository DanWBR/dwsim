Imports DWSIM.UnitOperations.UnitOperations
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Drawing

'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Public Class EditingForm_Column_Results

    Inherits SharedClasses.ObjectEditorForm

    Public dc As Column

    Private form As IFlowsheet

    Dim loaded As Boolean = False

    Dim su As SharedClasses.SystemsOfUnits.Units

    Dim nf As String

    Dim SelTab As Integer

    Private Sub UIResultsForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        form = dc.FlowSheet

        nf = form.FlowsheetOptions.NumberFormat
        su = form.FlowsheetOptions.SelectedUnitSystem

        Me.Text = dc.GraphicObject.Tag & " - " & Me.Text

        loaded = True

        FillTables()
        FillGraphs()

        SelTab = 0

        ChangeDefaultFont()

    End Sub

    Public Sub FillTables()

        Dim ns As Integer = dc.NumberOfStages - 1
        Dim nc As Integer = UBound(dc.x0(0))
        Dim i, j, k As Integer

        Dim T0(ns), Tf(ns), V0(ns), Vf(ns), L0(ns), Lf(ns), LSS0(ns), LSSf(ns), VSS0(ns), VSSf(ns), P0(ns) As Double
        Dim x0(ns)(), xf(ns)(), y0(ns)(), yf(ns)(), K0(ns)(), Kf(ns)() As Double
        Dim cx0(nc)(), cxf(nc)(), cy0(nc)(), cyf(nc)(), cK0(nc)(), cKf(nc)(), cxm(nc)(), cym(nc)() As Double

        For i = 0 To ns
            T0(i) = Format(cv.ConvertFromSI(su.temperature, dc.T0(i)), nf)
            Tf(i) = Format(cv.ConvertFromSI(su.temperature, dc.Tf(i)), nf)
            V0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.V0(i)), nf)
            Vf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.Vf(i)), nf)
            L0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.L0(i)), nf)
            Lf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.Lf(i)), nf)
            VSS0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.VSS0(i)), nf)
            VSSf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.VSSf(i)), nf)
            LSS0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.LSS0(i)), nf)
            LSSf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.LSSf(i)), nf)
            P0(i) = Format(cv.ConvertFromSI(su.pressure, dc.P0(i)), nf)
            x0(i) = dc.x0(i)
            xf(i) = dc.xf(i)
            y0(i) = dc.y0(i)
            yf(i) = dc.yf(i)
            K0(i) = dc.K0(i)
            Kf(i) = dc.Kf(i)
        Next
        For i = 0 To nc
            cx0(i) = dc.T0.Clone
            cxf(i) = dc.T0.Clone
            cy0(i) = dc.T0.Clone
            cyf(i) = dc.T0.Clone
            cxm(i) = dc.T0.Clone
            cym(i) = dc.T0.Clone
            cK0(i) = dc.T0.Clone
            cKf(i) = dc.T0.Clone
        Next
        For i = 0 To ns
            For j = 0 To nc
                cx0(j)(i) = Format(x0(i)(j) * L0(i), nf)
                cxf(j)(i) = Format(xf(i)(j) * Lf(i), nf)
                cy0(j)(i) = Format(y0(i)(j) * V0(i), nf)
                cyf(j)(i) = Format(yf(i)(j) * Vf(i), nf)
                cxm(j)(i) = Format(xf(i)(j), nf)
                cym(j)(i) = Format(yf(i)(j), nf)
                cK0(j)(i) = Format(K0(i)(j), nf)
                cKf(j)(i) = Format(Kf(i)(j), nf)
            Next
        Next

        'Fill tables
        With TableTP
            .Columns.Clear()
            .Columns.Add("0", "T0 (" & su.temperature & ")")
            .Columns.Add("1", "Tf (" & su.temperature & ")")
            .Rows.Clear()
            For i = 0 To ns
                .Rows.Add(New Object() {T0(i), Tf(i)})
                .Rows(.Rows.Count - 1).HeaderCell.Value = dc.Stages(i).Name
            Next
        End With
        With TableVL
            .Columns.Clear()
            .Columns.Add("0", "L0 (" & su.molarflow & ")")
            .Columns.Add("1", "Lf (" & su.molarflow & ")")
            .Columns.Add("2", "V0 (" & su.molarflow & ")")
            .Columns.Add("3", "Vf (" & su.molarflow & ")")
            .Rows.Clear()
            For i = 0 To ns
                .Rows.Add(New Object() {L0(i), Lf(i), V0(i), Vf(i)})
                .Rows(.Rows.Count - 1).HeaderCell.Value = dc.Stages(i).Name
            Next
        End With
        With DataGridView4
            .Columns.Clear()
            For i = 0 To nc
                .Columns.Add("[V]" & i, "[V] " & (dc.compids(i)) & " (" & su.molarfraction & ")")
            Next
            For i = 0 To nc
                .Columns.Add("[L]" & i, "[L] " & (dc.compids(i)) & " (" & su.molarfraction & ")")
            Next
            .Rows.Clear()
            For i = 0 To ns
                Dim obj((nc + 1) * 2 - 1) As Object
                k = 0
                For j = 0 To nc
                    obj(k) = cym(j)(i)
                    k = k + 1
                Next
                For j = 0 To nc
                    obj(k) = cxm(j)(i)
                    k = k + 1
                Next
                .Rows.Add(obj)
                .Rows(.Rows.Count - 1).HeaderCell.Value = dc.Stages(i).Name
            Next
        End With

        With TableCP
            .Columns.Clear()
            For i = 0 To nc
                .Columns.Add("[V]" & i, "[V] " & (dc.compids(i)) & " (" & su.molarflow & ")")
            Next
            For i = 0 To nc
                .Columns.Add("[L]" & i, "[L] " & (dc.compids(i)) & " (" & su.molarflow & ")")
            Next
            For i = 0 To nc
                .Columns.Add("[Kval]" & i, "[Kval] " & (dc.compids(i)))
            Next
            .Rows.Clear()
            For i = 0 To ns
                Dim obj((nc + 1) * 3 - 1) As Object
                k = 0
                For j = 0 To nc
                    obj(k) = cyf(j)(i)
                    k = k + 1
                Next
                For j = 0 To nc
                    obj(k) = cxf(j)(i)
                    k = k + 1
                Next
                For j = 0 To nc
                    obj(k) = cKf(j)(i)
                    k = k + 1
                Next
                .Rows.Add(obj)
                .Rows(.Rows.Count - 1).HeaderCell.Value = dc.Stages(i).Name
            Next
        End With
    End Sub

    Public Sub FillGraphs()



        Dim ns As Integer = dc.NumberOfStages - 1
        Dim nc As Integer = UBound(dc.x0(0))
        Dim i, j As Integer

        Dim T0(ns), Tf(ns), V0(ns), Vf(ns), L0(ns), Lf(ns), LSS0(ns), LSSf(ns), VSS0(ns), VSSf(ns), P0(ns) As Double
        Dim x0(ns)(), xf(ns)(), y0(ns)(), yf(ns)(), K0(ns)(), Kf(ns)(), cx0(nc)(), cxf(nc)(), cy0(nc)(), cyf(nc)(), cK0(nc)(), cKf(nc)(), cxm(nc)(), cym(nc)() As Double

        Dim py(ns) As Double
        For i = 0 To ns
            py(i) = i + 1
        Next
        For i = 0 To ns
            T0(i) = Format(cv.ConvertFromSI(su.temperature, dc.T0(i)), nf)
            Tf(i) = Format(cv.ConvertFromSI(su.temperature, dc.Tf(i)), nf)
            V0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.V0(i)), nf)
            Vf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.Vf(i)), nf)
            L0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.L0(i)), nf)
            Lf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.Lf(i)), nf)
            VSS0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.VSS0(i)), nf)
            VSSf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.VSSf(i)), nf)
            LSS0(i) = Format(cv.ConvertFromSI(su.molarflow, dc.LSS0(i)), nf)
            LSSf(i) = Format(cv.ConvertFromSI(su.molarflow, dc.LSSf(i)), nf)
            P0(i) = Format(cv.ConvertFromSI(su.pressure, dc.P0(i)), nf)
            x0(i) = dc.x0(i)
            xf(i) = dc.xf(i)
            y0(i) = dc.y0(i)
            yf(i) = dc.yf(i)
            K0(i) = dc.K0(i)
            Kf(i) = dc.Kf(i)
        Next
        For i = 0 To nc
            cx0(i) = dc.T0.Clone
            cxf(i) = dc.T0.Clone
            cy0(i) = dc.T0.Clone
            cyf(i) = dc.T0.Clone
            cK0(i) = dc.T0.Clone
            cKf(i) = dc.T0.Clone
            cxm(i) = dc.T0.Clone
            cym(i) = dc.T0.Clone
        Next
        For i = 0 To ns
            For j = 0 To nc
                cx0(j)(i) = Format(x0(i)(j) * L0(i), nf)
                cxf(j)(i) = Format(xf(i)(j) * Lf(i), nf)
                cy0(j)(i) = Format(y0(i)(j) * V0(i), nf)
                cyf(j)(i) = Format(yf(i)(j) * Vf(i), nf)
                cxm(j)(i) = Format(xf(i)(j), nf)
                cym(j)(i) = Format(yf(i)(j), nf)
                cK0(j)(i) = Format(K0(i)(j), nf)
                cKf(j)(i) = Format(Kf(i)(j), nf)
            Next
        Next

        Dim rnd As New Random(231)

        With Me.GraphTP.GraphPane
            .CurveList.Clear()
            With .AddCurve("Tf", Tf, py, Color.White, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .IsX2Axis = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            If CheckBox1.Checked Then
                With .AddCurve("T0", T0, py, Color.White, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .IsX2Axis = False
                    .Line.Style = Drawing2D.DashStyle.Dash
                    .Symbol.Fill.Type = ZedGraph.FillType.None
                End With
            End If
            With .AddCurve("P0", P0, py, Color.White, ZedGraph.SymbolType.Square)
                .Color = Color.Teal
                .Line.IsSmooth = False
                .IsX2Axis = True
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            With .Legend
                .IsVisible = True
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
            End With
            .Title.IsVisible = False
            .XAxis.Title.Text = "T (" & su.temperature & ")"
            .X2Axis.IsVisible = True
            .X2Axis.Title.Text = "P (" & su.pressure & ")"
            .YAxis.Title.Text = form.GetTranslatedString("DCStage")
            .YAxis.Scale.IsReverse = True
            .AxisChange(Me.CreateGraphics)
        End With
        Me.GraphTP.Invalidate()

        With Me.GraphConc.GraphPane
            .CurveList.Clear()
            For i = 0 To nc
                With .AddCurve("[L] " & (dc.compids(i)), cxm(i), py, Color.White, ZedGraph.SymbolType.Circle)
                    .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .IsX2Axis = False
                End With
            Next
            For i = 0 To nc
                With .AddCurve("[V] " & (dc.compids(i)), cym(i), py, Color.White, ZedGraph.SymbolType.Square)
                    .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                    .Line.IsSmooth = False
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .IsX2Axis = False
                End With
            Next
            With .Legend
                .IsVisible = True
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
            End With
            .Title.IsVisible = False
            .XAxis.Title.Text = form.GetTranslatedString("FraoMolar") & " (" & su.molarfraction & ")"
            .YAxis.Title.Text = form.GetTranslatedString("DCStage")
            .YAxis.Scale.IsReverse = True
            .AxisChange(Me.CreateGraphics)
        End With
        Me.GraphConc.Invalidate()

        With Me.GraphVL.GraphPane
            .CurveList.Clear()
            With .AddCurve("Vf", Vf, py, Color.White, ZedGraph.SymbolType.Circle)
                .Color = Color.MediumVioletRed
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            With .AddCurve("Lf", Lf, py, Color.White, ZedGraph.SymbolType.Circle)
                .Color = Color.SteelBlue
                .Line.IsSmooth = False
                .Symbol.Fill.Type = ZedGraph.FillType.Solid
            End With
            If CheckBox2.Checked Then
                With .AddCurve("V0", V0, py, Color.White, ZedGraph.SymbolType.Circle)
                    .Color = Color.MediumVioletRed
                    .Line.IsSmooth = False
                    .Line.Style = Drawing2D.DashStyle.Dash
                    .Symbol.Fill.Type = ZedGraph.FillType.None
                End With
                With .AddCurve("L0", L0, py, Color.White, ZedGraph.SymbolType.Circle)
                    .Color = Color.SteelBlue
                    .Line.IsSmooth = False
                    .Line.Style = Drawing2D.DashStyle.Dash
                    .Symbol.Fill.Type = ZedGraph.FillType.None
                End With
            End If
            With .Legend
                .IsVisible = True
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
            End With
            .Title.IsVisible = False
            .XAxis.Title.Text = form.GetTranslatedString("DCFlows") & " (" & su.molarflow & ")"
            .YAxis.Title.Text = form.GetTranslatedString("DCStage")
            .YAxis.Scale.IsReverse = True
            .AxisChange(Me.CreateGraphics)
        End With
        Me.GraphVL.Invalidate()



        With Me.GraphCP.GraphPane
            .CurveList.Clear()
            If CheckBox3.Checked And CheckBox4.Checked Then
                For i = 0 To nc
                    With .AddCurve("[L] " & (dc.compids(i)), cxf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        .IsX2Axis = False
                    End With
                Next
                For i = 0 To nc
                    With .AddCurve("[V] " & (dc.compids(i)), cyf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        .IsX2Axis = False
                    End With
                Next
                For i = 0 To nc
                    With .AddCurve("[Kval] " & (dc.compids(i)), cKf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                Next
            ElseIf CheckBox3.Checked Then
                For i = 0 To nc
                    With .AddCurve("[L] " & (dc.compids(i)), cxf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        .IsX2Axis = False
                    End With
                Next
                For i = 0 To nc
                    With .AddCurve("[V] " & (dc.compids(i)), cyf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                Next
            ElseIf CheckBox4.Checked Then
                For i = 0 To nc
                    With .AddCurve("[Kval] " & (dc.compids(i)), cKf(i), py, Color.White, ZedGraph.SymbolType.Circle)
                        .Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                        .Line.IsSmooth = False
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                Next
            End If
            With .Legend
                .IsVisible = True
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
            End With
            .Title.IsVisible = False
            .XAxis.Title.Text = form.GetTranslatedString("DCFlows") & " (" & su.molarflow & ")"
            .YAxis.Title.Text = form.GetTranslatedString("DCStage")
            .YAxis.Scale.IsReverse = True
            .YAxis.Title.Text = form.GetTranslatedString("DCStage")
            .AxisChange(Me.CreateGraphics)
            If CheckBox3.Checked And CheckBox4.Checked Then
                .X2Axis.IsVisible = True
                .X2Axis.Title.Text = "K"
                .XAxis.Title.Text = form.GetTranslatedString("DCFlows") & " (" & su.molarflow & ")"
            ElseIf CheckBox3.Checked Then
                .X2Axis.IsVisible = False
                .XAxis.Title.Text = form.GetTranslatedString("DCFlows") & " (" & su.molarflow & ")"
            ElseIf CheckBox4.Checked Then
                .X2Axis.IsVisible = False
                .XAxis.Title.Text = "K"
            End If
        End With
        Me.GraphCP.Invalidate()

    End Sub

    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox1.CheckedChanged, CheckBox2.CheckedChanged, CheckBox3.CheckedChanged, CheckBox4.CheckedChanged
        If loaded Then
            FillGraphs()
        End If
    End Sub

    Private Sub Content_TabStripItemSelectionChanged(e As FarsiLibrary.Win.TabStripItemChangedEventArgs) Handles Content.TabStripItemSelectionChanged
        If e.ChangeType = 3 Then ' -> selection changed
            'save selected tab for printing
            SelTab = e.Item.TabIndex
        End If
    End Sub

    Private Sub TSB_Print_Click(sender As System.Object, e As System.EventArgs) Handles TSB_Print.Click
        Select Case SelTab
            Case 0
                GraphTP.DoPrint()
            Case 1
                GraphVL.DoPrint()
            Case 2
                GraphCP.DoPrint()
            Case 3
                GraphConc.DoPrint()
        End Select
    End Sub

    Private Sub TSB_PrinterSetup_Click(sender As System.Object, e As System.EventArgs) Handles TSB_PrinterSetup.Click
        PrintDialog1.ShowDialog()
    End Sub

    Private Sub TSB_PageSetup_Click(sender As System.Object, e As System.EventArgs) Handles TSB_PageSetup.Click
        Select Case SelTab
            Case 0
                GraphTP.DoPageSetup()
            Case 1
                GraphVL.DoPageSetup()
            Case 2
                GraphCP.DoPageSetup()
            Case 3
                GraphConc.DoPageSetup()
        End Select
    End Sub

    Private Sub TSB_Preview_Click(sender As System.Object, e As System.EventArgs) Handles TSB_Preview.Click
        Select Case SelTab
            Case 0
                GraphTP.DoPrintPreview()
            Case 1
                GraphVL.DoPrintPreview()
            Case 2
                GraphCP.DoPrintPreview()
            Case 3
                GraphConc.DoPrintPreview()
        End Select
    End Sub

    Private Sub TSB_Copy_Click(sender As System.Object, e As System.EventArgs) Handles TSB_Copy.Click
        Select Case SelTab
            Case 0
                GraphTP.Copy(1, True)
            Case 1
                GraphVL.Copy(1, True)
            Case 2
                GraphCP.Copy(1, True)
            Case 3
                GraphConc.Copy(1, True)
        End Select
    End Sub
End Class