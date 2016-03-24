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

Imports System.Linq

Public Class FormPhEnv

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Dim mat As DWSIM.SimulationObjects.Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim cp As Utilities.TCP.Methods

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String

    Private loaded As Boolean = False
    Private calculated As Boolean = False
    Private qualitycalc As Boolean = False
    Private hydratecalc As Boolean = False
    Private phaseidentification As Boolean = False
    Private showoppoint As Boolean = True

    'desmembrar vetores
    Dim PB, PO, TVB, TVD, HB, HO, SB, SO, VB, VO, TE, PE, PHsI, PHsII, THsI, THsII, TQ, PQ, TI, PI, POWF, TOWF, HOWF, SOWF, VOWF As New ArrayList
    Dim UT, UP, UH, US, UV As New ArrayList
    Dim PC As ArrayList
    Dim ot, op, ov, oh, os As Double
    Dim strname As String = ""

    Public bw As System.ComponentModel.BackgroundWorker

    Private Sub FormPhEnv_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float

        Me.ComboBox1.SelectedIndex = 0
        Me.ComboBox2.SelectedIndex = 0

        Me.Frm = My.Application.ActiveSimulation

        Me.cp = New Utilities.TCP.Methods

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox3.Items.Clear()
        For Each mat2 In Me.Frm.Collections.FlowsheetObjectCollection.Values
            If mat2.GraphicObject.Calculated Then Me.ComboBox3.Items.Add(mat2.GraphicObject.Tag.ToString)
        Next

        If Me.ComboBox3.Items.Count > 0 Then Me.ComboBox3.SelectedIndex = 0

        Me.Text = DWSIM.App.GetLocalString("DWSIMUtilitriosDiagr1")

        Me.TabText = Me.Text


        If Not Me.DockHandler Is Nothing OrElse Not Me.DockHandler.FloatPane Is Nothing Then
            ' set the bounds of this form's FloatWindow to our desired position and size
            If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
                Dim floatWin = Me.DockHandler.FloatPane.FloatWindow
                If Not floatWin Is Nothing Then
                    floatWin.SetBounds(floatWin.Location.X, floatWin.Location.Y, 943, 484)
                End If
            End If
        End If

        Try
            Me.chkhyd.Enabled = Frm.Options.SelectedPropertyPackage.RET_VCAS().Contains("7732-18-5")
        Catch ex As Exception

        End Try

        If Frm.Options.SelectedPropertyPackage.ComponentName.Contains("(PR)") Or _
           Frm.Options.SelectedPropertyPackage.ComponentName.Contains("(SRK)") Then
            Me.CheckBox1.Enabled = True
            Me.TextBox1.Enabled = True
            Me.CheckBox3.Enabled = True
            chkpip.Enabled = True
        Else
            Me.CheckBox1.Enabled = False
            Me.TextBox1.Enabled = False
            Me.CheckBox3.Enabled = False
            chkpip.Enabled = False
        End If

        ComboBox2.Enabled = chkhyd.Checked

        Me.loaded = True

        If DWSIM.App.IsRunningOnMono Then GroupBox2.Width -= 80

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.ComboBox3.SelectedItem Is Nothing Then

            Dim x As Double

            If Me.CheckBox1.Checked Then
                If Double.TryParse(TextBox1.Text, x) Then
                    If x >= 0 And x <= 1 Then
                        GoTo exec
                    Else
                        MessageBox.Show(DWSIM.App.GetLocalString("Ovalorinformadoparaa"), DWSIM.App.GetLocalString("Parmetroinvlido"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                Else
                End If
            End If

exec:       With Me.GraphControl.GraphPane.Legend
                .Position = ZedGraph.LegendPos.TopCenter
                .Border.IsVisible = False
                .FontSpec.Size = 10
                .FontSpec.IsDropShadow = False
            End With
            If Me.CheckBox1.Enabled Then Me.qualitycalc = Me.CheckBox1.Checked Else Me.qualitycalc = False
            If Me.CheckBox2.Checked Then Me.showoppoint = True Else Me.showoppoint = False
            Me.phaseidentification = chkpip.Checked
            Me.hydratecalc = chkhyd.Checked
            Me.Button1.Enabled = False

            If My.Settings.EnableGPUProcessing Then
                Calculator.InitComputeDevice()
                Calculator.gpu.EnableMultithreading()
            End If

            Me.BackgroundWorker1.RunWorkerAsync(New Object() {0, Me.TextBox1.Text, Me.CheckBox1.Checked, Me.CheckBox3.Checked, Me.chkpip.Checked, Me.chkhyd.Checked, Me.CheckBoxHYDVAP.Checked})

            Me.bw = Me.BackgroundWorker1

            Me.PanelCalc.Visible = True
            Me.PanelCalc.Enabled = True

        End If

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged

        If Me.loaded And Me.calculated Then

            Select Case ComboBox1.SelectedIndex

                Case 0

                    Dim px1, py1, px2, py2, px3, py3, px4, py4, ph1, ph2, th1, th2, px5, py5, px6, py6 As New ArrayList

                    Dim i As Integer

                    For i = 0 To TVB.Count - 1
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                    Next
                    For i = 0 To TE.Count - 1
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TE(i)))
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PE(i)))
                    Next
                    For i = 0 To TQ.Count - 1
                        py4.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TQ(i)))
                        px4.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PQ(i)))
                    Next
                    For i = 0 To TI.Count - 1
                        py5.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TI(i)))
                        px5.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, THsI(i)))
                        ph1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, THsII(i)))
                        ph2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsII(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        py6.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TOWF(i)))
                        px6.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.pressure, tmp(1))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, tmp(0))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px6.ToArray(GetType(Double)), py6.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        If CheckBox3.Checked Then
                            With .AddCurve(DWSIM.App.GetLocalString("LimitedeEstabilidade"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkOrange
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If qualitycalc Then
                            With .AddCurve("V = " & Me.TextBox1.Text, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkGreen
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If phaseidentification Then
                            With .AddCurve(DWSIM.App.GetLocalString("PhaseIdentificationParameter"), px5.ToArray(GetType(Double)), py5.ToArray(GetType(Double)), Color.Brown, ZedGraph.SymbolType.Circle)
                                .Color = Color.Brown
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                        End If
                        If hydratecalc Then
                            With .AddCurve(DWSIM.App.GetLocalString("HydrateEquilibriumCurve") & " (sI)", ph1.ToArray(GetType(Double)), th1.ToArray(GetType(Double)), Color.LightCoral, ZedGraph.SymbolType.Circle)
                                .Color = Color.LightCoral
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                            With .AddCurve(DWSIM.App.GetLocalString("HydrateEquilibriumCurve") & " (sII)", ph2.ToArray(GetType(Double)), th2.ToArray(GetType(Double)), Color.Violet, ZedGraph.SymbolType.Circle)
                                .Color = Color.Violet
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                        End If
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {op}, New Double() {ot}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        .Title.Text = strname
                        .XAxis.Title.Text = "P / " & su.pressure
                        .YAxis.Title.Text = "T / " & su.temperature
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.IsAutoScrollRange = True
                        Me.GraphControl.Invalidate()
                    End With

                Case 1

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To PB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        '.AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TC)}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PC)}, Color.Black, ZedGraph.SymbolType.Circle)
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "P / " & su.pressure
                        .YAxis.Title.Text = "H / " & su.enthalpy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {op}, New Double() {oh}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 2

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To PB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SOWF(i)))
                    Next
                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "P / " & su.pressure
                        .YAxis.Title.Text = "S / " & su.entropy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {op}, New Double() {os}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 3

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To PB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, VOWF(i)))
                    Next
                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.pressure, tmp(1))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, tmp(2))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "P / " & su.pressure
                        .YAxis.Title.Text = "V / " & su.molar_volume
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {op}, New Double() {ov}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 4

                    Dim px1, py1, px2, py2, px3, py3, px4, py4, ph1, ph2, th1, th2, px5, py5, px6, py6 As New ArrayList

                    Dim i As Integer

                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                    Next
                    For i = 0 To TE.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TE(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PE(i)))
                    Next
                    For i = 0 To TQ.Count - 1
                        px4.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TQ(i)))
                        py4.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PQ(i)))
                    Next
                    For i = 0 To TI.Count - 1
                        px5.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TI(i)))
                        py5.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, THsI(i)))
                        ph1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, THsII(i)))
                        ph2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsII(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px6.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TOWF(i)))
                        py6.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, tmp(0))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.pressure, tmp(1))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px6.ToArray(GetType(Double)), py6.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        If CheckBox3.Checked Then
                            With .AddCurve(DWSIM.App.GetLocalString("LimitedeEstabilidade"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkOrange
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If qualitycalc Then
                            With .AddCurve("V = " & Me.TextBox1.Text, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkGreen
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If phaseidentification Then
                            With .AddCurve(DWSIM.App.GetLocalString("PhaseIdentificationParameter"), px5.ToArray(GetType(Double)), py5.ToArray(GetType(Double)), Color.Brown, ZedGraph.SymbolType.Circle)
                                .Color = Color.Brown
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                        End If
                        If hydratecalc Then
                            With .AddCurve(DWSIM.App.GetLocalString("HydrateEquilibriumCurve") & " (sI)", th1.ToArray(GetType(Double)), ph1.ToArray(GetType(Double)), Color.LightCoral, ZedGraph.SymbolType.Circle)
                                .Color = Color.LightCoral
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                            With .AddCurve(DWSIM.App.GetLocalString("HydrateEquilibriumCurve") & " (sII)", th2.ToArray(GetType(Double)), ph2.ToArray(GetType(Double)), Color.Violet, ZedGraph.SymbolType.Circle)
                                .Color = Color.Violet
                                .Line.IsSmooth = True
                                .Line.IsVisible = True
                                .Line.Width = 2
                                .Symbol.IsVisible = False
                            End With
                        End If
                        .Title.Text = strname
                        .XAxis.Title.Text = "T / " & su.temperature
                        .YAxis.Title.Text = "P / " & su.pressure
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ot}, New Double() {op}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 5

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, TOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, HOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "T / " & su.temperature
                        .YAxis.Title.Text = "H / " & su.enthalpy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ot}, New Double() {oh}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 6

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, TOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "T / " & su.temperature
                        .YAxis.Title.Text = "S / " & su.entropy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ot}, New Double() {os}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 7

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                    Next
                    For i = 0 To TVB.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, TOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, VOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, tmp(0))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, tmp(2))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "T / " & su.temperature
                        .YAxis.Title.Text = "V / " & su.molar_volume
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ot}, New Double() {ov}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 8

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To PB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, tmp(2))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.pressure, tmp(1))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "V / " & su.molar_volume
                        .YAxis.Title.Text = "P / " & su.pressure
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ov}, New Double() {op}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 9

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVD(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, TOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, tmp(2))}, New Double() {SystemsOfUnits.Converter.ConvertFromSI(su.temperature, tmp(0))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
                        Next
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = ""
                        .XAxis.Title.Text = "V / " & su.molar_volume
                        .YAxis.Title.Text = "T / " & su.temperature
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ov}, New Double() {ot}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 10

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, HOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = ""
                        .XAxis.Title.Text = "V / " & su.molar_volume
                        .YAxis.Title.Text = "H / " & su.enthalpy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ov}, New Double() {oh}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

                Case 11

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To TVB.Count - 1
                        px1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VB(i)))
                        py1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VO(i)))
                        py2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.molar_volume, VOWF(i)))
                        py3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha"), px1.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.SteelBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalho"), px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.DeepSkyBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.YellowGreen
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("PontosdeOrvalhoWF"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.DarkBlue, ZedGraph.SymbolType.Circle)
                            .Color = Color.DarkBlue
                            .Line.IsSmooth = False
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                        End With
                        .Title.Text = strname
                        .XAxis.Title.Text = "V / " & su.molar_volume
                        .YAxis.Title.Text = "S / " & su.entropy
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.Invalidate()
                        If Me.showoppoint Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontodeOperao"), New Double() {ov}, New Double() {os}, Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.Black
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                    End With

            End Select

            With Me.GraphControl.GraphPane.Title
                .Text = Me.ComboBox1.SelectedItem & " (PP: " & Me.Frm.Options.SelectedPropertyPackage.ComponentName & ")"
                .FontSpec.Size = 14
            End With

        End If

    End Sub

    Private Sub FormPhEnv_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        Me.loaded = True

        Frm.WriteToLog(DWSIM.App.GetLocalTipString("PENV001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

    End Sub

    Private Sub BackgroundWorker1_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        Dim gobj As GraphicObjects.GraphicObject = Nothing
        gobj = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetDesignSurface)
        Me.mat = Frm.Collections.FlowsheetObjectCollection(gobj.Name)
        Me.strname = gobj.Tag

        If Me.showoppoint Then
            ot = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, mat.Phases(0).Properties.temperature.GetValueOrDefault)
            op = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, mat.Phases(0).Properties.pressure.GetValueOrDefault)
            ov = mat.Phases(0).Properties.molecularWeight.GetValueOrDefault / mat.Phases(0).Properties.density.GetValueOrDefault / 1000
            oh = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, mat.Phases(0).Properties.enthalpy.GetValueOrDefault)
            os = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, mat.Phases(0).Properties.entropy.GetValueOrDefault)
        End If

        Dim pp As PropertyPackages.PropertyPackage = Frm.Options.SelectedPropertyPackage

        pp.CurrentMaterialStream = mat

        Dim diagdata As Object = pp.DW_ReturnPhaseEnvelope(e.Argument, Me.BackgroundWorker1)

        PC = diagdata(15)

        Dim th1, th2, ph1, ph2 As New ArrayList, Ph, Th As Object, Pmin, Pmax As Double, Vz() As Double, Vn() As String

        If e.Argument(5) = True Then

            Me.BackgroundWorker1.ReportProgress(99, "Hydrate Equilibrium Curves")

            Pmin = 101325
            Pmax = PC(0)(1) * 1.3
            Dim i As Integer = 0
            Vz = pp.RET_VMOL(PropertyPackages.Phase.Mixture)
            Vn = pp.RET_VNAMES

            Dim m_aux As New DWSIM.Utilities.HYD.AuxMethods

            For Ph = Pmin To Pmax Step 5 * 101325
                Try
                    Select Case Me.ComboBox2.SelectedIndex
                        Case 0
                            Th = New DWSIM.Utilities.HYD.vdwP_PP(mat).HYD_vdwP2T(Ph, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(Vn), e.Argument(6))
                        Case 1
                            Th = New DWSIM.Utilities.HYD.KlaudaSandler(mat).HYD_KS2T(Ph, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(Vn), e.Argument(6))
                        Case 2
                            Th = New DWSIM.Utilities.HYD.ChenGuo(mat).HYD_CG2T(Ph, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(Vn), e.Argument(6))
                        Case Else
                            Th = New DWSIM.Utilities.HYD.KlaudaSandlerMOD(mat).HYD_KS2T(Ph, Vz, m_aux.RetornarIDsParaCalculoDeHidratos(Vn), e.Argument(6))
                    End Select
                    ph1.Add(Ph)
                    ph2.Add(Ph)
                    th1.Add(Th(0))
                    th2.Add(Th(1))
                Catch ex As Exception
                End Try
            Next

            If th1.Count = 0 Then
                th1.Add(0.0#)
                ph1.Add(0.0#)
                th2.Add(0.0#)
                ph2.Add(0.0#)
            End If

        Else

            th1.Add(0.0#)
            ph1.Add(0.0#)
            th2.Add(0.0#)
            ph2.Add(0.0#)

        End If

        e.Result = New Object() {diagdata, ph1, th1, ph2, th2}

    End Sub

    Private Sub BackgroundWorker1_ProgressChanged(sender As Object, e As System.ComponentModel.ProgressChangedEventArgs) Handles BackgroundWorker1.ProgressChanged
        Me.LabelStatus.Text = e.UserState.ToString
    End Sub

    Private Sub BackgroundWorker1_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorker1.RunWorkerCompleted

        If My.Settings.EnableGPUProcessing Then
            Calculator.gpu.DisableMultithreading()
            Calculator.gpu.FreeAll()
        End If

        Me.Button1.Enabled = True

        Me.PanelCalc.Visible = False

        Dim r = e.Result(0)

        '{TVB, PB, HB, SB, VB, TVD, PO, HO, SO, VO, TE, PE, TH, PHsI, PHsII, TC, PC, VC}
        TVB = r(0)
        PB = r(1)
        HB = r(2)
        SB = r(3)
        VB = r(4)
        TVD = r(5)
        PO = r(6)
        HO = r(7)
        SO = r(8)
        VO = r(9)
        TE = r(10)
        PE = r(11)
        PHsI = e.Result(1)
        THsI = e.Result(2)
        PHsII = e.Result(3)
        THsII = e.Result(4)
        PC = r(15)
        TQ = r(16)
        PQ = r(17)
        TI = r(18)
        PI = r(19)
        TOWF = r(20)
        POWF = r(21)
        HOWF = r(22)
        SOWF = r(23)
        VOWF = r(24)

        calculated = True

        With Me.Grid1.Columns
            .Clear()
            .Add("c1", "Tbol (" & su.temperature & ")")
            .Add("c2", DWSIM.App.GetLocalString("Pbol") & su.pressure & ")")
            .Add("c3", "Hbol (" & su.enthalpy & ")")
            .Add("c4", DWSIM.App.GetLocalString("Sbol") & su.entropy & ")")
            .Add("c5", DWSIM.App.GetLocalString("Vbolm3mol"))
            .Add("c6", "Torv (" & su.temperature & ")")
            .Add("c7", DWSIM.App.GetLocalString("Porv") & su.pressure & ")")
            .Add("c8", "Horv (" & su.enthalpy & ")")
            .Add("c9", "Sorv (" & su.entropy & ")")
            .Add("c10", DWSIM.App.GetLocalString("Vorvm3mol"))
            .Add("c11", "Test (" & su.temperature & ")")
            .Add("c12", DWSIM.App.GetLocalString("Pest") & su.pressure & ")")
            .Add("c13", "TQ (" & su.temperature & ")")
            .Add("c14", "PQ (" & su.pressure & ")")
            .Add("c15", "TPIP (" & su.temperature & ")")
            .Add("c16", "PPIP (" & su.pressure & ")")
            .Add("c17", "THsI (" & su.temperature & ")")
            .Add("c18", "PHsI (" & su.pressure & ")")
            .Add("c19", "THsII (" & su.temperature & ")")
            .Add("c20", "PHsII (" & su.pressure & ")")
            .Add("c21", "TDWF (" & su.temperature & ")")
            .Add("c22", "PDWF (" & su.pressure & ")")
            .Add("c23", "HDWF (" & su.enthalpy & ")")
            .Add("c24", "SDWF (" & su.entropy & ")")
            .Add("c25", "VDWF (m3/mol)")
        End With

        For Each c As DataGridViewColumn In Me.Grid1.Columns
            c.SortMode = DataGridViewColumnSortMode.NotSortable
            c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            c.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        Next

        Dim maxl As Integer = MathEx.Common.Max(New Object() {TVB.Count, TVD.Count, TE.Count, TQ.Count, TI.Count, THsI.Count, TOWF.Count}) - 1

        Dim k, j As Integer
        Dim maxc As Integer = Me.Grid1.Columns.Count - 1
        Dim data(maxc, maxl) As String

        j = 0
        For Each d As Double In TVB
            data(0, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(1, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB(j))
            data(2, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB(j))
            data(3, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB(j))
            data(4, j) = VB(j)
            j = j + 1
        Next
        j = 0
        For Each d As Double In TVD
            data(5, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(6, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PO(j))
            data(7, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HO(j))
            data(8, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SO(j))
            data(9, j) = VO(j)
            j = j + 1
        Next
        j = 0
        For Each d As Double In TE
            data(10, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(11, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PE(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TQ
            data(12, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(13, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PQ(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TI
            data(14, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(15, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PI(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In THsI
            data(16, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(17, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsI(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In THsII
            data(18, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(19, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PHsII(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TOWF
            data(20, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(21, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, POWF(j))
            data(22, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HOWF(j))
            data(23, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SOWF(j))
            data(24, j) = VOWF(j)
            j = j + 1
        Next

        With Me.Grid1.Rows
            .Clear()
            k = 0
            Do
                .Add()
                j = 0
                Do
                    If Double.TryParse(data(j, k), New Double) Then
                        .Item(k).Cells(j).Value = Format(Convert.ToDouble(data(j, k)), nf)
                    Else
                        .Item(k).Cells(j).Value = data(j, k)
                    End If
                    j = j + 1
                Loop Until j = maxc + 1
                k = k + 1
            Loop Until k = maxl + 1
        End With

        Call Me.ComboBox1_SelectedIndexChanged(sender, e)

    End Sub

    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox1.CheckedChanged
        If Me.CheckBox1.Checked Then Me.TextBox1.Enabled = True Else Me.TextBox1.Enabled = False
    End Sub

    Private Sub CheckBox2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CheckBox2.CheckedChanged
        If Me.CheckBox2.Checked Then Me.showoppoint = True Else Me.showoppoint = False
    End Sub

    Private Sub FormPhEnv_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_PhaseEnvelope.htm")
    End Sub

    Private Sub chkhyd_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkhyd.CheckedChanged
        ComboBox2.Enabled = chkhyd.Checked
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBox2.SelectedIndexChanged
        If ComboBox2.SelectedIndex = 2 Then CheckBoxHYDVAP.Enabled = False Else CheckBoxHYDVAP.Enabled = True
    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                 DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                 DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                 DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                 DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        My.Application.CalculatorStopRequested = True
        If Not bw Is Nothing Then bw.CancelAsync()
    End Sub
End Class