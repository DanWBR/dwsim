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

    Dim cp As DWSIM.Utilities.TCP.Methods

    Public su As New DWSIM.SistemasDeUnidades.Unidades
    Public cv As New DWSIM.SistemasDeUnidades.Conversor
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

        Me.cp = New DWSIM.Utilities.TCP.Methods

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Me.ComboBox3.Items.Clear()
        For Each mat2 In Me.Frm.Collections.CLCS_MaterialStreamCollection.Values
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
                DWSIM.App.InitComputeDevice()
                My.Application.gpu.EnableMultithreading()
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
                        py1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                        px1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        py2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                        px2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                    Next
                    For i = 0 To TE.Count - 1
                        py3.Add(Conversor.ConverterDoSI(su.spmp_temperature, TE(i)))
                        px3.Add(Conversor.ConverterDoSI(su.spmp_pressure, PE(i)))
                    Next
                    For i = 0 To TQ.Count - 1
                        py4.Add(Conversor.ConverterDoSI(su.spmp_temperature, TQ(i)))
                        px4.Add(Conversor.ConverterDoSI(su.spmp_pressure, PQ(i)))
                    Next
                    For i = 0 To TI.Count - 1
                        py5.Add(Conversor.ConverterDoSI(su.spmp_temperature, TI(i)))
                        px5.Add(Conversor.ConverterDoSI(su.spmp_pressure, PI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th1.Add(Conversor.ConverterDoSI(su.spmp_temperature, THsI(i)))
                        ph1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PHsI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th2.Add(Conversor.ConverterDoSI(su.spmp_temperature, THsII(i)))
                        ph2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PHsII(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        py6.Add(Conversor.ConverterDoSI(su.spmp_temperature, TOWF(i)))
                        px6.Add(Conversor.ConverterDoSI(su.spmp_pressure, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.spmp_pressure, tmp(1))}, New Double() {Conversor.ConverterDoSI(su.spmp_temperature, tmp(0))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .XAxis.Title.Text = "P / " & su.spmp_pressure
                        .YAxis.Title.Text = "T / " & su.spmp_temperature
                        .AxisChange(Me.CreateGraphics)
                        Me.GraphControl.IsAutoScrollRange = True
                        Me.GraphControl.Invalidate()
                    End With

                Case 1

                    Dim px1, py1, px2, py2, px3, py3 As New ArrayList
                    Dim i As Integer
                    For i = 0 To PB.Count - 1
                        px1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.spmp_pressure, POWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        '.AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.spmp_temperature, TC)}, New Double() {Conversor.ConverterDoSI(su.spmp_pressure, PC)}, Color.Black, ZedGraph.SymbolType.Circle)
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
                        .XAxis.Title.Text = "P / " & su.spmp_pressure
                        .YAxis.Title.Text = "H / " & su.spmp_enthalpy
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_entropy, SB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_entropy, SO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.spmp_pressure, POWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, SOWF(i)))
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
                        .XAxis.Title.Text = "P / " & su.spmp_pressure
                        .YAxis.Title.Text = "S / " & su.spmp_entropy
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                    Next
                    For i = 0 To POWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.spmp_pressure, POWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, VOWF(i)))
                    Next
                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.spmp_pressure, tmp(1))}, New Double() {Conversor.ConverterDoSI(su.molar_volume, tmp(2))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .XAxis.Title.Text = "P / " & su.spmp_pressure
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                    Next
                    For i = 0 To TE.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.spmp_temperature, TE(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_pressure, PE(i)))
                    Next
                    For i = 0 To TQ.Count - 1
                        px4.Add(Conversor.ConverterDoSI(su.spmp_temperature, TQ(i)))
                        py4.Add(Conversor.ConverterDoSI(su.spmp_pressure, PQ(i)))
                    Next
                    For i = 0 To TI.Count - 1
                        px5.Add(Conversor.ConverterDoSI(su.spmp_temperature, TI(i)))
                        py5.Add(Conversor.ConverterDoSI(su.spmp_pressure, PI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th1.Add(Conversor.ConverterDoSI(su.spmp_temperature, THsI(i)))
                        ph1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PHsI(i)))
                    Next
                    For i = 0 To THsI.Count - 1
                        th2.Add(Conversor.ConverterDoSI(su.spmp_temperature, THsII(i)))
                        ph2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PHsII(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px6.Add(Conversor.ConverterDoSI(su.spmp_temperature, TOWF(i)))
                        py6.Add(Conversor.ConverterDoSI(su.spmp_pressure, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.spmp_temperature, tmp(0))}, New Double() {Conversor.ConverterDoSI(su.spmp_pressure, tmp(1))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .XAxis.Title.Text = "T / " & su.spmp_temperature
                        .YAxis.Title.Text = "P / " & su.spmp_pressure
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, TOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, HOWF(i)))
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
                        .XAxis.Title.Text = "T / " & su.spmp_temperature
                        .YAxis.Title.Text = "H / " & su.spmp_enthalpy
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_entropy, SB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_entropy, SO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, TOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, SOWF(i)))
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
                        .XAxis.Title.Text = "T / " & su.spmp_temperature
                        .YAxis.Title.Text = "S / " & su.spmp_entropy
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
                        px1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                    Next
                    For i = 0 To TVB.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                        py2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, TOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, VOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.spmp_temperature, tmp(0))}, New Double() {Conversor.ConverterDoSI(su.molar_volume, tmp(2))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .XAxis.Title.Text = "T / " & su.spmp_temperature
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
                        px1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_pressure, PB(i)))
                    Next
                    For i = 0 To PO.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_pressure, PO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, VOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, POWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.molar_volume, tmp(2))}, New Double() {Conversor.ConverterDoSI(su.spmp_pressure, tmp(1))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .YAxis.Title.Text = "P / " & su.spmp_pressure
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
                        px1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_temperature, TVD(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, VOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, TOWF(i)))
                    Next

                    With Me.GraphControl.GraphPane
                        .CurveList.Clear()
                        Dim tmp As Object
                        For Each tmp In PC
                            .AddCurve(DWSIM.App.GetLocalString("PontoCrtico"), New Double() {Conversor.ConverterDoSI(su.molar_volume, tmp(2))}, New Double() {Conversor.ConverterDoSI(su.spmp_temperature, tmp(0))}, Color.Red, ZedGraph.SymbolType.Circle).Symbol.Fill.Type = ZedGraph.FillType.Solid
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
                        .YAxis.Title.Text = "T / " & su.spmp_temperature
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
                        px1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_enthalpy, HO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, VOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, HOWF(i)))
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
                        .YAxis.Title.Text = "H / " & su.spmp_enthalpy
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
                        px1.Add(Conversor.ConverterDoSI(su.molar_volume, VB(i)))
                        py1.Add(Conversor.ConverterDoSI(su.spmp_entropy, SB(i)))
                    Next
                    For i = 0 To TVD.Count - 1
                        px2.Add(Conversor.ConverterDoSI(su.molar_volume, VO(i)))
                        py2.Add(Conversor.ConverterDoSI(su.spmp_entropy, SO(i)))
                    Next
                    For i = 0 To TOWF.Count - 1
                        px3.Add(Conversor.ConverterDoSI(su.molar_volume, VOWF(i)))
                        py3.Add(Conversor.ConverterDoSI(su.spmp_entropy, SOWF(i)))
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
                        .YAxis.Title.Text = "S / " & su.spmp_entropy
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

        Frm.WriteToLog(DWSIM.App.GetLocalTipString("PENV001"), Color.Black, DWSIM.FormClasses.TipoAviso.Dica)

    End Sub

    Private Sub BackgroundWorker1_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        Dim gobj As Microsoft.Msdn.Samples.GraphicObjects.GraphicObject = Nothing
        gobj = FormFlowsheet.SearchSurfaceObjectsByTag(Me.ComboBox3.SelectedItem, Frm.FormSurface.FlowsheetDesignSurface)
        Me.mat = Frm.Collections.CLCS_MaterialStreamCollection(gobj.Name)
        Me.strname = gobj.Tag

        If Me.showoppoint Then
            ot = Conversor.ConverterDoSI(su.spmp_temperature, mat.Fases(0).SPMProperties.temperature.GetValueOrDefault)
            op = Conversor.ConverterDoSI(su.spmp_pressure, mat.Fases(0).SPMProperties.pressure.GetValueOrDefault)
            ov = mat.Fases(0).SPMProperties.molecularWeight.GetValueOrDefault / mat.Fases(0).SPMProperties.density.GetValueOrDefault / 1000
            oh = Conversor.ConverterDoSI(su.spmp_enthalpy, mat.Fases(0).SPMProperties.enthalpy.GetValueOrDefault)
            os = Conversor.ConverterDoSI(su.spmp_entropy, mat.Fases(0).SPMProperties.entropy.GetValueOrDefault)
        End If

        Dim pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage = Frm.Options.SelectedPropertyPackage

        pp.CurrentMaterialStream = mat

        Dim diagdata As Object = pp.DW_ReturnPhaseEnvelope(e.Argument, Me.BackgroundWorker1)

        PC = diagdata(15)

        Dim th1, th2, ph1, ph2 As New ArrayList, Ph, Th As Object, Pmin, Pmax As Double, Vz() As Double, Vn() As String

        If e.Argument(5) = True Then

            Me.BackgroundWorker1.ReportProgress(99, "Hydrate Equilibrium Curves")

            Pmin = 101325
            Pmax = PC(0)(1) * 1.3
            Dim i As Integer = 0
            Vz = pp.RET_VMOL(DWSIM.SimulationObjects.PropertyPackages.Fase.Mixture)
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
            My.Application.gpu.DisableMultithreading()
            My.Application.gpu.FreeAll()
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
            .Add("c1", "Tbol (" & su.spmp_temperature & ")")
            .Add("c2", DWSIM.App.GetLocalString("Pbol") & su.spmp_pressure & ")")
            .Add("c3", "Hbol (" & su.spmp_enthalpy & ")")
            .Add("c4", DWSIM.App.GetLocalString("Sbol") & su.spmp_entropy & ")")
            .Add("c5", DWSIM.App.GetLocalString("Vbolm3mol"))
            .Add("c6", "Torv (" & su.spmp_temperature & ")")
            .Add("c7", DWSIM.App.GetLocalString("Porv") & su.spmp_pressure & ")")
            .Add("c8", "Horv (" & su.spmp_enthalpy & ")")
            .Add("c9", "Sorv (" & su.spmp_entropy & ")")
            .Add("c10", DWSIM.App.GetLocalString("Vorvm3mol"))
            .Add("c11", "Test (" & su.spmp_temperature & ")")
            .Add("c12", DWSIM.App.GetLocalString("Pest") & su.spmp_pressure & ")")
            .Add("c13", "TQ (" & su.spmp_temperature & ")")
            .Add("c14", "PQ (" & su.spmp_pressure & ")")
            .Add("c15", "TPIP (" & su.spmp_temperature & ")")
            .Add("c16", "PPIP (" & su.spmp_pressure & ")")
            .Add("c17", "THsI (" & su.spmp_temperature & ")")
            .Add("c18", "PHsI (" & su.spmp_pressure & ")")
            .Add("c19", "THsII (" & su.spmp_temperature & ")")
            .Add("c20", "PHsII (" & su.spmp_pressure & ")")
            .Add("c21", "TDWF (" & su.spmp_temperature & ")")
            .Add("c22", "PDWF (" & su.spmp_pressure & ")")
            .Add("c23", "HDWF (" & su.spmp_enthalpy & ")")
            .Add("c24", "SDWF (" & su.spmp_entropy & ")")
            .Add("c25", "VDWF (m3/mol)")
        End With

        For Each c As DataGridViewColumn In Me.Grid1.Columns
            c.SortMode = DataGridViewColumnSortMode.NotSortable
            c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            c.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        Next

        Dim maxl As Integer = DWSIM.MathEx.Common.Max(New Object() {TVB.Count, TVD.Count, TE.Count, TQ.Count, TI.Count, THsI.Count, TOWF.Count}) - 1

        Dim k, j As Integer
        Dim maxc As Integer = Me.Grid1.Columns.Count - 1
        Dim data(maxc, maxl) As String

        j = 0
        For Each d As Double In TVB
            data(0, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(1, j) = Conversor.ConverterDoSI(su.spmp_pressure, PB(j))
            data(2, j) = Conversor.ConverterDoSI(su.spmp_enthalpy, HB(j))
            data(3, j) = Conversor.ConverterDoSI(su.spmp_entropy, SB(j))
            data(4, j) = VB(j)
            j = j + 1
        Next
        j = 0
        For Each d As Double In TVD
            data(5, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(6, j) = Conversor.ConverterDoSI(su.spmp_pressure, PO(j))
            data(7, j) = Conversor.ConverterDoSI(su.spmp_enthalpy, HO(j))
            data(8, j) = Conversor.ConverterDoSI(su.spmp_entropy, SO(j))
            data(9, j) = VO(j)
            j = j + 1
        Next
        j = 0
        For Each d As Double In TE
            data(10, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(11, j) = Conversor.ConverterDoSI(su.spmp_pressure, PE(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TQ
            data(12, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(13, j) = Conversor.ConverterDoSI(su.spmp_pressure, PQ(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TI
            data(14, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(15, j) = Conversor.ConverterDoSI(su.spmp_pressure, PI(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In THsI
            data(16, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(17, j) = Conversor.ConverterDoSI(su.spmp_pressure, PHsI(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In THsII
            data(18, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(19, j) = Conversor.ConverterDoSI(su.spmp_pressure, PHsII(j))
            j = j + 1
        Next
        j = 0
        For Each d As Double In TOWF
            data(20, j) = Conversor.ConverterDoSI(su.spmp_temperature, d)
            data(21, j) = Conversor.ConverterDoSI(su.spmp_pressure, POWF(j))
            data(22, j) = Conversor.ConverterDoSI(su.spmp_enthalpy, HOWF(j))
            data(23, j) = Conversor.ConverterDoSI(su.spmp_entropy, SOWF(j))
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
                        .Item(k).Cells(j).Value = Format(CDbl(data(j, k)), nf)
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