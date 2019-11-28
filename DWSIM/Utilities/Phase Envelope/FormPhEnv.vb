'    Copyright 2008-2016 Daniel Wagner O. de Medeiros
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
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class FormPhEnv

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Dim mat As Streams.MaterialStream
    Dim Frm As FormFlowsheet

    Dim cp As Utilities.TCP.Methods

    Public su As New SystemsOfUnits.Units
    Public nf As String

    Private loaded As Boolean = False
    Private calculated As Boolean = False
    Private qualitycalc As Boolean = False
    Private hydratecalc As Boolean = False
    Private phaseidentification As Boolean = False
    Private showoppoint As Boolean = True

    Dim PB, PO, TVB, TVD, HB, HO, SB, SO, VB, VO, TE, PE,
        PB1, TVB1, HB1, SB1, VB1, PB2, TVB2, HB2, SB2, VB2,
        PHsI, PHsII, THsI, THsII, TQ, PQ, TI, PI, POWF, TOWF, HOWF, SOWF, VOWF As New ArrayList

    Dim UT, UP, UH, US, UV As New ArrayList

    Dim PC As ArrayList

    Dim ot, op, ov, oh, os As Double

    Dim strname As String = ""

    Public Property Cricondentherm As Double
    Public Property Cricondenbar As Double
    Public Property CriticalPressure As Double
    Public Property CriticalTemperature As Double
    Public Property CriticalVolume As Double

    Public Property EnvelopeSettings As New PropertyPackages.PhaseEnvelopeOptions

    Public bw As System.ComponentModel.BackgroundWorker

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

        Me.ComboBox1.SelectedIndex = 0
        Me.cbhydmodel.SelectedIndex = 0

        Me.Frm = My.Application.ActiveSimulation

        Me.cp = New Utilities.TCP.Methods

        Populate()

        Me.loaded = True

        If DWSIM.App.IsRunningOnMono Then GroupBox2.Width -= 80

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

        Me.su = Frm.Options.SelectedUnitSystem
        Me.nf = Frm.Options.NumberFormat

        Try
            Me.chkhyd.Enabled = Frm.SelectedCompounds.Values.Select(Function(x) x.CAS_Number).Contains("7732-18-5")
        Catch ex As Exception

        End Try

        If TypeOf DirectCast(AttachedTo, Streams.MaterialStream).PropertyPackage Is PropertyPackages.PengRobinsonPropertyPackage Or _
           TypeOf DirectCast(AttachedTo, Streams.MaterialStream).PropertyPackage Is PropertyPackages.SRKPropertyPackage Then
            Me.chkStabCurve.Enabled = True
            chkpip.Enabled = True
        Else
            Me.chkStabCurve.Enabled = False
            chkpip.Enabled = False
        End If

        cbhydmodel.Enabled = chkhyd.Checked

        With EnvelopeSettings

            chkQualityLine.Checked = .QualityLine
            tbQuality.TextAlign = .QualityValue
            chkhyd.Checked = .Hydrate
            chkHydVapOnly.Checked = .HydrateVaporOnly
            chkStabCurve.Checked = .StabilityCurve
            chkOp.Checked = .OperatingPoint
            chkpip.Checked = .PhaseIdentificationCurve
            cbhydmodel.SelectedIndex = .HydrateModel

            chkControlBubInit.Checked = .BubbleUseCustomParameters
            chkControlDewInit.Checked = .DewUseCustomParameters

            chkBubLiqInstability.Checked = .CheckLiquidInstability
            lbBubDP.Text = su.deltaP
            lbBubDT.Text = su.deltaT
            lbBubP0.Text = su.pressure
            lbBubT0.Text = su.temperature
            lblBubTmax.Text = su.temperature

            lbDewDP.Text = su.deltaP
            lbDewDT.Text = su.deltaT
            lbDewP0.Text = su.pressure
            lbDewT0.Text = su.temperature
            lblDewTmax.Text = su.temperature

            If .BubbleCurveInitialFlash = "PVF" Then rbBubPVF.Checked = True Else rbBubTVF.Checked = True

            tbBubDP.Text = cv.ConvertFromSI(su.deltaP, .BubbleCurveDeltaP).ToString(nf)
            tbBubDT.Text = cv.ConvertFromSI(su.deltaT, .BubbleCurveDeltaT).ToString(nf)
            tbBubP0.Text = cv.ConvertFromSI(su.pressure, .BubbleCurveInitialPressure).ToString(nf)
            tbBubT0.Text = cv.ConvertFromSI(su.temperature, .BubbleCurveInitialTemperature).ToString(nf)
            tbBubTmax.Text = cv.ConvertFromSI(su.temperature, .BubbleCurveMaximumTemperature).ToString(nf)
            tbBubMaxPoints.Text = .BubbleCurveMaximumPoints

            If .DewCurveInitialFlash = "PVF" Then rbDewPVF.Checked = True Else rbDewTVF.Checked = True

            tbDewDP.Text = cv.ConvertFromSI(su.deltaP, .DewCurveDeltaP).ToString(nf)
            tbDewDT.Text = cv.ConvertFromSI(su.deltaT, .DewCurveDeltaT).ToString(nf)
            tbDewP0.Text = cv.ConvertFromSI(su.pressure, .DewCurveInitialPressure).ToString(nf)
            tbDewT0.Text = cv.ConvertFromSI(su.temperature, .DewCurveInitialTemperature).ToString(nf)
            tbDewTmax.Text = cv.ConvertFromSI(su.temperature, .DewCurveMaximumTemperature).ToString(nf)
            tbDewMaxPoints.Text = .DewCurveMaximumPoints

        End With

    End Sub

    Private Sub FormPhEnv_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If Not loaded Then Initialize()

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        If Not Me.AttachedTo Is Nothing Then

            Dim x As Double

            If Me.chkQualityLine.Checked Then
                If Double.TryParse(tbQuality.Text, x) Then
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
            If Me.chkQualityLine.Enabled Then Me.qualitycalc = Me.chkQualityLine.Checked Else Me.qualitycalc = False
            If Me.chkOp.Checked Then Me.showoppoint = True Else Me.showoppoint = False
            Me.phaseidentification = chkpip.Checked
            Me.hydratecalc = chkhyd.Checked
            Me.Button1.Enabled = False

            If My.Settings.EnableGPUProcessing Then
                Calculator.InitComputeDevice()
                Settings.gpu.EnableMultithreading()
            End If

            Me.BackgroundWorker1.RunWorkerAsync(New Object() {0, Me.tbQuality.Value, Me.chkQualityLine.Checked, Me.chkStabCurve.Checked, Me.chkpip.Checked, Me.chkhyd.Checked, Me.chkHydVapOnly.Checked})

            Me.bw = Me.BackgroundWorker1

            Me.PanelCalc.Visible = True
            Me.PanelCalc.Enabled = True

        End If

    End Sub

    Private Sub ComboBox1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox1.SelectedIndexChanged

        If Me.loaded And Me.calculated Then

            Select Case ComboBox1.SelectedIndex

                Case 0

                    Dim px1, py1, px2, py2, px3, py3, px4, py4, ph1, ph2, th1, th2, px5, py5, px6, py6, px7, py7, px8, py8 As New ArrayList

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
                    For i = 0 To TVB1.Count - 1
                        py7.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB1(i)))
                        px7.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB1(i)))
                    Next
                    For i = 0 To TVB2.Count - 1
                        py8.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB2(i)))
                        px8.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB2(i)))
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
                        If px7.Count > 0 Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha") & " Liq I", px7.ToArray(GetType(Double)), py7.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                                .Color = Color.SteelBlue
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Symbol.IsVisible = False
                            End With
                            With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha") & " Liq II", px8.ToArray(GetType(Double)), py8.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                                .Color = Color.SteelBlue
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Symbol.IsVisible = False
                            End With
                        End If
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
                        If chkStabCurve.Checked Then
                            With .AddCurve(DWSIM.App.GetLocalString("LimitedeEstabilidade"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkOrange
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If qualitycalc Then
                            With .AddCurve("V = " & Me.tbQuality.Text, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
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

                    Dim px1, py1, px2, py2, px3, py3, px4, py4, ph1, ph2, th1, th2, px5, py5, px6, py6, px7, py7, px8, py8 As New ArrayList

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
                    For i = 0 To TVB1.Count - 1
                        px7.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB1(i)))
                        py7.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB1(i)))
                    Next
                    For i = 0 To TVB2.Count - 1
                        px8.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, TVB2(i)))
                        py8.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB2(i)))
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
                        If px7.Count > 0 Then
                            With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha") & " Liq I", px7.ToArray(GetType(Double)), py7.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                                .Color = Color.SteelBlue
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Symbol.IsVisible = False
                            End With
                            With .AddCurve(DWSIM.App.GetLocalString("PontosdeBolha") & " Liq II", px8.ToArray(GetType(Double)), py8.ToArray(GetType(Double)), Color.SlateBlue, ZedGraph.SymbolType.Circle)
                                .Color = Color.SteelBlue
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Symbol.IsVisible = False
                            End With
                        End If
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
                        If chkStabCurve.Checked Then
                            With .AddCurve(DWSIM.App.GetLocalString("LimitedeEstabilidade"), px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
                                .Color = Color.DarkOrange
                                .Line.IsSmooth = False
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            End With
                        End If
                        If qualitycalc Then
                            With .AddCurve("V = " & Me.tbQuality.Text, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Red, ZedGraph.SymbolType.Circle)
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

    Private Sub BackgroundWorker1_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        Me.mat = AttachedTo
        Me.strname = AttachedTo.GraphicObject.Tag

        If Me.showoppoint Then
            ot = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, mat.Phases(0).Properties.temperature.GetValueOrDefault)
            op = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, mat.Phases(0).Properties.pressure.GetValueOrDefault)
            ov = mat.Phases(0).Properties.molecularWeight.GetValueOrDefault / mat.Phases(0).Properties.density.GetValueOrDefault / 1000
            oh = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, mat.Phases(0).Properties.enthalpy.GetValueOrDefault)
            os = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, mat.Phases(0).Properties.entropy.GetValueOrDefault)
        End If

        mat.PropertyPackage.CurrentMaterialStream = mat

        Dim diagdata As Object = mat.PropertyPackage.DW_ReturnPhaseEnvelope(EnvelopeSettings, Me.BackgroundWorker1)

        PC = diagdata(15)

        Dim th1, th2, ph1, ph2 As New ArrayList, Ph, Th As Object, Pmin, Pmax As Double, Vz() As Double, Vn() As String

        If e.Argument(5) = True Then

            Me.BackgroundWorker1.ReportProgress(99, "Hydrate Equilibrium Curves")

            Pmin = 101325
            Pmax = PC(0)(1) * 1.3
            Dim i As Integer = 0
            Vz = mat.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture)
            Vn = mat.PropertyPackage.RET_VNAMES

            Dim m_aux As New DWSIM.Utilities.HYD.AuxMethods

            For Ph = Pmin To Pmax Step 5 * 101325
                Try
                    Select Case Me.cbhydmodel.SelectedIndex
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
            Settings.gpu.DisableMultithreading()
            Settings.gpu.FreeAll()
        End If

        Me.Button1.Enabled = True

        Me.PanelCalc.Visible = False

        If e.Error IsNot Nothing Then
            Me.AttachedTo.GetFlowsheet.ShowMessage(e.Error.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            Exit Sub
        End If

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
        TVB1 = r(25)
        PB1 = r(26)
        HB1 = r(27)
        SB1 = r(28)
        VB1 = r(29)
        TVB2 = r(30)
        PB2 = r(31)
        HB2 = r(32)
        SB2 = r(33)
        VB2 = r(34)

        Cricondentherm = TVD.ToArray().Max
        Cricondenbar = PB.ToArray().Max
        If PO.ToArray.Max > Cricondenbar Then Cricondenbar = PO.ToArray.Max
        If PC.Count > 0 Then
            CriticalPressure = PC(0)(1)
            CriticalTemperature = PC(0)(0)
            CriticalVolume = PC(0)(2) * 1000
        End If

        calculated = True

        With Me.Grid1.Columns
            .Clear()
            .Add("c1", "Tb (" & su.temperature & ")")
            .Add("c2", "Pb (" & su.pressure & ")")
            .Add("c3", "Hb (" & su.enthalpy & ")")
            .Add("c4", "Sb (" & su.entropy & ")")
            .Add("c5", "Vb (m3/mol)")
            .Add("c6", "Td (" & su.temperature & ")")
            .Add("c7", "Pd (" & su.pressure & ")")
            .Add("c8", "Hd (" & su.enthalpy & ")")
            .Add("c9", "Sd (" & su.entropy & ")")
            .Add("c10", "Vd (m3/mol)")
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
            .Add("c26", "Tb1 (" & su.temperature & ")")
            .Add("c27", "Pb1 (" & su.pressure & ")")
            .Add("c28", "Hb1 (" & su.enthalpy & ")")
            .Add("c29", "Sb1 (" & su.entropy & ")")
            .Add("c30", "Vb1 (m3/mol)")
            .Add("c31", "Tb2 (" & su.temperature & ")")
            .Add("c32", "Pb2 (" & su.pressure & ")")
            .Add("c33", "Hb2 (" & su.enthalpy & ")")
            .Add("c34", "Sb2 (" & su.entropy & ")")
            .Add("c35", "Vb2 (m3/mol)")
        End With

        For Each c As DataGridViewColumn In Me.Grid1.Columns
            c.SortMode = DataGridViewColumnSortMode.NotSortable
            c.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
            c.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        Next

        Dim maxl As Integer = MathEx.Common.Max(New Object() {TVB.Count, TVB1.Count, TVB2.Count, TVD.Count, TE.Count, TQ.Count, TI.Count, THsI.Count, TOWF.Count}) - 1

        Dim k, j As Integer
        Dim maxc As Integer = Me.Grid1.Columns.Count - 1
        Dim data(maxc, maxl) As String

        j = 0
        For Each d As Double In TVB
            data(0, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In PB
            data(1, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In HB
            data(2, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In SB
            data(3, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In VB
            data(4, j) = d
            j = j + 1
        Next

        j = 0
        For Each d As Double In TVD
            data(5, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In PO
            data(6, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In HO
            data(7, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In SO
            data(8, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, d)
            j = j + 1
        Next
        j = 0
        For Each d As Double In VO
            data(9, j) = d
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
        For Each d As Double In TVB1
            data(25, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(26, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB1(j))
            data(27, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB1(j))
            data(28, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB1(j))
            data(29, j) = VB1(j)
            j = j + 1
        Next
        j = 0
        For Each d As Double In TVB2
            data(30, j) = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, d)
            data(31, j) = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, PB2(j))
            data(32, j) = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, HB2(j))
            data(33, j) = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, SB2(j))
            data(34, j) = VB2(j)
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

    Private Sub CheckBox1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkQualityLine.CheckedChanged
        If Me.chkQualityLine.Checked Then Me.tbQuality.Enabled = True Else Me.tbQuality.Enabled = False
        EnvelopeSettings.QualityLine = chkQualityLine.Checked
    End Sub

    Private Sub CheckBox2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkOp.CheckedChanged
        If Me.chkOp.Checked Then Me.showoppoint = True Else Me.showoppoint = False
        EnvelopeSettings.OperatingPoint = chkOp.Checked
    End Sub

    Private Sub FormPhEnv_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_PhaseEnvelope.htm")
    End Sub

    Private Sub chkhyd_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkhyd.CheckedChanged
        cbhydmodel.Enabled = chkhyd.Checked
        chkHydVapOnly.Enabled = chkhyd.Checked
        EnvelopeSettings.Hydrate = chkhyd.Checked
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbhydmodel.SelectedIndexChanged
        If cbhydmodel.SelectedIndex = 2 Then chkHydVapOnly.Enabled = False Else chkHydVapOnly.Enabled = True
        EnvelopeSettings.HydrateModel = cbhydmodel.SelectedIndex
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        My.Application.CalculatorStopRequested = True
        If Not bw Is Nothing Then bw.CancelAsync()
    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList

        Dim plist As New List(Of String)(New String() {"Name", "AutoUpdate", "EnvelopeType", "EnvelopeSettings"})

        plist.Add("Cricondentherm")
        plist.Add("Cricondenbar")
        plist.Add("Critical Pressure")
        plist.Add("Critical Temperature")
        plist.Add("Critical Volume")

        Return plist

    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Select Case pname
            Case "Cricondentherm"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.temperature
            Case "Cricondenbar"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.pressure
            Case "Critical Pressure"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.pressure
            Case "Critical Temperature"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.temperature
            Case "Critical Volume"
                Return AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem.molar_volume
            Case Else
                Return ""
        End Select
    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Dim units = AttachedTo.GetFlowsheet.FlowsheetOptions.SelectedUnitSystem
        Select Case pname
            Case "Name"
                Return Name
            Case "AutoUpdate"
                Return AutoUpdate
            Case "EnvelopeType"
                Return ComboBox1.SelectedIndex
            Case "EnvelopeSettings"
                Return Newtonsoft.Json.JsonConvert.SerializeObject(EnvelopeSettings)
            Case "Cricondentherm"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.temperature, Cricondentherm)
            Case "Cricondenbar"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.pressure, Cricondenbar)
            Case "Critical Pressure"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.pressure, CriticalPressure)
            Case "Critical Temperature"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.temperature, CriticalTemperature)
            Case "Critical Volume"
                Return SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(units.molar_volume, CriticalVolume)
            Case Else
                Return ""
        End Select
        Return ""
    End Function

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property Name1 As String Implements Interfaces.IAttachedUtility.Name

    Public Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue
        Select Case pname
            Case "Name"
                Name = pvalue
            Case "AutoUpdate"
                AutoUpdate = pvalue
            Case "EnvelopeType"
                ComboBox1.SelectedIndex = pvalue
            Case "EnvelopeSettings"
                EnvelopeSettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of PropertyPackages.PhaseEnvelopeOptions)(pvalue.ToString)
        End Select
    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        Button1_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.PhaseEnvelope
    End Function

    Public Property AutoUpdate As Boolean Implements Interfaces.IAttachedUtility.AutoUpdate

    Public Sub LoadData(data As Dictionary(Of String, Object)) Implements Interfaces.IAttachedUtility.LoadData
        For Each item In data
            SetPropertyValue(item.Key, item.Value)
        Next
    End Sub

    Public Function SaveData() As Dictionary(Of String, Object) Implements Interfaces.IAttachedUtility.SaveData
        Dim props As New Dictionary(Of String, Object)
        For Each prop In GetPropertyList()
            props.Add(prop, GetPropertyValue(prop))
        Next
        Return props
    End Function

    Private Sub tbQuality_ValueChanged(sender As Object, e As EventArgs) Handles tbQuality.ValueChanged
        Try
            EnvelopeSettings.QualityValue = tbQuality.Value
            tbQuality.ForeColor = Color.Blue
        Catch ex As Exception
            tbQuality.ForeColor = Color.Red
        End Try
    End Sub

    Private Sub chkStabCurve_CheckedChanged(sender As Object, e As EventArgs) Handles chkStabCurve.CheckedChanged
        EnvelopeSettings.StabilityCurve = chkStabCurve.Checked
    End Sub

    Private Sub chkHydVapOnly_CheckedChanged(sender As Object, e As EventArgs) Handles chkHydVapOnly.CheckedChanged
        EnvelopeSettings.HydrateVaporOnly = chkHydVapOnly.Checked
    End Sub

    Private Sub chkpip_CheckedChanged(sender As Object, e As EventArgs) Handles chkpip.CheckedChanged
        EnvelopeSettings.PhaseIdentificationCurve = chkpip.Checked
    End Sub

    Private Sub rbBubPVF_CheckedChanged(sender As Object, e As EventArgs) Handles rbBubPVF.CheckedChanged, rbBubTVF.CheckedChanged
        If rbBubPVF.Checked Then EnvelopeSettings.BubbleCurveInitialFlash = "PVF" Else EnvelopeSettings.BubbleCurveInitialFlash = "TVF"
    End Sub

    Private Sub tbBubP0_TextChanged(sender As Object, e As EventArgs) Handles tbBubP0.TextChanged

        Try
            EnvelopeSettings.BubbleCurveInitialPressure = cv.ConvertToSI(su.pressure, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbBubT0_TextChanged(sender As Object, e As EventArgs) Handles tbBubT0.TextChanged
        Try
            EnvelopeSettings.BubbleCurveInitialTemperature = cv.ConvertToSI(su.temperature, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbBubMaxPoints_TextChanged(sender As Object, e As EventArgs) Handles tbBubMaxPoints.TextChanged
        Try
            EnvelopeSettings.BubbleCurveMaximumPoints = DirectCast(sender, TextBox).Text
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbBubDP_TextChanged(sender As Object, e As EventArgs) Handles tbBubDP.TextChanged
        Try
            EnvelopeSettings.BubbleCurveDeltaP = cv.ConvertToSI(su.deltaP, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbBubDT_TextChanged(sender As Object, e As EventArgs) Handles tbBubDT.TextChanged
        Try
            EnvelopeSettings.BubbleCurveDeltaT = cv.ConvertToSI(su.deltaT, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub chkBubLiqInstability_CheckedChanged(sender As Object, e As EventArgs) Handles chkBubLiqInstability.CheckedChanged
        EnvelopeSettings.CheckLiquidInstability = chkBubLiqInstability.Checked
    End Sub

    Private Sub rbDewPVF_CheckedChanged(sender As Object, e As EventArgs) Handles rbDewPVF.CheckedChanged
        If rbDewPVF.Checked Then EnvelopeSettings.DewCurveInitialFlash = "PVF" Else EnvelopeSettings.DewCurveInitialFlash = "TVF"
    End Sub

    Private Sub tbDewP0_TextChanged(sender As Object, e As EventArgs) Handles tbDewP0.TextChanged
        Try
            EnvelopeSettings.DewCurveInitialPressure = cv.ConvertToSI(su.pressure, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbDewT0_TextChanged(sender As Object, e As EventArgs) Handles tbDewT0.TextChanged
        Try
            EnvelopeSettings.DewCurveInitialTemperature = cv.ConvertToSI(su.temperature, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbDewMaxPoints_TextChanged(sender As Object, e As EventArgs) Handles tbDewMaxPoints.TextChanged
        Try
            EnvelopeSettings.DewCurveMaximumPoints = DirectCast(sender, TextBox).Text
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbDewDP_TextChanged(sender As Object, e As EventArgs) Handles tbDewDP.TextChanged
        Try
            EnvelopeSettings.DewCurveDeltaP = cv.ConvertToSI(su.deltaP, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbDewDT_TextChanged(sender As Object, e As EventArgs) Handles tbDewDT.TextChanged
        Try
            EnvelopeSettings.DewCurveDeltaT = cv.ConvertToSI(su.temperature, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub chkControlBubInit_CheckedChanged(sender As Object, e As EventArgs) Handles chkControlBubInit.CheckedChanged
        PanelBub.Enabled = chkControlBubInit.Checked
        EnvelopeSettings.BubbleUseCustomParameters = chkControlBubInit.Checked
    End Sub

    Private Sub chkControlDewInit_CheckedChanged(sender As Object, e As EventArgs) Handles chkControlDewInit.CheckedChanged
        PanelDew.Enabled = chkControlDewInit.Checked
        EnvelopeSettings.DewUseCustomParameters = chkControlDewInit.Checked
    End Sub

    Private Sub tbBubTmax_TextChanged(sender As Object, e As EventArgs) Handles tbBubTmax.TextChanged
        Try
            EnvelopeSettings.BubbleCurveMaximumTemperature = cv.ConvertToSI(su.temperature, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub

    Private Sub tbDewTmax_TextChanged(sender As Object, e As EventArgs) Handles tbDewTmax.TextChanged
        Try
            EnvelopeSettings.DewCurveMaximumTemperature = cv.ConvertToSI(su.temperature, DirectCast(sender, TextBox).Text)
            DirectCast(sender, TextBox).ForeColor = Color.Blue
        Catch ex As Exception
            DirectCast(sender, TextBox).ForeColor = Color.Red
        End Try
    End Sub
End Class