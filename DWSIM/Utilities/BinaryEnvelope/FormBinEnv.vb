Imports DWSIM.Thermodynamics.BaseClasses

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

Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.ExtensionMethods
Imports System.Linq

Public Class FormBinEnv

    Inherits UserControl

    Implements Interfaces.IAttachedUtility

    Public su As New SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String
    Dim mw1, mw2 As Double
    Dim MinX, MaxX As Double

    Private loaded As Boolean = False

    Public bw As System.ComponentModel.BackgroundWorker

    Dim P, T As Double

    Private Flowsheet As Interfaces.IFlowsheet
    Private mat As MaterialStream

    Private Sub FormBinEnv_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        If Not loaded Then Initialize()

    End Sub

    Public Sub Populate() Implements Interfaces.IAttachedUtility.Populate

        Me.su = Flowsheet.FlowsheetOptions.SelectedUnitSystem
        Me.nf = Flowsheet.FlowsheetOptions.NumberFormat

        Dim s1, s2, s3 As Object

        s1 = Me.cbComp1.SelectedItem
        s2 = Me.cbComp2.SelectedItem
        s3 = Me.cbPropPack.SelectedItem

        Me.cbComp1.Items.Clear()
        Me.cbComp2.Items.Clear()
        For Each co As ConstantProperties In Flowsheet.SelectedCompounds.Values
            Me.cbComp1.Items.Add((co.Name))
            Me.cbComp2.Items.Add((co.Name))
        Next

        Me.cbPropPack.Items.Clear()
        For Each pp As PropertyPackage In Flowsheet.PropertyPackages.Values
            Me.cbPropPack.Items.Add(pp.Tag & " (" & pp.ComponentName & ")")
        Next

        Try
            Me.cbComp1.SelectedItem = s1
            Me.cbComp2.SelectedItem = s2
            Me.cbPropPack.SelectedItem = s3
        Catch ex As Exception
        End Try

        Me.lblP.Text = su.pressure
        Me.lblT.Text = su.temperature


        Me.GridExpData.Columns(1).HeaderText = "x1 (" & DWSIM.App.GetLocalString("FraoMolar1") & ")"
        Me.GridExpData.Columns(2).HeaderText = "y1 (" & DWSIM.App.GetLocalString("FraoMolar1") & ")"
        Me.GridExpData.Columns(3).HeaderText = "T (" & su.temperature & ")"
        Me.GridExpData.Columns(4).HeaderText = "P (" & su.pressure & ")"

    End Sub

    Public Sub Initialize() Implements Interfaces.IAttachedUtility.Initialize

        Flowsheet = AttachedTo.GetFlowsheet

        If Flowsheet.SelectedCompounds.Count > 1 Then

            Populate()

            If Me.cbPropPack.Items.Count > 0 Then Me.cbPropPack.SelectedIndex = 0

            If Me.cbComp1.Items.Count > 0 Then Me.cbComp1.SelectedIndex = 0
            If Me.cbComp2.Items.Count > 1 Then Me.cbComp2.SelectedIndex = 1

            mw1 = Flowsheet.SelectedCompounds.Values.ElementAt(0).Molar_Weight
            mw2 = Flowsheet.SelectedCompounds.Values.ElementAt(1).Molar_Weight

            MinX = 0
            MaxX = 1
            Me.tbStepCount.Text = (40).ToString()

            cbXAxisBasis.SelectedIndex = 0

            Me.tbP.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, 101325), nf)
            Me.tbT.Text = Format(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, 298.15), nf)


            Me.tbMin.Text = MinX.ToString()
            Me.tbMax.Text = MaxX.ToString()


            Me.GraphControl.IsShowPointValues = True

            If DWSIM.App.IsRunningOnMono Then GroupBox2.Width -= 80

        Else

            MessageBox.Show(DWSIM.App.GetLocalString("BinEnvError_TwoCompoundsMinimum"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

        loaded = True

    End Sub

    Private Sub BtnCalculate_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnCalculate.Click

        If Me.cbComp1.SelectedItem.ToString <> Me.cbComp2.SelectedItem.ToString Then

            With Me.GraphControl.GraphPane.Legend
                .Position = ZedGraph.LegendPos.TopCenter
                .Border.IsVisible = False
                .FontSpec.Size = 10
                .FontSpec.IsDropShadow = False
            End With

            Me.mat = New MaterialStream("", "")

            For Each phase As BaseClasses.Phase In mat.Phases.Values
                For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
                    If (cp.Name) = cbComp1.SelectedItem.ToString Then
                        With phase
                            .Compounds.Add(cp.Name, New BaseClasses.Compound(cp.Name, ""))
                            .Compounds(cp.Name).ConstantProperties = cp
                            mw1 = cp.Molar_Weight
                        End With
                        Exit For
                    End If
                Next
            Next

            For Each phase As BaseClasses.Phase In mat.Phases.Values
                For Each cp As ConstantProperties In Flowsheet.SelectedCompounds.Values
                    If (cp.Name) = cbComp2.SelectedItem.ToString Then
                        With phase
                            .Compounds.Add(cp.Name, New BaseClasses.Compound(cp.Name, ""))
                            .Compounds(cp.Name).ConstantProperties = cp
                            mw2 = cp.Molar_Weight
                        End With
                        Exit For
                    End If
                Next
            Next

            P = SystemsOfUnits.Converter.ConvertToSI(su.pressure, tbP.Text)
            T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, tbT.Text)

            Dim tipocalc As String = ""
            If Me.RadioButton1.Checked Then
                tipocalc = "T-x-y"
            ElseIf Me.RadioButton2.Checked Then
                tipocalc = "P-x-y"
            ElseIf Me.RadioButton3.Checked Then
                tipocalc = "(T)x-y"
            ElseIf Me.RadioButton4.Checked Then
                tipocalc = "(P)x-y"
            End If

            Dim lle As Boolean = False

            If chkLLE.Enabled Then
                If chkLLE.Checked Then
                    lle = True
                Else
                    lle = False
                End If
            Else
                lle = False
            End If

            Me.BtnCalculate.Enabled = False
            Me.BtnCalculate.BackColor = Color.LightGreen

            If My.Settings.EnableGPUProcessing Then
                Calculator.InitComputeDevice()
                Settings.gpu.EnableMultithreading()
            End If

            Dim Parameters As New Object()
            Parameters = {tipocalc, P, T, chkVLE.Checked, lle,
                          chkSLE.Checked, chkCritical.Checked, rbSolidSolution.Checked,
                          chkCompareModels.Checked, cbPropPack.SelectedIndex,
                          tbStepCount.Text.ToDoubleFromCurrent, MinX, MaxX}

            Me.BackgroundWorker1.RunWorkerAsync(Parameters)

            Me.bw = Me.BackgroundWorker1

            Me.LabelStatus.Text = ""
            Me.PanelCalc.Visible = True
            Me.PanelCalc.Enabled = True

        Else

            MessageBox.Show(DWSIM.App.GetLocalString("BinEnvError_DuplicateCompound"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)

        End If

    End Sub

    Private Sub BackgroundWorker1_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork

        Dim compare As Boolean = e.Argument(8)
        Dim ppidx = e.Argument(9)

        Dim k As Integer
        Dim pp As PropertyPackages.PropertyPackage = Nothing

        Dim results As New ArrayList

        If Not compare Then

            For Each pp1 As PropertyPackage In Flowsheet.PropertyPackages.Values
                If k = ppidx Then pp = pp1
                k += 1
            Next

            mat.SetFlowsheet(Flowsheet)
            pp.CurrentMaterialStream = mat
            results.Add(New Object() {pp.DW_ReturnBinaryEnvelope(e.Argument, Me.BackgroundWorker1), pp.ComponentName})

        Else

            ' get results from all property packages

            For Each pp1 As PropertyPackage In Flowsheet.PropertyPackages.Values
                mat.SetFlowsheet(Flowsheet)
                pp1.CurrentMaterialStream = mat
                results.Add(New Object() {pp1.DW_ReturnBinaryEnvelope(e.Argument, Me.BackgroundWorker1), pp1.ComponentName})
            Next

        End If

        e.Result = results

    End Sub

    Private Sub BackgroundWorker1_ProgressChanged(sender As Object, e As System.ComponentModel.ProgressChangedEventArgs) Handles BackgroundWorker1.ProgressChanged
        Me.LabelStatus.Text = e.UserState.ToString
    End Sub
    Function ConvertMolToDisplay(ByVal X As Double) As Double
        Dim XRes As Double

        Select Case cbXAxisBasis.SelectedIndex
            Case 0
                XRes = X
            Case 1
                XRes = X * mw1 / (X * mw1 + (1 - X) * mw2)
            Case 2
                XRes = X * 100
            Case 3
                XRes = X * mw1 / (X * mw1 + (1 - X) * mw2) * 100
        End Select
        Return XRes
    End Function
    Function ConvertDisplayedToMol(ByVal X As Double) As Double
        Dim XRes As Double

        Select Case cbXAxisBasis.SelectedIndex
            Case 0
                XRes = X
            Case 1
                XRes = X / mw1 / (X / mw1 + (1 - X) / mw2)
            Case 2
                XRes = X / 100
            Case 3
                XRes = X / mw1 / (X / mw1 + (100 - X) / mw2)
        End Select
        Return XRes
    End Function
    Private Sub BackgroundWorker1_RunWorkerCompleted(ByVal sender As Object, ByVal e As System.ComponentModel.RunWorkerCompletedEventArgs) Handles BackgroundWorker1.RunWorkerCompleted

        If My.Settings.EnableGPUProcessing Then
            Settings.gpu.DisableMultithreading()
            Settings.gpu.FreeAll()
        End If

        For Each pp1 As PropertyPackage In Flowsheet.PropertyPackages.Values
            pp1.CurrentMaterialStream = Nothing
        Next

        Me.BtnCalculate.Enabled = True
        Me.BtnCalculate.BackColor = SystemColors.Control
        Me.PanelCalc.Visible = False

        Me.Grid1.Columns.Clear()
        Me.Grid1.Rows.Clear()

        Me.GraphControl.GraphPane.CurveList.Clear()

        If e.Error IsNot Nothing Then Flowsheet.ShowMessage(e.Error.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)

        Dim c(1) As String
        c(0) = cbComp1.SelectedItem.ToString
        c(1) = cbComp2.SelectedItem.ToString

        Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.CurrentUICulture

        Dim vxexp, vyexp, vtexp, vpexp As New ArrayList

        For Each r As DataGridViewRow In Me.GridExpData.Rows
            Try
                If r.Cells("check").Value Then
                    If Double.TryParse(r.Cells("colx1").Value, New Double) Then vxexp.Add(Double.Parse(r.Cells("colx1").Value, ci)) Else vxexp.Add(0.0#)
                    If Double.TryParse(r.Cells("coly1").Value, New Double) Then vyexp.Add(Double.Parse(r.Cells("coly1").Value, ci)) Else vyexp.Add(0.0#)
                    If Double.TryParse(r.Cells("colt").Value, New Double) Then vtexp.Add(Double.Parse(r.Cells("colt").Value, ci)) Else vtexp.Add(0.0#)
                    If Double.TryParse(r.Cells("colp").Value, New Double) Then vpexp.Add(Double.Parse(r.Cells("colp").Value, ci)) Else vpexp.Add(0.0#)
                End If
            Catch ex As Exception
            End Try
        Next

        If Me.RadioButton1.Checked Then
            With Me.GraphControl.GraphPane
                If vxexp.Count > 0 Then
                    With .AddCurve(DWSIM.App.GetLocalString("ExpData"), vxexp.ToArray(GetType(Double)), vtexp.ToArray(GetType(Double)), Color.Black, ZedGraph.SymbolType.Circle)
                        .Line.IsVisible = False
                        .Symbol.IsVisible = True
                        .Symbol.Type = ZedGraph.SymbolType.Circle
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                End If
            End With
        ElseIf Me.RadioButton2.Checked Then
            With Me.GraphControl.GraphPane
                If vxexp.Count > 0 Then
                    With .AddCurve(DWSIM.App.GetLocalString("ExpData"), vxexp.ToArray(GetType(Double)), vpexp.ToArray(GetType(Double)), Color.Black, ZedGraph.SymbolType.Circle)
                        .Line.IsVisible = False
                        .Symbol.IsVisible = True
                        .Symbol.Type = ZedGraph.SymbolType.Circle
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                End If
            End With
        ElseIf Me.RadioButton3.Checked Or Me.RadioButton4.Checked Then
            With Me.GraphControl.GraphPane
                If vxexp.Count > 0 Then
                    With .AddCurve(DWSIM.App.GetLocalString("ExpData"), vxexp.ToArray(GetType(Double)), vyexp.ToArray(GetType(Double)), Color.Black, ZedGraph.SymbolType.Circle)
                        .Line.IsVisible = False
                        .Symbol.IsVisible = True
                        .Symbol.Type = ZedGraph.SymbolType.Circle
                        .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    End With
                End If
            End With
        End If

        Dim ridx As Integer = 0
        Dim lidx As Integer = 0

        For Each res In e.Result

            Dim i As Integer

            Dim r = res(0)
            Dim ppname As String = res(1)
            Dim rand = New Random()

            Dim linetype As Integer = lidx

            'convert x+y axis VLE
            For i = 0 To r(0).count - 1
                r(0)(i) = ConvertMolToDisplay(r(0)(i))
                If Me.RadioButton3.Checked Or Me.RadioButton4.Checked Then
                    r(1)(i) = ConvertMolToDisplay(r(1)(i))
                End If
            Next
            If r.Length > 2 Then
                For i = 0 To r(3).count - 1
                    r(3)(i) = ConvertMolToDisplay(r(3)(i))
                Next
                For i = 0 To r(4).count - 1
                    r(4)(i) = ConvertMolToDisplay(r(4)(i))
                Next
            End If

            If Me.RadioButton1.Checked Then

                Dim px, py1, py2, px1l1, px1l2, py3, pxs1, pys1, pxs2, pys2, pxc, pyc As New ArrayList
                px = r(0)
                py1 = r(1)
                py2 = r(2)

                px1l1 = r(3)
                px1l2 = r(4)
                py3 = r(5)
                pxs1 = r(6)
                pys1 = r(7)
                pxs2 = r(8)
                pys2 = r(9)
                pxs2 = r(8)
                pxc = r(10)
                pyc = r(11)

                Dim vx1, vx2, vy1, vy2, vx1l1, vx1l2, vyL1, vyL2, vxs1, vys1, vxs2, vys2, vxc, vyc As New ArrayList

                If py1.Count > 0 Then
                    i = 0
                    Do
                        If py1(i) <> 0.0# Then
                            vx1.Add(px(i))
                            vy1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, py1(i)))
                        End If
                        If py2(i) <> 0.0# Then
                            vx2.Add(px(i))
                            vy2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, py2(i)))
                        End If
                        i += 1
                    Loop Until i = px.Count
                End If

                If px1l1.Count > 0 Then
                    i = 0
                    Do
                        If px1l1(i) > MinX And px1l1(i) < MaxX Then
                            vx1l1.Add(px1l1(i))
                            vyL1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, py3(i)))
                        End If
                        i += 1
                    Loop Until i = px1l1.Count
                End If
                If px1l2.Count > 0 Then
                    i = 0
                    Do
                        If px1l2(i) > MinX And px1l2(i) < MaxX Then
                            vx1l2.Add(px1l2(i))
                            vyL2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, py3(i)))
                        End If
                        i += 1
                    Loop Until i = px1l2.Count
                End If

                If pys1.Count > 0 Then
                    i = 0
                    Do
                        vxs1.Add(pxs1(i))
                        vys1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, pys1(i)))
                        i += 1
                    Loop Until i = pys1.Count
                End If

                If pys2.Count > 0 Then
                    i = 0
                    Do
                        vxs2.Add(pxs2(i))
                        vys2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, pys2(i)))
                        i += 1
                    Loop Until i = pys2.Count
                End If

                If pxc.Count > 0 Then
                    i = 0
                    Do
                        vxc.Add(pxc(i))
                        vyc.Add(SystemsOfUnits.Converter.ConvertFromSI(su.temperature, pyc(i)))
                        i += 1
                    Loop Until i = pxc.Count
                End If

                Dim n As Integer = New List(Of Integer)({vx1.Count, vx2.Count, vy1.Count, vy2.Count, vx1l1.Count, vyL1.Count, vyL2.Count, vxs1.Count, vxc.Count}).Max

                With Me.Grid1.Columns
                    .Add("[" & ppname & "] ""c1", "[" & ppname & "] " & "VLE x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c2", "[" & ppname & "] " & "VLE Tbub (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c3", "[" & ppname & "] " & "VLE x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c4", "[" & ppname & "] " & "VLE Tdew (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c5", "[" & ppname & "] " & "LLE x' (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c6", "[" & ppname & "] " & "LLE T (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c7", "[" & ppname & "] " & "LLE x'' (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c8", "[" & ppname & "] " & "LLE T (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c9", "[" & ppname & "] " & "SLE x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c10", "[" & ppname & "] " & "SLE L/SL T (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c11", "[" & ppname & "] " & "SLE x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c12", "[" & ppname & "] " & "SLE S/SL T (" & su.temperature & ")")
                    .Add("[" & ppname & "] ""c13", "[" & ppname & "] " & "CRIT x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c14", "[" & ppname & "] " & "CRIT T (" & su.temperature & ")")
                End With

                If n = 0 Then
                    MessageBox.Show("No points were generated for the selected diagram. Please check your thermo setup.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If Me.Grid1.Rows.Count = 0 Then Me.Grid1.Rows.Add(n)

                For Each co As DataGridViewColumn In Me.Grid1.Columns
                    co.SortMode = DataGridViewColumnSortMode.NotSortable
                    co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
                    co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
                Next
                Dim j, k As Integer
                Dim data(13, n) As String
                j = 0
                For Each d As Double In vx1
                    data(0, j) = vx1(j)
                    data(1, j) = vy1(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vx2
                    data(2, j) = vx2(j)
                    data(3, j) = vy2(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vx1l1
                    data(4, j) = vx1l1(j)
                    data(5, j) = vyL1(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vx1l2
                    data(6, j) = vx1l2(j)
                    data(7, j) = vyL2(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vxs1
                    data(8, j) = vxs1(j)
                    data(9, j) = vys1(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vxs2
                    data(10, j) = vxs2(j)
                    data(11, j) = vys2(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vxc
                    data(12, j) = vxc(j)
                    data(13, j) = vyc(j)
                    j = j + 1
                Next
                With Me.Grid1.Rows
                    k = 0
                    Do
                        j = 0
                        Do
                            If Double.TryParse(data(j, k), New Double) Then
                                .Item(k).Cells(j + (ridx * 14)).Value = Format(Convert.ToDouble(data(j, k)), nf)
                            Else
                                .Item(k).Cells(j + (ridx * 14)).Value = data(j, k)
                            End If
                            j = j + 1
                        Loop Until j = 14
                        k = k + 1
                    Loop Until k = n
                End With

                With Me.GraphControl.GraphPane
                    .Title.Text = c(0) & " / " & c(1) & vbCrLf & "P = " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P) & " " & su.pressure
                    With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("PontosdeBolha"), vx1.ToArray(GetType(Double)), vy1.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.SmoothTension = 0.3
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("PontosdeOrvalho"), vx2.ToArray(GetType(Double)), vy2.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.SmoothTension = 0.3
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    If vx1l1.Count > 0 Then
                        With .AddCurve(DWSIM.App.GetLocalString("[" & ppname & "] " & "LLE LP1"), vx1l1.ToArray(GetType(Double)), vyL1.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                            .Line.IsSmooth = False
                            .Line.SmoothTension = 0.3
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                        With .AddCurve(DWSIM.App.GetLocalString("[" & ppname & "] " & "LLE LP2"), vx1l2.ToArray(GetType(Double)), vyL2.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                            .Line.IsSmooth = False
                            .Line.SmoothTension = 0.3
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                    End If
                    If pys1.Count > 0 Then
                        With .AddCurve(DWSIM.App.GetLocalString("[" & ppname & "] " & "SLE SL/S"), vxs1.ToArray(GetType(Double)), vys1.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                            .Line.IsSmooth = False
                            .Line.SmoothTension = 0.3
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                    End If
                    If pys2.Count > 0 Then
                        With .AddCurve(DWSIM.App.GetLocalString("[" & ppname & "] " & "SLE L/SL"), vxs2.ToArray(GetType(Double)), vys2.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                            .Line.IsSmooth = False
                            .Line.SmoothTension = 0.3
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                    End If
                    If pxc.Count > 0 Then
                        With .AddCurve(DWSIM.App.GetLocalString("[" & ppname & "] " & "Critical"), vxc.ToArray(GetType(Double)), vyc.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                            .Line.IsSmooth = False
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                    End If
                    .XAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " / " & c(0)
                    .YAxis.Title.Text = "T / " & su.temperature
                    .Legend.IsVisible = True
                    .Legend.Position = ZedGraph.LegendPos.BottomFlushLeft
                    Me.GraphControl.IsAutoScrollRange = True

                    Me.GraphControl.GraphPane.XAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.XAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    .AxisChange(Me.CreateGraphics)
                    Me.GraphControl.Invalidate()

                End With

            ElseIf Me.RadioButton2.Checked Then

                Dim px, py1, py2, px1l1, px1l2, py3 As New ArrayList
                px = r(0)
                py1 = r(1)
                py2 = r(2)
                px1l1 = r(3)
                px1l2 = r(4)
                py3 = r(5)

                Dim vx1, vx2, vy1, vy2, vx1l1, vx1l2, vy3 As New ArrayList

                i = 0
                Do
                    If py1(i) <> 0.0# Then
                        vx1.Add(px(i))
                        vy1.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, py1(i)))
                    End If
                    If py2(i) <> 0.0# Then
                        vx2.Add(px(i))
                        vy2.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, py2(i)))
                    End If
                    i += 1
                Loop Until i = px.Count

                If px1l1.Count > 0 Then
                    i = 0
                    Do
                        vx1l1.Add(px1l1(i))
                        vx1l2.Add(px1l2(i))
                        vy3.Add(SystemsOfUnits.Converter.ConvertFromSI(su.pressure, py3(i)))
                        i += 1
                    Loop Until i = px1l1.Count
                End If

                With Me.Grid1.Columns
                    .Add("[" & ppname & "] ""c1", "[" & ppname & "] " & "x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c2", "[" & ppname & "] " & "Pbub (" & su.pressure & ")")
                    .Add("[" & ppname & "] ""c3", "[" & ppname & "] " & "x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c4", "[" & ppname & "] " & "Pdew (" & su.pressure & ")")
                End With

                Dim n As Integer = New List(Of Integer)({vx1.Count, vx2.Count, vy1.Count, vy2.Count, vx1l1.Count, vy3.Count}).Max

                If n = 0 Then
                    MessageBox.Show("No points were generated for the selected diagram. Please check your thermo setup.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If Me.Grid1.Rows.Count = 0 Then Me.Grid1.Rows.Add(n)

                For Each co As DataGridViewColumn In Me.Grid1.Columns
                    co.SortMode = DataGridViewColumnSortMode.NotSortable
                    co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
                    co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
                Next
                Dim j, k As Integer
                Dim data(11, Math.Max(vx1.Count - 1, vx2.Count - 1)) As String
                j = 0
                For Each d As Double In vx1
                    data(0, j) = vx1(j)
                    data(1, j) = vy1(j)
                    j = j + 1
                Next
                j = 0
                For Each d As Double In vx2
                    data(2, j) = vx2(j)
                    data(3, j) = vy2(j)
                    j = j + 1
                Next
                With Me.Grid1.Rows
                    k = 0
                    Do
                        j = 0
                        Do
                            If Double.TryParse(data(j, k), New Double) Then
                                .Item(k).Cells(j + (ridx * 4)).Value = Format(Convert.ToDouble(data(j, k)), nf)
                            Else
                                .Item(k).Cells(j + (ridx * 4)).Value = data(j, k)
                            End If
                            j = j + 1
                        Loop Until j = 4
                        k = k + 1
                    Loop Until k = n
                End With

                With Me.GraphControl.GraphPane
                    .Title.Text = c(0) & " / " & c(1) & vbCrLf & "T = " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T) & " " & su.temperature
                    With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("PontosdeBolha"), vx1.ToArray(GetType(Double)), vy1.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("PontosdeOrvalho"), vx2.ToArray(GetType(Double)), vy2.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    If vx1l1.Count > 0 Then
                        With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("LLE LP1"), vx1l1.ToArray(GetType(Double)), vy3.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Diamond)
                            .Line.IsSmooth = False
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                        With .AddCurve("[" & ppname & "] " & DWSIM.App.GetLocalString("LLE LP2"), vx1l2.ToArray(GetType(Double)), vy3.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Diamond)
                            .Line.IsSmooth = False
                            .Line.Style = linetype
                            .Line.Width = 2
                            .Symbol.IsVisible = False
                        End With
                    End If
                    .XAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " / " & c(0)
                    .YAxis.Title.Text = "P / " & su.pressure
                    .Legend.IsVisible = True
                    .Legend.Position = ZedGraph.LegendPos.BottomFlushLeft
                    Me.GraphControl.IsAutoScrollRange = True

                    Me.GraphControl.GraphPane.XAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.XAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    Me.GraphControl.GraphPane.YAxis.Scale.MinAuto = True
                    Me.GraphControl.GraphPane.YAxis.Scale.MaxAuto = True

                    .AxisChange(Me.CreateGraphics)
                    Me.GraphControl.Invalidate()
                End With

            ElseIf Me.RadioButton3.Checked Then

                Dim px, py As New ArrayList
                px = r(0)
                py = r(1)

                Dim vx, vy As New ArrayList

                i = 0
                Do
                    vx.Add(px(i))
                    vy.Add(py(i))
                    i += 1
                Loop Until i = px.Count

                With Me.Grid1.Columns
                    .Add("[" & ppname & "] ""c1", "[" & ppname & "] " & "x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c2", "[" & ppname & "] " & "y (" & c(0) & ")")
                End With

                If vx.Count = 0 Then
                    MessageBox.Show("No points were generated for the selected diagram. Please check your thermo setup.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If Me.Grid1.Rows.Count = 0 Then Me.Grid1.Rows.Add(vx.Count)

                For Each co As DataGridViewColumn In Me.Grid1.Columns
                    co.SortMode = DataGridViewColumnSortMode.NotSortable
                    co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
                    co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
                Next
                Dim j, k As Integer
                Dim data(11, vx.Count - 1) As String
                j = 0
                For Each d As Double In px
                    data(0, j) = vx(j)
                    data(1, j) = vy(j)
                    j = j + 1
                Next
                With Me.Grid1.Rows
                    k = 0
                    Do
                        j = 0
                        Do
                            If Double.TryParse(data(j, k), New Double) Then
                                .Item(k).Cells(j + (ridx * 2)).Value = Format(Convert.ToDouble(data(j, k)), nf)
                            Else
                                .Item(k).Cells(j + (ridx * 2)).Value = data(j, k)
                            End If
                            j = j + 1
                        Loop Until j = 2
                        k = k + 1
                    Loop Until k = vx.Count
                End With

                With Me.GraphControl.GraphPane
                    .Title.Text = c(0) & " / " & c(1) & vbCrLf & "P = " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P) & " " & su.pressure
                    With .AddCurve("[" & ppname & "]", vx.ToArray(GetType(Double)), vy.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    vx.Clear()
                    vx.Add(0.0)
                    vx.Add(1.0)
                    With .AddCurve("[" & ppname & "]", vx.ToArray(GetType(Double)), vx.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.None)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    .XAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " (X) - " & c(0)
                    .YAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " (Y) - " & c(0)
                    .Legend.IsVisible = True
                    .Legend.Position = ZedGraph.LegendPos.BottomFlushLeft
                    Me.GraphControl.IsAutoScrollRange = True

                    Me.GraphControl.GraphPane.XAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.XAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    Me.GraphControl.GraphPane.YAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.YAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    .AxisChange(Me.CreateGraphics)
                    Me.GraphControl.Invalidate()
                End With

            ElseIf Me.RadioButton4.Checked Then

                Dim px, py As New ArrayList
                px = r(0)
                py = r(1)

                Dim vx, vy As New ArrayList

                i = 0
                Do
                    vx.Add(px(i))
                    vy.Add(py(i))
                    i += 1
                Loop Until i = px.Count

                With Me.Grid1.Columns
                    .Add("[" & ppname & "] ""c1", "[" & ppname & "] " & "x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c2", "[" & ppname & "] " & "y (" & c(0) & ")")
                End With

                If vx.Count = 0 Then
                    MessageBox.Show("No points were generated for the selected diagram. Please check your thermo setup.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If Me.Grid1.Rows.Count = 0 Then Me.Grid1.Rows.Add(vx.Count)

                For Each co As DataGridViewColumn In Me.Grid1.Columns
                    co.SortMode = DataGridViewColumnSortMode.NotSortable
                    co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
                    co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
                Next
                Dim j, k As Integer
                Dim data(11, vx.Count - 1) As String
                j = 0
                For Each d As Double In px
                    data(0, j) = vx(j)
                    data(1, j) = vy(j)
                    j = j + 1
                Next
                With Me.Grid1.Rows
                    k = 0
                    Do
                        j = 0
                        Do
                            If Double.TryParse(data(j, k), New Double) Then
                                .Item(k).Cells(j + (ridx * 2)).Value = Format(Convert.ToDouble(data(j, k)), nf)
                            Else
                                .Item(k).Cells(j + (ridx * 2)).Value = data(j, k)
                            End If
                            j = j + 1
                        Loop Until j = 2
                        k = k + 1
                    Loop Until k = vx.Count
                End With

                With Me.GraphControl.GraphPane
                    .Title.Text = c(0) & " / " & c(1) & vbCrLf & "T = " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T) & " " & su.temperature
                    With .AddCurve("[" & ppname & "]", vx.ToArray(GetType(Double)), vy.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    vx.Clear()
                    vx.Add(0.0)
                    vx.Add(1.0)
                    With .AddCurve("[" & ppname & "]", vx.ToArray(GetType(Double)), vx.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.None)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    .XAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " (X) - " & c(0)
                    .YAxis.Title.Text = cbXAxisBasis.SelectedItem.ToString & " (Y) - " & c(0)
                    .Legend.IsVisible = True
                    .Legend.Position = ZedGraph.LegendPos.BottomFlushLeft
                    Me.GraphControl.IsAutoScrollRange = True

                    Me.GraphControl.GraphPane.XAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.XAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    Me.GraphControl.GraphPane.YAxis.Scale.Min = ConvertMolToDisplay(MinX)
                    Me.GraphControl.GraphPane.YAxis.Scale.Max = ConvertMolToDisplay(MaxX)

                    .AxisChange(Me.CreateGraphics)
                    Me.GraphControl.Invalidate()
                End With

            Else

                Dim px, py As ArrayList
                px = r(0)
                py = r(1)

                Dim vx, vy As New ArrayList

                i = 0
                Do
                    vx.Add(px(i))
                    vy.Add(py(i))
                    i += 1
                Loop Until i = px.Count

                With Me.Grid1.Columns
                    .Add("[" & ppname & "] ""c1", "[" & ppname & "] " & "x (" & c(0) & ")")
                    .Add("[" & ppname & "] ""c2", "[" & ppname & "] " & DWSIM.App.GetLocalString("DeltaGRT"))
                End With

                If Me.Grid1.Rows.Count = 0 Then Me.Grid1.Rows.Add(vx.Count)

                For Each co As DataGridViewColumn In Me.Grid1.Columns
                    co.SortMode = DataGridViewColumnSortMode.NotSortable
                    co.DefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleRight
                    co.HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleCenter
                Next
                Dim j, k As Integer
                Dim data(11, vx.Count - 1) As String
                j = 0
                For Each d As Double In px
                    data(0, j) = vx(j)
                    data(1, j) = vy(j)
                    j = j + 1
                Next
                With Me.Grid1.Rows
                    k = 0
                    Do
                        j = 0
                        Do
                            If Double.TryParse(data(j, k), New Double) Then
                                .Item(k).Cells(j + (ridx * 14)).Value = Format(Convert.ToDouble(data(j, k)), nf)
                            Else
                                .Item(k).Cells(j + (ridx * 14)).Value = data(j, k)
                            End If
                            j = j + 1
                        Loop Until j = 2
                        k = k + 1
                    Loop Until k = vx.Count
                End With

                With Me.GraphControl.GraphPane
                    .Title.Text = c(0) & " / " & c(1) & vbCrLf & "T = " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, T) & " " & su.temperature & ", P = " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P) & " " & su.pressure
                    With .AddCurve("[" & ppname & "]", vx.ToArray(GetType(Double)), vy.ToArray(GetType(Double)), Color.FromArgb(rand.Next(256), rand.Next(256), rand.Next(256)), ZedGraph.SymbolType.Circle)
                        .Line.IsSmooth = False
                        .Line.Style = linetype
                        .Line.Width = 2
                        .Symbol.IsVisible = False
                    End With
                    .XAxis.Title.Text = DWSIM.App.GetLocalString("FraoMolarx") & c(0)
                    .YAxis.Title.Text = DWSIM.App.GetLocalString("DeltaGRT")
                    .Legend.IsVisible = True
                    .Legend.Position = ZedGraph.LegendPos.BottomFlushLeft
                    .AxisChange(Me.CreateGraphics)
                    Me.GraphControl.IsAutoScrollRange = True
                    Select Case cbXAxisBasis.SelectedIndex
                        Case 0, 1
                            Me.GraphControl.GraphPane.XAxis.Scale.Max = 1
                        Case 2, 3
                            Me.GraphControl.GraphPane.XAxis.Scale.Max = 100
                    End Select
                    Me.GraphControl.Invalidate()
                End With

            End If

            ridx += 1
            lidx += 1

            If lidx > 4 Then lidx = 0

        Next

    End Sub

    Private Sub RadioButton1_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles RadioButton1.CheckedChanged, RadioButton2.CheckedChanged, RadioButton3.CheckedChanged, RadioButton4.CheckedChanged

        chkVLE.Enabled = RadioButton1.Checked
        chkLLE.Enabled = RadioButton1.Checked
        chkSLE.Enabled = RadioButton1.Checked
        chkCritical.Enabled = RadioButton1.Checked

    End Sub

    Private Sub TSB_Print_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSB_Print.Click
        Me.GraphControl.DoPrint()
    End Sub

    Private Sub TSB_PrinterSetup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSB_PrinterSetup.Click
        Me.PrintDialog1.ShowDialog()
    End Sub

    Private Sub TSB_PageSetup_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSB_PageSetup.Click
        Me.GraphControl.DoPageSetup()
    End Sub

    Private Sub TSB_Preview_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSB_Preview.Click
        Me.GraphControl.DoPrintPreview()
    End Sub

    Private Sub TSB_Copy_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSB_Copy.Click
        Me.GraphControl.Copy(1, False)
    End Sub

    Private Sub chkVLE_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkVLE.CheckedChanged
        chkLLE.Enabled = chkVLE.Checked
    End Sub

    Private Sub chkSLE_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkSLE.CheckedChanged
        rbEutectic.Enabled = chkSLE.Checked
        rbSolidSolution.Enabled = chkSLE.Checked
    End Sub

    Private Sub FormBinEnv_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested
        DWSIM.App.HelpRequested("UT_BinaryEnvelope.htm")
    End Sub

    Private Sub chkCompareModels_CheckedChanged(sender As Object, e As EventArgs) Handles chkCompareModels.CheckedChanged
        cbPropPack.Enabled = Not chkCompareModels.Checked
    End Sub

    Private Sub GridExpData_KeyDown1(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles GridExpData.KeyDown

        If e.KeyCode = Keys.Delete And e.Modifiers = Keys.Shift Then
            Dim toremove As New ArrayList
            For Each c As DataGridViewCell In Me.GridExpData.SelectedCells
                If Not toremove.Contains(c.RowIndex) Then toremove.Add(c.RowIndex)
            Next
            For Each i As Integer In toremove
                Try
                    Me.GridExpData.Rows.RemoveAt(i)
                Catch ex As Exception

                End Try
            Next
        ElseIf e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(GridExpData)
        ElseIf e.KeyCode = Keys.Delete Then
            For Each c As DataGridViewCell In Me.GridExpData.SelectedCells
                If c.ColumnIndex <> 0 Then c.Value = "" Else c.Value = False
            Next
        ElseIf e.KeyCode = Keys.C And e.Modifiers = Keys.Control Then
            Clipboard.SetDataObject(GridExpData.GetClipboardContent())
        End If

    End Sub

    Public Sub PasteData(ByRef dgv As DataGridView)

        Dim tArr() As String
        Dim arT() As String
        Dim i, ii As Integer
        Dim c, cc, r As Integer

        tArr = Clipboard.GetText().Split(Environment.NewLine)

        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If

        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
                For ii = 0 To arT.Length - 1
                    If r > dgv.Rows.Count - 1 Then
                        dgv.Rows.Add(New Object() {"True"})
                        dgv.Rows(0).Cells(1).Selected = True
                    End If
                Next
                r = r + 1
            End If
        Next

        If dgv.SelectedCells.Count > 0 Then
            r = dgv.SelectedCells(0).RowIndex
            c = dgv.SelectedCells(0).ColumnIndex
        Else
            r = 0
            c = 0
        End If

        For i = 0 To tArr.Length - 1
            If tArr(i) <> "" Then
                arT = tArr(i).Split(vbTab)
                cc = c
                For ii = 0 To arT.Length - 1
                    cc = GetNextVisibleCol(dgv, cc)
                    If cc > dgv.ColumnCount - 1 Then Exit For
                    dgv.Item(cc, r).Value = arT(ii).TrimStart
                    cc = cc + 1
                Next
                r = r + 1
            End If
        Next

    End Sub

    Private Function GetNextVisibleCol(ByRef dgv As DataGridView, ByVal stidx As Integer) As Integer

        Dim i As Integer

        For i = stidx To dgv.ColumnCount - 1
            If dgv.Columns(i).Visible Then Return i
        Next

        Return Nothing

    End Function

    Private Sub GridExpData_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles GridExpData.DataError

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        My.Application.CalculatorStopRequested = True
        If Not bw Is Nothing Then bw.CancelAsync()
    End Sub

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return New List(Of String)(New String() {"Name", "AutoUpdate", "Comp1", "Comp2", "Type", "VLE", "LLE", "SLE", "SLE_SS", "CRIT", "XAxisBase", "P", "T", "dx", "PP", "CompareModels", "ExpX", "ExpY", "ExpT", "ExpP"})
    End Function

    Public Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Return ""
    End Function

    Public Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Select Case pname
            Case "Name"
                Return Name
            Case "AutoUpdate"
                Return AutoUpdate
            Case "Comp1"
                Return cbComp1.SelectedItem.ToString
            Case "Comp2"
                Return cbComp2.SelectedItem.ToString
            Case "Type"
                If RadioButton1.Checked Then Return "Txy"
                If RadioButton2.Checked Then Return "Pxy"
                If RadioButton3.Checked Then Return "(T)xy"
                If RadioButton4.Checked Then Return "(P)xy"
            Case "VLE"
                Return chkVLE.Checked
            Case "LLE"
                Return chkLLE.Checked
            Case "SLE"
                Return chkSLE.Checked
            Case "SLE_SS"
                Return rbSolidSolution.Checked
            Case "CRIT"
                Return chkCritical.Checked
            Case "XAxisBase"
                Return cbXAxisBasis.SelectedIndex
            Case "P"
                Return Double.Parse(tbP.Text)
            Case "T"
                Return Double.Parse(tbT.Text)
            Case "dx"
                Return Double.Parse(tbStepCount.Text)
            Case "PP"
                Return cbPropPack.SelectedIndex
            Case "CompareModels"
                Return chkCompareModels.Checked
            Case "ExpX"
                Dim datap As New List(Of Double)
                For Each r As DataGridViewRow In Me.GridExpData.Rows
                    Try
                        If r.Cells("check").Value Then
                            If Double.TryParse(r.Cells("colx1").Value, New Double) Then datap.Add(Double.Parse(r.Cells("colx1").Value)) Else datap.Add(0.0#)
                        End If
                    Catch ex As Exception
                    End Try
                Next
                Return datap
            Case "ExpY"
                Dim datap As New List(Of Double)
                For Each r As DataGridViewRow In Me.GridExpData.Rows
                    Try
                        If r.Cells("check").Value Then
                            If Double.TryParse(r.Cells("coly1").Value, New Double) Then datap.Add(Double.Parse(r.Cells("coly1").Value)) Else datap.Add(0.0#)
                        End If
                    Catch ex As Exception
                    End Try
                Next
                Return datap
            Case "ExpT"
                Dim datap As New List(Of Double)
                For Each r As DataGridViewRow In Me.GridExpData.Rows
                    Try
                        If r.Cells("check").Value Then
                            If Double.TryParse(r.Cells("colt").Value, New Double) Then datap.Add(Double.Parse(r.Cells("colt").Value)) Else datap.Add(0.0#)
                        End If
                    Catch ex As Exception
                    End Try
                Next
                Return datap
            Case "ExpP"
                Dim datap As New List(Of Double)
                For Each r As DataGridViewRow In Me.GridExpData.Rows
                    Try
                        If r.Cells("check").Value Then
                            If Double.TryParse(r.Cells("colp").Value, New Double) Then datap.Add(Double.Parse(r.Cells("colp").Value)) Else datap.Add(0.0#)
                        End If
                    Catch ex As Exception
                    End Try
                Next
                Return datap
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
            Case "Comp1"
                cbComp1.SelectedItem = pvalue
            Case "Comp2"
                cbComp2.SelectedItem = pvalue
            Case "Type"
                If pvalue = "Txy" Then RadioButton1.Checked = True
                If pvalue = "Pxy" Then RadioButton2.Checked = True
                If pvalue = "(T)xy" Then RadioButton3.Checked = True
                If pvalue = "(P)xy" Then RadioButton4.Checked = True
            Case "VLE"
                chkVLE.Checked = pvalue
            Case "LLE"
                chkLLE.Checked = pvalue
            Case "SLE"
                chkSLE.Checked = pvalue
            Case "SLE_SS"
                rbSolidSolution.Checked = pvalue
            Case "CRIT"
                chkCritical.Checked = pvalue
            Case "XAxisBase"
                cbXAxisBasis.SelectedIndex = pvalue
            Case "P"
                tbP.Text = pvalue
            Case "T"
                tbT.Text = pvalue
            Case "dx"
                tbStepCount.Text = pvalue
            Case "PP"
                cbPropPack.SelectedIndex = pvalue
            Case "CompareModels"
                chkCompareModels.Checked = pvalue
            Case "ExpX"
                If GridExpData.Rows.Count < 100 Then GridExpData.Rows.Add(100)
                Dim datap As List(Of Double) = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of Double))(pvalue.ToString)
                For i As Integer = 0 To datap.Count - 1
                    GridExpData.Rows(i).Cells("colx1").Value = datap(i)
                Next
            Case "ExpY"
                If GridExpData.Rows.Count < 100 Then GridExpData.Rows.Add(100)
                Dim datap As List(Of Double) = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of Double))(pvalue.ToString)
                For i As Integer = 0 To datap.Count - 1
                    GridExpData.Rows(i).Cells("coly1").Value = datap(i)
                Next
            Case "ExpT"
                If GridExpData.Rows.Count < 100 Then GridExpData.Rows.Add(100)
                Dim datap As List(Of Double) = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of Double))(pvalue.ToString)
                For i As Integer = 0 To datap.Count - 1
                    GridExpData.Rows(i).Cells("colt").Value = datap(i)
                Next
            Case "ExpP"
                If GridExpData.Rows.Count < 100 Then GridExpData.Rows.Add(100)
                Dim datap As List(Of Double) = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of Double))(pvalue.ToString)
                For i As Integer = 0 To datap.Count - 1
                    GridExpData.Rows(i).Cells("colp").Value = datap(i)
                Next
        End Select
    End Sub

    Public Sub Update1() Implements Interfaces.IAttachedUtility.Update
        BtnCalculate_Click(Me, New EventArgs)
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return FlowsheetUtility.PhaseEnvelopeBinary
    End Function

    Private Sub tbMin_TextChanged(sender As Object, e As EventArgs) Handles tbMin.TextChanged
        MinX = ConvertDisplayedToMol(tbMin.Text.ToDoubleFromCurrent)
    End Sub

    Private Sub tbMax_TextChanged(sender As Object, e As EventArgs) Handles tbMax.TextChanged
        MaxX = ConvertDisplayedToMol(tbMax.Text.ToDoubleFromCurrent)
    End Sub

    Private Sub cbXAxisBasis_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbXAxisBasis.SelectedIndexChanged
        tbMin.Text = ConvertMolToDisplay(MinX)
        tbMax.Text = ConvertMolToDisplay(MaxX)
    End Sub

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

End Class