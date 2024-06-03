'    Data Regression Utility
'    Copyright 2012 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.DrawingTools
Imports DWSIM.MathOps.MathEx
Imports System.Math
Imports ZedGraph
Imports DotNumerics
Imports Cureos.Numerics
Imports DWSIM.DWSIM.Optimization.DatRegression
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Threading.Tasks
Imports System.Linq
Imports System.IO
Imports DWSIM.Thermodynamics.Databases.KDBLink
Imports DWSIM.Simulate365.Models
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class FormDataRegression

    Inherits Form

    Public fmin As Double
    Public info As Integer
    Public cancel As Boolean = False

    Public finalval2() As Double = Nothing
    Public itn As Integer = 0

    Private _penval As Double = 0
    Private forceclose As Boolean = False

    Public currcase As RegressionCase
    Public IP As InteractionParameter
    Public simulate365File As S365File = Nothing

    Public proppack As PropertyPackage
    Public ppname As String = ""

    Public regressedparameters As New Dictionary(Of String, Double)
    Public A12, B12, C12, A21, B21, C21 As Double
    Public drawtdep As Boolean = False
    Public output As Boolean = True

    Dim doparallel As Boolean = False
    Dim dogpu As Boolean = False

    Public Sub FormDataRegression_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.MenuStrip1.Visible = Not GlobalSettings.Settings.OldUI

        IP = New InteractionParameter

        'get list of compounds
        Dim compounds As New ArrayList
        For Each c As ConstantProperties In FormMain.AvailableComponents.Values
            compounds.Add(c.Name)
        Next

        compounds.Sort()

        Me.cbCompound1.Items.AddRange(compounds.ToArray())
        Me.cbCompound2.Items.AddRange(compounds.ToArray())

        LoadCase(New RegressionCase, True)

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub FormDataRegression_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        If Not forceclose Then
            Dim x = MessageBox.Show(DWSIM.App.GetLocalString("Desejasalvarasaltera"), DWSIM.App.GetLocalString("Fechando") & " " & Me.Text, MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)

            If x = MsgBoxResult.Yes Then

                Call FormMain.SaveFileDialog()
                forceclose = True
                Me.Close()

            ElseIf x = MsgBoxResult.Cancel Then

                e.Cancel = True

            Else

                forceclose = True
                Me.Close()

            End If
        End If

    End Sub

    Function StoreCase() As RegressionCase

        Dim mycase As New RegressionCase

        With mycase
            .comp1 = Me.cbCompound1.SelectedItem.ToString
            .comp2 = Me.cbCompound2.SelectedItem.ToString
            .model = Me.cbModel.SelectedItem.ToString
            .idealvapormodel = Me.chkIdealVaporPhase.Checked
            .useTLdata = Me.chkTL.Checked
            .useTSdata = Me.chkTS.Checked
            .datatype = Me.cbDataType.SelectedIndex
            .method = Me.cbRegMethod.SelectedItem.ToString
            .objfunction = Me.cbObjFunc.SelectedItem.ToString
            .tunit = Me.cbTunit.SelectedItem.ToString
            .punit = Me.cbPunit.SelectedItem.ToString
            .title = tbTitle.Text
            .description = tbDescription.Text
            .results = tbRegResults.Text
            .filename = currcase.filename
            For Each r As DataGridViewRow In Me.GridExpData.Rows
                If r.Index < Me.GridExpData.Rows.Count - 1 Then
                    If r.Cells("check").Value Then .checkp.Add(True) Else .checkp.Add(False)
                    If Double.TryParse(r.Cells("colx1").Value, New Double) Then .x1p.Add(Double.Parse(r.Cells("colx1").Value)) Else .x1p.Add(0.0#)
                    If Double.TryParse(r.Cells("colx2").Value, New Double) Then .x2p.Add(Double.Parse(r.Cells("colx2").Value)) Else .x2p.Add(0.0#)
                    If Double.TryParse(r.Cells("coly1").Value, New Double) Then .yp.Add(Double.Parse(r.Cells("coly1").Value)) Else .yp.Add(0.0#)
                    If Double.TryParse(r.Cells("colt").Value, New Double) Then .tp.Add(Double.Parse(r.Cells("colt").Value)) Else .tp.Add(0.0#)
                    If Double.TryParse(r.Cells("colp").Value, New Double) Then .pp.Add(Double.Parse(r.Cells("colp").Value)) Else .pp.Add(0.0#)
                    Try
                        If Double.TryParse(r.Cells("colts").Value, New Double) Then .ts.Add(Double.Parse(r.Cells("colts").Value)) Else .ts.Add(0.0#)
                        If Double.TryParse(r.Cells("coltl").Value, New Double) Then .tl.Add(Double.Parse(r.Cells("coltl").Value)) Else .tl.Add(0.0#)
                    Catch ex As Exception
                        .ts.Add(0.0#)
                        .tl.Add(0.0#)
                    End Try
                End If
            Next
            Select Case cbModel.SelectedItem.ToString()
                Case "Peng-Robinson", "Soave-Redlich-Kwong"
                    .iepar1 = gridInEst.Rows(0).Cells(2).Value
                    .fixed1 = gridInEst.Rows(0).Cells(4).Value
                    .llim1 = gridInEst.Rows(0).Cells(1).Value
                    .ulim1 = gridInEst.Rows(0).Cells(3).Value
                Case "Lee-Kesler-Plöcker"
                    .iepar1 = gridInEst.Rows(0).Cells(2).Value
                    .fixed1 = gridInEst.Rows(0).Cells(4).Value
                    .llim1 = gridInEst.Rows(0).Cells(1).Value
                    .ulim1 = gridInEst.Rows(0).Cells(3).Value
                Case "UNIQUAC", "PRSV2-M", "PRSV2-VL", "Wilson"
                    .iepar1 = gridInEst.Rows(0).Cells(2).Value
                    .iepar2 = gridInEst.Rows(1).Cells(2).Value
                    .fixed1 = gridInEst.Rows(0).Cells(4).Value
                    .fixed2 = gridInEst.Rows(1).Cells(4).Value
                    .llim1 = gridInEst.Rows(0).Cells(1).Value
                    .ulim1 = gridInEst.Rows(0).Cells(3).Value
                    .llim2 = gridInEst.Rows(1).Cells(1).Value
                    .ulim2 = gridInEst.Rows(1).Cells(3).Value
                Case "NRTL"
                    .iepar1 = gridInEst.Rows(0).Cells(2).Value
                    .iepar2 = gridInEst.Rows(1).Cells(2).Value
                    .iepar3 = gridInEst.Rows(2).Cells(2).Value
                    .fixed1 = gridInEst.Rows(0).Cells(4).Value
                    .fixed2 = gridInEst.Rows(1).Cells(4).Value
                    .fixed3 = gridInEst.Rows(2).Cells(4).Value
                    .llim1 = gridInEst.Rows(0).Cells(1).Value
                    .ulim1 = gridInEst.Rows(0).Cells(3).Value
                    .llim2 = gridInEst.Rows(1).Cells(1).Value
                    .ulim2 = gridInEst.Rows(1).Cells(3).Value
                    .llim3 = gridInEst.Rows(2).Cells(1).Value
                    .ulim3 = gridInEst.Rows(2).Cells(3).Value
            End Select

        End With

        Return mycase

    End Function

    Sub LoadCase(ByVal mycase As RegressionCase, ByVal first As Boolean)

        With mycase
            If .comp1 IsNot Nothing Then
                If Me.cbCompound1.Items.Contains(.comp1) Then Me.cbCompound1.SelectedItem = .comp1 Else MessageBox.Show(.comp1 & ": " & DWSIM.App.GetLocalString("CompoundNotFound"))
            End If
            If .comp2 IsNot Nothing Then
                If Me.cbCompound2.Items.Contains(.comp2) Then Me.cbCompound2.SelectedItem = .comp2 Else MessageBox.Show(.comp2 & ": " & DWSIM.App.GetLocalString("CompoundNotFound"))
            End If
            If .model.ToLower = "prsv2" Then .model = "PRSV2-M"
            If .model = "PC-SAFT" Then .model = "Peng-Robinson"
            Me.cbModel.SelectedItem = .model
            Select Case cbModel.SelectedItem.ToString()
                Case "Peng-Robinson", "Soave-Redlich-Kwong"
                    gridInEst.Rows.Clear()
                    If .llim1 = 0.0# Then .llim1 = -0.5#
                    If .ulim1 = 0.0# Then .ulim1 = 0.5#
                    gridInEst.Rows.Add(New Object() {"kij", .llim1, .iepar1, .ulim1, .fixed1})
                Case "Lee-Kesler-Plöcker"
                    gridInEst.Rows.Clear()
                    If .llim1 = 0.0# Then .llim1 = 0.5#
                    If .ulim1 = 0.0# Then .ulim1 = 1.5#
                    gridInEst.Rows.Add(New Object() {"kij", .llim1, .iepar1, .ulim1, .fixed1})
                Case "UNIQUAC", "Wilson"
                    gridInEst.Rows.Clear()
                    If .llim1 = 0.0# Then .llim1 = -5000.0#
                    If .ulim1 = 0.0# Then .ulim1 = 5000.0#
                    If .llim2 = 0.0# Then .llim2 = -5000.0#
                    If .ulim2 = 0.0# Then .ulim2 = 5000.0#
                    gridInEst.Rows.Add(New Object() {"A12 (cal/mol)", .llim1, .iepar1, .ulim1, .fixed1})
                    gridInEst.Rows.Add(New Object() {"A21 (cal/mol)", .llim2, .iepar2, .ulim2, .fixed2})
                Case "PRSV2-M", "PRSV2-VL"
                    gridInEst.Rows.Clear()
                    If .llim1 = 0.0# Then .llim1 = -0.5#
                    If .ulim1 = 0.0# Then .ulim1 = 0.5#
                    If .llim2 = 0.0# Then .llim2 = -0.5#
                    If .ulim2 = 0.0# Then .ulim2 = 0.5#
                    gridInEst.Rows.Add(New Object() {"kij", .llim1, .iepar1, .ulim1, .fixed1})
                    gridInEst.Rows.Add(New Object() {"kji", .llim2, .iepar2, .ulim2, .fixed2})
                Case "NRTL"
                    gridInEst.Rows.Clear()
                    If .llim1 = 0.0# Then .llim1 = -5000.0#
                    If .ulim1 = 0.0# Then .ulim1 = 5000.0#
                    If .llim2 = 0.0# Then .llim2 = -5000.0#
                    If .ulim2 = 0.0# Then .ulim2 = 5000.0#
                    If .llim3 = 0.0# Then .llim3 = 0.0#
                    If .ulim3 = 0.0# Then .ulim3 = 0.8#
                    gridInEst.Rows.Add(New Object() {"A12 (cal/mol)", .llim1, .iepar1, .ulim1, .fixed1})
                    gridInEst.Rows.Add(New Object() {"A21 (cal/mol)", .llim2, .iepar2, .ulim2, .fixed2})
                    gridInEst.Rows.Add(New Object() {"alpha12", .llim3, .iepar3, .ulim3, .fixed3})
            End Select

            Me.chkTL.Checked = .useTLdata
            Me.chkTS.Checked = .useTSdata
            Me.chkIdealVaporPhase.Checked = .idealvapormodel
            Me.cbDataType.SelectedIndex = .datatype
            Try
                Me.cbRegMethod.SelectedItem = .method
            Catch ex As Exception
                Me.cbRegMethod.SelectedIndex = 0
            End Try
            Me.cbObjFunc.SelectedItem = .objfunction
            Me.cbTunit.SelectedItem = .tunit
            Me.cbPunit.SelectedItem = .punit
            Me.tbTitle.Text = .title
            Me.tbDescription.Text = .description

            Dim val0 As Boolean, val1, val2, val3, val4, val5, val6, val7 As String, i As Integer

            If .checkp Is Nothing Then .checkp = New ArrayList

            For i = 0 To .x1p.Count - 1
                If .checkp.Count - 1 >= i Then val0 = .checkp(i) Else val0 = True
                If Double.TryParse(.x1p(i), New Double) Then val1 = Double.Parse(.x1p(i)).ToString() Else val1 = ""
                If Double.TryParse(.x2p(i), New Double) Then val2 = Double.Parse(.x2p(i)).ToString() Else val2 = ""
                If Double.TryParse(.yp(i), New Double) Then val3 = Double.Parse(.yp(i)).ToString() Else val3 = ""
                If Double.TryParse(.tp(i), New Double) Then val4 = Double.Parse(.tp(i)).ToString() Else val4 = ""
                If Double.TryParse(.pp(i), New Double) Then val5 = Double.Parse(.pp(i)).ToString() Else val5 = ""
                Try
                    If Double.TryParse(.ts(i), New Double) Then val6 = Double.Parse(.ts(i)).ToString() Else val6 = ""
                    If Double.TryParse(.tl(i), New Double) Then val7 = Double.Parse(.tl(i)).ToString() Else val7 = ""
                Catch ex As Exception
                    val6 = ""
                    val7 = ""
                End Try
                Me.GridExpData.Rows.Add(val0, val1, val2, val3, val4, val7, val6, val5)
            Next
            Me.tbRegResults.Text = .results
            currcase = mycase
            If Not first Then UpdateData()
        End With

    End Sub

    Private Sub cbDataType_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbDataType.SelectedIndexChanged
        Me.chkTL.Enabled = False
        Me.chkTS.Enabled = False
        Select Case cbDataType.SelectedIndex
            Case 0
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = False
                Me.GridExpData.Columns("coly1").Visible = True
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 1
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = False
                Me.GridExpData.Columns("coly1").Visible = True
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 2
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = False
                Me.GridExpData.Columns("coly1").Visible = True
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 3
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = True
                Me.GridExpData.Columns("coly1").Visible = False
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 4
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = True
                Me.GridExpData.Columns("coly1").Visible = False
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 5
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = True
                Me.GridExpData.Columns("coly1").Visible = False
                Me.GridExpData.Columns("colT").Visible = True
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = False
                Me.GridExpData.Columns("colts").Visible = False
                cbObjFunc.Enabled = True
            Case 6, 7
                Me.GridExpData.Columns("colx1").Visible = True
                Me.GridExpData.Columns("colx2").Visible = False
                Me.GridExpData.Columns("coly1").Visible = False
                Me.GridExpData.Columns("colT").Visible = False
                Me.GridExpData.Columns("colP").Visible = True
                Me.GridExpData.Columns("coltl").Visible = True
                Me.GridExpData.Columns("colts").Visible = True
                cbObjFunc.Enabled = True
                Me.chkTL.Enabled = True
                Me.chkTS.Enabled = True
        End Select
    End Sub

    Public Function FunctionValue(ByVal x() As Double) As Double

        Application.DoEvents()
        If cancel Then Exit Function

        Dim poptions As New ParallelOptions()

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
            poptions.MaxDegreeOfParallelism = My.Settings.MaxDegreeOfParallelism
        Else
            doparallel = True
            poptions.MaxDegreeOfParallelism = 4
        End If

        Dim Vx1, Vx2, Vy As New ArrayList, IP(x.Length - 1, x.Length) As Double
        Dim Vx1c, Vx2c, Vyc As New ArrayList
        Dim VP, VT, VPc, VTc, VTL, VTS, VTLc, VTSc As New ArrayList
        Dim np As Integer = 0
        Dim i As Integer = 0

        For Each b As Boolean In currcase.checkp
            If b Then np += 1
        Next

        For i = 0 To np - 1
            Vx1c.Add(0.0#)
            Vx2c.Add(0.0#)
            Vyc.Add(0.0#)
            VPc.Add(0.0#)
            VTc.Add(0.0#)
            VTLc.Add(0.0#)
            VTSc.Add(0.0#)
        Next

        Dim PVF As Boolean = False

        Select Case currcase.datatype
            Case DataType.Pxy
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vy.Add(currcase.yp(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(i)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(0)))
                    End If
                    i += 1
                Next
            Case DataType.Txy
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vy.Add(currcase.yp(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(0)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(i)))
                    End If
                    i += 1
                Next
                PVF = True
            Case DataType.TPxy
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vy.Add(currcase.yp(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(i)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(i)))
                    End If
                    i += 1
                Next
            Case DataType.Pxx
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vx2.Add(currcase.x2p(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(i)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(0)))
                    End If
                    i += 1
                Next
            Case DataType.Txx
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vx2.Add(currcase.x2p(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(0)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(i)))
                    End If
                    i += 1
                Next
            Case DataType.TPxx
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        Vx2.Add(currcase.x2p(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(i)))
                        VT.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tp(i)))
                    End If
                    i += 1
                Next
            Case DataType.TTxSE, DataType.TTxSS
                i = 0
                For Each b As Boolean In currcase.checkp
                    If b Then
                        Vx1.Add(currcase.x1p(i))
                        VP.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.punit, currcase.pp(0)))
                        VTL.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.tl(i)))
                        VTS.Add(SystemsOfUnits.Converter.ConvertToSI(currcase.tunit, currcase.ts(i)))
                    End If
                    i += 1
                Next
        End Select

        Dim f As Double = 0.0#
        Dim result As Object = Nothing
        Dim vartext As String = ""

        regressedparameters.Clear()

        Try

            Me.currcase.calcp.Clear()
            Me.currcase.calct.Clear()
            Me.currcase.calcy.Clear()
            Me.currcase.calcx1l1.Clear()
            Me.currcase.calcx1l2.Clear()
            Me.currcase.calctl.Clear()
            Me.currcase.calcts.Clear()

            Select Case currcase.datatype
                Case DataType.Pxy, DataType.Txy
                    Select Case currcase.model
                        Case "Peng-Robinson", "Soave-Redlich-Kwong", "Lee-Kesler-Plöcker"
                            If PVF Then
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubT(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), VT(ipar))
                                                                                     VTc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(proppack, 2, VP(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VTc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            Else
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubP(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VT(0), VP(ipar))
                                                                                     VPc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(proppack, 2, VT(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VPc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            End If
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                        Case "UNIQUAC"
                            If PVF Then
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubT(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), VT(ipar))
                                                                                     VTc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(proppack, 2, VP(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VTc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            Else
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                             Sub(ipar)
                                                                                 Dim result2 As Object
                                                                                 result2 = proppack.DW_CalcBubP(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VT(0), VP(ipar))
                                                                                 VPc(ipar) = result2(4)
                                                                                 Vyc(ipar) = result2(3)(0)
                                                                             End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(proppack, 1, VT(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VPc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "Wilson"
                            If PVF Then
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                Dim casids = proppack.RET_VCAS()
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {casids(0), casids(1)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubT(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), VT(ipar))
                                                                                     VTc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(proppack, 2, VP(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VTc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            Else
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                Dim casids = proppack.RET_VCAS()
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {casids(0), casids(1)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                             Sub(ipar)
                                                                                 Dim result2 As Object
                                                                                 result2 = proppack.DW_CalcBubP(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VT(0), VP(ipar))
                                                                                 VPc(ipar) = result2(4)
                                                                                 Vyc(ipar) = result2(3)(0)
                                                                             End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(proppack, 1, VT(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VPc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "PRSV2-M", "PRSV2-VL"
                            proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            If PVF Then
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubT(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), VT(ipar))
                                                                                     VTc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(proppack, 2, VP(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VTc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            Else
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubP(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VT(0), VP(ipar))
                                                                                     VPc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(proppack, 2, VT(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                        VPc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            End If
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4") & ", "
                            vartext += "kji = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                            regressedparameters.Add("kji", x(1))
                        Case "NRTL"
                            proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                            If PVF Then
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                            Sub(ipar)
                                                                                                Dim result2 As Object
                                                                                                result2 = proppack.DW_CalcBubT(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), VT(ipar))
                                                                                                VTc(ipar) = result2(4)
                                                                                                Vyc(ipar) = result2(3)(0)
                                                                                            End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(proppack, 2, VP(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                                        VTc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            Else
                                proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoops
                                ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                                If doparallel Then

                                    Try
                                        Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                 Sub(ipar)
                                                                                     Dim result2 As Object
                                                                                     result2 = proppack.DW_CalcBubP(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VT(0), VP(ipar))
                                                                                     VPc(ipar) = result2(4)
                                                                                     Vyc(ipar) = result2(3)(0)
                                                                                 End Sub))
                                        task1.Wait()
                                    Catch ae As AggregateException
                                        Throw ae.Flatten().InnerException
                                    End Try

                                Else
                                    For i = 0 To np - 1
                                        result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(proppack, 2, VT(0), 0.0#, New Object() {currcase.comp1, currcase.comp2}, New Double() {Vx1(i), 1 - Vx1(i)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                                        VPc(i) = result(4, 0)
                                        Vyc(i) = result(2, 0)
                                    Next
                                End If
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4") & ", "
                            vartext += "alpha12 = " & x(2).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                            regressedparameters.Add("alpha12", x(2))
                    End Select
                    For i = 0 To np - 1
                        Me.currcase.calct.Add(VTc(i))
                        Me.currcase.calcp.Add(VPc(i))
                        Me.currcase.calcy.Add(Vyc(i))
                        Me.currcase.calcx1l1.Add(0.0#)
                        Me.currcase.calcx1l2.Add(0.0#)
                        Me.currcase.calctl.Add(0.0#)
                        Me.currcase.calcts.Add(0.0#)
                        Select Case currcase.objfunction
                            Case "Least Squares (min T/P+y/x)"
                                If PVF Then
                                    f += (VTc(i) - VT(i)) ^ 2 + ((Vyc(i) - Vy(i))) ^ 2
                                Else
                                    f += (VPc(i) - VP(i)) ^ 2 + ((Vyc(i) - Vy(i))) ^ 2
                                End If
                            Case "Least Squares (min T/P)"
                                If PVF Then
                                    f += (VTc(i) - VT(i)) ^ 2
                                Else
                                    f += (VPc(i) - VP(i)) ^ 2
                                End If
                            Case "Least Squares (min y/x)"
                                If PVF Then
                                    f += ((Vyc(i) - Vy(i))) ^ 2
                                Else
                                    f += ((Vyc(i) - Vy(i))) ^ 2
                                End If
                            Case "Weighted Least Squares (min T/P+y/x)"
                                If PVF Then
                                    f += ((VTc(i) - VT(i)) / VT(i)) ^ 2 + (((Vyc(i) - Vy(i))) / Vy(i)) ^ 2
                                Else
                                    f += ((VPc(i) - VP(i)) / VP(i)) ^ 2 + (((Vyc(i) - Vy(i))) / Vy(i)) ^ 2
                                End If
                            Case "Weighted Least Squares (min T/P)"
                                If PVF Then
                                    f += ((VTc(i) - VT(i)) / VT(i)) ^ 2
                                Else
                                    f += ((VPc(i) - VP(i)) / VP(i)) ^ 2
                                End If
                            Case "Weighted Least Squares (min y/x)"
                                If PVF Then
                                    f += ((Vyc(i) - Vy(i)) / Vy(i)) ^ 2
                                Else
                                    f += ((Vyc(i) - Vy(i)) / Vy(i)) ^ 2
                                End If
                            Case "Chi Square"
                        End Select
                    Next
                Case DataType.TPxy
                Case DataType.Pxx, DataType.Txx
                    proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.SimpleLLE
                    Dim flashinstance As PropertyPackages.Auxiliary.FlashAlgorithms.SimpleLLE = TryCast(proppack.FlashBase, PropertyPackages.Auxiliary.FlashAlgorithms.SimpleLLE)
                    If Not flashinstance Is Nothing Then
                        With flashinstance
                            .UseInitialEstimatesForPhase1 = True
                            .UseInitialEstimatesForPhase2 = True
                        End With
                    End If
                    Select Case currcase.model
                        Case "PRSV2-M", "PRSV2-VL"
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            For i = 0 To np - 1
                                With flashinstance
                                    .InitialEstimatesForPhase1 = New Double() {Vx1(i), 1 - Vx1(i)}
                                    .InitialEstimatesForPhase2 = New Double() {Vx2(i), 1 - Vx2(i)}
                                End With
                                result = proppack.FlashBase.Flash_PT(New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, VP(0), VT(i), proppack)
                                Vx1c(i) = result(2)(0)
                                Vx2c(i) = result(6)(0)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4") & ", "
                            vartext += "kji = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                            regressedparameters.Add("kji", x(1))
                        Case "UNIQUAC"
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            If drawtdep Then
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, A12}, {A21, 0.0#}}, New Double(,) {{0.0#, A21}, {A12, 0.0#}}, New Double(,) {{0.0#, B12}, {B21, 0.0#}}, New Double(,) {{0.0#, B21}, {B12, 0.0#}}, New Double(,) {{0.0#, C12}, {C21, 0.0#}}, New Double(,) {{0.0#, C21}, {C12, 0.0#}}, Nothing)
                            Else
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                            End If
                            For i = 0 To np - 1
                                With flashinstance
                                    .InitialEstimatesForPhase1 = New Double() {Vx1(i), 1 - Vx1(i)}
                                    .InitialEstimatesForPhase2 = New Double() {Vx2(i), 1 - Vx2(i)}
                                End With
                                result = proppack.FlashBase.Flash_PT(New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, VP(0), VT(i), proppack)
                                Vx1c(i) = result(2)(0)
                                Vx2c(i) = result(6)(0)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "Wilson"
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            Dim casids = proppack.RET_VCAS()
                            If drawtdep Then
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {casids(0), casids(1)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, A12}, {A21, 0.0#}}, New Double(,) {{0.0#, A21}, {A12, 0.0#}}, New Double(,) {{0.0#, B12}, {B21, 0.0#}}, New Double(,) {{0.0#, B21}, {B12, 0.0#}}, New Double(,) {{0.0#, C12}, {C21, 0.0#}}, New Double(,) {{0.0#, C21}, {C12, 0.0#}}, Nothing)
                            Else
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {casids(0), casids(1)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                            End If
                            For i = 0 To np - 1
                                With flashinstance
                                    .InitialEstimatesForPhase1 = New Double() {Vx1(i), 1 - Vx1(i)}
                                    .InitialEstimatesForPhase2 = New Double() {Vx2(i), 1 - Vx2(i)}
                                End With
                                result = proppack.FlashBase.Flash_PT(New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, VP(0), VT(i), proppack)
                                Vx1c(i) = result(2)(0)
                                Vx2c(i) = result(6)(0)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "NRTL"
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            If drawtdep Then
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, A12}, {A21, 0.0#}}, New Double(,) {{0.0#, A21}, {A12, 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, New Double(,) {{0.0#, B12}, {B21, 0.0#}}, New Double(,) {{0.0#, B21}, {B12, 0.0#}}, New Double(,) {{0.0#, C12}, {C21, 0.0#}}, New Double(,) {{0.0#, C21}, {C12, 0.0#}})
                            Else
                                ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                            End If
                            For i = 0 To np - 1
                                With flashinstance
                                    .InitialEstimatesForPhase1 = New Double() {Vx1(i), 1 - Vx1(i)}
                                    .InitialEstimatesForPhase2 = New Double() {Vx2(i), 1 - Vx2(i)}
                                End With
                                result = proppack.FlashBase.Flash_PT(New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, VP(0), VT(i), proppack)
                                Vx1c(i) = result(2)(0)
                                Vx2c(i) = result(6)(0)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4") & ", "
                            vartext += "alpha12 = " & x(2).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                            regressedparameters.Add("alpha12", x(2))
                        Case "Lee-Kesler-Plöcker", "Peng-Robinson", "Soave-Redlich-Kwong"
                            ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            For i = 0 To np - 1
                                With flashinstance
                                    .InitialEstimatesForPhase1 = New Double() {Vx1(i), 1 - Vx1(i)}
                                    .InitialEstimatesForPhase2 = New Double() {Vx2(i), 1 - Vx2(i)}
                                End With
                                result = proppack.FlashBase.Flash_PT(New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, VP(0), VT(i), proppack)
                                Vx1c(i) = result(2)(0)
                                Vx2c(i) = result(6)(0)
                            Next
                            vartext = ", Parameters = {kij = "
                            For i = 0 To x.Length - 1
                                vartext += x(i).ToString("N4")
                            Next
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                    End Select
                    If Abs(Vx1(0) - Vx1c(0)) > Abs(Vx1(0) - Vx2c(0)) Then
                        Dim tmpvec As ArrayList = Vx1c.Clone
                        Vx1c = Vx2c.Clone
                        Vx2c = tmpvec
                    End If
                    For i = 0 To np - 1
                        Me.currcase.calcx1l1.Add(Vx1c(i))
                        Me.currcase.calcx1l2.Add(Vx2c(i))
                        Me.currcase.calct.Add(0.0#)
                        Me.currcase.calcp.Add(0.0#)
                        Me.currcase.calcy.Add(0.0#)
                        Me.currcase.calctl.Add(0.0#)
                        Me.currcase.calcts.Add(0.0#)
                        Select Case currcase.objfunction
                            Case "Least Squares (min T/P+y/x)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += (-Vx1c(i) + Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += (-Vx1c(i) + Vx1(i) - Vx2c(i) + Vx2(i)) ^ 2
                                End If
                            Case "Least Squares (min T/P)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += (-Vx1c(i) + Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += (-Vx1c(i) + Vx1(i) - Vx2c(i) + Vx2(i)) ^ 2
                                End If
                            Case "Least Squares (min y/x)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += (-Vx1c(i) + Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += (-Vx1c(i) + Vx1(i) - Vx2c(i) + Vx2(i)) ^ 2
                                End If
                            Case "Weighted Least Squares (min T/P+y/x)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                                End If
                            Case "Weighted Least Squares (min T/P)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                                End If
                            Case "Weighted Least Squares (min y/x)"
                                If Abs(Vx1c(i) - Vx2c(i)) < 0.001 Then
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 * 10000000000.0
                                Else
                                    f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                                End If
                            Case "Chi Square"
                        End Select
                    Next
                Case DataType.TPxx
                    Select Case currcase.model
                        Case "PRSV2-M"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4") & ", "
                            vartext += "kji = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                            regressedparameters.Add("kji", x(1))
                        Case "PRSV2-VL"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4") & ", "
                            vartext += "kji = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                            regressedparameters.Add("kji", x(1))
                        Case "UNIQUAC"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "Wilson"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "NRTL"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4") & ", "
                            vartext += "alpha12 = " & x(2).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                            regressedparameters.Add("alpha12", x(2))
                        Case "Lee-Kesler-Plöcker"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {kij = "
                            For i = 0 To x.Length - 1
                                vartext += x(i).ToString("N4")
                            Next
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                        Case "Peng-Robinson"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {kij = "
                            For i = 0 To x.Length - 1
                                vartext += x(i).ToString("N4")
                            Next
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                        Case "Soave-Redlich-Kwong"
                            For i = 0 To np - 1
                                result = ExcelAddIn.ExcelIntegrationNoAttr.PTFlash(proppack, 3, VP(i), VT(i), New Object() {currcase.comp1, currcase.comp2}, New Double() {(Vx1(i) + Vx2(i)) / 2, 1 - (Vx1(i) + Vx2(i)) / 2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                                Vx1c(i) = result(2, 1)
                                Vx2c(i) = result(2, 2)
                            Next
                            vartext = ", Parameters = {kij = "
                            For i = 0 To x.Length - 1
                                vartext += x(i).ToString("N4")
                            Next
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                    End Select
                    If Abs(Vx1(0) - Vx1c(0)) > Abs(Vx1(0) - Vx2c(0)) Then
                        Dim tmpvec As ArrayList = Vx1c.Clone
                        Vx1c = Vx2c.Clone
                        Vx2c = tmpvec
                    End If
                    For i = 0 To np - 1
                        Me.currcase.calcx1l1.Add(Vx1c(i))
                        Me.currcase.calcx1l2.Add(Vx2c(i))
                        Me.currcase.calct.Add(0.0#)
                        Me.currcase.calcp.Add(0.0#)
                        Me.currcase.calcy.Add(0.0#)
                        Me.currcase.calctl.Add(0.0#)
                        Me.currcase.calcts.Add(0.0#)
                        Select Case currcase.objfunction
                            Case "Least Squares (min T/P+y/x)"
                                f += ((Vx1c(i) - Vx1(i))) ^ 2 + ((Vx2c(i) - Vx2(i))) ^ 2
                            Case "Least Squares (min T/P)"
                                f += ((Vx1c(i) - Vx1(i))) ^ 2 + ((Vx2c(i) - Vx2(i))) ^ 2
                            Case "Least Squares (min y/x)"
                                f += ((Vx1c(i) - Vx1(i))) ^ 2 + ((Vx2c(i) - Vx2(i))) ^ 2
                            Case "Weighted Least Squares (min T/P+y/x)"
                                f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                            Case "Weighted Least Squares (min T/P)"
                                f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                            Case "Weighted Least Squares (min y/x)"
                                f += ((Vx1c(i) - Vx1(i)) / Vx1(i)) ^ 2 + ((Vx2c(i) - Vx2(i)) / Vx2(i)) ^ 2
                            Case "Chi Square"
                        End Select
                    Next
                Case DataType.TTxSE, DataType.TTxSS
                    If currcase.datatype = DataType.TTxSE Then
                        proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE
                    Else
                        proppack.FlashAlgorithm = New Auxiliary.FlashAlgorithms.NestedLoopsSLE With {.SolidSolution = True}
                    End If
                    ExcelAddIn.ExcelIntegrationNoAttr.AddCompounds(proppack, New Object() {currcase.comp1, currcase.comp2})
                    Select Case currcase.model
                        Case "Peng-Robinson", "Soave-Redlich-Kwong", "Lee-Kesler-Plöcker"
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            If doparallel Then

                                Try
                                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                                Sub(ipar)
                                                                                    Dim result2 As Object
                                                                                    If Me.currcase.useTLdata Then
                                                                                        result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.999, VTL(ipar), proppack)
                                                                                        VTLc(ipar) = result2(4)
                                                                                    Else
                                                                                        VTLc(ipar) = 0.0#
                                                                                    End If
                                                                                    If Me.currcase.useTSdata Then
                                                                                        result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.001, VTS(ipar), proppack)
                                                                                        VTSc(ipar) = result2(4)
                                                                                    Else
                                                                                        VTSc(ipar) = 0.0#
                                                                                    End If
                                                                                End Sub))
                                    task1.Wait()
                                Catch ae As AggregateException
                                    Throw ae.Flatten().InnerException
                                End Try

                            Else
                                For i = 0 To np - 1
                                    If Me.currcase.useTLdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.999, VTL(i), proppack)
                                        VTLc(i) = result(4)
                                    Else
                                        VTLc(i) = 0.0#
                                    End If
                                    If Me.currcase.useTSdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.001, VTS(i), proppack)
                                        VTSc(i) = result(4)
                                    Else
                                        VTSc(i) = 0.0#
                                    End If
                                    Application.DoEvents()
                                Next
                            End If
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                        Case "UNIQUAC"
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                            If doparallel Then

                                Try
                                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                         Sub(ipar)
                                                                             Dim result2 As Object
                                                                             If Me.currcase.useTLdata Then
                                                                                 result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.999, VTL(ipar), proppack)
                                                                                 VTLc(ipar) = result2(4)
                                                                             Else
                                                                                 VTLc(ipar) = 0.0#
                                                                             End If
                                                                             If Me.currcase.useTSdata Then
                                                                                 result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.001, VTS(ipar), proppack)
                                                                                 VTSc(ipar) = result2(4)
                                                                             Else
                                                                                 VTSc(ipar) = 0.0#
                                                                             End If
                                                                         End Sub))
                                    task1.Wait()
                                Catch ae As AggregateException
                                    Throw ae.Flatten().InnerException
                                End Try

                            Else
                                For i = 0 To np - 1
                                    If Me.currcase.useTLdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.999, VTL(i), proppack)
                                        VTLc(i) = result(4)
                                    Else
                                        VTLc(i) = 0.0#
                                    End If
                                    If Me.currcase.useTSdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.001, VTS(i), proppack)
                                        VTSc(i) = result(4)
                                    Else
                                        VTSc(i) = 0.0#
                                    End If
                                    Application.DoEvents()
                                Next
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "Wilson"
                            Dim casids = proppack.RET_VCAS()
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {casids(0), casids(1)}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing)
                            If doparallel Then

                                Try
                                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                         Sub(ipar)
                                                                             Dim result2 As Object
                                                                             If Me.currcase.useTLdata Then
                                                                                 result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.999, VTL(ipar), proppack)
                                                                                 VTLc(ipar) = result2(4)
                                                                             Else
                                                                                 VTLc(ipar) = 0.0#
                                                                             End If
                                                                             If Me.currcase.useTSdata Then
                                                                                 result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.001, VTS(ipar), proppack)
                                                                                 VTSc(ipar) = result2(4)
                                                                             Else
                                                                                 VTSc(ipar) = 0.0#
                                                                             End If
                                                                         End Sub))
                                    task1.Wait()
                                Catch ae As AggregateException
                                    Throw ae.Flatten().InnerException
                                End Try

                            Else
                                For i = 0 To np - 1
                                    If Me.currcase.useTLdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.999, VTL(i), proppack)
                                        VTLc(i) = result(4)
                                    Else
                                        VTLc(i) = 0.0#
                                    End If
                                    If Me.currcase.useTSdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.001, VTS(i), proppack)
                                        VTSc(i) = result(4)
                                    Else
                                        VTSc(i) = 0.0#
                                    End If
                                    Application.DoEvents()
                                Next
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                        Case "PRSV2-M", "PRSV2-VL"
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, x(0)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(1), 0.0#}}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            If doparallel Then

                                Try
                                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                             Sub(ipar)
                                                                                 Dim result2 As Object
                                                                                 If Me.currcase.useTLdata Then
                                                                                     result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.999, VTL(ipar), proppack)
                                                                                     VTLc(ipar) = result2(4)
                                                                                 Else
                                                                                     VTLc(ipar) = 0.0#
                                                                                 End If
                                                                                 If Me.currcase.useTSdata Then
                                                                                     result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.001, VTS(ipar), proppack)
                                                                                     VTSc(ipar) = result2(4)
                                                                                 Else
                                                                                     VTSc(ipar) = 0.0#
                                                                                 End If
                                                                             End Sub))
                                    task1.Wait()
                                Catch ae As AggregateException
                                    Throw ae.Flatten().InnerException
                                End Try

                            Else
                                For i = 0 To np - 1
                                    If Me.currcase.useTLdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.999, VTL(i), proppack)
                                        VTLc(i) = result(4)
                                    Else
                                        VTLc(i) = 0.0#
                                    End If
                                    If Me.currcase.useTSdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.001, VTS(i), proppack)
                                        VTSc(i) = result(4)
                                    Else
                                        VTSc(i) = 0.0#
                                    End If
                                    Application.DoEvents()
                                Next
                            End If
                            vartext = ", Parameters = {"
                            vartext += "kij = " & x(0).ToString("N4") & ", "
                            vartext += "kji = " & x(1).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("kij", x(0))
                            regressedparameters.Add("kji", x(1))
                        Case "NRTL"
                            ExcelAddIn.ExcelIntegrationNoAttr.SetIP(proppack.ComponentName, proppack, New Object() {currcase.comp1, currcase.comp2}, New Double(,) {{0.0#, 0.0#}, {0.0#, 0.0#}}, New Double(,) {{0.0#, x(0)}, {x(1), 0.0#}}, New Double(,) {{0.0#, x(1)}, {x(0), 0.0#}}, New Double(,) {{0.0#, x(2)}, {x(2), 0.0#}}, Nothing, Nothing, Nothing, Nothing)
                            If doparallel Then

                                Try
                                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, np, poptions,
                                                                             Sub(ipar)
                                                                                 Dim result2 As Object
                                                                                 If Me.currcase.useTLdata Then
                                                                                     result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.999, VTL(ipar), proppack)
                                                                                     VTLc(ipar) = result2(4)
                                                                                 Else
                                                                                     VTLc(ipar) = 0.0#
                                                                                 End If
                                                                                 If Me.currcase.useTSdata Then
                                                                                     result2 = proppack.FlashBase.Flash_PV(New Double() {Vx1(ipar), 1 - Vx1(ipar)}, VP(0), 0.001, VTS(ipar), proppack)
                                                                                     VTSc(ipar) = result2(4)
                                                                                 Else
                                                                                     VTSc(ipar) = 0.0#
                                                                                 End If
                                                                             End Sub))
                                    task1.Wait()
                                Catch ae As AggregateException
                                    Throw ae.Flatten().InnerException
                                End Try

                            Else
                                For i = 0 To np - 1
                                    If Me.currcase.useTLdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.999, VTL(i), proppack)
                                        VTLc(i) = result(4)
                                    Else
                                        VTLc(i) = 0.0#
                                    End If
                                    If Me.currcase.useTSdata Then
                                        result = proppack.FlashBase.Flash_PV(New Double() {Vx1(i), 1 - Vx1(i)}, VP(0), 0.001, VTS(i), proppack)
                                        VTSc(i) = result(4)
                                    Else
                                        VTSc(i) = 0.0#
                                    End If
                                    Application.DoEvents()
                                Next
                            End If
                            vartext = ", Parameters = {"
                            vartext += "A12 = " & x(0).ToString("N4") & ", "
                            vartext += "A21 = " & x(1).ToString("N4") & ", "
                            vartext += "alpha12 = " & x(2).ToString("N4")
                            vartext += "}"
                            regressedparameters.Add("A12", x(0))
                            regressedparameters.Add("A21", x(1))
                            regressedparameters.Add("alpha12", x(2))
                    End Select
                    For i = 0 To np - 1
                        Me.currcase.calct.Add(VTc(i))
                        Me.currcase.calctl.Add(VTLc(i))
                        Me.currcase.calcts.Add(VTSc(i))
                        Me.currcase.calcp.Add(VPc(i))
                        Me.currcase.calcy.Add(Vyc(i))
                        Me.currcase.calcx1l1.Add(Vx1c(i))
                        Me.currcase.calcx1l2.Add(Vx2c(i))
                        If Me.currcase.useTLdata Then
                            f += (VTLc(i) - VTL(i)) ^ 2
                        End If
                        If Me.currcase.useTSdata Then
                            f += (VTSc(i) - VTS(i)) ^ 2
                        End If
                    Next
            End Select

            itn += 1
            If output Then Me.tbRegResults.AppendText("Iteration #" & itn & ", Function Value = " & Format(f, "E") & vartext & vbCrLf)

            UpdateData()
            Application.DoEvents()

        Catch ex As Exception

            itn += 1
            Me.tbRegResults.AppendText("Iteration #" & itn & ", Exception: " & ex.Message.ToString & vbCrLf)

            f = Double.MaxValue

            Console.WriteLine(ex.ToString)

        End Try

        Return f

    End Function

    Public Function FunctionGradient(ByVal x() As Double) As Double()

        Application.DoEvents()
        If cancel Then
            Return x
            Exit Function
        End If

        Dim g(x.Length - 1) As Double

        Dim epsilon As Double = 0.01

        Dim f2(x.Length - 1), f3(x.Length - 1) As Double
        Dim x2(x.Length - 1), x3(x.Length - 1) As Double
        Dim i, j As Integer

        For i = 0 To x.Length - 1
            For j = 0 To x.Length - 1
                x2(j) = x(j)
                x3(j) = x(j)
            Next
            If x(i) <> 0.0# Then
                x2(i) = x(i) * (1 + epsilon)
                x3(i) = x(i) * (1 - epsilon)
            Else
                x2(i) = x(i) + epsilon / 1000
                x3(i) = x(i) - epsilon / 1000
            End If
            f2(i) = FunctionValue(x2)
            f3(i) = FunctionValue(x3)
            g(i) = (f2(i) - f3(i)) / (x2(i) - x3(i))
        Next

        Return g

    End Function

    Private Function GetSolver(solver As String) As SwarmOps.Optimizer

        Select Case solver
            Case "Differential Evolution"
                Return New SwarmOps.Optimizers.DE()
            Case "Gradient Descent"
                Return New SwarmOps.Optimizers.GD()
            Case "Local Unimodal Sampling"
                Return New SwarmOps.Optimizers.LUS()
            Case "Many Optimizing Liaisons"
                Return New SwarmOps.Optimizers.MOL()
            Case "Mesh"
                Return New SwarmOps.Optimizers.MESH()
            Case "Particle Swarm"
                Return New SwarmOps.Optimizers.PS()
            Case "Particle Swarm Optimization"
                Return New SwarmOps.Optimizers.PSO()
            Case Else
                Return Nothing
        End Select

    End Function

    'IPOPT

    Public Function eval_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef obj_value As Double) As Boolean
        Dim fval As Double = FunctionValue(x)
        obj_value = fval
        Return True
    End Function

    Public Function eval_grad_f(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByRef grad_f As Double()) As Boolean
        Dim g As Double() = FunctionGradient(x)
        grad_f = g
        Return True
    End Function

    Public Function eval_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByRef g As Double()) As Boolean
        'g(0) = x(0) * x(1) * x(2) * x(3)
        'g(1) = x(0) * x(0) + x(1) * x(1) + x(2) * x(2) + x(3) * x(3)
        Return True
    End Function

    Public Function eval_jac_g(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal m As Integer, ByVal nele_jac As Integer, ByRef iRow As Integer(),
ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        If values Is Nothing Then
            ' set the structure of the jacobian 
            ' this particular jacobian is dense 
            'iRow(0) = 0
            'jCol(0) = 0
            'iRow(1) = 0
            'jCol(1) = 1
            'iRow(2) = 0
            'jCol(2) = 2
            'iRow(3) = 0
            'jCol(3) = 3
            'iRow(4) = 1
            'jCol(4) = 0
            'iRow(5) = 1
            'jCol(5) = 1
            'iRow(6) = 1
            'jCol(6) = 2
            'iRow(7) = 1
            'jCol(7) = 3
        Else
            '' return the values of the jacobian of the constraints 
            'values(0) = x(1) * x(2) * x(3)  ' 0,0 
            'values(1) = x(0) * x(2) * x(3)  ' 0,1 
            'values(2) = x(0) * x(1) * x(3)  ' 0,2 
            'values(3) = x(0) * x(1) * x(2)  ' 0,3 

            'values(4) = 2 * x(0)            ' 1,0 
            'values(5) = 2 * x(1)            ' 1,1 
            'values(6) = 2 * x(2)            ' 1,2 
            'values(7) = 2 * x(3)            ' 1,3 
        End If

        Return False
    End Function

    Public Function eval_h(ByVal n As Integer, ByVal x As Double(), ByVal new_x As Boolean, ByVal obj_factor As Double, ByVal m As Integer, ByVal lambda As Double(),
ByVal new_lambda As Boolean, ByVal nele_hess As Integer, ByRef iRow As Integer(), ByRef jCol As Integer(), ByRef values As Double()) As Boolean
        Return False
    End Function

    Private Sub btnDoReg_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDoReg.Click

        Me.tbRegResults.Clear()

        cancel = False

        Me.btnDoReg.Enabled = False
        Me.btnCalcOnce.Enabled = False
        Me.btnCancel.Enabled = True

        Try

            currcase = Me.StoreCase()

            Dim initval As Double() = Nothing

            Select Case currcase.model
                Case "Peng-Robinson"
                    initval = New Double() {currcase.iepar1}
                Case "PRSV2-M", "PRSV2-VL"
                    initval = New Double() {currcase.iepar1, currcase.iepar2}
                Case "Soave-Redlich-Kwong"
                    initval = New Double() {currcase.iepar1}
                Case "UNIQUAC", "Wilson"
                    initval = New Double() {currcase.iepar1, currcase.iepar2}
                Case "NRTL"
                    initval = New Double() {currcase.iepar1, currcase.iepar2, currcase.iepar3}
                Case "Lee-Kesler-Plöcker"
                    initval = New Double() {currcase.iepar1}
            End Select

            drawtdep = False
            output = True

            If Not chkDoTDepRegression.Checked Then

                DoRegression(initval)

                Dim k As Integer
                IP.Parameters.Clear()
                For k = 0 To regressedparameters.Count - 1
                    IP.Parameters(regressedparameters.Keys(k)) = regressedparameters.Values(k)
                Next

            Else

                'get initial list of included parameters.

                Dim toinclude As New ArrayList

                For Each b As Boolean In currcase.checkp
                    toinclude.Add(b)
                Next

                'do regression for each checked point one at a time

                Dim regpars As New ArrayList
                Dim j As Integer = 0

                tbRegResults.AppendText("Starting temperature-dependent regression for selected data points...")
                tbRegResults.AppendText(vbCrLf)

                For Each b As Boolean In toinclude
                    For i As Integer = 0 To currcase.checkp.Count - 1
                        currcase.checkp(i) = False
                    Next
                    currcase.checkp(j) = b
                    tbRegResults.AppendText("Regressing parameters for data set #" & j + 1 & "...")
                    tbRegResults.AppendText(vbCrLf)
                    tbRegResults.AppendText(vbCrLf)
                    DoRegression(initval)
                    tbRegResults.AppendText(vbCrLf)
                    tbRegResults.AppendText(vbCrLf)
                    initval = regressedparameters.Values.ToArray()
                    regpars.Add(initval.Clone)
                    j += 1
                Next

                For i As Integer = 0 To currcase.checkp.Count - 1
                    currcase.checkp(i) = toinclude(i)
                Next

                'regress calculated parameters to obtain temperature dependency

                Dim px(regpars.Count - 1), py_a12(regpars.Count - 1), py_a21(regpars.Count - 1) As Double

                For i As Integer = 0 To currcase.tp.Count - 1
                    If currcase.checkp(i) Then
                        px(i) = SystemsOfUnits.Converter.ConvertToSI(Me.cbTunit.SelectedItem.ToString(), currcase.tp(i))
                    End If
                Next

                For i As Integer = 0 To regpars.Count - 1
                    py_a12(i) = regpars(i)(0)
                    py_a21(i) = regpars(i)(1)
                Next

                Dim obj As Object = Nothing
                Dim lmfit As New Utilities.PetroleumCharacterization.LMFit

                Dim c_a12(2), c_a21(2) As Double
                Dim r_a12, r_a21, n_a12, n_a21 As Double

                c_a12(0) = py_a12(0)
                c_a12(1) = 0.1
                c_a12(2) = 0.01

                obj = lmfit.GetCoeffs(px, py_a12, c_a12.Clone, Utilities.PetroleumCharacterization.LMFit.FitType.SecondDegreePoly, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_a12 = obj(0)
                r_a12 = obj(2)
                n_a12 = obj(3)

                c_a21(0) = py_a21(0)
                c_a21(1) = 0.1
                c_a21(2) = 0.01

                obj = lmfit.GetCoeffs(px, py_a21, c_a21.Clone, Utilities.PetroleumCharacterization.LMFit.FitType.SecondDegreePoly, 0.0000000001, 0.0000000001, 0.0000000001, 10000)
                c_a21 = obj(0)
                r_a21 = obj(2)
                n_a21 = obj(3)

                tbRegResults.AppendText("Finished temperature-dependent regression." & vbCrLf)
                tbRegResults.AppendText(vbCrLf)
                tbRegResults.AppendText("Interaction Parameter 1-2, f(T) = " & c_a12(0) & " + " & c_a12(1) & "*T + " & c_a12(2) & "*T^2, R^2 = " & r_a12)
                tbRegResults.AppendText(vbCrLf)
                tbRegResults.AppendText("A12 = " & c_a12(0) & vbCrLf)
                tbRegResults.AppendText("B12 = " & c_a12(1) & vbCrLf)
                tbRegResults.AppendText("C12 = " & c_a12(2) & vbCrLf)
                tbRegResults.AppendText(vbCrLf)
                tbRegResults.AppendText("Interaction Parameter 2-1, f(T) = " & c_a21(0) & " + " & c_a21(1) & "*T + " & c_a21(2) & "*T^2, R^2 = " & r_a21)
                tbRegResults.AppendText(vbCrLf)
                tbRegResults.AppendText("A21 = " & c_a21(0) & vbCrLf)
                tbRegResults.AppendText("B21 = " & c_a21(1) & vbCrLf)
                tbRegResults.AppendText("C21 = " & c_a21(2) & vbCrLf)
                tbRegResults.AppendText(vbCrLf)
                tbRegResults.AppendText("Plotting results... ")

                IP.Parameters.Clear()
                IP.Parameters("A12") = c_a12(0)
                IP.Parameters("B12") = c_a12(1)
                IP.Parameters("C12") = c_a12(2)
                IP.Parameters("A21") = c_a21(0)
                IP.Parameters("B21") = c_a21(1)
                IP.Parameters("C21") = c_a21(2)

                If currcase.model = "NRTL" Then
                    IP.Parameters("alpha12") = regpars(0)(2)
                End If


                drawtdep = True
                output = False

                A12 = c_a12(0)
                B12 = c_a12(1)
                C12 = c_a12(2)

                A21 = c_a21(0)
                B21 = c_a21(1)
                C21 = c_a21(2)

                FunctionValue(initval)

                tbRegResults.AppendText("done")

            End If

            If IP IsNot Nothing Then
                tbParam.Text = ""
                tbParam.AppendText("Model: " + currcase.model + vbCrLf)
                For Each param In IP.Parameters
                    tbParam.AppendText(param.Key + ": " + param.Value.ToString() + vbCrLf)
                Next
                btnTransfere.Enabled = True
            End If

        Catch ex As Exception

            Me.tbRegResults.AppendText(ex.ToString)

            Console.WriteLine(ex.ToString)

        Finally

            currcase.results = Me.tbRegResults.Text

            Me.btnDoReg.Enabled = True
            Me.btnCancel.Enabled = False
            Me.btnCalcOnce.Enabled = True

            GlobalSettings.Settings.CAPEOPENMode = False

        End Try

    End Sub

    Sub UpdateData()

        Dim i As Integer = 0
        Dim j As Integer = 0
        With Me.currcase
            px = New ArrayList
            px2 = New ArrayList
            px3 = New ArrayList
            px4 = New ArrayList
            py1 = New ArrayList
            py2 = New ArrayList
            py3 = New ArrayList
            py4 = New ArrayList
            py5 = New ArrayList
            ycurvetypes = New ArrayList
            xformat = 1
            title = tbTitle.Text & " / " & .datatype.ToString
            Select Case .datatype
                Case DataType.Txy
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.tp(i)))
                                py2.Add(SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)))
                                py4.Add(SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)))
                                px2.Add(Double.Parse(.yp(i)))
                                py3.Add(Double.Parse(.tp(i)))
                                py5.Add(Double.Parse(.calcy(j)))
                            Catch ex As Exception
                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Liquid Phase Mole Fraction " & .comp1
                    ytitle = "T / " & .tunit
                    y2title = "Vapor Phase Mole Fraction " & .comp1
                    y1ctitle = "Tx exp."
                    y2ctitle = "Tx calc."
                    y3ctitle = "Ty exp."
                    y4ctitle = "Ty calc."
                    y5ctitle = "y exp."
                    y6ctitle = "y calc."
                    ycurvetypes.AddRange(New Integer() {1, 3, 1, 3, 1, 3})
                Case DataType.Pxy
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.pp(i)))
                                py2.Add(SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)))
                                py4.Add(SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)))
                                px2.Add(Double.Parse(.yp(i)))
                                py3.Add(Double.Parse(.pp(i)))
                                py5.Add(Double.Parse(.calcy(j)))
                            Catch ex As Exception
                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Liquid Phase Mole Fraction " & .comp1
                    ytitle = "P / " & .punit
                    y2title = "Vapor Phase Mole Fraction " & .comp1
                    y1ctitle = "Px exp."
                    y2ctitle = "Px calc."
                    y3ctitle = "Py exp."
                    y4ctitle = "Py calc."
                    y5ctitle = "y exp."
                    y6ctitle = "y calc."
                    ycurvetypes.AddRange(New Integer() {1, 3, 1, 3, 1, 3})
                Case DataType.TPxy
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.tp(i)))
                                py2.Add(SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)))
                                py4.Add(SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)))
                                py3.Add(Double.Parse(.pp(i)))
                                py5.Add(Double.Parse(.calcy(j)))
                            Catch ex As Exception
                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Liquid Phase Mole Fraction " & .comp1
                    ytitle = "T / " & .tunit & " - P / " & .punit
                    y2title = "Vapor Phase Mole Fraction " & .comp1
                    y5ctitle = "y exp."
                    y6ctitle = "y calc."
                    ycurvetypes.AddRange(New Integer() {1, 3, 1, 3, 1, 3})
                Case DataType.Txx
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.tp(i)))
                                px2.Add(Double.Parse(.x2p(i)))
                                py2.Add(Double.Parse(.tp(i)))
                                px3.Add(Double.Parse(.calcx1l1(j)))
                                py3.Add(Double.Parse(.tp(i)))
                                px4.Add(Double.Parse(.calcx1l2(j)))
                                py4.Add(Double.Parse(.tp(i)))
                            Catch ex As Exception
                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Mole Fraction " & .comp1
                    ytitle = "T / " & .tunit
                    y1ctitle = "Tx1' exp."
                    y3ctitle = "Tx1' calc."
                    y2ctitle = "Tx1'' exp."
                    y4ctitle = "Tx1'' calc."
                    ycurvetypes.AddRange(New Integer() {1, 1, 3, 3})
                Case DataType.Pxx
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.pp(i)))
                                px2.Add(Double.Parse(.x2p(i)))
                                py2.Add(Double.Parse(.pp(i)))
                                px3.Add(Double.Parse(.calcx1l1(j)))
                                py3.Add(Double.Parse(.pp(i)))
                                px4.Add(Double.Parse(.calcx1l2(j)))
                                py4.Add(Double.Parse(.pp(i)))
                            Catch ex As Exception
                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Mole Fraction " & .comp1
                    ytitle = "P / " & .tunit
                    y1ctitle = "Px1' exp."
                    y3ctitle = "Px1' calc."
                    y2ctitle = "Px1'' exp."
                    y4ctitle = "Px1'' calc."
                    ycurvetypes.AddRange(New Integer() {1, 1, 3, 3})
                Case DataType.TPxx
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.tp(i)))
                                px2.Add(Double.Parse(.x2p(i)))
                                py2.Add(Double.Parse(.tp(i)))
                                px3.Add(Double.Parse(.calcx1l1(j)))
                                py3.Add(Double.Parse(.tp(i)))
                                px4.Add(Double.Parse(.calcx1l2(j)))
                                py4.Add(Double.Parse(.tp(i)))
                            Catch ex As Exception

                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Mole Fraction " & .comp1
                    ytitle = "T / " & .tunit & " - P / " & .punit
                    y1ctitle = "Tx1' exp."
                    y3ctitle = "Tx1' calc."
                    y2ctitle = "Tx1'' exp."
                    y4ctitle = "Tx1'' calc."
                    ycurvetypes.AddRange(New Integer() {1, 1, 3, 3})
                Case DataType.TTxSE, DataType.TTxSS
                    i = 0
                    j = 0
                    For Each b As Boolean In .checkp
                        If b Then
                            Try
                                px.Add(Double.Parse(.x1p(i)))
                                py1.Add(Double.Parse(.tl(i)))
                                px2.Add(Double.Parse(.x1p(i)))
                                py2.Add(Double.Parse(.ts(i)))
                                px3.Add(Double.Parse(.x1p(i)))
                                py3.Add(Double.Parse(.calctl(j)))
                                px4.Add(Double.Parse(.x1p(i)))
                                py4.Add(Double.Parse(.calcts(j)))
                            Catch ex As Exception

                            End Try
                            j += 1
                        End If
                        i += 1
                    Next
                    xtitle = "Mole Fraction " & .comp1
                    ytitle = "T / " & .tunit
                    y1ctitle = "TL exp."
                    y3ctitle = "TL calc."
                    y2ctitle = "TS exp."
                    y4ctitle = "TS calc."
                    ycurvetypes.AddRange(New Integer() {1, 1, 3, 3})
            End Select
        End With

        UpdateTable()
        DrawChart()

    End Sub

    Sub UpdateTable()

        Me.gridstats.Rows.Clear()
        Dim i As Integer = 0
        Dim j As Integer = 0
        With currcase
            For Each b As Boolean In .checkp
                If b Then
                    Try
                        Me.gridstats.Rows.Add(New Object() { .x1p(i), .calcx1l1(j), .x2p(i), .calcx1l2(j), .yp(i), .calcy(j), .tp(i), SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)), .pp(i), SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)),
                                                            .calcy(j) - .yp(i), (.calcy(j) - .yp(i)) / .yp(i), (.calcy(j) - .yp(i)) / .yp(i) * 100,
                                                           SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)) - .pp(i), (SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)) - .pp(i)) / .pp(i), (SystemsOfUnits.Converter.ConvertFromSI(.punit, .calcp(j)) - .pp(i)) / .pp(i) * 100,
                                                            SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)) - .tp(i), (SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)) - .tp(i)) / .tp(i), (SystemsOfUnits.Converter.ConvertFromSI(.tunit, .calct(j)) - .tp(i)) / .tp(i) * 100,
                                                            .calcx1l1(j) - .x1p(i), (.calcx1l1(j) - .x1p(i)) / .x1p(i), (.calcx1l1(j) - .x1p(i)) / .x1p(i) * 100,
                                                            .calcx1l2(j) - .x2p(i), (.calcx1l2(j) - .x2p(i)) / .x2p(i), (.calcx1l2(j) - .x2p(i)) / .x2p(i) * 100, (.calctl(j) - .tl(i)) / .tl(i) * 100, (.calcts(j) - .ts(i)) / .ts(i) * 100})
                    Catch ex As Exception

                    End Try
                    j += 1
                End If
                i += 1
            Next
        End With

        Select Case currcase.datatype
            Case DataType.Txx, DataType.Pxx, DataType.TPxx
                Me.gridstats.Columns(0).Visible = True
                Me.gridstats.Columns(1).Visible = True
                Me.gridstats.Columns(2).Visible = True
                Me.gridstats.Columns(3).Visible = True
                Me.gridstats.Columns(4).Visible = False
                Me.gridstats.Columns(5).Visible = False
                Me.gridstats.Columns(6).Visible = False
                Me.gridstats.Columns(7).Visible = False
                Me.gridstats.Columns(8).Visible = False
                Me.gridstats.Columns(9).Visible = False
                Me.gridstats.Columns(10).Visible = False
                Me.gridstats.Columns(11).Visible = False
                Me.gridstats.Columns(12).Visible = False
                Me.gridstats.Columns(13).Visible = False
                Me.gridstats.Columns(14).Visible = False
                Me.gridstats.Columns(15).Visible = False
                Me.gridstats.Columns(16).Visible = False
                Me.gridstats.Columns(17).Visible = False
                Me.gridstats.Columns(18).Visible = False
                Me.gridstats.Columns(19).Visible = True
                Me.gridstats.Columns(20).Visible = True
                Me.gridstats.Columns(21).Visible = True
                Me.gridstats.Columns(22).Visible = True
                Me.gridstats.Columns(23).Visible = True
                Me.gridstats.Columns(24).Visible = True
                Me.gridstats.Columns(25).Visible = False
                Me.gridstats.Columns(26).Visible = False
            Case DataType.Pxy
                Me.gridstats.Columns(0).Visible = True
                Me.gridstats.Columns(1).Visible = False
                Me.gridstats.Columns(2).Visible = False
                Me.gridstats.Columns(3).Visible = False
                Me.gridstats.Columns(4).Visible = True
                Me.gridstats.Columns(5).Visible = True
                Me.gridstats.Columns(6).Visible = True
                Me.gridstats.Columns(7).Visible = False
                Me.gridstats.Columns(8).Visible = True
                Me.gridstats.Columns(9).Visible = True
                Me.gridstats.Columns(10).Visible = True
                Me.gridstats.Columns(11).Visible = True
                Me.gridstats.Columns(12).Visible = True
                Me.gridstats.Columns(13).Visible = True
                Me.gridstats.Columns(14).Visible = True
                Me.gridstats.Columns(15).Visible = True
                Me.gridstats.Columns(16).Visible = False
                Me.gridstats.Columns(17).Visible = False
                Me.gridstats.Columns(18).Visible = False
                Me.gridstats.Columns(19).Visible = False
                Me.gridstats.Columns(20).Visible = False
                Me.gridstats.Columns(21).Visible = False
                Me.gridstats.Columns(22).Visible = False
                Me.gridstats.Columns(23).Visible = False
                Me.gridstats.Columns(24).Visible = False
                Me.gridstats.Columns(25).Visible = False
                Me.gridstats.Columns(26).Visible = False
            Case DataType.Txy
                Me.gridstats.Columns(0).Visible = True
                Me.gridstats.Columns(1).Visible = False
                Me.gridstats.Columns(2).Visible = False
                Me.gridstats.Columns(3).Visible = False
                Me.gridstats.Columns(4).Visible = True
                Me.gridstats.Columns(5).Visible = True
                Me.gridstats.Columns(6).Visible = True
                Me.gridstats.Columns(7).Visible = True
                Me.gridstats.Columns(8).Visible = True
                Me.gridstats.Columns(9).Visible = False
                Me.gridstats.Columns(10).Visible = True
                Me.gridstats.Columns(11).Visible = True
                Me.gridstats.Columns(12).Visible = True
                Me.gridstats.Columns(13).Visible = False
                Me.gridstats.Columns(14).Visible = False
                Me.gridstats.Columns(15).Visible = False
                Me.gridstats.Columns(16).Visible = True
                Me.gridstats.Columns(17).Visible = True
                Me.gridstats.Columns(18).Visible = True
                Me.gridstats.Columns(19).Visible = False
                Me.gridstats.Columns(20).Visible = False
                Me.gridstats.Columns(21).Visible = False
                Me.gridstats.Columns(22).Visible = False
                Me.gridstats.Columns(23).Visible = False
                Me.gridstats.Columns(24).Visible = False
                Me.gridstats.Columns(25).Visible = False
                Me.gridstats.Columns(26).Visible = False
            Case DataType.TTxSE, DataType.TTxSS
                Me.gridstats.Columns(0).Visible = True
                Me.gridstats.Columns(1).Visible = False
                Me.gridstats.Columns(2).Visible = False
                Me.gridstats.Columns(3).Visible = False
                Me.gridstats.Columns(4).Visible = False
                Me.gridstats.Columns(5).Visible = False
                Me.gridstats.Columns(6).Visible = False
                Me.gridstats.Columns(7).Visible = False
                Me.gridstats.Columns(8).Visible = True
                Me.gridstats.Columns(9).Visible = False
                Me.gridstats.Columns(10).Visible = False
                Me.gridstats.Columns(11).Visible = False
                Me.gridstats.Columns(12).Visible = False
                Me.gridstats.Columns(13).Visible = False
                Me.gridstats.Columns(14).Visible = False
                Me.gridstats.Columns(15).Visible = False
                Me.gridstats.Columns(16).Visible = False
                Me.gridstats.Columns(17).Visible = False
                Me.gridstats.Columns(18).Visible = False
                Me.gridstats.Columns(19).Visible = False
                Me.gridstats.Columns(20).Visible = False
                Me.gridstats.Columns(21).Visible = False
                Me.gridstats.Columns(22).Visible = False
                Me.gridstats.Columns(23).Visible = False
                Me.gridstats.Columns(24).Visible = False
                Me.gridstats.Columns(25).Visible = True
                Me.gridstats.Columns(26).Visible = True
        End Select


    End Sub

    Public px, px2, px3, px4, py1, py2, py3, py4, py5 As ArrayList
    Public xtitle, ytitle, y2title, title, y1ctitle, y2ctitle, y3ctitle, y4ctitle, y5ctitle, y6ctitle As String
    Public ycurvetypes As ArrayList
    Public xformat As Integer

    'xformat:
    '1 - double number
    '2 - integer
    '3 - date (dd/MM)

    'ycurvetypes:
    '1 - points only
    '2 - points and line
    '3 - line only
    '4 - dashed line
    '5 - dashed line with points
    '6 - non-smoothed line

    Sub DrawChart()

        Dim rnd As New Random()

        With graph.GraphPane
            .GraphObjList.Clear()
            .CurveList.Clear()
            .YAxisList.Clear()
            If py1.Count > 0 Then
                Dim ya0 As New ZedGraph.YAxis(ytitle)
                ya0.Scale.FontSpec.Size = 10
                ya0.Title.FontSpec.Size = 11
                .YAxisList.Add(ya0)
                Dim mycurve As LineItem = Nothing
                Select Case currcase.datatype
                    Case DataType.Txy, DataType.Pxy, DataType.TPxy
                        mycurve = .AddCurve(y1ctitle, px.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                    Case DataType.Txy, DataType.Pxy
                        mycurve = .AddCurve(y1ctitle, px.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                    Case DataType.TPxx, DataType.Txx, DataType.Pxx
                        mycurve = .AddCurve(y1ctitle, px.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                    Case DataType.TTxSE, DataType.TTxSS
                        mycurve = .AddCurve(y1ctitle, px.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                End Select
                With mycurve
                    Dim ppl As ZedGraph.PointPairList = .Points
                    ppl.Sort(ZedGraph.SortType.XValues)
                    Select Case ycurvetypes(0)
                        '1 - somente pontos
                        '2 - pontos e linha
                        '3 - somente linha
                        '4 - linha tracejada
                        '5 - linha tracejada com pontos
                        Case 1
                            .Line.IsVisible = False
                            .Line.IsSmooth = False
                            .Color = Color.Blue
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = Color.Blue
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 2
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Color = Color.Blue
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = Color.Blue
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 3
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Color = Color.Blue
                            .Symbol.IsVisible = False
                        Case 4
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Line.Style = Drawing2D.DashStyle.Dash
                            .Color = Color.Blue
                            .Symbol.IsVisible = False
                        Case 5
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            .Line.Style = Drawing2D.DashStyle.Dash
                            .Color = Color.Blue
                            .Symbol.Type = ZedGraph.SymbolType.Circle
                            .Symbol.Fill.Type = ZedGraph.FillType.Solid
                            .Symbol.Fill.Color = Color.Blue
                            .Symbol.Fill.IsVisible = True
                            .Symbol.Size = 5
                        Case 6
                            .Line.IsVisible = True
                            .Line.IsSmooth = False
                            Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                            .Color = Color.Blue
                            .Symbol.IsVisible = False
                    End Select
                    .YAxisIndex = 0
                End With
            End If
            If Not py2 Is Nothing Or Not px2 Is Nothing Then
                If py2.Count > 0 Or px2.Count > 0 Then
                    Dim mycurve As LineItem = Nothing
                    Select Case currcase.datatype
                        Case DataType.Txy, DataType.Pxy, DataType.TPxy
                            mycurve = .AddCurve(y2ctitle, px.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.Black)
                        Case DataType.Txy, DataType.Pxy
                            mycurve = .AddCurve(y2ctitle, px2.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TPxx, DataType.Txx, DataType.Pxx
                            mycurve = .AddCurve(y2ctitle, px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TTxSE, DataType.TTxSS
                            mycurve = .AddCurve(y2ctitle, px2.ToArray(GetType(Double)), py2.ToArray(GetType(Double)), Color.Black)
                    End Select
                    With mycurve
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(1)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                .Color = Color.LightBlue
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightBlue
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightBlue
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightBlue
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightBlue
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.LightBlue
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.LightBlue
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightBlue
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightBlue
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If
            If Not py3 Is Nothing Or Not px3 Is Nothing Then
                If py3.Count > 0 Or px3.Count > 0 Then
                    Dim mycurve As LineItem = Nothing
                    Select Case currcase.datatype
                        Case DataType.Txy, DataType.Pxy, DataType.TPxy
                            mycurve = .AddCurve(y3ctitle, px2.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Black)
                        Case DataType.Txy, DataType.Pxy
                            mycurve = .AddCurve(y3ctitle, px3.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TPxx, DataType.Txx, DataType.Pxx
                            mycurve = .AddCurve(y3ctitle, px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TTxSE, DataType.TTxSS
                            mycurve = .AddCurve(y3ctitle, px3.ToArray(GetType(Double)), py3.ToArray(GetType(Double)), Color.Black)
                    End Select
                    With mycurve
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(2)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                .Color = Color.Red
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.Red
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.Red
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.Red
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.Red
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.Red
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.Red
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.Red
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.Red
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If
            If Not py4 Is Nothing Or Not px4 Is Nothing Then
                If py4.Count > 0 Or px4.Count > 0 Then
                    Dim mycurve As LineItem = Nothing
                    Select Case currcase.datatype
                        Case DataType.Txy, DataType.Pxy, DataType.TPxy
                            mycurve = .AddCurve(y4ctitle, px2.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Black)
                        Case DataType.Txy, DataType.Pxy
                            mycurve = .AddCurve(y4ctitle, px4.ToArray(GetType(Double)), py1.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TPxx, DataType.Txx, DataType.Pxx
                            mycurve = .AddCurve(y4ctitle, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Black)
                        Case DataType.TTxSE, DataType.TTxSS
                            mycurve = .AddCurve(y4ctitle, px4.ToArray(GetType(Double)), py4.ToArray(GetType(Double)), Color.Black)
                    End Select
                    With mycurve
                        Dim ppl As ZedGraph.PointPairList = .Points
                        ppl.Sort(ZedGraph.SortType.XValues)
                        Select Case ycurvetypes(3)
                            '1 - somente pontos
                            '2 - pontos e linha
                            '3 - somente linha
                            '4 - linha tracejada
                            '5 - linha tracejada com pontos
                            Case 1
                                .Line.IsVisible = False
                                .Line.IsSmooth = False
                                .Color = Color.LightSalmon
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightSalmon
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 2
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightSalmon
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightSalmon
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 3
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightSalmon
                                .Symbol.IsVisible = False
                            Case 4
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.LightSalmon
                                .Symbol.IsVisible = False
                            Case 5
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Line.Style = Drawing2D.DashStyle.Dash
                                .Color = Color.LightSalmon
                                .Symbol.Type = ZedGraph.SymbolType.Circle
                                .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                .Symbol.Fill.Color = Color.LightSalmon
                                .Symbol.Fill.IsVisible = True
                                .Symbol.Size = 5
                            Case 6
                                .Line.IsVisible = True
                                .Line.IsSmooth = False
                                .Color = Color.LightSalmon
                                .Symbol.IsVisible = False
                        End Select
                        .YAxisIndex = 0
                    End With
                End If
            End If

            With .Legend
                .Border.IsVisible = False
                .Position = ZedGraph.LegendPos.BottomCenter
                .IsHStack = True
                .FontSpec.Size = 10
            End With

            With .XAxis
                .Title.Text = xtitle
                .Title.FontSpec.Size = 11
                .Scale.MinAuto = False
                .Scale.MaxAuto = False
                .Scale.Min = 0.0#
                .Scale.Max = 1.0#
                .Scale.FontSpec.Size = 10
                Select Case xformat
                    Case 1
                        .Type = ZedGraph.AxisType.Linear
                    Case 2
                        .Type = ZedGraph.AxisType.Linear
                    Case 3
                        .Type = ZedGraph.AxisType.DateAsOrdinal
                        .Scale.Format = "dd/MM/yy"
                End Select
            End With

            If Not .YAxis Is Nothing Then
                With .YAxis
                    .Scale.MinAuto = True
                    .Scale.MaxAuto = True
                End With
            End If

            With .Legend
                .Border.IsVisible = False
                .IsVisible = True
                .Position = ZedGraph.LegendPos.TopCenter
                .FontSpec.Size = 11
            End With

            .Margin.All = 10

            With .Title
                .IsVisible = True
                .Text = title
                .FontSpec.Size = 12
            End With

            Me.graph.IsAntiAlias = True

            Try
                Me.graph.AxisChange()
                Me.graph.Invalidate()
            Catch ex As Exception

            End Try

        End With

        Select Case Me.currcase.datatype
            Case DataType.Pxy, DataType.Txy, DataType.TPxy
                With graph2.GraphPane
                    .GraphObjList.Clear()
                    .CurveList.Clear()
                    .YAxisList.Clear()
                    If px2.Count > 0 Then
                        Dim ya0 As New ZedGraph.YAxis(y2title)
                        ya0.Scale.FontSpec.Size = 10
                        ya0.Title.FontSpec.Size = 11
                        ya0.Scale.Min = 0.0#
                        ya0.Scale.Max = 1.0#
                        .YAxisList.Add(ya0)
                        Dim mycurve As LineItem = Nothing
                        mycurve = .AddCurve(y5ctitle, px.ToArray(GetType(Double)), px2.ToArray(GetType(Double)), Color.Black)
                        With mycurve
                            Dim ppl As ZedGraph.PointPairList = .Points
                            ppl.Sort(ZedGraph.SortType.XValues)
                            Select Case ycurvetypes(4)
                                '1 - somente pontos
                                '2 - pontos e linha
                                '3 - somente linha
                                '4 - linha tracejada
                                '5 - linha tracejada com pontos
                                Case 1
                                    .Line.IsVisible = False
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 2
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 3
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                                Case 4
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Line.Style = Drawing2D.DashStyle.Dash
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                                Case 5
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Line.Style = Drawing2D.DashStyle.Dash
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 6
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                            End Select
                            .YAxisIndex = 0
                        End With
                    End If
                    If py5.Count > 0 Then
                        Dim mycurve As LineItem = Nothing
                        mycurve = .AddCurve(y6ctitle, px.ToArray(GetType(Double)), py5.ToArray(GetType(Double)), Color.Black)
                        With mycurve
                            Dim ppl As ZedGraph.PointPairList = .Points
                            ppl.Sort(ZedGraph.SortType.XValues)
                            Select Case ycurvetypes(5)
                                '1 - somente pontos
                                '2 - pontos e linha
                                '3 - somente linha
                                '4 - linha tracejada
                                '5 - linha tracejada com pontos
                                Case 1
                                    .Line.IsVisible = False
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 2
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 3
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                                Case 4
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Line.Style = Drawing2D.DashStyle.Dash
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                                Case 5
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    .Line.Style = Drawing2D.DashStyle.Dash
                                    .Color = Color.Red
                                    .Symbol.Type = ZedGraph.SymbolType.Circle
                                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                                    .Symbol.Fill.Color = Color.Red
                                    .Symbol.Fill.IsVisible = True
                                    .Symbol.Size = 5
                                Case 6
                                    .Line.IsVisible = True
                                    .Line.IsSmooth = False
                                    Dim c1 As Color = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                                    .Color = Color.Red
                                    .Symbol.IsVisible = False
                            End Select
                            .YAxisIndex = 0
                        End With
                    End If

                    With .AddCurve("", New Double() {0.0#, 1.0#}, New Double() {0.0#, 1.0#}, Color.Black, SymbolType.None)
                        .Line.IsVisible = True
                        .Line.Width = 1
                        .YAxisIndex = 0
                    End With

                    With .Legend
                        .Border.IsVisible = False
                        .Position = ZedGraph.LegendPos.BottomCenter
                        .IsHStack = True
                        .FontSpec.Size = 10
                    End With

                    With .XAxis
                        .Title.Text = xtitle
                        .Title.FontSpec.Size = 11
                        .Scale.MinAuto = False
                        .Scale.MaxAuto = False
                        .Scale.Min = 0.0#
                        .Scale.Max = 1.0#
                        .Scale.FontSpec.Size = 10
                        Select Case xformat
                            Case 1
                                .Type = ZedGraph.AxisType.Linear
                            Case 2
                                .Type = ZedGraph.AxisType.Linear
                            Case 3
                                .Type = ZedGraph.AxisType.DateAsOrdinal
                                .Scale.Format = "dd/MM/yy"
                        End Select
                    End With

                    With .Legend
                        .Border.IsVisible = False
                        .IsVisible = True
                        .Position = ZedGraph.LegendPos.TopCenter
                        .FontSpec.Size = 11
                    End With

                    .Margin.All = 10

                    With .Title
                        .IsVisible = True
                        .Text = title
                        .FontSpec.Size = 12
                    End With

                End With

                Me.graph2.IsAntiAlias = True

                Try
                    Me.graph2.AxisChange()
                    Me.graph2.Invalidate()
                Catch ex As Exception

                End Try

        End Select

    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        cancel = True
        Application.DoEvents()
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
                        dgv.Rows.Add()
                        dgv.Rows(0).Cells(0).Value = True
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
                arT = tArr(i).Split(Char.ConvertFromUtf32(9))
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

    Private Sub cbModel_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbModel.SelectedIndexChanged
        Select Case cbModel.SelectedItem.ToString
            Case "Peng-Robinson", "Soave-Redlich-Kwong"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"kij", -0.5, 0.0#, 0.5, False})
                End With
                Button1.Enabled = False
                Button2.Enabled = False
                Button3.Enabled = False
                chkIdealVaporPhase.Enabled = False
                chkDoTDepRegression.Enabled = False
                chkDoTDepRegression.Checked = False
            Case "Lee-Kesler-Plöcker"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"kij", 0.9, 1.0#, 1.1, False})
                End With
                Button1.Enabled = False
                Button2.Enabled = False
                Button3.Enabled = False
                chkIdealVaporPhase.Enabled = False
                chkDoTDepRegression.Enabled = False
                chkDoTDepRegression.Checked = False
            Case "PRSV2-M"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"kij", -0.5, 0.0#, 0.5, False})
                    .Add(New Object() {"kji", -0.5, 0.0#, 0.5, False})
                End With
                Button1.Enabled = False
                Button2.Enabled = False
                Button3.Enabled = False
                chkIdealVaporPhase.Enabled = False
                chkDoTDepRegression.Enabled = False
                chkDoTDepRegression.Checked = False
            Case "PRSV2-VL"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"kij", -0.5, 0.0#, 0.5, False})
                    .Add(New Object() {"kji", -0.5, 0.0#, 0.5, False})
                End With
                Button1.Enabled = False
                Button2.Enabled = False
                Button3.Enabled = False
                chkIdealVaporPhase.Enabled = False
                chkDoTDepRegression.Enabled = False
                chkDoTDepRegression.Checked = False
            Case "UNIQUAC"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"A12 (cal/mol)", -5000, 0.0#, 5000, False})
                    .Add(New Object() {"A21 (cal/mol)", -5000, 0.0#, 5000, False})
                End With
                Button1.Enabled = True
                Button2.Enabled = True
                Button3.Enabled = True
                chkIdealVaporPhase.Enabled = True
                chkDoTDepRegression.Enabled = True
            Case "Wilson"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"A12 (cal/mol)", -5000, 0.0#, 5000, False})
                    .Add(New Object() {"A21 (cal/mol)", -5000, 0.0#, 5000, False})
                End With
                Button1.Enabled = True
                Button2.Enabled = True
                Button3.Enabled = True
                chkIdealVaporPhase.Enabled = True
                chkDoTDepRegression.Enabled = False
            Case "NRTL"
                With gridInEst.Rows
                    .Clear()
                    .Add(New Object() {"A12 (cal/mol)", -5000, 0.0#, 5000, False})
                    .Add(New Object() {"A21 (cal/mol)", -5000, 0.0#, 5000, False})
                    .Add(New Object() {"alpha12", 0.0#, 0.3#, 0.8#, False})
                End With
                Button1.Enabled = True
                Button2.Enabled = True
                Button3.Enabled = True
                chkIdealVaporPhase.Enabled = True
                chkDoTDepRegression.Enabled = True
        End Select
    End Sub

    Private Sub tbTitle_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tbTitle.TextChanged
        Me.Text = tbTitle.Text
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        Select Case cbModel.SelectedItem.ToString
            Case "NRTL"
                If cbDataType.SelectedItem.ToString.Contains("LL") Then
                    Try
                        Dim estimates As Double() = EstimateNRTL(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "UNIFAC-LL")
                        Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                        Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                        Me.gridInEst.Rows(2).Cells(2).Value = estimates(2)
                    Catch ex As Exception
                        MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        Cursor = Cursors.Default
                    End Try
                Else
                    Try
                        Dim estimates As Double() = EstimateNRTL(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "UNIFAC")
                        Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                        Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                        Me.gridInEst.Rows(2).Cells(2).Value = estimates(2)
                    Catch ex As Exception
                        MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        Cursor = Cursors.Default
                    End Try
                End If
            Case "UNIQUAC"
                If cbDataType.SelectedItem.ToString.Contains("LL") Then
                    Try
                        Dim estimates As Double() = EstimateUNIQUAC(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "UNIFAC-LL")
                        Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                        Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                    Catch ex As Exception
                        MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        Cursor = Cursors.Default
                    End Try
                Else
                    Try
                        Dim estimates As Double() = EstimateUNIQUAC(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "UNIFAC")
                        Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                        Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                    Catch ex As Exception
                        MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Finally
                        Cursor = Cursors.Default
                    End Try
                End If
            Case "Wilson"

                Try
                    Dim estimates As Double() = EstimateWilson(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "UNIFAC")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
        End Select

    End Sub

    Dim actu(5), actn(5) As Double

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        If IP IsNot Nothing Then

            IP.Comp1 = currcase.comp1
            IP.Comp2 = currcase.comp2

            Dim BIPs As Object = Nothing

            tbParam.Text = ""
            tbParam.AppendText("Model: " + currcase.model + vbCrLf)
            For Each param In IP.Parameters
                tbParam.AppendText(param.Key + ": " + param.Value.ToString() + vbCrLf)
            Next
            btnTransfere.Enabled = True

            Select Case currcase.model
                Case "UNIQUAC"
                    BIPs = New List(Of Auxiliary.UNIQUAC_IPData) From {
                        New Auxiliary.UNIQUAC_IPData With {.Name1 = IP.Comp1, .Name2 = IP.Comp2,
                                 .A12 = IP.Parameters("A12"), .A21 = IP.Parameters("A21"),
                                 .B12 = IP.Parameters("B12"), .B21 = IP.Parameters("B21"),
                                 .C12 = IP.Parameters("C12"), .C21 = IP.Parameters("C21")}
                    }
                Case "NRTL"
                    BIPs = New List(Of Auxiliary.NRTL_IPData) From {
                        New Auxiliary.NRTL_IPData With {.ID1 = IP.Comp1, .ID2 = IP.Comp2,
                                 .A12 = IP.Parameters("A12"), .A21 = IP.Parameters("A21"),
                                 .B12 = IP.Parameters("B12"), .B21 = IP.Parameters("B21"),
                                 .C12 = IP.Parameters("C12"), .C21 = IP.Parameters("C21"),
                                 .alpha12 = IP.Parameters("alpha12")}
                    }
                Case "Wilson"
                    BIPs = New List(Of Auxiliary.UNIQUAC_IPData) From {
                        New Auxiliary.UNIQUAC_IPData With {.Name1 = IP.Comp1, .Name2 = IP.Comp2,
                                 .A12 = IP.Parameters("A12"), .A21 = IP.Parameters("A21")}
                    }
                Case "Peng-Robinson", "Soave-Redlich-Kwong", "Lee-Kesler-Plöcker"
                    BIPs = New List(Of Auxiliary.PR_IPData) From {
                        New Auxiliary.PR_IPData With {.Name1 = IP.Comp1, .Name2 = IP.Comp2,
                                 .kij = IP.Parameters("kij")}
                    }
                Case "PRSV2-M", "PRSV2-VL"
                    BIPs = New List(Of Auxiliary.PRSV2_IPData) From {
                        New Auxiliary.PRSV2_IPData With {.id1 = IP.Comp1, .id2 = IP.Comp2,
                                 .kij = IP.Parameters("kij"), .kji = IP.Parameters("kji")}
                    }
            End Select

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

            If handler IsNot Nothing Then
                Using stream As New IO.MemoryStream()
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        Try
                            Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(BIPs, Newtonsoft.Json.Formatting.Indented)
                            writer.Write(jsondata)
                            handler.Write(stream)
                            MessageBox.Show("File saved successfully.", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information)
                        Catch ex As Exception
                            MessageBox.Show("Error saving file: " + ex.Message.ToString, "Information", MessageBoxButtons.OK, MessageBoxIcon.Error)
                        End Try
                    End Using
                End Using
            End If

        End If

    End Sub

    Dim ppu As PropertyPackages.UNIQUACPropertyPackage
    Dim uniquac As PropertyPackages.Auxiliary.UNIQUAC
    Dim ppn As PropertyPackages.NRTLPropertyPackage
    Dim nrtl As PropertyPackages.Auxiliary.NRTL
    Dim ppw As WilsonPropertyPackage
    Dim ms As Streams.MaterialStream

    Private Function FunctionValueNRTL(ByVal x() As Double) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
        Else
            doparallel = True
        End If

        Dim a1(1), a2(1), a3(1) As Double

        nrtl.InteractionParameters.Clear()
        nrtl.InteractionParameters.Add(ppn.RET_VIDS()(0), New Dictionary(Of String, PropertyPackages.Auxiliary.NRTL_IPData))
        nrtl.InteractionParameters(ppn.RET_VIDS()(0)).Add(ppn.RET_VIDS()(1), New PropertyPackages.Auxiliary.NRTL_IPData())
        nrtl.InteractionParameters(ppn.RET_VIDS()(0))(ppn.RET_VIDS()(1)).A12 = x(0)
        nrtl.InteractionParameters(ppn.RET_VIDS()(0))(ppn.RET_VIDS()(1)).A21 = x(1)
        nrtl.InteractionParameters(ppn.RET_VIDS()(0))(ppn.RET_VIDS()(1)).alpha12 = 0.2

        If doparallel Then

            Try
                Dim task1 As Task = New Task(Sub()
                                                 a1 = nrtl.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppn.RET_VIDS)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 a2 = nrtl.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppn.RET_VIDS)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 a3 = nrtl.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppn.RET_VIDS)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try

        Else
            a1 = nrtl.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppn.RET_VIDS)
            a2 = nrtl.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppn.RET_VIDS)
            a3 = nrtl.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppn.RET_VIDS)
        End If

        actn(0) = a1(0)
        actn(1) = a2(0)
        actn(2) = a3(0)
        actn(3) = a1(1)
        actn(4) = a2(1)
        actn(5) = a3(1)

        Dim fval As Double = 0.0#
        For i As Integer = 0 To 5
            fval += (actn(i) - actu(i)) ^ 2
        Next

        Return fval

    End Function

    Private Function FunctionValueUNIQUAC(ByVal x() As Double) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
        Else
            doparallel = True
        End If

        Dim a1(1), a2(1), a3(1) As Double

        uniquac.InteractionParameters.Clear()
        uniquac.InteractionParameters.Add(ppu.RET_VIDS()(0), New Dictionary(Of String, PropertyPackages.Auxiliary.UNIQUAC_IPData))
        uniquac.InteractionParameters(ppu.RET_VIDS()(0)).Add(ppu.RET_VIDS()(1), New PropertyPackages.Auxiliary.UNIQUAC_IPData())
        uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A12 = x(0)
        uniquac.InteractionParameters(ppu.RET_VIDS()(0))(ppu.RET_VIDS()(1)).A21 = x(1)

        If doparallel Then

            Try
                Dim task1 As Task = New Task(Sub()
                                                 a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try

        Else
            a1 = uniquac.GAMMA_MR(298.15, New Double() {0.25, 0.75}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
            a2 = uniquac.GAMMA_MR(298.15, New Double() {0.5, 0.5}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
            a3 = uniquac.GAMMA_MR(298.15, New Double() {0.75, 0.25}, ppu.RET_VIDS, ppu.RET_VQ, ppu.RET_VR)
        End If

        actn(0) = a1(0)
        actn(1) = a2(0)
        actn(2) = a3(0)
        actn(3) = a1(1)
        actn(4) = a2(1)
        actn(5) = a3(1)

        Dim fval As Double = 0.0#
        For i As Integer = 0 To 5
            fval += (actn(i) - actu(i)) ^ 2
        Next

        Return fval

    End Function

    Private Function FunctionValueWilson(ByVal x() As Double) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
        Else
            doparallel = True
        End If

        Dim a1(1), a2(1), a3(1) As Double

        ppw.WilsonM.BIPs.Clear()
        ppw.WilsonM.BIPs.Add(ppw.RET_VCAS()(0), New Dictionary(Of String, Double()))
        ppw.WilsonM.BIPs(ppw.RET_VCAS()(0)).Add(ppw.RET_VCAS()(1), New Double() {0.0, 0.0})
        ppw.WilsonM.BIPs(ppw.RET_VCAS()(0))(ppw.RET_VCAS()(1))(0) = x(0)
        ppw.WilsonM.BIPs(ppw.RET_VCAS()(0))(ppw.RET_VCAS()(1))(1) = x(1)

        If doparallel Then

            Try
                Dim task1 As Task = New Task(Sub()
                                                 a1 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.25, 0.75}, ppw.GetArguments())
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 a2 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.5, 0.5}, ppw.GetArguments())
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 a3 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.75, 0.25}, ppw.GetArguments())
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            End Try

        Else
            a1 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.25, 0.75}, ppw.GetArguments())
            a2 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.5, 0.5}, ppw.GetArguments())
            a3 = ppw.WilsonM.CalcActivityCoefficients(298.15, New Double() {0.75, 0.25}, ppw.GetArguments())
        End If

        actn(0) = a1(0)
        actn(1) = a2(0)
        actn(2) = a3(0)
        actn(3) = a1(1)
        actn(4) = a2(1)
        actn(5) = a3(1)

        Dim fval As Double = 0.0#
        For i As Integer = 0 To 5
            fval += (actn(i) - actu(i)) ^ 2
        Next

        Return fval

    End Function

    Function EstimateWilson(ByVal id1 As String, ByVal id2 As String, ByVal model As String) As Double()

        Dim x(1) As Double

        Cursor = Cursors.WaitCursor

        ppw = New WilsonPropertyPackage()

        ms = New Streams.MaterialStream("", "")

        Dim comp1, comp2 As ConstantProperties
        comp1 = FormMain.AvailableComponents(id1)
        comp2 = FormMain.AvailableComponents(id2)

        With ms
            For Each phase In ms.Phases.Values
                With phase
                    .Compounds.Add(comp1.Name, New Compound(comp1.Name, ""))
                    .Compounds(comp1.Name).ConstantProperties = comp1
                    .Compounds.Add(comp2.Name, New Compound(comp2.Name, ""))
                    .Compounds(comp2.Name).ConstantProperties = comp2
                End With
            Next
        End With

        ppw.CurrentMaterialStream = ms

        Dim ppuf As UNIFACPropertyPackage = Nothing
        Dim ppufll As UNIFACLLPropertyPackage = Nothing
        Dim ppmu As MODFACPropertyPackage = Nothing
        Dim ppmun As NISTMFACPropertyPackage = Nothing
        Dim unif As PropertyPackages.Auxiliary.Unifac = Nothing
        Dim unifll As PropertyPackages.Auxiliary.UnifacLL = Nothing
        Dim modf As PropertyPackages.Auxiliary.Modfac = Nothing
        Dim nmodf As PropertyPackages.Auxiliary.NISTMFAC = Nothing

        Select Case model
            Case "UNIFAC"
                ppuf = New PropertyPackages.UNIFACPropertyPackage(True)
                ppuf.CurrentMaterialStream = ms
                unif = New PropertyPackages.Auxiliary.Unifac
            Case "UNIFAC-LL"
                ppufll = New PropertyPackages.UNIFACLLPropertyPackage(True)
                ppufll.CurrentMaterialStream = ms
                unifll = New PropertyPackages.Auxiliary.UnifacLL
            Case "MODFAC"
                ppmu = New PropertyPackages.MODFACPropertyPackage(True)
                ppmu.CurrentMaterialStream = ms
                modf = New PropertyPackages.Auxiliary.Modfac
            Case Else
                ppmun = New PropertyPackages.NISTMFACPropertyPackage(True)
                ppmun.CurrentMaterialStream = ms
                nmodf = New PropertyPackages.Auxiliary.NISTMFAC
        End Select

        Dim T1 = 298.15

        Dim a1(1), a2(1), a3(1) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
            dogpu = My.Settings.EnableGPUProcessing
        Else
            doparallel = True
            dogpu = False
        End If

        If dogpu Then Calculator.InitComputeDevice()

        If doparallel Then

            If dogpu Then GlobalSettings.Settings.gpu.EnableMultithreading()
            Try
                Dim task1 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            Finally
                If dogpu Then
                    GlobalSettings.Settings.gpu.DisableMultithreading()
                    GlobalSettings.Settings.gpu.FreeAll()
                End If
            End Try

        Else
            Select Case model
                Case "UNIFAC"
                    a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                Case "UNIFAC-LL"
                    a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                Case "MODFAC"
                    a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                Case Else
                    a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
            End Select
        End If

        actu(0) = a1(0)
        actu(1) = a2(0)
        actu(2) = a3(0)
        actu(3) = a1(1)
        actu(4) = a2(1)
        actu(5) = a3(1)

        x(0) = gridInEst.Rows(0).Cells(1).Value
        x(1) = gridInEst.Rows(1).Cells(1).Value

        Dim initval2() As Double = New Double() {x(0), x(1)}
        Dim lconstr2() As Double = New Double() {-10000.0#, -10000.0#}
        Dim uconstr2() As Double = New Double() {+10000.0#, +10000.0#}
        Dim finalval2() As Double = Nothing

        Dim variables(1) As Optimization.OptBoundVariable
        For i As Integer = 0 To 1
            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
        Next
        Dim solver As New Optimization.Simplex
        solver.Tolerance = 0.01
        solver.MaxFunEvaluations = 1000
        finalval2 = solver.ComputeMin(AddressOf FunctionValueWilson, variables)

        ppuf = Nothing
        ppufll = Nothing
        ppmu = Nothing
        ppmun = Nothing
        ppw.Dispose()
        ppw = Nothing
        uniquac = Nothing
        unif = Nothing
        unifll = Nothing
        modf = Nothing
        nmodf = Nothing
        ms.Dispose()
        ms = Nothing

        Cursor = Cursors.Default

        Return New Double() {finalval2(0), finalval2(1)}

    End Function

    Function EstimateUNIQUAC(ByVal id1 As String, ByVal id2 As String, ByVal model As String) As Double()

        Dim x(1) As Double

        Cursor = Cursors.WaitCursor

        ppu = New PropertyPackages.UNIQUACPropertyPackage(True)
        uniquac = New PropertyPackages.Auxiliary.UNIQUAC

        ms = New Streams.MaterialStream("", "")

        Dim comp1, comp2 As ConstantProperties
        comp1 = FormMain.AvailableComponents(id1)
        comp2 = FormMain.AvailableComponents(id2)

        With ms
            For Each phase In ms.Phases.Values
                With phase
                    .Compounds.Add(comp1.Name, New Compound(comp1.Name, ""))
                    .Compounds(comp1.Name).ConstantProperties = comp1
                    .Compounds.Add(comp2.Name, New Compound(comp2.Name, ""))
                    .Compounds(comp2.Name).ConstantProperties = comp2
                End With
            Next
        End With

        ppu.CurrentMaterialStream = ms

        Dim ppuf As UNIFACPropertyPackage = Nothing
        Dim ppufll As UNIFACLLPropertyPackage = Nothing
        Dim ppmu As MODFACPropertyPackage = Nothing
        Dim ppmun As NISTMFACPropertyPackage = Nothing
        Dim unif As PropertyPackages.Auxiliary.Unifac = Nothing
        Dim unifll As PropertyPackages.Auxiliary.UnifacLL = Nothing
        Dim modf As PropertyPackages.Auxiliary.Modfac = Nothing
        Dim nmodf As PropertyPackages.Auxiliary.NISTMFAC = Nothing

        Select Case model
            Case "UNIFAC"
                ppuf = New PropertyPackages.UNIFACPropertyPackage(True)
                ppuf.CurrentMaterialStream = ms
                unif = New PropertyPackages.Auxiliary.Unifac
            Case "UNIFAC-LL"
                ppufll = New PropertyPackages.UNIFACLLPropertyPackage(True)
                ppufll.CurrentMaterialStream = ms
                unifll = New PropertyPackages.Auxiliary.UnifacLL
            Case "MODFAC"
                ppmu = New PropertyPackages.MODFACPropertyPackage(True)
                ppmu.CurrentMaterialStream = ms
                modf = New PropertyPackages.Auxiliary.Modfac
            Case Else
                ppmun = New PropertyPackages.NISTMFACPropertyPackage(True)
                ppmun.CurrentMaterialStream = ms
                nmodf = New PropertyPackages.Auxiliary.NISTMFAC
        End Select

        Dim T1 = 298.15

        Dim a1(1), a2(1), a3(1) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
            dogpu = My.Settings.EnableGPUProcessing
        Else
            doparallel = True
            dogpu = False
        End If

        If dogpu Then Calculator.InitComputeDevice()

        If doparallel Then

            If dogpu Then GlobalSettings.Settings.gpu.EnableMultithreading()
            Try
                Dim task1 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            Finally
                If dogpu Then
                    GlobalSettings.Settings.gpu.DisableMultithreading()
                    GlobalSettings.Settings.gpu.FreeAll()
                End If
            End Try

        Else
            Select Case model
                Case "UNIFAC"
                    a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                Case "UNIFAC-LL"
                    a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                Case "MODFAC"
                    a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                Case Else
                    a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
            End Select
        End If

        actu(0) = a1(0)
        actu(1) = a2(0)
        actu(2) = a3(0)
        actu(3) = a1(1)
        actu(4) = a2(1)
        actu(5) = a3(1)

        x(0) = gridInEst.Rows(0).Cells(1).Value
        x(1) = gridInEst.Rows(1).Cells(1).Value

        Dim initval2() As Double = New Double() {x(0), x(1)}
        Dim lconstr2() As Double = New Double() {-10000.0#, -10000.0#}
        Dim uconstr2() As Double = New Double() {+10000.0#, +10000.0#}
        Dim finalval2() As Double = Nothing

        Dim variables(1) As Optimization.OptBoundVariable
        For i As Integer = 0 To 1
            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
        Next
        Dim solver As New Optimization.Simplex
        solver.Tolerance = 0.01
        solver.MaxFunEvaluations = 1000
        finalval2 = solver.ComputeMin(AddressOf FunctionValueUNIQUAC, variables)

        ppuf = Nothing
        ppufll = Nothing
        ppmu = Nothing
        ppmun = Nothing
        ppu.Dispose()
        ppu = Nothing
        uniquac = Nothing
        unif = Nothing
        unifll = Nothing
        modf = Nothing
        nmodf = Nothing
        ms.Dispose()
        ms = Nothing

        Cursor = Cursors.Default

        Return New Double() {finalval2(0), finalval2(1)}

    End Function

    Function EstimateNRTL(ByVal id1 As String, ByVal id2 As String, ByVal model As String) As Double()

        Dim x(1) As Double

        Cursor = Cursors.WaitCursor

        ppn = New PropertyPackages.NRTLPropertyPackage(True)
        nrtl = New PropertyPackages.Auxiliary.NRTL

        ms = New Streams.MaterialStream("", "")

        Dim comp1, comp2 As ConstantProperties
        comp1 = FormMain.AvailableComponents(id1)
        comp2 = FormMain.AvailableComponents(id2)

        With ms
            For Each phase In ms.Phases.Values
                With phase
                    .Compounds.Add(comp1.Name, New Compound(comp1.Name, ""))
                    .Compounds(comp1.Name).ConstantProperties = comp1
                    .Compounds.Add(comp2.Name, New Compound(comp2.Name, ""))
                    .Compounds(comp2.Name).ConstantProperties = comp2
                End With
            Next
        End With

        ppn.CurrentMaterialStream = ms

        Dim ppuf As UNIFACPropertyPackage = Nothing
        Dim ppufll As UNIFACLLPropertyPackage = Nothing
        Dim ppmu As MODFACPropertyPackage = Nothing
        Dim ppmun As NISTMFACPropertyPackage = Nothing
        Dim unif As PropertyPackages.Auxiliary.Unifac = Nothing
        Dim unifll As PropertyPackages.Auxiliary.UnifacLL = Nothing
        Dim modf As PropertyPackages.Auxiliary.Modfac = Nothing
        Dim nmodf As PropertyPackages.Auxiliary.NISTMFAC = Nothing

        Select Case model
            Case "UNIFAC"
                ppuf = New PropertyPackages.UNIFACPropertyPackage(True)
                ppuf.CurrentMaterialStream = ms
                unif = New PropertyPackages.Auxiliary.Unifac
            Case "UNIFAC-LL"
                ppufll = New PropertyPackages.UNIFACLLPropertyPackage(True)
                ppufll.CurrentMaterialStream = ms
                unifll = New PropertyPackages.Auxiliary.UnifacLL
            Case "MODFAC"
                ppmu = New PropertyPackages.MODFACPropertyPackage(True)
                ppmu.CurrentMaterialStream = ms
                modf = New PropertyPackages.Auxiliary.Modfac
            Case Else
                ppmun = New PropertyPackages.NISTMFACPropertyPackage(True)
                ppmun.CurrentMaterialStream = ms
                nmodf = New PropertyPackages.Auxiliary.NISTMFAC
        End Select

        Dim T1 = 298.15

        Dim a1(1), a2(1), a3(1) As Double

        If GlobalSettings.Settings.OldUI Then
            doparallel = My.Settings.EnableParallelProcessing
            dogpu = My.Settings.EnableGPUProcessing
        Else
            doparallel = True
            dogpu = False
        End If

        If dogpu Then Calculator.InitComputeDevice()

        If doparallel Then

            If dogpu Then GlobalSettings.Settings.gpu.EnableMultithreading()
            Try
                Dim task1 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task2 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                Dim task3 As Task = New Task(Sub()
                                                 Select Case model
                                                     Case "UNIFAC"
                                                         a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                                                     Case "UNIFAC-LL"
                                                         a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                                                     Case "MODFAC"
                                                         a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                                                     Case Else
                                                         a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                                                 End Select
                                             End Sub)
                task1.Start()
                task2.Start()
                task3.Start()
                Task.WaitAll(task1, task2, task3)
            Catch ae As AggregateException
                Throw ae.Flatten().InnerException
            Finally
                If dogpu Then
                    GlobalSettings.Settings.gpu.DisableMultithreading()
                    GlobalSettings.Settings.gpu.FreeAll()
                End If
            End Try

        Else
            Select Case model
                Case "UNIFAC"
                    a1 = unif.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a2 = unif.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                    a3 = unif.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppuf.RET_VQ, ppuf.RET_VR, ppuf.RET_VEKI)
                Case "UNIFAC-LL"
                    a1 = unifll.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a2 = unifll.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                    a3 = unifll.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppufll.RET_VQ, ppufll.RET_VR, ppufll.RET_VEKI)
                Case "MODFAC"
                    a1 = modf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a2 = modf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                    a3 = modf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmu.RET_VQ, ppmu.RET_VR, ppmu.RET_VEKI)
                Case Else
                    a1 = nmodf.GAMMA_MR(T1, New Double() {0.25, 0.75}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a2 = nmodf.GAMMA_MR(T1, New Double() {0.5, 0.5}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
                    a3 = nmodf.GAMMA_MR(T1, New Double() {0.75, 0.25}, ppmun.RET_VQ, ppmun.RET_VR, ppmun.RET_VEKI)
            End Select
        End If

        actu(0) = a1(0)
        actu(1) = a2(0)
        actu(2) = a3(0)
        actu(3) = a1(1)
        actu(4) = a2(1)
        actu(5) = a3(1)

        x(0) = gridInEst.Rows(0).Cells(1).Value
        x(1) = gridInEst.Rows(1).Cells(1).Value

        Dim initval2() As Double = New Double() {x(0), x(1)}
        Dim lconstr2() As Double = New Double() {-10000.0#, -10000.0#}
        Dim uconstr2() As Double = New Double() {+10000.0#, +10000.0#}
        Dim finalval2() As Double = Nothing

        Dim variables(1) As Optimization.OptBoundVariable
        For i As Integer = 0 To 1
            variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), False, lconstr2(i), uconstr2(i))
        Next
        Dim solver As New Optimization.Simplex
        solver.Tolerance = 0.01
        solver.MaxFunEvaluations = 1000
        finalval2 = solver.ComputeMin(AddressOf FunctionValueNRTL, variables)

        ppuf = Nothing
        ppufll = Nothing
        ppmu = Nothing
        ppmun = Nothing
        ppn.Dispose()
        ppn = Nothing
        nrtl = Nothing
        unif = Nothing
        unifll = Nothing
        modf = Nothing
        nmodf = Nothing
        ms.Dispose()
        ms = Nothing

        Cursor = Cursors.WaitCursor

        Return New Double() {finalval2(0), finalval2(1), 0.2#}

    End Function

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        Select Case cbModel.SelectedItem.ToString
            Case "NRTL"
                Try
                    Dim estimates As Double() = EstimateNRTL(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "MODFAC")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                    Me.gridInEst.Rows(2).Cells(2).Value = estimates(2)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
            Case "UNIQUAC"
                Try
                    Dim estimates As Double() = EstimateUNIQUAC(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "MODFAC")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
        End Select

    End Sub

    Private Sub ToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click, ToolStripMenuItem2.Click,
    ToolStripMenuItem3.Click, ToolStripMenuItem4.Click, ToolStripMenuItem5.Click, ToolStripMenuItem6.Click, ToolStripMenuItem7.Click

        Dim P, T, x11, x12, y1 As Double, comp1, comp2, model As String
        comp1 = Me.cbCompound1.SelectedItem.ToString
        comp2 = Me.cbCompound2.SelectedItem.ToString

        If CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem1" Then
            model = "UNIFAC"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem2" Then
            model = "NRTL"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem3" Then
            model = "UNIQUAC"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem4" Then
            model = "Peng-Robinson (PR)"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem5" Then
            model = "Soave-Redlich-Kwong (SRK)"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem6" Then
            model = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
        ElseIf CType(sender, ToolStripMenuItem).Name = "ToolStripMenuItem7" Then
            model = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
        Else
            model = ""
        End If

        For Each c As DataGridViewCell In Me.GridExpData.SelectedCells

            If Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value IsNot Nothing Then
                If Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value.ToString() <> "" Then
                    P = SystemsOfUnits.Converter.ConvertToSI(Me.cbPunit.SelectedItem.ToString(), Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value)
                Else
                    P = 0
                End If
            Else
                P = 0
            End If
            If Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value IsNot Nothing Then
                If Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value.ToString() <> "" Then
                    T = SystemsOfUnits.Converter.ConvertToSI(Me.cbTunit.SelectedItem.ToString(), Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value)
                Else
                    T = 0
                End If
            Else
                T = 0
            End If
            If Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value IsNot Nothing Then
                If Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value.ToString() <> "" Then
                    x11 = Double.Parse(Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value)
                Else
                    x11 = -1
                End If
            Else
                x11 = -1
            End If
            If Me.GridExpData.Rows(c.RowIndex).Cells("colx2").Value IsNot Nothing Then
                If Me.GridExpData.Rows(c.RowIndex).Cells("colx2").Value.ToString() <> "" Then
                    x12 = Double.Parse(Me.GridExpData.Rows(c.RowIndex).Cells("colx2").Value)
                Else
                    x12 = -1
                End If
            Else
                x12 = -1
            End If
            If Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value IsNot Nothing Then
                If Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value.ToString() <> "" Then
                    y1 = Double.Parse(Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value)
                Else
                    y1 = -1
                End If
            Else
                y1 = -1
            End If

            Dim result As Object = Nothing

            Try

                Select Case c.ColumnIndex
                    Case 0
                        If P = 0.0# Then
                            'T-y => x, P
                            result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(model, 1, T, 1, New Object() {comp1, comp2}, New Double() {y1, 1 - y1}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            P = result(4, 0)
                            x11 = result(2, 1)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbPunit.SelectedItem.ToString, P), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value = Format(x11, "N4")
                        ElseIf T = 0.0# Then
                            'P-y => x, T
                            result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(model, 1, P, 1, New Object() {comp1, comp2}, New Double() {y1, 1 - y1}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            T = result(4, 0)
                            x11 = result(2, 1)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbTunit.SelectedItem.ToString, T), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value = Format(x11, "N4")
                        End If
                    Case 2
                        If P = 0.0# Then
                            'T-x => y, P
                            result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(model, 1, T, 0, New Object() {comp1, comp2}, New Double() {x11, 1 - x11}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            P = result(4, 0)
                            y1 = result(2, 0)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbPunit.SelectedItem.ToString, P), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value = Format(y1, "N4")
                        ElseIf T = 0.0# Then
                            'P-x => y, T
                            result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(model, 1, P, 0, New Object() {comp1, comp2}, New Double() {x11, 1 - x11}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            T = result(4, 0)
                            y1 = result(2, 0)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbTunit.SelectedItem.ToString, T), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value = Format(y1, "N4")
                        End If
                    Case 3
                        If y1 = -1 Then
                            'P-x => y, T
                            result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(model, 1, P, 0, New Object() {comp1, comp2}, New Double() {x11, 1 - x11}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            T = result(4, 0)
                            y1 = result(2, 0)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbTunit.SelectedItem.ToString, T), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value = Format(y1, "N4")
                        ElseIf x11 = -1 Then
                            'P-y => x, T
                            result = ExcelAddIn.ExcelIntegrationNoAttr.PVFFlash(model, 1, P, 1, New Object() {comp1, comp2}, New Double() {y1, 1 - y1}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            P = result(4, 0)
                            x11 = result(2, 1)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colT").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbTunit.SelectedItem.ToString, T), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value = Format(x11, "N4")
                        End If
                    Case 4
                        If y1 = -1 Then
                            'T-x => y, P
                            result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(model, 1, T, 0, New Object() {comp1, comp2}, New Double() {x11, 1 - x11}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            P = result(4, 0)
                            y1 = result(2, 0)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbPunit.SelectedItem.ToString, P), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("coly1").Value = Format(y1, "N4")
                        ElseIf x11 = -1 Then
                            'T-y => x, P
                            result = ExcelAddIn.ExcelIntegrationNoAttr.TVFFlash(model, 1, T, 1, New Object() {comp1, comp2}, New Double() {y1, 1 - y1}, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
                            P = result(4, 0)
                            x11 = result(2, 1)
                            Me.GridExpData.Rows(c.RowIndex).Cells("colP").Value = Format(SystemsOfUnits.Converter.ConvertFromSI(Me.cbPunit.SelectedItem.ToString, P), "N4")
                            Me.GridExpData.Rows(c.RowIndex).Cells("colx1").Value = Format(x11, "N4")
                        End If
                End Select

            Catch ex As Exception
                c.Value = "*"
            End Try

        Next

    End Sub

    Private Sub btnCalcOnce_Click(sender As System.Object, e As System.EventArgs) Handles btnCalcOnce.Click

        cancel = False

        Me.btnDoReg.Enabled = False
        Me.btnCancel.Enabled = True
        Me.btnCalcOnce.Enabled = False

        currcase = Me.StoreCase()

        Dim nvar As Integer = 0

        Dim initval2() As Double = Nothing

        Try

            Select Case currcase.model
                Case "Peng-Robinson"
                    initval2 = New Double() {currcase.iepar1}
                    nvar = 1
                Case "PRSV2-M", "PRSV2-VL"
                    nvar = 2
                    initval2 = New Double() {currcase.iepar1, currcase.iepar2}
                Case "Soave-Redlich-Kwong"
                    initval2 = New Double() {currcase.iepar1}
                    nvar = 1
                Case "UNIQUAC", "Wilson"
                    nvar = 2
                    initval2 = New Double() {currcase.iepar1, currcase.iepar2}
                Case "NRTL"
                    nvar = 3
                    initval2 = New Double() {currcase.iepar1, currcase.iepar2, currcase.iepar3}
                Case "Lee-Kesler-Plöcker"
                    initval2 = New Double() {currcase.iepar1}
                    nvar = 1
            End Select

            Me.tbRegResults.AppendText("Starting one-time calc for " & currcase.model & " model parameter estimation..." & vbCrLf)

            Dim ppm As New Thermodynamics.CAPEOPENManager()

            Select Case currcase.model
                Case "Peng-Robinson"
                    ppname = "Peng-Robinson (PR)"
                Case "Soave-Redlich-Kwong"
                    ppname = "Soave-Redlich-Kwong (SRK)"
                Case "UNIQUAC"
                    ppname = "UNIQUAC"
                Case "PRSV2-M"
                    ppname = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
                Case "PRSV2-VL"
                    ppname = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                Case "NRTL"
                    ppname = "NRTL"
                Case "Lee-Kesler-Plöcker"
                    ppname = "Lee-Kesler-Plöcker"
                Case "Wilson"
                    ppname = "Wilson"
            End Select

            proppack = ppm.GetPropertyPackage(ppname)
            proppack.ComponentName = ppname
            proppack._availablecomps = New Dictionary(Of String, BaseClasses.ConstantProperties)
            For Each kvp In FormMain.AvailableComponents
                proppack._availablecomps.Add(kvp.Key, kvp.Value)
            Next

            proppack.VaporPhaseFugacityCalculationMode = Not chkIdealVaporPhase.Checked
            proppack.ActivityCoefficientModels_IgnoreMissingInteractionParameters = True

            FunctionValue(initval2)

        Catch ex As Exception

            Me.tbRegResults.AppendText(ex.ToString)

        Finally

            currcase.results = Me.tbRegResults.Text

            Me.btnDoReg.Enabled = True
            Me.btnCancel.Enabled = False
            Me.btnCalcOnce.Enabled = True

            GlobalSettings.Settings.CAPEOPENMode = False

        End Try

    End Sub

    Sub DoRegression(initval As Double())

        Dim nvar As Integer = 0
        Dim i As Integer

        Dim initval2() As Double = Nothing
        Dim lconstr2() As Double = Nothing
        Dim uconstr2() As Double = Nothing
        Dim finalval2() As Double = Nothing
        Dim fixed() As Boolean = Nothing

        Select Case currcase.model
            Case "Peng-Robinson"
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1}
                uconstr2 = New Double() {currcase.ulim1}
                fixed = New Boolean() {currcase.fixed1}
                nvar = 1
            Case "PRSV2-M", "PRSV2-VL"
                nvar = 2
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1, currcase.llim2}
                uconstr2 = New Double() {currcase.ulim1, currcase.ulim2}
                fixed = New Boolean() {currcase.fixed1, currcase.fixed2}
            Case "Soave-Redlich-Kwong"
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1}
                uconstr2 = New Double() {currcase.ulim1}
                nvar = 1
                fixed = New Boolean() {currcase.fixed1}
            Case "UNIQUAC", "Wilson"
                nvar = 2
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1, currcase.llim2}
                uconstr2 = New Double() {currcase.ulim1, currcase.ulim2}
                fixed = New Boolean() {currcase.fixed1, currcase.fixed2}
            Case "NRTL"
                nvar = 3
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1, currcase.llim2, currcase.llim3}
                uconstr2 = New Double() {currcase.ulim1, currcase.ulim2, currcase.ulim3}
                fixed = New Boolean() {currcase.fixed1, currcase.fixed2, currcase.fixed3}
            Case "Lee-Kesler-Plöcker"
                initval2 = initval
                lconstr2 = New Double() {currcase.llim1}
                uconstr2 = New Double() {currcase.ulim1}
                nvar = 1
                fixed = New Boolean() {currcase.fixed1}
        End Select

        itn = 0

        Me.tbRegResults.AppendText("Starting experimental data regression for " & currcase.model & " model parameter estimation..." & vbCrLf)

        Dim ppm As New Thermodynamics.CAPEOPENManager()

        Select Case currcase.model
            Case "Peng-Robinson"
                ppname = "Peng-Robinson (PR)"
            Case "Soave-Redlich-Kwong"
                ppname = "Soave-Redlich-Kwong (SRK)"
            Case "UNIQUAC"
                ppname = "UNIQUAC"
            Case "PRSV2-M"
                ppname = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
            Case "PRSV2-VL"
                ppname = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
            Case "NRTL"
                ppname = "NRTL"
            Case "Wilson"
                ppname = "Wilson"
            Case "Lee-Kesler-Plöcker"
                ppname = "Lee-Kesler-Plöcker"
        End Select

        proppack = ppm.GetPropertyPackage(ppname)
        proppack.ComponentName = ppname
        proppack._availablecomps = New Dictionary(Of String, BaseClasses.ConstantProperties)
        For Each kvp In FormMain.AvailableComponents
            proppack._availablecomps.Add(kvp.Key, kvp.Value)
        Next

        proppack.VaporPhaseFugacityCalculationMode = Not chkIdealVaporPhase.Checked
        proppack.ActivityCoefficientModels_IgnoreMissingInteractionParameters = True

        Select Case currcase.method
            Case "Limited Memory BFGS"
                Dim variables(nvar - 1) As Optimization.OptBoundVariable
                For i = 0 To nvar - 1
                    variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), fixed(i), lconstr2(i), uconstr2(i))
                Next
                Dim solver As New Optimization.L_BFGS_B
                solver.Tolerance = currcase.tolerance
                solver.MaxFunEvaluations = currcase.maxits
                solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
            Case "Truncated Newton"
                Dim variables(nvar - 1) As Optimization.OptBoundVariable
                For i = 0 To nvar - 1
                    variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), fixed(i), lconstr2(i), uconstr2(i))
                Next
                Dim solver As New Optimization.TruncatedNewton
                solver.Tolerance = currcase.tolerance
                solver.MaxFunEvaluations = currcase.maxits
                solver.ComputeMin(AddressOf FunctionValue, AddressOf FunctionGradient, variables)
            Case "Nelder-Mead Simplex Downhill"
                Dim variables(nvar - 1) As Optimization.OptBoundVariable
                For i = 0 To nvar - 1
                    variables(i) = New Optimization.OptBoundVariable("x" & CStr(i + 1), initval2(i), fixed(i), lconstr2(i), uconstr2(i))
                Next
                Dim solver As New Optimization.Simplex
                solver.Tolerance = currcase.tolerance
                solver.MaxFunEvaluations = currcase.maxits
                solver.ComputeMin(AddressOf FunctionValue, variables)
            Case "IPOPT"
                For i = 0 To nvar - 1
                    If fixed(i) Then
                        lconstr2(i) = initval2(i)
                        uconstr2(i) = initval2(i)
                    End If
                Next
                Dim obj As Double
                Dim status As IpoptReturnCode
                Using problem As New Ipopt(initval2.Length, lconstr2, uconstr2, 0, Nothing, Nothing,
                 0, 0, AddressOf eval_f, AddressOf eval_g,
                 AddressOf eval_grad_f, AddressOf eval_jac_g, AddressOf eval_h)
                    problem.AddOption("tol", currcase.tolerance)
                    problem.AddOption("max_iter", Convert.ToInt32(currcase.maxits))
                    problem.AddOption("mu_strategy", "adaptive")
                    problem.AddOption("hessian_approximation", "limited-memory")
                    'solve the problem 
                    status = problem.SolveProblem(initval2, obj, Nothing, Nothing, Nothing, Nothing)
                End Using

            Case "Particle Swarm", "Local Unimodal Sampling", "Gradient Descent", "Differential Evolution",
                "Particle Swarm Optimization", "Many Optimizing Liaisons", "Mesh"

                SwarmOps.Globals.Random = New RandomOps.MersenneTwister()

                For i = 0 To nvar - 1
                    If fixed(i) Then
                        lconstr2(i) = initval2(i)
                        uconstr2(i) = initval2(i)
                    End If
                Next

                Dim sproblem As New RegressionProblem(Me) With {._Dim = initval2.Length, ._LB = lconstr2, ._UB = uconstr2, ._INIT = initval2, ._Name = "Regression"}
                sproblem.MaxIterations = currcase.maxits * initval2.Length
                sproblem.MinIterations = currcase.maxits
                sproblem.Tolerance = currcase.tolerance
                Dim opt As SwarmOps.Optimizer = GetSolver(currcase.method)
                opt.Problem = sproblem
                opt.RequireFeasible = True
                Dim sresult = opt.Optimize(opt.DefaultParameters)

                If Not sresult.Feasible Then Throw New Exception("Error: Feasible solution not found after " & sresult.Iterations & " iterations.")

                'initval2 = sresult.Parameters

        End Select

        Me.tbRegResults.AppendText("Finished!")

    End Sub

    Private Sub chkDoTDepRegression_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkDoTDepRegression.CheckedChanged

        If chkDoTDepRegression.Checked Then
            If cbModel.SelectedItem.ToString = "NRTL" Then
                gridInEst.Rows(2).Cells(4).Value = True
            End If
        End If

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Select Case cbModel.SelectedItem.ToString
            Case "NRTL"
                Try
                    Dim estimates As Double() = EstimateNRTL(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "MODFAC-NIST")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                    Me.gridInEst.Rows(2).Cells(2).Value = estimates(2)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
            Case "UNIQUAC"
                Try
                    Dim estimates As Double() = EstimateUNIQUAC(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "MODFAC-NIST")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
            Case "Wilson"
                Try
                    Dim estimates As Double() = EstimateWilson(cbCompound1.SelectedItem.ToString, cbCompound2.SelectedItem.ToString, "MODFAC-NIST")
                    Me.gridInEst.Rows(0).Cells(2).Value = estimates(0)
                    Me.gridInEst.Rows(1).Cells(2).Value = estimates(1)
                Catch ex As Exception
                    MessageBox.Show(ex.ToString, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
                Finally
                    Cursor = Cursors.Default
                End Try
        End Select
    End Sub

    Private Sub GridExpData_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles GridExpData.CellValidating
        If e.ColumnIndex > 0 Then DirectCast(sender, DataGridView).ValidateCellForDouble(e)
    End Sub

    Private Sub btnSearchKDB_Click(sender As Object, e As EventArgs) Handles btnSearchKDB.Click

        Dim fsearch As New FormSearchingKDB()

        Dim fresult As New FormSearchKDBVLE()

        Dim comp1, comp2 As String

        Dim swap, replace As Boolean
        swap = False
        replace = False

        comp1 = cbCompound1.SelectedItem.ToString
        comp2 = cbCompound2.SelectedItem.ToString

        fresult.Show()
        fresult.Enabled = False
        fsearch.Show()

        Dim sets As New List(Of String())

        Dim tcs As New Threading.CancellationTokenSource()

        Dim t As New Task(Of List(Of String()))(Function()
                                                    Dim id1 = KDBParser.GetCompoundIDs(comp1, True)
                                                    Dim id2 = KDBParser.GetCompoundIDs(comp2, True)
                                                    Return KDBParser.GetBinaryVLESetIDs(id1(0)(0), id2(0)(0))
                                                End Function, tcs.Token)

        t.ContinueWith(Sub()
                           UIThread(Sub()
                                        fsearch.Close()
                                        If DWSIM.App.IsRunningOnMono Then
                                            fsearch.Hide()
                                            fsearch.Close()
                                        End If
                                        If t.Exception Is Nothing Then
                                            fresult.Enabled = True
                                            fresult.ListBox1.Items.Clear()
                                            sets = t.Result
                                            fresult.lblRecords.Text = t.Result.Count
                                            For Each item In t.Result
                                                fresult.ListBox1.Items.Add(item(0).PadRight(10) + item(1))
                                            Next
                                            If t.Result.Count = 0 Then
                                                fresult.Button1.Enabled = False
                                            Else
                                                fresult.Button1.Enabled = True
                                            End If
                                        Else
                                            fresult.Close()
                                            MessageBox.Show(t.Exception.GetBaseException.Message, DWSIM.App.GetLocalString("Erro"))
                                        End If
                                    End Sub)
                       End Sub)

        AddHandler fsearch.btnCancel.Click, Sub()
                                                fsearch.Close()
                                                fresult.Close()
                                                tcs.Cancel()
                                            End Sub

        t.Start()

        AddHandler fresult.Button1.Click, Sub()
                                              Dim tcs2 As New Threading.CancellationTokenSource()
                                              Dim vleid As Integer = sets(fresult.ListBox1.SelectedIndex)(0)
                                              Dim t2 As New Task(Of KDBVLEDataSet)(Function() KDBParser.GetVLEData(vleid), tcs2.Token)
                                              Dim fsearch2 As New FormSearchingKDB()
                                              AddHandler fsearch2.btnCancel.Click, Sub()
                                                                                       fsearch2.Close()
                                                                                       tcs2.Cancel()
                                                                                   End Sub
                                              t2.ContinueWith(Sub()
                                                                  UIThread(Sub()
                                                                               swap = fresult.CheckBox1.Checked
                                                                               replace = fresult.CheckBox2.Checked
                                                                               fsearch2.Close()
                                                                               If DWSIM.App.IsRunningOnMono Then
                                                                                   fsearch2.Hide()
                                                                                   fsearch2.Close()
                                                                               End If
                                                                               If t2.Exception Is Nothing Then
                                                                                   If Not t2.Result Is Nothing Then
                                                                                       If replace Then Me.GridExpData.Rows.Clear()
                                                                                       For Each record In t2.Result.Data
                                                                                           If swap Then
                                                                                               Me.GridExpData.Rows.Add(True, 1.0# - record.X, "", 1.0# - record.Y, record.T, "", "", record.P)
                                                                                           Else
                                                                                               Me.GridExpData.Rows.Add(True, record.X, "", record.Y, record.T, "", "", record.P)
                                                                                           End If
                                                                                       Next
                                                                                       cbTunit.SelectedItem = t2.Result.Tunits
                                                                                       cbPunit.SelectedItem = t2.Result.Punits
                                                                                   End If
                                                                               Else
                                                                                   MessageBox.Show(t2.Exception.GetBaseException.Message, DWSIM.App.GetLocalString("Erro"))
                                                                               End If
                                                                           End Sub)
                                                              End Sub, TaskContinuationOptions.ExecuteSynchronously)
                                              t2.Start()
                                              fresult.Close()
                                              fsearch2.Show()
                                          End Sub


    End Sub

    Private Sub SalvarEmBancoDeDadosXMLToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SalvarEmBancoDeDadosXMLToolStripMenuItem.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("XML File", "*.xml")})

        If handler IsNot Nothing Then
            'Save Regression results to database
            If IP.Parameters.Count > 0 Then
                With currcase
                    IP.Comp1 = .comp1
                    IP.Comp2 = .comp2
                    IP.Model = .model
                    IP.DataType = [Enum].GetName(GetType(DataType), .datatype)
                    IP.Description = .description
                    IP.RegressionFile = .filename
                End With
                currcase.databasepath = handler.FullPath
                Try
                    Using stream As New MemoryStream
                        If handler.Exists() Then
                            Using str = handler.OpenRead()
                                str.CopyTo(stream)
                                stream.Position = 0
                            End Using
                        End If
                        Global.DWSIM.Thermodynamics.Databases.UserIPDB.AddInteractionParameters(New InteractionParameter() {IP}, stream, True)
                        handler.Write(stream)
                    End Using
                    MessageBox.Show(DWSIM.App.GetLocalString("ParametrosAdicionadosComSucesso"))
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erroaosalvararquivo") & ex.Message.ToString, DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Else
                MessageBox.Show(DWSIM.App.GetLocalString("NoRegParmsAvail"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub btnTransfere_Click(sender As Object, e As EventArgs) Handles btnTransfere.Click
        If IP IsNot Nothing Then
            Dim i As Integer

            For Each param In IP.Parameters
                gridInEst.Rows(i).Cells(2).Value = param.Value
                i += 1
            Next

            btnTransfere.Enabled = False
        End If
    End Sub
End Class

Public Class RegressionProblem

    Inherits SwarmOps.Problem

    Public _Dim As Integer, _LB(), _UB(), _INIT() As Double, _Name As String

    Private _f As FormDataRegression
    Private _fit As Double

    Sub New(f As FormDataRegression)
        _f = f
    End Sub

    Public Overrides ReadOnly Property Dimensionality As Integer
        Get
            Return _Dim
        End Get
    End Property

    Public Overrides ReadOnly Property LowerBound As Double()
        Get
            Return _LB
        End Get
    End Property

    Public Overrides ReadOnly Property LowerInit As Double()
        Get
            Return _INIT
        End Get
    End Property
    Public Overrides ReadOnly Property UpperInit As Double()
        Get
            Return _INIT
        End Get
    End Property

    Public Overrides ReadOnly Property MinFitness As Double
        Get
            Return Double.MinValue
        End Get
    End Property

    Public Overrides ReadOnly Property Name As String
        Get
            Return _Name
        End Get
    End Property

    Public Overrides ReadOnly Property UpperBound As Double()
        Get
            Return _UB
        End Get
    End Property

    Public Overrides ReadOnly Property HasGradient As Boolean
        Get
            Return True
        End Get
    End Property

    Public Overrides Function Gradient(x() As Double, ByRef v() As Double) As Integer

        v = _f.FunctionGradient(x)

        Return 0

    End Function

    Public Overrides Function Fitness(parameters() As Double) As Double

        Return _f.FunctionValue(parameters)

    End Function

End Class


Namespace DWSIM.Optimization.DatRegression


    <System.Serializable()> Public Class RegressionCase
        Public comp1, comp2, comp3 As String
        Public filename As String = ""
        Public databasepath As String = ""
        Public model As String = "Peng-Robinson"
        Public datatype As DataType = DataType.Pxy
        Public tp, x1p, x2p, yp, pp, calct, calcp, calcy, calcx1l1, calcx1l2, checkp, ts, tl, calcts, calctl As New ArrayList
        Public method As String = "IPOPT"
        Public objfunction As String = "Least Squares (min T/P)"
        Public includesd As Boolean = False
        Public results As String = ""
        Public advsettings As Object = Nothing
        Public tunit As String = "C"
        Public punit As String = "bar"
        Public cunit As String = ""
        Public tolerance As Double = 0.00001
        Public maxits As Double = 250
        Public iepar1 As Double = 0.0#
        Public iepar2 As Double = 0.0#
        Public iepar3 As Double = 0.0#
        Public llim1 As Double = 0.0#
        Public llim2 As Double = 0.0#
        Public llim3 As Double = 0.0#
        Public ulim1 As Double = 0.0#
        Public ulim2 As Double = 0.0#
        Public ulim3 As Double = 0.0#
        Public fixed1 As Boolean = False
        Public fixed2 As Boolean = False
        Public fixed3 As Boolean = False
        Public title As String = ""
        Public description As String = ""
        Public idealvapormodel As Boolean = True
        Public useTLdata As Boolean = True
        Public useTSdata As Boolean = True
    End Class

    Public Enum DataType
        Txy = 0
        Pxy = 1
        TPxy = 2
        Txx = 3
        Pxx = 4
        TPxx = 5
        TTxSE = 6
        TTxSS = 7
    End Enum

End Namespace
