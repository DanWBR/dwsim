'    Sensitivity Analysis Classes
'    Copyright 2009 Daniel Wagner O. de Medeiros
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
Imports DWSIM.DWSIM.Optimization
Imports DWSIM.DrawingTools
Imports Ciloci.Flee
Imports DWSIM.FlowsheetSolver
Imports System.Linq
Imports DWSIM.SharedClasses.Flowsheet.Optimization
Imports DWSIM.SharedClasses.DWSIM.Flowsheet

Public Class FormSensAnalysis

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public nf As String
    Public su As SystemsOfUnits.Units
    Public cv As SystemsOfUnits.Converter
    Public form As FormFlowsheet

    Public abortCalc As Boolean = False
    Public selectedindex As Integer = -1
    Public selectedsacase As SensitivityAnalysisCase
    Private selected As Boolean = False
    Private EnableAutoSave As Boolean = True

    Public cbc2, cbc3, cbc0, cbc1 As DataGridViewComboBoxCell

    Private Sub FormSensAnalysis_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.TabText = Me.Text

        If Not Me.DockHandler Is Nothing OrElse Not Me.DockHandler.FloatPane Is Nothing Then
            ' set the bounds of this form's FloatWindow to our desired position and size
            If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
                Dim floatWin = Me.DockHandler.FloatPane.FloatWindow
                If Not floatWin Is Nothing Then
                    floatWin.SetBounds(floatWin.Location.X, floatWin.Location.Y, 930, 568)
                End If
            End If
        End If

        form = My.Application.ActiveSimulation

        cv = New SystemsOfUnits.Converter
        su = form.Options.SelectedUnitSystem
        nf = form.Options.NumberFormat

        Me.lbCases.Items.Clear()

        If form.Collections.OPT_SensAnalysisCollection Is Nothing Then form.Collections.OPT_SensAnalysisCollection = New List(Of SensitivityAnalysisCase)

        For Each sacase As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
            Me.lbCases.Items.Add(sacase.name)
        Next

        Me.cbObjIndVar1.Items.Clear()
        Me.cbObjIndVar2.Items.Clear()
        Me.cbPropIndVar1.Items.Clear()
        Me.cbPropIndVar2.Items.Clear()

        Me.cbObjIndVar1.Items.Add(DWSIM.App.GetLocalString("SpreadsheetCell"))
        Me.cbObjIndVar2.Items.Add(DWSIM.App.GetLocalString("SpreadsheetCell"))
        Me.cbObjIndVar1.Items.Add(DWSIM.App.GetLocalString("ReactionProperty"))
        Me.cbObjIndVar2.Items.Add(DWSIM.App.GetLocalString("ReactionProperty"))
        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            Me.cbObjIndVar1.Items.Add(obj.GraphicObject.Tag)
            Me.cbObjIndVar2.Items.Add(obj.GraphicObject.Tag)
        Next

        cbc0 = New DataGridViewComboBoxCell
        cbc0.Sorted = True
        cbc0.MaxDropDownItems = 10
        cbc0.Items.Add(DWSIM.App.GetLocalString("SpreadsheetCell"))
        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            cbc0.Items.Add(obj.GraphicObject.Tag)
        Next
        cbc1 = New DataGridViewComboBoxCell
        cbc1.MaxDropDownItems = 10

        cbc2 = New DataGridViewComboBoxCell
        cbc2.Sorted = True
        cbc2.MaxDropDownItems = 10
        cbc2.Items.Add(DWSIM.App.GetLocalString("SpreadsheetCell"))
        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            cbc2.Items.Add(obj.GraphicObject.Tag)
        Next
        cbc3 = New DataGridViewComboBoxCell
        cbc3.MaxDropDownItems = 10

        Dim tbc1 As New DataGridViewTextBoxCell()
        Dim tbc2 As New DataGridViewTextBoxCell()
        Dim tbc3 As New DataGridViewTextBoxCell()
        With tbc1
            .Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With
        With tbc2
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
        End With
        With tbc3
            .Style.Alignment = DataGridViewContentAlignment.MiddleCenter
            .Style.BackColor = Color.FromKnownColor(KnownColor.Control)
        End With

        With Me.dgDepVariables
            .Columns(1).CellTemplate = cbc0
            .Columns(2).CellTemplate = cbc1
        End With

        With Me.dgVariables
            .Columns(0).CellTemplate = tbc1
            .Columns(1).CellTemplate = tbc1
            .Columns(2).CellTemplate = cbc2
            .Columns(3).CellTemplate = cbc3
            .Columns(4).CellTemplate = tbc2
            .Columns(5).CellTemplate = tbc3
            .Columns(1).HeaderCell.Style.Alignment = DataGridViewContentAlignment.MiddleLeft
        End With

        If Me.lbCases.Items.Count > 0 Then Me.lbCases.SelectedIndex = Me.lbCases.Items.Count - 1

        form.WriteToLog(DWSIM.App.GetLocalTipString("FSAN001"), Color.Black, MessageType.Tip)

    End Sub

    Private Function ReturnProperties(ByVal objectTAG As String, ByVal dependent As Boolean) As String()

        If objectTAG = DWSIM.App.GetLocalString("SpreadsheetCell") Then
            Return form.FormSpreadsheet.GetCellString()
        ElseIf objectTAG = DWSIM.App.GetLocalString("ReactionProperty") Then
            Dim rprops As New List(Of String)
            For Each rx In form.Reactions.Values
                Dim props As String() = rx.GetPropertyList
                For Each p In props
                    rprops.Add(rx.Name & "|" & p)
                Next
            Next
            Return rprops.ToArray
        Else
            For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
                If objectTAG = obj.GraphicObject.Tag Then
                    If dependent Then
                        Return obj.GetProperties(Interfaces.Enums.PropertyType.ALL)
                    Else
                        Return obj.GetProperties(Interfaces.Enums.PropertyType.WR)
                    End If
                    Exit Function
                End If
            Next
        End If

        Return Nothing

    End Function

    Private Sub cbObjIndVar2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbObjIndVar2.SelectedIndexChanged
        Me.cbPropIndVar2.Items.Clear()
        If Not Me.cbObjIndVar2.SelectedItem Is Nothing Then
            If Me.cbObjIndVar2.SelectedItem.ToString <> "" Then
                Dim props As String() = Me.ReturnProperties(Me.cbObjIndVar2.SelectedItem.ToString, False)
                For Each prop As String In props
                    Me.cbPropIndVar2.Items.Add(form.GetTranslatedString1(prop))
                Next
            End If
        End If
    End Sub

    Private Sub cbObjIndVar1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbObjIndVar1.SelectedIndexChanged
        Me.cbPropIndVar1.Items.Clear()
        If Not Me.cbObjIndVar1.SelectedItem Is Nothing Then
            If Me.cbObjIndVar1.SelectedItem.ToString <> "" Then
                Dim props As String() = Me.ReturnProperties(Me.cbObjIndVar1.SelectedItem.ToString, False)
                For Each prop As String In props
                    Me.cbPropIndVar1.Items.Add(form.GetTranslatedString1(prop))
                Next
            End If
        End If
    End Sub

    Private Sub cbPropIndVar1_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbPropIndVar1.SelectedIndexChanged
        Dim props As String() = Me.ReturnProperties(Me.cbObjIndVar1.SelectedItem.ToString, False)
        If Me.cbPropIndVar1.SelectedItem <> "" Then
            btnRun.Enabled = True
            chkIndVar2.Enabled = True
        Else
            btnRun.Enabled = False
            chkIndVar2.Enabled = False
        End If
        If Me.cbObjIndVar1.SelectedItem.ToString <> DWSIM.App.GetLocalString("SpreadsheetCell") And
            Me.cbObjIndVar1.SelectedItem.ToString <> DWSIM.App.GetLocalString("ReactionProperty") Then
            For Each prop As String In props
                If form.GetTranslatedString1(prop) = Me.cbPropIndVar1.SelectedItem.ToString Then
                    For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
                        If Me.cbObjIndVar1.SelectedItem.ToString = obj.GraphicObject.Tag Then
                            Me.tbUnitIndVar1.Text = obj.GetPropertyUnit(prop, su)
                            Me.tbCurrValIndVar1.Text = obj.GetPropertyValue(prop, su)
                            If EnableAutoSave Then SaveForm(selectedsacase)
                            Exit Sub
                        End If
                    Next
                End If
            Next
        Else
            If EnableAutoSave Then SaveForm(selectedsacase)
        End If
    End Sub

    Private Sub cbPropIndVar2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbPropIndVar2.SelectedIndexChanged
        Dim props As String() = Me.ReturnProperties(Me.cbObjIndVar2.SelectedItem.ToString, False)
        If Me.cbObjIndVar2.SelectedItem.ToString <> DWSIM.App.GetLocalString("SpreadsheetCell") And
            Me.cbObjIndVar1.SelectedItem.ToString <> DWSIM.App.GetLocalString("ReactionProperty") Then
            For Each prop As String In props
                If form.GetTranslatedString1(prop) = Me.cbPropIndVar2.SelectedItem.ToString Then
                    For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
                        If Me.cbObjIndVar2.SelectedItem.ToString = obj.GraphicObject.Tag Then
                            Me.tbUnitIndVar2.Text = obj.GetPropertyUnit(prop, su)
                            Me.tbCurrValIndVar2.Text = obj.GetPropertyValue(prop, su)
                            If EnableAutoSave Then SaveForm(selectedsacase)
                            Exit Sub
                        End If
                    Next
                End If
            Next
        Else
            If EnableAutoSave Then SaveForm(selectedsacase)
        End If
    End Sub

    Private Function GetNameIndex(ByVal N As String) As Integer

        Dim i As Integer

        For Each s As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
            If s.name = N Then Return i
            i += 1
        Next

        Return -1

    End Function

    Private Sub btnNewCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnNewCase.Click

        Dim sacase As New SensitivityAnalysisCase
        Dim n As Integer = form.Collections.OPT_SensAnalysisCollection.Count

        Do
            If GetNameIndex("SACase" & n) < 0 Then Exit Do
            n += 1
        Loop

        sacase.name = "SACase" & n

        form.Collections.OPT_SensAnalysisCollection.Add(sacase)

        Me.lbCases.Items.Add(sacase.name)
        Me.lbCases.SelectedItem = sacase.name

    End Sub

    Private Sub PopulateForm(ByRef sacase As SensitivityAnalysisCase)
        EnableAutoSave = False 'disable automatic saving during populating on changing of fields
        With sacase
            Me.tbCaseName.Text = sacase.name
            Me.tbCaseDesc.Text = sacase.description
            With sacase.iv1
                If .objectID <> "SpreadsheetCell" And .objectID <> "ReactionProperty" Then
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(.objectID) Then
                        Me.cbObjIndVar1.SelectedIndex = Me.cbObjIndVar1.Items.IndexOf(form.Collections.FlowsheetObjectCollection(.objectID).GraphicObject.Tag)
                        Me.cbPropIndVar1.SelectedIndex = Me.cbPropIndVar1.Items.IndexOf(form.GetTranslatedString1(.propID))
                    Else
                        Me.cbObjIndVar1.SelectedIndex = -1
                        Me.cbPropIndVar1.SelectedIndex = -1
                        Me.tbUnitIndVar1.Text = ""
                    End If
                ElseIf .objectID = "ReactionProperty" Then
                    Me.cbObjIndVar1.SelectedIndex = Me.cbObjIndVar1.Items.IndexOf(DWSIM.App.GetLocalString("ReactionProperty"))
                    Me.cbPropIndVar1.SelectedIndex = Me.cbPropIndVar1.Items.IndexOf(form.GetTranslatedString1(.propID))
                Else
                    Me.cbObjIndVar1.SelectedIndex = Me.cbObjIndVar1.Items.IndexOf(DWSIM.App.GetLocalString("SpreadsheetCell"))
                    Me.cbPropIndVar1.SelectedIndex = Me.cbPropIndVar1.Items.IndexOf(form.GetTranslatedString1(.propID))
                End If
                If .propID <> "" Then
                    btnRun.Enabled = True
                    chkIndVar2.Enabled = True
                Else
                    btnRun.Enabled = False
                    chkIndVar2.Enabled = False
                End If
                Me.tbLowerLimIndVar1.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(Me.tbUnitIndVar1.Text, .lowerlimit.GetValueOrDefault)
                Me.tbUpperLimIndVar1.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(Me.tbUnitIndVar1.Text, .upperlimit.GetValueOrDefault)
                Me.nuNumPointsIndVar1.Value = .points
            End With
            With sacase.iv2
                If .objectID <> "SpreadsheetCell" And .objectID <> "ReactionProperty" Then
                    If form.Collections.FlowsheetObjectCollection.ContainsKey(.objectID) Then
                        Me.cbObjIndVar2.SelectedIndex = Me.cbObjIndVar2.Items.IndexOf(form.Collections.FlowsheetObjectCollection(.objectID).GraphicObject.Tag)
                        Me.cbPropIndVar2.SelectedIndex = Me.cbPropIndVar2.Items.IndexOf(form.GetTranslatedString1(.propID))
                    Else
                        Me.cbObjIndVar2.SelectedIndex = -1
                        Me.cbPropIndVar2.SelectedIndex = -1
                        Me.tbUnitIndVar2.Text = ""
                    End If
                ElseIf .objectID = "ReactionProperty" Then
                    Me.cbObjIndVar2.SelectedIndex = Me.cbObjIndVar2.Items.IndexOf(DWSIM.App.GetLocalString("ReactionProperty"))
                    Me.cbPropIndVar2.SelectedIndex = Me.cbPropIndVar2.Items.IndexOf(form.GetTranslatedString1(.propID))
                Else
                    Me.cbObjIndVar2.SelectedIndex = Me.cbObjIndVar2.Items.IndexOf(DWSIM.App.GetLocalString("SpreadsheetCell"))
                    Me.cbPropIndVar2.SelectedItem = form.GetTranslatedString1(.propID)
                End If
                Me.tbLowerLimIndVar2.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(Me.tbUnitIndVar2.Text, .lowerlimit.GetValueOrDefault)
                Me.tbUpperLimIndVar2.Text = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(Me.tbUnitIndVar2.Text, .upperlimit.GetValueOrDefault)
                Me.nuNumPointsIndVar2.Value = .points
            End With
            Me.dgDepVariables.Rows.Clear()
            For Each var As SAVariable In .depvariables.Values
                With var
                    Me.dgDepVariables.Rows.Add()
                    Dim dgrow As DataGridViewRow = Me.dgDepVariables.Rows(Me.dgDepVariables.Rows.Count - 1)
                    dgrow.Cells(0).Value = .id
                    If .objectID <> "SpreadsheetCell" Then
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(.objectID) Then
                            dgrow.Cells(1).Value = form.Collections.FlowsheetObjectCollection(.objectID).GraphicObject.Tag
                            dgrow.Cells(2).Value = form.GetTranslatedString1(.propID)
                        End If
                    Else
                        dgrow.Cells(1).Value = DWSIM.App.GetLocalString("SpreadsheetCell")
                        dgrow.Cells(2).Value = form.GetTranslatedString1(.propID)
                    End If
                    dgrow.Cells(3).Value = .unit
                End With
            Next
            Me.dgvResults.Rows.Clear()
            For Each result As Double() In .results
                Me.dgvResults.Rows.Add(New Object() {Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv1.unit, result(0)), nf), Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv2.unit, result(1)), nf), Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.dv.unit, result(2)), nf)})
            Next
            Me.tbStats.Text = .stats
            Me.tbExpression.Text = .expression
            If .numvar = 2 Then Me.chkIndVar2.Checked = True Else Me.chkIndVar2.Checked = False
            If .depvartype = SADependentVariableType.Expression Then rbExp.Checked = True Else rbExp.Checked = False
            Me.dgVariables.Rows.Clear()
            For Each var As SAVariable In .variables.Values
                With var
                    Me.dgVariables.Rows.Add()
                    Dim dgrow As DataGridViewRow = Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1)
                    dgrow.Cells(0).Value = .id
                    dgrow.Cells(1).Value = .name
                    If .objectID <> "SpreadsheetCell" Then
                        If form.Collections.FlowsheetObjectCollection.ContainsKey(.objectID) Then
                            dgrow.Cells(2).Value = form.Collections.FlowsheetObjectCollection(.objectID).GraphicObject.Tag
                            dgrow.Cells(3).Value = form.GetTranslatedString1(.propID)
                        End If
                    Else
                        dgrow.Cells(2).Value = DWSIM.App.GetLocalString("SpreadsheetCell")
                        dgrow.Cells(3).Value = form.GetTranslatedString1(.propID)
                    End If
                    dgrow.Cells(4).Value = SystemsOfUnits.Converter.ConvertFromSI(.unit, .currentvalue)
                    dgrow.Cells(5).Value = .unit
                End With
            Next
        End With
        graph.GraphPane.CurveList.Clear()
        BtnDrawChart.Enabled = False
        EnableAutoSave = True
    End Sub

    Private Sub lbCases_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lbCases.SelectedIndexChanged

        If Me.selectedindex <> Me.lbCases.SelectedIndex Then

            Me.selectedindex = Me.lbCases.SelectedIndex

            If Not Me.lbCases.SelectedItem Is Nothing Then
                For Each sacase As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
                    If sacase.name = Me.lbCases.SelectedItem.ToString Then
                        Me.selectedsacase = sacase
                        Me.PopulateForm(sacase)
                        Exit For
                    End If
                Next
                'TabPage2.Enabled = True
                'TabPage3.Enabled = True
                GroupBox8.Enabled = True
                GroupBox9.Enabled = True
                btnRun.Enabled = True
                'gbExp.Enabled = True
            Else
                'TabPage2.Enabled = False
                'TabPage3.Enabled = False
                'gbExp.Enabled = False
                GroupBox8.Enabled = False
                GroupBox9.Enabled = False
                btnRun.Enabled = False
            End If

            selected = True

        End If

    End Sub

    Private Sub btnCopyCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCopyCase.Click

        Dim sacase2 As New SensitivityAnalysisCase

        Dim sacase = form.Collections.OPT_SensAnalysisCollection(Me.lbCases.SelectedIndex)
        sacase2 = sacase.Clone
        sacase2.name = sacase.name & "_1"

        Me.lbCases.Items.Add(sacase2.name)
        Me.lbCases.SelectedItem = sacase2.name
        form.Collections.OPT_SensAnalysisCollection.Add(sacase2)

    End Sub

    Private Sub btnSaveCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnSaveCase.Click

        Dim prevselected = lbCases.SelectedIndex

        For i As Integer = 0 To lbCases.Items.Count - 1
            lbCases.SelectedIndex = i
            Dim sacase = form.Collections.OPT_SensAnalysisCollection(Me.lbCases.SelectedIndex)
            SaveForm(sacase)
        Next

        lbCases.SelectedIndex = prevselected

    End Sub

    Private Sub SaveForm(ByRef sacase As SensitivityAnalysisCase)

        With sacase
            sacase.name = Me.tbCaseName.Text
            sacase.description = Me.tbCaseDesc.Text
            With sacase.iv1
                If Me.cbObjIndVar1.SelectedItem Is Nothing Then
                    .objectTAG = ""
                    .objectID = ""
                ElseIf Me.cbObjIndVar1.SelectedItem.ToString <> DWSIM.App.GetLocalString("SpreadsheetCell") And
                        Me.cbObjIndVar1.SelectedItem.ToString <> DWSIM.App.GetLocalString("ReactionProperty") Then
                    .objectTAG = Me.cbObjIndVar1.SelectedItem.ToString
                    .objectID = CType(FormFlowsheet.SearchSurfaceObjectsByTag(.objectTAG, form.FormSurface.FlowsheetSurface), Drawing.SkiaSharp.GraphicObjects.GraphicObject).Name
                ElseIf Me.cbObjIndVar1.SelectedItem.ToString = DWSIM.App.GetLocalString("ReactionProperty") Then
                    .objectTAG = Me.cbObjIndVar1.SelectedItem.ToString
                    .objectID = "ReactionProperty"
                Else
                    .objectTAG = Me.cbObjIndVar1.SelectedItem.ToString
                    .objectID = "SpreadsheetCell"
                End If
                If .objectTAG <> "" Then
                    Dim props As String() = Me.ReturnProperties(.objectTAG, False)
                    For Each prop As String In props
                        If form.GetTranslatedString1(prop) = Me.cbPropIndVar1.SelectedItem.ToString Then
                            .propID = prop
                            Exit For
                        End If
                    Next
                Else
                    .propID = ""
                End If
                .unit = Me.tbUnitIndVar1.Text
                .lowerlimit = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(.unit, Double.Parse(Me.tbLowerLimIndVar1.Text))
                .upperlimit = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(.unit, Double.Parse(Me.tbUpperLimIndVar1.Text))
                .points = Me.nuNumPointsIndVar1.Value
            End With
            If Me.chkIndVar2.Checked And cbPropIndVar2.SelectedItem <> Nothing Then
                With sacase.iv2
                    If Me.cbObjIndVar2.SelectedItem Is Nothing Then
                        .objectTAG = ""
                        .objectID = ""
                    ElseIf Me.cbObjIndVar2.SelectedItem.ToString <> DWSIM.App.GetLocalString("SpreadsheetCell") And
                        Me.cbObjIndVar2.SelectedItem.ToString <> DWSIM.App.GetLocalString("ReactionProperty") Then
                        .objectTAG = Me.cbObjIndVar2.SelectedItem
                        .objectID = CType(FormFlowsheet.SearchSurfaceObjectsByTag(.objectTAG, form.FormSurface.FlowsheetSurface), Drawing.SkiaSharp.GraphicObjects.GraphicObject).Name
                    ElseIf Me.cbObjIndVar2.SelectedItem.ToString = DWSIM.App.GetLocalString("ReactionProperty") Then
                        .objectTAG = Me.cbObjIndVar2.SelectedItem.ToString
                        .objectID = "ReactionProperty"
                    Else
                        .objectTAG = Me.cbObjIndVar2.SelectedItem.ToString
                        .objectID = "SpreadsheetCell"
                    End If
                    If .objectTAG <> "" Then
                        Dim props As String() = Me.ReturnProperties(.objectTAG, False)
                        For Each prop As String In props
                            If form.GetTranslatedString1(prop) = Me.cbPropIndVar2.SelectedItem.ToString Then
                                .propID = prop
                                Exit For
                            End If
                        Next
                    Else
                        .propID = ""
                    End If
                    .lowerlimit = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(.unit, Double.Parse(Me.tbLowerLimIndVar2.Text))
                    .upperlimit = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(.unit, Double.Parse(Me.tbUpperLimIndVar2.Text))
                    .points = Me.nuNumPointsIndVar2.Value
                    .unit = Me.tbUnitIndVar2.Text
                End With
            End If
            If Me.rbVar.Checked Then
                If .depvariables Is Nothing Then .depvariables = New Dictionary(Of String, SAVariable)
                .depvariables.Clear()
                For Each dgrow As DataGridViewRow In Me.dgDepVariables.Rows
                    Dim var As New SAVariable
                    With var
                        .id = dgrow.Cells(0).Value
                        If dgrow.Cells(1).Value = Nothing Then
                            .objectTAG = ""
                            .objectID = ""
                        ElseIf dgrow.Cells(1).Value <> DWSIM.App.GetLocalString("SpreadsheetCell") Then
                            .objectTAG = dgrow.Cells(1).Value
                            .objectID = Me.ReturnObject(dgrow.Cells(1).Value).Name
                        Else
                            .objectTAG = dgrow.Cells(1).Value
                            .objectID = "SpreadsheetCell"
                        End If
                        .propID = Me.ReturnPropertyID(.objectID, dgrow.Cells(2).Value)
                        .unit = dgrow.Cells(3).Value
                    End With
                    .depvariables.Add(var.id, var)
                Next
            End If
            Try
                .results.Clear()
                For Each row As DataGridViewRow In Me.dgvResults.Rows
                    .results.Add(New Double() {row.Cells(0).Value, row.Cells(1).Value, row.Cells(2).Value})
                Next
            Catch ex As Exception
                form.WriteToLog(ex.Message, Color.BurlyWood, MessageType.Warning)
            End Try
            .stats = Me.tbStats.Text
            If Me.chkIndVar2.Checked Then .numvar = 2 Else .numvar = 1
            .expression = Me.tbExpression.Text
            .variables.Clear()
            If Me.rbExp.Checked Then .depvartype = SADependentVariableType.Expression Else .depvartype = SADependentVariableType.Variable
            For Each dgrow As DataGridViewRow In Me.dgVariables.Rows
                Dim var As New SAVariable
                With var
                    .id = dgrow.Cells(0).Value
                    .name = dgrow.Cells(1).Value
                    If dgrow.Cells(1).Value <> DWSIM.App.GetLocalString("SpreadsheetCell") Then
                        .objectTAG = dgrow.Cells(2).Value
                        .objectID = Me.ReturnObject(dgrow.Cells(2).Value).Name
                    Else
                        .objectTAG = dgrow.Cells(2).Value
                        .objectID = "SpreadsheetCell"
                    End If
                    .propID = Me.ReturnPropertyID(.objectID, dgrow.Cells(3).Value)
                    .currentvalue = SystemsOfUnits.Converter.ConvertToSI(dgrow.Cells(5).Value, dgrow.Cells(4).Value)
                    .unit = dgrow.Cells(5).Value
                End With
                .variables.Add(var.id, var)
            Next
        End With

    End Sub

    Private Sub btnDeleteCase_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDeleteCase.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.Yes Then
            form.Collections.OPT_SensAnalysisCollection.RemoveAt(lbCases.SelectedIndex)
            Me.lbCases.Items.Remove(Me.lbCases.SelectedItem)
            If lbCases.Items.Count > 0 Then Me.lbCases.SelectedIndex = 0
        End If
    End Sub

    Private Sub btnRun_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRun.Click

        Dim idx As Integer = 0
        Dim iv1ll, iv1ul, iv1np, iv2ll, iv2ul, iv2np, dvval, iv1val, iv2val, iv1val0, iv2val0 As Double
        Dim iv1id, iv2id, iv1prop, iv2prop, dvid, dvprop As String
        Dim i, j, counter As Integer
        Dim res As New ArrayList

        Me.lbCases.SelectedIndex = Me.selectedindex

        For Each sacase2 As SensitivityAnalysisCase In form.Collections.OPT_SensAnalysisCollection
            If sacase2.name = Me.lbCases.SelectedItem.ToString Then
                idx = form.Collections.OPT_SensAnalysisCollection.IndexOf(sacase2)
                Exit For
            End If
        Next

        Dim sacase As SensitivityAnalysisCase = form.Collections.OPT_SensAnalysisCollection(idx)

        SaveForm(sacase)

        With sacase
            iv1ll = .iv1.lowerlimit
            iv1ul = .iv1.upperlimit
            iv1np = .iv1.points - 1
            iv1id = .iv1.objectID
            iv1prop = .iv1.propID
            If chkIndVar2.Checked Then
                iv2ll = .iv2.lowerlimit
                iv2ul = .iv2.upperlimit
                iv2np = .iv2.points - 1
                iv2id = .iv2.objectID
                iv2prop = .iv2.propID
            Else
                iv2id = ""
                iv2prop = ""
            End If
            dvid = .dv.objectID
            dvprop = .dv.propID
        End With

        Try
            Me.btnRun.Enabled = False
            Me.btnAbort.Enabled = True
            btnExportToNewSheet.Enabled = False
            Me.abortCalc = False
            res.Clear()
            counter = 0
            Me.tbStats.Text = ""
            With Me.dgvResults
                .Columns.Clear()
                .Columns.Add("IV1", "")
                .Columns.Add("IV2", "")
                .Rows.Clear()
                If Me.tbUnitIndVar1.Text <> "" Then
                    .Columns(0).HeaderText = Me.cbObjIndVar1.SelectedItem.ToString & " - " & Me.cbPropIndVar1.SelectedItem.ToString & " (" & Me.tbUnitIndVar1.Text & ")"
                Else
                    .Columns(0).HeaderText = Me.cbObjIndVar1.SelectedItem.ToString & " - " & Me.cbPropIndVar1.SelectedItem.ToString
                End If
                If chkIndVar2.Checked Then
                    .Columns(1).Visible = True
                    If Me.tbUnitIndVar2.Text <> "" Then
                        .Columns(1).HeaderText = Me.cbObjIndVar2.SelectedItem.ToString & " - " & Me.cbPropIndVar2.SelectedItem.ToString & " (" & Me.tbUnitIndVar2.Text & ")"
                    Else
                        .Columns(1).HeaderText = Me.cbObjIndVar2.SelectedItem.ToString & " - " & Me.cbPropIndVar2.SelectedItem.ToString
                    End If
                Else
                    .Columns(1).Visible = False
                End If
                If Me.rbExp.Checked Then
                    .Columns.Add("DV", "EXP Val")
                Else
                    For Each var As SAVariable In selectedsacase.depvariables.Values
                        .Columns.Add(var.propID, var.objectTAG & " - " & form.GetTranslatedString1(var.propID) & " (" & var.unit & ")")
                    Next
                End If
            End With
            'store original values
            If iv1id <> "SpreadsheetCell" And iv1id <> "ReactionProperty" Then
                iv1val0 = form.Collections.FlowsheetObjectCollection(iv1id).GetPropertyValue(iv1prop)
            ElseIf iv1id = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv1prop.Split("|")(0)).FirstOrDefault
                iv1val0 = rx.GetPropertyValue(iv1prop.Split("|")(1))
            Else
                iv1val0 = form.FormSpreadsheet.GetCellValue(iv1prop).Data
            End If
            If Me.chkIndVar2.Checked Then
                If iv2id <> "SpreadsheetCell" And iv2id <> "ReactionProperty" Then
                    iv2val0 = form.Collections.FlowsheetObjectCollection(iv2id).GetPropertyValue(iv2prop)
                ElseIf iv2id = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv2prop.Split("|")(0)).FirstOrDefault
                    iv2val0 = rx.GetPropertyValue(iv2prop.Split("|")(1))
                Else
                    iv2val0 = form.FormSpreadsheet.GetCellValue(iv2prop).Data
                End If
            Else
                iv2val0 = 0.0#
            End If
            For i = 0 To iv1np
                For j = 0 To iv2np
                    iv1val = iv1ll + i * (iv1ul - iv1ll) / iv1np
                    If Me.chkIndVar2.Checked Then iv2val = iv2ll + j * (iv2ul - iv2ll) / iv2np Else iv2val = 0
                    'set object properties
                    If iv1id <> "SpreadsheetCell" And iv1id <> "ReactionProperty" Then
                        form.Collections.FlowsheetObjectCollection(iv1id).SetPropertyValue(iv1prop, iv1val)
                    ElseIf iv1id = "ReactionProperty" Then
                        Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv1prop.Split("|")(0)).FirstOrDefault
                        rx.SetPropertyValue(iv1prop.Split("|")(1), iv1val)
                    Else
                        form.FormSpreadsheet.SetCellValue(iv1prop, iv1val)
                    End If
                    If Me.chkIndVar2.Checked Then
                        If iv2id <> "SpreadsheetCell" And iv2id <> "ReactionProperty" Then
                            form.Collections.FlowsheetObjectCollection(iv2id).SetPropertyValue(iv2prop, iv2val)
                        ElseIf iv2id = "ReactionProperty" Then
                            Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv2prop.Split("|")(0)).FirstOrDefault
                            rx.SetPropertyValue(iv2prop.Split("|")(1), iv2val)
                        Else
                            form.FormSpreadsheet.SetCellValue(iv2prop, iv2val)
                        End If
                    End If
                    'run simulation
                    Dim exceptions = FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, Settings.SolverMode)
                    If exceptions.Count > 0 Then Throw New AggregateException(exceptions)
                    'get the value of the dependent variable
                    If rbExp.Checked Then
                        Me.selectedsacase.econtext = New ExpressionContext
                        Me.selectedsacase.expression = Me.tbExpression.Text
                        With Me.selectedsacase.econtext
                            .Imports.AddType(GetType(System.Math))
                            For Each var As SAVariable In selectedsacase.variables.Values
                                If var.objectID <> "SpreadsheetCell" Then
                                    .Variables.Add(var.name, SystemsOfUnits.Converter.ConvertFromSI(var.unit, form.Collections.FlowsheetObjectCollection(var.objectID).GetPropertyValue(var.propID)))
                                Else
                                    .Variables.Add(var.name, form.FormSpreadsheet.GetCellValue(var.propID).Data)
                                End If
                            Next
                            Me.selectedsacase.exbase = Me.selectedsacase.econtext.CompileGeneric(Of Double)(Me.selectedsacase.expression)
                        End With
                        dvval = Me.selectedsacase.exbase.Evaluate
                        'store results
                        res.Add(New Double() {iv1val, iv2val, dvval})
                    Else
                        'store results
                        Dim currresults As New ArrayList
                        currresults.Add(iv1val)
                        currresults.Add(iv2val)
                        For Each var As SAVariable In selectedsacase.depvariables.Values
                            If var.objectID <> "SpreadsheetCell" Then
                                var.currentvalue = form.Collections.FlowsheetObjectCollection(var.objectID).GetPropertyValue(var.propID)
                            Else
                                var.currentvalue = form.FormSpreadsheet.GetCellValue(var.propID).Data
                            End If
                            currresults.Add(var.currentvalue)
                        Next
                        res.Add(currresults.ToArray(Type.GetType("System.Double")))
                    End If
                    If rbExp.Checked Then
                        Me.dgvResults.Rows.Add(New Object() {Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv1.unit, iv1val), nf), Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv2.unit, iv2val), nf), Format(dvval, nf)})
                    Else
                        Dim formattedvalues As New ArrayList
                        formattedvalues.Add(Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv1.unit, iv1val), nf))
                        formattedvalues.Add(Format(SystemsOfUnits.Converter.ConvertFromSI(sacase.iv2.unit, iv2val), nf))
                        For Each var As SAVariable In selectedsacase.depvariables.Values
                            formattedvalues.Add(Format(SystemsOfUnits.Converter.ConvertFromSI(var.unit, var.currentvalue), nf))
                        Next
                        Me.dgvResults.Rows.Add(formattedvalues.ToArray())
                    End If
                    Me.dgvResults.FirstDisplayedScrollingRowIndex = Me.dgvResults.Rows.Count - 1
                    counter += 1
                    Me.tbStats.Text += "Run #" & counter & " completed..." & vbCrLf
                    Me.tbStats.SelectionStart = Me.tbStats.Text.Length - 1
                    Me.tbStats.SelectionLength = 1
                    Me.tbStats.ScrollToCaret()
                    If Me.abortCalc Then Exit Sub
                Next
            Next
        Catch ex As Exception
            Me.tbStats.Text += "Error: " & ex.Message.ToString & vbCrLf
        Finally
            Me.btnRun.Enabled = True
            Me.btnAbort.Enabled = False
            're-run simulation to restore original state
            Me.tbStats.Text += "Restoring simulation to its original state..." & vbCrLf
            Me.tbStats.SelectionStart = Me.tbStats.Text.Length - 1
            Me.tbStats.SelectionLength = 1
            Me.tbStats.ScrollToCaret()
            If iv1id <> "SpreadsheetCell" And iv1id <> "ReactionProperty" Then
                form.Collections.FlowsheetObjectCollection(iv1id).SetPropertyValue(iv1prop, iv1val0)
            ElseIf iv1id = "ReactionProperty" Then
                Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv1prop.Split("|")(0)).FirstOrDefault
                rx.SetPropertyValue(iv1prop.Split("|")(1), iv1val)
            Else
                form.FormSpreadsheet.SetCellValue(iv1prop, iv1val0)
            End If
            If Me.chkIndVar2.Checked Then
                If iv2id <> "SpreadsheetCell" And iv2id <> "ReactionProperty" Then
                    form.Collections.FlowsheetObjectCollection(iv2id).SetPropertyValue(iv2prop, iv2val0)
                ElseIf iv2id = "ReactionProperty" Then
                    Dim rx = form.Reactions.Values.Where(Function(x) x.Name = iv2prop.Split("|")(0)).FirstOrDefault
                    rx.SetPropertyValue(iv2prop.Split("|")(1), iv2val)
                Else
                    form.FormSpreadsheet.SetCellValue(iv2prop, iv2val0)
                End If
            End If
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(form, Settings.SolverMode)
            Me.tbStats.Text += "Done!" & vbCrLf
            Me.tbStats.SelectionStart = Me.tbStats.Text.Length - 1
            Me.tbStats.SelectionLength = 1
            Me.tbStats.ScrollToCaret()
            Me.BtnDrawChart.Enabled = True

            If My.Application.UtilityPlugins.ContainsKey("DF7368D6-5A06-4856-9B7A-D7F09D81F71F") Then

                btnRegressData.Enabled = True

            End If

            btnExportToNewSheet.Enabled = True

            FillChartData()
        End Try

    End Sub
    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAbort.Click
        abortCalc = True
    End Sub

    Private Sub LinkLabel1_LinkClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        Process.Start("http://www.zunzun.com")
    End Sub

    Private Sub chkIndVar2_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles chkIndVar2.CheckedChanged
        If Me.chkIndVar2.Checked Then
            Me.gbIndVar2.Enabled = True
        Else
            Me.gbIndVar2.Enabled = False
        End If
    End Sub

    Private Sub RadioButton1_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles rbVar.CheckedChanged
        If Me.rbVar.Checked Then
            GroupBox8.Enabled = False
            GroupBox9.Enabled = False
            GroupBox10.Enabled = True
        Else
            GroupBox8.Enabled = True
            GroupBox9.Enabled = True
            GroupBox10.Enabled = False
        End If
    End Sub

    Private Sub btnVerify_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnVerify.Click
        Try
            Dim econtext As New ExpressionContext
            econtext.Imports.AddType(GetType(System.Math))
            For Each row As DataGridViewRow In Me.dgVariables.Rows
                With econtext
                    .Variables.Add(row.Cells(1).Value, Convert.ToDouble(row.Cells(4).Value))
                End With
            Next
            Dim exbase As IGenericExpression(Of Double) = econtext.CompileGeneric(Of Double)(Me.tbExpression.Text)
            Me.tbCurrentValue.Text = exbase.Evaluate
            If EnableAutoSave Then SaveForm(selectedsacase)
        Catch ex As Exception
            Me.tbCurrentValue.Text = ex.Message
        End Try
    End Sub

    Private Sub btnClear_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnClear.Click
        Me.tbExpression.Text = ""
    End Sub

    Private Sub tsbAddVar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbAddVar.Click
        Me.dgVariables.Rows.Add()
        Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1).HeaderCell.Value = Me.dgVariables.Rows.Count.ToString
        Me.dgVariables.Rows(Me.dgVariables.Rows.Count - 1).Cells(0).Value = Guid.NewGuid.ToString
    End Sub

    Private Sub tsbDelVar_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbDelVar.Click
        If Me.dgVariables.SelectedRows.Count > 0 Then
            For i As Integer = 0 To Me.dgVariables.SelectedRows.Count - 1
                Me.dgVariables.Rows.Remove(Me.dgVariables.SelectedRows(0))
            Next
        End If
    End Sub

    Private Sub dgVariables_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgVariables.CellValueChanged
        If e.RowIndex >= 0 Then
            Select Case e.ColumnIndex
                Case 2
                    Dim cbc As DataGridViewComboBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex + 1)
                    cbc.Items.Clear()
                    With cbc.Items
                        If Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString <> "" Then
                            Dim props As String()
                            props = Me.ReturnProperties(Me.dgVariables.Rows(e.RowIndex).Cells(2).Value, True)
                            For Each prop As String In props
                                .Add(form.GetTranslatedString1(prop))
                            Next
                        End If
                    End With
                Case 3
                    If Not Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value Is Nothing Then
                        If Me.dgVariables.Rows(e.RowIndex).Cells(2).Value <> DWSIM.App.GetLocalString("SpreadsheetCell") Then
                            Dim tbc0 As DataGridViewTextBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(4)
                            Dim tbc1 As DataGridViewTextBoxCell = Me.dgVariables.Rows(e.RowIndex).Cells(5)
                            Dim props As String() = Me.ReturnProperties(Me.dgVariables.Rows(e.RowIndex).Cells(2).Value, True)
                            For Each prop As String In props
                                If form.GetTranslatedString1(prop) = Me.dgVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString Then
                                    Dim obj As SharedClasses.UnitOperations.BaseClass = ReturnObject(Me.dgVariables.Rows(e.RowIndex).Cells(2).Value)
                                    tbc0.Value = Format(Val(obj.GetPropertyValue(prop, su)), nf)
                                    tbc1.Value = obj.GetPropertyUnit(prop, su)
                                    Exit For
                                End If
                            Next
                        End If
                    End If
                    If EnableAutoSave Then SaveForm(selectedsacase)
            End Select
        End If
    End Sub

    Private Sub dgDepVariables_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgDepVariables.CellValueChanged
        If e.RowIndex >= 0 Then
            Select Case e.ColumnIndex
                Case 1
                    Dim cbc As DataGridViewComboBoxCell = Me.dgDepVariables.Rows(e.RowIndex).Cells(e.ColumnIndex + 1)
                    cbc.Items.Clear()
                    With cbc.Items
                        If Me.dgDepVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString <> "" Then
                            Dim props As String()
                            props = Me.ReturnProperties(Me.dgDepVariables.Rows(e.RowIndex).Cells(1).Value, True)
                            For Each prop As String In props
                                .Add(form.GetTranslatedString1(prop))
                            Next
                        End If
                    End With
                    cbc.Sorted = True
                Case 2
                    If Not Me.dgDepVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value Is Nothing Then
                        If Me.dgDepVariables.Rows(e.RowIndex).Cells(1).Value <> DWSIM.App.GetLocalString("SpreadsheetCell") Then
                            Dim tbc0 As DataGridViewTextBoxCell = Me.dgDepVariables.Rows(e.RowIndex).Cells(3)
                            Dim props As String() = Me.ReturnProperties(Me.dgDepVariables.Rows(e.RowIndex).Cells(1).Value, True)
                            For Each prop As String In props
                                If form.GetTranslatedString1(prop) = Me.dgDepVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString Then
                                    Dim obj As SharedClasses.UnitOperations.BaseClass = ReturnObject(Me.dgDepVariables.Rows(e.RowIndex).Cells(1).Value)
                                    tbc0.Value = obj.GetPropertyUnit(prop, su)
                                    Exit For
                                End If
                            Next
                        End If
                    End If
                    If EnableAutoSave Then SaveForm(selectedsacase)
            End Select
        End If
    End Sub

    Private Function ReturnObject(ByVal objectTAG As String) As SharedClasses.UnitOperations.BaseClass

        For Each obj As SharedClasses.UnitOperations.BaseClass In form.Collections.FlowsheetObjectCollection.Values
            If objectTAG = obj.GraphicObject.Tag Then
                Return obj
                Exit Function
            End If
        Next

        Return Nothing

    End Function

    Private Function ReturnPropertyID(ByVal objectID As String, ByVal propTAG As String) As String

        If objectID = "SpreadsheetCell" Then
            Return propTAG
        ElseIf objectID = "ReactionProperty" Then
            Return propTAG
        ElseIf objectID <> "" Then
            Dim props As String() = form.Collections.FlowsheetObjectCollection(objectID).GetProperties(Interfaces.Enums.PropertyType.ALL)
            For Each prop As String In props
                If form.GetTranslatedString1(prop) = propTAG Then
                    Return prop
                End If
            Next
        End If

        Return Nothing

    End Function

    Private Sub dgVariables_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgVariables.DataError

    End Sub

    Private Sub ToolStripButton1_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripButton1.Click
        Me.dgDepVariables.Rows.Add()
        Me.dgDepVariables.Rows(Me.dgDepVariables.Rows.Count - 1).HeaderCell.Value = Me.dgDepVariables.Rows.Count.ToString
        Me.dgDepVariables.Rows(Me.dgDepVariables.Rows.Count - 1).Cells(0).Value = Guid.NewGuid.ToString
    End Sub

    Private Sub ToolStripButton2_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripButton2.Click
        If Me.dgDepVariables.SelectedRows.Count > 0 Then
            For i As Integer = 0 To Me.dgDepVariables.SelectedRows.Count - 1
                Me.dgDepVariables.Rows.Remove(Me.dgDepVariables.SelectedRows(0))
            Next
        End If
    End Sub

    Private Sub tbCaseName_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbCaseName.TextChanged
        If selected Then
            Me.lbCases.Items(Me.lbCases.SelectedIndex) = Me.tbCaseName.Text
            form.Collections.OPT_SensAnalysisCollection(Me.lbCases.SelectedIndex).name = Me.tbCaseName.Text
        End If
    End Sub

    Private Sub dgDepVariables_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgDepVariables.DataError

    End Sub

    Private Sub dgvResults_DataError(sender As Object, e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgvResults.DataError

    End Sub

    Private Sub btnRegressData_Click(sender As System.Object, e As System.EventArgs) Handles btnRegressData.Click

        'data fitting plugin is available
        Dim myUPlugin As Interfaces.IUtilityPlugin = My.Application.UtilityPlugins.Item("DF7368D6-5A06-4856-9B7A-D7F09D81F71F")

        myUPlugin.SetFlowsheet(form)

        Dim pform As Form = myUPlugin.UtilityForm

        dgvResults.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithoutHeaderText
        dgvResults.SelectAll()
        pform.Tag = TryCast(dgvResults.GetClipboardContent(), DataObject).GetText()
        dgvResults.ClipboardCopyMode = DataGridViewClipboardCopyMode.EnableWithAutoHeaderText
        pform.ShowDialog(Me)

    End Sub
    Private Sub FillChartData()
        Dim i As Integer
        Dim s As String

        CbCrtX.Items.Clear()
        CbCrtY.Items.Clear()
        For i = 0 To dgvResults.ColumnCount - 1
            If dgvResults.Columns(i).Visible Then
                s = dgvResults.Columns(i).HeaderText
                CbCrtX.Items.Add(s)
                CbCrtY.Items.Add(s)
            End If
        Next
        CbCrtX.SelectedIndex = 0
        If dgvResults.Columns(1).Visible Then
            CbCrtY.SelectedIndex = 2
        Else
            CbCrtY.SelectedIndex = 1
        End If

        CbCrtPar.Visible = chkIndVar2.Checked
        LblParam.Visible = chkIndVar2.Checked
        CbCrtPar.Items.Clear()
        CbCrtPar.Items.Add(dgvResults.Columns(0).HeaderText)
        CbCrtPar.Items.Add(dgvResults.Columns(1).HeaderText)
        CbCrtPar.SelectedIndex = 1

        graph.GraphPane.CurveList.Clear()
    End Sub

    Private Sub BtnExportToNewSheet_Click(sender As Object, e As EventArgs) Handles btnExportToNewSheet.Click

        dgvResults.SelectAll()

        Clipboard.SetDataObject(dgvResults.GetClipboardContent())

        Dim sheet = form.FormSpreadsheet.Spreadsheet.NewWorksheet()

        sheet.Paste()

        form.FormSpreadsheet.Activate()

        form.FormSpreadsheet.Spreadsheet.CurrentWorksheet = sheet

    End Sub

    Private Sub BtnDrawChart_Click(sender As System.Object, e As System.EventArgs) Handles BtnDrawChart.Click
        Dim px, py, IV2 As New ArrayList
        Dim k, j As Integer
        Dim x, y As Integer
        Dim v, vl As String
        Dim rnd As New System.Random


        'find selected columns
        For k = 0 To dgvResults.ColumnCount - 1
            If CbCrtX.SelectedItem = dgvResults.Columns(k).HeaderText Then x = k
            If CbCrtY.SelectedItem = dgvResults.Columns(k).HeaderText Then y = k
        Next

        If chkIndVar2.Checked Then
            v = ""
            vl = ""
            For k = 0 To dgvResults.Rows.Count - 1
                v = dgvResults.Rows(k).Cells(CbCrtPar.SelectedIndex).Value
                If k = 0 Then
                    IV2.Add(v)
                    vl = v
                Else
                    If IV2.IndexOf(v) = -1 Then
                        IV2.Add(v)
                        vl = v
                    End If
                End If
            Next
        Else
            IV2.Add("")
        End If

        With graph.GraphPane
            .CurveList.Clear()
            .Title.Text = selectedsacase.name
            .XAxis.Title.Text = CbCrtX.SelectedItem
            .YAxis.Title.Text = CbCrtY.SelectedItem

            For j = 0 To IV2.Count - 1 'run through all curve parameter values
                'collect data to display
                px.Clear()
                py.Clear()
                For k = 0 To dgvResults.Rows.Count - 1 'run through all result points to find values
                    If Not chkIndVar2.Checked Or dgvResults.Rows(k).Cells(CbCrtPar.SelectedIndex).Value = IV2(j) Then
                        px.Add(CType(dgvResults.Rows(k).Cells(x).Value, Double))
                        py.Add(CType(dgvResults.Rows(k).Cells(y).Value, Double))
                    End If
                Next

                If chkIndVar2.Checked Then
                    .Legend.IsVisible = True
                Else
                    .Legend.IsVisible = False
                End If

                With .AddCurve(IV2(j), px.ToArray(GetType(Double)), py.ToArray(GetType(Double)), Color.Black)
                    .Symbol.Fill.Color = Color.FromArgb(rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                    .Symbol.Fill.Type = ZedGraph.FillType.Solid
                    .Symbol.Size = 5

                End With
            Next

            .AxisChange()
        End With


        graph.Refresh()
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

    Private Sub tbCaseDesc_TextChanged(sender As Object, e As EventArgs) Handles tbCaseDesc.TextChanged
        If selected Then
            form.Collections.OPT_SensAnalysisCollection(Me.lbCases.SelectedIndex).description = Me.tbCaseDesc.Text
        End If
    End Sub

End Class