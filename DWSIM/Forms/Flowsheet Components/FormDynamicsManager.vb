Imports DWSIM.DynamicsManager
Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces
Imports System.Linq

Public Class FormDynamicsManager

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Manager As DynamicsManager.Manager

    Private Adding As Boolean = False

    Private Sub FormDynamicsManager_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Manager = Flowsheet.DynamicsManager

        UpdateSelectables()

        UpdateAllPanels()

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkDynamics.CheckedChanged
        If chkDynamics.Checked Then
            chkDynamics.Text = DWSIM.App.GetLocalString("Deactivate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynEnabled")
            Flowsheet.DynamicMode = True
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkGreen
            Flowsheet.ModoDinamicoAtivoToolStripMenuItem.Checked = True
        Else
            chkDynamics.Text = DWSIM.App.GetLocalString("Activate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynDisabled")
            Flowsheet.DynamicMode = False
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkRed
            Flowsheet.ModoDinamicoAtivoToolStripMenuItem.Checked = False
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Flowsheet.FormIntegratorControls.Show(Flowsheet.GetDockPanel)
        'Flowsheet.dckPanel.SaveAsXml("C:\Users\Daniel\Desktop\layout.xml")
    End Sub

    Sub UpdateSelectables()

        Dim cbobjects = New DataGridViewComboBoxCell
        cbobjects.Items.Add("")
        cbobjects.Items.AddRange(Flowsheet.SimulationObjects.Values.Select(Function(x) x.GraphicObject.Tag).ToArray)

        Dim cbindicators = New DataGridViewComboBoxCell
        cbindicators.Items.Add("")
        cbindicators.Items.AddRange(Flowsheet.SimulationObjects.Values.Where(Function(x0) TypeOf x0 Is IIndicator).Select(Function(x) x.GraphicObject.Tag).ToArray)

        Dim cbeventtype = New DataGridViewComboBoxCell
        cbeventtype.Items.Add(Flowsheet.GetTranslatedString1("ChangeProperty"))
        cbeventtype.Items.Add(Flowsheet.GetTranslatedString1("RunScript"))

        Dim cbalarmtypes = New DataGridViewComboBoxCell
        cbalarmtypes.Items.AddRange(New Object() {"LL", "L", "H", "HH"})

        gridselectedset.Columns(4).CellTemplate = cbeventtype

        gridselectedset.Columns(5).CellTemplate = cbobjects

        grdiselmatrix.Columns(3).CellTemplate = cbindicators

        grdiselmatrix.Columns(4).CellTemplate = cbalarmtypes

        grdiselmatrix.Columns(5).CellTemplate = cbobjects

        cbAssociatedIntegrator.Items.Clear()
        cbAssociatedIntegrator.Items.AddRange(Manager.IntegratorList.Values.Select(Function(x) x.Description).ToArray)

        cbSelectedCauseAndEffectMatrix.Items.Clear()
        cbSelectedCauseAndEffectMatrix.Items.AddRange(Manager.CauseAndEffectMatrixList.Values.Select(Function(x) x.Description).ToArray)

        cbSelectedEventSet.Items.Clear()
        cbSelectedEventSet.Items.AddRange(Manager.EventSetList.Values.Select(Function(x) x.Description).ToArray)

        cbScheduleInitialState.Items.Clear()
        cbScheduleInitialState.Items.AddRange(Flowsheet.StoredSolutions.Keys.ToArray)

        gridintegrators_SelectionChanged(gridintegrators, New EventArgs)

        gridschedules_SelectionChanged(gridschedules, New EventArgs)

    End Sub

    Sub UpdateAllPanels()

        For Each ev In Manager.EventSetList.Values
            gridsets.Rows.Add(New Object() {ev.ID, ev.Description})
        Next

        For Each ev In Manager.CauseAndEffectMatrixList.Values
            gridmatrices.Rows.Add(New Object() {ev.ID, ev.Description})
        Next

        For Each ev In Manager.IntegratorList.Values
            gridintegrators.Rows.Add(New Object() {ev.ID, ev.Description})
        Next

        For Each ev In Manager.ScheduleList.Values
            gridschedules.Rows.Add(New Object() {ev.ID, ev.Description})
        Next

    End Sub

    Private Sub btnAddEventSet_Click(sender As Object, e As EventArgs) Handles btnAddEventSet.Click

        Dim f As New FormEnterName

        If f.ShowDialog(Me) = DialogResult.OK Then
            Dim name = f.tbName.Text
            If name <> "" Then
                If Not Manager.EventSetList.ContainsKey(name) Then
                    Dim es = New EventSet With {.ID = Guid.NewGuid.ToString, .Description = name}
                    Manager.EventSetList.Add(es.ID, es)
                    gridsets.Rows.Add(New Object() {es.ID, es.Description})
                    UpdateSelectables()
                Else
                    MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Else
                MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub gridsets_SelectionChanged(sender As Object, e As EventArgs) Handles gridsets.SelectionChanged

        PopulateEventsFromSelectedSet()

    End Sub

    Sub PopulateEventsFromSelectedSet()

        Adding = True

        Dim es = Manager.EventSetList(gridsets.Rows(gridsets.SelectedCells(0).RowIndex).Cells(0).Value)

        gridselectedset.Rows.Clear()

        For Each ev In es.Events.Values
            With ev
                Dim etype As String
                If ev.EventType = Dynamics.DynamicsEventType.ChangeProperty Then
                    etype = Flowsheet.GetTranslatedString1("ChangeProperty")
                Else
                    etype = Flowsheet.GetTranslatedString1("RunScript")
                End If
                Dim obj, prop As String
                gridselectedset.Rows.Add(New Object() { .ID, .Enabled, .Description, .TimeStamp, etype, "", "", "", ""})
                Dim addedrow = gridselectedset.Rows(gridselectedset.Rows.Count - 1)
                If Flowsheet.SimulationObjects.ContainsKey(ev.SimulationObjectID) Then
                    obj = Flowsheet.SimulationObjects(ev.SimulationObjectID).GraphicObject.Tag
                    addedrow.Cells(5).Value = obj
                    Dim props = Flowsheet.SimulationObjects(ev.SimulationObjectID).GetProperties(PropertyType.WR)
                    Dim cbcell = DirectCast(addedrow.Cells(6), DataGridViewComboBoxCell)
                    cbcell.Items.Clear()
                    cbcell.Items.AddRange("")
                    cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    If props.Contains(ev.SimulationObjectProperty) Then
                        prop = Flowsheet.GetTranslatedString1(ev.SimulationObjectProperty)
                        addedrow.Cells(6).Value = prop
                    End If
                End If
                addedrow.Cells(7).Value = .SimulationObjectPropertyValue
                addedrow.Cells(8).Value = .SimulationObjectPropertyUnits
            End With
        Next

        Adding = False

    End Sub

    Private Sub btnAddEvent_Click(sender As Object, e As EventArgs) Handles btnAddEvent.Click

        Dim es = Manager.EventSetList(gridsets.Rows(gridsets.SelectedCells(0).RowIndex).Cells(0).Value)

        Dim ev As New DynamicEvent With {.ID = Guid.NewGuid.ToString}

        es.Events.Add(ev.ID, ev)

        With ev
            Dim etype As String
            If .EventType = Dynamics.DynamicsEventType.ChangeProperty Then
                etype = Flowsheet.GetTranslatedString1("ChangeProperty")
            Else
                etype = Flowsheet.GetTranslatedString1("RunScript")
            End If
            gridselectedset.Rows.Add(New Object() { .ID, .Enabled, .Description, .TimeStamp, etype, "", "", "", ""})
        End With

    End Sub

    Private Sub gridselectedset_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridselectedset.CellValueChanged

        If e.RowIndex < 0 Or Adding Then Exit Sub

        Dim es = Manager.EventSetList(gridsets.Rows(gridsets.SelectedCells(0).RowIndex).Cells(0).Value)

        Dim ev = es.Events(gridselectedset.Rows(e.RowIndex).Cells(0).Value)

        Dim value = gridselectedset.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
        Try
            Select Case e.ColumnIndex
                Case 1
                    ev.Enabled = value
                Case 2
                    ev.Description = value
                Case 3
                    ev.TimeStamp = value
                Case 4
                    If value = Flowsheet.GetTranslatedString1("ChangeProperty") Then
                        ev.EventType = Dynamics.DynamicsEventType.ChangeProperty
                    Else
                        ev.EventType = Dynamics.DynamicsEventType.RunScript
                    End If
                Case 5
                    If value <> "" Then
                        ev.SimulationObjectID = Flowsheet.GetFlowsheetGraphicObject(value).Name
                        Dim props = Flowsheet.SimulationObjects(ev.SimulationObjectID).GetProperties(PropertyType.WR)
                        Dim cbcell = DirectCast(gridselectedset.Rows(e.RowIndex).Cells(6), DataGridViewComboBoxCell)
                        cbcell.Items.Clear()
                        cbcell.Items.AddRange("")
                        cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    End If
                Case 6
                    If value <> "" Then
                        Dim props = Flowsheet.SimulationObjects(ev.SimulationObjectID).GetProperties(PropertyType.WR)
                        Dim cbcell = DirectCast(gridselectedset.Rows(e.RowIndex).Cells(6), DataGridViewComboBoxCell)
                        ev.SimulationObjectProperty = props(cbcell.Items.IndexOf(value))
                    End If
                Case 7
                    ev.SimulationObjectPropertyValue = value
                Case 8
                    ev.SimulationObjectPropertyUnits = value
            End Select
        Catch ex As Exception
            MessageBox.Show(ex.Message, Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub grdiselmatrix_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles grdiselmatrix.CellValueChanged

        If e.RowIndex < 0 Or Adding Then Exit Sub

        Dim cem = Manager.CauseAndEffectMatrixList(gridmatrices.Rows(gridmatrices.SelectedCells(0).RowIndex).Cells(0).Value)

        Dim cei = cem.Items(grdiselmatrix.Rows(e.RowIndex).Cells(0).Value)

        Dim value = grdiselmatrix.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
        Try
            Select Case e.ColumnIndex
                Case 1
                    cei.Enabled = value
                Case 2
                    cei.Description = value
                Case 3
                    If value <> "" Then
                        cei.AssociatedIndicator = Flowsheet.GetFlowsheetGraphicObject(value).Name
                    End If
                Case 4
                    Select Case value.ToString
                        Case "L"
                            cei.AssociatedIndicatorAlarm = Dynamics.DynamicsAlarmType.L
                        Case "LL"
                            cei.AssociatedIndicatorAlarm = Dynamics.DynamicsAlarmType.LL
                        Case "H"
                            cei.AssociatedIndicatorAlarm = Dynamics.DynamicsAlarmType.H
                        Case "HH"
                            cei.AssociatedIndicatorAlarm = Dynamics.DynamicsAlarmType.HH
                    End Select
                Case 5
                    If value <> "" Then
                        cei.SimulationObjectID = Flowsheet.GetFlowsheetGraphicObject(value).Name
                        Dim props = Flowsheet.SimulationObjects(cei.SimulationObjectID).GetProperties(PropertyType.WR)
                        Dim cbcell = DirectCast(gridselectedset.Rows(e.RowIndex).Cells(6), DataGridViewComboBoxCell)
                        cbcell.Items.Clear()
                        cbcell.Items.AddRange("")
                        cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    End If
                Case 6
                    If value <> "" Then
                        Dim props = Flowsheet.SimulationObjects(cei.SimulationObjectID).GetProperties(PropertyType.WR)
                        Dim cbcell = DirectCast(gridselectedset.Rows(e.RowIndex).Cells(6), DataGridViewComboBoxCell)
                        cei.SimulationObjectProperty = props(cbcell.Items.IndexOf(value))
                    End If
                Case 7
                    cei.SimulationObjectPropertyValue = value
                Case 8
                    cei.SimulationObjectPropertyUnits = value
            End Select
        Catch ex As Exception
            MessageBox.Show(ex.Message, Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub gridsets_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridsets.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim es = Manager.EventSetList(gridsets.Rows(gridsets.SelectedCells(0).RowIndex).Cells(0).Value)

        es.Description = gridsets.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

    End Sub

    Private Sub btnAddMatrix_Click(sender As Object, e As EventArgs) Handles btnAddMatrix.Click

        Dim f As New FormEnterName

        If f.ShowDialog(Me) = DialogResult.OK Then
            Dim name = f.tbName.Text
            If name <> "" Then
                If Not Manager.CauseAndEffectMatrixList.ContainsKey(name) Then
                    Dim cem = New CauseAndEffectMatrix With {.ID = Guid.NewGuid.ToString, .Description = name}
                    Manager.CauseAndEffectMatrixList.Add(cem.ID, cem)
                    gridmatrices.Rows.Add(New Object() {cem.ID, cem.Description})
                    UpdateSelectables()
                Else
                    MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Else
                MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub btnAddMatrixItem_Click(sender As Object, e As EventArgs) Handles btnAddMatrixItem.Click

        Dim cem = Manager.CauseAndEffectMatrixList(gridmatrices.Rows(gridmatrices.SelectedCells(0).RowIndex).Cells(0).Value)

        Dim cei As New CauseAndEffectItem With {.ID = Guid.NewGuid.ToString}

        cem.Items.Add(cei.ID, cei)

        With cei
            grdiselmatrix.Rows.Add(New Object() { .ID, .Enabled, .Description, "", "", "", "", "", ""})
        End With

    End Sub

    Private Sub gridmatrices_SelectionChanged(sender As Object, e As EventArgs) Handles gridmatrices.SelectionChanged

        PopulateCEMFromSelection()

    End Sub

    Sub PopulateCEMFromSelection()

        Adding = True

        Dim cem = Manager.CauseAndEffectMatrixList(gridmatrices.Rows(gridmatrices.SelectedCells(0).RowIndex).Cells(0).Value)

        grdiselmatrix.Rows.Clear()

        For Each cei In cem.Items.Values
            With cei
                Dim atype As String = ""
                Select Case .AssociatedIndicatorAlarm
                    Case Dynamics.DynamicsAlarmType.L
                        atype = "L"
                    Case Dynamics.DynamicsAlarmType.LL
                        atype = "LL"
                    Case Dynamics.DynamicsAlarmType.H
                        atype = "H"
                    Case Dynamics.DynamicsAlarmType.HH
                        atype = "HH"
                End Select
                Dim obj, prop, ind As String
                gridselectedset.Rows.Add(New Object() { .ID, .Enabled, .Description, "", atype, "", "", "", ""})
                Dim addedrow = gridselectedset.Rows(gridselectedset.Rows.Count - 1)
                If Flowsheet.SimulationObjects.ContainsKey(.AssociatedIndicator) Then
                    ind = Flowsheet.SimulationObjects(.AssociatedIndicator).GraphicObject.Tag
                    addedrow.Cells(3).Value = ind
                End If
                If Flowsheet.SimulationObjects.ContainsKey(.SimulationObjectID) Then
                    obj = Flowsheet.SimulationObjects(.SimulationObjectID).GraphicObject.Tag
                    addedrow.Cells(5).Value = obj
                    Dim props = Flowsheet.SimulationObjects(.SimulationObjectID).GetProperties(PropertyType.WR)
                    Dim cbcell = DirectCast(addedrow.Cells(6), DataGridViewComboBoxCell)
                    cbcell.Items.Clear()
                    cbcell.Items.AddRange("")
                    cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    If props.Contains(.SimulationObjectProperty) Then
                        prop = Flowsheet.GetTranslatedString1(.SimulationObjectProperty)
                        addedrow.Cells(6).Value = prop
                    End If
                End If
                addedrow.Cells(7).Value = .SimulationObjectPropertyValue
                addedrow.Cells(8).Value = .SimulationObjectPropertyUnits
            End With
        Next

        Adding = False

    End Sub

    Private Sub btnAddIntegrator_Click(sender As Object, e As EventArgs) Handles btnAddIntegrator.Click

        Dim f As New FormEnterName

        If f.ShowDialog(Me) = DialogResult.OK Then
            Dim name = f.tbName.Text
            If name <> "" Then
                If Not Manager.IntegratorList.ContainsKey(name) Then
                    Dim it1 = New Integrator With {.ID = Guid.NewGuid.ToString, .Description = name}
                    Manager.IntegratorList.Add(it1.ID, it1)
                    gridintegrators.Rows.Add(New Object() {it1.ID, it1.Description})
                    UpdateSelectables()
                Else
                    MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Else
                MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub btnAddSchedule_Click(sender As Object, e As EventArgs) Handles btnAddSchedule.Click

        Dim f As New FormEnterName

        If f.ShowDialog(Me) = DialogResult.OK Then
            Dim name = f.tbName.Text
            If name <> "" Then
                If Not Manager.ScheduleList.ContainsKey(name) Then
                    Dim it1 = New Schedule With {.ID = Guid.NewGuid.ToString, .Description = name}
                    Manager.ScheduleList.Add(it1.ID, it1)
                    gridschedules.Rows.Add(New Object() {it1.ID, it1.Description})
                    UpdateSelectables()
                Else
                    MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Else
                MessageBox.Show(Flowsheet.GetTranslatedString1("InvalidName"), Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub gridintegrators_SelectionChanged(sender As Object, e As EventArgs) Handles gridintegrators.SelectionChanged

        If gridintegrators.SelectedCells.Count < 1 Then Exit Sub

        Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)

        dtpIntegrationStep.Value = dtpIntegrationStep.MinDate.Add(i1.IntegrationStep)

        dtpIntegratorDuration.Value = dtpIntegratorDuration.MinDate.Add(i1.Duration)

        nupCalcBalFreq.Value = i1.CalculationRatePressureFlow

        nupCalcControlFreq.Value = i1.CalculationRateControl

        nupCalcEqFreq.Value = i1.CalculationRateEquilibrium

        panelSelIntegrator.Enabled = True

    End Sub

    Private Sub dtpIntegrationStep_ValueChanged(sender As Object, e As EventArgs) Handles dtpIntegrationStep.ValueChanged

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.IntegrationStep = dtpIntegrationStep.Value - dtpIntegrationStep.MinDate
        Catch ex As Exception
        End Try

    End Sub

    Private Sub dtpIntegratorDuration_ValueChanged(sender As Object, e As EventArgs) Handles dtpIntegratorDuration.ValueChanged

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.Duration = dtpIntegratorDuration.Value - dtpIntegratorDuration.MinDate
        Catch ex As Exception
        End Try

    End Sub

    Private Sub nupCalcEqFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcEqFreq.ValueChanged

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRateEquilibrium = nupCalcEqFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub nupCalcBalFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcBalFreq.ValueChanged

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRatePressureFlow = nupCalcBalFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub nupCalcControlFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcControlFreq.ValueChanged

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRateControl = nupCalcControlFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridmatrices_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridmatrices.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Dim item = Manager.CauseAndEffectMatrixList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)

        item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

    End Sub

    Private Sub gridintegrators_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridintegrators.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Dim item = Manager.IntegratorList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)

        item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

    End Sub

    Private Sub gridschedules_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridschedules.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Dim item = Manager.ScheduleList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)

        item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

    End Sub

    Private Sub gridschedules_SelectionChanged(sender As Object, e As EventArgs) Handles gridschedules.SelectionChanged

        If gridschedules.SelectedCells.Count < 1 Then Exit Sub

        Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)

        Try
            cbAssociatedIntegrator.SelectedItem = Manager.IntegratorList(s1.CurrentIntegrator).Description
        Catch ex As Exception
        End Try

        Try
            cbSelectedEventSet.SelectedItem = Manager.EventSetList(s1.CurrentEventList).Description
        Catch ex As Exception
        End Try

        Try
            cbSelectedCauseAndEffectMatrix.SelectedItem = Manager.CauseAndEffectMatrixList(s1.CurrentCauseAndEffectMatrix).Description
        Catch ex As Exception
        End Try

        Try
            cbScheduleInitialState.SelectedItem = s1.InitialFlowsheetStateID
        Catch ex As Exception
        End Try

        chkIntegratorUseEventSet.Checked = s1.UsesEventList

        chkIntegratorUseMatrix.Checked = s1.UsesCauseAndEffectMatrix

        chkSchUseCurrentState.Checked = s1.UseCurrentStateAsInitial

        panelSelSchedule.Enabled = True

    End Sub

    Private Sub cbAssociatedIntegrator_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbAssociatedIntegrator.SelectedIndexChanged

        Try
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            s1.CurrentIntegrator = Manager.IntegratorList.Values.Where(Function(x) x.Description = cbAssociatedIntegrator.SelectedItem).FirstOrDefault.ID
        Catch ex As Exception
        End Try

    End Sub

    Private Sub cbSelectedEventSet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSelectedEventSet.SelectedIndexChanged

        Try
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            s1.CurrentEventList = Manager.EventSetList.Values.Where(Function(x) x.Description = cbSelectedEventSet.SelectedItem).FirstOrDefault.ID
        Catch ex As Exception
        End Try

    End Sub

    Private Sub cbSelectedCauseAndEffectMatrix_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSelectedCauseAndEffectMatrix.SelectedIndexChanged

        Try
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            s1.CurrentCauseAndEffectMatrix = Manager.CauseAndEffectMatrixList.Values.Where(Function(x) x.Description = cbSelectedCauseAndEffectMatrix.SelectedItem).FirstOrDefault.ID
        Catch ex As Exception
        End Try

    End Sub

    Private Sub cbScheduleInitialState_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbScheduleInitialState.SelectedIndexChanged

        Try
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            s1.InitialFlowsheetStateID = cbScheduleInitialState.SelectedItem
        Catch ex As Exception
        End Try

    End Sub

    Private Sub chkSchUseCurrentState_CheckedChanged(sender As Object, e As EventArgs) Handles chkSchUseCurrentState.CheckedChanged

        cbScheduleInitialState.Enabled = Not chkSchUseCurrentState.Checked
        Try
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            s1.UseCurrentStateAsInitial = chkSchUseCurrentState.Checked
        Catch ex As Exception
        End Try

    End Sub

End Class