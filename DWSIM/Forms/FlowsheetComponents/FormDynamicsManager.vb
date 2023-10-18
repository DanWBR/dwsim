Imports DWSIM.DynamicsManager
Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces
Imports DWSIM.Thermodynamics.Streams
Imports System.Linq

Public Class FormDynamicsManager

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Manager As DynamicsManager.Manager

    Private Adding As Boolean = False

    Private Sub FormDynamicsManager_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Manager = Flowsheet.DynamicsManager

        Dim dpi = Settings.DpiScale

        gridselectedset.ColumnHeadersHeight *= dpi
        grdiselmatrix.ColumnHeadersHeight *= dpi
        gridMonitoredVariables.ColumnHeadersHeight *= dpi
        dgvControllers.ColumnHeadersHeight *= dpi
        dgvIndicators.ColumnHeadersHeight *= dpi

        UpdateSelectables()

        UpdateAllPanels()

        UpdateControllerList()

        UpdateIndicatorList()

        CheckModelStatus()

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkDynamics.CheckedChanged
        If chkDynamics.Checked Then
            chkDynamics.Text = DWSIM.App.GetLocalString("Deactivate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynEnabled")
            Flowsheet.DynamicMode = True
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkGreen
            Flowsheet.ModoDinamicoAtivoToolStripMenuItem.Checked = True
            Flowsheet.tsbDynamics.Checked = True
        Else
            chkDynamics.Text = DWSIM.App.GetLocalString("Activate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynDisabled")
            Flowsheet.DynamicMode = False
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkRed
            Flowsheet.ModoDinamicoAtivoToolStripMenuItem.Checked = False
            Flowsheet.tsbDynamics.Checked = False
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Flowsheet.FormIntegratorControls.Show(Flowsheet.GetDockPanel)
        'Flowsheet.dckPanel.SaveAsXml("C:\Users\Daniel\Desktop\layout.xml")
    End Sub

    Public Sub UpdateSelectables()

        Dim cbobjects = New DataGridViewComboBoxCell
        cbobjects.Items.Add("")
        cbobjects.Items.AddRange(Flowsheet.SimulationObjects.Values.Select(Function(x) x.GraphicObject.Tag).OrderBy(Function(o) o).ToArray)

        Dim cbindicators = New DataGridViewComboBoxCell
        cbindicators.Items.Add("")
        cbindicators.Items.AddRange(Flowsheet.SimulationObjects.Values.Where(Function(x0) TypeOf x0 Is IIndicator).Select(Function(x) x.GraphicObject.Tag).OrderBy(Function(o) o).ToArray)

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

        gridMonitoredVariables.Columns(2).CellTemplate = cbobjects

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

    Public Sub UpdateAllPanels()

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

    Public Sub UpdateControllerList()

        dgvControllers.Rows.Clear()

        Dim Controllers = Flowsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is PIDController).ToList

        For Each controller In Controllers
            With DirectCast(controller, PIDController)
                Try
                    Dim row = dgvControllers.Rows.Add(New Object() { .Name, .GraphicObject.Tag, .Active, .Kp, .Ki, .Kd, .AdjustValue, .Offset})
                Catch ex As Exception
                End Try
            End With
        Next

    End Sub

    Public Sub UpdateIndicatorList()

        dgvIndicators.Rows.Clear()

        Dim Indicators = Flowsheet.SimulationObjects.Values.Where(Function(x) x.ObjectClass = SimulationObjectClass.Indicators).ToList

        For Each item In Indicators
            With DirectCast(item, Interfaces.IIndicator)
                Dim SelectedObject = Flowsheet.SimulationObjects.Values.Where(Function(x) x.Name = .SelectedObjectID).FirstOrDefault
                If Not SelectedObject Is Nothing Then
                    .CurrentValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(.SelectedPropertyUnits, SelectedObject.GetPropertyValue(.SelectedProperty))
                End If
                Try
                    dgvIndicators.Rows.Add(New Object() {item.Name, item.GraphicObject.Tag, .CurrentValue, .VeryLowAlarmValue, .LowAlarmValue, .HighAlarmValue, .VeryHighAlarmValue})
                Catch ex As Exception
                End Try
            End With
        Next

        For Each row As DataGridViewRow In dgvIndicators.Rows
            Dim indicator = DirectCast(Flowsheet.SimulationObjects(row.Cells(0).Value), IIndicator)
            With indicator
                If .VeryLowAlarmEnabled Then
                    If .VeryLowAlarmActive Then
                        row.Cells(3).Style.BackColor = Color.Red
                        row.Cells(3).Style.ForeColor = Color.White
                    Else
                        row.Cells(3).Style.BackColor = Color.Black
                        row.Cells(3).Style.ForeColor = Color.White
                    End If
                Else
                    row.Cells(3).Style.BackColor = Color.LightGray
                    row.Cells(3).Style.ForeColor = Color.Black
                End If
                If .LowAlarmEnabled Then
                    If .LowAlarmActive Then
                        row.Cells(4).Style.BackColor = Color.Red
                        row.Cells(4).Style.ForeColor = Color.White
                    Else
                        row.Cells(4).Style.BackColor = Color.Black
                        row.Cells(4).Style.ForeColor = Color.White
                    End If
                Else
                    row.Cells(4).Style.BackColor = Color.LightGray
                    row.Cells(4).Style.ForeColor = Color.Black
                End If
                If .HighAlarmEnabled Then
                    If .HighAlarmActive Then
                        row.Cells(5).Style.BackColor = Color.Red
                        row.Cells(5).Style.ForeColor = Color.White
                    Else
                        row.Cells(5).Style.BackColor = Color.Black
                        row.Cells(5).Style.ForeColor = Color.White
                    End If
                Else
                    row.Cells(5).Style.BackColor = Color.LightGray
                    row.Cells(5).Style.ForeColor = Color.Black
                End If
                If .VeryHighAlarmEnabled Then
                    If .VeryHighAlarmActive Then
                        row.Cells(6).Style.BackColor = Color.Red
                        row.Cells(6).Style.ForeColor = Color.White
                    Else
                        row.Cells(6).Style.BackColor = Color.Black
                        row.Cells(6).Style.ForeColor = Color.White
                    End If
                Else
                    row.Cells(6).Style.BackColor = Color.LightGray
                    row.Cells(6).Style.ForeColor = Color.Black
                End If
            End With
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
                gridselectedset.Rows.Add(New Object() { .ID, .Enabled, .Description, .TimeStamp - Date.MinValue, etype, "", "", "", ""})
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
                Select Case ev.TransitionType
                    Case Dynamics.DynamicsEventTransitionType.StepChange
                        addedrow.Cells(9).Value = "Step"
                    Case Dynamics.DynamicsEventTransitionType.LinearChange
                        addedrow.Cells(9).Value = "Linear"
                    Case Dynamics.DynamicsEventTransitionType.LogChange
                        addedrow.Cells(9).Value = "Log"
                    Case Dynamics.DynamicsEventTransitionType.InverseLogChange
                        addedrow.Cells(9).Value = "Inverse Log"
                    Case Dynamics.DynamicsEventTransitionType.RandomChange
                        addedrow.Cells(9).Value = "Random"
                End Select
                Select Case ev.TransitionReference
                    Case Dynamics.DynamicsEventTransitionReferenceType.InitialState
                        addedrow.Cells(10).Value = "Initial State"
                    Case Dynamics.DynamicsEventTransitionReferenceType.PreviousEvent
                        addedrow.Cells(10).Value = "Previous Event"
                End Select
                addedrow.Cells(11).Value = .TransitionReferenceEventID
            End With
        Next

        Adding = False

    End Sub

    Private Sub btnAddEvent_Click(sender As Object, e As EventArgs) Handles btnAddEvent.Click

        Try

            UpdateSelectables()

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
                gridselectedset.Rows.Add(New Object() { .ID, .Enabled, .Description, .TimeStamp - Date.MinValue, etype, "", "", "", "", "Step", "Previous Event"})
            End With

        Catch ex As Exception

        End Try

    End Sub

    Private Sub gridselectedset_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridselectedset.CellValueChanged

        If Manager Is Nothing Then Exit Sub

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
                    ev.TimeStamp = Date.MinValue.Add(TimeSpan.Parse(value))
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
                        ev.SimulationObjectProperty = props(cbcell.Items.IndexOf(value) - 1)
                    End If
                Case 7
                    ev.SimulationObjectPropertyValue = value
                Case 8
                    ev.SimulationObjectPropertyUnits = value
                Case 9
                    If value IsNot Nothing Then
                        If value = "Step" Then
                            ev.TransitionType = Dynamics.DynamicsEventTransitionType.StepChange
                        ElseIf value = "Linear" Then
                            ev.TransitionType = Dynamics.DynamicsEventTransitionType.LinearChange
                        ElseIf value = "Log" Then
                            ev.TransitionType = Dynamics.DynamicsEventTransitionType.LogChange
                        ElseIf value = "Inverse Log" Then
                            ev.TransitionType = Dynamics.DynamicsEventTransitionType.InverseLogChange
                        ElseIf value = "Random" Then
                            ev.TransitionType = Dynamics.DynamicsEventTransitionType.RandomChange
                        End If
                    End If
                Case 10
                    If value IsNot Nothing Then
                        If value = "Initial State" Then
                            ev.TransitionReference = Dynamics.DynamicsEventTransitionReferenceType.InitialState
                        ElseIf value = "Previous Event" Then
                            ev.TransitionReference = Dynamics.DynamicsEventTransitionReferenceType.PreviousEvent
                        End If
                    End If
            End Select
        Catch ex As Exception
            MessageBox.Show(ex.Message, Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub grdiselmatrix_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles grdiselmatrix.CellValueChanged

        If Manager Is Nothing Then Exit Sub

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
                        cei.SimulationObjectProperty = props(cbcell.Items.IndexOf(value) - 1)
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

        If Manager Is Nothing Then Exit Sub

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

        Try

            UpdateSelectables()

            Dim cem = Manager.CauseAndEffectMatrixList(gridmatrices.Rows(gridmatrices.SelectedCells(0).RowIndex).Cells(0).Value)

            Dim cei As New CauseAndEffectItem With {.ID = Guid.NewGuid.ToString}

            cem.Items.Add(cei.ID, cei)

            With cei
                grdiselmatrix.Rows.Add(New Object() { .ID, .Enabled, .Description, "", "", "", "", "", ""})
            End With

        Catch ex As Exception

        End Try

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

        nupIntegrationStep.Value = i1.IntegrationStep.TotalMilliseconds

        nupDays.Value = i1.Duration.Days

        nupHours.Value = i1.Duration.Hours

        nupMinutes.Value = i1.Duration.Minutes

        nupSeconds.Value = i1.Duration.Seconds

        nupRTStep.Value = i1.RealTimeStepMs

        nupCalcBalFreq.Value = i1.CalculationRatePressureFlow

        nupCalcControlFreq.Value = i1.CalculationRateControl

        nupCalcEqFreq.Value = i1.CalculationRateEquilibrium

        panelSelIntegrator.Enabled = True

        Adding = True

        gridMonitoredVariables.Rows.Clear()

        For Each cei In i1.MonitoredVariables
            With cei
                Dim obj, prop As String
                gridMonitoredVariables.Rows.Add(New Object() { .ID, .Description, "", "", .PropertyUnits})
                Dim addedrow = gridMonitoredVariables.Rows(gridMonitoredVariables.Rows.Count - 1)
                If Flowsheet.SimulationObjects.ContainsKey(.ObjectID) Then
                    obj = Flowsheet.SimulationObjects(.ObjectID).GraphicObject.Tag
                    addedrow.Cells(2).Value = obj
                    Dim props = Flowsheet.SimulationObjects(.ObjectID).GetProperties(PropertyType.WR)
                    Dim cbcell = DirectCast(addedrow.Cells(3), DataGridViewComboBoxCell)
                    cbcell.Items.Clear()
                    cbcell.Items.AddRange("")
                    cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    If props.Contains(.PropertyID) Then
                        prop = Flowsheet.GetTranslatedString1(.PropertyID)
                        addedrow.Cells(3).Value = prop
                    End If
                End If
            End With
        Next

        Adding = False

    End Sub

    Private Sub dtpIntegratorDuration_ValueChanged(sender As Object, e As EventArgs)

    End Sub

    Private Sub nupCalcEqFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcEqFreq.ValueChanged
        If Manager Is Nothing Then Exit Sub

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRateEquilibrium = nupCalcEqFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub nupCalcBalFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcBalFreq.ValueChanged
        If Manager Is Nothing Then Exit Sub

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRatePressureFlow = nupCalcBalFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub nupCalcControlFreq_ValueChanged(sender As Object, e As EventArgs) Handles nupCalcControlFreq.ValueChanged
        If Manager Is Nothing Then Exit Sub

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.CalculationRateControl = nupCalcControlFreq.Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridmatrices_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridmatrices.CellValueChanged

        If Manager Is Nothing Then Exit Sub

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Try
            Dim item = Manager.CauseAndEffectMatrixList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)
            item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridintegrators_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridintegrators.CellValueChanged

        If Manager Is Nothing Then Exit Sub

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Try
            Dim item = Manager.IntegratorList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)
            item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridschedules_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridschedules.CellValueChanged

        If Manager Is Nothing Then Exit Sub

        If e.RowIndex < 0 Then Exit Sub

        Dim grid = DirectCast(sender, DataGridView)

        Try
            Dim item = Manager.ScheduleList(grid.Rows(grid.SelectedCells(0).RowIndex).Cells(0).Value)
            item.Description = grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridschedules_SelectionChanged(sender As Object, e As EventArgs) Handles gridschedules.SelectionChanged

        If Manager Is Nothing Then Exit Sub

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

        chkResetAll.Checked = s1.ResetContentsOfAllObjects

        panelSelSchedule.Enabled = True

    End Sub

    Private Sub cbAssociatedIntegrator_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbAssociatedIntegrator.SelectedIndexChanged
        If Manager IsNot Nothing Then
            Try
                Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
                s1.CurrentIntegrator = Manager.IntegratorList.Values.Where(Function(x) x.Description = cbAssociatedIntegrator.SelectedItem).FirstOrDefault.ID
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbSelectedEventSet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSelectedEventSet.SelectedIndexChanged
        If Manager IsNot Nothing Then
            Try
                Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
                s1.CurrentEventList = Manager.EventSetList.Values.Where(Function(x) x.Description = cbSelectedEventSet.SelectedItem).FirstOrDefault.ID
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbSelectedCauseAndEffectMatrix_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSelectedCauseAndEffectMatrix.SelectedIndexChanged
        If Manager IsNot Nothing Then
            Try
                Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
                s1.CurrentCauseAndEffectMatrix = Manager.CauseAndEffectMatrixList.Values.Where(Function(x) x.Description = cbSelectedCauseAndEffectMatrix.SelectedItem).FirstOrDefault.ID
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub cbScheduleInitialState_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbScheduleInitialState.SelectedIndexChanged
        If Manager IsNot Nothing Then
            Try
                Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
                s1.InitialFlowsheetStateID = cbScheduleInitialState.SelectedItem
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub chkSchUseCurrentState_CheckedChanged(sender As Object, e As EventArgs) Handles chkSchUseCurrentState.CheckedChanged
        If Manager IsNot Nothing Then
            cbScheduleInitialState.Enabled = Not chkSchUseCurrentState.Checked
            Try
                Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
                s1.UseCurrentStateAsInitial = chkSchUseCurrentState.Checked
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub chkIntegratorUseEventSet_CheckedChanged(sender As Object, e As EventArgs) Handles chkIntegratorUseEventSet.CheckedChanged
        If Manager IsNot Nothing Then
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            Try
                s1.UsesEventList = chkIntegratorUseEventSet.Checked
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub chkIntegratorUseMatrix_CheckedChanged(sender As Object, e As EventArgs) Handles chkIntegratorUseMatrix.CheckedChanged
        If Manager IsNot Nothing Then
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            Try
                s1.UsesCauseAndEffectMatrix = chkIntegratorUseMatrix.Checked
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub dgvControllers_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvControllers.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim controller = DirectCast(Flowsheet.SimulationObjects(dgvControllers.Rows(e.RowIndex).Cells(0).Value), PIDController)

        Dim value = dgvControllers.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

        Try
            Select Case e.ColumnIndex
                Case 1
                    controller.GraphicObject.Tag = value
                Case 2
                    controller.Active = value
                Case 3
                    controller.Kp = value
                Case 4
                    controller.Ki = value
                Case 5
                    controller.Kd = value
                Case 6
                    controller.AdjustValue = value
                Case 7
                    controller.Offset = value
            End Select
            Flowsheet.UpdateOpenEditForms()
        Catch ex As Exception
        End Try

    End Sub

    Private Sub dgvIndicators_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvIndicators.CellValueChanged

        If e.RowIndex < 0 Then Exit Sub

        Dim indicator = DirectCast(Flowsheet.SimulationObjects(dgvIndicators.Rows(e.RowIndex).Cells(0).Value), IIndicator)

        Dim value = dgvControllers.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

        Try
            Select Case e.ColumnIndex
                Case 1
                    DirectCast(indicator, ISimulationObject).GraphicObject.Tag = value
                Case 2
                    indicator.CurrentValue = value
                Case 3
                    indicator.VeryLowAlarmValue = value
                Case 4
                    indicator.LowAlarmValue = value
                Case 5
                    indicator.HighAlarmValue = value
                Case 6
                    indicator.VeryHighAlarmValue = value
            End Select
            Flowsheet.UpdateOpenEditForms()
        Catch ex As Exception
        End Try

    End Sub

    Private Sub btnRemoveMatrix_Click(sender As Object, e As EventArgs) Handles btnRemoveMatrix.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                           DWSIM.App.GetLocalString("Ateno2"),
                                           MessageBoxButtons.YesNo,
                                           MessageBoxIcon.Question) = DialogResult.Yes Then

            Manager.CauseAndEffectMatrixList.Remove(gridmatrices.SelectedCells(0).OwningRow.Cells(0).Value)

            gridmatrices.Rows.RemoveAt(gridmatrices.SelectedCells(0).RowIndex)

        End If

    End Sub

    Private Sub btnRemoveEventSet_Click(sender As Object, e As EventArgs) Handles btnRemoveEventSet.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                           DWSIM.App.GetLocalString("Ateno2"),
                                           MessageBoxButtons.YesNo,
                                           MessageBoxIcon.Question) = DialogResult.Yes Then

            Manager.EventSetList.Remove(gridsets.SelectedCells(0).OwningRow.Cells(0).Value)

            gridsets.Rows.RemoveAt(gridsets.SelectedCells(0).RowIndex)

        End If

    End Sub

    Private Sub btnRemoveEvent_Click(sender As Object, e As EventArgs) Handles btnRemoveEvent.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                          DWSIM.App.GetLocalString("Ateno2"),
                                          MessageBoxButtons.YesNo,
                                          MessageBoxIcon.Question) = DialogResult.Yes Then

            Dim es = Manager.EventSetList(gridsets.Rows(gridsets.SelectedCells(0).RowIndex).Cells(0).Value)

            Manager.EventSetList(es.ID).Events.Remove(gridselectedset.SelectedCells(0).OwningRow.Cells(0).Value)

            gridselectedset.Rows.RemoveAt(gridselectedset.SelectedCells(0).RowIndex)

        End If

    End Sub

    Private Sub btnRemoveMatrixItem_Click(sender As Object, e As EventArgs) Handles btnRemoveMatrixItem.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                          DWSIM.App.GetLocalString("Ateno2"),
                                          MessageBoxButtons.YesNo,
                                          MessageBoxIcon.Question) = DialogResult.Yes Then

            Dim ce = Manager.CauseAndEffectMatrixList(gridmatrices.Rows(gridmatrices.SelectedCells(0).RowIndex).Cells(0).Value)

            Manager.CauseAndEffectMatrixList(ce.ID).Items.Remove(grdiselmatrix.SelectedCells(0).OwningRow.Cells(0).Value)

            grdiselmatrix.Rows.RemoveAt(grdiselmatrix.SelectedCells(0).RowIndex)

        End If

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        If Manager IsNot Nothing Then

            Try

                UpdateSelectables()

                Dim cbobjects = New DataGridViewComboBoxCell
                cbobjects.Items.Add("")
                cbobjects.Items.AddRange(Flowsheet.SimulationObjects.Values.Select(Function(x) x.GraphicObject.Tag).OrderBy(Function(o) o).ToArray)

                gridMonitoredVariables.Columns(2).CellTemplate = cbobjects


                Dim int = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)

                Dim v1 As New MonitoredVariable With {.ID = Guid.NewGuid.ToString}

                int.MonitoredVariables.Add(v1)

                With v1
                    gridMonitoredVariables.Rows.Add(New Object() { .ID, .Description, "", "", ""})
                End With

            Catch ex As Exception

            End Try

        End If

    End Sub

    Private Sub gridMonitoredVariables_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridMonitoredVariables.CellValueChanged

        If e.RowIndex < 0 Or Adding Then Exit Sub

        Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)

        Dim v1 = i1.MonitoredVariables.Where(Function(x) x.ID = gridMonitoredVariables.Rows(e.RowIndex).Cells(0).Value).FirstOrDefault

        Dim value = gridMonitoredVariables.Rows(e.RowIndex).Cells(e.ColumnIndex).Value

        Try
            Select Case e.ColumnIndex
                Case 1
                    v1.Description = value
                Case 2
                    If value <> "" Then
                        v1.ObjectID = Flowsheet.GetFlowsheetGraphicObject(value).Name
                        Dim props = Flowsheet.SimulationObjects(v1.ObjectID).GetProperties(PropertyType.ALL)
                        Dim cbcell = DirectCast(gridMonitoredVariables.Rows(e.RowIndex).Cells(3), DataGridViewComboBoxCell)
                        cbcell.Items.Clear()
                        cbcell.Items.AddRange("")
                        cbcell.Items.AddRange(props.Select(Function(p) Flowsheet.GetTranslatedString1(p)).ToArray)
                    End If
                Case 3
                    If value <> "" Then
                        Dim props = Flowsheet.SimulationObjects(v1.ObjectID).GetProperties(PropertyType.ALL)
                        Dim cbcell = DirectCast(gridMonitoredVariables.Rows(e.RowIndex).Cells(3), DataGridViewComboBoxCell)
                        v1.PropertyID = props(cbcell.Items.IndexOf(value) - 1)
                    End If
                Case 4
                    v1.PropertyUnits = value
            End Select
        Catch ex As Exception
            MessageBox.Show(ex.Message, Flowsheet.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try


    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                         DWSIM.App.GetLocalString("Ateno2"),
                                         MessageBoxButtons.YesNo,
                                         MessageBoxIcon.Question) = DialogResult.Yes Then

            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)

            Dim v1 = i1.MonitoredVariables.Where(Function(x) x.ID = gridMonitoredVariables.SelectedCells(0).OwningRow.Cells(0).Value).FirstOrDefault

            i1.MonitoredVariables.Remove(v1)

            gridMonitoredVariables.Rows.RemoveAt(gridMonitoredVariables.SelectedCells(0).RowIndex)

        End If

    End Sub

    Private Sub TabControl1_Selected(sender As Object, e As TabControlEventArgs) Handles TabControl1.Selected

        'If e.TabPage Is TabPage1 Then
        '    CheckModelStatus()
        'End If

    End Sub

    Public Sub CheckModelStatus()

        Dim streams_ok, uos_ok, valves_ok As Boolean

        streams_ok = True
        uos_ok = True
        valves_ok = True

        'material streams

        For Each stream In Flowsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is MaterialStream)
            If stream.GraphicObject.InputConnectors(0).IsAttached And Not stream.GraphicObject.OutputConnectors(0).IsAttached Then
                If stream.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType <> ObjectType.Valve Then
                    streams_ok = False
                    Exit For
                End If
            End If
            If stream.GraphicObject.OutputConnectors(0).IsAttached And Not stream.GraphicObject.InputConnectors(0).IsAttached Then
                If stream.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.ObjectType <> ObjectType.Valve Then
                    streams_ok = False
                    Exit For
                End If
            End If
        Next

        'unit operations

        For Each obj In Flowsheet.SimulationObjects.Values
            If Not obj.SupportsDynamicMode Then
                uos_ok = False
                Exit For
            End If
        Next

        'valves

        For Each v In Flowsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is Valve)
            If DirectCast(v, Valve).CalcMode = Valve.CalculationMode.DeltaP Then
                valves_ok = False
                Exit For
            End If
            If DirectCast(v, Valve).CalcMode = Valve.CalculationMode.OutletPressure Then
                valves_ok = False
                Exit For
            End If
        Next

        'If streams_ok Then
        '    pbStreamValves.Image = My.Resources.icons8_ok
        'Else
        '    pbStreamValves.Image = My.Resources.icons8_cancel
        'End If

        'If uos_ok Then
        '    pbUnitOps.Image = My.Resources.icons8_ok
        'Else
        '    pbUnitOps.Image = My.Resources.icons8_cancel
        'End If

        'If valves_ok Then
        '    pbValves.Image = My.Resources.icons8_ok
        'Else
        '    pbValves.Image = My.Resources.icons8_cancel
        'End If

    End Sub

    Private Sub FormDynamicsManager_Activated(sender As Object, e As EventArgs) Handles Me.Activated

        CheckModelStatus()

    End Sub

    Private Sub TabPage1_MouseHover(sender As Object, e As EventArgs)

        CheckModelStatus()

    End Sub

    Private Sub nupRTStep_ValueChanged(sender As Object, e As EventArgs) Handles nupRTStep.ValueChanged

        If Manager IsNot Nothing Then
            Try
                Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
                i1.RealTimeStepMs = nupRTStep.Value
            Catch ex As Exception
            End Try
        End If

    End Sub

    Private Sub FormDynamicsManager_MouseEnter(sender As Object, e As EventArgs) Handles Me.MouseEnter

        CheckModelStatus()

    End Sub

    Private Sub chkResetAll_CheckedChanged(sender As Object, e As EventArgs) Handles chkResetAll.CheckedChanged
        If Manager IsNot Nothing Then
            Dim s1 = Manager.ScheduleList(gridschedules.Rows(gridschedules.SelectedCells(0).RowIndex).Cells(0).Value)
            Try
                s1.ResetContentsOfAllObjects = chkResetAll.Checked
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub btnRemoveIntegrator_Click(sender As Object, e As EventArgs) Handles btnRemoveIntegrator.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                           DWSIM.App.GetLocalString("Ateno2"),
                                           MessageBoxButtons.YesNo,
                                           MessageBoxIcon.Question) = DialogResult.Yes Then

            Manager.IntegratorList.Remove(gridintegrators.SelectedCells(0).OwningRow.Cells(0).Value)

            gridintegrators.Rows.RemoveAt(gridintegrators.SelectedCells(0).RowIndex)
            UpdateSelectables()
        End If
    End Sub

    Private Sub btnRemoveSchedule_Click(sender As Object, e As EventArgs) Handles btnRemoveSchedule.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                   DWSIM.App.GetLocalString("Ateno2"),
                                   MessageBoxButtons.YesNo,
                                   MessageBoxIcon.Question) = DialogResult.Yes Then

            Manager.ScheduleList.Remove(gridschedules.SelectedCells(0).OwningRow.Cells(0).Value)

            gridschedules.Rows.RemoveAt(gridschedules.SelectedCells(0).RowIndex)
            UpdateSelectables()
        End If
    End Sub

    Private Sub nupIntegrationStep_ValueChanged(sender As Object, e As EventArgs) Handles nupIntegrationStep.ValueChanged
        If Manager Is Nothing Then Exit Sub

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.IntegrationStep = New TimeSpan(0, 0, 0, 0, nupIntegrationStep.Value)
        Catch ex As Exception
        End Try
    End Sub

    Private Sub FormDynamicsManager_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

    Private Sub gridsets_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridsets.DataError

    End Sub

    Private Sub gridselectedset_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridselectedset.DataError

    End Sub

    Private Sub gridmatrices_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridmatrices.DataError

    End Sub

    Private Sub grdiselmatrix_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles grdiselmatrix.DataError

    End Sub

    Private Sub gridintegrators_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridintegrators.DataError

    End Sub

    Private Sub gridMonitoredVariables_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridMonitoredVariables.DataError

    End Sub

    Private Sub nupDays_ValueChanged(sender As Object, e As EventArgs) Handles nupHours.ValueChanged, nupDays.ValueChanged, nupSeconds.ValueChanged, nupMinutes.ValueChanged

        If Manager Is Nothing Then Exit Sub

        Try
            Dim i1 = Manager.IntegratorList(gridintegrators.Rows(gridintegrators.SelectedCells(0).RowIndex).Cells(0).Value)
            i1.Duration = New TimeSpan(nupDays.Value, nupHours.Value, nupMinutes.Value, nupSeconds.Value)
        Catch ex As Exception
        End Try

    End Sub

    Private Sub gridselectedset_SelectionChanged(sender As Object, e As EventArgs) Handles gridselectedset.SelectionChanged

        If gridselectedset.CurrentCell IsNot Nothing Then

            Dim eventID = gridselectedset.Rows(gridselectedset.CurrentCell.RowIndex).Cells(0).Value
            tbEventID.Text = eventID

        End If

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Clipboard.SetText(tbEventID.Text)

    End Sub

End Class