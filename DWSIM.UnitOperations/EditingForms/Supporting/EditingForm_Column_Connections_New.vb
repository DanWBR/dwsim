Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps

Public Class EditingForm_Column_Connections_New

    Public rc As UnitOperations.Column

    Private loaded As Boolean = False

    Private stageNames, stageIDs As List(Of String)

    Public ownerform As EditingForm_Column

    Sub UpdateInfo()

        ChangeDefaultFont(Me)

        Dim i As Integer = 0

        Dim su = rc.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
        Dim nf = rc.GetFlowsheet().FlowsheetOptions.NumberFormat

        loaded = False

        gridAssociations.Rows.Clear()
        gridDuties.Rows.Clear()
        gridFeeds.Rows.Clear()
        gridProducts.Rows.Clear()
        gridSideDraws.Rows.Clear()
        gridSideDrawSpecs.Rows.Clear()

        gridSideDrawSpecs.Columns(3).HeaderText = "Molar Flow (" + su.molarflow + ")"

        Dim istrs = rc.GraphicObject.InputConnectors.Where(Function(x) (x.IsAttached AndAlso x.ConnectorName.Contains("Feed"))).Select(Function(x2) x2.AttachedConnector.AttachedFrom.Name).ToList
        Dim ostrs = rc.GraphicObject.OutputConnectors.Where(Function(x) (x.IsAttached AndAlso x.ConnectorName.Contains("Side"))).Select(Function(x2) x2.AttachedConnector.AttachedTo.Name).ToList
        Dim dist = rc.GraphicObject.OutputConnectors.Where(Function(x) (x.IsAttached AndAlso (x.ConnectorName.Contains("Distillate") Or x.ConnectorName.Contains("Top")))).Select(Function(x2) x2.AttachedConnector.AttachedTo.Name).ToList
        Dim ov = rc.GraphicObject.OutputConnectors.Where(Function(x) (x.IsAttached AndAlso (x.ConnectorName.Contains("Overhead")))).Select(Function(x2) x2.AttachedConnector.AttachedTo.Name).ToList
        Dim bottoms = rc.GraphicObject.OutputConnectors.Where(Function(x) (x.IsAttached AndAlso x.ConnectorName.Contains("Bottoms"))).Select(Function(x2) x2.AttachedConnector.AttachedTo.Name).ToList
        Dim rduty = rc.GraphicObject.InputConnectors.Where(Function(x) (x.IsAttached AndAlso x.ConnectorName.Contains("Reboiler"))).Select(Function(x2) x2.AttachedConnector.AttachedFrom.Name).ToList
        Dim cduty = rc.GraphicObject.OutputConnectors.Where(Function(x) (x.IsAttached AndAlso x.ConnectorName.Contains("Condenser"))).Select(Function(x2) x2.AttachedConnector.AttachedTo.Name).ToList

        For Each id In istrs
            If (rc.MaterialStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.Feed
                    })
            End If
        Next
        For Each id In ostrs
            If (rc.MaterialStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.Sidedraw
                    })
            End If
        Next
        For Each id In ov
            If (rc.MaterialStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.OverheadVapor
                    })
            End If
        Next
        For Each id In dist
            If (rc.MaterialStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                If TypeOf rc Is DistillationColumn Then
                    rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.Distillate
                    })
                ElseIf TypeOf rc Is AbsorptionColumn Then
                    rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.OverheadVapor
                    })
                End If
            End If
        Next
        For Each id In bottoms
            If (rc.MaterialStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.MaterialStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Material,
                        .StreamBehavior = StreamInformation.Behavior.BottomsLiquid
                    })
            End If
        Next

        Dim remove As List(Of String) = New List(Of String)
        For Each si In rc.MaterialStreams
            If (Not istrs.Contains(si.Value.StreamID) _
                        AndAlso (Not ov.Contains(si.Value.StreamID) _
                        AndAlso (Not ostrs.Contains(si.Value.StreamID) _
                        AndAlso (Not dist.Contains(si.Value.StreamID) _
                        AndAlso Not bottoms.Contains(si.Value.StreamID))))) Then
                If si.Value.ID <> "" Then remove.Add(si.Value.ID) Else remove.Add(si.Key)
            End If
            If Not rc.GetFlowsheet.SimulationObjects.ContainsKey(si.Value.StreamID) Then
                If si.Value.ID <> "" Then remove.Add(si.Value.ID) Else remove.Add(si.Key)
            End If
        Next
        For Each id In remove
            If rc.MaterialStreams.ContainsKey(id) Then
                rc.MaterialStreams.Remove(id)
            End If
        Next

        For Each id In cduty
            If (rc.EnergyStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.EnergyStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Energy,
                        .StreamBehavior = StreamInformation.Behavior.Distillate
                    })
            End If
        Next
        For Each id In rduty
            If (rc.EnergyStreams.Values.Where(Function(x) (x.StreamID = id)).Count = 0) Then
                rc.EnergyStreams.Add(id, New StreamInformation With
                    {
                        .StreamID = id,
                        .ID = id,
                        .StreamType = StreamInformation.Type.Energy,
                        .StreamBehavior = StreamInformation.Behavior.BottomsLiquid
                    })
            End If
        Next

        Dim remove2 As List(Of String) = New List(Of String)
        For Each si In rc.EnergyStreams
            If (Not rduty.Contains(si.Value.StreamID) _
                        AndAlso Not cduty.Contains(si.Value.StreamID)) Then
                If si.Value.ID <> "" Then remove2.Add(si.Value.ID) Else remove2.Add(si.Key)
            End If
            If Not rc.GetFlowsheet.SimulationObjects.ContainsKey(si.Value.StreamID) Then
                If si.Value.ID <> "" Then remove2.Add(si.Value.ID) Else remove2.Add(si.Key)
            End If
        Next
        For Each id In remove2
            If rc.EnergyStreams.ContainsKey(id) Then
                rc.EnergyStreams.Remove(id)
            End If
        Next

        stageNames = rc.Stages.Select(Function(x) x.Name).ToList
        stageIDs = rc.Stages.Select(Function(x) x.ID).ToList
        stageNames.Insert(0, "")
        stageIDs.Insert(0, "")

        Dim cbMS As New DataGridViewComboBoxCell
        Dim cbES As New DataGridViewComboBoxCell
        Dim cbST As New DataGridViewComboBoxCell

        cbMS.Items.Add("")
        Try
            cbMS.Items.AddRange(rc.GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.MaterialStream).Select(Function(x2) x2.GraphicObject.Tag).OrderBy(Function(x) x).ToArray)
        Catch ex As Exception
        End Try
        cbES.Items.Add("")
        Try
            cbES.Items.AddRange(rc.GetFlowsheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = Enums.GraphicObjects.ObjectType.EnergyStream).Select(Function(x2) x2.GraphicObject.Tag).OrderBy(Function(x) x).ToArray)
        Catch ex As Exception
        End Try
        cbST.Items.AddRange(stageNames.ToArray)

        DirectCast(gridFeeds.Columns(2), DataGridViewComboBoxColumn).CellTemplate = cbMS
        DirectCast(gridProducts.Columns(2), DataGridViewComboBoxColumn).CellTemplate = cbMS
        DirectCast(gridSideDraws.Columns(2), DataGridViewComboBoxColumn).CellTemplate = cbMS
        DirectCast(gridDuties.Columns(2), DataGridViewComboBoxColumn).CellTemplate = cbES
        DirectCast(gridAssociations.Columns(3), DataGridViewComboBoxColumn).CellTemplate = cbST

        i = 0
        For Each item In rc.GraphicObject.InputConnectors
            If item.IsEnergyConnector Or item.Type = Enums.GraphicObjects.ConType.ConEn Then
                gridDuties.Rows.Add(New Object() {i, item.ConnectorName, If(item.IsAttached, item.AttachedConnector.AttachedFrom.Tag, "")})
            Else
                gridFeeds.Rows.Add(New Object() {i, item.ConnectorName, If(item.IsAttached, item.AttachedConnector.AttachedFrom.Tag, "")})
            End If
            i += 1
        Next
        i = 0
        For Each item In rc.GraphicObject.OutputConnectors
            If item.IsEnergyConnector Or item.Type = Enums.GraphicObjects.ConType.ConEn Then
                gridDuties.Rows.Add(New Object() {i, item.ConnectorName, If(item.IsAttached, item.AttachedConnector.AttachedTo.Tag, "")})
            Else
                If item.ConnectorName.ToLower.Contains("side") Then
                    Try
                        gridSideDraws.Rows.Add(New Object() {i, item.ConnectorName, If(item.IsAttached, item.AttachedConnector.AttachedTo.Tag, "")})
                    Catch ex As Exception
                    End Try
                Else
                    gridProducts.Rows.Add(New Object() {i, item.ConnectorName, If(item.IsAttached, item.AttachedConnector.AttachedTo.Tag, "")})
                End If
            End If
            i += 1
        Next

        gridDuties.Sort(gridDuties.Columns(1), System.ComponentModel.ListSortDirection.Ascending)

        Dim stage As String = ""

        For Each si In rc.MaterialStreams.Values
            If stageNames.Contains(si.AssociatedStage) Then
                stage = si.AssociatedStage
            Else
                If Integer.TryParse(si.AssociatedStage, New Integer) Then
                    Try
                        stage = stageNames(CInt(si.AssociatedStage) + 1)
                    Catch ex As Exception
                    End Try
                Else
                    Try
                        stage = stageNames(stageIDs.IndexOf(si.AssociatedStage))
                    Catch ex As Exception
                    End Try
                End If
            End If
            If (si.StreamBehavior = StreamInformation.Behavior.Feed) Then
                gridAssociations.Rows.Add(New Object() {si.ID, "Feed", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.Sidedraw) Then
                gridAssociations.Rows.Add(New Object() {si.ID, "Side Draw", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.Distillate) Then
                If stage = "" Then stage = stageNames(1)
                gridAssociations.Rows.Add(New Object() {si.ID, "Distillate", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.OverheadVapor) Then
                If stage = "" Then stage = stageNames(1)
                gridAssociations.Rows.Add(New Object() {si.ID, "Overhead Vapor", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.OverheadVapor) Then
                If stage = "" Then stage = stageNames(1)
                gridAssociations.Rows.Add(New Object() {si.ID, "Top Product", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.BottomsLiquid) Then
                If stage = "" Then stage = stageNames.Last
                gridAssociations.Rows.Add(New Object() {si.ID, "Bottoms Product", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            End If

        Next
        For Each si In rc.EnergyStreams.Values
            If stageNames.Contains(si.AssociatedStage) Then
                stage = si.AssociatedStage
            Else
                If Integer.TryParse(si.AssociatedStage, New Integer) Then
                    Try
                        stage = stageNames(CInt(si.AssociatedStage) + 1)
                    Catch ex As Exception
                    End Try
                Else
                    Try
                        stage = stageNames(stageIDs.IndexOf(si.AssociatedStage))
                    Catch ex As Exception
                    End Try
                End If
            End If
            If (si.StreamBehavior = StreamInformation.Behavior.Distillate) Then
                'gridAssociations.Rows.Add(New Object() {si.ID, "Condenser Duty", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            ElseIf (si.StreamBehavior = StreamInformation.Behavior.BottomsLiquid) Then
                'gridAssociations.Rows.Add(New Object() {si.ID, "Reboiler Duty", rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, stage})
            End If
        Next

        Dim cbSDP As New DataGridViewComboBoxCell
        cbSDP.Items.Add("Liquid")
        cbSDP.Items.Add("Vapor")

        DirectCast(gridSideDrawSpecs.Columns(2), DataGridViewComboBoxColumn).CellTemplate = cbSDP

        For Each si In rc.MaterialStreams.Values
            Dim sp As String = "Liquid"
            Select Case si.StreamPhase
                Case StreamInformation.Phase.L
                    sp = "Liquid"
                Case StreamInformation.Phase.V
                    sp = "Vapor"
            End Select
            If (si.StreamBehavior = StreamInformation.Behavior.Sidedraw) Then
                gridSideDrawSpecs.Rows.Add(New Object() {si.ID, rc.GetFlowsheet().SimulationObjects(si.StreamID).GraphicObject.Tag, sp, si.FlowRate.Value.ConvertFromSI(su.molarflow).ToString(nf)})
            End If
        Next

        TabControl1.SelectedIndex = ownerform.tab1
        TabControl2.SelectedIndex = ownerform.tab2

        loaded = True


        AddHandler gridAssociations.EditingControlShowing, AddressOf Me.EditingControlShowing1

        AddHandler gridDuties.EditingControlShowing, AddressOf Me.EditingControlShowing2

        AddHandler gridFeeds.EditingControlShowing, AddressOf Me.EditingControlShowing3

        AddHandler gridProducts.EditingControlShowing, AddressOf Me.EditingControlShowing4

        AddHandler gridSideDraws.EditingControlShowing, AddressOf Me.EditingControlShowing5

        AddHandler gridSideDrawSpecs.EditingControlShowing, AddressOf Me.EditingControlShowing6

    End Sub

    Private Sub EditingControlShowing1(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted(ByVal sender As Object, ByVal e As EventArgs)
        gridAssociations.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub EditingControlShowing2(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted2
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted2
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted2(ByVal sender As Object, ByVal e As EventArgs)
        gridDuties.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub EditingControlShowing3(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted3
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted3
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted3(ByVal sender As Object, ByVal e As EventArgs)
        gridFeeds.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub EditingControlShowing4(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted4
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted4
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted4(ByVal sender As Object, ByVal e As EventArgs)
        gridProducts.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub EditingControlShowing5(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted5
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted5
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted5(ByVal sender As Object, ByVal e As EventArgs)
        gridSideDraws.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub EditingControlShowing6(ByVal sender As Object, ByVal e As DataGridViewEditingControlShowingEventArgs)
        If (e.Control.GetType = GetType(DataGridViewComboBoxEditingControl)) Then
            Dim cmb As ComboBox = CType(e.Control, ComboBox)
            RemoveHandler DirectCast(sender, DataGridView).EditingControlShowing, AddressOf cmb_SelectionChangeCommitted6
            AddHandler cmb.SelectionChangeCommitted, AddressOf cmb_SelectionChangeCommitted6
            SendKeys.Send("{F4}")
        End If
    End Sub

    Private Sub cmb_SelectionChangeCommitted6(ByVal sender As Object, ByVal e As EventArgs)
        gridSideDrawSpecs.CurrentCell.Value = CType(sender, DataGridViewComboBoxEditingControl).EditingControlFormattedValue
    End Sub

    Private Sub gridSideDrawSpecs_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridSideDrawSpecs.CellValueChanged
        If loaded Then
            rc.FlowSheet.RegisterSnapshot(Enums.SnapshotType.ObjectData, rc)
            Dim id = gridSideDrawSpecs.Rows(e.RowIndex).Cells(0).Value
            Dim value = gridSideDrawSpecs.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            If rc.MaterialStreams.ContainsKey(id) Then
                If e.ColumnIndex = 3 Then
                    Dim su = rc.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
                    Try
                        rc.MaterialStreams(id).FlowRate.Value = value.ToString.ToDoubleFromCurrent.ConvertToSI(su.molarflow)
                    Catch ex As Exception
                        rc.FlowSheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                    End Try
                ElseIf e.ColumnIndex = 2 Then
                    If value = "Liquid" Then
                        rc.MaterialStreams(id).StreamPhase = StreamInformation.Phase.L
                    Else
                        rc.MaterialStreams(id).StreamPhase = StreamInformation.Phase.V
                    End If
                End If
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub gridAssociations_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridAssociations.CellValueChanged
        If loaded Then
            rc.FlowSheet.RegisterSnapshot(Enums.SnapshotType.ObjectData, rc)
            Dim id = gridAssociations.Rows(e.RowIndex).Cells(0).Value
            Dim value = gridAssociations.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            If rc.MaterialStreams.ContainsKey(id) Then
                rc.MaterialStreams(id).AssociatedStage = stageIDs(stageNames.IndexOf(value))
            ElseIf rc.EnergyStreams.ContainsKey(id) Then
                rc.EnergyStreams(id).AssociatedStage = stageIDs(stageNames.IndexOf(value))
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub gridFeeds_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridFeeds.CellValueChanged
        If loaded Then
            If e.ColumnIndex = 2 Then
                Dim id = gridFeeds.Rows(e.RowIndex).Cells(0).Value
                Dim value = gridFeeds.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim connector = rc.GraphicObject.InputConnectors(id)
                Dim direction = "In"
                ConnectionChanged(connector, direction, value)
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub gridProducts_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridProducts.CellValueChanged
        If loaded Then
            If e.ColumnIndex = 2 Then
                Dim id = gridProducts.Rows(e.RowIndex).Cells(0).Value
                Dim value = gridProducts.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim connector = rc.GraphicObject.OutputConnectors(id)
                Dim direction = "Out"
                ConnectionChanged(connector, direction, value)
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub gridDuties_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridDuties.CellValueChanged
        If loaded Then
            If e.ColumnIndex = 2 Then
                Dim id = gridDuties.Rows(e.RowIndex).Cells(0).Value
                Dim name = gridDuties.Rows(e.RowIndex).Cells(1).Value
                Dim value = gridDuties.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                If name = "Reboiler Duty" Then
                    Dim connector = rc.GraphicObject.InputConnectors(id)
                    Dim direction = "In"
                    ConnectionChanged(connector, direction, value)
                ElseIf name = "Condenser Duty" Then
                    Dim connector = rc.GraphicObject.OutputConnectors(id)
                    Dim direction = "Out"
                    ConnectionChanged(connector, direction, value)
                End If
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub gridSideDraws_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridSideDraws.CellValueChanged
        If loaded Then
            If e.ColumnIndex = 2 Then
                Dim id = gridSideDraws.Rows(e.RowIndex).Cells(0).Value
                Dim value = gridSideDraws.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim connector = rc.GraphicObject.OutputConnectors(id)
                Dim direction = "Out"
                ConnectionChanged(connector, direction, value)
            End If
            UpdateInfo()
        End If
    End Sub

    Private Sub TabControl1_TabIndexChanged(sender As Object, e As EventArgs) Handles TabControl1.Selected
        If loaded Then ownerform.tab1 = TabControl1.SelectedIndex
    End Sub

    Private Sub TabControl2_TabIndexChanged(sender As Object, e As EventArgs) Handles TabControl2.Selected
        If loaded Then ownerform.tab2 = TabControl2.SelectedIndex
    End Sub

    Private Sub ConnectionChanged(connector As IConnectionPoint, direction As String, value As String)

        Dim sel = value
        Dim gobj = rc.GraphicObject
        Dim flowsheet = rc.GetFlowsheet

        If (connector.IsAttached AndAlso (sel = "")) Then
            If ((direction = "In") AndAlso ((connector.Type = ConType.ConIn) Or (connector.Type = ConType.ConEn))) Then
                Try
                    flowsheet.DisconnectObjects(connector.AttachedConnector.AttachedFrom, gobj)
                Catch ex As Exception
                    flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                End Try
                flowsheet.UpdateInterface()
                Return
            ElseIf ((connector.Type = ConType.ConOut) Or (connector.Type = ConType.ConEn)) Then
                Try
                    flowsheet.DisconnectObjects(gobj, connector.AttachedConnector.AttachedTo)
                Catch ex As Exception
                    flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                End Try
                flowsheet.UpdateInterface()
                Return
            End If
        End If

        If (sel <> "") Then
            Dim gobj2 = flowsheet.GetFlowsheetSimulationObject(sel).GraphicObject
            If ((direction = "In") AndAlso ((connector.Type = ConType.ConIn) Or (connector.Type = ConType.ConEn))) Then
                If connector.IsAttached Then
                    Try
                        flowsheet.DisconnectObjects(connector.AttachedConnector.AttachedFrom, gobj)
                    Catch ex As Exception
                        flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                    End Try
                End If
                If connector.IsEnergyConnector Then
                    If gobj2.InputConnectors(0).IsAttached Then
                        flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError)
                        Return
                    End If
                    Try
                        flowsheet.ConnectObjects(gobj, gobj2, 0, 0)
                    Catch ex As Exception
                        flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                    End Try
                Else
                    If gobj2.OutputConnectors(0).IsAttached Then
                        flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError)
                        Return
                    End If
                    Try
                        flowsheet.ConnectObjects(gobj2, gobj, 0, gobj.InputConnectors.IndexOf(connector))
                    Catch ex As Exception
                        flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                    End Try
                End If
            ElseIf ((connector.Type = ConType.ConOut) Or (connector.Type = ConType.ConEn)) Then
                If gobj2.InputConnectors(0).IsAttached Then
                    flowsheet.ShowMessage("Selected object already connected to another object.", IFlowsheet.MessageType.GeneralError)
                    Return
                End If
                Try
                    If connector.IsAttached Then
                        flowsheet.DisconnectObjects(gobj, connector.AttachedConnector.AttachedTo)
                    End If

                    flowsheet.ConnectObjects(gobj, gobj2, gobj.OutputConnectors.IndexOf(connector), 0)
                Catch ex As Exception
                    flowsheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
                End Try
            End If
            flowsheet.UpdateInterface()
        End If

    End Sub

    Private Sub grid_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridSideDraws.DataError, gridAssociations.DataError, gridDuties.DataError, gridFeeds.DataError, gridProducts.DataError, gridSideDrawSpecs.DataError
        'data error
    End Sub

End Class
