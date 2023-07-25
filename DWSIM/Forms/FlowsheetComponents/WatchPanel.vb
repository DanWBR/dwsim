Imports DWSIM.Interfaces
Imports DWSIM.SharedClasses.Extras

<System.Serializable()> Public Class WatchPanel

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private updating As Boolean = False
    Private loaded As Boolean = False

    Public Property Flowsheet As IFlowsheet

    Private Sub frmWatch_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub frmWatch_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        loaded = True

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Sub PopulateList()

        updating = True

        Me.dgv.Rows.Clear()

        Dim i As Integer = 0
        For Each newitem As IWatchItem In Flowsheet.WatchItems
            If My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection.ContainsKey(newitem.ObjID) Then
                Dim myobjname As String = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(newitem.ObjID).GraphicObject.Tag
                Dim propname As String = DWSIM.App.GetPropertyName(newitem.PropID)
                Dim propvalue As Object = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(newitem.ObjID).GetPropertyValue(newitem.PropID, My.Application.ActiveSimulation.Options.SelectedUnitSystem)
                Dim propunit As String = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(newitem.ObjID).GetPropertyUnit(newitem.PropID, My.Application.ActiveSimulation.Options.SelectedUnitSystem)
                Me.dgv.Rows.Add(New Object() {i, newitem.ObjID, newitem.PropID, newitem.IsReadOnly, myobjname, propname & " (" & propunit & ")", propvalue})
                If newitem.IsReadOnly Then
                    Me.dgv.Rows(Me.dgv.Rows.Count - 1).ReadOnly = True
                    Me.dgv.Rows(Me.dgv.Rows.Count - 1).Cells(6).Style.BackColor = Color.LightGray
                End If
            End If
            i += 1
        Next

        updating = False

    End Sub

    Public Sub UpdateList()

        updating = True

        Dim toremove As New ArrayList

        For Each r As DataGridViewRow In dgv.Rows
            Dim wi As WatchItem = Flowsheet.WatchItems(r.Cells(0).Value)
            If My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection.ContainsKey(wi.ObjID) Then
                Dim myobjname As String = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(wi.ObjID).GraphicObject.Tag
                Dim propname As String = DWSIM.App.GetPropertyName(wi.PropID)
                Dim propvalue As Object = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(wi.ObjID).GetPropertyValue(wi.PropID, My.Application.ActiveSimulation.Options.SelectedUnitSystem)
                Dim propunit As String = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(wi.ObjID).GetPropertyUnit(wi.PropID, My.Application.ActiveSimulation.Options.SelectedUnitSystem)
                With r
                    .Cells(4).Value = myobjname
                    .Cells(5).Value = propname & " (" & propunit & ")"
                    .Cells(6).Value = propvalue
                End With
            Else
                toremove.Add(Me.dgv.Rows.IndexOf(r))
                Flowsheet.WatchItems.RemoveAt(r.Cells(0).Value)
            End If
        Next
        For Each r As Integer In toremove
            Me.dgv.Rows.RemoveAt(r)
        Next

        updating = False

    End Sub

    Private Sub btnAdd_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnAdd.Click

        Dim frmprop As New FormPropSelection

        frmprop.ssmode = False
        frmprop.ShowDialog(Me)

        Dim newitem As WatchItem = frmprop.wi

        If Not newitem Is Nothing Then
            Dim id As Integer = New Random().Next
            Flowsheet.WatchItems.Add(newitem)
            Me.PopulateList()
        End If

    End Sub

    Private Sub dgv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv.CellValueChanged

        If updating = False And loaded Then

            If e.ColumnIndex = 6 And e.RowIndex >= 0 Then

                Dim wi As WatchItem = Flowsheet.WatchItems(Me.dgv.Rows(e.RowIndex).Cells(0).Value)
                If My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection.ContainsKey(wi.ObjID) Then
                    Dim myobjname As String = My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(wi.ObjID).GraphicObject.Tag
                    Dim propname As String = DWSIM.App.GetPropertyName(wi.PropID)
                    Dim propvalue As Object = Me.dgv.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                    My.Application.ActiveSimulation.Collections.FlowsheetObjectCollection(wi.ObjID).SetPropertyValue(wi.PropID, propvalue, My.Application.ActiveSimulation.Options.SelectedUnitSystem)
                    FlowsheetSolver.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, wi.ObjID)
                    Me.dgv.Focus()
                End If

            End If

        End If

    End Sub

    Private Sub btnRemove_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRemove.Click

        For i As Integer = 0 To Me.dgv.SelectedRows.Count - 1
            Flowsheet.WatchItems.RemoveAt(Me.dgv.SelectedRows(i).Cells(0).Value)
            Me.dgv.Rows.Remove(Me.dgv.SelectedRows(i))
        Next

    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        UpdateList()
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

End Class