Imports WeifenLuo.WinFormsUI.Docking

Public Class LogPanel

    Inherits DockContent

    Public loaded As Boolean = False

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Private Sub frmLog_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Grid1.Sort(Grid1.Columns(1), System.ComponentModel.ListSortDirection.Descending)
        Grid1.AutoSizeRowsMode = DataGridViewAutoSizeRowsMode.AllCells

    End Sub

    Private Sub frmLog_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        loaded = True
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

    Private Sub Grid1_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles Grid1.CellContentClick

        Dim senderGrid = DirectCast(sender, DataGridView)

        If TypeOf senderGrid.Columns(e.ColumnIndex) Is DataGridViewButtonColumn AndAlso e.RowIndex >= 0 Then

            Dim eid As String = senderGrid.Rows(e.RowIndex).Cells(e.ColumnIndex).Tag

            If eid Is Nothing Then eid = ""

            If SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.ContainsKey(eid) Then

                Dim ex As Exception = SharedClasses.ExceptionProcessing.ExceptionList.Exceptions(eid)

                Dim pex As ExceptionProcessing.ProcessedException = ExceptionProcessing.ExceptionParser.ParseException(ex)

                Dim fevent As New FormEventDescription With {.PEx = pex}

                fevent.Show()

            End If

        End If

    End Sub

    Private Sub LimparListaToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LimparListaToolStripMenuItem.Click

        Grid1.Rows.Clear()

    End Sub

    Private Sub CopiarInformaçõesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopiarInformaçõesToolStripMenuItem.Click

        If Grid1.SelectedRows.Count > 0 Then

            Dim selectedrow = Grid1.SelectedRows(0)

            Dim eid As String = Grid1.Rows(selectedrow.Index).Cells(5).Tag

            If eid Is Nothing Then eid = ""

            If SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.ContainsKey(eid) Then

                Dim ex As Exception = SharedClasses.ExceptionProcessing.ExceptionList.Exceptions(eid)

                Clipboard.SetText(ex.ToString())

            Else

                Clipboard.SetDataObject(Grid1.GetClipboardContent())

            End If

        End If


    End Sub

End Class