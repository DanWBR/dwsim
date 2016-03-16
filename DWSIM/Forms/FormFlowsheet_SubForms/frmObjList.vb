Imports WeifenLuo.WinFormsUI.Docking
Imports Microsoft.MSDN.Samples.GraphicObjects

Public Class frmObjList
    Inherits DockContent

    Private Flowsheet As FormFlowsheet

    Public ACSC As New AutoCompleteStringCollection

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Private Sub frmObjList_GotFocus(ByVal sender As Object, ByVal e As System.EventArgs) Handles TBSearch.GotFocus
        If Not Flowsheet Is Nothing Then
            Dim arrays(Flowsheet.Collections.ObjectCollection.Count) As String
            Dim aNode As TreeNode
            Dim i As Integer = 0
            For Each aNode In Me.TreeViewObj.Nodes
                If aNode.Level > 0 Then
                    arrays(i) = aNode.Text
                    i += 1
                End If
            Next
            ACSC.AddRange(arrays)
            Me.TBSearch.AutoCompleteCustomSource = ACSC
        End If

    End Sub

    Private Sub frmObjList_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Flowsheet = My.Application.ActiveSimulation
    End Sub

    Private Sub TBSearch_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TBSearch.TextChanged
        Dim nodes As TreeNode() = Me.TreeViewObj.Nodes.Find(Me.TBSearch.Text, True)
        If nodes.Length > 0 Then
            Me.TreeViewObj.SelectedNode = nodes(0)
            Me.TBSearch.Focus()
            Me.TBSearch.ForeColor = Color.Blue
        Else
            Me.TBSearch.ForeColor = Color.Red
        End If
    End Sub

    Private Sub ToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        If Not Me.TreeViewObj.SelectedNode Is Nothing Then
            Flowsheet = My.Application.ActiveSimulation
            Dim gObj = FormFlowsheet.SearchSurfaceObjectsByName(Me.TreeViewObj.SelectedNode.Name, Flowsheet.FormSurface.FlowsheetDesignSurface)

            If Not gObj Is Nothing Then

                Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject = gObj
                Call Flowsheet.FormSurface.FlowsheetDesignSurface_MouseUp(sender, New MouseEventArgs(Windows.Forms.MouseButtons.Left, 1, MousePosition.X, MousePosition.Y, 0))

            End If
        End If

    End Sub

    Private Sub ToolStripMenuItem2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem2.Click
        Flowsheet = My.Application.ActiveSimulation
        Try
            Dim gObj = FormFlowsheet.SearchSurfaceObjectsByName(Me.TreeViewObj.SelectedNode.Name, Flowsheet.FormSurface.FlowsheetDesignSurface)
            If Not gObj Is Nothing Then

                Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject = gObj
                Call Flowsheet.FormSurface.FlowsheetDesignSurface_MouseUp(sender, New MouseEventArgs(Windows.Forms.MouseButtons.Left, 1, MousePosition.X, MousePosition.Y, 0))

                Flowsheet.FormSurface.FlowsheetDesignSurface.AutoScrollPosition = New Point(gObj.X * Flowsheet.FormSurface.FlowsheetDesignSurface.Zoom - Flowsheet.FormSurface.Width / 2, gObj.Y * Flowsheet.FormSurface.FlowsheetDesignSurface.Zoom - Flowsheet.FormSurface.Height / 2)

            End If
        Catch ex As Exception
            Flowsheet.WriteToLog(ex.Message, Color.Red, DWSIM.FormClasses.TipoAviso.Erro)
        End Try


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