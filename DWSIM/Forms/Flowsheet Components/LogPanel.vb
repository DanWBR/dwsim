Imports WeifenLuo.WinFormsUI.Docking

Public Class LogPanel

    Inherits DockContent

    Private dt As New DataTable("Log")
    Private dv As New DataView
    Private bindingSource1 As New BindingSource()

    Public loaded As Boolean = False

    Public Property GridDT() As DataTable
        Get
            Return Me.dt
        End Get
        Set(ByVal value As DataTable)
            Me.dt = value
        End Set
    End Property

    Public Property GridDV() As DataView
        Get
            Return Me.dv
        End Get
        Set(ByVal value As DataView)
            Me.dv = value
        End Set
    End Property

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Private Sub frmLog_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        If dt.Columns.Count < 4 Then
            dt.Columns.Add("Imagem", GetType(Bitmap))
            dt.Columns.Add("Data")
            dt.Columns.Add("Tipo")
            dt.Columns.Add("Mensagem")
            dt.Columns.Add("Cor", GetType(Color))
            dt.Columns.Add("Indice")
        ElseIf dt.Columns.Count = 4 Then
            dt.Columns.Add("Cor", GetType(Color))
            dt.Columns.Add("Indice")
        ElseIf dt.Columns.Count = 5 Then
            dt.Columns.Add("Indice")
        End If
        dt.RemotingFormat = SerializationFormat.Binary
        dv = New DataView(dt)
        Me.bindingSource1.DataSource = dv
        With Me.Grid1
            .AutoGenerateColumns = False
            If Not DWSIM.App.IsRunningOnMono Then
                .DataSource = Me.bindingSource1
            End If
            .Columns("Imagem").DataPropertyName = "Imagem"
            .Columns("Data").DataPropertyName = "Data"
            .Columns("Tipo").DataPropertyName = "Tipo"
            .Columns("Mensagem").DataPropertyName = "Mensagem"
            .Columns("Indice").DataPropertyName = "Indice"
        End With
        Me.dt.PrimaryKey = New DataColumn() {Me.dt.Columns("Indice")}
        With Me.dt.Columns("Indice")
            .AutoIncrement = True
            .AutoIncrementSeed = 1
            .AutoIncrementStep = 1
            .Unique = True
        End With
        Grid1.Sort(Grid1.Columns(1), System.ComponentModel.ListSortDirection.Ascending)
    End Sub

    Private Sub Grid1_RowsAdded(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewRowsAddedEventArgs) Handles Grid1.RowsAdded

        Dim ci, cw, ce As Integer
        ci = 0
        cw = 0
        ce = 0
        For Each rw As DataRow In Me.dt.Rows
            Select Case rw.Item(2)
                Case DWSIM.App.GetLocalString("Aviso")
                    cw += 1
                Case DWSIM.App.GetLocalString("Erro")
                    ce += 1
                Case DWSIM.App.GetLocalString("Mensagem")
                    ci += 1
            End Select
        Next
        If ci = 1 Then Me.ToolStripButton5.Text = ci & DWSIM.App.GetLocalString("Mensagens1") Else Me.ToolStripButton5.Text = ci & DWSIM.App.GetLocalString("Mensagens")
        If cw = 1 Then Me.ToolStripButton1.Text = cw & DWSIM.App.GetLocalString("Avisos1") Else Me.ToolStripButton1.Text = cw & DWSIM.App.GetLocalString("Avisos")
        If ce = 1 Then Me.ToolStripButton3.Text = ce & DWSIM.App.GetLocalString("Erros1") Else Me.ToolStripButton3.Text = ce & DWSIM.App.GetLocalString("Erros")

        If Me.Grid1.Rows.Count > 0 Then
            Me.Grid1.ClearSelection()
            Me.Grid1.Rows(0).Selected = True
            Try
                Me.Grid1.FirstDisplayedScrollingRowIndex = 0
            Catch ex As Exception

            End Try
        End If


        Try
            Dim currentrow As DataGridViewRow = Grid1.Rows(e.RowIndex)
            If DWSIM.App.IsRunningOnMono Then currentrow.Height = currentrow.GetPreferredHeight(e.RowIndex, DataGridViewAutoSizeRowMode.AllCells, True)
            currentrow.Cells(4).Style.ForeColor = dt.Rows(currentrow.Cells(1).Value).Item("Cor")
        Catch ex As Exception
        End Try

        If DWSIM.App.IsRunningOnMono Then Grid1.Sort(Grid1.Columns(1), System.ComponentModel.ListSortDirection.Descending)

    End Sub

    Private Sub Grid1_RowsRemoved(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewRowsRemovedEventArgs) Handles Grid1.RowsRemoved
        Dim ci, cw, ce As Integer
        ci = 0
        cw = 0
        ce = 0
        For Each rw As DataRow In Me.dt.Rows
            Select Case rw.Item(2)
                Case DWSIM.App.GetLocalString("Aviso")
                    cw += 1
                Case DWSIM.App.GetLocalString("Erro")
                    ce += 1
                Case DWSIM.App.GetLocalString("Mensagem")
                    ci += 1
            End Select
        Next
        If ci = 1 Then Me.ToolStripButton5.Text = ci & DWSIM.App.GetLocalString("Mensagens1") Else Me.ToolStripButton5.Text = ci & DWSIM.App.GetLocalString("Mensagens")
        If cw = 1 Then Me.ToolStripButton1.Text = cw & DWSIM.App.GetLocalString("Avisos1") Else Me.ToolStripButton1.Text = cw & DWSIM.App.GetLocalString("Avisos")
        If ce = 1 Then Me.ToolStripButton3.Text = ce & DWSIM.App.GetLocalString("Erros1") Else Me.ToolStripButton3.Text = ce & DWSIM.App.GetLocalString("Erros")

        If Me.Grid1.Rows.Count > 0 Then
            Me.Grid1.ClearSelection()
            Me.Grid1.Rows(Me.Grid1.Rows.Count - 1).Selected = True
            Try
                Me.Grid1.FirstDisplayedScrollingRowIndex = Me.Grid1.Rows.Count - 1
            Catch ex As Exception
            End Try
        End If

        Try
            If Grid1.Rows.Count > e.RowIndex Then
                Dim currentrow As DataGridViewRow = Grid1.Rows(e.RowIndex)
                currentrow.Cells(4).Style.ForeColor = dt.Rows.Find(currentrow.Cells(1).Value).Item("Cor")
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.CheckedChanged
        If loaded Then
            If Me.ToolStripButton5.Checked Then
                If ToolStripButton1.Checked = False And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                ElseIf ToolStripButton1.Checked = False And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Aviso") + "'"
                ElseIf ToolStripButton1.Checked = True And ToolStripButton3.Checked = True Then
                    dv.RowFilter = ""
                ElseIf ToolStripButton1.Checked = True And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "'"
                End If
            Else
                If ToolStripButton1.Checked = False And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Aviso") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                ElseIf ToolStripButton1.Checked = False And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton1.Checked = True And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                ElseIf ToolStripButton1.Checked = True And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Aviso") + "'"
                End If
            End If
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.CheckedChanged
        If loaded Then
            If Me.ToolStripButton3.Checked Then
                If ToolStripButton5.Checked = False And ToolStripButton1.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton5.Checked = False And ToolStripButton1.Checked = True Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Aviso") + "' OR Tipo = '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton1.Checked = True Then
                    dv.RowFilter = ""
                ElseIf ToolStripButton5.Checked = True And ToolStripButton1.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Mensagem") + "' OR Tipo = '" + DWSIM.App.GetLocalString("Erro") + "'"
                End If
            Else
                If ToolStripButton5.Checked = False And ToolStripButton1.Checked = False Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Mensagem") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Aviso") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton5.Checked = False And ToolStripButton1.Checked = True Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Aviso") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton1.Checked = True Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton1.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                End If
            End If
        End If
    End Sub

    Private Sub frmLog_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        loaded = True
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.CheckedChanged
        If loaded Then
            If Me.ToolStripButton1.Checked Then
                If ToolStripButton5.Checked = False And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Aviso") + "'"
                ElseIf ToolStripButton5.Checked = False And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton3.Checked = True Then
                    dv.RowFilter = ""
                ElseIf ToolStripButton5.Checked = True And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "'"
                End If
            Else
                If ToolStripButton5.Checked = False And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Mensagem") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Erro") + "' AND Tipo <> '" + DWSIM.App.GetLocalString("Aviso") + "'"
                ElseIf ToolStripButton5.Checked = False And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Erro") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton3.Checked = True Then
                    dv.RowFilter = "Tipo <> '" + DWSIM.App.GetLocalString("Aviso") + "'"
                ElseIf ToolStripButton5.Checked = True And ToolStripButton3.Checked = False Then
                    dv.RowFilter = "Tipo = '" + DWSIM.App.GetLocalString("Mensagem") + "'"
                End If
            End If
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        Me.dt.Clear()
        If DWSIM.App.IsRunningOnMono Then
            Me.Grid1.Rows.Clear()
        End If
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