Imports LukeSw.Windows.Forms

Public Class UISpreadsheetEditorForm
    Inherits System.Windows.Forms.Form

    Friend Loaded As Boolean = False
    Friend OldValue As String = ""
    Friend OldTag As String = ""

    Private Sub UISpreadsheetEditorForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        e.Cancel = True
        Me.Hide()
    End Sub

    Private Sub UISpreadsheetEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Me.DataGridView1.Rows.Add(20)
        Dim row As DataGridViewRow
        Dim i As Integer = 1
        For Each row In Me.DataGridView1.Rows
            row.HeaderCell.Value = i.ToString
            i = i + 1
        Next
    End Sub

    Public Function GetCellValue(ByVal cell As String) As DataGridViewCell

        Dim column, row As Integer
        Dim colLetra As String

        colLetra = cell.Substring(0, 1)
        row = CInt(cell.Substring(1)) - 1

        Select Case colLetra
            Case "A"
                column = 0
            Case "B"
                column = 1
            Case "C"
                column = 2
            Case "D"
                column = 3
            Case "E"
                column = 4
            Case "F"
                column = 5
            Case "G"
                column = 6
            Case "H"
                column = 7
            Case Else
                Return Nothing
        End Select

        Return Me.DataGridView1.Rows(row).Cells(column)

    End Function

    Private Sub DataGridView1_CellBeginEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellCancelEventArgs) Handles DataGridView1.CellBeginEdit

        Dim c, r As Integer

        c = e.ColumnIndex
        r = e.RowIndex

        Dim Cell As DataGridViewCell = Me.DataGridView1.Rows(r).Cells(c)

        Me.OldValue = Cell.Value
        Me.OldTag = Cell.Tag

        Cell.Value = Cell.Tag
        Me.tbValue.Text = Cell.Tag

    End Sub

    Private Sub DataGridView1_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellEndEdit

        Dim c, r As Integer
        Dim expression As String

        c = e.ColumnIndex
        r = e.RowIndex


        Dim Cell As DataGridViewCell = Me.DataGridView1.Rows(r).Cells(c)

        expression = Cell.Value

        Try
            Cell.Tag = expression
            'Cell.Value = ExpressionEval.Evaluate(expression)
        Catch ex As Exception
            Cell.Value = Me.OldValue
            Cell.Tag = Me.OldTag
            VDialog.Show("Expressão inválida." & vbCrLf & ex.Message, GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub DataGridView1_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValueChanged

    End Sub

    Private Sub DataGridView1_SelectionChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles DataGridView1.SelectionChanged

        Me.tbCell.Text = Me.GetCellString(Me.DataGridView1.SelectedCells(0))
        Me.tbValue.Text = Me.DataGridView1.SelectedCells(0).Tag

    End Sub

    Public Function GetCellString(ByVal cell As DataGridViewCell) As String

        Dim str As String = Me.DataGridView1.Columns(cell.ColumnIndex).Name & CStr(cell.RowIndex + 1)

        Return str

    End Function


    Private Sub UISpreadsheetEditorForm_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.Loaded = True
    End Sub
End Class