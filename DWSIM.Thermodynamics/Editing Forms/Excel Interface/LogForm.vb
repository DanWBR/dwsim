Public Class LogForm

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        TextBox1.Text = ""
    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        Clipboard.SetText(TextBox1.Text)
    End Sub

    Private Sub LogForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub
End Class