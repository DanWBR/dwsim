Public Class FormCoolPropWarning

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        My.Settings.ShowCoolPropWarning = Not Me.CheckBox1.Checked
        Me.Close()

    End Sub

    Private Sub FormCoolPropWarning_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.CheckBox1.Checked = Not My.Settings.ShowCoolPropWarning

    End Sub
End Class