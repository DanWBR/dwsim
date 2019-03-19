Public Class FormWebPanel

    Inherits UserControl

    Private Sub chkDoNotShow_CheckedChanged(sender As Object, e As EventArgs) Handles chkDoNotShow.CheckedChanged
        My.Settings.ShowWebPanel = Not chkDoNotShow.Checked
    End Sub

    Private Sub FormWebPanel_Load(sender As Object, e As EventArgs) Handles Me.Load
        chkDoNotShow.Checked = Not My.Settings.ShowWebPanel
    End Sub

End Class