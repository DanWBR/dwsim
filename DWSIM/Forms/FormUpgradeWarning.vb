Public Class FormUpgradeWarning

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Me.Close()
    End Sub

    Private Sub chkDoNotShowAgain_CheckedChanged(sender As Object, e As EventArgs) Handles chkDoNotShowAgain.CheckedChanged
        My.Settings.SimulationUpgradeWarning = chkDoNotShowAgain.Checked
    End Sub

End Class