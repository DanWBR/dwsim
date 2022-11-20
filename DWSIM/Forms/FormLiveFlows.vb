Public Class FormLiveFlows
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Close()
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Process.Start("https://simulate365.com/registration-dwsim-pro/")
    End Sub
End Class