Public Class FormShareAnonymousUsageData
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Process.Start("https://dwsim.inforside.com.br/new/index.php/privacy-policy-2/")

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        My.Settings.SendCrashAndUsageAnalytics = True
        My.Settings.UserEmail = TextBox1.Text
        Me.Close()

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        My.Settings.SendCrashAndUsageAnalytics = False
        Me.Close()

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        Process.Start("https://dwsim.inforside.com.br/new/index.php/sample-collected-data")

    End Sub
End Class