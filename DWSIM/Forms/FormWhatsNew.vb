Public Class FormWhatsNew

    Private Sub FormWhatsNew_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        My.Settings.ShowWhatsNew = False
    End Sub

    Private Sub FormWhatsNew_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        Me.WebBrowser1.Url = New Uri("http://dwsim.inforside.com.br/wiki/index.php?title=What%27s_New&printable=yes")
    End Sub
End Class