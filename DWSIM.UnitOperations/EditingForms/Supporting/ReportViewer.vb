Public Class ReportViewer
    Private Sub ReportViewer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub ReportViewer_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        TextBox1.DeselectAll()
    End Sub
End Class