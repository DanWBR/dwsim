Public Class FormCosting

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private Sub FormCosting_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Process.Start("https://simulate365.com/registration/")
    End Sub

End Class