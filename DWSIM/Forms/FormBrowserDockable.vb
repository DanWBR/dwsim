Public Class FormBrowserDockable

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private Sub FormBrowserDockable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub DisplayURL(url As String)

        UIThread(Sub()
                     Viewer.Source = New Uri(url)
                     Me.Activate()
                 End Sub)

    End Sub

End Class