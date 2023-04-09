Imports System.Security.Policy

Public Class FormBrowserDockable

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private Sub FormBrowserDockable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub DisplayURL(url As String, Optional title As String = "")

        If title <> "" Then
            Text = title
            TabText = title
        End If

        Viewer.EnsureCoreWebView2Async().ContinueWith(Sub()
                                                          UIThread(Sub()
                                                                       Viewer.Source = New Uri(url)
                                                                   End Sub)
                                                      End Sub)

    End Sub

    Public Sub DisplayHTML(html As String, Optional title As String = "")

        If title <> "" Then
            Text = title
            TabText = title
        End If

        Viewer.EnsureCoreWebView2Async().ContinueWith(Sub()
                                                          UIThread(Sub()
                                                                       Viewer.NavigateToString(html)
                                                                   End Sub)
                                                      End Sub)

    End Sub

End Class