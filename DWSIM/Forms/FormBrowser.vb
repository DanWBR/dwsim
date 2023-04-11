Public Class FormBrowser

    Private Sub FormBrowser_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub DisplayURL(url As String, Optional title As String = "")

        If title <> "" Then Text = title

        Viewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(Sub()
                                                                                      UIThread(Sub()
                                                                                                   Viewer.Source = New Uri(url)
                                                                                                   Me.Activate()
                                                                                               End Sub)
                                                                                  End Sub)

    End Sub

    Public Sub DisplayHTML(html As String, Optional title As String = "")

        If title <> "" Then Text = title

        Viewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(Sub()
                                                                                      UIThread(Sub()
                                                                                                   Viewer.NavigateToString(html)
                                                                                                   Me.Activate()
                                                                                               End Sub)
                                                                                  End Sub)

    End Sub

End Class