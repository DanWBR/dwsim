Imports System.Security.Policy

Public Class FormWhatsNew
    Private Sub FormWhatsNew_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Viewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(Sub()
                                                                                      UIThread(Sub()
                                                                                                   Viewer.Source = New Uri("https://dwsim.org/index.php/whatsnew/")
                                                                                                   Me.Activate()
                                                                                               End Sub)
                                                                                  End Sub)
    End Sub

End Class