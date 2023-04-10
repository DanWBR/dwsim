Imports System.Security.Policy

Public Class FormWhatsNew
    Private Sub FormWhatsNew_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim newUserFolder = System.IO.Path.Combine(System.IO.Path.GetTempPath(), "DWSIM", "BrowserData")
        Dim environment = Microsoft.Web.WebView2.Core.CoreWebView2Environment.CreateAsync(Nothing, newUserFolder, Nothing).Result

        Viewer.EnsureCoreWebView2Async(environment).ContinueWith(Sub()
                                                                     UIThread(Sub()
                                                                                  Viewer.Source = New Uri("https://dwsim.org/index.php/whatsnew/")
                                                                                  Me.Activate()
                                                                              End Sub)
                                                                 End Sub)
    End Sub

End Class