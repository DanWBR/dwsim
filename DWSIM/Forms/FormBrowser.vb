Imports Eto.Forms
Imports Microsoft.Web.WebView2.Core

Public Class FormBrowser

    Private Sub FormBrowser_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub DisplayURL(url As String, Optional title As String = "")

        If title <> "" Then Text = title


        Viewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(
            Sub()
                UIThread(Sub()
                             Viewer.CoreWebView2.Settings.HiddenPdfToolbarItems = CoreWebView2PdfToolbarItems.Save +
                                                                                  CoreWebView2PdfToolbarItems.SaveAs +
                                                                                  CoreWebView2PdfToolbarItems.Print
                             Viewer.Source = New Uri(url)
                             Me.Activate()
                         End Sub)
            End Sub)

    End Sub

    Public Sub DisplayHTML(html As String, Optional title As String = "")

        If title <> "" Then Text = title

        Viewer.CoreWebView2.Settings.HiddenPdfToolbarItems = CoreWebView2PdfToolbarItems.Save And CoreWebView2PdfToolbarItems.SaveAs

        Viewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(
            Sub()
                UIThread(Sub()
                             Viewer.CoreWebView2.Settings.HiddenPdfToolbarItems = CoreWebView2PdfToolbarItems.Save +
                                                                                  CoreWebView2PdfToolbarItems.SaveAs +
                                                                                  CoreWebView2PdfToolbarItems.Print
                             Viewer.NavigateToString(html)
                             Me.Activate()
                         End Sub)
            End Sub)

    End Sub

End Class