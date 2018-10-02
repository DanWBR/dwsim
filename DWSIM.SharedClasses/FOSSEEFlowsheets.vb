Imports System.Net
Imports System.Net.Http
Imports System.Text
Imports HtmlAgilityPack

Public Class FOSSEEFlowsheet

    Public Property Address As String = ""
    Public Property ProposerName As String = ""
    Public Property Title As String = ""
    Public Property Institution As String = ""
    Public Property DWSIMVersion As String = ""
    Public Property Reference As String = ""
    Public Property DownloadLink As String = ""

End Class

Public Class FOSSEEFlowsheets

    Public Shared Function GetFOSSEEFlowsheets() As List(Of FOSSEEFlowsheet)

        Dim website As String = "http://dwsim.fossee.in/flowsheeting-project/completed-flowsheet"

        Dim siteUri As Uri = New Uri(website)
        Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

        Dim handler As New HttpClientHandler()

        If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
            Dim proxyObj As New WebProxy(proxyUri)
            proxyObj.Credentials = CredentialCache.DefaultCredentials
            handler.Proxy = proxyObj
        End If

        Dim http As New HttpClient(handler)

        Dim response = http.GetByteArrayAsync(website)
        response.Wait()

        Dim source As [String] = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
        source = WebUtility.HtmlDecode(source)

        Dim htmlpage As New HtmlDocument()

        htmlpage.LoadHtml(source)

        Dim rows = htmlpage.DocumentNode.Descendants("tbody").FirstOrDefault.Descendants("tr").ToList


    End Function


End Class
