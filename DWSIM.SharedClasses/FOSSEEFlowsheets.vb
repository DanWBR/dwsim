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

        Dim list As New List(Of FOSSEEFlowsheet)
        For Each r In rows

            Dim fs As New FOSSEEFlowsheet

            fs.Address = "http://dwsim.fossee.in" & r.ChildNodes(1).ChildNodes(0).Attributes(0).Value

            response = http.GetByteArrayAsync(fs.Address)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            htmlpage.LoadHtml(source)

            Dim details = htmlpage.DocumentNode.Descendants("div").Where(Function(x) x.Attributes.Contains("id") AndAlso x.Attributes("id").Value = "ajax_flowsheet_details").FirstOrDefault.ChildNodes.Descendants("li").ToList

            With fs
                .DownloadLink = htmlpage.DocumentNode.Descendants("a").Where(Function(x) x.InnerText = "Download Flowsheet").SingleOrDefault.Attributes("href").Value
                .DWSIMVersion = r.ChildNodes(1).InnerText
                .Institution = r.ChildNodes(1).InnerText
                .ProposerName = r.ChildNodes(1).InnerText
                .Reference = r.ChildNodes(1).InnerText
                .Title = r.ChildNodes(1).InnerText
            End With
            list.Add(fs)
        Next

        Return list

    End Function


End Class
