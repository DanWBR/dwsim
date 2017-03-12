Imports HtmlAgilityPack
Imports System.Net.Http
Imports System.Net
Imports System.Text
Imports System.Collections.Specialized
Imports System.IO

Public Class DDBStructureParser

    Shared Function GetID(CAS As String) As String

        Dim proxyObj As New WebProxy(Net.WebRequest.GetSystemWebProxy.GetProxy(New Uri("http://ddbonline.ddbst.com/OnlinePropertyEstimation/OnlineUNIFACGroupAssignmentSA.exe")))
        proxyObj.Credentials = CredentialCache.DefaultCredentials

        Dim handler As New HttpClientHandler()
        handler.Proxy = proxyObj

        Dim client = New HttpClient(handler)
        Dim content = New MultipartFormDataContent("----WebKitFormBoundary" + New Guid().ToString)
        content.Add(New StringContent("DDBSearch"), """selection""")
        content.Add(New StringContent(CAS), """casn""")
        content.Add(New StringContent("Search"), """search_cas""")

        Dim result = client.PostAsync("http://ddbonline.ddbst.com/OnlinePropertyEstimation/OnlineUNIFACGroupAssignmentSA.exe", content)
        result.Wait()
        Dim t1 = result.Result.Content.ReadAsByteArrayAsync()
        t1.Wait()
        Dim bytes = t1.Result
        Dim source As [String] = Encoding.GetEncoding("utf-8").GetString(bytes, 0, bytes.Length - 1)
        source = WebUtility.HtmlDecode(source)

        Dim htmlpage As New HtmlDocument()

        htmlpage.LoadHtml(source)

        'get ids

        Dim founditem = htmlpage.DocumentNode.Descendants("option").FirstOrDefault

        If Not founditem Is Nothing Then
            Return founditem.Attributes("value").Value
        Else
            Return Nothing
        End If

    End Function

    Shared Function GetData(ID As String) As Dictionary(Of String, List(Of String()))

        Dim proxyObj As New WebProxy(Net.WebRequest.GetSystemWebProxy.GetProxy(New Uri("http://ddbonline.ddbst.com/OnlinePropertyEstimation/OnlineUNIFACGroupAssignmentSA.exe")))
        proxyObj.Credentials = CredentialCache.DefaultCredentials

        Dim handler As New HttpClientHandler()
        handler.Proxy = proxyObj

        Dim client = New HttpClient(handler)
        Dim content = New MultipartFormDataContent("----WebKitFormBoundary" + New Guid().ToString)
        content.Add(New StringContent("DDBSearch"), """selection""")
        content.Add(New StringContent(ID), """complist""")
        content.Add(New StringContent("Assign Groups"), """Assign""")

        Dim result = client.PostAsync("http://ddbonline.ddbst.com/OnlinePropertyEstimation/OnlineUNIFACGroupAssignmentSA.exe", content)
        result.Wait()
        Dim t1 = result.Result.Content.ReadAsByteArrayAsync()
        t1.Wait()
        Dim bytes = t1.Result
        Dim source As [String] = Encoding.GetEncoding("utf-8").GetString(bytes, 0, bytes.Length - 1)
        source = WebUtility.HtmlDecode(source)

        Dim htmlpage As New HtmlDocument()

        htmlpage.LoadHtml(source)

        'get data

        Dim results As New Dictionary(Of String, List(Of String()))

        Dim origunifac = htmlpage.DocumentNode.Descendants("h3").Where(Function(x) x.InnerHtml.Contains("Original UNIFAC")).FirstOrDefault
        Dim status = origunifac.NextSibling.NextSibling

        If status.InnerText.Contains("Group assignment successful") Then

            Dim unifac As New Thermodynamics.PropertyPackages.Auxiliary.Unifac

            Dim list As New List(Of String())

            'get unifac structure info
            Dim rows = status.NextSibling.Descendants("tr").Where(Function(x) Not x.InnerHtml.Contains("Count"))
            For Each row In rows
                Dim count = row.Descendants("td")(0).InnerText
                Dim maingroup = row.Descendants("td")(2).InnerText
                Dim subgroup = unifac.Group2ID(row.Descendants("td")(3).InnerText)
                list.Add(New String() {maingroup, subgroup, count})
            Next

            results.Add("Original", list)

        End If

        Dim modifunifac = htmlpage.DocumentNode.Descendants("h3").Where(Function(x) x.InnerHtml.Contains("Modified UNIFAC (Dortmund)")).FirstOrDefault
        status = modifunifac.NextSibling.NextSibling

        If status.InnerText.Contains("Group assignment successful") Then

            Dim modfac As New Thermodynamics.PropertyPackages.Auxiliary.Modfac

            Dim list As New List(Of String())

            'get modfac structure info
            Dim rows = status.NextSibling.Descendants("tr").Where(Function(x) Not x.InnerHtml.Contains("Count"))
            For Each row In rows
                Dim count = row.Descendants("td")(0).InnerText
                Dim maingroup = row.Descendants("td")(2).InnerText
                Dim gname As String = row.Descendants("td")(3).InnerText.Replace(" ", "")
                If gname = "OH(p)" Then gname = "OH"
                Dim subgroup = modfac.Group2ID(gname)
                list.Add(New String() {maingroup, subgroup, count})
            Next

            results.Add("Modified", list)

        End If

        Return results

    End Function

End Class
