Imports HtmlAgilityPack
Imports System.Net.Http
Imports System.Net
Imports System.Text
Imports System.Collections.Specialized
Imports System.IO
Imports System.Web
Imports Globalization = System.Globalization

Public Class ChemeoParser

    Shared Function GetCompoundIDs(searchstring As String, exact As Boolean) As List(Of String())

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim website As String = "https://www.chemeo.com/search?q=" + HttpUtility.UrlEncode(searchstring)

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

        Dim exactmatch = htmlpage.DocumentNode.Descendants("p").Where(Function(x) x.InnerText.Contains("Found one exact match")).SingleOrDefault

        Dim results As New List(Of String())

        If Not exactmatch Is Nothing Then
            Dim link As String = exactmatch.Descendants("a").SingleOrDefault.Attributes("href").Value
            Dim splittedstr = link.Split("/".ToCharArray, StringSplitOptions.RemoveEmptyEntries)
            Dim id As String = splittedstr(1)
            Dim name As String = HttpUtility.UrlDecode(splittedstr(2))
            results.Add(New String() {id, name})
        End If

        Dim rows = htmlpage.DocumentNode.Descendants("table")(1).Descendants("tr").ToList

        For Each r In rows
            If Not r.InnerHtml.Contains("CAS") And Not r.InnerHtml.Contains("Next Results") Then
                Try
                    Dim id As String = ""
                    Dim name As String = ""
                    id = r.Descendants("a")(0).InnerText
                    name = r.Descendants("a")(2).InnerText
                    results.Add(New String() {id, name})
                Catch ex As Exception
                End Try
            End If
        Next

        If results.Count > 0 Then
            If exact Then
                Dim match = results.Where(Function(x) x(1).ToLower.Equals(searchstring.ToLower)).ToList
                If match.Count > 0 Then
                    Return match
                Else
                    Dim list As New List(Of String())
                    list.Add(results(0))
                    Return list
                End If
            Else
                Return results
            End If
        Else
            Throw New Exception("No matching compounds found.")
        End If

    End Function

    Shared Function GetCompoundData(ID As String) As BaseClasses.ConstantProperties

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim website As String = "https://www.chemeo.com/cid/" + ID

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

        Dim comp As New BaseClasses.ConstantProperties

        comp.Comments = website

        comp.OriginalDB = "Cheméo"

        comp.CurrentDB = "Cheméo"

        comp.ID = New Random().Next(600001, 699999)

        Dim group1 = htmlpage.DocumentNode.Descendants("dl").FirstOrDefault

        Dim element = group1.Descendants("dd")(0)

        comp.InChI = element.InnerText.Split("=")(1)

        element = group1.Descendants("dd")(5)

        comp.CAS_Number = element.InnerText

        element = group1.Descendants("dd")(2)

        comp.Formula = element.InnerText

        element = group1.Descendants("dd")(4)

        comp.Molar_Weight = Double.Parse(element.InnerText, ci)

        element = group1.Descendants("dd")(3)

        comp.SMILES = element.InnerText

        Dim tablerows = htmlpage.DocumentNode.Descendants("table").FirstOrDefault.Descendants("tr")

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Normal Boiling Point Temperature")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.Normal_Boiling_Point = Double.Parse(text.Split("; ")(0), ci)
                comp.NBP = comp.Normal_Boiling_Point
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.Normal_Boiling_Point = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
                comp.NBP = comp.Normal_Boiling_Point
            Else
                comp.Normal_Boiling_Point = Double.Parse(element.Descendants("td")(1).InnerText, ci)
                comp.NBP = comp.Normal_Boiling_Point
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Normal melting (fusion) point")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.TemperatureOfFusion = Double.Parse(text.Split("; ")(0), ci)
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.TemperatureOfFusion = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
            Else
                comp.TemperatureOfFusion = Double.Parse(element.Descendants("td")(1).InnerText, ci)
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Temperature")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.Critical_Temperature = Double.Parse(text.Split("; ")(0), ci)
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.Critical_Temperature = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
            Else
                comp.Critical_Temperature = Double.Parse(element.Descendants("td")(1).InnerText, ci)
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Pressure")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.Critical_Pressure = Double.Parse(text.Split("; ")(0), ci) * 1000
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.Critical_Pressure = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000
            Else
                comp.Critical_Pressure = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Volume")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.Critical_Volume = Double.Parse(text.Split("; ")(0), ci)
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.Critical_Volume = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
            Else
                comp.Critical_Volume = Double.Parse(element.Descendants("td")(1).InnerText, ci)
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Enthalpy of formation at standard conditions")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.IG_Enthalpy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) * 1000 / comp.Molar_Weight
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.IG_Enthalpy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000 / comp.Molar_Weight
            Else
                comp.IG_Enthalpy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000 / comp.Molar_Weight
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Standard Gibbs free energy of formation")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) * 1000 / comp.Molar_Weight
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000 / comp.Molar_Weight
            Else
                comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000 / comp.Molar_Weight
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Molar entropy at standard conditions")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.IG_Entropy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) / comp.Molar_Weight
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.IG_Entropy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) / comp.Molar_Weight
            Else
                comp.IG_Entropy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) / comp.Molar_Weight
            End If
        End If

        element = tablerows.Where(Function(x) x.InnerHtml.Contains("Enthalpy of fusion at standard conditions")).FirstOrDefault

        If Not element Is Nothing Then
            If element.Descendants("td")(1).InnerText.Contains("[") Then
                Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
                comp.EnthalpyOfFusionAtTf = Double.Parse(text.Split("; ")(0), ci)
            ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
                comp.EnthalpyOfFusionAtTf = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
            Else
                comp.EnthalpyOfFusionAtTf = Double.Parse(element.Descendants("td")(1).InnerText, ci)
            End If
        End If

        Return comp

    End Function

End Class
