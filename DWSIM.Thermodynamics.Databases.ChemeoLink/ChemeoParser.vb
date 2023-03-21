Imports HtmlAgilityPack
Imports System.Net.Http
Imports System.Net
Imports System.Text
Imports System.Web
Imports System.Threading.Tasks
Imports System.Web.Script.Serialization
Imports System.Globalization

Public Class ChemeoParser

    Shared Async Function GetCompoundIDs(searchstring As String, exact As Boolean) As Task(Of List(Of String()))

        Dim url As String = "https://www.chemeo.com/api/v1/search?q=" & HttpUtility.UrlEncode(searchstring)

        Dim response As HttpResponseMessage = Await GetResponse(url)
        If response.IsSuccessStatusCode Then
            Dim jss As New JavaScriptSerializer()
            jss.MaxJsonLength = 10 * 1024 * 1024
            Dim json As String = Await response.Content.ReadAsStringAsync()
            Dim result = jss.Deserialize(Of SearchResponse)(json)
            Dim resultFilter = If(exact,
                Function(x As Compound) String.Equals(x.Compound, searchstring, StringComparison.OrdinalIgnoreCase),
                Function(x As Compound) True)
            Return result.Comps.Where(resultFilter).Select(Function(x) New String() {x.Id, x.Compound}).ToList()
        Else
            'do we want to report a failure?
            Return New List(Of String())
        End If

    End Function

    Shared Function GetCompoundData2(cid As String) As BaseClasses.ConstantProperties

        Dim url As String = "https://www.chemeo.com/api/v1/cid/" & HttpUtility.UrlEncode(cid)

        Dim cdata As CompoundData

        Dim response As HttpResponseMessage = GetResponse(url).GetAwaiter().GetResult()
        If response.IsSuccessStatusCode Then
            Dim jss As New JavaScriptSerializer()
            jss.MaxJsonLength = 10 * 1024 * 1024
            Dim json As String = response.Content.ReadAsStringAsync().GetAwaiter().GetResult()
            cdata = jss.Deserialize(Of CompoundData)(json)
        Else
            Throw New Exception("unable to parse output")
        End If

        Dim comp As New BaseClasses.ConstantProperties

        Dim data = cdata.comp

        comp.Comments = url

        comp.OriginalDB = "Cheméo"

        comp.CurrentDB = "Cheméo"

        comp.ID = New Random().Next(600001, 699999)

        comp.InChI = data.inchi.Replace("InChI=", "")

        comp.CAS_Number = data.cas

        comp.Formula = data.formula

        comp.Molar_Weight = data.mw

        comp.SMILES = data.smiles

        If data.fixed_props.tb IsNot Nothing Then
            If data.fixed_props.tb.Count > 0 Then
                comp.Normal_Boiling_Point = data.fixed_props.tb(0).v
                comp.NBP = comp.Normal_Boiling_Point
            End If
        End If

        If data.fixed_props.tf IsNot Nothing Then
            If data.fixed_props.tf.Count > 0 Then
                comp.TemperatureOfFusion = data.fixed_props.tf(0).v
            End If
        End If

        If data.fixed_props.tc IsNot Nothing Then
            If data.fixed_props.tc.Count > 0 Then
                comp.Critical_Temperature = data.fixed_props.tc(0).v
            End If
        End If

        If data.fixed_props.pc IsNot Nothing Then
            If data.fixed_props.pc.Count > 0 Then
                comp.Critical_Pressure = data.fixed_props.pc(0).v * 1000
            End If
        End If

        If data.fixed_props.vc IsNot Nothing Then
            If data.fixed_props.vc.Count > 0 Then
                comp.Critical_Volume = data.fixed_props.vc(0).v
            End If
        End If

        If data.fixed_props.af IsNot Nothing Then
            If data.fixed_props.af.Count > 0 Then
                comp.Acentric_Factor = data.fixed_props.af(0).v
            End If
        End If

        If data.fixed_props.zc IsNot Nothing Then
            If data.fixed_props.zc.Count > 0 Then
                comp.Critical_Compressibility = data.fixed_props.zc(0).v
            End If
        End If

        If data.fixed_props.zra IsNot Nothing Then
            If data.fixed_props.zra.Count > 0 Then
                comp.Z_Rackett = data.fixed_props.zra(0).v
            End If
        End If

        If data.fixed_props.hf IsNot Nothing Then
            If data.fixed_props.hf.Count > 0 Then
                comp.IG_Enthalpy_of_Formation_25C = data.fixed_props.hf(0).v * 1000 / comp.Molar_Weight
            End If
        End If

        If data.fixed_props.gf IsNot Nothing Then
            If data.fixed_props.gf.Count > 0 Then
                comp.IG_Gibbs_Energy_of_Formation_25C = data.fixed_props.gf(0).v * 1000 / comp.Molar_Weight
            End If
        End If

        If data.fixed_props.hf IsNot Nothing And data.fixed_props.gf IsNot Nothing Then
            If data.fixed_props.hf.Count > 0 And data.fixed_props.gf.Count > 0 Then
                comp.IG_Entropy_of_Formation_25C = (comp.IG_Enthalpy_of_Formation_25C - comp.IG_Gibbs_Energy_of_Formation_25C) / 298.15
            End If
        End If

        If data.fixed_props.hfus IsNot Nothing Then
            If data.fixed_props.hfus.Count > 0 Then
                comp.EnthalpyOfFusionAtTf = data.fixed_props.hfus(0).v
            End If
        End If

        Return comp

    End Function


    Private Shared Async Function GetResponse(url As String) As Task(Of HttpResponseMessage)

        Dim siteUri As Uri = New Uri(url)
        Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

        Dim handler As New HttpClientHandler()

        handler.AllowAutoRedirect = True
        handler.AutomaticDecompression = DecompressionMethods.GZip
        handler.ClientCertificateOptions = ClientCertificateOption.Automatic

        If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
            Dim proxyObj As New WebProxy(proxyUri) With {
                .Credentials = CredentialCache.DefaultCredentials
            }
            handler.Proxy = proxyObj
        End If

        Dim http As New HttpClient(handler)

        http.DefaultRequestHeaders.Add("Authorization", "Bearer ef3e3742_798e_4285_8657_6254f7922167")

        Dim response = Await http.GetAsync(url)
        Return response

    End Function

    Shared Function GetCompoundData(ID As String) As BaseClasses.ConstantProperties

        Return GetCompoundData2(ID)

        'Dim ci As CultureInfo = New CultureInfo("en-US")

        'Dim website As String = "https://www.chemeo.com/cid/" + HttpUtility.UrlEncode(ID)

        'Dim siteUri As Uri = New Uri(website)
        'Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

        'Dim handler As New HttpClientHandler()

        'handler.AllowAutoRedirect = True
        'handler.AutomaticDecompression = DecompressionMethods.GZip
        'handler.ClientCertificateOptions = ClientCertificateOption.Automatic

        'If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
        '    Dim proxyObj As New WebProxy(proxyUri)
        '    proxyObj.Credentials = CredentialCache.DefaultCredentials
        '    handler.Proxy = proxyObj
        'End If

        'Dim http As New HttpClient(handler)

        'http.DefaultRequestHeaders.Add("Authorization", "Bearer ef3e3742_798e_4285_8657_6254f7922167")

        'Dim response = http.GetByteArrayAsync(website)
        'response.Wait()

        'Dim source As [String] = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
        'source = WebUtility.HtmlDecode(source)

        'Dim htmlpage As New HtmlDocument()

        'htmlpage.LoadHtml(source)

        'Dim comp As New BaseClasses.ConstantProperties

        'comp.Comments = website

        'comp.OriginalDB = "Cheméo"

        'comp.CurrentDB = "Cheméo"

        'comp.ID = New Random().Next(600001, 699999)

        'Dim group1 = htmlpage.DocumentNode.Descendants("dl").FirstOrDefault

        'Dim element = group1.Descendants("dd")(0)

        'comp.InChI = element.InnerText.Split("=")(1)

        'element = group1.Descendants("dd")(5)

        'comp.CAS_Number = element.InnerText

        'element = group1.Descendants("dd")(2)

        'comp.Formula = element.InnerText

        'element = group1.Descendants("dd")(4)

        'comp.Molar_Weight = Double.Parse(element.InnerText, ci)

        'element = group1.Descendants("dd")(3)

        'comp.SMILES = element.InnerText

        'Dim tablerows = htmlpage.DocumentNode.Descendants("table").FirstOrDefault.Descendants("tr")

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Normal Boiling Point Temperature")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.Normal_Boiling_Point = Double.Parse(text.Split("; ")(0), ci)
        '        comp.NBP = comp.Normal_Boiling_Point
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.Normal_Boiling_Point = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
        '        comp.NBP = comp.Normal_Boiling_Point
        '    Else
        '        comp.Normal_Boiling_Point = Double.Parse(element.Descendants("td")(1).InnerText, ci)
        '        comp.NBP = comp.Normal_Boiling_Point
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Normal melting (fusion) point")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.TemperatureOfFusion = Double.Parse(text.Split("; ")(0), ci)
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.TemperatureOfFusion = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
        '    Else
        '        comp.TemperatureOfFusion = Double.Parse(element.Descendants("td")(1).InnerText, ci)
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Temperature")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.Critical_Temperature = Double.Parse(text.Split("; ")(0), ci)
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.Critical_Temperature = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
        '    Else
        '        comp.Critical_Temperature = Double.Parse(element.Descendants("td")(1).InnerText, ci)
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Pressure")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.Critical_Pressure = Double.Parse(text.Split("; ")(0), ci) * 1000
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.Critical_Pressure = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000
        '    Else
        '        comp.Critical_Pressure = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Critical Volume")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.Critical_Volume = Double.Parse(text.Split("; ")(0), ci)
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.Critical_Volume = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
        '    Else
        '        comp.Critical_Volume = Double.Parse(element.Descendants("td")(1).InnerText, ci)
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Enthalpy of formation at standard conditions")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.IG_Enthalpy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) * 1000 / comp.Molar_Weight
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.IG_Enthalpy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000 / comp.Molar_Weight
        '    Else
        '        comp.IG_Enthalpy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000 / comp.Molar_Weight
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Standard Gibbs free energy of formation")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) * 1000 / comp.Molar_Weight
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) * 1000 / comp.Molar_Weight
        '    Else
        '        comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) * 1000 / comp.Molar_Weight
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Molar entropy at standard conditions")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.IG_Entropy_of_Formation_25C = Double.Parse(text.Split("; ")(0), ci) / comp.Molar_Weight
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.IG_Entropy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci) / comp.Molar_Weight
        '    Else
        '        comp.IG_Entropy_of_Formation_25C = Double.Parse(element.Descendants("td")(1).InnerText, ci) / comp.Molar_Weight
        '    End If
        'End If

        'element = tablerows.Where(Function(x) x.InnerHtml.Contains("Enthalpy of fusion at standard conditions")).FirstOrDefault

        'If Not element Is Nothing Then
        '    If element.Descendants("td")(1).InnerText.Contains("[") Then
        '        Dim text = element.Descendants("td")(1).InnerText.Trim(New Char() {"[", "]"})
        '        comp.EnthalpyOfFusionAtTf = Double.Parse(text.Split("; ")(0), ci)
        '    ElseIf element.Descendants("td")(1).InnerText.Contains("±") Then
        '        comp.EnthalpyOfFusionAtTf = Double.Parse(element.Descendants("td")(1).InnerText.Split(" ± ")(0), ci)
        '    Else
        '        comp.EnthalpyOfFusionAtTf = Double.Parse(element.Descendants("td")(1).InnerText, ci)
        '    End If
        'End If

        'Return comp

    End Function

End Class
