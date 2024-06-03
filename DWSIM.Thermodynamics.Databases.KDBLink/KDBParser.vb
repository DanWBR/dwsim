Imports HtmlAgilityPack
Imports System.Net.Http
Imports System.Net
Imports System.Text
Imports System.Web
Imports Globalization = System.Globalization

Public Class KDBParser

    Shared Function GetCompoundIDs(searchstring As String, exact As Boolean) As List(Of String())

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim website As String = "https://www.cheric.org/research/kdb/hcprop/listcmp.php?componentsearch=" + HttpUtility.UrlEncode(searchstring)

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

        Dim results As New List(Of String())

        If htmlpage.DocumentNode.InnerHtml.ToLower.Contains("no data found") Then
            Return results
        ElseIf Not htmlpage.DocumentNode.InnerHtml.ToLower.Contains("tbody") Then
            Return results
        End If

        Dim rows = htmlpage.DocumentNode.Descendants("tbody").FirstOrDefault.Descendants("tr").ToList

        For Each r In rows
            Dim id As String = ""
            Dim name As String = ""
            Dim formula As String = ""
            id = r.ChildNodes(1).InnerText
            name = ci.TextInfo.ToTitleCase(r.ChildNodes(3).InnerText.ToLower)
            formula = r.ChildNodes(5).InnerText
            results.Add(New String() {id, name, formula})
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
            Return results
        End If

    End Function

    Shared Function GetCompoundData(cid As Integer) As BaseClasses.ConstantProperties

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim website As String = "http://www.cheric.org/research/kdb/hcprop/showprop.php?cmpid=" + cid.ToString

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

        comp.OriginalDB = "KDB"

        comp.CurrentDB = "KDB"

        comp.ID = 500000 + cid

        Dim element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Name")).FirstOrDefault.ChildNodes(3)

        comp.Name = ci.TextInfo.ToTitleCase(element.InnerText.ToLower)

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("CAS No.")).FirstOrDefault.ChildNodes(3)

        comp.CAS_Number = element.InnerText

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Formula")).FirstOrDefault.ChildNodes(3)

        comp.Formula = element.InnerText

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Molecular Wt. (WT)")).FirstOrDefault.ChildNodes(3)

        comp.Molar_Weight = Double.Parse(element.InnerText, ci)

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Normal Boiling Point Temp. (TB)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Normal_Boiling_Point = Double.Parse(element.InnerText.Split(" ")(0), ci)
            comp.NBP = Double.Parse(element.InnerText.Split(" ")(0), ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Freezing Point Temp. (TF)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.TemperatureOfFusion = Double.Parse(element.InnerText.Split(" ")(0), ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Critical Temperature. (TC)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Critical_Temperature = Double.Parse(element.InnerText.Split(" ")(0), ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Critical Pressure (PC)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Critical_Pressure = Double.Parse(element.InnerText.Split(" ")(0), ci) * 1000
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Critical Volume (VC)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Critical_Volume = Double.Parse(element.InnerText.Split(" ")(0), ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Critical Compressibility (ZC)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Critical_Compressibility = Double.Parse(element.InnerText, ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Accentric Factor (ACCF)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Acentric_Factor = Double.Parse(element.InnerText, ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Rackett parameter (ZRA)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Z_Rackett = Double.Parse(element.InnerText, ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("H(formation,ideal gas)at 25 C")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.IG_Enthalpy_of_Formation_25C = Double.Parse(element.InnerText.Split(" ")(0), ci) / comp.Molar_Weight
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("G(formation,ideal gas) at 25 C")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.IG_Gibbs_Energy_of_Formation_25C = Double.Parse(element.InnerText.Split(" ")(0), ci) / comp.Molar_Weight
        End If

        If Not element.InnerText.Contains("NA") Then
            comp.IG_Entropy_of_Formation_25C = (comp.IG_Enthalpy_of_Formation_25C - comp.IG_Gibbs_Energy_of_Formation_25C) / 298.15
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("UNIQUAC Ri Parameter (RI)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.UNIQUAC_R = Double.Parse(element.InnerText, ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("UNIQUAC Qi Parameter (QI)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.UNIQUAC_Q = Double.Parse(element.InnerText, ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Dipole Moment (DM)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Dipole_Moment = Double.Parse(element.InnerText.Split(" ")(0), ci)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Solubility Parameters (SOLP)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            comp.Chao_Seader_Solubility_Parameter = Double.Parse(element.InnerText.Split(" ")(0), ci) * (1 / 4.16) ^ 0.5
        End If

        'get vapor pressure coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Vapor Pressure")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=PVP&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.VaporPressureEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_Constant_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_Constant_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_Constant_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_Constant_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_Constant_E)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, from")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_TMIN)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, to")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Vapor_Pressure_TMAX)

        End If

        'get ideal cp coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Heat Capacity (Ideal Gas)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=CPG&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.IdealgasCpEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Ideal_Gas_Heat_Capacity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Ideal_Gas_Heat_Capacity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Ideal_Gas_Heat_Capacity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Ideal_Gas_Heat_Capacity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Ideal_Gas_Heat_Capacity_Const_E)

        End If

        'get liquid cp coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Heat Capacity (Liquid)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=CPL&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.LiquidHeatCapacityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Const_E)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, from")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Tmin)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, to")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Liquid_Heat_Capacity_Tmax)

        End If

        'get vapor visc coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Viscosity (Gas, Low P)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=VSG&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.VaporViscosityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Const_E)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, from")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Tmin)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("T range, to")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Vapor_Viscosity_Tmax)

        End If

        'get liquid visc coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Viscosity (Liquid)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=VSL&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.LiquidViscosityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Viscosity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Viscosity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Viscosity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Viscosity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Viscosity_Const_E)

        End If

        'get vapor thermcond coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Themal Conductivity (Gas, Low P)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=THG&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.VaporThermalConductivityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Thermal_Conductivity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Thermal_Conductivity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Thermal_Conductivity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Thermal_Conductivity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Vapor_Thermal_Conductivity_Const_E)

        End If

        'get liquid thermcond coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Themal Conductivity (Gas, Low P)")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=THL&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.LiquidThermalConductivityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_E)

        End If

        'get liquid surface tension coefficients

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Surface Tension")).FirstOrDefault.ChildNodes(3)

        If element.InnerText.Contains("Coeff.s Available") Then

            website = "http://www.cheric.org/research/kdb/hcprop/showcoef.php?prop=THL&cmpid=" + cid.ToString

            response = http.GetByteArrayAsync(website)
            response.Wait()

            source = Encoding.GetEncoding("utf-8").GetString(response.Result, 0, response.Result.Length - 1)
            source = WebUtility.HtmlDecode(source)

            Dim htmlpage2 As New HtmlDocument()
            htmlpage2.LoadHtml(source)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Equation")).ElementAt(1).ChildNodes(3)

            comp.LiquidThermalConductivityEquation = element.InnerText

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient A")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_A)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient B")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_B)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient C")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_C)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient D")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_D)

            element = htmlpage2.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Coefficient E")).FirstOrDefault.ChildNodes(3)

            Double.TryParse(element.InnerText, Globalization.NumberStyles.Any, ci, comp.Liquid_Thermal_Conductivity_Const_E)

        Else

            element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Surface Tension (SRF)")).FirstOrDefault.ChildNodes(3)

            If Not element.InnerText.Contains("NA") Then
                Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Surface_Tension_Const_A)
            End If

            element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Temperature of SRF (TSRF)")).FirstOrDefault.ChildNodes(3)

            If Not element.InnerText.Contains("NA") Then
                Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Surface_Tension_Const_B)
            End If

        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Liquid Density (DENL)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Liquid_Density_Const_A)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Temperature of DENL (TDENL)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.Liquid_Density_Const_B)
        End If

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Heat of Vaporizaiton (HVAP)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.HVap_A)
        End If

        comp.HVap_A /= comp.Molar_Weight

        element = htmlpage.DocumentNode.Descendants("tr").Where(Function(x) x.InnerText.Contains("Temperature of HVAP (THVAP)")).FirstOrDefault.ChildNodes(3)

        If Not element.InnerText.Contains("NA") Then
            Double.TryParse(element.InnerText.Split(" ")(0), Globalization.NumberStyles.Any, ci, comp.HVap_B)
        End If

        Return comp

    End Function

    Shared Function GetVLEData(vleid As Integer) As KDBVLEDataSet

        Dim website = "http://www.cheric.org/research/kdb/hcvle/showvle.php?vleid=" + vleid.ToString

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

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

        Dim result As New KDBVLEDataSet

        Dim description = htmlpage.DocumentNode.Descendants("h5").FirstOrDefault.Descendants("strong").ToList

        result.Description = description(0).InnerText

        Dim method = htmlpage.DocumentNode.Descendants("h5").FirstOrDefault.Descendants("strong").ToList

        Dim head = htmlpage.DocumentNode.Descendants("thead").FirstOrDefault.Descendants("th").ToList

        result.Tunits = head(0).InnerText.Split(", ")(1).Replace("deg.C", "C").Trim
        result.Punits = head(1).InnerText.Split(", ")(1).Trim

        Dim rows = htmlpage.DocumentNode.Descendants("tbody").FirstOrDefault.Descendants("tr").ToList

        For Each r In rows
            Dim T, P, X, Y As Double
            Double.TryParse(r.ChildNodes(1).InnerText, Globalization.NumberStyles.Any, ci, T)
            Double.TryParse(r.ChildNodes(3).InnerText, Globalization.NumberStyles.Any, ci, P)
            Double.TryParse(r.ChildNodes(5).InnerText, Globalization.NumberStyles.Any, ci, X)
            Double.TryParse(r.ChildNodes(7).InnerText, Globalization.NumberStyles.Any, ci, Y)
            result.Data.Add(New KDBVLEDataPoint With {.T = T, .P = P, .X = X, .Y = Y})
        Next

        Return result

    End Function

    Shared Function GetBinaryVLESetIDs(cid1 As Integer, cid2 As Integer) As List(Of String())

        Dim ci As System.Globalization.CultureInfo = New Globalization.CultureInfo("en-US")

        Dim website As String = "http://www.cheric.org/research/kdb/hcvle/listvle.php?cno1=" + cid1.ToString + "&cno2=" + cid2.ToString

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

        Dim results As New List(Of String())

        Try
            Dim rows = htmlpage.DocumentNode.Descendants("tbody").FirstOrDefault.Descendants("tr").ToList
            For Each r In rows
                Dim id As String = ""
                Dim description As String = ""
                id = r.ChildNodes(1).InnerText
                description = r.ChildNodes(3).InnerText
                results.Add(New String() {id, description})
            Next
        Catch ex As Exception
        End Try

        Return results

    End Function

End Class

Public Class KDBVLEDataSet

    Public Data As New List(Of KDBVLEDataPoint)

    Public Tunits As String = ""
    Public Punits As String = ""

    Public Description As String = ""

End Class

Public Class KDBVLEDataPoint

    Public X, Y, T, P As Double

End Class