Imports System.IO
Imports System.Net
Imports System.Net.Http
Imports System.Text
Imports System.Threading
Imports HtmlAgilityPack
Imports ICSharpCode.SharpZipLib.Zip

Public Class FOSSEEFlowsheet

    Public Property Address As String = ""
    Public Property ProposerName As String = ""
    Public Property Title As String = ""
    Public Property Institution As String = ""
    Public Property DWSIMVersion As String = ""
    Public Property Reference As String = ""
    Public Property DownloadLink As String = ""
    Public Property DisplayName As String = ""

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
        Dim i As Integer = 1
        For Each r In rows

            Dim fs As New FOSSEEFlowsheet

            fs.Address = "http://dwsim.fossee.in" & r.ChildNodes(1).ChildNodes(0).Attributes(0).Value

            With fs
                .Institution = r.ChildNodes(3).ChildNodes(0).InnerText
                .ProposerName = r.ChildNodes(2).ChildNodes(0).InnerText
                .Title = r.ChildNodes(1).ChildNodes(0).InnerText
                .DownloadLink = fs.Address.Replace("dwsim-flowsheet-run", "full-download/project")
                .DisplayName = i.ToString("000") + ". " + .Title
            End With
            list.Add(fs)
            i += 1
        Next

        Return list.Where(Function(x) Not x.ProposerName.Contains("Daniel Medeiros") And Not x.ProposerName.Contains("Daniel Wagner")).ToList

    End Function

    Public Shared Function GetFOSSEEFlowsheetInfo(address As String) As FOSSEEFlowsheet

        Dim website As String = "http://dwsim.fossee.in/flowsheeting-project/completed-flowsheet"

        Dim siteUri As Uri = New Uri(website)
        Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

        Dim fs As New FOSSEEFlowsheet

        fs.Address = address

        Dim handler2 As New HttpClientHandler()

        If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
            Dim proxyObj2 As New WebProxy(proxyUri)
            proxyObj2.Credentials = CredentialCache.DefaultCredentials
            handler2.Proxy = proxyObj2
        End If

        Dim http2 As New HttpClient(handler2)

        Dim response2 = http2.GetByteArrayAsync(fs.Address)
        response2.Wait()

        Dim source2 As [String] = Encoding.GetEncoding("utf-8").GetString(response2.Result, 0, response2.Result.Length - 1)
        source2 = WebUtility.HtmlDecode(source2)

        Dim htmlpage2 As New HtmlDocument()

        htmlpage2.LoadHtml(source2)

        Dim details = htmlpage2.DocumentNode.Descendants("div").Where(Function(x) x.Attributes.Contains("id") AndAlso x.Attributes("id").Value = "ajax_flowsheet_details").FirstOrDefault.ChildNodes.Descendants("li").ToList

        With fs
            .DownloadLink = "http://dwsim.fossee.in" & htmlpage2.DocumentNode.Descendants("a").Where(Function(x) x.InnerText = "Download Flowsheet").SingleOrDefault.Attributes("href").Value
            .DWSIMVersion = details(3).InnerText.Split(":")(1).Trim()
            .Institution = details(2).InnerText.Split(":")(1).Trim()
            .ProposerName = details(0).InnerText.Split(":")(1).Trim()
            .Reference = details(4).InnerText.Remove(0, 11)
            .Title = details(1).InnerText.Split(":")(1).Trim()
        End With

        Return fs

    End Function

    Public Shared Function LoadFlowsheet(fpath As String) As XDocument

        Dim fpath2 = Path.Combine(Path.GetTempPath(), "FOSSEE_DWSIM_TEMP")
        Directory.CreateDirectory(fpath2)
        Dim simname As String = ""

        Dim pdffiledir = GlobalSettings.Settings.GetConfigFileDir() + "FOSSEE"
        If Not Directory.Exists(pdffiledir) Then Directory.CreateDirectory(pdffiledir)
        Dim abstractfile As String = ""
        Dim abstractfile0 As String = ""

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(fpath))
            stream.Password = Nothing
            Dim entry As ZipEntry
label0:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(Path.Combine(fpath2, Path.GetFileName(entry.Name)))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                If Path.GetExtension(entry.Name).ToLower = ".dwxmz" Or Path.GetExtension(entry.Name).ToLower = ".dwxml" Then
                                    simname = Path.Combine(fpath2, Path.GetFileName(entry.Name))
                                ElseIf Path.GetExtension(entry.Name).ToLower = ".pdf" Then
                                    abstractfile0 = Path.Combine(fpath2, Path.GetFileName(entry.Name))
                                End If
                                GoTo label0
                            End If
                            stream2.Write(buffer, 0, count)
                        Loop
                    End Using
                End If
                entry = stream.GetNextEntry
            Loop
        End Using

        Dim xdoc As XDocument = Nothing

        If Path.GetExtension(simname).ToLower = ".dwxmz" Then
            xdoc = LoadZippedXML(simname)
        Else
            xdoc = XDocument.Load(simname)
        End If

        If abstractfile0 <> "" Then
            Try
                File.Copy(abstractfile0, Path.Combine(pdffiledir, Path.GetFileName(abstractfile0)), True)
                abstractfile = Path.Combine(pdffiledir, Path.GetFileName(abstractfile0))
            Catch ex As Exception
                Console.WriteLine("Error copying " & abstractfile0 & ": " & ex.ToString)
            End Try
        End If

        Try
            File.Delete(fpath)
        Catch ex As Exception
            Console.WriteLine("Error deleting " & fpath & ": " & ex.ToString)
        End Try
        Try
            Directory.Delete(fpath2, True)
        Catch ex As Exception
            Console.WriteLine("Error deleting " & fpath2 & ": " & ex.ToString)
        End Try

        If abstractfile <> "" Then
            Task.Factory.StartNew(Sub()
                                      Dim p = Process.Start(abstractfile)
                                      p?.WaitForExit()
                                      If MessageBox.Show(String.Format("Delete Abstract File '{0}'?", abstractfile), "Delete Abstract File", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
                                          Try
                                              File.Delete(abstractfile)
                                              MessageBox.Show("Abstract File deleted successfully.", "DWSIM")
                                          Catch ex As Exception
                                              MessageBox.Show(ex.Message, "Error deleting Abstract File")
                                          End Try
                                      End If
                                  End Sub)
        End If

        Return xdoc

    End Function

    Public Shared Function DownloadFlowsheet(address As String, pa As Action(Of Integer)) As String

        Dim wc As New WebClient()

        Dim siteUri As Uri = New Uri(address)
        Dim proxyUri As Uri = Net.WebRequest.GetSystemWebProxy.GetProxy(siteUri)

        If Not siteUri.AbsolutePath = proxyUri.AbsolutePath Then
            Dim proxyObj As New WebProxy(proxyUri)
            proxyObj.Credentials = CredentialCache.DefaultCredentials
            wc.Proxy = proxyObj
        End If

        Dim fpath = Path.GetTempFileName()

        AddHandler wc.DownloadProgressChanged, Sub(sender, e)
                                                   If pa IsNot Nothing Then pa.Invoke(e.ProgressPercentage)
                                               End Sub

        Dim t = wc.DownloadFileTaskAsync(New Uri(address), fpath)

        While Not t.IsCompleted
            Thread.Sleep(100)
        End While

        Return fpath

    End Function

    Private Shared Function LoadZippedXML(pathtofile As String) As XDocument

        Dim pathtosave As String = Path.GetTempPath()
        Dim fullname As String = ""

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(pathtofile))
            stream.Password = Nothing
            Dim entry As ZipEntry
label2:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(pathtosave + Path.GetFileName(entry.Name))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                fullname = pathtosave + Path.GetFileName(entry.Name)
                                GoTo label2
                            End If
                            stream2.Write(buffer, 0, count)
                        Loop
                    End Using
                End If
                entry = stream.GetNextEntry
            Loop
        End Using

        Dim xdoc = XDocument.Load(fullname)
        File.Delete(fullname)

        Return xdoc

    End Function



End Class
