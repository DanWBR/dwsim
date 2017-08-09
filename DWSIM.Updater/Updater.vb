Imports System.Net
Imports System.Text
Imports System.IO

Public Class Updater

    Public Downloader As FileDownloader

    Public BeginUpdater, UpdaterRunning As Action

    Public Sub LaunchUpdateProcess()

        If CheckForUpdateFile() Then

            Dim create As Boolean = False
            Try
                Directory.CreateDirectory(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update")
                create = True
            Catch ex As Exception
            End Try

            If create Then

                Dim ok As Boolean = False
                Dim updfile As String = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update" & Path.DirectorySeparatorChar & "update.txt"
                Try
                    Dim webcl As New System.Net.WebClient()
                    Dim proxyObj As New WebProxy(Net.WebRequest.GetSystemWebProxy.GetProxy(New Uri("http://dwsim.inforside.com.br")))
                    proxyObj.Credentials = CredentialCache.DefaultCredentials
                    webcl.Proxy = proxyObj
                    webcl.DownloadFile("http://dwsim.inforside.com.br/update/update.txt", updfile)
                    ok = True
                Catch ex As Exception
                End Try

                If ok Then

                    'check version

                    Dim v As String, u As Integer, md5 As String, filep As String

                    Dim appu As Integer
                    If File.Exists(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "version.info") Then
                        appu = Convert.ToInt32(File.ReadAllText(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "version.info"))
                    Else
                        appu = 0
                    End If

                    Dim filei As String() = File.ReadAllLines(updfile)
                    Dim md5list, filelist As New List(Of String)

                    For Each line In filei
                        v = line.Split(vbTab)(1)
                        u = line.Split(vbTab)(2)
                        filep = line.Split(vbTab)(3)
                        md5 = line.Split(vbTab)(4)
                        If v = My.Application.Info.Version.Major.ToString & "." & My.Application.Info.Version.Minor And u > appu Then
                            filelist.Add(filep)
                            md5list.Add(md5)
                        End If
                    Next

                    Try
                        File.Delete(updfile)
                    Catch ex As Exception
                        Exit Sub
                    End Try

                    Downloader = New FileDownloader(True)

                    Dim ftext As New StringBuilder
                    Dim i As Integer
                    If filelist.Count > 0 Then

                        For i = 0 To filelist.Count - 1
                            ftext.AppendLine(filelist(i) & vbTab & md5list(i))
                            Downloader.Files.Add(New FileDownloader.FileInfo("http://dwsim.inforside.com.br/update/" & filelist(i)))
                        Next

                        File.WriteAllText(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update" & Path.DirectorySeparatorChar & "filelist.txt", ftext.ToString)

                        'download files

                        BeginUpdater.Invoke()

                        Downloader.LocalDirectory = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update"
                        Downloader.DeleteCompletedFilesAfterCancel = True

                        UpdaterRunning.Invoke()

                        Downloader.Start()

                    End If

                End If

            End If

        End If

    End Sub

    Public Sub DeleteFiles()

        Dim files = Directory.GetFiles(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update")
        For Each f In files
            Try
                File.Delete(f)
            Catch ex As Exception
            End Try
        Next

    End Sub

    Private Function CheckForUpdateFile()

        Dim exists = False
        Dim response As HttpWebResponse = Nothing
        Dim request = WebRequest.Create("http://dwsim.inforside.com.br/update/update.txt")
        request.Proxy = New WebProxy(Net.WebRequest.GetSystemWebProxy.GetProxy(New Uri("http://dwsim.inforside.com.br")))
        request.Proxy.Credentials = CredentialCache.DefaultCredentials
        request.Method = "HEAD"

        Try
            response = request.GetResponse()
            exists = True
        Catch ex As WebException
            ' A WebException will be thrown if the status of the response is not `200 OK` */
        Finally
            If response IsNot Nothing Then response.Close()
        End Try

        Return exists

    End Function

End Class
