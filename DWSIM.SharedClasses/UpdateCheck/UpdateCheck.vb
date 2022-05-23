Imports System.Net

Public Class UpdateCheck

    Public Shared Function CheckForUpdates() As Boolean
        Try
            Dim webClient = New WebClient()
            Dim url = New Uri("https://dwsim.org/update/desktop.txt")
            Dim latestversion As String = ""
            latestversion = WebClient.DownloadString(url)
            'Console.WriteLine("Latest Version: " & latestversion)
            If latestversion = "" Then Return False
            latestversion = latestversion.TrimEnd(vbCrLf).TrimEnd(Environment.NewLine).TrimEnd()
            Dim currver = New Version(GlobalSettings.Settings.CurrentRunningVersion)
            Dim latver = New Version(latestversion)
            'Console.WriteLine("Current Version: " & currver.ToString)
            'Console.WriteLine("Latest Version: " & latver.ToString)
            If latver > currver Then
                Return True
            Else
                Return False
            End If
        Catch ex As Exception
            Console.WriteLine("Error checking latest version: " & ex.ToString)
            Return False
        End Try
    End Function

    Public Shared Function GetWhatsNew() As String
        Try
            Dim webClient = New WebClient()
            Dim url = New Uri("https://dwsim.org/update/whatsnew_d.txt")
            Dim whatsnew As String = ""
            whatsnew = webClient.DownloadString(url)
            Return whatsnew
        Catch ex As Exception
            Return ""
        End Try
    End Function

End Class
