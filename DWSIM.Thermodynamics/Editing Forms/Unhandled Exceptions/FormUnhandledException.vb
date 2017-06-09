Imports System.IO
Imports System.Net
Imports System.Globalization
Imports System.Text
Imports DWSIM.SharedClasses

'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Public Class FormUnhandledException

    Inherits System.Windows.Forms.Form

    Dim Loaded As Boolean = False
    Dim githublink As String = ""
    Public ex As Exception

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click
        Me.Close()
    End Sub

    Private Sub FormUnhandledException_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.Loaded = True
    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton4.Click

        Process.GetCurrentProcess.Kill()

    End Sub

    Private Sub FormUnhandledException_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Dim mystring = SharedClasses.EncryptString.StringCipher.Decrypt("YEZeCozmw0l3XjOYI0EpOXHh1LK9as6Bi5Gwqr7pYZyXtcNYQyzayHXts6NjAJlpfixoim98NAwVHli/+h1fYk6g4W82ewXDxkLwzg5SFCCSS2W0K3TvGMgC0wQWuKfrut0QdnByVKZ4x+/svdQwwXsUkZdELOUtnWiOdeV6WIQ=", "dwsim000000")
        Dim mystring2 = SharedClasses.EncryptString.StringCipher.Decrypt("T+h/AQaXoM7xMDrov6dkD/82uHShQ6gX7MD+yyPG1ALdchPnpYsxHZWU8YcwP3jTPCZWRL9mmAWnQnWtp4ETyYh17Cgjt1EDYbEJJvh/PacWXami/6btnnbE0D5HBpnYrKamsf6qjjx9JbhQOZIvXJv6dIlJ7lMm5vWkhmLpNuc=", "dwsim000000")

        Me.TextBox1.Text = ex.Message.ToString.Replace(mystring, "").Replace(mystring2, "")
        Me.TextBox2.Text = ex.ToString.Replace(mystring, "").Replace(mystring2, "")

        Button4.Enabled = False

        Try
            Dim baseaddress As String = "https://github.com/DanWBR/dwsim5/blob/master/"
            Dim st As New StackTrace(ex, True)
            Dim frame As StackFrame = st.GetFrame(0)
            Dim path As String = frame.GetFileName.Replace(mystring, baseaddress)
            path = path.Replace(mystring2, baseaddress)
            Dim line As Integer = frame.GetFileLineNumber()
            If path.Contains(baseaddress) Then
                githublink = path & "#L" & line
                Button4.Enabled = True
            End If
        Catch ex As Exception
        End Try

    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        Dim msg As New SendFileTo.MAPI()
        msg.AddRecipientTo("dwsim@inforside.com.br")
        msg.SendMailPopup("DWSIM Exception", "[PLEASE ADD EXCEPTION DETAILS HERE]" & vbCrLf & vbCrLf & "DWSIM version: " & My.Application.Info.Version.ToString & vbCrLf & ex.ToString)

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        If githublink <> "" Then Process.Start(githublink)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Application.Restart()

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click

        Dim baseaddress As String = "https://sourceforge.net/p/dwsim/search/?q="

        Dim searchtext As String = ex.Message.ToString.Replace(" ", "+")

        Process.Start(baseaddress + searchtext)

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        Dim baseaddress As String = "http://dwsim.inforside.com.br/wiki/index.php?title=Special:Search&fulltext=Search&profile=all&redirs=1&search="

        Dim searchtext As String = ex.Message.ToString.Replace(" ", "+")

        Process.Start(baseaddress + searchtext)

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        Dim myfile As String = My.Computer.FileSystem.SpecialDirectories.Temp & Path.DirectorySeparatorChar & "DWSIM" & Path.DirectorySeparatorChar & "dwsim.txt"
        Dim webcl As New System.Net.WebClient()

        Dim proxyObj As New WebProxy(Net.WebRequest.GetSystemWebProxy.GetProxy(New Uri("http://dwsim.inforside.com.br")))
        proxyObj.Credentials = CredentialCache.DefaultCredentials

        webcl.Proxy = proxyObj

        Try
            webcl.DownloadFile("http://dwsim.inforside.com.br/dwsim.txt", myfile)
            If File.Exists(myfile) Then
                Dim txt() As String = File.ReadAllLines(myfile)
                Dim build As Integer
                build = txt(0)
                If My.Application.Info.Version.Build < Convert.ToInt32(build) Then
                    Dim bdate As Date, fname As String, dlpath As String, changelog As String = ""
                    bdate = Date.Parse(txt(1), New CultureInfo("en-US"))
                    dlpath = txt(2)
                    fname = txt(3)
                    For i As Integer = 4 To txt.Length - 1
                        changelog += txt(i) + vbCrLf
                    Next
                    Dim strb As New StringBuilder()
                    With strb
                        .AppendLine("Build Number" & ": " & build & vbCrLf)
                        .AppendLine("Build Date" & ": " & bdate.ToString(My.Application.Culture.DateTimeFormat.ShortDatePattern, My.Application.Culture) & vbCrLf)
                        .AppendLine("Changes" & ": " & vbCrLf & changelog & vbCrLf)
                        .AppendLine("Do you want to go to the Downloads page?")
                    End With
                    Dim msgresult As MsgBoxResult = MessageBox.Show(strb.ToString, "A new version is available", MessageBoxButtons.YesNo, MessageBoxIcon.Information)
                    If msgresult = MsgBoxResult.Yes Then Process.Start(dlpath)
                Else
                    MessageBox.Show("DWSIM is already up-to-date.", "Checking for updates...", MessageBoxButtons.OK, MessageBoxIcon.Information)
                End If
            End If
        Catch ex As Exception
            MessageBox.Show(ex.Message.ToString, "Error while checking for updates", MessageBoxButtons.OK, MessageBoxIcon.Error)
        Finally
            webcl = Nothing
            proxyObj = Nothing
        End Try

    End Sub

End Class