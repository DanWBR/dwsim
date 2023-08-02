Imports System.IO
Imports System.Net
Imports System.Globalization
Imports System.Text

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

    Private Sub FormUnhandledException_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.Loaded = True
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

    Private Sub FormUnhandledException_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        TextBox1.Text = ex.Message

        TextBox2.Text = ex.ToString()

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click

        Dim baseaddress As String = "https://sourceforge.net/p/dwsim/search/?q="

        Dim searchtext As String = ex.Message.ToString.Replace(" ", "+")

        If FormMain.IsPro Then
            Dim fb As New FormBrowser()
            fb.DisplayURL(baseaddress + searchtext, "SourceForge Forums")
            fb.Show()
        Else
            Process.Start(baseaddress + searchtext)
        End If

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        Dim baseaddress As String = "https://github.com/DanWBR/dwsim/issues?q="

        Dim searchtext As String = ex.Message.ToString.Replace(" ", "+")

        If FormMain.IsPro Then
            Dim fb As New FormBrowser()
            fb.DisplayURL(baseaddress + searchtext, "GitHub Issues")
            fb.Show()
        Else
            Process.Start(baseaddress + searchtext)
        End If

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Clipboard.SetText(ex.ToString())

        Dim url = "https://github.com/DanWBR/dwsim/issues/new?assignees=&labels=bug&projects=&template=bug_report.md&title=%5BBug%5D+"
        If FormMain.IsPro Then
            Dim fb As New FormBrowser()
            fb.DisplayURL(url, "GitHub Issues")
            fb.Show()
        Else
            Process.Start(url)
        End If

    End Sub

End Class