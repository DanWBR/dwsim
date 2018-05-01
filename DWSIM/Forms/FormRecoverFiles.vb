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


Imports System.IO

Public Class FormRecoverFiles

    Inherits System.Windows.Forms.Form

    Private Sub FormRecoverFiles_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Dim data, nomearquivo As String
        For Each str As String In My.Settings.BackupFiles
            If File.Exists(str) Then
                nomearquivo = str
                data = File.GetLastWriteTime(str).ToString
                Me.Grid1.Rows.Add(New Object() {1, nomearquivo, data})
            End If
        Next
    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click
        My.Settings.BackupFiles.Clear()
        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
        Me.Close()
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click

        For Each row As DataGridViewRow In Me.Grid1.SelectedRows
            Try
                If row.Cells(0).Value = 1 Then
                    Application.DoEvents()
                    FormMain.LoadAndExtractXMLZIP(row.Cells(1).Value, Nothing)
                End If
            Catch ex As Exception
                MessageBox.Show(ex.Message, DWSIM.App.GetLocalString("Erroaoabrircpiadeseg"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        Next

        My.Settings.BackupFiles.Clear()
        If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
        Me.Close()

    End Sub

End Class