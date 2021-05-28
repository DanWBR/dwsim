'    Excel Unit Editor Form 
'    Copyright 2014 Gregor Reichert
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


Imports Microsoft.VisualBasic.Strings
Imports NetOffice
Imports Excel = NetOffice.ExcelApi
Imports NetOffice.ExcelApi.Enums
Imports System.IO

Public Class ExcelUOEditorForm
    Public FilePath As String

    Private Sub BtnSearch_Click(sender As System.Object, e As System.EventArgs) Handles BtnSearch.Click
        OpenFileDialog1.FileName = TbFileName.Text
        OpenFileDialog1.Filter = "Spreadsheet files|*.xlsx; *xls; *.ods"

        OpenFileDialog1.ValidateNames = True
        OpenFileDialog1.CheckFileExists = True
        OpenFileDialog1.CheckPathExists = True

        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            TbFileName.Text = OpenFileDialog1.FileName
        End If
    End Sub

    Private Sub BtnEdit_Click(sender As System.Object, e As System.EventArgs) Handles BtnEdit.Click

        If TbFileName.Text <> "" Then
            If My.Computer.FileSystem.FileExists(TbFileName.Text) Then
                If Not DWSIM.App.IsRunningOnMono Then
                    Process.Start(TbFileName.Text)
                Else
                    Process.Start(New ProcessStartInfo("xdg-open", TbFileName.Text) With {.UseShellExecute = False})
                End If
            Else
                MessageBox.Show(DWSIM.App.GetLocalString("Oarquivonoexisteoufo"), DWSIM.App.GetLocalString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            End If
        End If

    End Sub

    Private Sub BtnNew_Click(sender As System.Object, e As System.EventArgs) Handles BtnNew.Click

        Dim FileName As String = My.Application.ActiveSimulation.Text
        OpenFileDialog1.Title = "New Filename"
        OpenFileDialog1.Filter = "Spreadsheet files|*.xlsx; *xls; *.ods"
        OpenFileDialog1.ValidateNames = False
        OpenFileDialog1.CheckFileExists = False
        OpenFileDialog1.CheckPathExists = True
        OpenFileDialog1.InitialDirectory = Path.GetDirectoryName(FileName)

        If OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            Dim s As String = OpenFileDialog1.FileName
            If Path.GetExtension(s).ToLower = ".ods" Then
                FileCopy(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "TemplateExcelUO.ods", s)
            ElseIf Path.GetExtension(s).ToLower = ".xls" Then
                FileCopy(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "TemplateExcelUO.xls", s)
            Else
                FileCopy(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "TemplateExcelUO.xlsx", s)
            End If
            TbFileName.Text = s
        End If

    End Sub

End Class