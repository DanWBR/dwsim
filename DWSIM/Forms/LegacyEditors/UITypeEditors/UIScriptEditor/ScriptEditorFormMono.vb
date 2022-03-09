'    UITypeEditor for Custom UO Script
'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Imports System.Drawing.Text

<System.Serializable()> Public Class ScriptEditorFormMono

    Public scripttext As String
    Public language As Integer
    Public includes As String()
    Public fontname As String = "Courier New"
    Public fontsize As Integer = 10

    '0 = VBScript
    '1 = JScript
    '2 = IronPython
    '3 = IronRuby

    Private Sub ScriptEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.txtScript.Text = scripttext

        Me.ListBox1.Items.Clear()
        If Not includes Is Nothing Then
            For Each i As String In includes
                Me.ListBox1.Items.Add(i)
            Next
        End If

        ' Get the installed fonts collection.
        Dim installed_fonts As New InstalledFontCollection
        ' Get an array of the system's font familiies.
        Dim font_families() As FontFamily = installed_fonts.Families()
        ' Display the font families.
        For Each font_family As FontFamily In font_families
            tscb1.Items.Add(font_family.Name)
        Next font_family

        tscb2.Items.AddRange(New Object() {6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16})

        Try
            tscb1.SelectedItem = fontname
        Catch ex As Exception
            tscb1.SelectedIndex = 0
        End Try
        tscb2.SelectedItem = fontsize

    End Sub

    Private Sub OpenToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OpenToolStripButton.Click
        If Me.txtScript.Text <> "" Then
            If MessageBox.Show(DWSIM.App.GetLocalString("DesejaSalvaroScriptAtual"), DWSIM.App.GetLocalString("Ateno"), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                SaveToolStripButton_Click(sender, e)
            End If
        End If
        If Me.ofd2.ShowDialog = Windows.Forms.DialogResult.OK Then
            Me.txtScript.Text = ""
            For Each fname As String In Me.ofd2.FileNames
                Me.txtScript.Text += File.ReadAllText(fname) & vbCrLf
            Next
        End If
    End Sub

    Private Sub SaveToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripButton.Click
        If Me.sfd1.ShowDialog = Windows.Forms.DialogResult.OK Then
            My.Computer.FileSystem.WriteAllText(Me.sfd1.FileName, Me.txtScript.Text, False)
        End If
    End Sub

    Private Sub PrintToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PrintToolStripButton.Click
        'Dim pd As Alsing.SourceCode.SourceCodePrintDocument
        'pd = New Alsing.SourceCode.SourceCodePrintDocument(txtScript.Document)
        'pd1.Document = pd
        'If pd1.ShowDialog(Me) = DialogResult.OK Then
        '    pd.Print()
        'End If
    End Sub

    Private Sub CutToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CutToolStripButton.Click
        txtScript.Cut()
    End Sub

    Private Sub CopyToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CopyToolStripButton.Click
        txtScript.Copy()
    End Sub

    Private Sub PasteToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PasteToolStripButton.Click
        txtScript.Paste(Clipboard.GetText)
    End Sub

    Private Sub tscb1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb1.SelectedIndexChanged
        Try
            txtScript.Font = New Font(tscb1.SelectedItem.ToString, tscb2.SelectedItem, System.Drawing.FontStyle.Regular)
            fontname = tscb1.SelectedItem.ToString
        Catch ex As Exception
        End Try
    End Sub

    Private Sub tscb2_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb2.SelectedIndexChanged
        Try
            txtScript.Font = New Font(tscb1.SelectedItem.ToString, tscb2.SelectedItem, System.Drawing.FontStyle.Regular)
            fontsize = tscb2.SelectedItem
        Catch ex As Exception
        End Try
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        If Me.ofd1.ShowDialog = Windows.Forms.DialogResult.OK Then
            For Each fname As String In Me.ofd1.FileNames
                Me.ListBox1.Items.Add(fname)
            Next
            ReDim includes(Me.ListBox1.Items.Count - 1)
            Dim i As Integer = 0
            For Each item As Object In Me.ListBox1.Items
                includes(i) = item.ToString
                i += 1
            Next
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        If Me.ListBox1.SelectedItems.Count > 0 Then
            Dim names As New ArrayList
            For Each fname As Object In Me.ListBox1.SelectedItems
                names.Add(fname)
            Next
            For Each fname As String In names
                Me.ListBox1.Items.Remove(fname)
            Next
            names = Nothing
            ReDim includes(Me.ListBox1.Items.Count - 1)
            Dim i As Integer = 0
            For Each item As Object In Me.ListBox1.Items
                includes(i) = item.ToString
                i += 1
            Next
        End If
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnDebug.Click
        Dim mycuo As CustomUO = My.Application.ActiveSimulation.SimulationObjects(My.Application.ActiveSimulation.FormSurface.FlowsheetSurface.SelectedObject.Name)
        mycuo.Includes = includes
        mycuo.ScriptText = Me.txtScript.Text
        mycuo.Solve()
    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        If Me.Opacity < 1.0# Then Me.Opacity += 0.05
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        If Me.Opacity > 0.0# Then Me.Opacity -= 0.05
    End Sub

    Private Sub APIHelptsbutton_Click(sender As Object, e As EventArgs) Handles APIHelptsbutton.Click
        Process.Start("https://dwsim.org/api_help/index.html")
    End Sub

    Private Sub HelpToolStripButton_Click(sender As Object, e As EventArgs) Handles HelpToolStripButton.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=Using_the_IronPython_Script_Manager")
    End Sub

    Private Sub txtScript_KeyDown_1(sender As Object, e As KeyEventArgs) Handles txtScript.KeyDown
        If e.KeyCode = Keys.F5 Then ToolStripButton1_Click(sender, e)
        Dim line As Integer = txtScript.GetLineFromCharIndex(txtScript.SelectionStart)
        Dim column As Integer = txtScript.SelectionStart - txtScript.GetFirstCharIndexOfCurrentLine()
        tsl1.Text = String.Format(DWSIM.App.GetLocalString("ScriptEditor_LineColumn"), line, column)
    End Sub
End Class