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
Imports System.Reflection
Imports System.Drawing.Text
Imports ScintillaNET
Imports System.Xml.Linq
Imports System.Linq

<System.Serializable()> Public Class ScriptEditorForm

    Public scripttext As String
    Public language As Integer
    Public includes As String()
    Public fontname As String = "Courier New"
    Public fontsize As Integer = 10
    Public highlightspaces As Boolean = False

    Private reader As Jolt.XmlDocCommentReader

#Region "Custom members"
    Private maxLineNumberCharLength As Integer
    Private loaded As Boolean = False
#End Region

    Private Sub ScriptEditorForm_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        scripttext = txtScript.Text
    End Sub

    Private Sub ScriptEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Try
            reader = New Jolt.XmlDocCommentReader(Assembly.GetExecutingAssembly())
        Catch ex As Exception

        End Try

        Me.txtScript.Tag = 0

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

        tscb1.SelectedItem = fontname

        tscb2.Items.AddRange(New Object() {6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16})

        tscb2.SelectedItem = fontsize

        btnHighlightSpaces.Checked = highlightspaces

        txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked)

        loaded = True

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

    Private Sub CutToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CutToolStripButton.Click
        txtScript.Cut()
    End Sub

    Private Sub CopyToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CopyToolStripButton.Click
        If txtScript.SelectedText <> "" Then Clipboard.SetText(txtScript.SelectedText)
    End Sub

    Private Sub PasteToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PasteToolStripButton.Click
        txtScript.Paste()
    End Sub

    Private Sub tscb1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb1.SelectedIndexChanged
        If loaded Then txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked)
    End Sub

    Private Sub tscb2_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb2.SelectedIndexChanged
        If loaded Then txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked)
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

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        If Me.Opacity < 1.0# Then Me.Opacity += 0.05
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        If Me.Opacity > 0.1# Then Me.Opacity -= 0.05
    End Sub

    Private Sub APIHelptsbutton_Click(sender As Object, e As EventArgs) Handles APIHelptsbutton.Click
        Process.Start("https://dwsim.org/api_help/index.html")
    End Sub

    Private Sub HelpToolStripButton_Click(sender As Object, e As EventArgs) Handles HelpToolStripButton.Click
        Process.Start("https://dwsim.org/wiki/index.php?title=Using_the_IronPython_Script_Manager")
    End Sub

    Private Sub btnDebug_Click(sender As Object, e As EventArgs) Handles btnDebug.Click
        Dim mycuo As UnitOperations.UnitOperations.CustomUO = My.Application.ActiveSimulation.SimulationObjects(My.Application.ActiveSimulation.FormSurface.FlowsheetSurface.SelectedObject.Name)
        mycuo.Includes = includes
        mycuo.ScriptText = Me.txtScript.Text
        FlowsheetSolver.FlowsheetSolver.CalculateObject(My.Application.ActiveSimulation, mycuo.Name)
    End Sub

    Private Sub txtScript__KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtScript.KeyDown

        If e.KeyValue = Keys.F5 Then

            btnDebug_Click(sender, e)

        End If

    End Sub

    Private Sub txtScript_TextChanged(sender As Object, e As EventArgs) Handles txtScript.TextChanged

        txtScript.SetColumnMargins()

        btnUndo.Enabled = txtScript.CanUndo
        btnRedo.Enabled = txtScript.CanRedo

        txtScript.ShowAutoComplete()

        txtScript.ShowToolTip()

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles btnUndo.Click
        txtScript.Undo()
    End Sub

    Private Sub btnRedo_Click(sender As Object, e As EventArgs) Handles btnRedo.Click
        txtScript.Redo()
    End Sub

    Private Sub btnHighlightSpaces_Click(sender As Object, e As EventArgs) Handles btnHighlightSpaces.CheckedChanged
        highlightspaces = btnHighlightSpaces.Checked
        If loaded Then txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked)
    End Sub

End Class