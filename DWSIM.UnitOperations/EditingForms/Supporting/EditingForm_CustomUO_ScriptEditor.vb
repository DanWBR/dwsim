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
Imports System.Windows.Forms
Imports System.Drawing

<System.Serializable()> Public Class EditingForm_CustomUO_ScriptEditor

    Inherits SharedClasses.ObjectEditorForm

    Public ScriptUO As UnitOperations.CustomUO

    Public language As Integer
    Public includes As String()
    Public scripttext As String = ""

    Public interpreter As String = "IronPython"
    Public pythonpath As String = ""

    Public FontSize As Integer = 10
    Public FontName As String = ""

    Private reader As New List(Of Jolt.XmlDocCommentReader)

    Public CAPEOPEN As Boolean = False

#Region "Custom members"
    Private maxLineNumberCharLength As Integer
    Private loaded As Boolean = False
#End Region

    Private Sub ScriptEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If CAPEOPEN Then
            Me.Text = "Script Editor"
            Width = 1024
            Height = 600
            ToolStrip2.Visible = True
        Else
            ToolStrip2.Visible = False
            Me.Text = ScriptUO.GraphicObject.Tag & " - " & Me.Text
        End If

        Dim apppath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

        If Not CAPEOPEN Then

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations,")).FirstOrDefault
            Dim fsolverassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.FlowsheetSolver,")).FirstOrDefault

            reader.Add(New Jolt.XmlDocCommentReader(Assembly.GetExecutingAssembly()))
            reader.Add(New Jolt.XmlDocCommentReader(calculatorassembly))
            reader.Add(New Jolt.XmlDocCommentReader(unitopassembly))
            reader.Add(New Jolt.XmlDocCommentReader(fsolverassembly))

        Else

            reader.Add(New Jolt.XmlDocCommentReader(Path.Combine(apppath, "CapeOpen.xml")))

        End If

        Me.txtScript.Tag = 0

        If Not CAPEOPEN Then Me.txtScript.Text = ScriptUO.ScriptText

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

        If Not CAPEOPEN Then btnHighlightSpaces.Checked = ScriptUO.HighlightSpaces

        If CAPEOPEN Then
            tscb1.SelectedItem = FontName
            tscb2.SelectedItem = FontSize
        Else
            Try
                tscb1.SelectedItem = ScriptUO.FontName
            Catch ex As Exception
            End Try
            Try
                tscb2.SelectedItem = ScriptUO.FontSize
            Catch ex As Exception
            End Try
        End If

        txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked, CAPEOPEN)

        Dim snippets = SharedClasses.Scripts.IronPythonSnippets.GetSnippets()

        For Each group1 In snippets.GroupBy(Function(x) x.Category1)

            Dim tsmi = New ToolStripMenuItem() With {.Text = group1.Key}
            tsbInsertSnippet.DropDownItems.Add(tsmi)

            For Each group2 In group1.GroupBy(Function(x2) x2.Category2)

                Dim tsmi2 = New ToolStripMenuItem() With {.Text = group2.Key}
                tsmi.DropDownItems.Add(tsmi2)

                For Each snippet In group2

                    Dim tsmi3 = New ToolStripMenuItem() With {.Text = snippet.Name & " (" & snippet.Scope & ")", .Tag = snippet.Snippet}

                    AddHandler tsmi3.Click, Sub()

                                                txtScript.InsertText(txtScript.CurrentPosition, tsmi3.Tag.ToString)

                                            End Sub

                    tsmi2.DropDownItems.Add(tsmi3)

                Next

            Next

        Next

        loaded = True

    End Sub

    Private Sub OpenToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles OpenToolStripButton.Click
        If Me.txtScript.Text <> "" Then
            If Not CAPEOPEN Then
                If MessageBox.Show(ScriptUO.FlowSheet.GetTranslatedString("DesejaSalvaroScriptAtual"), ScriptUO.FlowSheet.GetTranslatedString("Ateno"), MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                    SaveToolStripButton_Click(sender, e)
                End If
            Else
                If MessageBox.Show("Save current script?", "Save changes", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                    SaveToolStripButton_Click(sender, e)
                End If
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
        If CAPEOPEN Then
            Try
                ScriptUO.FontName = tscb1.SelectedItem.ToString
            Catch ex As Exception
            End Try
        Else
            FontName = tscb1.SelectedItem.ToString
        End If
        If loaded Then txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked, CAPEOPEN)
    End Sub

    Private Sub tscb2_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb2.SelectedIndexChanged
        If CAPEOPEN Then
            Try
                ScriptUO.FontSize = tscb2.SelectedItem.ToString
            Catch ex As Exception
            End Try
        Else
            FontSize = tscb2.SelectedItem.ToString
        End If
        If loaded Then txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, btnHighlightSpaces.Checked, CAPEOPEN)
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

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.Opacity < 1.0# Then Me.Opacity += 0.05
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.Opacity > 0.1# Then Me.Opacity -= 0.05
    End Sub

    Private Sub APIHelptsbutton_Click(sender As Object, e As EventArgs) Handles APIHelptsbutton.Click
        Process.Start("https://dwsim.org/api_help/html/G_DWSIM.htm")
    End Sub

    Private Sub HelpToolStripButton_Click(sender As Object, e As EventArgs) Handles HelpToolStripButton.Click
        Process.Start("https://sourceforge.net/p/dwsim/discussion/scripting/thread/3f56e2f421/")
    End Sub

    Private Sub btnDebug_Click(sender As Object, e As EventArgs) Handles btnDebug.Click
        ScriptUO.Includes = includes
        ScriptUO.ScriptText = Me.txtScript.Text
        ScriptUO.FlowSheet.RequestCalculation(ScriptUO)
    End Sub

    Private Sub txtScript__KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles txtScript.KeyDown

        If e.KeyValue = Keys.F5 And Not CAPEOPEN Then

            btnDebug_Click(sender, e)

        End If

    End Sub

    Private Sub txtScript_TextChanged(sender As Object, e As EventArgs) Handles txtScript.TextChanged

        txtScript.SetColumnMargins()

        btnUndo.Enabled = txtScript.CanUndo
        btnRedo.Enabled = txtScript.CanRedo

        Try
            txtScript.ShowAutoComplete(CAPEOPEN)
        Catch ex As Exception
        End Try
        Try
            txtScript.ShowToolTip(reader, CAPEOPEN)
        Catch ex As Exception
        End Try

        If Not CAPEOPEN Then ScriptUO.ScriptText = txtScript.Text Else scripttext = txtScript.Text

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles btnUndo.Click
        txtScript.Undo()
    End Sub

    Private Sub btnRedo_Click(sender As Object, e As EventArgs) Handles btnRedo.Click
        txtScript.Redo()
    End Sub

    Private Sub btnHighlightSpaces_Click(sender As Object, e As EventArgs) Handles btnHighlightSpaces.CheckedChanged
        If loaded Then
            ScriptUO.HighlightSpaces = btnHighlightSpaces.Checked
            txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, ScriptUO.HighlightSpaces, CAPEOPEN)
        End If
    End Sub

    Private Sub ToolStripButton1_Click_1(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        Process.Start("https://dwsim.fossee.in/custom-model")
    End Sub

    Private Sub ToolStripButton4_Click_1(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        FolderBrowserDialog1.SelectedPath = pythonpath
        If FolderBrowserDialog1.ShowDialog() = DialogResult.OK Then
            pythonpath = FolderBrowserDialog1.SelectedPath
            ToolStripTextBox1.Text = pythonpath
        End If
    End Sub

    Private Sub ToolStripComboBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ToolStripComboBox1.SelectedIndexChanged
        interpreter = ToolStripComboBox1.SelectedItem.ToString()
        If ToolStripComboBox1.SelectedIndex = 0 Then
            ToolStripLabel2.Enabled = False
            ToolStripTextBox1.Enabled = False
            ToolStripButton4.Enabled = False
        Else
            ToolStripLabel2.Enabled = True
            ToolStripTextBox1.Enabled = True
            ToolStripButton4.Enabled = True
        End If
    End Sub

    Private Sub EditingForm_CustomUO_ScriptEditor_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        If CAPEOPEN Then
            ToolStripTextBox1.Text = pythonpath
            ToolStripComboBox1.SelectedItem = interpreter
        End If
    End Sub
End Class