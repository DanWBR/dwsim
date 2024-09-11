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

Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Linq

Public Class FormOptions

    Inherits UserControl

    Private loaded As Boolean = False

    Public AddMoreTabs As Action(Of TabControl)

    Private Sub FormOptions_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Dim i As Integer = 0

        If FormMain.IsPro Then
            KryptonButton1.Enabled = False
            btnSelectPythonPath.Enabled = False
            Button4.Enabled = False
            Button7.Enabled = False
            btnDownPy.Enabled = False
            tbPythonPath.Enabled = False
            FaTabStrip1.TabPages.Remove(TabUserDatasets)
        End If

        Me.chkEnableParallelCalcs.Checked = My.Settings.EnableParallelProcessing
        Me.chkEnableSIMD.Checked = My.Settings.UseSIMDExtensions

        Me.chkEnableInspector.Checked = My.Settings.InspectorEnabled

        Me.chkSaveBackupFile.Checked = My.Settings.SaveBackupFile
        Me.KryptonCheckBox6.Checked = My.Settings.BackupActivated
        Me.KryptonTextBox1.Text = My.Settings.BackupFolder
        Me.TrackBar1.Value = My.Settings.BackupInterval

        chkoverrjson.Checked = My.Settings.ReplaceJSONUserCompounds

        Me.chkIgnoreCompConstData.Checked = My.Settings.IgnoreCompoundPropertiesOnLoad

        Me.cbudb.Checked = My.Settings.ReplaceComps

        cbDefaultLocation.Items.Clear()
        cbDefaultLocation.Items.AddRange([Enum].GetNames(FlowsheetSurface_SkiaSharp.DockState.GetType))
        cbDefaultLocation.SelectedIndex = My.Settings.DefaultEditorLocation

        If TrackBar1.Value = 1 Then
            Me.KryptonLabel3.Text = DWSIM.App.GetLocalString("minuto1")
        Else
            Me.KryptonLabel3.Text = TrackBar1.Value & " " & DWSIM.App.GetLocalString("minutos")
        End If

        cbDebugLevel.SelectedIndex = My.Settings.DebugLevel

        chkHideSolidPhaseCO.Checked = My.Settings.HideSolidPhase_CO

        chkAA.Checked = My.Settings.FlowsheetAntiAliasing

        'solver

        If My.Settings.SolverMode = 0 Then My.Settings.SolverMode = 1

        tbSolverTimeout.Text = My.Settings.SolverTimeoutSeconds
        cbDebugLevel.SelectedIndex = My.Settings.DebugLevel
        chkSolverBreak.Checked = My.Settings.SolverBreakOnException

        chkCloseFormsOnDeselect.Checked = My.Settings.CloseFormsOnDeselecting
        chkEnableMultipleEditors.Checked = My.Settings.EnableMultipleObjectEditors
        chkEditorDoubleClick.Checked = My.Settings.DoubleClickToEdit

        'databases

        Me.PopulateDBGrid()

        'script paths
        'For Each s As String In My.Settings.ScriptPaths
        '    Me.lbpaths.Items.Add(s)
        'Next

        ComboBoxCompoundCopyMode.SelectedIndex = My.Settings.ClipboardCopyMode_Compounds
        ComboBoxPropPackCopyMode.SelectedIndex = My.Settings.ClipboardCopyMode_PropertyPackages
        CheckBoxUndoRedoRecalc.Checked = My.Settings.UndoRedo_RecalculateFlowsheet

        tbPythonPath.Text = My.Settings.PythonPath

        Dim configdir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "DWSIM_Project")

        If Directory.Exists(configdir) Then tbConfigDir.Text = configdir Else tbConfigDir.Text = "N/A"

        chkUpdates.Checked = Settings.CheckForUpdates

        Dim sukeys = FormMain.AvailableUnitSystems.Keys.ToArray()

        cbDefaultSU.Items.Clear()
        cbDefaultSU.Items.AddRange(sukeys)

        If sukeys.Contains(My.Settings.PreferredSystemOfUnits) Then
            cbDefaultSU.SelectedItem = My.Settings.PreferredSystemOfUnits
        Else
            My.Settings.PreferredSystemOfUnits = "SI"
            cbDefaultSU.SelectedItem = My.Settings.PreferredSystemOfUnits
        End If

        If Not FormMain.IsPro Then
            gbLoadExtensions.Visible = True
            chkLoadExtensions.Checked = My.Settings.LoadExtensionsAndPlugins
        Else
            gbLoadExtensions.Visible = False
        End If

        loaded = True

        AddMoreTabs?.Invoke(FaTabStrip1)

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub KryptonCheckBox6_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonCheckBox6.CheckedChanged
        My.Settings.BackupActivated = Me.KryptonCheckBox6.Checked
        If Me.KryptonCheckBox6.Checked Then
            My.Application.MainWindowForm.TimerBackup.Enabled = True
            Me.KryptonButton1.Enabled = True
            Me.KryptonTextBox1.Enabled = True
            Me.TrackBar1.Enabled = True
        Else
            My.Application.MainWindowForm.TimerBackup.Enabled = False
            Me.KryptonButton1.Enabled = False
            Me.KryptonTextBox1.Enabled = False
            Me.TrackBar1.Enabled = False
        End If
        If FormMain.IsPro Then KryptonButton1.Enabled = False
    End Sub

    Private Sub KryptonButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton1.Click
        FolderBrowserDialog1.SelectedPath = KryptonTextBox1.Text
        If FolderBrowserDialog1.ShowDialog() = DialogResult.OK Then
            KryptonTextBox1.Text = FolderBrowserDialog1.SelectedPath
            My.Settings.BackupFolder = KryptonTextBox1.Text
        End If
    End Sub

    Private Sub TrackBar1_Scroll(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TrackBar1.Scroll
        If TrackBar1.Value = 1 Then
            Me.KryptonLabel3.Text = DWSIM.App.GetLocalString("minuto1")
        Else
            Me.KryptonLabel3.Text = TrackBar1.Value & " " & DWSIM.App.GetLocalString("minutos")
        End If
        My.Settings.BackupInterval = TrackBar1.Value
        My.Application.MainWindowForm.TimerBackup.Interval = TrackBar1.Value * 60000
    End Sub

    Private Sub Button7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button7.Click
        'add user component database
        If Me.OpenFileDialog1.ShowDialog() = DialogResult.OK Then
            Dim path = Me.OpenFileDialog1.FileName
            Try
                Dim componentes As ConstantProperties()
                componentes = Databases.UserDB.ReadComps(path)
                If componentes.Length > 0 Then
                    If Me.cbudb.Checked Then
                        For Each c As ConstantProperties In componentes
                            If Not My.Application.MainWindowForm.AvailableComponents.ContainsKey(c.Name) Then
                                My.Application.MainWindowForm.AvailableComponents.Add(c.Name, c)
                            Else
                                My.Application.MainWindowForm.AvailableComponents(c.Name) = c
                            End If
                        Next
                    Else
                        For Each c As ConstantProperties In componentes
                            If Not My.Application.MainWindowForm.AvailableComponents.ContainsKey(c.Name) Then
                                My.Application.MainWindowForm.AvailableComponents.Add(c.Name, c)
                            End If
                        Next
                    End If
                    Me.AddDatabase("User" & CStr(My.Settings.UserDatabases.Count + 1), path)
                    MessageBox.Show(DWSIM.App.GetLocalString("UDBAdded"))
                End If
            Catch ex As System.Runtime.Serialization.SerializationException
                MessageBox.Show(DWSIM.App.GetLocalString("OarquivoXMLinformado") & vbCrLf & ex.Message, DWSIM.App.GetLocalString("ErroaoleroarquivoXML"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Finally
            End Try
        End If
    End Sub
    Private Sub Button4_Click(sender As System.Object, e As System.EventArgs) Handles Button4.Click
        'Add interaction parameter user database
        If Me.OpenFileDialog1.ShowDialog() = System.Windows.Forms.DialogResult.OK Then
            Dim path = Me.OpenFileDialog1.FileName

            If Not My.Settings.UserInteractionsDatabases.Contains(path) Then
                My.Settings.UserInteractionsDatabases.Add(path)
                If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
                Me.dgvIPDB.Rows.Add(New Object() {dgvIPDB.Rows.Count + 1, "User   ", path, My.Resources.disconnect})
                Me.dgvIPDB.Rows(Me.dgvIPDB.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Remove")
            End If
        End If

    End Sub

    Sub AddDatabase(ByVal name As String, ByVal path As String)
        If Not My.Settings.UserDatabases.Contains(path) And File.Exists(path) Then
            My.Settings.UserDatabases.Add(path)
            If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path, My.Resources.disconnect, My.Resources.application_form_edit})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ReadOnly = True
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Remove")
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Cliqueparaeditar")
        End If
    End Sub

    Sub PopulateDBGrid()

        Dim pathsep As Char = Path.DirectorySeparatorChar

        Me.dgvdb.Rows.Clear()
        Me.dgvIPDB.Rows.Clear()

        Dim name, path2 As String

        '===========================
        '=== Component databases ===
        '===========================

        'dwsim databases
        name = "DWSIM   "
        path2 = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "databases" & pathsep & "dwsim.xml"
        If File.Exists(path2) Then
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path2, My.Resources.lock, My.Resources.cross})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).ReadOnly = True
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Selado")
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Selado")
        End If

        name = "Biodiesel   "
        path2 = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "databases" & pathsep & "biod_db.xml"
        If File.Exists(path2) Then
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path2, My.Resources.lock, My.Resources.cross})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).ReadOnly = True
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Selado")
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Selado")
        End If

        name = "Electrolyte   "
        path2 = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "databases" & pathsep & "electrolyte.xml"
        If File.Exists(path2) Then
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path2, My.Resources.lock, My.Resources.cross})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).ReadOnly = True
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Selado")
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Selado")
        End If

        name = "CoolProp   "
        path2 = My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "databases" & pathsep & "coolprop.txt"
        If File.Exists(path2) Then
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path2, My.Resources.lock, My.Resources.cross})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).ReadOnly = True
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Selado")
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Selado")
        End If

        For Each cpath In My.Settings.UserCompounds
            If cpath.StartsWith("//Simulate 365 Dashboard") Then
                Using fileStream As Stream = Simulate365.Services.FileDownloadService.GetFileBySimulatePath(cpath)
                    If fileStream IsNot Nothing Then
                        dgjsonfiles.Rows.Add(New Object() {dgjsonfiles.Rows.Count + 1, cpath, My.Resources.icons8_cancel})
                        dgjsonfiles.Rows(dgjsonfiles.Rows.Count - 1).Cells(2).ToolTipText = DWSIM.App.GetLocalString("Remove")
                    End If
                End Using
            Else
                If File.Exists(cpath) Then
                    dgjsonfiles.Rows.Add(New Object() {dgjsonfiles.Rows.Count + 1, cpath, My.Resources.icons8_cancel})
                    dgjsonfiles.Rows(dgjsonfiles.Rows.Count - 1).Cells(2).ToolTipText = DWSIM.App.GetLocalString("Remove")
                End If
            End If
        Next

        'chemsep database
        If My.Application.MainWindowForm.loadedCSDB Then
            name = "ChemSep   "
            path2 = My.Settings.ChemSepDatabasePath
            If File.Exists(path2) Then
                Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path2, My.Resources.disconnect, My.Resources.cross})
                Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ReadOnly = True
                Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Remove")
                Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Selado")
            End If
        End If

        Dim i As Integer = 1

        'user databases
        If Not My.Settings.UserDatabases Is Nothing Then
            For Each str As String In My.Settings.UserDatabases
                If File.Exists(str) Then
                    Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, "User   " & CStr(i) & "   ", str, My.Resources.disconnect, My.Resources.application_form_edit})
                    Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).ReadOnly = True
                    Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Remove")
                    Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(4).ToolTipText = DWSIM.App.GetLocalString("Cliqueparaeditar")
                    i = i + 1
                End If
            Next
        End If

        '=======================================
        '=== Interaction parameter databases ===
        '=======================================

        'user databases
        If Not My.Settings.UserInteractionsDatabases Is Nothing Then
            For Each str As String In My.Settings.UserInteractionsDatabases
                If File.Exists(str) Then
                    Me.dgvIPDB.Rows.Add(New Object() {dgvIPDB.Rows.Count + 1, "User   ", str, My.Resources.disconnect})
                    Me.dgvIPDB.Rows(Me.dgvIPDB.Rows.Count - 1).Cells(3).ReadOnly = True
                    Me.dgvIPDB.Rows(Me.dgvIPDB.Rows.Count - 1).Cells(3).ToolTipText = DWSIM.App.GetLocalString("Remove")
                End If
            Next
        End If

    End Sub

    Private Sub dgvdb_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvdb.CellContentClick
        'remove component database
        If e.ColumnIndex = 3 Then
            Dim result = MessageBox.Show("Delete database " & dgvdb.Rows(e.RowIndex).Cells(1).Value & "?", "Question", MessageBoxButtons.OKCancel, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1)
            If result = DialogResult.OK Then
                'remove user database
                My.Settings.UserDatabases.Remove(Me.dgvdb.Rows(e.RowIndex).Cells(2).Value)
                Me.dgvdb.Rows.RemoveAt(e.RowIndex)
                MessageBox.Show(DWSIM.App.GetLocalString("UDBRemoved"), DWSIM.App.GetLocalString("Informao"), MessageBoxButtons.OK, MessageBoxIcon.Information)
            End If
        End If

        If e.ColumnIndex = 4 Then
            If Mid(Me.dgvdb.Rows(e.RowIndex).Cells(1).Value, 1, 4) = "User" Then
                'call database editor
                FormDBManager.DBPath = Me.dgvdb.Rows(e.RowIndex).Cells(2).Value
                FormDBManager.ShowDialog()
            End If
        End If
    End Sub

    Private Sub dgvIPDB_CellContentClick(sender As System.Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvIPDB.CellContentClick

        'remove user interactions database

        If e.ColumnIndex = 3 Then
            If Me.dgvIPDB.Rows(e.RowIndex).Cells(1).Value = "User   " Then
                My.Settings.UserInteractionsDatabases.Remove(Me.dgvIPDB.Rows(e.RowIndex).Cells(2).Value)
                If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
                Me.dgvIPDB.Rows.RemoveAt(e.RowIndex)
                MessageBox.Show(DWSIM.App.GetLocalString("UDBRemoved"), DWSIM.App.GetLocalString("Informao"), MessageBoxButtons.OK, MessageBoxIcon.Information)
            End If

        End If

    End Sub

    Private Sub cbudb_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles cbudb.CheckedChanged
        My.Settings.ReplaceComps = cbudb.Checked
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("AreYouSure"), "DWSIM", MessageBoxButtons.YesNo) = DialogResult.Yes Then
            If Directory.Exists(My.Settings.BackupFolder) Then
                Try
                    For Each f As String In Directory.GetFiles(My.Settings.BackupFolder, "*.dw*")
                        File.Delete(f)
                    Next
                    MessageBox.Show(DWSIM.App.GetLocalString("CleanSuccessful"), "DWSIM", MessageBoxButtons.OK)
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("CleanFailed") + " " + ex.Message, "DWSIM", MessageBoxButtons.OK)
                End Try
            End If
        End If
    End Sub

    Private Sub chkEnableParallelCalcs_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkEnableParallelCalcs.CheckedChanged
        My.Settings.EnableParallelProcessing = Me.chkEnableParallelCalcs.Checked
        Settings.EnableParallelProcessing = My.Settings.EnableParallelProcessing
    End Sub

    Private Sub cbDebugLevel_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDebugLevel.SelectedIndexChanged
        My.Settings.DebugLevel = cbDebugLevel.SelectedIndex
        Settings.DebugLevel = My.Settings.DebugLevel
    End Sub

    Private Sub tbSolverTimeout_TextChanged(sender As Object, e As EventArgs) Handles tbSolverTimeout.TextChanged
        If Integer.TryParse(tbSolverTimeout.Text, New Integer) Then
            My.Settings.SolverTimeoutSeconds = Integer.Parse(tbSolverTimeout.Text)
            GlobalSettings.Settings.SolverTimeoutSeconds = My.Settings.SolverTimeoutSeconds
        End If
    End Sub

    Private Sub FormOptions_HelpRequested(sender As Object, hlpevent As HelpEventArgs) Handles MyBase.HelpRequested
        Select Case FaTabStrip1.SelectedTab.Name
            Case "FaTabStripItem1"
                DWSIM.App.HelpRequested("CONF_GlobalSettings1.htm")
            Case "FaTabStripItem3"
                DWSIM.App.HelpRequested("CONF_GlobalSettings2.htm")
            Case "FaTabStripItem6"
                DWSIM.App.HelpRequested("CONF_GlobalSettings3.htm")
            Case "FaTabStripItem4"
                DWSIM.App.HelpRequested("CONF_GlobalSettings4.htm")
            Case "FaTabStripItem2"
                DWSIM.App.HelpRequested("CONF_GlobalSettings5.htm")
            Case "FaTabStripItem5"
                DWSIM.App.HelpRequested("CONF_GlobalSettings6.htm")
            Case "FaTabStripItem7"
                DWSIM.App.HelpRequested("CONF_GlobalSettings7.htm")
        End Select

    End Sub
    Private Sub chkSolverBreak_CheckedChanged(sender As Object, e As EventArgs) Handles chkSolverBreak.CheckedChanged
        My.Settings.SolverBreakOnException = chkSolverBreak.Checked
        Settings.SolverBreakOnException = My.Settings.SolverBreakOnException
    End Sub

    Private Sub chkSaveBackupFile_CheckedChanged(sender As Object, e As EventArgs) Handles chkSaveBackupFile.CheckedChanged
        My.Settings.SaveBackupFile = chkSaveBackupFile.Checked
    End Sub

    Private Sub chkEnableSIMD_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableSIMD.CheckedChanged
        My.Settings.UseSIMDExtensions = chkEnableSIMD.Checked
        Settings.UseSIMDExtensions = My.Settings.UseSIMDExtensions
    End Sub

    Private Sub ComboBoxCompoundCopyMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxCompoundCopyMode.SelectedIndexChanged
        My.Settings.ClipboardCopyMode_Compounds = ComboBoxCompoundCopyMode.SelectedIndex
    End Sub

    Private Sub ComboBoxPropPackCopyMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ComboBoxPropPackCopyMode.SelectedIndexChanged
        My.Settings.ClipboardCopyMode_PropertyPackages = ComboBoxPropPackCopyMode.SelectedIndex
    End Sub

    Private Sub CheckBoxUndoRedoRecalc_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBoxUndoRedoRecalc.CheckedChanged
        My.Settings.UndoRedo_RecalculateFlowsheet = CheckBoxUndoRedoRecalc.Checked
    End Sub

    Private Sub chkCloseFormsOnDeselect_CheckedChanged(sender As Object, e As EventArgs) Handles chkCloseFormsOnDeselect.CheckedChanged
        My.Settings.CloseFormsOnDeselecting = chkCloseFormsOnDeselect.Checked
    End Sub

    Private Sub cbDefaultLocation_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDefaultLocation.SelectedIndexChanged
        My.Settings.DefaultEditorLocation = cbDefaultLocation.SelectedIndex
        GlobalSettings.Settings.DefaultEditFormLocation = cbDefaultLocation.SelectedIndex
    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableMultipleEditors.CheckedChanged
        My.Settings.EnableMultipleObjectEditors = chkEnableMultipleEditors.Checked
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles chkHideSolidPhaseCO.CheckedChanged
        My.Settings.HideSolidPhase_CO = chkHideSolidPhaseCO.Checked
        GlobalSettings.Settings.HideSolidPhaseFromCAPEOPENComponents = My.Settings.HideSolidPhase_CO
    End Sub

    Private Sub chkIgnoreCompConstData_CheckedChanged(sender As Object, e As EventArgs) Handles chkIgnoreCompConstData.CheckedChanged
        My.Settings.IgnoreCompoundPropertiesOnLoad = Me.chkIgnoreCompConstData.Checked
    End Sub

    Private Sub btnSelectPythonPath_Click(sender As Object, e As EventArgs) Handles btnSelectPythonPath.Click
        FolderBrowserDialog1.SelectedPath = tbPythonPath.Text
        If FolderBrowserDialog1.ShowDialog() = DialogResult.OK Then
            tbPythonPath.Text = FolderBrowserDialog1.SelectedPath
            My.Settings.PythonPath = tbPythonPath.Text
            GlobalSettings.Settings.PythonPath = tbPythonPath.Text
        End If
    End Sub

    Private Sub chkEnableInspector_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableInspector.CheckedChanged
        My.Settings.InspectorEnabled = chkEnableInspector.Checked
        Settings.InspectorEnabled = My.Settings.InspectorEnabled
        My.Application.MainWindowForm.tsbInspector.Checked = chkEnableInspector.Checked
    End Sub

    Private Sub chkAA_CheckedChanged(sender As Object, e As EventArgs) Handles chkAA.CheckedChanged
        My.Settings.FlowsheetAntiAliasing = chkAA.Checked
        GlobalSettings.Settings.DrawingAntiAlias = chkAA.Checked
    End Sub

    Private Sub chkEditorDoubleClick_CheckedChanged(sender As Object, e As EventArgs) Handles chkEditorDoubleClick.CheckedChanged
        My.Settings.DoubleClickToEdit = chkEditorDoubleClick.Checked
        GlobalSettings.Settings.EditOnSelect = Not chkEditorDoubleClick.Checked
    End Sub

    Private Sub tbPythonPath_TextChanged(sender As Object, e As EventArgs) Handles tbPythonPath.TextChanged
        My.Settings.PythonPath = tbPythonPath.Text
        GlobalSettings.Settings.PythonPath = tbPythonPath.Text
    End Sub

    Private Sub btnClearDir_Click(sender As Object, e As EventArgs) Handles btnClearDir.Click
        If MessageBox.Show(DWSIM.App.GetLocalString("AreYouSure"), "DWSIM", MessageBoxButtons.YesNo) = DialogResult.Yes Then
            Dim configdir = tbConfigDir.Text
            If Directory.Exists(configdir) Then
                Try
                    Directory.Delete(configdir, True)
                    MessageBox.Show(DWSIM.App.GetLocalString("CleanSuccessful"), "DWSIM", MessageBoxButtons.OK)
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("CleanFailed") + " " + ex.Message, "DWSIM", MessageBoxButtons.OK)
                End Try
            End If
        End If
    End Sub

    Private Sub CheckBox1_CheckedChanged_1(sender As Object, e As EventArgs) Handles chkUpdates.CheckedChanged
        My.Settings.CheckForUpdates = chkUpdates.Checked
        Settings.CheckForUpdates = chkUpdates.Checked
    End Sub

    Private Sub chkLoadExtensions_CheckedChanged(sender As Object, e As EventArgs) Handles chkLoadExtensions.CheckedChanged
        My.Settings.LoadExtensionsAndPlugins = chkLoadExtensions.Checked
    End Sub

    Private Sub btnDownPy_Click(sender As Object, e As EventArgs) Handles btnDownPy.Click
        Process.Start("https://winpython.github.io/")
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Importing Compounds from JSON Files", "", Nothing)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON file", "*.json")})
        If openedFile IsNot Nothing Then
            Try
                Dim c = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(openedFile.ReadAllText())
                If chkoverrjson.Checked Then
                    If Not My.Application.MainWindowForm.AvailableComponents.ContainsKey(c.Name) Then
                        My.Application.MainWindowForm.AvailableComponents.Add(c.Name, c)
                    Else
                        My.Application.MainWindowForm.AvailableComponents(c.Name) = c
                    End If
                Else
                    If Not My.Application.MainWindowForm.AvailableComponents.ContainsKey(c.Name) Then
                        My.Application.MainWindowForm.AvailableComponents.Add(c.Name, c)
                    End If
                End If
                If Not My.Settings.UserCompounds.Contains(openedFile.FullPath) Then
                    My.Settings.UserCompounds.Add(openedFile.FullPath)
                    dgjsonfiles.Rows.Add(New Object() {dgjsonfiles.Rows.Count + 1, openedFile.FullPath, My.Resources.icons8_cancel})
                    dgjsonfiles.Rows(dgjsonfiles.Rows.Count - 1).Cells(2).ToolTipText = DWSIM.App.GetLocalString("Remove")
                    My.Settings.Save()
                    MessageBox.Show("File successfully added to the list.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                Else
                    MessageBox.Show("The selected file is already on the list.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ": " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub chkoverrjson_CheckedChanged(sender As Object, e As EventArgs) Handles chkoverrjson.CheckedChanged

        My.Settings.ReplaceJSONUserCompounds = chkoverrjson.Checked

    End Sub

    Private Sub dgjsonfiles_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgjsonfiles.CellContentClick
        'remove component
        If e.ColumnIndex = 2 Then
            Dim result = MessageBox.Show("Remove file " & dgjsonfiles.Rows(e.RowIndex).Cells(1).Value & " from list?", "Question", MessageBoxButtons.YesNo, MessageBoxIcon.Question, MessageBoxDefaultButton.Button1)
            If result = DialogResult.Yes Then
                My.Settings.UserCompounds.Remove(Me.dgjsonfiles.Rows(e.RowIndex).Cells(1).Value)
                Me.dgjsonfiles.Rows.RemoveAt(e.RowIndex)
                My.Settings.Save()
                MessageBox.Show("File successfully removed from list.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
            End If
        End If
    End Sub

    Private Sub cbDefaultSU_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDefaultSU.SelectedIndexChanged
        My.Settings.PreferredSystemOfUnits = cbDefaultSU.SelectedItem.ToString()
    End Sub
End Class