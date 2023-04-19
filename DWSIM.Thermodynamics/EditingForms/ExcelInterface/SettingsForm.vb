Imports System.IO
Imports Cudafy
Imports Cudafy.Host

Public Class SettingsForm

    Private loaded As Boolean = False

    Private flashsettings As Dictionary(Of Interfaces.Enums.FlashSetting, String)

    Private Sub SettingsForm_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"

        GlobalSettings.Settings.DebugLevel = 0

        Try
            GlobalSettings.Settings.SaveExcelSettings(inifile)
        Catch ex As Exception
            MessageBox.Show("Error saving current settings." & vbCrLf & "Error message: " & ex.ToString, "Error reading settings", MessageBoxButtons.OK)
        End Try

    End Sub

    Private Sub SettingsForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Application.EnableVisualStyles()

        My.Application.ChangeCulture("en")
        My.Application.ChangeUICulture("en")

        Dim inifile As String = My.Computer.FileSystem.SpecialDirectories.MyDocuments & Path.DirectorySeparatorChar & "DWSIM Application Data" & Path.DirectorySeparatorChar & "config.ini"

        tbConfigFilePath.Text = inifile

        Try
            GlobalSettings.Settings.LoadExcelSettings(inifile)
        Catch ex As Exception
            'MessageBox.Show("Error reading config file. Default settings will be used instead." & vbCrLf & "Error message: " & ex.ToString, "Error reading settings", MessageBoxButtons.OK)
        End Try

        Me.cbErrorHandlingMode.SelectedIndex = GlobalSettings.Settings.ExcelErrorHandlingMode

        Dim i As Integer = 0
        Me.cbParallelism.Items.Clear()
        Me.cbParallelism.Items.Add("Default")
        For i = 1 To System.Environment.ProcessorCount
            Me.cbParallelism.Items.Add(i.ToString)
        Next
        If GlobalSettings.Settings.MaxDegreeOfParallelism = -1 Then
            Me.cbParallelism.SelectedIndex = 0
        ElseIf GlobalSettings.Settings.MaxDegreeOfParallelism <= System.Environment.ProcessorCount Then
            Me.cbParallelism.SelectedItem = GlobalSettings.Settings.MaxDegreeOfParallelism.ToString
        Else
            Me.cbParallelism.SelectedIndex = Me.cbParallelism.Items.Count - 1
        End If

        Me.chkEnableParallelCalcs.Checked = GlobalSettings.Settings.EnableParallelProcessing
        Me.cbParallelism.Enabled = Me.chkEnableParallelCalcs.Checked
        Me.chkEnableSIMD.Checked = GlobalSettings.Settings.UseSIMDExtensions

        tbOctavePath.Text = Settings.OctavePath

        'user databases

        If Not GlobalSettings.Settings.UserDatabases Is Nothing Then
            For Each str As String In GlobalSettings.Settings.UserDatabases
                If File.Exists(str) Then
                    Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, Path.GetFileNameWithoutExtension(str), str, My.Resources.bullet_cross})
                    Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = "Remove"
                    i = i + 1
                End If
            Next
        End If

        If Not GlobalSettings.Settings.UserInteractionsDatabases Is Nothing Then
            For Each str As String In GlobalSettings.Settings.UserInteractionsDatabases
                If File.Exists(str) Then
                    Me.dgvIPDB.Rows.Add(New Object() {dgvIPDB.Rows.Count + 1, Path.GetFileNameWithoutExtension(str), str, My.Resources.bullet_cross})
                    Me.dgvIPDB.Rows(Me.dgvIPDB.Rows.Count - 1).Cells(3).ToolTipText = "Remove"
                End If
            Next
        End If

    End Sub

    Private Sub chkEnableParallelCalcs_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableParallelCalcs.CheckedChanged
        Me.cbParallelism.Enabled = Me.chkEnableParallelCalcs.Checked
        GlobalSettings.Settings.EnableParallelProcessing = Me.chkEnableParallelCalcs.Checked
    End Sub
    Private Sub cbParallelism_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbParallelism.SelectedIndexChanged
        If Me.cbParallelism.SelectedIndex = 0 Then
            GlobalSettings.Settings.MaxDegreeOfParallelism = -1
        Else
            GlobalSettings.Settings.MaxDegreeOfParallelism = Me.cbParallelism.SelectedItem
        End If
        Settings.MaxDegreeOfParallelism = GlobalSettings.Settings.MaxDegreeOfParallelism
    End Sub

    Private Sub chkEnableSIMD_CheckedChanged(sender As Object, e As EventArgs) Handles chkEnableSIMD.CheckedChanged
        GlobalSettings.Settings.UseSIMDExtensions = chkEnableSIMD.Checked
        Settings.UseSIMDExtensions = GlobalSettings.Settings.UseSIMDExtensions
    End Sub

    Private Sub cbErrorHandlingMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbErrorHandlingMode.SelectedIndexChanged
        GlobalSettings.Settings.ExcelErrorHandlingMode = cbErrorHandlingMode.SelectedIndex
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        If GlobalSettings.Settings.ExcelFlashSettings <> "" Then
            flashsettings = Newtonsoft.Json.JsonConvert.DeserializeObject(Of Dictionary(Of Interfaces.Enums.FlashSetting, String))(GlobalSettings.Settings.ExcelFlashSettings)
        Else
            flashsettings = PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm.GetDefaultSettings
        End If

        Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = flashsettings, .ExcelMode = True}

        f.ShowDialog(Me)
        flashsettings = f.Settings

        GlobalSettings.Settings.ExcelFlashSettings = Newtonsoft.Json.JsonConvert.SerializeObject(flashsettings, Newtonsoft.Json.Formatting.Indented)
 
        f.Dispose()
        f = Nothing

    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        If Me.OpenFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim path = Me.OpenFileDialog1.FileName
            Try
                Me.AddDatabase(IO.Path.GetFileNameWithoutExtension(path), path)
            Catch ex As System.Runtime.Serialization.SerializationException
                MessageBox.Show(ex.Message, "Error reading XML file", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        'Add interaction parameter user database
        If Me.OpenFileDialog1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim path = Me.OpenFileDialog1.FileName
            If Not GlobalSettings.Settings.UserInteractionsDatabases.Contains(path) Then
                GlobalSettings.Settings.UserInteractionsDatabases.Add(path)
                Me.dgvIPDB.Rows.Add(New Object() {dgvIPDB.Rows.Count + 1, IO.Path.GetFileNameWithoutExtension(path), path, My.Resources.bullet_cross})
                Me.dgvIPDB.Rows(Me.dgvIPDB.Rows.Count - 1).Cells(3).ToolTipText = "Remove"
            End If
        End If
    End Sub

    Sub AddDatabase(ByVal name As String, ByVal path As String)
        If Not GlobalSettings.Settings.UserDatabases.Contains(path) And File.Exists(path) Then
            GlobalSettings.Settings.UserDatabases.Add(path)
            Me.dgvdb.Rows.Add(New Object() {dgvdb.Rows.Count + 1, name, path, My.Resources.bullet_cross})
            Me.dgvdb.Rows(Me.dgvdb.Rows.Count - 1).Cells(3).ToolTipText = "Remove"
        End If
    End Sub

    Private Sub dgvdb_CellContentClick(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvdb.CellContentClick
        'remove component database
        If e.ColumnIndex = 3 Then
            Dim result = MessageBox.Show("Remove dataset " & dgvdb.Rows(e.RowIndex).Cells(1).Value & "?", "Question", MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            If result = DialogResult.Yes Then
                'remove user database
                Settings.UserDatabases.Remove(Me.dgvdb.Rows(e.RowIndex).Cells(2).Value)
                Me.dgvdb.Rows.RemoveAt(e.RowIndex)
            End If
        End If
    End Sub

    Private Sub dgvIPDB_CellContentClick(sender As System.Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvIPDB.CellContentClick
        'remove user interactions database
        If e.ColumnIndex = 3 Then
            Dim result = MessageBox.Show("Remove dataset " & dgvIPDB.Rows(e.RowIndex).Cells(1).Value & "?", "Question", MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            If result = DialogResult.Yes Then
                Settings.UserInteractionsDatabases.Remove(Me.dgvIPDB.Rows(e.RowIndex).Cells(2).Value)
                Me.dgvIPDB.Rows.RemoveAt(e.RowIndex)
            End If
        End If
    End Sub

    Private Sub btnSelectOctavePath_Click(sender As Object, e As EventArgs) Handles btnSelectOctavePath.Click
        FolderBrowserDialog1.SelectedPath = tbOctavePath.Text
        If FolderBrowserDialog1.ShowDialog() = DialogResult.OK Then
            tbOctavePath.Text = FolderBrowserDialog1.SelectedPath
            GlobalSettings.Settings.OctavePath = tbOctavePath.Text
        End If
    End Sub

End Class