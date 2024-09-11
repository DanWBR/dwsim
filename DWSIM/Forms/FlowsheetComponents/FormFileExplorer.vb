Imports System.ComponentModel
Imports System.IO
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.SharedClassesCSharp.FilePicker.Windows
Imports Eto.WinForms

Public Class FormFileExplorer

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private TempDir As String

    Private Loaded As Boolean = False

    Public Flowsheet As FormFlowsheet

    Private Sub FormFileExplorer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        TempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory(TempDir)

        If Settings.DpiScale > 1.0 Then
            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.AutoSize = True
            Me.ToolStrip1.Invalidate()
        End If

        Loaded = True

        UpdateSize()
        ListFiles()

    End Sub

    Private Sub UpdateSize()

        lblSize.Text = String.Format(Flowsheet.GetTranslatedString1("DBSize"), Flowsheet.FileDatabaseProvider.GetSizeinKB())

    End Sub

    Public Sub ListFiles()

        Try
            Dim provider = Flowsheet.FileDatabaseProvider
            Dim files = provider.GetFiles()
            ListView1.Items.Clear()
            For Each item In files
                ListView1.Items.Add(item)
            Next
        Catch ex As Exception
        End Try

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListView1.SelectedIndexChanged

        If Loaded Then
            If ListView1.SelectedItems.Count <= 0 Then Exit Sub
            Dim provider = Flowsheet.FileDatabaseProvider
            DisplayFileInViewer(ListView1.SelectedItems(0).Text)
        End If

    End Sub

    Public Sub DisplayFileInViewer(filename As String)

        UIThread(Sub()
                     Dim provider = Flowsheet.FileDatabaseProvider
                     If provider.CheckIfExists(filename) Then
                         Dim TempFilePath As String = Path.Combine(TempDir, filename)
                         provider.ExportFile(filename, TempFilePath)
                         Viewer.Source = New Uri(TempFilePath)
                     End If
                     Me.Activate()
                 End Sub)


    End Sub

    Public Sub SetViewerURL(url As String)

        UIThread(Sub()
                     Viewer.Source = New Uri(url)
                     Me.Activate()
                 End Sub)

    End Sub

    Private Sub btnImport_Click(sender As Object, e As EventArgs) Handles btnImport.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        'All Supported Files|*.pdf;*.jpg;*.png;*.mov;*.mp4;*.mp3;*.txt;*.py;*.html;*.dwxmz;*.dwxml;*.xml;*.json;*.dwcsd2;*.dwrsd2

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("All Supported Files",
                                                                               New String() {"*.pdf", "*.jpg", "*.png", "*.mov", "*.mp4", "*.mp3", "*.txt", "*.py", "*.html", "*.dwxmz", "*.dwxml", "*.xml", "*.json", "*.dwcsd2", "*.dwrsd2", "*.xlsx", "*.xls", "*.pxml", "*.dwcdi"})})

        If openedFile IsNot Nothing Then
            Dim provider = Flowsheet.FileDatabaseProvider
            If TypeOf openedFile Is WindowsFile Then
                Try
                    provider.PutFile(openedFile.FullPath, openedFile.Filename)
                Catch ex As Exception
                    MessageBox.Show(openedFile.Filename + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                End Try
            Else
                Using str = openedFile.OpenRead()
                    Try
                        provider.PutFile(str, openedFile.Filename)
                    Catch ex As Exception
                        MessageBox.Show(openedFile.Filename + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                    End Try
                End Using
            End If
            ListFiles()
            UpdateSize()
            Flowsheet.UpdateOpenEditForms()
        End If

    End Sub

    Private Sub btnExport_Click(sender As Object, e As EventArgs) Handles btnExport.Click

        If ListView1.SelectedItems.Count > 0 Then

            Dim filename = ListView1.SelectedItems(0).Text

            Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

            filePickerForm.SuggestedFilename = filename

            Dim extension = Path.GetExtension(filename)

            Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                New List(Of FilePickerAllowedType) From {New FilePickerAllowedType(String.Format("{0} File", extension), String.Format("*{0}", extension))})

            If handler IsNot Nothing Then
                Dim provider = Flowsheet.FileDatabaseProvider
                If TypeOf handler Is WindowsFile Then
                    Try
                        provider.ExportFile(filename, handler.FullPath)
                    Catch ex As Exception
                        MessageBox.Show(filename + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                    End Try
                Else
                    Using stream As New MemoryStream()
                        Try
                            provider.ExportFile(filename, stream)
                            handler.Write(stream)
                        Catch ex As Exception
                            MessageBox.Show(ListView1.SelectedItems(0).Text + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                        End Try
                    End Using
                End If
            End If
        End If
    End Sub

    Private Sub btnDelete_Click(sender As Object, e As EventArgs) Handles btnDelete.Click
        If ListView1.SelectedItems.Count > 0 Then
            If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                          DWSIM.App.GetLocalString("Ateno2"),
                                          MessageBoxButtons.YesNo,
                                          MessageBoxIcon.Question) = DialogResult.Yes Then
                Dim provider = Flowsheet.FileDatabaseProvider
                Try
                    provider.DeleteFile(ListView1.SelectedItems(0).Text)
                Catch ex As Exception
                    MessageBox.Show(ListView1.SelectedItems(0).Text + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                End Try
                ListFiles()
                UpdateSize()
            End If
        End If
    End Sub

    Private Sub FormFileExplorer_Disposed(sender As Object, e As EventArgs) Handles Me.Disposed

        Try
            Directory.Delete(TempDir, True)
        Catch ex As Exception
        End Try

    End Sub

    Private Sub FormFileExplorer_Activated(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        Try
            If Flowsheet IsNot Nothing Then
                ListFiles()
                UpdateSize()
            End If
        Catch ex As Exception
        End Try

    End Sub

    Private Sub FormFileExplorer_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub

End Class