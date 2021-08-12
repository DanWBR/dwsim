Imports System.ComponentModel
Imports System.IO

Public Class FormFileExplorer

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Private TempDir As String

    Private Loaded As Boolean = False

    Public Flowsheet As FormFlowsheet

    Private Sub FormFileExplorer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        TempDir = Path.Combine(Path.GetTempPath(), Path.GetRandomFileName())
        Directory.CreateDirectory(TempDir)

        Loaded = True

        UpdateSize()
        ListFiles()

    End Sub

    Private Sub UpdateSize()

        lblSize.Text = String.Format(Flowsheet.GetTranslatedString1("DBSize"), Flowsheet.FileDatabaseProvider.GetSizeinKB())

    End Sub

    Public Sub ListFiles()

        Dim provider = Flowsheet.FileDatabaseProvider
        Dim files = provider.GetFiles()
        ListBox1.Items.Clear()
        For Each item In files
            ListBox1.Items.Add(item)
        Next

    End Sub

    Private Sub FormFileExplorer_Closing(sender As Object, e As CancelEventArgs) Handles Me.Closing

        Try
            Directory.Delete(TempDir, True)
        Catch ex As Exception
        End Try

    End Sub

    Private Sub ListBox1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListBox1.SelectedIndexChanged

        If Loaded Then
            If ListBox1.SelectedIndex < 0 Then Exit Sub
            Dim provider = Flowsheet.FileDatabaseProvider
            If provider.CheckIfExists(ListBox1.SelectedItem) Then
                Dim TempFilePath As String = Path.Combine(TempDir, ListBox1.SelectedItem)
                provider.ExportFile(ListBox1.SelectedItem, TempFilePath)
                Viewer.Source = New Uri(TempFilePath)
            End If

        End If

    End Sub

    Private Sub btnImport_Click(sender As Object, e As EventArgs) Handles btnImport.Click

        If Me.ofd1.ShowDialog() = Windows.Forms.DialogResult.OK Then
            Dim provider = Flowsheet.FileDatabaseProvider
            For Each file In ofd1.FileNames
                Try
                    provider.PutFile(file)
                Catch ex As Exception
                    MessageBox.Show(file + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                End Try
            Next
            ListFiles()
            UpdateSize()
        End If

    End Sub

    Private Sub btnExport_Click(sender As Object, e As EventArgs) Handles btnExport.Click
        If ListBox1.SelectedItem IsNot Nothing Then
            sfd1.FileName = ListBox1.SelectedItem
            If Me.sfd1.ShowDialog() = Windows.Forms.DialogResult.OK Then
                Dim provider = Flowsheet.FileDatabaseProvider
                Try
                    provider.ExportFile(ListBox1.SelectedItem, sfd1.FileName)
                Catch ex As Exception
                    MessageBox.Show(ListBox1.SelectedItem + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                End Try
            End If
        End If
    End Sub

    Private Sub btnDelete_Click(sender As Object, e As EventArgs) Handles btnDelete.Click
        If ListBox1.SelectedItem IsNot Nothing Then
            If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                          DWSIM.App.GetLocalString("Ateno2"),
                                          MessageBoxButtons.YesNo,
                                          MessageBoxIcon.Question) = DialogResult.Yes Then
                Dim provider = Flowsheet.FileDatabaseProvider
                Try
                    provider.DeleteFile(ListBox1.SelectedItem)
                Catch ex As Exception
                    MessageBox.Show(ListBox1.SelectedItem + ":" + ex.Message, Flowsheet.GetTranslatedString1("Erro"))
                End Try
                ListFiles()
                UpdateSize()
            End If
        End If
    End Sub

    Private Sub FormFileExplorer_GotFocus(sender As Object, e As EventArgs) Handles Me.GotFocus

        ListFiles()
        UpdateSize()

    End Sub

End Class