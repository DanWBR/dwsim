Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Linq
Imports System.Threading.Tasks
Imports System.Text
Imports DWSIM.SharedClassesCSharp.FilePicker.Windows

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

Public Class FormWelcome

    Inherits UserControl

    Dim index As Integer = 0

    Dim fslist As New Dictionary(Of String, SharedClasses.FOSSEEFlowsheet)

    Public Property Owner As FormMain

    Private Sub FormTips_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If DWSIM.App.IsRunningOnMono Then Me.BackgroundImageLayout = ImageLayout.Stretch

        Dim existingfiles As New List(Of String)

        If GlobalSettings.Settings.OldUI Then
            For Each f As String In My.Settings.MostRecentFiles
                existingfiles.Add(f)
            Next
        End If

        If Settings.DpiScale > 1.0 Then
            lvlatest.SmallImageList = ImageList2
            lvlatest.LargeImageList = ImageList2
            lvsamples.SmallImageList = ImageList2
            lvsamples.LargeImageList = ImageList2
            lvlatestfolders.SmallImageList = ImageList2
            lvlatestfolders.LargeImageList = ImageList2
            FOSSEEList.SmallImageList = ImageList2
            FOSSEEList.LargeImageList = ImageList2
        End If

        existingfiles = existingfiles.Where(Function(x) File.Exists(x)).OrderByDescending(Function(x) File.GetLastWriteTime(x)).ToList

        For Each f As String In existingfiles
            If Path.GetExtension(f).ToLower <> ".dwbcs" Then
                Me.lvlatest.Items.Add(Path.GetFileName(f), 0).Tag = f
                Dim lvi = Me.lvlatest.Items(Me.lvlatest.Items.Count - 1)
                lvi.ToolTipText = f
                Select Case Path.GetExtension(f).ToLower
                    Case ".dwsim"
                        lvi.ImageIndex = 0
                    Case ".dwxml", ".dwxmz"
                        lvi.ImageIndex = 0
                    Case ".dwcsd", ".dwcsd2"
                        lvi.ImageIndex = 1
                    Case ".dwrsd", ".dwrsd2"
                        lvi.ImageIndex = 2
                End Select
                If Not Me.lvlatestfolders.Items.ContainsKey(Path.GetDirectoryName(f)) Then
                    Me.lvlatestfolders.Items.Add(Path.GetDirectoryName(f), Path.GetDirectoryName(f), 3).Tag = Path.GetDirectoryName(f)
                    Me.lvlatestfolders.Items(Me.lvlatestfolders.Items.Count - 1).ToolTipText = Path.GetDirectoryName(f)
                End If
            End If
        Next

        Dim samples = Directory.EnumerateFiles(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "samples", "*.dw*", SearchOption.TopDirectoryOnly)

        For Each f As String In samples
            My.Application.MainWindowForm.SampleList.Add(f)
            Me.lvsamples.Items.Add(Path.GetFileName(f), 0).Tag = f
            Dim lvi = Me.lvsamples.Items(Me.lvsamples.Items.Count - 1)
            lvi.ToolTipText = f
            Select Case Path.GetExtension(f).ToLower
                Case ".dwsim"
                    lvi.ImageIndex = 0
                Case ".dwxml", ".dwxmz"
                    lvi.ImageIndex = 0
                Case ".dwcsd", ".dwcsd2"
                    lvi.ImageIndex = 1
                Case ".dwrsd", ".dwrsd2"
                    lvi.ImageIndex = 2
            End Select
        Next

        FOSSEEList.Items.Add(New ListViewItem("Downloading flowsheet list, please wait...", 1) With {.Tag = ""})

        Task.Factory.StartNew(Function()
                                  Return SharedClasses.FOSSEEFlowsheets.GetFOSSEEFlowsheets()
                              End Function).ContinueWith(Sub(t)
                                                             Me.UIThreadInvoke(Sub()
                                                                                   FOSSEEList.Items.Clear()
                                                                                   If (t.Exception IsNot Nothing) Then
                                                                                       FOSSEEList.Items.Add(New ListViewItem("Error loading flowsheet list. Check your internet connection.", 1) With {.Tag = ""})
                                                                                   Else
                                                                                       For Each item As FOSSEEFlowsheet In t.Result
                                                                                           fslist.Add(item.DownloadLink, item)
                                                                                           FOSSEEList.Items.Add(New ListViewItem(item.DisplayName, 0) With {.Tag = item.DownloadLink})
                                                                                           My.Application.MainWindowForm.FOSSEEList.Add(item)
                                                                                       Next
                                                                                       'Owner.UpdateFOSSEEList()
                                                                                   End If
                                                                               End Sub)
                                                         End Sub)


        If DWSIM.App.IsRunningOnMono Then
            Me.lvlatest.View = View.List
            Me.lvlatestfolders.View = View.List
        End If

        ChangeDefaultFont(Me)

        NewsViewer.EnsureCoreWebView2Async(FormMain.WebView2Environment).ContinueWith(Sub()
                                                                                          UIThread(Sub()
                                                                                                       NewsViewer.Source = New Uri("https://www.patreon.com/dwsim/posts")
                                                                                                   End Sub)
                                                                                      End Sub)

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub KryptonButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        My.Application.MainWindowForm.NewToolStripButton_Click(sender, e)

    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog()
    End Sub

    Public Sub lvlatest_ItemActivate(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lvlatest.ItemActivate, lvsamples.ItemActivate

        Me.Parent.Visible = False
        My.Application.MainWindowForm.PainelDeBoasvindasToolStripMenuItem.Checked = False

        Dim lview = DirectCast(sender, ListView)

        If File.Exists(lview.SelectedItems(0).Tag) Then

            Dim handler = New WindowsFile(lview.SelectedItems(0).Tag)

            My.Application.MainWindowForm.LoadFile(handler)

        Else

            Throw New FileNotFoundException("File not found.", lview.SelectedItems(0).Tag.ToString)

        End If

    End Sub

    Private Sub lvlatestfolders_ItemActivate(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lvlatestfolders.ItemActivate

        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog()

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim NewMDIChild As New FormCompoundCreator()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "CompoundCreator" & My.Application.MainWindowForm.m_childcount
        Me.Parent.Visible = False
        Application.DoEvents()
        Application.DoEvents()
        NewMDIChild.Show()
        NewMDIChild.MdiParent = Me.Owner
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Dim NewMDIChild As New FormDataRegression()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "DataRegression" & My.Application.MainWindowForm.m_childcount
        Me.Parent.Visible = False
        Application.DoEvents()
        Application.DoEvents()
        NewMDIChild.Show()
        NewMDIChild.MdiParent = Me.Owner
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs)
        Process.Start("https://sourceforge.net/p/dwsim/donate/?source=navbar")
    End Sub

    Protected Overrides Sub OnPaint(ByVal e As System.Windows.Forms.PaintEventArgs)
        ' Do nothing here!
    End Sub

    Private Sub Button12_Click(sender As Object, e As EventArgs)
        Process.Start("https://itunes.apple.com/us/app/dwsim-simulator/id1162110266?ls=1&mt=8")
    End Sub

    Private Sub Button11_Click(sender As Object, e As EventArgs)
        Process.Start("https://play.google.com/store/apps/details?id=com.danielmedeiros.dwsim_simulator")
    End Sub

    Private Sub LinkLabel4_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://dwsim.org")
    End Sub

    Private Sub LinkLabel5_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://sourceforge.net/p/dwsim/discussion/?source=navbar")
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://www.youtube.com/channel/UCzzBQrycKoN5XbCeLV12y3Q")
    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://dwsim.org/wiki/index.php?title=Category:Tutorials")
    End Sub

    Private Sub LinkLabel3_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("http://dwsim.fossee.in/flowsheeting-project/completed-flowsheet")
    End Sub

    Private Sub fosseeselected(sender As Object, e As EventArgs) Handles FOSSEEList.ItemActivate
        If FOSSEEList.SelectedIndices.Count > 0 Then
            If ((FOSSEEList.SelectedIndices(0) >= 0) _
                AndAlso (FOSSEEList.SelectedItems(0).Tag <> "")) Then
                Dim item = fslist(FOSSEEList.SelectedItems(0).Tag)
                Dim sb = New StringBuilder
                sb.AppendLine(("Title: " + item.Title))
                sb.AppendLine(("Author: " + item.ProposerName))
                sb.AppendLine(("Institution: " + item.Institution))
                sb.AppendLine()
                sb.AppendLine("Click 'Yes' to download and open this flowsheet.")
                If MessageBox.Show(sb.ToString, "Open FOSSEE Flowsheet", MessageBoxButtons.YesNo, MessageBoxIcon.Information) = DialogResult.Yes Then
                    Dim floading As New FormLoadingSimulation
                    Dim fdlding As New FormLoadingSimulation
                    fdlding.Text = "Downloading file..." & " (" & item.Title & ")"
                    fdlding.Show()
                    Application.DoEvents()
                    Task.Factory.StartNew(Function()
                                              Return SharedClasses.FOSSEEFlowsheets.DownloadFlowsheet(item.DownloadLink, Sub(px)
                                                                                                                             Me.UIThread(Sub()
                                                                                                                                             fdlding.Text = "Downloading file... (" & px & "%)" & vbCrLf & "(" & item.Title & ")"
                                                                                                                                             fdlding.ProgressBar1.Value = px
                                                                                                                                             fdlding.Refresh()
                                                                                                                                         End Sub)
                                                                                                                         End Sub)
                                          End Function).ContinueWith(Sub(tk)
                                                                         Me.UIThread(Sub() fdlding.Close())
                                                                         If tk.Exception IsNot Nothing Then
                                                                             MessageBox.Show(tk.Exception, "Error downloading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                         Else
                                                                             Dim xdoc = SharedClasses.FOSSEEFlowsheets.LoadFlowsheet(tk.Result)
                                                                             Me.UIThread(Sub()
                                                                                             Me.Parent.Visible = False
                                                                                             My.Application.MainWindowForm.PainelDeBoasvindasToolStripMenuItem.Checked = False
                                                                                             floading.Text = DWSIM.App.GetLocalString("Loading") + " " + item.Title
                                                                                             floading.Show()
                                                                                             Application.DoEvents()
                                                                                             Try
                                                                                                 My.Application.MainWindowForm.LoadXML2(xdoc, Sub(x)
                                                                                                                                                  Me.Invoke(Sub()
                                                                                                                                                                floading.ProgressBar1.Value = x
                                                                                                                                                                fdlding.Refresh()
                                                                                                                                                            End Sub)
                                                                                                                                              End Sub)
                                                                                             Catch ex As Exception
                                                                                                 MessageBox.Show(tk.Exception, "Error loading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                                             Finally
                                                                                                 floading.Close()
                                                                                             End Try
                                                                                         End Sub)
                                                                         End If
                                                                     End Sub)
                End If
            End If
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Process.Start("https://dwsim.fossee.in/flowsheeting-project")
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        Process.Start("https://fossee.in/")
    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs)
        Process.Start("https://gumroad.com/products/PTljX")
    End Sub

    Private Sub Button8_Click_1(sender As Object, e As EventArgs)
        Process.Start("https://patreon.com/dwsim")
    End Sub

    Private Sub LinkLabel1_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel1.LinkClicked
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        My.Application.MainWindowForm.NewToolStripButton_Click(sender, e)
    End Sub

    Private Sub LinkLabel2_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog()
    End Sub

    Private Sub LinkLabel4_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel4.LinkClicked

        Dim NewMDIChild As New FormCompoundCreator()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "CompoundCreator" & My.Application.MainWindowForm.m_childcount
        Me.Parent.Visible = False
        Application.DoEvents()
        Application.DoEvents()
        NewMDIChild.Show()
        NewMDIChild.MdiParent = Me.Owner
    End Sub

    Private Sub LinkLabel6_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel6.LinkClicked
        Dim NewMDIChild As New FormDataRegression()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "DataRegression" & My.Application.MainWindowForm.m_childcount
        Me.Parent.Visible = False
        Application.DoEvents()
        Application.DoEvents()
        NewMDIChild.Show()
        NewMDIChild.MdiParent = Me.Owner
    End Sub

    Private Sub LinkLabel3_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog()
    End Sub

    Private Sub LinkLabel5_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel5.LinkClicked
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog()
    End Sub

    Private Sub LinkLabel8_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel8.LinkClicked
        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "User_Guide.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "user_guide.pdf")
        End If
    End Sub

    Private Sub LinkLabel10_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel10.LinkClicked
        Process.Start("https://dwsim.org/wiki/index.php?title=Tutorials")
    End Sub

    Private Sub FormWelcome_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        My.Application.MainWindowForm.PainelDeBoasvindasToolStripMenuItem.Checked = Visible

    End Sub

    Private Sub LinkLabel11_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel11.LinkClicked

        Dim wform As New UI.Desktop.Editors.CompoundCreatorWizard(Nothing)
        wform.SetupAndDisplayPage(1)

    End Sub

    Private Sub LinkLabel7_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel7.LinkClicked
        Process.Start("https://simulate365.com/dwsim-pro/")
    End Sub

    Private Sub LinkLabel12_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel12.LinkClicked
        Process.Start("https://dwsim.org/wiki/index.php?title=Literature")
    End Sub

    Private Sub LinkLabel13_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel13.LinkClicked
        Process.Start("https://dwsim.org/api_help/html/R_Project_DWSIM_Class_Library_Documentation.htm")
    End Sub

    Private Sub LinkLabel14_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel14.LinkClicked
        Process.Start("https://dwsim.org/index.php/dwsim-pro/")
    End Sub

    Private Sub LinkLabel15_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://dwsim.org/index.php/dwsim-social-responsibility-program/")
    End Sub

    Private Sub LinkLabel3_LinkClicked_2(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel3.LinkClicked
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = False
        Call My.Application.MainWindowForm.LoadFileDialog(True)
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Process.Start("https://github.com/sponsors/DanWBR")
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Process.Start("https://www.patreon.com/join/dwsim?")
    End Sub

    Private Sub Button3_Click_1(sender As Object, e As EventArgs) Handles Button3.Click
        Process.Start("https://www.buymeacoffee.com/dwsim")
    End Sub

    Private Sub LinkLabel9_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel9.LinkClicked
        Dim fqc As New FormCreateNewSolid()
        fqc.ShowDialog(Me)
    End Sub

    Private Sub LinkLabel15_LinkClicked_1(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel15.LinkClicked
        If DWSIM.App.IsRunningOnMono Then
            Dim p As New Process()
            With p
                .StartInfo.FileName = "xdg-open"
                .StartInfo.Arguments = My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "Pro_User_Guide.pdf"
                .StartInfo.UseShellExecute = False
                .Start()
            End With
        Else
            Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "docs" & Path.DirectorySeparatorChar & "Pro_User_Guide.pdf")
        End If
    End Sub
End Class