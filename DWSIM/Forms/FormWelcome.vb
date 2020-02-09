Imports System.IO
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Linq
Imports System.Threading.Tasks
Imports System.Text

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

        chkAutoClose.Checked = My.Settings.AutoCloseWelcomePanel

        If DWSIM.App.IsRunningOnMono Then Me.BackgroundImageLayout = ImageLayout.Stretch

        Dim existingfiles As New List(Of String)

        If GlobalSettings.Settings.OldUI Then
            For Each f As String In My.Settings.MostRecentFiles
                existingfiles.Add(f)
            Next
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
                        lvi.ImageIndex = 1
                    Case ".dwcsd", ".dwcsd2"
                        lvi.ImageIndex = 2
                    Case ".dwrsd", ".dwrsd2"
                        lvi.ImageIndex = 3
                End Select
                If Not Me.lvlatestfolders.Items.ContainsKey(Path.GetDirectoryName(f)) Then
                    Me.lvlatestfolders.Items.Add(Path.GetDirectoryName(f), Path.GetDirectoryName(f), 4).Tag = Path.GetDirectoryName(f)
                    Me.lvlatestfolders.Items(Me.lvlatestfolders.Items.Count - 1).ToolTipText = Path.GetDirectoryName(f)
                End If
            End If
        Next

        Dim samples = Directory.EnumerateFiles(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "samples", "*.dw*", SearchOption.TopDirectoryOnly)

        For Each f As String In samples
            FormMain.SampleList.Add(f)
            Me.lvsamples.Items.Add(Path.GetFileName(f), 0).Tag = f
            Dim lvi = Me.lvsamples.Items(Me.lvsamples.Items.Count - 1)
            lvi.ToolTipText = f
            Select Case Path.GetExtension(f).ToLower
                Case ".dwsim"
                    lvi.ImageIndex = 0
                Case ".dwxml", ".dwxmz"
                    lvi.ImageIndex = 1
                Case ".dwcsd", ".dwcsd2"
                    lvi.ImageIndex = 2
                Case ".dwrsd", ".dwrsd2"
                    lvi.ImageIndex = 3
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
                                                                                           FOSSEEList.Items.Add(New ListViewItem(item.DisplayName, 1) With {.Tag = item.DownloadLink})
                                                                                           FormMain.FOSSEEList.Add(item)
                                                                                       Next
                                                                                       Owner.UpdateFOSSEEList()
                                                                                   End If
                                                                               End Sub)
                                                         End Sub)


        If DWSIM.App.IsRunningOnMono Then
            Me.lvlatest.View = View.List
            Me.lvlatestfolders.View = View.List
        End If

    End Sub

    Private Sub KryptonButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = Not chkAutoClose.Checked
        FormMain.NewToolStripButton_Click(sender, e)

    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = Not chkAutoClose.Checked
        Call FormMain.LoadFileDialog()
    End Sub

    Private Sub lvlatest_ItemActivate(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lvlatest.ItemActivate, lvsamples.ItemActivate

        Me.Parent.Visible = Not chkAutoClose.Checked

        Dim lview = DirectCast(sender, ListView)

        If File.Exists(lview.SelectedItems(0).Tag) Then

            Dim floading As New FormLoadingSimulation

            floading.Label1.Text = DWSIM.App.GetLocalString("LoadingFile") & vbCrLf & "(" & lview.SelectedItems(0).Tag.ToString & ")"
            floading.Show()

            Application.DoEvents()

            FormMain.filename = lview.SelectedItems(0).Tag
            Select Case Path.GetExtension(lview.SelectedItems(0).Tag).ToLower
                Case ".dwxml"
                    'FormMain.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + lview.SelectedItems(0).Tag + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    FormMain.LoadXML(lview.SelectedItems(0).Tag, Sub(x)
                                                                     Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                                 End Sub)
                Case ".dwxmz"
                    'FormMain.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + lview.SelectedItems(0).Tag + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    FormMain.LoadAndExtractXMLZIP(lview.SelectedItems(0).Tag, Sub(x)
                                                                                  Me.Invoke(Sub() floading.ProgressBar1.Value = x)
                                                                              End Sub)
                Case ".dwsim"
                    'FormMain.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + lview.SelectedItems(0).Tag + "..."
                    'Application.DoEvents()
                    'Application.DoEvents()
                    'FormMain.LoadF(lview.SelectedItems(0).Tag)
                Case ".xml"
                    'FormMain.ToolStripStatusLabel1.Text = DWSIM.App.GetLocalString("Abrindosimulao") + " " + lview.SelectedItems(0).Tag + "..."
                    Application.DoEvents()
                    Application.DoEvents()
                    FormMain.LoadMobileXML(lview.SelectedItems(0).Tag)
                Case ".dwcsd"
                    Dim NewMDIChild As New FormCompoundCreator()
                    NewMDIChild.MdiParent = FormMain
                    NewMDIChild.Show()
                    Dim objStreamReader As New FileStream(lview.SelectedItems(0).Tag, FileMode.Open, FileAccess.Read)
                    Dim x As New BinaryFormatter()
                    x.Binder = New VersionDeserializationBinder
                    NewMDIChild.mycase = x.Deserialize(objStreamReader)
                    NewMDIChild.mycase.Filename = lview.SelectedItems(0).Tag
                    objStreamReader.Close()
                    NewMDIChild.WriteData()
                    NewMDIChild.Activate()
                Case ".dwcsd2"
                    Dim NewMDIChild As New FormCompoundCreator()
                    NewMDIChild.MdiParent = FormMain
                    NewMDIChild.Show()
                    NewMDIChild.mycase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of CompoundGeneratorCase)(File.ReadAllText(lview.SelectedItems(0).Tag))
                    NewMDIChild.WriteData()
                    NewMDIChild.Activate()
                Case ".dwrsd"
                    Dim NewMDIChild As New FormDataRegression()
                    NewMDIChild.MdiParent = Me.Owner
                    NewMDIChild.Show()
                    Dim objStreamReader As New FileStream(lview.SelectedItems(0).Tag, FileMode.Open, FileAccess.Read)
                    Dim x As New BinaryFormatter()
                    x.Binder = New VersionDeserializationBinder
                    NewMDIChild.currcase = x.Deserialize(objStreamReader)
                    NewMDIChild.currcase.filename = lview.SelectedItems(0).Tag
                    objStreamReader.Close()
                    NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                    NewMDIChild.Activate()
                Case ".dwrsd2"
                    Dim NewMDIChild As New FormDataRegression()
                    NewMDIChild.MdiParent = Me.Owner
                    NewMDIChild.Show()
                    NewMDIChild.currcase = Newtonsoft.Json.JsonConvert.DeserializeObject(Of DWSIM.Optimization.DatRegression.RegressionCase)(File.ReadAllText(lview.SelectedItems(0).Tag))
                    NewMDIChild.currcase.filename = lview.SelectedItems(0).Tag
                    NewMDIChild.LoadCase(NewMDIChild.currcase, False)
                    NewMDIChild.Activate()
                Case ".dwruf"
                    Dim NewMDIChild As New FormUNIFACRegression()
                    NewMDIChild.MdiParent = Me.Owner
                    NewMDIChild.Show()
                    Dim objStreamReader As New FileStream(lview.SelectedItems(0).Tag, FileMode.Open, FileAccess.Read)
                    Dim x As New BinaryFormatter()
                    x.Binder = New VersionDeserializationBinder
                    NewMDIChild.mycase = x.Deserialize(objStreamReader)
                    NewMDIChild.mycase.Filename = lview.SelectedItems(0).Tag
                    objStreamReader.Close()
                    NewMDIChild.LoadCase(NewMDIChild.mycase, False)
                    NewMDIChild.Activate()
            End Select

            floading.Close()

        Else

            Throw New FileNotFoundException("File not found.", lview.SelectedItems(0).Tag.ToString)

        End If

    End Sub

    Private Sub lvlatestfolders_ItemActivate(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles lvlatestfolders.ItemActivate

        Application.DoEvents()
        Application.DoEvents()
        Me.Parent.Visible = Not chkAutoClose.Checked
        FormMain.OpenFileDialog1.InitialDirectory = Me.lvlatestfolders.SelectedItems(0).Tag
        Call FormMain.LoadFileDialog()

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim NewMDIChild As New FormCompoundCreator()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "CompoundCreator" & FormMain.m_childcount
        Me.Parent.Visible = Not chkAutoClose.Checked
        Application.DoEvents()
        Application.DoEvents()
        NewMDIChild.Show()
        NewMDIChild.MdiParent = Me.Owner
    End Sub

    Private Sub Button5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button5.Click
        Dim NewMDIChild As New FormDataRegression()
        'Set the Parent Form of the Child window.
        NewMDIChild.MdiParent = Me.Owner
        'Display the new form.
        NewMDIChild.Text = "DataRegression" & FormMain.m_childcount
        Me.Parent.Visible = Not chkAutoClose.Checked
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
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Main_Page")
    End Sub

    Private Sub LinkLabel5_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://sourceforge.net/p/dwsim/discussion/?source=navbar")
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("https://www.youtube.com/channel/UCzzBQrycKoN5XbCeLV12y3Q")
    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs)
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Category:Tutorials")
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
                    fdlding.Label1.Text = "Downloading file..." & vbCrLf & "(" & item.Title & ")"
                    fdlding.Show()
                    Application.DoEvents()
                    Task.Factory.StartNew(Function()
                                              Return SharedClasses.FOSSEEFlowsheets.DownloadFlowsheet(item.DownloadLink, Sub(px)
                                                                                                                             Me.UIThread(Sub()
                                                                                                                                             fdlding.Label1.Text = "Downloading file... (" & px & "%)" & vbCrLf & "(" & item.Title & ")"
                                                                                                                                             fdlding.ProgressBar1.Value = px
                                                                                                                                         End Sub)
                                                                                                                         End Sub)
                                          End Function).ContinueWith(Sub(tk)
                                                                         Me.UIThread(Sub() fdlding.Close())
                                                                         If tk.Exception IsNot Nothing Then
                                                                             MessageBox.Show(tk.Exception, "Error downloading file", MessageBoxButtons.OK, MessageBoxIcon.Error)
                                                                         Else
                                                                             Dim xdoc = SharedClasses.FOSSEEFlowsheets.LoadFlowsheet(tk.Result)
                                                                             Me.UIThread(Sub()
                                                                                             Me.Parent.Visible = Not chkAutoClose.Checked
                                                                                             floading.Label1.Text = DWSIM.App.GetLocalString("LoadingFile") & vbCrLf & "(" & item.Title & ")"
                                                                                             floading.Show()
                                                                                             Application.DoEvents()
                                                                                             Try
                                                                                                 FormMain.LoadXML2(xdoc, Sub(x)
                                                                                                                             Me.Invoke(Sub() floading.ProgressBar1.Value = x)
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

    Private Sub Button8_Click_1(sender As Object, e As EventArgs) Handles Button8.Click
        Process.Start("https://patreon.com/dwsim")
    End Sub

    Private Sub chkAutoClose_CheckedChanged(sender As Object, e As EventArgs) Handles chkAutoClose.CheckedChanged
        My.Settings.AutoCloseWelcomePanel = chkAutoClose.Checked
    End Sub
End Class