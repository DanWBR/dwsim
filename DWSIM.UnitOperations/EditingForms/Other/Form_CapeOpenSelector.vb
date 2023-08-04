Imports Microsoft.Win32
Imports System.Windows.Forms

Public Class Form_CapeOpenSelector

    Private _couos As New List(Of UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)
    Public _seluo As UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo

    Private Sub FormCapeOpenUnitSelector_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.BackgroundWorker1.RunWorkerAsync()

        Dim frmwait As New FormLS

        frmwait.Text = "CAPE-OPEN Unit Operation Socket"
        frmwait.Label1.Text = "Scanning Registry for CAPE-OPEN Unit Operations"
        frmwait.StartPosition = FormStartPosition.CenterScreen
        frmwait.Opacity = 1.0#

        Me.Enabled = False

        Application.DoEvents()

        frmwait.Show(Me)

        While Me.BackgroundWorker1.IsBusy
            Application.DoEvents()
        End While

        frmwait.Close()

        Me.Enabled = True

        With Me.ListBox1.Items
            For Each uo As UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo In _couos
                .Add(uo.Name)
            Next
        End With
        If Me.ListBox1.Items.Count > 0 Then
            Me.ListBox1.SelectedIndex = 0
        End If

    End Sub

    Shared Function SearchCOUOS(ByVal chemseponly As Boolean) As List(Of UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)

        Dim clist As New List(Of UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)

        Dim keys As String() = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).GetSubKeyNames()

        For Each k2 In keys
            Dim mykey As RegistryKey = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).OpenSubKey(k2, False)
            For Each s As String In mykey.GetSubKeyNames()
                If s = "Implemented Categories" Then
                    Dim arr As Array = mykey.OpenSubKey("Implemented Categories").GetSubKeyNames()
                    For Each s2 As String In arr
                        If s2.ToLower = "{678c09a5-7d66-11d2-a67d-00105a42887f}" Then
                            'this is a CAPE-OPEN UO
                            Dim myuo As New UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo
                            With myuo
                                .AboutInfo = mykey.OpenSubKey("CapeDescription").GetValue("About")
                                .CapeVersion = mykey.OpenSubKey("CapeDescription").GetValue("CapeVersion")
                                .Description = mykey.OpenSubKey("CapeDescription").GetValue("Description")
                                .HelpURL = mykey.OpenSubKey("CapeDescription").GetValue("HelpURL")
                                .Name = mykey.OpenSubKey("CapeDescription").GetValue("Name")
                                .VendorURL = mykey.OpenSubKey("CapeDescription").GetValue("VendorURL")
                                .Version = mykey.OpenSubKey("CapeDescription").GetValue("ComponentVersion")
                                Try
                                    .TypeName = mykey.OpenSubKey("ProgID").GetValue("")
                                Catch ex As Exception
                                End Try
                                Dim key = mykey.OpenSubKey("InProcServer32")
                                If key IsNot Nothing Then
                                    .Location = key.GetValue("")
                                Else
                                    key = mykey.OpenSubKey("LocalServer32")
                                    If key IsNot Nothing Then .Location = key.GetValue("")
                                End If
                            End With
                            clist.Add(myuo)
                            If chemseponly And myuo.Name.ToLower.Contains("chemsep") Then
                                Return clist
                            End If
                        End If
                    Next
                End If
            Next
            mykey.Close()
        Next

        keys = My.Computer.Registry.CurrentUser.OpenSubKey("SOFTWARE").OpenSubKey("Classes").OpenSubKey("CLSID", False).GetSubKeyNames()

        For Each k2 In keys
            Dim mykey As RegistryKey = My.Computer.Registry.CurrentUser.OpenSubKey("SOFTWARE").OpenSubKey("Classes").OpenSubKey("CLSID", False).OpenSubKey(k2, False)
            For Each s As String In mykey.GetSubKeyNames()
                If s = "Implemented Categories" Then
                    Dim arr As Array = mykey.OpenSubKey("Implemented Categories").GetSubKeyNames()
                    For Each s2 As String In arr
                        If s2.ToLower = "{678c09a5-7d66-11d2-a67d-00105a42887f}" Then
                            'this is a CAPE-OPEN UO
                            Dim myuo As New UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo
                            With myuo
                                .AboutInfo = mykey.OpenSubKey("CapeDescription").GetValue("About")
                                .CapeVersion = mykey.OpenSubKey("CapeDescription").GetValue("CapeVersion")
                                .Description = mykey.OpenSubKey("CapeDescription").GetValue("Description")
                                .HelpURL = mykey.OpenSubKey("CapeDescription").GetValue("HelpURL")
                                .Name = mykey.OpenSubKey("CapeDescription").GetValue("Name")
                                .VendorURL = mykey.OpenSubKey("CapeDescription").GetValue("VendorURL")
                                .Version = mykey.OpenSubKey("CapeDescription").GetValue("ComponentVersion")
                                Try
                                    .TypeName = mykey.OpenSubKey("ProgID").GetValue("")
                                Catch ex As Exception
                                End Try
                                Dim key = mykey.OpenSubKey("InProcServer32")
                                If key IsNot Nothing Then
                                    .Location = key.GetValue("")
                                Else
                                    key = mykey.OpenSubKey("LocalServer32")
                                    If key IsNot Nothing Then .Location = key.GetValue("")
                                End If
                            End With
                            clist.Add(myuo)
                            If chemseponly And myuo.Name.ToLower.Contains("chemsep") Then
                                Return clist
                            End If
                        End If
                    Next
                End If
            Next
            mykey.Close()
        Next

        Return clist.OrderBy(Function(x) x.Name).ToList()

    End Function

    Private Sub ListBox1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ListBox1.SelectedIndexChanged
        With Me._couos(ListBox1.SelectedIndex)
            Me.txtAbout.Text = .AboutInfo
            Me.lblAuthorURL.Text = .VendorURL
            Me.txtDesc.Text = .Description
            Me.lblHelpURL.Text = .HelpURL
            Me.lblName.Text = .Name
            Me.lblVersion.Text = .Version
            Me.lblCOversion.Text = .CapeVersion
        End With
    End Sub

    Private Sub btnOK_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnOK.Click
        If Not Me.ListBox1.SelectedItem Is Nothing Then Me._seluo = _couos(Me.ListBox1.SelectedIndex)
        Me.Hide()
    End Sub

    Private Sub btnCancel_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnCancel.Click
        Me.Hide()
    End Sub

    Private Sub BackgroundWorker1_DoWork(ByVal sender As Object, ByVal e As System.ComponentModel.DoWorkEventArgs) Handles BackgroundWorker1.DoWork
        _couos = SearchCOUOS(False)
    End Sub

End Class