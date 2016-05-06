Imports Microsoft.Win32

Public Class FormCapeOpenUnitSelector

    Private _couos As New List(Of UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo)
    Public _seluo As UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo

    Private Sub FormCapeOpenUnitSelector_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.BackgroundWorker1.RunWorkerAsync()

        Me.Enabled = False

        Application.DoEvents()

        While Me.BackgroundWorker1.IsBusy
            Application.DoEvents()
        End While

        Me.Enabled = True

        With Me.ListBox1.Items
            For Each uo In _couos
                .Add(uo.Name)
            Next
        End With
        If Me.ListBox1.Items.Count > 0 Then
            Me.ListBox1.SelectedIndex = 0
        End If

    End Sub



    Sub SearchCOUOS()

        Dim keys As String() = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).GetSubKeyNames()
       
        For Each k2 In keys
            Dim mykey As RegistryKey = My.Computer.Registry.ClassesRoot.OpenSubKey("CLSID", False).OpenSubKey(k2, False)
            For Each s As String In mykey.GetSubKeyNames()
                If s = "Implemented Categories" Then
                    Dim arr As Array = mykey.OpenSubKey("Implemented Categories").GetSubKeyNames()
                    For Each s2 As String In arr
                        If s2.ToLower = "{678c09a5-7d66-11d2-a67d-00105a42887f}" Then
                            'this is a CAPE-OPEN UO
                            Dim myuo As New UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo
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
                                Try
                                    .Location = mykey.OpenSubKey("InProcServer32").GetValue("")
                                Catch ex As Exception
                                    .Location = mykey.OpenSubKey("LocalServer32").GetValue("")
                                End Try
                            End With
                            _couos.Add(myuo)
                        End If
                    Next
                End If
            Next
            mykey.Close()
        Next

    End Sub

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
        SearchCOUOS()
    End Sub

End Class