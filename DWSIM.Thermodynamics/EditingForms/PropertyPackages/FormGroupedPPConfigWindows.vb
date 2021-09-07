Public Class FormGroupedPPConfigWindows

    Public PropertyPackage As PropertyPackages.PropertyPackage
    Public Flowsheet As Interfaces.IFlowsheet

    Private Loaded As Boolean

    Private Sub FormGroupedPPConfigWindows_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Loaded = False

        For Each comp In Flowsheet.SelectedCompounds.Values

            CLBSolids.Items.Add(comp.Name, PropertyPackage.ForcedSolids.Contains(comp.Name))

        Next

        If Not PropertyPackage.IsConfigurable Then
            TabControl1.TabPages.Remove(TabPageBIPs)
            TabControl1.SelectedTab = TabPageFlash
        End If

        Loaded = True

    End Sub

    Private Sub CLBSolids_ItemCheck(sender As Object, e As ItemCheckEventArgs) Handles CLBSolids.ItemCheck
        If Loaded Then
            Dim comp = CLBSolids.Items(e.Index).ToString()
            If e.NewValue = CheckState.Checked Then
                If Not PropertyPackage.ForcedSolids.Contains(comp) Then PropertyPackage.ForcedSolids.Add(comp)
            Else
                If PropertyPackage.ForcedSolids.Contains(comp) Then PropertyPackage.ForcedSolids.Remove(comp)
            End If
        End If
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click

        PropertyPackage.DisplayAdvancedEditingForm()

    End Sub

End Class