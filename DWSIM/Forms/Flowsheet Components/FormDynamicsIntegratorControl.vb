Public Class FormDynamicsIntegratorControl

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Sub FormDynamicsIntegratorControl_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub FormDynamicsIntegratorControl_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        If Visible Then

            cbScenario.Items.Clear()

            For Each item In Flowsheet.DynamicsManager.IntegratorList
                cbScenario.Items.Add(item.Value.Description)
            Next

            If cbScenario.Items.Count > 0 Then
                cbScenario.SelectedIndex = 0
            End If

        End If

    End Sub

End Class