Imports DWSIM.Interfaces

Public Class FormCosting

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public CurrentFlowsheet As IFlowsheet

    Private Sub FormCosting_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Functions.CreateTransitionObject(CurrentFlowsheet, "", "Costing", "", "", Nothing)

        Functions.ProcessTransition(CurrentFlowsheet)

    End Sub

End Class