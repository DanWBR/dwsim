Imports DWSIM.Interfaces

Public Class FormGHG

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public CurrentFlowsheet As IFlowsheet

    Private Sub FormGHG_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Functions.CreateTransitionObject(CurrentFlowsheet, "", "GHG Emissions", "", "", Nothing)

        Functions.ProcessTransition(CurrentFlowsheet)

    End Sub

End Class