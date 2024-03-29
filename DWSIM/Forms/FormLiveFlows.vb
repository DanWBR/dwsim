Imports DWSIM.Interfaces

Public Class FormLiveFlows

    Public CurrentFlowsheet As IFlowsheet
    Private Transitioning As Boolean = False

    Private Sub Button2_Click_1(sender As Object, e As EventArgs) Handles Button2.Click

        ProFeatures.Functions.CreateTransitionObject(CurrentFlowsheet, "", "Live Flows", "", "", Nothing)

        ProFeatures.Functions.ProcessTransition(CurrentFlowsheet)

        Transitioning = True

        Close()

    End Sub

    Private Sub FormLiveFlows_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

    End Sub

    Private Sub FormLiveFlows_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed

        If Not Transitioning Then CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = Nothing

    End Sub

End Class