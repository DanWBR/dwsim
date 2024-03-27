Imports DWSIM.Interfaces

Public Class FormBridgeToPro

    Public CurrentFlowsheet As IFlowsheet
    Private Transitioning As Boolean = False

    Private Sub FormBridgeToPro_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        DWSIM.ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = Nothing

        Close()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Transitioning = True

        Functions.ProcessTransition(CurrentFlowsheet)

        Close()

    End Sub

    Private Sub FormBridgeToPro_FormClosing(sender As Object, e As Windows.Forms.FormClosingEventArgs) Handles MyBase.FormClosing

        If Not Transitioning Then CurrentFlowsheet.FlowsheetOptions.FlowsheetTransitionObject = Nothing

    End Sub

End Class