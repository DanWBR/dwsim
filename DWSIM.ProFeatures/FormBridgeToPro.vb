Imports DWSIM.Interfaces

Public Class FormBridgeToPro

    Public Shared TransitionAction As Action(Of IFlowsheet)
    Public CurrentFlowsheet As IFlowsheet

    Private Sub FormBridgeToPro_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        DWSIM.ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Close()

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        TransitionAction?.Invoke(CurrentFlowsheet)

    End Sub

End Class