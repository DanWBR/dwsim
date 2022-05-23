Public Class FormEventDescription

    Public Property PEx As SharedClasses.ExceptionProcessing.ProcessedException

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Close()
    End Sub

    Private Sub FormEventDescription_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub FormEventDescription_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        If PEx IsNot Nothing Then

            tbEventType.Text = "Error (Exception)"

            tbEventDescription.Text = PEx.DetailedDescription + vbCrLf + vbCrLf + PEx.ExceptionObject.ToString()

            tbEventLocation.Text = "Code Location: " + PEx.CodeLocation + vbCrLf + vbCrLf + "Calling Method: " + PEx.CallingMethod + vbCrLf + vbCrLf + PEx.CodeLocationDetails

            tbEventActions.Text = PEx.UserAction

        End If

    End Sub

End Class