Public Class FormOccupancyQuestion
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim usertype As String = ""

        If rbConsultant.Checked Then
            usertype = "Consultant"
        ElseIf rbEmployee.Checked Then
            usertype = "Employee"
        ElseIf rbHobbyist.Checked Then
            usertype = "Hobbyist/Enthusiast"
        ElseIf rbStudent.Checked Then
            usertype = "Student"
        ElseIf rbTeacher.Checked Then
            usertype = "Teacher"
        ElseIf rbOther.Checked Then
            usertype = tbOther.Text
        End If

        FormMain.AnalyticsProvider?.RegisterEvent("User Profile", usertype, Nothing)

        My.Settings.UserTypeSent = True
        My.Settings.Save()

        FormMain.tsbQuickQuestion.Visible = False

        If FormMain.TranslateFunction IsNot Nothing Then
            MessageBox.Show(FormMain.TranslateFunction.Invoke("Thank you very much for your contribution!"),
                            FormMain.TranslateFunction.Invoke("Thank you"),
                            MessageBoxButtons.OK, MessageBoxIcon.Information)
        Else
            MessageBox.Show("Thank you very much for your contribution!", "Thank you",
                            MessageBoxButtons.OK, MessageBoxIcon.Information)
        End If

        Close()

    End Sub

    Private Sub FormOccupancyQuestion_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

End Class