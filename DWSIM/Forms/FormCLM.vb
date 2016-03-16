Public Class FormCLM

    Private Sub BtnAbort_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BtnAbort.Click
        My.Application.CalculatorStopRequested = True
    End Sub
End Class