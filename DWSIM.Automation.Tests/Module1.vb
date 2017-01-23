Module Module1

    Sub Main()

        Dim interf As New DWSIM.Automation.Automation()

        Dim sim = interf.LoadFlowsheet("samples\Cavett's Problem.dwxml")

    End Sub

End Module
