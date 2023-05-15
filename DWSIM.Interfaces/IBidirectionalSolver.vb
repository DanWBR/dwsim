Public Interface IBidirectionalSolver

    Sub ObjectClickAction(sender As Object, args As EventArgs)

    Sub ObjectDoubleClickAction(sender As Object, args As EventArgs)

    Property Activated As Boolean

    Sub DisplayHelp()

End Interface
