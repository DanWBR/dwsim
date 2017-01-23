<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IReactionStoichBase

    Property CompName() As String

    Property StoichCoeff() As Double

    Property DirectOrder() As Double

    Property ReverseOrder() As Double

    Property IsBaseReactant() As Boolean

End Interface
