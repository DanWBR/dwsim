<InterfaceType(ComInterfaceType.InterfaceIsDual)> Public Interface IFlowsheetEvents

    Event StatusChanged()

    Event NewMessageSent()

    Property Message As String

End Interface
