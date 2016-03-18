Public Interface IFlowsheet

    Enum MessageType
        Information
        Warning
        GeneralError
        Tip
        Other
    End Enum

    ReadOnly Property SimulationObjects As Dictionary(Of String, ISimulationObject)

    ReadOnly Property GraphicObjects As Dictionary(Of String, IGraphicObject)

    Sub ShowMessage(ByVal text As String, ByVal mtype As MessageType)

    Sub CheckStatus()

    ReadOnly Property Settings As Dictionary(Of String, Object)

End Interface
