Public Interface IConnectionPortInfo

    Property Name As String

    Property Index As Integer

    Property IsConnected As Boolean

    Property IsInput As Boolean

    Property IsOutput As Boolean

    Property IsEnergyPort As Boolean

    Property ConnectedObject As ISimulationObject

End Interface
