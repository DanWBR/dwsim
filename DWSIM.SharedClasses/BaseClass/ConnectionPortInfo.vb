Public Class ConnectionPortInfo

    Implements IConnectionPortInfo

    Public Property Name As String Implements IConnectionPortInfo.Name

    Public Property Index As Integer Implements IConnectionPortInfo.Index

    Public Property IsConnected As Boolean Implements IConnectionPortInfo.IsConnected

    Public Property IsInput As Boolean Implements IConnectionPortInfo.IsInput

    Public Property IsOutput As Boolean Implements IConnectionPortInfo.IsOutput

    Public Property IsEnergyPort As Boolean Implements IConnectionPortInfo.IsEnergyPort

    Public Property ConnectedObject As ISimulationObject Implements IConnectionPortInfo.ConnectedObject

End Class
