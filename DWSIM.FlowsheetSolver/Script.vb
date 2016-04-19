<System.Serializable()> Public Class Script

    Implements XMLSerializer.Interfaces.ICustomXMLSerialization

    Public Enum ObjectType
        Simulation = 0
        Solver = 1
        FlowsheetObject = 2
    End Enum

    Public Enum EventType
        SimulationOpened = 0
        SimulationSaved = 1
        SimulationClosed = 2
        ObjectCalculationStarted = 3
        ObjectCalculationFinished = 4
        ObjectCalculationError = 5
        SolverStarted = 6
        SolverFinished = 7
        SolverRecycleLoop = 8
        SimulationTimer1 = 9
        SimulationTimer5 = 10
        SimulationTimer15 = 11
        SimulationTimer30 = 12
        SimulationTimer60 = 13
    End Enum

    Public Property ID As String = ""
    Public Property Title As String = ""
    Public Property ScriptText As String = ""
    Public Property LinkedObjectType As ObjectType = ObjectType.FlowsheetObject
    Public Property LinkedObjectName As String = ""
    Public Property LinkedEventType As EventType = EventType.SimulationOpened
    Public Property Linked As Boolean = False

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

    Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

End Class
