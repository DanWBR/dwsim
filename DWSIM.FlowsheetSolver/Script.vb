<System.Serializable()> Public Class Script

    Implements Interfaces.ICustomXMLSerialization

    Public Property ID As String = ""
    Public Property Title As String = ""
    Public Property ScriptText As String = ""
    Public Property LinkedObjectType As Interfaces.Enums.Scripts.ObjectType = Interfaces.Enums.Scripts.ObjectType.FlowsheetObject
    Public Property LinkedObjectName As String = ""
    Public Property LinkedEventType As Interfaces.Enums.Scripts.EventType = Interfaces.Enums.Scripts.EventType.SimulationOpened
    Public Property Linked As Boolean = False
    Public Property PythonInterpreter As Interfaces.Enums.Scripts.Interpreter = Interfaces.Enums.Scripts.Interpreter.IronPython

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

    Public Function SaveData() As List(Of XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

End Class
