<System.Serializable()> Public Class Script

    Implements XMLSerializer.Interfaces.ICustomXMLSerialization

    Public Property ID As String = ""
    Public Property Title As String = ""
    Public Property ScriptText As String = ""
    Public Property LinkedObjectType As Interfaces.Enums.Scripts.ObjectType = Interfaces.Enums.Scripts.ObjectType.FlowsheetObject
    Public Property LinkedObjectName As String = ""
    Public Property LinkedEventType As Interfaces.Enums.Scripts.EventType = Interfaces.Enums.Scripts.EventType.SimulationOpened
    Public Property Linked As Boolean = False

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

    Public Function SaveData() As List(Of XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

End Class
