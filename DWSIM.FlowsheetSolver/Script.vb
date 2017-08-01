Imports DWSIM.Interfaces
<System.Serializable()> Public Class Script

    Implements ICustomXMLSerialization
    Implements IScript

    Public Property ID As String = "" Implements IScript.ID
    Public Property Title As String = "" Implements IScript.Title
    Public Property ScriptText As String = "" Implements IScript.ScriptText
    Public Property LinkedObjectType As Interfaces.Enums.Scripts.ObjectType = Interfaces.Enums.Scripts.ObjectType.FlowsheetObject Implements IScript.LinkedObjectType
    Public Property LinkedObjectName As String = "" Implements IScript.LinkedObjectName
    Public Property LinkedEventType As Interfaces.Enums.Scripts.EventType = Interfaces.Enums.Scripts.EventType.SimulationOpened Implements IScript.LinkedEventType
    Public Property Linked As Boolean = False Implements IScript.Linked
    Public Property PythonInterpreter As Interfaces.Enums.Scripts.Interpreter = Interfaces.Enums.Scripts.Interpreter.IronPython Implements IScript.PythonInterpreter

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data)
        Return True
    End Function

    Public Function SaveData() As List(Of XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

End Class
