Imports DWSIM.Interfaces.Enums

Public Interface IScript
    Property ID As String
    Property Linked As Boolean
    Property LinkedEventType As Scripts.EventType
    Property LinkedObjectName As String
    Property LinkedObjectType As Scripts.ObjectType
    Property PythonInterpreter As Scripts.Interpreter
    Property ScriptText As String
    Property Title As String

End Interface
