Public Interface ISimulationObject

    Property ErrorMessage() As String

    Function GetDebugReport() As String

    Sub AppendDebugLine(text As String)

    Property Calculated As Boolean

    Property DebugMode As Boolean

    Property DebugText As String

    <Xml.Serialization.XmlIgnore> Property CreatedWithThreadID As Integer

    <Xml.Serialization.XmlIgnore> Property LastUpdated As Date


End Interface
