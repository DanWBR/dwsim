<System.Serializable>
Public Class TabularData

    Implements ITabularData, ICustomXMLSerialization

    Public Property XName As String = "" Implements ITabularData.XName

    Public Property YName As String = "" Implements ITabularData.YName

    Public Property XUnit As String = "" Implements ITabularData.XUnit

    Public Property YUnit As String = "" Implements ITabularData.YUnit

    Public Property XData As New List(Of Double) Implements ITabularData.XData

    Public Property YData As New List(Of Double) Implements ITabularData.YData

    Public Property Source As String = "" Implements ITabularData.Source

    Public Property Comments As String = "" Implements ITabularData.Comments

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

End Class
