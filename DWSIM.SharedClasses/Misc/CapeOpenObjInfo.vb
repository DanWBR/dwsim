<System.Serializable()> Public Class CapeOpenObjInfo

    Implements Interfaces.ICustomXMLSerialization

    Public TypeName As String = ""
    Public Version As String = ""
    Public Description As String = ""
    Public HelpURL As String = ""
    Public VendorURL As String = ""
    Public AboutInfo As String = ""
    Public CapeVersion As String = ""
    Public Location As String = ""
    Public Name As String = ""
    Public ImplementedCategory As String = ""

    Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
        XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        Return True
    End Function

    Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me, True)
    End Function

End Class
