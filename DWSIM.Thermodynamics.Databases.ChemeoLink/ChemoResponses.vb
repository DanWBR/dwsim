' based on https://www.chemeo.com/api/v1/
Class SearchResponse
    Public Property Compounds() As IEnumerable(Of Compound)
End Class


Class Compound
    Public Property Id() As String
    Public Property Cas() As String
    Public Property Compound() As String
End Class
