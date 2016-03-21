Imports CapeOpen

<System.Serializable()> Public Class CCapeCollection

    Implements ICapeCollection

    Public _icol As List(Of ICapeIdentification)

    Sub New()
        _icol = New List(Of ICapeIdentification)
    End Sub

    Public Function Count() As Integer Implements Global.CapeOpen.ICapeCollection.Count
        Return _icol.Count
    End Function

    Public Function Item(ByVal index As Object) As Object Implements Global.CapeOpen.ICapeCollection.Item
        If Integer.TryParse(index, New Integer) Then
            Return _icol(index - 1)
        Else
            For Each p As ICapeIdentification In _icol
                If p.ComponentName = index Then Return p Else Return Nothing
            Next
            Return Nothing
        End If
    End Function
End Class
