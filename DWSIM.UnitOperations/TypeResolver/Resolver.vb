Public Module Resolver

    Public Function ReturnInstance(typename As String) As Object

        Dim t As Type = Type.GetType(typename, False)
        If Not t Is Nothing Then Return Activator.CreateInstance(t) Else Return Nothing

    End Function

End Module
