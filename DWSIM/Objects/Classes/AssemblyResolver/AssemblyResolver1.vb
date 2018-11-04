Imports System.Reflection
Imports System.Runtime.Serialization

Public NotInheritable Class VersionDeserializationBinder

    Inherits SerializationBinder

    Public Overrides Function BindToType(assemblyName As String, typeName As String) As Type
        If Not String.IsNullOrEmpty(assemblyName) AndAlso Not String.IsNullOrEmpty(typeName) Then
            Dim typeToDeserialize As Type = Nothing
            If assemblyName.Contains("DWSIM,") Then
                assemblyName = Assembly.GetExecutingAssembly().FullName
                typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
            Else
                typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
            End If
            Return typeToDeserialize
        End If
        Return Nothing
    End Function

End Class
