Imports System.Reflection
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization

Public NotInheritable Class DeserializationBinder

    Inherits SerializationBinder

    Public Shared Property SatelliteAssemblies As New List(Of Assembly)

    Public Sub New()

        MyBase.New()

        If SatelliteAssemblies.Count = 0 Then
            SatelliteAssemblies = SharedClasses.Utility.LoadAdditionalPropertyPackageAssemblies()
        End If

    End Sub

    Public Overrides Function BindToType(assemblyName As String, typeName As String) As Type

        If Not String.IsNullOrEmpty(assemblyName) AndAlso Not String.IsNullOrEmpty(typeName) Then
            Dim typeToDeserialize As Type = Nothing
            If assemblyName.Contains("DWSIM.Thermodynamics") Then
                assemblyName = Assembly.GetExecutingAssembly().FullName
                typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
            Else
                Dim assemblies = Assembly.GetEntryAssembly().GetReferencedAssemblies().ToList()
                Dim a1 = (From a As AssemblyName In assemblies Where a.FullName.Contains(assemblyName)).FirstOrDefault
                If a1 IsNot Nothing Then
                    assemblyName = a1.FullName
                    typeToDeserialize = Type.[GetType]([String].Format("{0}, {1}", typeName, assemblyName))
                Else
                    Dim a2 = SatelliteAssemblies.Where(Function(a) a.FullName.Contains(assemblyName)).FirstOrDefault()
                    typeToDeserialize = a2.GetType(typeName)
                End If
            End If
            ' The following line of code returns the type. 
            Return typeToDeserialize
        End If
        Return Nothing

    End Function

End Class
