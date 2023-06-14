Imports FileHelpers
Imports System.IO

<DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()>
Public Class SetschenowCoefficient

    Public Property Name As String
    <FieldNullValue(0.0)> Public Property Coefficient As String

End Class

Public Class SetschenowCoefficients

    Private Property Maps As New Dictionary(Of String, SetschenowCoefficient)

    Public Sub New()

        Dim map() As SetschenowCoefficient

        Dim filestr As Stream = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.Thermodynamics.ReaktoroPropertyPackage.SetschenowCoefficients.txt")

        Dim fh1 As New FileHelperEngine(Of SetschenowCoefficient)

        Using t As New IO.StreamReader(filestr)
            map = fh1.ReadStream(t)
        End Using

        filestr.Dispose()

        Maps.Clear()

        For Each item In map
            If Not Maps.ContainsKey(item.Name) Then Maps.Add(item.Name, item)
        Next

    End Sub

    Public Function GetValue(Name As String) As Double
        If Maps.ContainsKey(Name.ToLower()) Then
            Return Maps(Name.ToLower()).Coefficient
        Else
            Return 0.0
        End If
    End Function

End Class
