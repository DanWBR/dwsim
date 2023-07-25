Imports FileHelpers
Imports System.IO

<DelimitedRecord(vbTab)> <IgnoreFirst()> <System.Serializable()>
Public Class CompoundMap

    Public Property Name As String
    <FieldNullValue("")> Public Property Formula As String
    <FieldNullValue("")> Public Property AqueousName As String
    <FieldNullValue("")> Public Property GaseousName As String
    <FieldNullValue("")> Public Property LiquidName As String

End Class

Public Class CompoundMapper

    Public Property Maps As New Dictionary(Of String, CompoundMap)

    Public Sub New()

        Dim map() As CompoundMap
        Dim fh1 As New FileHelperEngine(Of CompoundMap)

        Dim filestr As Stream = System.Reflection.Assembly.GetExecutingAssembly.GetManifestResourceStream("DWSIM.Thermodynamics.ReaktoroPropertyPackage.CompoundMaps.txt")

        Using t As New IO.StreamReader(filestr)
            map = fh1.ReadStream(t)
        End Using

        filestr.Dispose()

        Maps.Clear()

        For Each item In map
            Maps.Add(item.Name, item)
        Next

    End Sub

End Class
