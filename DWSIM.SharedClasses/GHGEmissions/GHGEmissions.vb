Public Class GHGEmissionComposition

    Implements Interfaces.IGHGComposition, ICustomXMLSerialization

    Public Property ID As String = "" Implements IGHGComposition.ID

    Public Property Name As String = "" Implements IGHGComposition.Name

    Public Property CarbonDioxide As Double Implements IGHGComposition.CarbonDioxide

    Public Property Methane As Double Implements IGHGComposition.Methane

    Public Property NitrousOxide As Double Implements IGHGComposition.NitrousOxide

    Public Property Water As Double Implements IGHGComposition.Water

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

    Public Function GetMolecularWeight() As Double Implements IGHGComposition.GetMolecularWeight

        Return CarbonDioxide * 44.01 + Methane * 16.04 + Water * 18.0 + NitrousOxide * 44.013

    End Function

    Public Function GetDryMolecularWeight() As Double Implements IGHGComposition.GetDryMolecularWeight

        Dim c1 = CarbonDioxide / (CarbonDioxide + Methane + NitrousOxide)
        Dim c2 = Methane / (CarbonDioxide + Methane + NitrousOxide)
        Dim c3 = NitrousOxide / (CarbonDioxide + Methane + NitrousOxide)

        Return c1 * 44.01 + c2 * 16.04 + c3 * 44.013

    End Function

End Class