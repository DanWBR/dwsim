Public Class GHGEmitter

    Implements Interfaces.IGHGEmitter, ICustomXMLSerialization

    Private Const MethaneCO2eq As Double = 28.0

    Private Const NitrousOxideCO2eq As Double = 256.0

    Public Property GHGEmissionMassFlow As Double Implements IGHGEmitter.GHGEmissionMassFlow

    Public Property GHGEmissionMolarFlow As Double Implements IGHGEmitter.GHGEmissionMolarFlow

    Public Property CO2eqEmissionMassFlow As Double Implements IGHGEmitter.CO2eqEmissionMassFlow

    Public Property CO2eqEmissionMolarFlow As Double Implements IGHGEmitter.CO2eqEmissionMolarFlow

    Public Property GHGEmissionFactor As Double Implements IGHGEmitter.GHGEmissionFactor

    Public Property GHGEmissionCompositionID As String = "" Implements IGHGEmitter.GHGEmissionCompositionID

    Public Function GetCompoundMassEmission(compound As String, units As String) As Double Implements IGHGEmitter.GetCompoundMassEmission

    End Function

    Public Function GetCompoundMolarEmission(compound As String, units As String) As Double Implements IGHGEmitter.GetCompoundMolarEmission

    End Function

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

End Class

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

End Class

Public Class GHGEmissionsSummary

    Implements IGHGEmissionsSummary, ICustomXMLSerialization

    Public Property TotalGHGMassEmission As Double Implements IGHGEmissionsSummary.TotalGHGMassEmission

    Public Property TotalGHGMolarEmission As Double Implements IGHGEmissionsSummary.TotalGHGMolarEmission

    Public Property TotalMethaneMassEmission As Double Implements IGHGEmissionsSummary.TotalMethaneMassEmission

    Public Property TotalMethaneMolarEmission As Double Implements IGHGEmissionsSummary.TotalMethaneMolarEmission

    Public Property TotalCarbonDioxideMassEmission As Double Implements IGHGEmissionsSummary.TotalCarbonDioxideMassEmission

    Public Property TotalCarbonDioxideMolarEmission As Double Implements IGHGEmissionsSummary.TotalCarbonDioxideMolarEmission

    Public Property TotalNitrousOxideMassEmission As Double Implements IGHGEmissionsSummary.TotalNitrousOxideMassEmission

    Public Property TotalNitrousOxideMolarEmission As Double Implements IGHGEmissionsSummary.TotalNitrousOxideMolarEmission

    Public Property TotalWaterMassEmission As Double Implements IGHGEmissionsSummary.TotalWaterMassEmission

    Public Property TotalWaterMolarEmission As Double Implements IGHGEmissionsSummary.TotalWaterMolarEmission

    Public Property TotalCO2eqMassEmission As Double Implements IGHGEmissionsSummary.TotalCO2eqMassEmission

    Public Property TotalCO2eqMolarEmission As Double Implements IGHGEmissionsSummary.TotalCO2eqMolarEmission

    Public Function SaveData() As List(Of XElement) Implements ICustomXMLSerialization.SaveData
        Return XMLSerializer.XMLSerializer.Serialize(Me)
    End Function

    Public Function LoadData(data As List(Of XElement)) As Boolean Implements ICustomXMLSerialization.LoadData
        Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
    End Function

End Class
