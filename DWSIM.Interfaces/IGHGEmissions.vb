Public Interface IGHGComposition

    Property ID As String

    Property Name As String

    Property CarbonDioxide As Double

    Property Methane As Double

    Property NitrousOxide As Double

    Property Water As Double

End Interface

Public Interface IGHGEmitter

    Property GHGEmissionMassFlow As Double

    Property GHGEmissionMolarFlow As Double

    Property CO2eqEmissionMassFlow As Double

    Property CO2eqEmissionMolarFlow As Double

    Property GHGEmissionFactor As Double

    Property GHGEmissionCompositionID As String

    Function GetCompoundMassEmission(compound As String, units As String) As Double

    Function GetCompoundMolarEmission(compound As String, units As String) As Double

End Interface

Public Interface IGHGEmissionsSummary

    Property TotalGHGMassEmission As Double

    Property TotalGHGMolarEmission As Double

    Property TotalMethaneMassEmission As Double

    Property TotalMethaneMolarEmission As Double

    Property TotalCarbonDioxideMassEmission As Double

    Property TotalCarbonDioxideMolarEmission As Double

    Property TotalNitrousOxideMassEmission As Double

    Property TotalNitrousOxideMolarEmission As Double

    Property TotalWaterMassEmission As Double

    Property TotalWaterMolarEmission As Double

    Property TotalCO2eqMassEmission As Double

    Property TotalCO2eqMolarEmission As Double

End Interface