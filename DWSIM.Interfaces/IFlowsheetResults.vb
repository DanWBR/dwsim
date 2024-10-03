Public Interface IFlowsheetResults
    Property GHGEmissionsSummary As IGHGEmissionsSummary

    Property TotalCAPEX As Double

    Property TotalOPEX As Double

    Property Additional As Dynamic.ExpandoObject

    Property ResidualMassBalance As Double

    Property TotalEnergyBalance As Double

End Interface
