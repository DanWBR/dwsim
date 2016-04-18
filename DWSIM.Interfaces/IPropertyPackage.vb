Public Interface IPropertyPackage

    Property UniqueID As String

    ReadOnly Property Name As String

    Property Tag As String

    Property CurrentMaterialStream As IMaterialStream

    Function CalculateEquilibrium(calctype As Enums.FlashCalculationType,
                                            val1 As Double, val2 As Double,
                                            mixmolefrac As Double(),
                                            initialKval As Double(),
                                            initialestimate As Double) As IFlashCalculationResult

    Function CalculateEquilibrium2(calctype As Enums.FlashCalculationType,
                                            val1 As Double, val2 As Double,
                                            initialestimate As Double) As IFlashCalculationResult

    Function AUX_DELGig_RT(p1 As Double, T As Double, id As String(), stcoef As Double(), bcidx As Integer, Optional ByVal mode2 As Boolean = False) As Double

    Function AUX_CPm(phase As Enums.PhaseLabel, Ti As Double) As Double

    Function AUX_MMM(phase As Enums.PhaseLabel) As Double

End Interface
