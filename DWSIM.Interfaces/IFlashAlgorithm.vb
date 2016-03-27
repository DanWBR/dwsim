Public Interface IFlashCalculationResult

    Property BaseMoleAmount As Double
    Property Kvalues As List(Of Double)
    Property MixtureMoleAmounts As List(Of Double)
    Property VaporPhaseMoleAmounts As List(Of Double)
    Property LiquidPhase1MoleAmounts As List(Of Double)
    Property LiquidPhase2MoleAmounts As List(Of Double)
    Property SolidPhaseMoleAmounts As List(Of Double)
    Property CalculatedTemperature As Nullable(Of Double)
    Property CalculatedPressure As Nullable(Of Double)
    Property CalculatedEnthalpy As Nullable(Of Double)
    Property CalculatedEntropy As Nullable(Of Double)
    Property CompoundProperties As List(Of ICompoundConstantProperties)
    Property FlashAlgorithmType As String
    Property ResultException As Exception
    Property IterationsTaken As Integer
    Property TimeTaken As TimeSpan

    Function GetVaporPhaseMoleFractions() As Double()

    Function GetLiquidPhase1MoleFractions() As Double()

    Function GetLiquidPhase2MoleFractions() As Double()

    Function GetSolidPhaseMoleFractions() As Double()

    Function GetVaporPhaseMoleFraction() As Double

    Function GetLiquidPhase1MoleFraction() As Double

    Function GetLiquidPhase2MoleFraction() As Double

    Function GetSolidPhaseMoleFraction() As Double

    Function GetVaporPhaseMassFractions() As Double()

    Function GetLiquidPhase1MassFractions() As Double()

    Function GetLiquidPhase2MassFractions() As Double()

    Function GetSolidPhaseMassFractions() As Double()

    Function ConvertToMassFractions(ByVal Vz As Double()) As Double()

    Function CalcMolarWeight(ByVal Vz() As Double) As Double

    Function GetVaporPhaseMassFraction() As Double

    Function GetLiquidPhase1MassFraction() As Double

    Function GetLiquidPhase2MassFraction() As Double

    Function GetSolidPhaseMassFraction() As Double

End Interface