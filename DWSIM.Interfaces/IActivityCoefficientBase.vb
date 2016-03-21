Public Interface IActivityCoefficientBase

    Function CalcActivityCoefficients(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Array

    Function CalcExcessEnthalpy(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Double

    Function CalcExcessHeatCapacity(ByVal T As Double, ByVal Vx As Array, ByVal otherargs As Object) As Double

End Interface
