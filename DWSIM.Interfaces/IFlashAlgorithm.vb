'    DWSIM Interface definitions
'    Copyright 2010-2017 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

''' <summary>
''' This interface defines the basic properties of a flash algorithm, including an instance of the class which contains its current settings.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlashAlgorithm

    Property FlashSettings As Dictionary(Of Enums.FlashSetting, String)

    Function Clone() As IFlashAlgorithm

    Function GetNewInstance() As IFlashAlgorithm

    ReadOnly Property AlgoType As Enums.FlashMethod

    ReadOnly Property Name As String

    ReadOnly Property Description As String

    Property Tag As String

    ReadOnly Property InternalUseOnly As Boolean

    ReadOnly Property MobileCompatible As Boolean

    Property Order As Integer

End Interface

''' <summary>
''' This interface defines the parameters of a flash calculation result.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IFlashCalculationResult

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
