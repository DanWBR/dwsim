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
''' This interface defines the basic Property Package funcionality to be exposed for communication between the various DWSIM components.
''' The actual Property Package class exposes much more funcionality through the implementation of other interfaces, including CAPE-OPEN ones.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IPropertyPackage

    Property UniqueID As String

    ReadOnly Property Name As String

    Property Tag As String

    Property FlashAlgorithm As IFlashAlgorithm

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

    Function AUX_Z(Vx As Double(), T As Double, P As Double, state As Enums.PhaseName) As Double

    Function Clone() As IPropertyPackage

    Property Flowsheet As IFlowsheet

    Sub DisplayEditingForm()

    Function DisplayAdvancedEditingForm() As Object

    Sub CalcAdditionalPhaseProperties()

    ReadOnly Property MobileCompatible As Boolean

    ReadOnly Property IsFunctional As Boolean

    ReadOnly Property ShouldUseKvalueMethod2 As Boolean

    Function ReturnInstance(typename As String) As Object

    Sub DisplayGroupedEditingForm()

    ReadOnly Property DisplayName As String

    ReadOnly Property DisplayDescription As String
    ReadOnly Property HasReactivePhase As Boolean
    ReadOnly Property ShouldUseKvalueMethod3 As Boolean
    Function GetAsObject() As Object

End Interface

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IPhaseEnvelopeOptions

    Property QualityLine As Boolean
    Property QualityValue As Double
    Property StabilityCurve As Boolean
    Property PhaseIdentificationCurve As Boolean
    Property CheckLiquidInstability As Boolean
    Property Hydrate As Boolean
    Property HydrateModel As Integer
    Property HydrateVaporOnly As Boolean
    Property OperatingPoint As Boolean

    Property BubbleCurveInitialFlash As String
    Property BubbleCurveInitialPressure As Double
    Property BubbleCurveInitialTemperature As Double
    Property BubbleCurveMaximumTemperature As Double

    Property DewCurveInitialFlash As String
    Property DewCurveInitialPressure As Double
    Property DewCurveInitialTemperature As Double
    Property DewCurveMaximumTemperature As Double

    Property BubbleCurveMaximumPoints As Integer
    Property DewCurveMaximumPoints As Integer

    Property BubbleCurveDeltaP As Double
    Property BubbleCurveDeltaT As Double

    Property DewCurveDeltaP As Double
    Property DewCurveDeltaT As Double

    Property BubbleUseCustomParameters As Boolean
    Property DewUseCustomParameters As Boolean

    Property ImmiscibleWater As Boolean


End Interface
