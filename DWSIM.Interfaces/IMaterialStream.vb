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
''' This is the interface which defines the basic properties of a Material Stream.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IMaterialStream

    Property ForcePhase As Enums.ForcedPhase

    Property SpecType As Enums.StreamSpec

    Property DefinedFlow As Enums.FlowSpec

    Property IsElectrolyteStream As Boolean

    Property ReferenceSolvent As String

    Property InputComposition As Dictionary(Of String, Double)

    Property CompositionBasis As Enums.CompositionBasis

    ReadOnly Property Phases() As Dictionary(Of Integer, IPhase)

    ReadOnly Property PhasesArray As IPhase()

    Function GetPhase(ByVal phasename As String) As IPhase

    Property AtEquilibrium As Boolean

    Sub SetPhaseComposition(Vx As Array, phs As Integer)

    Sub SetOverallComposition(Vx As Array)

    Sub SetOverallMassComposition(Vx As Double())

    Function GetPhaseComposition(phs As Integer) As Double()

    Function GetOverallComposition() As Double()

    Function GetOverallMassComposition() As Double()

    Function Clone() As IMaterialStream

    ReadOnly Property Flowsheet As IFlowsheet

    Sub Validate()

    Sub ClearAllProps()

    Function GetPropertyPackageObject() As Object

    Function GetPropertyPackageObjectCopy() As Object

    Sub SetPropertyPackageObject(pp As Object)

    Sub SetCurrentMaterialStream(ms As Object)

    Property FloatingTableAmountBasis As Enums.CompositionBasis

    Function GetOverallHeatOfFormation() As Double

    Function GetTemperature() As Double

    Function GetPressure() As Double

    Function GetMassFlow() As Double

    Function GetMolarFlow() As Double

    Function GetVolumetricFlow() As Double

    Function GetMassEnthalpy() As Double

    Function GetEnergyFlow() As Double

    Function GetCompoundMassFlow(name As String) As Double

    Function GetCompoundMassConcentration(name As String) As Double

    Function SetTemperature(value As Double) As String

    Function SetPressure(value As Double) As String

    Function SetMassFlow(value As Double) As String

    Function SetMolarFlow(value As Double) As String

    Function SetVolumetricFlow(value As Double) As String

    Function SetMassEnthalpy(value As Double) As String

    Function GetCompoundMolarFlow(name As String) As Double

    Sub SetOverallMolarComposition(Vx() As Double)

    Property SolidParticleData As ISolidParticleData

    Property AdditionalSolidPhaseProperties As IAdditionalSolidPhaseProperties

End Interface
