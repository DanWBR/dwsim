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

Imports DWSIM.Interfaces.Enums
''' <summary>
''' This interface defines the basic properties of a System of Units class.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IUnitsOfMeasure

    Function GetUnitSet(measureID As Enums.UnitOfMeasure) As List(Of String)

    Function GetCurrentUnits(measureID As Enums.UnitOfMeasure) As String
    Property Name As String

    Property mass As String

    Property reac_rate_heterog As String

    Property area As String

    Property conductance As String

    Property distance As String

    Property time As String

    Property volume As String

    Property molar_volume As String

    Property diameter As String

    Property thickness As String

    Property molar_conc As String

    Property mass_conc As String

    Property heat_transf_coeff As String

    Property force As String

    Property accel As String

    Property spec_vol As String

    Property reac_rate As String

    Property velocity As String

    Property foulingfactor As String

    Property cakeresistance As String

    Property mediumresistance As String

    Property gor As String

    Property molar_enthalpy As String

    Property molar_entropy As String

    Property idealGasHeatCapacity As String

    Property thermalConductivityOfLiquid As String

    Property thermalConductivityOfVapor As String

    Property vaporPressure As String

    Property viscosityOfLiquid As String

    Property viscosityOfVapor As String

    Property pdp_boilingPointTemperature As String

    Property pdp_meltingTemperature As String

    Property activity As String

    Property activityCoefficient As String

    Property compressibility As String

    Property compressibilityFactor As String

    Property density As String

    Property enthalpy As String

    Property entropy As String

    Property excessEnthalpy As String

    Property excessEntropy As String

    Property molarflow As String

    Property massflow As String

    Property molarfraction As String

    Property massfraction As String

    Property fugacity As String

    Property fugacityCoefficient As String

    Property heatCapacityCp As String

    Property heatCapacityCv As String

    Property jouleThomsonCoefficient As String

    Property logFugacityCoefficient As String

    Property molecularWeight As String

    Property pressure As String

    Property temperature As String

    Property speedOfSound As String

    Property thermalConductivity As String

    Property viscosity As String

    Property cinematic_viscosity As String

    Property volumetricFlow As String

    Property heatflow As String

    Property head As String

    Property deltaT As String

    Property deltaP As String

    Property kvalue As String

    Property logKvalue As String

    Property surfaceTension As String

    Property diffusivity As String

    Property heat As String

    Property mole As String

    Function GetUnitType(unit As String) As UnitOfMeasure

    Property emission_factor As String

End Interface
