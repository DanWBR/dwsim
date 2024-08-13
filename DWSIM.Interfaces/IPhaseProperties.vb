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

<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface IPhaseProperties

    Property osmoticCoefficient As Nullable(Of Double)
    Property freezingPointDepression As Nullable(Of Double)
    Property freezingPoint As Nullable(Of Double)
    Property ionicStrength As Nullable(Of Double)
    Property pH As Nullable(Of Double)
    Property dewTemperature As Nullable(Of Double)
    Property dewPressure As Nullable(Of Double)
    Property bubbleTemperature As Nullable(Of Double)
    Property bubblePressure As Nullable(Of Double)
    Property activity As Nullable(Of Double)
    Property activityCoefficient As Nullable(Of Double)
    Property compressibility As Nullable(Of Double)
    Property compressibilityFactor As Nullable(Of Double)
    Property density As Nullable(Of Double)
    Property enthalpy As Nullable(Of Double)
    Property entropy As Nullable(Of Double)
    Property enthalpyF As Nullable(Of Double)
    Property entropyF As Nullable(Of Double)
    Property excessEnthalpy As Nullable(Of Double)
    Property excessEntropy As Nullable(Of Double)
    Property molarflow As Nullable(Of Double)
    Property massflow As Nullable(Of Double)
    Property molarfraction As Nullable(Of Double)
    Property massfraction As Nullable(Of Double)
    Property fugacity As Nullable(Of Double)
    Property fugacityCoefficient As Nullable(Of Double)
    Property heatCapacityCp As Nullable(Of Double)
    Property heatCapacityCv As Nullable(Of Double)
    Property jouleThomsonCoefficient As Nullable(Of Double)
    Property logFugacityCoefficient As Nullable(Of Double)
    Property molecularWeight As Nullable(Of Double)
    Property pressure As Nullable(Of Double)
    Property temperature As Nullable(Of Double)
    Property speedOfSound As Nullable(Of Double)
    Property thermalConductivity As Nullable(Of Double)
    Property viscosity As Nullable(Of Double)
    Property kinematic_viscosity As Nullable(Of Double)
    Property volumetric_flow As Nullable(Of Double)
    Property molar_enthalpy As Nullable(Of Double)
    Property molar_entropy As Nullable(Of Double)
    Property molar_enthalpyF As Nullable(Of Double)
    Property molar_entropyF As Nullable(Of Double)
    Property kvalue As Nullable(Of Double)
    Property logKvalue As Nullable(Of Double)
    Property surfaceTension As Nullable(Of Double)
    Property internal_energy As Nullable(Of Double)
    Property molar_internal_energy As Nullable(Of Double)
    Property gibbs_free_energy As Nullable(Of Double)
    Property molar_gibbs_free_energy As Nullable(Of Double)
    Property helmholtz_energy As Nullable(Of Double)
    Property molar_helmholtz_energy As Nullable(Of Double)
    Property bulk_modulus As Nullable(Of Double)
    Property isothermal_compressibility As Nullable(Of Double)
    Property mean_ionic_acitivty_coefficient As Double?
    Property idealGasHeatCapacityCp As Double?
    Property idealGasHeatCapacityRatio As Double?
    Property CO2loading As Double?
    Property CO2partialpressure As Double?
    Property H2Sloading As Double?
    Property H2Spartialpressure As Double?
    Property particleSize_Mean As Double?
    Property particleSize_StdDev As Double?

End Interface
