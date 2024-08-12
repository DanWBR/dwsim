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
''' This interface defines the constant properties of a compound.
''' </summary>
<InterfaceType(ComInterfaceType.InterfaceIsIDispatch)> Public Interface ICompoundConstantProperties

    ''' <summary>
    ''' Dynamically added properties.
    ''' </summary>
    ''' <returns></returns>
    Property ExtraProperties As Dynamic.ExpandoObject

    ''' <summary>
    ''' Name of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property Name As String

    ''' <summary>
    ''' CAS Registry Number® of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property CAS_Number As String

    ''' <summary>
    ''' Chemical formula of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property Formula As String

    ''' <summary>
    ''' The simplified molecular-input line-entry system (SMILES) of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property SMILES As String

    ''' <summary>
    ''' The IUPAC International Chemical Identifier (InChI) of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property InChI As String

    ''' <summary>
    ''' Chemical structure of this compound.
    ''' </summary>
    ''' <returns></returns>
    Property ChemicalStructure As String

    ''' <summary>
    ''' Molecular weight in kg/kmol
    ''' </summary>
    ''' <returns></returns>
    Property Molar_Weight As Double

    ''' <summary>
    ''' Critical temperature in K
    ''' </summary>
    ''' <returns></returns>
    Property Critical_Temperature As Double

    ''' <summary>
    ''' Critical pressure in Pa
    ''' </summary>
    ''' <returns></returns>
    Property Critical_Pressure As Double

    ''' <summary>
    ''' Critical volume in m3/kmol
    ''' </summary>
    ''' <returns></returns>
    Property Critical_Volume As Double

    ''' <summary>
    ''' Critical compressibility
    ''' </summary>
    ''' <returns></returns>
    Property Critical_Compressibility As Double

    ''' <summary>
    ''' Acentric factor
    ''' </summary>
    ''' <returns></returns>
    Property Acentric_Factor As Double

    ''' <summary>
    ''' Rackett compressibility factor
    ''' </summary>
    ''' <returns></returns>
    Property Z_Rackett As Double

    ''' <summary>
    ''' PR EOS Volume translation coefficient
    ''' </summary>
    ''' <returns></returns>
    Property PR_Volume_Translation_Coefficient As Double

    ''' <summary>
    ''' SRK EOS Volume translation coefficient
    ''' </summary>
    ''' <returns></returns>
    Property SRK_Volume_Translation_Coefficient As Double

    ''' <summary>
    ''' Chao-Seader acentric factor
    ''' </summary>
    ''' <returns></returns>
    Property Chao_Seader_Acentricity As Double

    ''' <summary>
    ''' Chao Seader solubility parameter in (cal/mL)^0.5
    ''' </summary>
    ''' <returns></returns>
    Property Chao_Seader_Solubility_Parameter As Double

    ''' <summary>
    ''' Chao-Seader liquid molar volume in mL/mol
    ''' </summary>
    ''' <returns></returns>
    Property Chao_Seader_Liquid_Molar_Volume As Double

    ''' <summary>
    ''' Ideal Gas entropy of formation in kJ/[kg.K]
    ''' </summary>
    ''' <returns></returns>
    Property IG_Entropy_of_Formation_25C As Double

    ''' <summary>
    ''' Ideal Gas enthalpy of formation in kJ/kg
    ''' </summary>
    ''' <returns></returns>
    Property IG_Enthalpy_of_Formation_25C As Double

    ''' <summary>
    ''' Ideal Gas gibbs energy of formation in kJ/kg
    ''' </summary>
    ''' <returns></returns>
    Property IG_Gibbs_Energy_of_Formation_25C As Double

    ''' <summary>
    ''' Dipole moment in coloumb.m
    ''' </summary>
    ''' <returns></returns>
    Property Dipole_Moment As Double

    ''' <summary>
    ''' Vapor pressure equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_Constant_A As Double

    ''' <summary>
    ''' Vapor pressure equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_Constant_B As Double

    ''' <summary>
    ''' Vapor pressure equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_Constant_C As Double

    ''' <summary>
    ''' Vapor pressure equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_Constant_D As Double

    ''' <summary>
    ''' Vapor pressure equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_Constant_E As Double

    ''' <summary>
    ''' Vapor pressure equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_TMIN As Double

    ''' <summary>
    ''' Vapor pressure equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Pressure_TMAX As Double

    Property Vapor_Pressure_Regression_Fit As Double

    Property Vapor_Pressure_Tabular_Data As ITabularData

    ''' <summary>
    ''' Ideal gas heat capacity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Ideal_Gas_Heat_Capacity_Const_A As Double

    ''' <summary>
    ''' Ideal gas heat capacity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Ideal_Gas_Heat_Capacity_Const_B As Double

    ''' <summary>
    ''' Ideal gas heat capacity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Ideal_Gas_Heat_Capacity_Const_C As Double

    ''' <summary>
    ''' Ideal gas heat capacity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Ideal_Gas_Heat_Capacity_Const_D As Double

    ''' <summary>
    ''' Ideal gas heat capacity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Ideal_Gas_Heat_Capacity_Const_E As Double

    Property Ideal_Gas_Heat_Capacity_Regression_Fit As Double

    Property Ideal_Gas_Heat_Capacity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Liquid viscosity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Viscosity_Const_A As Double

    ''' <summary>
    ''' Liquid viscosity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Viscosity_Const_B As Double

    ''' <summary>
    ''' Liquid viscosity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Viscosity_Const_C As Double

    ''' <summary>
    ''' Liquid viscosity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Viscosity_Const_D As Double

    ''' <summary>
    ''' Liquid viscosity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Viscosity_Const_E As Double

    Property Liquid_Viscosity_Regression_Fit As Double

    Property Liquid_Viscosity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Liquid density equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Const_A As Double

    ''' <summary>
    ''' Liquid density equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Const_B As Double

    ''' <summary>
    ''' Liquid density equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Const_C As Double

    ''' <summary>
    ''' Liquid density equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Const_D As Double

    ''' <summary>
    ''' Liquid density equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Const_E As Double

    ''' <summary>
    ''' Liquid density equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Tmin As Double

    ''' <summary>
    ''' Liquid density equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Density_Tmax As Double

    Property Liquid_Density_Regression_Fit As Double

    Property Liquid_Density_Tabular_Data As ITabularData

    ''' <summary>
    ''' Liquid heat capacity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Const_A As Double

    ''' <summary>
    ''' Liquid heat capacity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Const_B As Double

    ''' <summary>
    ''' Liquid heat capacity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Const_C As Double

    ''' <summary>
    ''' Liquid heat capacity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Const_D As Double

    ''' <summary>
    ''' Liquid heat capacity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Const_E As Double

    ''' <summary>
    ''' Liquid heat capacity minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Tmin As Double

    ''' <summary>
    ''' Liquid heat capacity maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Heat_Capacity_Tmax As Double

    Property Liquid_Heat_Capacity_Regression_Fit As Double

    Property Liquid_Heat_Capacity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Liquid thermal conductivity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Const_A As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Const_B As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Const_C As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Const_D As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Const_E As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Tmin As Double

    ''' <summary>
    ''' Liquid thermal conductivity equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Liquid_Thermal_Conductivity_Tmax As Double

    Property Liquid_Thermal_Conductivity_Regression_Fit As Double

    Property Liquid_Thermal_Conductivity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Vapor thermal conductivity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Const_A As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Const_B As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Const_C As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Const_D As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Const_E As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Tmin As Double

    ''' <summary>
    ''' Vapor thermal conductivity equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Thermal_Conductivity_Tmax As Double

    Property Vapor_Thermal_Conductivity_Regression_Fit As Double

    Property Vapor_Thermal_Conductivity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Vapor viscosity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Const_A As Double

    ''' <summary>
    ''' Vapor viscosity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Const_B As Double

    ''' <summary>
    ''' Vapor viscosity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Const_C As Double

    ''' <summary>
    ''' Vapor viscosity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Const_D As Double

    ''' <summary>
    ''' Vapor viscosity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Const_E As Double

    ''' <summary>
    ''' Vapor viscosity equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Tmin As Double

    ''' <summary>
    ''' Vapor viscosity equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Vapor_Viscosity_Tmax As Double

    Property Vapor_Viscosity_Regression_Fit As Double

    Property Vapor_Viscosity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Solid density equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Const_A As Double

    ''' <summary>
    ''' Solid density equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Const_B As Double

    ''' <summary>
    ''' Solid density equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Const_C As Double

    ''' <summary>
    ''' Solid density equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Const_D As Double

    ''' <summary>
    ''' Solid density equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Const_E As Double

    ''' <summary>
    ''' Solid density equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Tmin As Double

    ''' <summary>
    ''' Solid density equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Density_Tmax As Double

    Property Solid_Density_Regression_Fit As Double

    Property Solid_Density_Tabular_Data As ITabularData

    ''' <summary>
    ''' Surface tension equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Const_A As Double

    ''' <summary>
    ''' Surface tension equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Const_B As Double

    ''' <summary>
    ''' Surface tension equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Const_C As Double

    ''' <summary>
    ''' Surface tension equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Const_D As Double

    ''' <summary>
    ''' Surface tension equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Const_E As Double

    ''' <summary>
    ''' Surface tension equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Tmin As Double

    ''' <summary>
    ''' Surface tension equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Surface_Tension_Tmax As Double

    Property Surface_Tension_Regression_Fit As Double

    Property Surface_Tension_Tabular_Data As ITabularData

    ''' <summary>
    ''' Solid heat capacity equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Const_A As Double

    ''' <summary>
    ''' Solid heat capacity equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Const_B As Double

    ''' <summary>
    ''' Solid heat capacity equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Const_C As Double

    ''' <summary>
    ''' Solid heat capacity equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Const_D As Double

    ''' <summary>
    ''' Solid heat capacity equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Const_E As Double

    ''' <summary>
    ''' Solid heat capacity equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Tmin As Double

    ''' <summary>
    ''' Solid heat capacity equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property Solid_Heat_Capacity_Tmax As Double

    Property Solid_Heat_Capacity_Regression_Fit As Double

    Property Solid_Heat_Capacity_Tabular_Data As ITabularData

    ''' <summary>
    ''' Normal boiling point in K
    ''' </summary>
    ''' <returns></returns>
    Property Normal_Boiling_Point As Double

    ''' <summary>
    ''' Unique ID for this compound
    ''' </summary>
    ''' <returns></returns>
    Property ID As Integer

    ''' <summary>
    ''' True if this compound is a petroleum fraction (pseudocompound)
    ''' </summary>
    ''' <returns></returns>
    Property IsPF As Integer

    ''' <summary>
    '''  Deprecated
    ''' </summary>
    ''' <returns></returns>
    Property IsHYPO As Integer

    ''' <summary>
    ''' Heat of vaporization equation A constant
    ''' </summary>
    ''' <returns></returns>
    Property HVap_A As Double

    ''' <summary>
    ''' Heat of vaporization equation B constant
    ''' </summary>
    ''' <returns></returns>
    Property HVap_B As Double

    ''' <summary>
    ''' Heat of vaporization equation C constant
    ''' </summary>
    ''' <returns></returns>
    Property HVap_C As Double

    ''' <summary>
    ''' Heat of vaporization equation D constant
    ''' </summary>
    ''' <returns></returns>
    Property HVap_D As Double

    ''' <summary>
    ''' Heat of vaporization equation E constant
    ''' </summary>
    ''' <returns></returns>
    Property HVap_E As Double

    ''' <summary>
    ''' Heat of vaporization equation minimum temperature
    ''' </summary>
    ''' <returns></returns>
    Property HVap_TMIN As Double

    ''' <summary>
    ''' Heat of vaporization equation maximum temperature
    ''' </summary>
    ''' <returns></returns>
    Property HVap_TMAX As Double

    Property Enthalpy_Of_Vaporization_Regression_Fit As Double

    Property Enthalpy_Of_Vaporization_Tabular_Data As ITabularData

    ''' <summary>
    ''' UNIQUAC R parameter
    ''' </summary>
    ''' <returns></returns>
    Property UNIQUAC_R As Double

    ''' <summary>
    ''' UNIQUAC Q parameter
    ''' </summary>
    ''' <returns></returns>
    Property UNIQUAC_Q As Double

    ''' <summary>
    ''' Deprecated
    ''' </summary>
    ''' <returns></returns>
    Property IsFPROPSSupported As Boolean

    ''' <summary>
    ''' True if this compound is supported by the CoolProp Property Package
    ''' </summary>
    ''' <returns></returns>
    Property IsCOOLPROPSupported As Boolean

    ''' <summary>
    ''' True if the data for this compound was modified in memory (when added to a simulation)
    ''' </summary>
    ''' <returns></returns>
    Property IsModified As Boolean

    ''' <summary>
    ''' Vapor pressure equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property VaporPressureEquation As String

    ''' <summary>
    ''' Ideal Gas Cp equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property IdealgasCpEquation As String

    ''' <summary>
    ''' Liquid viscosity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property LiquidViscosityEquation As String

    ''' <summary>
    ''' Vapor viscosity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property VaporViscosityEquation As String

    ''' <summary>
    ''' Heat of vaporization equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property VaporizationEnthalpyEquation As String

    ''' <summary>
    ''' Liquid density equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property LiquidDensityEquation As String

    ''' <summary>
    ''' Liquid heat capacity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property LiquidHeatCapacityEquation As String

    ''' <summary>
    ''' Liquid thermal conductivity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property LiquidThermalConductivityEquation As String

    ''' <summary>
    ''' Vapor thermal conductivity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property VaporThermalConductivityEquation As String

    ''' <summary>
    ''' Solid density equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property SolidDensityEquation As String

    ''' <summary>
    ''' Solid heat capacity equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property SolidHeatCapacityEquation As String

    ''' <summary>
    ''' Surface tension equation number or expression
    ''' </summary>
    ''' <returns></returns>
    Property SurfaceTensionEquation As String

    ''' <summary>
    ''' Deprecated
    ''' </summary>
    ''' <returns></returns>
    Property PC_SAFT_sigma As Double

    ''' <summary>
    ''' Deprecated
    ''' </summary>
    ''' <returns></returns>
    Property PC_SAFT_epsilon_k As Double

    ''' <summary>
    ''' Deprecated
    ''' </summary>
    ''' <returns></returns>
    Property PC_SAFT_m As Double

    ''' <summary>
    ''' Molecular weight of the petroleum fraction (redundant, deprecated)
    ''' </summary>
    ''' <returns></returns>
    Property PF_MM As Nullable(Of Double)

    ''' <summary>
    ''' Normal boiling point in K (redundant, deprecated)
    ''' </summary>
    ''' <returns></returns>
    Property NBP As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction viscosity A parameter
    ''' </summary>
    ''' <returns></returns>
    Property PF_vA As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction viscosity B parameter
    ''' </summary>
    ''' <returns></returns>
    Property PF_vB As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction Watson K parameter
    ''' </summary>
    ''' <returns></returns>
    Property PF_Watson_K As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction specific gravity
    ''' </summary>
    ''' <returns></returns>
    Property PF_SG As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction viscosity value 1
    ''' </summary>
    ''' <returns></returns>
    Property PF_v1 As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction temperature for viscosity value 1 in K
    ''' </summary>
    ''' <returns></returns>
    Property PF_Tv1 As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction viscosity value 2
    ''' </summary>
    ''' <returns></returns>
    Property PF_v2 As Nullable(Of Double)

    ''' <summary>
    ''' Petroleum fraction temperature for viscosity value 1 in K
    ''' </summary>
    ''' <returns></returns>
    Property PF_Tv2 As Nullable(Of Double)

    'Databases: 'DWSIM', 'CheResources', 'ChemSep' OR 'User'
    'User databases are XML-serialized versions of this base class, and they may include hypos and pseudos.

    ''' <summary>
    ''' Databases: 'DWSIM', 'CheResources', 'Biodiesel', 'ChemSep', 'CheDL_Thermo' OR 'User'
    ''' </summary>
    ''' <returns></returns>
    Property OriginalDB As String

    ''' <summary>
    ''' Same as Original_DB
    ''' </summary>
    ''' <returns></returns>
    Property CurrentDB As String

    ''' <summary>
    ''' If this is an user compound, this will store a reference to the compound creator study file path
    ''' </summary>
    ''' <returns></returns>
    Property CompCreatorStudyFile As String

    ''' <summary>
    ''' COSMO-SAC's database equivalent name (deprecated)
    ''' </summary>
    ''' <returns></returns>
    Property COSMODBName

    'Electrolyte-related properties

    ''' <summary>
    ''' True if this compound is an ion
    ''' </summary>
    ''' <returns></returns>
    Property IsIon As Boolean

    ''' <summary>
    ''' True if this compound is a salt
    ''' </summary>
    ''' <returns></returns>
    Property IsSalt As Boolean

    ''' <summary>
    ''' True if this compound is a hydrated salt
    ''' </summary>
    ''' <returns></returns>
    Property IsHydratedSalt As Boolean

    ''' <summary>
    ''' Hydration number, if hydrated salt
    ''' </summary>
    ''' <returns></returns>
    Property HydrationNumber As Double

    ''' <summary>
    ''' Charge, if ion
    ''' </summary>
    ''' <returns></returns>
    Property Charge As Integer

    ''' <summary>
    ''' Positive ion formula
    ''' </summary>
    ''' <returns></returns>
    Property PositiveIon As String

    ''' <summary>
    ''' Negative ion formula
    ''' </summary>
    ''' <returns></returns>
    Property NegativeIon As String

    ''' <summary>
    ''' Positive ion stoichiometric coefficient, if salt
    ''' </summary>
    ''' <returns></returns>
    Property PositiveIonStoichCoeff As Integer

    ''' <summary>
    ''' Negative ion stoichiometric coefficient, if salt
    ''' </summary>
    ''' <returns></returns>
    Property NegativeIonStoichCoeff As Integer

    ''' <summary>
    ''' Sum of stoichiometric coefficients
    ''' </summary>
    ''' <returns></returns>
    Property StoichSum As Integer

    ''' <summary>
    ''' Electrolyte Gibbs energy of formation in kJ/mol
    ''' </summary>
    ''' <returns></returns>
    Property Electrolyte_DelGF As Double

    ''' <summary>
    ''' Electrolyte enthalpy of formation in kJ/mol
    ''' </summary>
    ''' <returns></returns>
    Property Electrolyte_DelHF As Double

    ''' <summary>
    ''' Electrolyte standard heat capacity, kJ/mol.K
    ''' </summary>
    ''' <returns></returns>
    Property Electrolyte_Cp0 As Double

    ''' <summary>
    ''' Temperature of fusion in K
    ''' </summary>
    ''' <returns></returns>
    Property TemperatureOfFusion As Double

    ''' <summary>
    ''' Enthalpy of fusion, kJ/mol
    ''' </summary>
    ''' <returns></returns>
    Property EnthalpyOfFusionAtTf As Double

    ''' <summary>
    ''' Temperature for solid density (if a single value is available), K
    ''' </summary>
    ''' <returns></returns>
    Property SolidTs As Double

    ''' <summary>
    ''' Solid density (if a single value is available), kg/m3
    ''' </summary>
    ''' <returns></returns>
    Property SolidDensityAtTs As Double

    ''' <summary>
    ''' Standard State Molar Volume in cm3/mol
    ''' </summary>
    ''' <returns></returns>
    Property StandardStateMolarVolume As Double

    ''' <summary>
    ''' Molar volume parameter for ion
    ''' </summary>
    ''' <returns></returns>
    Property MolarVolume_v2i As Double

    ''' <summary>
    ''' Molar volume parameter for ion
    ''' </summary>
    ''' <returns></returns>
    Property MolarVolume_v3i As Double

    ''' <summary>
    ''' Molar volume parameter for ion
    ''' </summary>
    ''' <returns></returns>
    Property MolarVolume_k1i As Double

    ''' <summary>
    ''' Molar volume parameter for ion
    ''' </summary>
    ''' <returns></returns>
    Property MolarVolume_k2i As Double

    ''' <summary>
    ''' Molar volume parameter for ion
    ''' </summary>
    ''' <returns></returns>
    Property MolarVolume_k3i As Double

    ''' <summary>
    ''' Cp for aqueous ion solution, parameter A
    ''' </summary>
    ''' <returns></returns>
    Property Ion_CpAq_a As Double

    ''' <summary>
    ''' Cp for aqueous ion solution, parameter B
    ''' </summary>
    ''' <returns></returns>
    Property Ion_CpAq_b As Double

    ''' <summary>
    ''' Cp for aqueous ion solution, parameter C
    ''' </summary>
    ''' <returns></returns>
    Property Ion_CpAq_c As Double

    'Black-Oil Properties

    ''' <summary>
    ''' True if this compound a black-oil one
    ''' </summary>
    ''' <returns></returns>
    Property IsBlackOil As Boolean

    ''' <summary>
    ''' black oil gas specific gravity
    ''' </summary>
    ''' <returns></returns>
    Property BO_SGG As Double

    ''' <summary>
    ''' black oil oil specific gravity
    ''' </summary>
    ''' <returns></returns>
    Property BO_SGO As Double

    ''' <summary>
    ''' black oil gas-oil ratio (m3/m3 STD)
    ''' </summary>
    ''' <returns></returns>
    Property BO_GOR As Double

    ''' <summary>
    ''' black oil basic sediments and water (%)
    ''' </summary>
    ''' <returns></returns>
    Property BO_BSW As Double

    ''' <summary>
    ''' black oil oil viscosity at T1 (m2/s)
    ''' </summary>
    ''' <returns></returns>
    Property BO_OilVisc1 As Double

    ''' <summary>
    ''' black oil oil viscosity T1 (K)
    ''' </summary>
    ''' <returns></returns>
    Property BO_OilViscTemp1 As Double

    ''' <summary>
    ''' black oil oil viscosity at T2 (m2/s)
    ''' </summary>
    ''' <returns></returns>
    Property BO_OilVisc2 As Double

    ''' <summary>
    ''' black oil oil viscosity T2 (K)
    ''' </summary>
    ''' <returns></returns>
    Property BO_OilViscTemp2 As Double

    ''' <summary>
    ''' black oil oil parafins percentage
    ''' </summary>
    ''' <returns></returns>
    Property BO_PNA_P As Double

    ''' <summary>
    ''' black oil oil napthenics percentage
    ''' </summary>
    ''' <returns></returns>
    Property BO_PNA_N As Double

    ''' <summary>
    ''' black oil oil aromatics percentage
    ''' </summary>
    ''' <returns></returns>
    Property BO_PNA_A As Double

    ''' <summary>
    ''' variable to hold general comments
    ''' </summary>
    ''' <returns></returns>
    Property Comments As String

    ''' <summary>
    ''' List of elements and their amounts
    ''' </summary>
    ''' <returns></returns>
    Property Elements As SortedList

    ''' <summary>
    ''' List of UNIFAC groups and their amounts
    ''' </summary>
    ''' <returns></returns>
    Property UNIFACGroups As SortedList

    ''' <summary>
    ''' List of MODFAC (Do) groups and their amounts
    ''' </summary>
    ''' <returns></returns>
    Property MODFACGroups As SortedList

    ''' <summary>
    ''' List of MODFAC (NIST) groups and their amounts
    ''' </summary>
    ''' <returns></returns>
    Property NISTMODFACGroups As SortedList

    ''' <summary>
    ''' Lennard Jones diameter in meters
    ''' </summary>
    ''' <returns></returns>
    Property LennardJonesDiameter As Double

    ''' <summary>
    ''' Lennard Jones energy in K
    ''' </summary>
    ''' <returns></returns>
    Property LennardJonesEnergy As Double

    ''' <summary>
    ''' Parachor value in kg^0.25.m3/s^0.5/kmol
    ''' </summary>
    ''' <returns></returns>
    Property Parachor As Double

    ''' <summary>
    ''' Fuller diffusion volume
    ''' </summary>
    ''' <returns></returns>
    Property FullerDiffusionVolume As Double

    ''' <summary>
    ''' for custom ordering purposes
    ''' </summary>
    ''' <returns></returns>
    Property Tag As String

    'helper functions

    ''' <summary>
    ''' exports compound data to a JSON file.
    ''' </summary>
    ''' <returns></returns>
    Function ExportToJSON() As String

    ''' <summary>
    ''' import data from a JSON file.
    ''' </summary>
    ''' <param name="data"></param>
    Sub ImportFromJSON(data As String)

    ''' <summary>
    ''' Returns the Vapor Pressure in Pa
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetVaporPressure(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Ideal Gas Heat Capacity in kJ/[kg.K]
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetIdealGasHeatCapacity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Enthalpy of Vaporization in kJ/kg
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetEnthalpyOfVaporization(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Vapor Viscosity in Pa.s
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetVaporViscosity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Vapor Thermal Conductivity in W/(m.K)
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetVaporThermalConductivity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Liquid Viscosity in Pa.s
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetLiquidViscosity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Liquid Thermal Conductivity in W/(m.K)
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetLiquidThermalConductivity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Liquid Heat Capacity in kJ/[kg.K]
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetLiquidHeatCapacity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Liquid Density in kg/m3
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetLiquidDensity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Solid Density in kg/m3
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetSolidDensity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' Returns the Solid Cp in kJ/[kg.K]
    ''' </summary>
    ''' <param name="T">Temperature in K</param>
    ''' <param name="message"></param>
    ''' <returns></returns>
    Function GetSolidHeatCapacity(T As Double, Optional ByRef message As String = "") As Double

    ''' <summary>
    ''' exports compound data to a XLSX file.
    ''' </summary>
    Sub ExportToXLSX(filepath As String)

    Function GetLiquidSurfaceTension(T As Double, ByRef Optional message As String = "") As Double

    ''' <summary>
    ''' COSTALD SRK Acentric factor
    ''' </summary>
    ''' <returns></returns>
    Property COSTALD_SRK_Acentric_Factor As Double

    ''' <summary>
    ''' COSTALD Characteristic Volume (m3/kmol)
    ''' </summary>
    ''' <returns></returns>
    Property COSTALD_Characteristic_Volume As Double

    Property IsSolid As Boolean

    Property ChemSepFamily As Integer

    Property StandardHeatOfCombustion_LHV As Double

End Interface
