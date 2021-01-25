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

    Property ExtraProperties As Dynamic.ExpandoObject

    Property Name As String
    Property CAS_Number As String
    Property Formula As String
    Property SMILES As String
    Property InChI As String
    Property ChemicalStructure As String
    Property Molar_Weight As Double
    Property Critical_Temperature As Double
    Property Critical_Pressure As Double
    Property Critical_Volume As Double
    Property Critical_Compressibility As Double
    Property Acentric_Factor As Double
    Property Z_Rackett As Double
    Property PR_Volume_Translation_Coefficient As Double
    Property SRK_Volume_Translation_Coefficient As Double
    Property Chao_Seader_Acentricity As Double
    Property Chao_Seader_Solubility_Parameter As Double
    Property Chao_Seader_Liquid_Molar_Volume As Double
    Property IG_Entropy_of_Formation_25C As Double
    Property IG_Enthalpy_of_Formation_25C As Double
    Property IG_Gibbs_Energy_of_Formation_25C As Double
    Property Dipole_Moment As Double
    Property Vapor_Pressure_Constant_A As Double
    Property Vapor_Pressure_Constant_B As Double
    Property Vapor_Pressure_Constant_C As Double
    Property Vapor_Pressure_Constant_D As Double
    Property Vapor_Pressure_Constant_E As Double
    Property Vapor_Pressure_TMIN As Double
    Property Vapor_Pressure_TMAX As Double
    Property Ideal_Gas_Heat_Capacity_Const_A As Double
    Property Ideal_Gas_Heat_Capacity_Const_B As Double
    Property Ideal_Gas_Heat_Capacity_Const_C As Double
    Property Ideal_Gas_Heat_Capacity_Const_D As Double
    Property Ideal_Gas_Heat_Capacity_Const_E As Double
    Property Liquid_Viscosity_Const_A As Double
    Property Liquid_Viscosity_Const_B As Double
    Property Liquid_Viscosity_Const_C As Double
    Property Liquid_Viscosity_Const_D As Double
    Property Liquid_Viscosity_Const_E As Double
    Property Liquid_Density_Const_A As Double
    Property Liquid_Density_Const_B As Double
    Property Liquid_Density_Const_C As Double
    Property Liquid_Density_Const_D As Double
    Property Liquid_Density_Const_E As Double
    Property Liquid_Density_Tmin As Double
    Property Liquid_Density_Tmax As Double
    Property Liquid_Heat_Capacity_Const_A As Double
    Property Liquid_Heat_Capacity_Const_B As Double
    Property Liquid_Heat_Capacity_Const_C As Double
    Property Liquid_Heat_Capacity_Const_D As Double
    Property Liquid_Heat_Capacity_Const_E As Double
    Property Liquid_Heat_Capacity_Tmin As Double
    Property Liquid_Heat_Capacity_Tmax As Double
    Property Liquid_Thermal_Conductivity_Const_A As Double
    Property Liquid_Thermal_Conductivity_Const_B As Double
    Property Liquid_Thermal_Conductivity_Const_C As Double
    Property Liquid_Thermal_Conductivity_Const_D As Double
    Property Liquid_Thermal_Conductivity_Const_E As Double
    Property Liquid_Thermal_Conductivity_Tmin As Double
    Property Liquid_Thermal_Conductivity_Tmax As Double
    Property Vapor_Thermal_Conductivity_Const_A As Double
    Property Vapor_Thermal_Conductivity_Const_B As Double
    Property Vapor_Thermal_Conductivity_Const_C As Double
    Property Vapor_Thermal_Conductivity_Const_D As Double
    Property Vapor_Thermal_Conductivity_Const_E As Double
    Property Vapor_Thermal_Conductivity_Tmin As Double
    Property Vapor_Thermal_Conductivity_Tmax As Double
    Property Vapor_Viscosity_Const_A As Double
    Property Vapor_Viscosity_Const_B As Double
    Property Vapor_Viscosity_Const_C As Double
    Property Vapor_Viscosity_Const_D As Double
    Property Vapor_Viscosity_Const_E As Double
    Property Vapor_Viscosity_Tmin As Double
    Property Vapor_Viscosity_Tmax As Double
    Property Solid_Density_Const_A As Double
    Property Solid_Density_Const_B As Double
    Property Solid_Density_Const_C As Double
    Property Solid_Density_Const_D As Double
    Property Solid_Density_Const_E As Double
    Property Solid_Density_Tmin As Double
    Property Solid_Density_Tmax As Double
    Property Surface_Tension_Const_A As Double
    Property Surface_Tension_Const_B As Double
    Property Surface_Tension_Const_C As Double
    Property Surface_Tension_Const_D As Double
    Property Surface_Tension_Const_E As Double
    Property Surface_Tension_Tmin As Double
    Property Surface_Tension_Tmax As Double
    Property Solid_Heat_Capacity_Const_A As Double
    Property Solid_Heat_Capacity_Const_B As Double
    Property Solid_Heat_Capacity_Const_C As Double
    Property Solid_Heat_Capacity_Const_D As Double
    Property Solid_Heat_Capacity_Const_E As Double
    Property Solid_Heat_Capacity_Tmin As Double
    Property Solid_Heat_Capacity_Tmax As Double
    Property Normal_Boiling_Point As Double
    Property ID As Integer
    Property IsPF As Integer
    Property IsHYPO As Integer
    Property HVap_A As Double
    Property HVap_B As Double
    Property HVap_C As Double
    Property HVap_D As Double
    Property HVap_E As Double
    Property HVap_TMIN As Double
    Property HVap_TMAX As Double
    Property UNIQUAC_R As Double
    Property UNIQUAC_Q As Double

    Property IsFPROPSSupported As Boolean
    Property IsCOOLPROPSupported As Boolean
    Property IsModified As Boolean

    Property VaporPressureEquation As String
    Property IdealgasCpEquation As String
    Property LiquidViscosityEquation As String
    Property VaporViscosityEquation As String
    Property VaporizationEnthalpyEquation As String
    Property LiquidDensityEquation As String
    Property LiquidHeatCapacityEquation As String
    Property LiquidThermalConductivityEquation As String
    Property VaporThermalConductivityEquation As String
    Property SolidDensityEquation As String
    Property SolidHeatCapacityEquation As String
    Property SurfaceTensionEquation As String

    Property PC_SAFT_sigma As Double
    Property PC_SAFT_epsilon_k As Double
    Property PC_SAFT_m As Double

    Property PF_MM As Nullable(Of Double)
    Property NBP As Nullable(Of Double)
    Property PF_vA As Nullable(Of Double)
    Property PF_vB As Nullable(Of Double)
    Property PF_Watson_K As Nullable(Of Double)
    Property PF_SG As Nullable(Of Double)
    Property PF_v1 As Nullable(Of Double)
    Property PF_Tv1 As Nullable(Of Double)
    Property PF_v2 As Nullable(Of Double)
    Property PF_Tv2 As Nullable(Of Double)

    'Databases: 'DWSIM', 'CheResources', 'ChemSep' OR 'User'
    'User databases are XML-serialized versions of this base class, and they may include hypos and pseudos.
    Property OriginalDB As String
    Property CurrentDB As String
    Property CompCreatorStudyFile As String

    'COSMO-SAC's database equivalent name
    Property COSMODBName

    'Electrolyte-related properties
    Property IsIon As Boolean
    Property IsSalt As Boolean
    Property IsHydratedSalt As Boolean
    Property HydrationNumber As Double
    Property Charge As Integer
    Property PositiveIon As String
    Property NegativeIon As String
    Property PositiveIonStoichCoeff As Integer
    Property NegativeIonStoichCoeff As Integer
    Property StoichSum As Integer
    Property Electrolyte_DelGF As Double
    Property Electrolyte_DelHF As Double
    Property Electrolyte_Cp0 As Double
    Property TemperatureOfFusion As Double
    Property EnthalpyOfFusionAtTf As Double
    Property SolidTs As Double
    Property SolidDensityAtTs As Double
    Property StandardStateMolarVolume As Double
    Property MolarVolume_v2i As Double
    Property MolarVolume_v3i As Double
    Property MolarVolume_k1i As Double
    Property MolarVolume_k2i As Double
    Property MolarVolume_k3i As Double
    Property Ion_CpAq_a As Double
    Property Ion_CpAq_b As Double
    Property Ion_CpAq_c As Double

    'Black-Oil Properties

    Property IsBlackOil As Boolean
    'gas specific gravity
    Property BO_SGG As Double
    'oil specific gravity
    Property BO_SGO As Double
    'gas-oil ratio (m3/m3 STD)
    Property BO_GOR As Double
    'basic sediments and water (%)
    Property BO_BSW As Double
    'oil viscosity at T1 (m2/s)
    Property BO_OilVisc1 As Double
    'oil viscosity T1 (K)
    Property BO_OilViscTemp1 As Double
    'oil viscosity at T2 (m2/s)
    Property BO_OilVisc2 As Double
    'oil viscosity T2 (K)
    Property BO_OilViscTemp2 As Double
    'oil parafins percentage
    Property BO_PNA_P As Double
    'oil napthenics percentage
    Property BO_PNA_N As Double
    'oil aromatics percentage
    Property BO_PNA_A As Double

    Property Comments As String

    Property Elements As SortedList
    Property UNIFACGroups As SortedList
    Property MODFACGroups As SortedList
    Property NISTMODFACGroups As SortedList

    Property LennardJonesDiameter As Double
    Property LennardJonesEnergy As Double
    Property Parachor As Double
    Property FullerDiffusionVolume As Double

    'for custom ordering purposes
    Property Tag As String

End Interface
