'    Unit System Classes for DWSIM
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Namespace DWSIM.SistemasDeUnidades

    <System.Serializable()> Public Class Unidades

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public nome As String

        Public mass As String = "kg", reac_rate_heterog As String = "mol/[kg.s]", area, distance,
            time, volume, molar_volume, diameter, thickness, molar_conc, mass_conc,
            heat_transf_coeff, force, accel, spec_vol, reac_rate, velocity, foulingfactor,
            cakeresistance, mediumresistance As String

        Public gor As String = "m3/m3"

        Public molar_enthalpy, molar_entropy As String

        Public tdp_idealGasHeatCapacity As String
        Public tdp_surfaceTension As String
        Public tdp_thermalConductivityOfLiquid As String
        Public tdp_thermalConductivityOfVapor As String
        Public tdp_vaporPressure As String
        Public tdp_viscosityOfLiquid As String
        Public tdp_viscosityOfVapor As String

        Public pdp_boilingPointTemperature As String
        Public pdp_meltingTemperature As String

        Public spmp_activity As String
        Public spmp_activityCoefficient As String
        Public spmp_compressibility As String
        Public spmp_compressibilityFactor As String
        Public spmp_density As String
        Public spmp_enthalpy As String
        Public spmp_entropy As String
        Public spmp_excessEnthalpy As String
        Public spmp_excessEntropy As String
        Public spmp_molarflow As String
        Public spmp_massflow As String
        Public spmp_molarfraction As String
        Public spmp_massfraction As String
        Public spmp_fugacity As String
        Public spmp_fugacityCoefficient As String
        Public spmp_heatCapacityCp As String
        Public spmp_heatCapacityCv As String
        Public spmp_jouleThomsonCoefficient As String
        Public spmp_logFugacityCoefficient As String
        Public spmp_molecularWeight As String
        Public spmp_pressure As String
        Public spmp_temperature As String
        Public spmp_speedOfSound As String
        Public spmp_thermalConductivity As String
        Public spmp_viscosity As String
        Public spmp_cinematic_viscosity As String
        Public spmp_volumetricFlow As String
        Public spmp_heatflow As String
        Public spmp_head As String
        Public spmp_deltaT As String
        Public spmp_deltaP As String

        Public tpmp_kvalue As String
        Public tpmp_logKvalue As String
        Public tpmp_surfaceTension As String

        Public Sub New()

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me, True)
        End Function

    End Class

    <System.Serializable()> Public Class UnidadesSI

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("SistemaSI")

                .accel = "m2/s"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "mol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "mol/[m3.s]"
                .reac_rate_heterog = "mol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "s"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "K"
                .pdp_meltingTemperature = "K"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/s"
                .spmp_massfraction = "-"
                .spmp_molarflow = "mol/s"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "Pa"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "K"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "Pa.s"
                .spmp_volumetricFlow = "m3/s"
                .spmp_cinematic_viscosity = "m2/s"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "Pa"
                .tdp_viscosityOfLiquid = "Pa.s"
                .tdp_viscosityOfVapor = "Pa.s"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kW"
                .spmp_head = "m"
                .spmp_deltaP = "Pa"
                .spmp_deltaT = "K"

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesSI_Deriv1

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("Personalizado1BR")

                .accel = "m2/s"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol.[m3.s]"
                .reac_rate_heterog = "kmol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "s"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/h"
                .spmp_massfraction = "-"
                .spmp_molarflow = "m3/d @ BR"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "kgf/cm2_a"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "C"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "cP"
                .spmp_volumetricFlow = "m3/h"
                .spmp_cinematic_viscosity = "cSt"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "kgf/cm2_a"
                .tdp_viscosityOfLiquid = "cP"
                .tdp_viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kW"
                .spmp_head = "m"
                .spmp_deltaP = "kgf/cm2"
                .spmp_deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesSI_Deriv2

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("Personalizado2SC")

                .accel = "m2/s"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol.[m3.s]"
                .reac_rate_heterog = "kmol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "s"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/h"
                .spmp_massfraction = "-"
                .spmp_molarflow = "m3/d @ SC"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "kgf/cm2_a"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "C"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "cP"
                .spmp_volumetricFlow = "m3/h"
                .spmp_cinematic_viscosity = "cSt"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "kgf/cm2_a"
                .tdp_viscosityOfLiquid = "cP"
                .tdp_viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kW"
                .spmp_head = "m"
                .spmp_deltaP = "kgf/cm2"
                .spmp_deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesSI_Deriv3

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("Personalizado3CNTP")

                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol.[m3.s]"
                .reac_rate_heterog = "kmol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "s"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/h"
                .spmp_massfraction = "-"
                .spmp_molarflow = "m3/d @ CNTP"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "kgf/cm2_a"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "C"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "cP"
                .spmp_volumetricFlow = "m3/h"
                .spmp_cinematic_viscosity = "cSt"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "kgf/cm2_a"
                .tdp_viscosityOfLiquid = "cP"
                .tdp_viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kW"
                .spmp_head = "m"
                .spmp_deltaP = "kgf/cm2"
                .spmp_deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesSI_Deriv4

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("Personalizado4")

                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol.[m3.s]"
                .reac_rate_heterog = "kmol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "s"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/d"
                .spmp_massfraction = "-"
                .spmp_molarflow = "m3/d @ SC"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "kPa"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "C"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "cP"
                .spmp_volumetricFlow = "m3/d"
                .spmp_cinematic_viscosity = "cSt"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "kPa"
                .tdp_viscosityOfLiquid = "cP"
                .tdp_viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kJ/d"
                .spmp_head = "m"
                .spmp_deltaP = "kgf/cm2"
                .spmp_deltaT = "C."

            End With

        End Sub

    End Class
    <System.Serializable()> Public Class UnidadesSI_Deriv5

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("Personalizado5")

                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol.[m3.s]"
                .reac_rate_heterog = "kmol/[kg.s]"
                .spec_vol = "m3/kg"
                .time = "h"
                .volume = "m3"
                .mass = "kg"
                .thickness = "mm"
                .molar_enthalpy = "kJ/kmol"
                .molar_entropy = "kJ/[kmol.K]"
                .velocity = "m/s"
                .foulingfactor = "K.m2/W"

                .cakeresistance = "m/kg"
                .mediumresistance = "m-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "Pa"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "Pa-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "kg/m3"
                .spmp_enthalpy = "kJ/kg"
                .spmp_entropy = "kJ/[kg.K]"
                .spmp_excessEnthalpy = "kJ/kg"
                .spmp_excessEntropy = "kJ/[kg.K]"
                .spmp_fugacity = "Pa"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "kJ/[kg.K]"
                .spmp_heatCapacityCv = "kJ/[kg.K]"
                .spmp_jouleThomsonCoefficient = "K/Pa"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "kg/h"
                .spmp_massfraction = "-"
                .spmp_molarflow = "kmol/h"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "kg/kmol"
                .spmp_pressure = "bar"
                .spmp_speedOfSound = "m/s"
                .spmp_temperature = "°C"
                .spmp_thermalConductivity = "W/[m.K]"
                .spmp_viscosity = "Pa.s"
                .spmp_volumetricFlow = "m3/h"
                .spmp_cinematic_viscosity = "m2/s"
                .tdp_idealGasHeatCapacity = "kJ/[kg.K]"
                .tdp_surfaceTension = "N/m"
                .tdp_thermalConductivityOfLiquid = "W/[m.K]"
                .tdp_thermalConductivityOfVapor = "W/[m.K]"
                .tdp_vaporPressure = "bar"
                .tdp_viscosityOfLiquid = "Pa.s"
                .tdp_viscosityOfVapor = "Pa.s"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .spmp_heatflow = "kW"
                .spmp_head = "m"
                .spmp_deltaP = "bar"
                .spmp_deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesINGLES

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("SistemaIngls")

                .gor = "ft3/bbl"

                .accel = "ft/s2"
                .area = "ft2"
                .diameter = "in"
                .distance = "ft"
                .force = "lbf"
                .heat_transf_coeff = "BTU/[ft2.h.R]"
                .mass_conc = "lbm/ft3"
                .molar_conc = "lbmol/ft3"
                .molar_volume = "ft3/lbmol"
                .reac_rate = "lbmol.[ft3.h]"
                .reac_rate_heterog = "lbmol.[lbm.h]"
                .spec_vol = "ft3/lbm"
                .time = "h"
                .volume = "ft3"
                .mass = "lb"
                .thickness = "in"
                .molar_enthalpy = "BTU/lbmol"
                .molar_entropy = "BTU/[lbmol.R]"
                .velocity = "ft/s"
                .foulingfactor = "ft2.h.F/BTU"

                .cakeresistance = "ft/lbm"
                .mediumresistance = "ft-1"

                .pdp_boilingPointTemperature = "R"
                .pdp_meltingTemperature = "R"
                .spmp_activity = "lbf/ft2"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "ft2/lbf"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "lbm/ft3"
                .spmp_enthalpy = "BTU/lbm"
                .spmp_entropy = "BTU/[lbm.R]"
                .spmp_excessEnthalpy = "BTU/lbm"
                .spmp_excessEntropy = "BTU/[lbm.R]"
                .spmp_fugacity = "lbf/ft2"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "BTU/[lbm.R]"
                .spmp_heatCapacityCv = "BTU/[lbm.R]"
                .spmp_jouleThomsonCoefficient = "R/[lbf/ft2]"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "lbm/h"
                .spmp_massfraction = "-"
                .spmp_molarflow = "lbmol/h"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "lbm/lbmol"
                .spmp_pressure = "lbf/ft2"
                .spmp_speedOfSound = "ft/s"
                .spmp_temperature = "R"
                .spmp_thermalConductivity = "BTU/[ft.h.R]"
                .spmp_viscosity = "lbm/[ft.h]"
                .tdp_idealGasHeatCapacity = "BTU/[lbm.R]"
                .tdp_surfaceTension = "lbf/in"
                .tdp_thermalConductivityOfLiquid = "BTU/[ft.h.R]"
                .tdp_thermalConductivityOfVapor = "BTU/[ft.h.R]"
                .tdp_vaporPressure = "lbf/ft2"
                .tdp_viscosityOfLiquid = "lbm/[ft.h]"
                .tdp_viscosityOfVapor = "lbm/[ft.h]"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "lbf/in"
                .spmp_volumetricFlow = "ft3/s"
                .spmp_cinematic_viscosity = "ft2/s"
                .spmp_heatflow = "BTU/h"
                .spmp_head = "ft"
                .spmp_deltaP = "lbf/ft2"
                .spmp_deltaT = "R."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class UnidadesCGS

        Inherits Unidades

        Public Sub New()

            With Me

                .nome = DWSIM.App.GetLocalString("SistemaCGS")

                .accel = "cm/s2"
                .area = "cm2"
                .diameter = "mm"
                .distance = "cm"
                .force = "dyn"
                .heat_transf_coeff = "cal/[cm2.s.C]"
                .mass_conc = "g/cm3"
                .molar_conc = "mol/cm3"
                .molar_volume = "cm3/mol"
                .reac_rate = "mol.[cm3.s]"
                .reac_rate_heterog = "mol/[g.s]"
                .spec_vol = "cm3/g"
                .time = "s"
                .volume = "cm3"
                .mass = "g"
                .thickness = "cm"
                .molar_enthalpy = "cal/mol"
                .molar_entropy = "cal/[mol.C]"
                .velocity = "cm/s"
                .foulingfactor = "C.cm2.s/cal"

                .cakeresistance = "cm/g"
                .mediumresistance = "cm-1"

                .pdp_boilingPointTemperature = "C"
                .pdp_meltingTemperature = "C"
                .spmp_activity = "atm"
                .spmp_activityCoefficient = "-"
                .spmp_compressibility = "atm-1"
                .spmp_compressibilityFactor = "-"
                .spmp_density = "g/cm3"
                .spmp_enthalpy = "cal/g"
                .spmp_entropy = "cal/[g.C]"
                .spmp_excessEnthalpy = "cal/g"
                .spmp_excessEntropy = "cal/[g.C]"
                .spmp_fugacity = "atm"
                .spmp_fugacityCoefficient = "-"
                .spmp_heatCapacityCp = "cal/[g.C]"
                .spmp_heatCapacityCv = "cal/[g.C]"
                .spmp_jouleThomsonCoefficient = "C/atm"
                .spmp_logFugacityCoefficient = "-"
                .spmp_massflow = "g/s"
                .spmp_massfraction = "-"
                .spmp_molarflow = "mol/s"
                .spmp_molarfraction = "-"
                .spmp_molecularWeight = "g/mol"
                .spmp_pressure = "atm"
                .spmp_speedOfSound = "cm/s"
                .spmp_temperature = "C"
                .spmp_thermalConductivity = "cal/[cm.s.C]"
                .spmp_viscosity = "cP"
                .tdp_idealGasHeatCapacity = "cal/[g.C]"
                .tdp_surfaceTension = "dyn/cm"
                .tdp_thermalConductivityOfLiquid = "cal/[cm.s.C]"
                .tdp_thermalConductivityOfVapor = "cal/[cm.s.C]"
                .tdp_vaporPressure = "atm"
                .tdp_viscosityOfLiquid = "cP"
                .tdp_viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "dyn/cm2"
                .spmp_volumetricFlow = "cm3/s"
                .spmp_cinematic_viscosity = "cSt"
                .spmp_heatflow = "kcal/h"
                .spmp_head = "m"
                .spmp_deltaT = "C."
                .spmp_deltaP = "atm"

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class Conversor

        Public Shared Function ConverterParaSI(ByVal unidade As String, ByVal valor As Double)

            Select Case unidade

                Case "ft3/bbl"
                    Return valor * 0.177295
                Case "g"
                    Return valor / 1000
                Case "lb"
                    Return valor / 2.20462
                Case "m/s"
                    Return valor
                Case "cm/s"
                    Return valor / 100
                Case "mm/s"
                    Return valor / 1000
                Case "km/h"
                    Return valor / 3.6
                Case "ft/h"
                    Return valor / 11811
                Case "ft/min"
                    Return valor / 196.85
                Case "ft/s"
                    Return valor / 3.28084
                Case "in/s"
                    Return valor / 39.3701

                Case "kPa"
                    Return valor / 0.001
                Case "ftH2O"
                    Return valor / 0.000334
                Case "inH2O"
                    Return valor / 0.00401463
                Case "inHg"
                    Return valor / 0.000295301
                Case "lbf/ft2"
                    Return valor / 0.0208854
                Case "mbar"
                    Return valor / 0.01
                Case "mH2O"
                    Return valor / 0.000101972
                Case "mmH2O"
                    Return valor / 0.101972
                Case "mmHg"
                    Return valor / 0.00750064
                Case "MPa"
                    Return valor / 0.000001
                Case "psi"
                    Return valor / 0.000145038
                Case "bar"
                    Return valor * 100000
                Case "kPag"
                    Return (valor + 101.325) * 1000
                Case "barg"
                    Return (valor + 1.01325) * 100000
                Case "kgf/cm2g"
                    Return (valor + 1.033) * 101325 / 1.033
                Case "psig"
                    Return (valor + 14.696) * 6894.8

                Case "kg/d"
                    Return valor / 60 / 60 / 24
                Case "kg/min"
                    Return valor / 60
                Case "lb/h"
                    Return valor / 7936.64
                Case "lb/min"
                    Return valor / 132.277
                Case "lb/s"
                    Return valor / 2.20462

                Case "mol/h"
                    Return valor / 3600
                Case "mol/d"
                    Return valor / 3600 / 24
                Case "kmol/s"
                    Return valor * 1000
                Case "kmol/h"
                    Return valor * 1000 / 3600
                Case "kmol/d"
                    Return valor * 1000 / 3600 / 24

                Case "kmol/[kg.s]"
                    Return valor * 1000
                Case "kmol/[kg.min.]"
                    Return valor / 60 * 1000
                Case "kmol/[kg.h]"
                    Return valor / 60 / 60 * 1000
                Case "mol/[kg.s]"
                    Return valor
                Case "mol/[kg.min.]"
                    Return valor / 60
                Case "mol/[kg.h]"
                    Return valor / 60 / 60
                Case "lbmol/[lbm.h]"
                    Return valor * 453.59237 * 2.20462 / 60 / 60

                Case "bbl/h"
                    Return valor / 22643.3
                Case "bbl/d"
                    Return valor / 543440
                Case "ft3/min"
                    Return valor / 35.3147 / 60
                Case "ft3/d"
                    Return valor / 35.3147 / 60 / 60 / 24
                Case "ft3/s"
                    Return valor / 35.3147
                Case "gal[UK]/h"
                    Return valor / 791889
                Case "gal[UK]/s"
                    Return valor / 219.969
                Case "gal[US]/h"
                    Return valor / 951019
                Case "gal[US]/min"
                    Return valor / 15850.3
                Case "L/h"
                    Return valor / 3600000.0
                Case "L/min"
                    Return valor / 60000
                Case "L/s"
                    Return valor / 1000
                Case "m3/d"
                    Return valor / 3600 / 24

                Case "BTU/h"
                    Return valor / 3412.14
                Case "BTU/s"
                    Return valor / 0.947817
                Case "cal/s"
                    Return valor / 238.846
                Case "HP"
                    Return valor / 1.35962
                Case "kcal/h"
                    Return valor / 859.845
                Case "kJ/h"
                    Return valor / 3600
                Case "kJ/d"
                    Return valor / 3600 / 24
                Case "MW"
                    Return valor / 0.001
                Case "W"
                    Return valor / 1000

                Case "BTU/lb"
                    Return valor / 0.429923
                Case "cal/g"
                    Return valor / 0.238846
                Case "kcal/kg"
                    Return valor / 0.238846

                Case "kJ/kmol"
                    Return valor
                Case "cal/mol"
                    Return valor * 0.0041868 * 1000
                Case "BTU/lbmol"
                    Return valor * 1.05506 * 1000
                Case "kJ/[kmol.K]"
                    Return valor * 1
                Case "cal/[mol.°C]"
                    Return valor * 0.0041868 * 1000
                Case "cal/[mol.C]"
                    Return valor * 0.0041868 * 1000
                Case "BTU/[lbmol.R]"
                    Return valor * 1.05506 * 1000

                Case "K.m2/W"                           'fouling factor
                    Return valor
                Case "C.cm2.s/cal"
                    Return valor * 0.000023885
                Case "ft2.h.F/BTU"
                    Return valor * 0.17611

                Case "m/kg"
                    Return valor
                Case "ft/lb"
                    Return valor / 3.28084 * 2.20462
                Case "cm/g"
                    Return valor * 10
                Case "m-1"
                    Return valor
                Case "ft-1"
                    Return valor * 3.28084
                Case "cm-1"
                    Return valor * 100

                Case "m2"                               'area
                    Return valor
                Case "cm2"
                    Return valor / 10000.0
                Case "ft2"
                    Return valor / 10.7639
                Case "h"                                'tempo
                    Return valor * 3600
                Case "s"
                    Return valor
                Case "min."
                    Return valor * 60
                Case "ft3"                              'volume
                    Return valor / 35.3147
                Case "m3"
                    Return valor
                Case "cm3"
                    Return valor / 1000000.0
                Case "L"
                    Return valor / 1000.0
                Case "cm3/mol"                          'volume molar'
                    Return valor / 1000.0
                Case "m3/kmol"
                    Return valor
                Case "m3/mol"
                    Return valor * 1000
                Case "ft3/lbmol"
                    Return valor / 35.3147 / 1000
                Case "mm"                               'comprimento'
                    Return valor / 1000
                Case "in.", "in"
                    Return valor / 39.3701
                Case "dyn"                              'força
                    Return valor / 100000
                Case "N"
                    Return valor
                Case "lbf"
                    Return valor / 0.224809
                Case "mol/L"                            'conc molar
                    Return valor
                Case "kmol/m3"
                    Return valor
                Case "mol/cm3"
                    Return valor * 1000000.0 / 1000
                Case "mol/mL"
                    Return valor * 1000000.0 / 1000
                Case "lbmol/ft3"
                    Return valor * 35.3147 * 1000
                Case "g/L"                              'conc mássica
                    Return valor
                Case "kg/m3"
                    Return valor
                Case "g/cm3"
                    Return valor * 1000000.0 / 1000
                Case "g/mL"
                    Return valor * 1000000.0 / 1000
                Case "m2/s"                              'k.visc
                    Return valor
                Case "ft/s2"
                    Return valor / 3.28084
                Case "cm2/s"
                    Return valor / 10000.0
                Case "W/[m2.K]"                              'HTC
                    Return valor
                Case "BTU/[ft2.h.R]"
                    Return valor / 0.17611
                Case "cal/[cm.s.°C]"
                    Return valor / 0.0000238846
                Case "cal/[cm.s.C]"
                    Return valor / 0.0000238846
                Case "m3/kg"                                'vol especif
                    Return valor
                Case "ft3/lbm"
                    Return valor * 0.062428
                Case "cm3/g"
                    Return valor / 1000
                Case "kmol/[m3.s]"
                    Return valor * 1000                            'taxa reacao
                Case "kmol/[m3.min.]"
                    Return valor / 60 * 1000
                Case "kmol/[m3.h]"
                    Return valor / 3600 * 1000
                Case "mol/[m3.s]"
                    Return valor
                Case "mol/[m3.min.]"
                    Return valor / 60
                Case "mol/[m3.h]"
                    Return valor / 3600
                Case "mol/[L.s]"
                    Return valor / 1000
                Case "mol/[L.min.]"
                    Return valor / 60 / 1000
                Case "mol/[L.h]"
                    Return valor / 3600 / 1000
                Case "mol/[cm3.s]"
                    Return valor * 1000000.0
                Case "mol/[cm3.min.]"
                    Return valor / 60 * 1000000.0
                Case "mol/[cm3.h]"
                    Return valor / 3600 * 1000000.0
                Case "lbmol.[ft3.h]"
                    Return valor / 3600 * 35.3147 * 1000
                Case "°C"                               'temperatura e demais
                    Return valor + 273.15
                Case "C"                               'temperatura e demais
                    Return valor + 273.15
                Case "°C."
                    Return valor
                Case "C."
                    Return valor
                Case "atm"
                    Return valor * 101325
                Case "g/s"
                    Return valor / 1000
                Case "mol/s"
                    Return valor
                Case "kmol/s"
                    Return valor * 1000
                Case "cal/g"
                    Return valor / 0.238846
                Case "g/cm3"
                    Return valor * 1000
                Case "dyn/cm"
                    Return valor * 0.001
                Case "dyn/cm2"
                    Return valor * 0.001
                Case "cal/[cm.s.°C]"
                    Return valor / 0.00238846
                Case "cal/[cm.s.C]"
                    Return valor / 0.00238846
                Case "cm3/s"
                    Return valor * 0.000001
                Case "cal/[g.°C]"
                    Return valor / 0.238846
                Case "cal/[g.C]"
                    Return valor / 0.238846
                Case "cSt"
                    Return valor / 1000000.0
                Case "mm2/s"
                    Return valor / 1000000.0
                Case "Pa.s"
                    Return valor
                Case "cP"
                    Return valor / 1000
                Case "kcal/h"
                    Return valor / 859.845
                Case "m"
                    Return valor
                Case "R"
                    Return valor / 1.8
                Case "R."
                    Return valor / 1.8
                Case "lbf/ft2"
                    Return valor / 0.0208854
                Case "lbm/h"
                    Return valor / 7936.64
                Case "lbmol/h"
                    Return valor * 453.59237 / 3600
                Case "BTU/lbm"
                    Return valor / 0.429923
                Case "lbm/ft3"
                    Return valor / 0.062428
                Case "lbf/in"
                    Return valor / 0.00571015
                Case "BTU/[ft.h.R]"
                    Return valor / 0.577789
                Case "ft3/s"
                    Return valor / 35.3147
                Case "BTU/[lbm.R]"
                    Return valor / 0.238846
                Case "ft2/s"
                    Return valor / 10.7639
                Case "lbm/[ft.s]"
                    Return valor / 0.671969
                Case "BTU/h"
                    Return valor / 3412.14
                Case "ft"
                    Return valor / 3.28084
                    'Personalizados
                Case "kgf/cm2_a"
                    Return valor * 101325 / 1.033
                Case "kgf/cm2"
                    Return valor * 101325 / 1.033
                Case "kgf/cm2_g"
                    Return (valor + 1.033) * 101325 / 1.033
                Case "kg/h"
                    Return valor / 3600
                Case "kg/d"
                    Return valor / 3600 / 24
                Case "m3/h"
                    Return valor / 3600
                Case "m3/d"
                    Return valor / 3600 / 24
                Case "m3/d @ BR", "m3/d @ 20 C, 1 atm"
                    Return valor / (24.055 * 3600 * 24 / 1000)
                Case "m3/d @ CNTP"
                    Return valor / (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ NC", "m3/d @ 0 C, 1 atm"
                    Return valor / (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ SC", "m3/d @ 15.56 C, 1 atm"
                    Return valor / (23.69 * 3600 * 24 / 1000)
                Case "ft3/d @ 60 F, 14.7 psia"
                    Return valor / (23.69 * 3600 * 24 / 1000) / 35.3147
                Case "ft3/d @ 0 C, 1 atm"
                    Return valor / (22.71 * 3600 * 24 / 1000) / 35.3147
                Case "°F"
                    Return (valor - 32) * 5 / 9 + 273.15
                Case "°F."
                    Return valor / 1.8
                Case "F"
                    Return (valor - 32) * 5 / 9 + 273.15
                Case "F."
                    Return valor / 1.8
                Case "cm"
                    Return valor / 100
                Case "cal/[mol.°C]"
                    Return valor * 238.846 / 1000
                Case "cal/[mol.C]"
                    Return valor * 238.846 / 1000
                Case "BTU/[lbmol.R]"
                    Return valor * 1.8 * 0.947817
                Case Else
                    Return valor
            End Select

        End Function

        Public Shared Function ConverterDoSI(ByVal unidade As String, ByVal valor As Double)

            Select Case unidade

                Case "ft3/bbl"
                    Return valor / 0.177295
                Case "g"
                    Return valor * 1000
                Case "lb"
                    Return valor * 2.20462
                Case "m/s"
                    Return valor
                Case "cm/s"
                    Return valor * 100
                Case "mm/s"
                    Return valor * 1000
                Case "km/h"
                    Return valor * 3.6
                Case "ft/h"
                    Return valor * 11811
                Case "ft/min"
                    Return valor * 196.85
                Case "ft/s"
                    Return valor * 3.28084
                Case "in/s"
                    Return valor * 39.3701

                Case "kPa"
                    Return valor * 0.001
                Case "ftH2O"
                    Return valor * 0.000334
                Case "inH2O"
                    Return valor * 0.00401463
                Case "inHg"
                    Return valor * 0.000295301
                Case "lbf/ft2"
                    Return valor * 0.0208854
                Case "mbar"
                    Return valor * 0.01
                Case "mH2O"
                    Return valor * 0.000101972
                Case "mmH2O"
                    Return valor * 0.101972
                Case "mmHg"
                    Return valor * 0.00750064
                Case "MPa"
                    Return valor * 0.000001
                Case "psi"
                    Return valor * 0.000145038
                Case "bar"
                    Return valor / 100000
                Case "kPag"
                    Return (valor - 101325) / 1000
                Case "barg"
                    Return (valor - 101325) / 100000
                Case "kgf/cm2g"
                    Return (valor - 101325) / 101325 * 1.033
                Case "psig"
                    Return (valor - 101325) * 0.000145038

                Case "kg/d"
                    Return valor * 60 * 60 * 24
                Case "kg/min"
                    Return valor * 60
                Case "lb/h"
                    Return valor * 7936.64
                Case "lb/min"
                    Return valor * 132.277
                Case "lb/s"
                    Return 2.20462

                Case "bbl/h"
                    Return valor * 22643.3
                Case "bbl/d"
                    Return valor * 543440
                Case "ft3/min"
                    Return valor * 35.3147 * 60
                Case "ft3/d"
                    Return valor * 35.3147 * 60 * 60 * 24
                Case "ft3/s"
                    Return valor * 35.3147
                Case "gal[UK]/h"
                    Return valor * 791889
                Case "gal[UK]/s"
                    Return valor * 219.969
                Case "gal[US]/h"
                    Return valor * 951019
                Case "gal[US]/min"
                    Return valor * 15850.3
                Case "L/h"
                    Return valor * 3600000.0
                Case "L/min"
                    Return valor * 60000
                Case "L/s"
                    Return valor * 1000
                Case "m3/d"
                    Return valor * 3600 * 24

                Case "mol/h"
                    Return valor * 3600
                Case "mol/d"
                    Return valor * 3600 * 24
                Case "kmol/s"
                    Return valor / 1000
                Case "kmol/h"
                    Return valor / 1000 * 3600
                Case "kmol/d"
                    Return valor / 1000 * 3600 * 24

                Case "kmol/[kg.s]"
                    Return valor / 1000
                Case "kmol/[kg.min.]"
                    Return valor * 60 / 1000
                Case "kmol/[kg.h]"
                    Return valor * 60 * 60 / 1000
                Case "mol/[kg.s]"
                    Return valor
                Case "mol/[kg.min.]"
                    Return valor * 60
                Case "mol/[kg.h]"
                    Return valor * 60 * 60
                Case "lbmol/[lbm.h]"
                    Return valor * 453.59237 * 2.20462 * 60 * 60

                Case "BTU/h"
                    Return valor * 3412.14
                Case "BTU/s"
                    Return valor * 0.947817
                Case "cal/s"
                    Return valor * 238.846
                Case "HP"
                    Return valor * 1.35962
                Case "kcal/h"
                    Return valor * 859.845
                Case "kJ/h"
                    Return valor * 3600
                Case "kJ/d"
                    Return valor * 3600 * 24
                Case "MW"
                    Return valor * 0.001
                Case "W"
                    Return valor * 1000

                Case "BTU/lb"
                    Return valor * 0.429923
                Case "cal/g"
                    Return valor * 0.238846
                Case "kcal/kg"
                    Return valor * 0.238846

                Case "kJ/kmol"
                    Return valor
                Case "cal/mol"
                    Return valor / 0.0041868 / 1000
                Case "BTU/lbmol"
                    Return valor / 1.05506 / 1000
                Case "kJ/[kmol.K]"
                    Return valor
                Case "cal/[mol.°C]"
                    Return valor / 0.0041868 / 1000
                Case "cal/[mol.C]"
                    Return valor / 0.0041868 / 1000
                Case "BTU/[lbmol.R]"
                    Return valor / 1.05506 / 1000

                Case "K.m2/W"                           'fouling factor
                    Return valor
                Case "C.cm2.s/cal"
                    Return valor / 0.000023885
                Case "ft2.h.F/BTU"
                    Return valor / 0.17611

                Case "m/kg"
                    Return valor
                Case "ft/lb"
                    Return valor * 3.28084 / 2.20462
                Case "cm/g"
                    Return valor / 10
                Case "m-1"
                    Return valor
                Case "ft-1"
                    Return valor / 3.28084
                Case "cm-1"
                    Return valor / 100

                Case "m2"                               'area
                    Return valor
                Case "cm2"
                    Return valor * 10000.0
                Case "ft2"
                    Return valor * 10.7639
                Case "h"                                'tempo
                    Return valor / 3600
                Case "s"
                    Return valor
                Case "min."
                    Return valor / 60
                Case "ft3"                              'volume
                    Return valor * 35.3147
                Case "m3"
                    Return valor
                Case "cm3"
                    Return valor * 1000000.0
                Case "L"
                    Return valor * 1000.0
                Case "cm3/mol"                          'volume molar'
                    Return valor * 1000.0
                Case "m3/kmol"
                    Return valor
                Case "m3/mol"
                    Return valor / 1000
                Case "ft3/lbmol"
                    Return valor * 35.3147 * 1000
                Case "mm"                               'comprimento'
                    Return valor * 1000
                Case "in.", "in"
                    Return valor * 39.3701
                Case "dyn"                              'força
                    Return valor * 100000
                Case "N"
                    Return valor
                Case "lbf"
                    Return valor * 0.224809
                Case "mol/L"                            'conc molar
                    Return valor
                Case "kmol/m3"
                    Return valor
                Case "mol/cm3"
                    Return valor / 1000000.0 * 1000
                Case "mol/mL"
                    Return valor / 1000000.0 * 1000
                Case "lbmol/ft3"
                    Return valor * 35.3147 * 1000
                Case "g/L"                              'conc mássica
                    Return valor
                Case "kg/m3"
                    Return valor
                Case "g/cm3"
                    Return valor / 1000000.0 * 1000
                Case "g/mL"
                    Return valor / 1000000.0 * 1000
                Case "lbm/ft3"
                    Return valor * 0.062428
                Case "m2/s"                              'k.visc
                    Return valor
                Case "ft/s2"
                    Return valor * 3.28084
                Case "cm2/s"
                    Return valor * 10000.0
                Case "W/[m2.K]"                              'HTC
                    Return valor
                Case "BTU/[ft2.h.R]"
                    Return valor * 0.17611
                Case "cal/[cm.s.°C]"
                    Return valor * 0.0000238846
                Case "cal/[cm.s.C]"
                    Return valor * 0.0000238846
                Case "m3/kg"                                'vol especif
                    Return valor
                Case "ft3/lbm"
                    Return valor / 0.062428
                Case "cm3/g"
                    Return valor * 1000
                Case "kmol/[m3.s]"
                    Return valor / 1000                            'taxa reacao
                Case "kmol/[m3.min.]"
                    Return valor * 60 / 1000
                Case "kmol/[m3.h]"
                    Return valor * 3600 / 1000
                Case "mol/[m3.s]"
                    Return valor
                Case "mol/[m3.min.]"
                    Return valor * 60
                Case "mol/[m3.h]"
                    Return valor * 3600
                Case "mol/[L.s]"
                    Return valor * 1000
                Case "mol/[L.min.]"
                    Return valor * 60 * 1000
                Case "mol/[L.h]"
                    Return valor * 3600 * 1000
                Case "mol/[cm3.s]"
                    Return valor / 1000000.0
                Case "mol/[cm3.min.]"
                    Return valor * 60 / 1000000.0
                Case "mol/[cm3.h]"
                    Return valor * 3600 / 1000000.0
                Case "lbmol.[ft3.h]"
                    Return valor * 3600 / 35.3147 / 1000
                Case "°C"                               'temperatura e demais
                    Return valor - 273.15
                Case "C"                               'temperatura e demais
                    Return valor - 273.15
                Case "°C."
                    Return valor
                Case "C."
                    Return valor
                Case "atm"
                    Return valor / 101325
                Case "g/s"
                    Return valor * 1000
                Case "mol/s"
                    Return valor
                Case "cal/g"
                    Return valor * 0.238846
                Case "g/cm3"
                    Return valor / 1000
                Case "dyn/cm"
                    Return valor / 0.001
                Case "dyn/cm2"
                    Return valor / 0.001
                Case "cal/[cm.s.°C]"
                    Return valor * 0.00238846
                Case "cal/[cm.s.C]"
                    Return valor * 0.00238846
                Case "cm3/s"
                    Return valor / 0.000001
                Case "cal/[g.°C]"
                    Return valor * 0.238846
                Case "cal/[g.C]"
                    Return valor * 0.238846
                Case "cSt"
                    Return valor * 1000000.0
                Case "mm2/s"
                    Return valor * 1000000.0
                Case "Pa.s"
                    Return valor
                Case "cP"
                    Return valor * 1000
                Case "kcal/h"
                    Return valor * 859.845
                Case "R"
                    Return valor * 1.8
                Case "R."
                    Return valor * 1.8
                Case "lbf/ft2"
                    Return valor * 0.0208854
                Case "lbm/h"
                    Return valor * 7936.64
                Case "lbmol/h"
                    Return valor / 453.59237 * 3600
                Case "BTU/lbm"
                    Return valor * 0.429923
                Case "lbf/in"
                    Return valor * 0.00571015
                Case "BTU/[ft.h.R]"
                    Return valor * 0.577789
                Case "ft3/s"
                    Return valor * 35.3147
                Case "BTU/[lbm.R]"
                    Return valor * 0.238846
                Case "ft2/s"
                    Return valor * 10.7639
                Case "lbm/[ft.s]"
                    Return valor * 0.671969
                Case "BTU/h"
                    Return valor * 3412.14
                Case "ft"
                    Return valor * 3.28084
                    'Personalizados
                Case "kgf/cm2_a"
                    Return valor / 101325 * 1.033
                Case "kgf/cm2"
                    Return valor / 101325 * 1.033
                Case "kgf/cm2_g"
                    Return valor * 1.033 / 101325 - 1.033
                Case "kg/h"
                    Return valor * 3600
                Case "kg/d"
                    Return valor * 3600 * 24
                Case "m3/h"
                    Return valor * 3600
                Case "m3/d"
                    Return valor * 3600 * 24
                Case "m3/d @ BR", "m3/d @ 20 C, 1 atm"
                    Return valor * (24.055 * 3600 * 24 / 1000)
                Case "m3/d @ CNTP"
                    Return valor * (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ NC", "m3/d @ 0 C, 1 atm"
                    Return valor * (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ SC", "m3/d @ 15.56 C, 1 atm"
                    Return valor * (23.69 * 3600 * 24 / 1000)
                Case "ft3/d @ 60 F, 14.7 psia"
                    Return valor * (23.69 * 3600 * 24 / 1000) * 35.3147
                Case "ft3/d @ 0 C, 1 atm"
                    Return valor * (22.71 * 3600 * 24 / 1000) * 35.3147
                Case "°F"
                    Return (valor - 273.15) * 9 / 5 + 32
                Case "°F."
                    Return valor * 1.8
                Case "F"
                    Return (valor - 273.15) * 9 / 5 + 32
                Case "F."
                    Return valor * 1.8
                Case "cm"
                    Return valor * 100
                Case "cal/[mol.°C]"
                    Return valor / 238.846 * 1000
                Case "cal/[mol.C]"
                    Return valor / 238.846 * 1000
                Case "BTU/[lbmol.R]"
                    Return valor / 1.8 / 0.947817
                Case Else
                    Return valor
            End Select

        End Function

        Public Shared Function Convert(fromunit As String, tounit As String, value As Double) As Double

            Dim sival As Double = ConverterParaSI(fromunit, value)
            Return Double.Parse(ConverterDoSI(tounit, sival))

        End Function

        Public Sub New()

        End Sub

    End Class

End Namespace
