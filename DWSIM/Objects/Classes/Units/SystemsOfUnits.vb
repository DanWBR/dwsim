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

Namespace DWSIM.SystemsOfUnits

    <System.Serializable()> Public Class Units

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public nome As String

        Public mass As String = "kg", reac_rate_heterog As String = "mol/[kg.s]", area, distance,
            time, volume, molar_volume, diameter, thickness, molar_conc, mass_conc,
            heat_transf_coeff, force, accel, spec_vol, reac_rate, velocity, foulingfactor,
            cakeresistance, mediumresistance As String

        Public gor As String = "m3/m3"

        Public molar_enthalpy, molar_entropy As String

        Public idealGasHeatCapacity As String
        Public surfaceTension As String
        Public thermalConductivityOfLiquid As String
        Public thermalConductivityOfVapor As String
        Public vaporPressure As String
        Public viscosityOfLiquid As String
        Public viscosityOfVapor As String

        Public pdp_boilingPointTemperature As String
        Public pdp_meltingTemperature As String

        Public activity As String
        Public activityCoefficient As String
        Public compressibility As String
        Public compressibilityFactor As String
        Public density As String
        Public enthalpy As String
        Public entropy As String
        Public excessEnthalpy As String
        Public excessEntropy As String
        Public molarflow As String
        Public massflow As String
        Public molarfraction As String
        Public massfraction As String
        Public fugacity As String
        Public fugacityCoefficient As String
        Public heatCapacityCp As String
        Public heatCapacityCv As String
        Public jouleThomsonCoefficient As String
        Public logFugacityCoefficient As String
        Public molecularWeight As String
        Public pressure As String
        Public temperature As String
        Public speedOfSound As String
        Public thermalConductivity As String
        Public viscosity As String
        Public cinematic_viscosity As String
        Public volumetricFlow As String
        Public heatflow As String
        Public head As String
        Public deltaT As String
        Public deltaP As String

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

    <System.Serializable()> Public Class SI

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/s"
                .massfraction = "-"
                .molarflow = "mol/s"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "Pa"
                .speedOfSound = "m/s"
                .temperature = "K"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "Pa.s"
                .volumetricFlow = "m3/s"
                .cinematic_viscosity = "m2/s"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "Pa"
                .viscosityOfLiquid = "Pa.s"
                .viscosityOfVapor = "Pa.s"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "Pa"
                .deltaT = "K"

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom1

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/h"
                .massfraction = "-"
                .molarflow = "m3/d @ BR"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "kgf/cm2_a"
                .speedOfSound = "m/s"
                .temperature = "C"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "cP"
                .volumetricFlow = "m3/h"
                .cinematic_viscosity = "cSt"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "kgf/cm2_a"
                .viscosityOfLiquid = "cP"
                .viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "kgf/cm2"
                .deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom2

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/h"
                .massfraction = "-"
                .molarflow = "m3/d @ SC"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "kgf/cm2_a"
                .speedOfSound = "m/s"
                .temperature = "C"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "cP"
                .volumetricFlow = "m3/h"
                .cinematic_viscosity = "cSt"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "kgf/cm2_a"
                .viscosityOfLiquid = "cP"
                .viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "kgf/cm2"
                .deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom3

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/h"
                .massfraction = "-"
                .molarflow = "m3/d @ CNTP"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "kgf/cm2_a"
                .speedOfSound = "m/s"
                .temperature = "C"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "cP"
                .volumetricFlow = "m3/h"
                .cinematic_viscosity = "cSt"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "kgf/cm2_a"
                .viscosityOfLiquid = "cP"
                .viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "kgf/cm2"
                .deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom4

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/d"
                .massfraction = "-"
                .molarflow = "m3/d @ SC"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "kPa"
                .speedOfSound = "m/s"
                .temperature = "C"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "cP"
                .volumetricFlow = "m3/d"
                .cinematic_viscosity = "cSt"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "kPa"
                .viscosityOfLiquid = "cP"
                .viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kJ/d"
                .head = "m"
                .deltaP = "kgf/cm2"
                .deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom5

        Inherits Units

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
                .activity = "Pa"
                .activityCoefficient = "-"
                .compressibility = "Pa-1"
                .compressibilityFactor = "-"
                .density = "kg/m3"
                .enthalpy = "kJ/kg"
                .entropy = "kJ/[kg.K]"
                .excessEnthalpy = "kJ/kg"
                .excessEntropy = "kJ/[kg.K]"
                .fugacity = "Pa"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "kJ/[kg.K]"
                .heatCapacityCv = "kJ/[kg.K]"
                .jouleThomsonCoefficient = "K/Pa"
                .logFugacityCoefficient = "-"
                .massflow = "kg/h"
                .massfraction = "-"
                .molarflow = "kmol/h"
                .molarfraction = "-"
                .molecularWeight = "kg/kmol"
                .pressure = "bar"
                .speedOfSound = "m/s"
                .temperature = "°C"
                .thermalConductivity = "W/[m.K]"
                .viscosity = "Pa.s"
                .volumetricFlow = "m3/h"
                .cinematic_viscosity = "m2/s"
                .idealGasHeatCapacity = "kJ/[kg.K]"
                .surfaceTension = "N/m"
                .thermalConductivityOfLiquid = "W/[m.K]"
                .thermalConductivityOfVapor = "W/[m.K]"
                .vaporPressure = "bar"
                .viscosityOfLiquid = "Pa.s"
                .viscosityOfVapor = "Pa.s"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "bar"
                .deltaT = "C."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class English

        Inherits Units

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
                .activity = "lbf/ft2"
                .activityCoefficient = "-"
                .compressibility = "ft2/lbf"
                .compressibilityFactor = "-"
                .density = "lbm/ft3"
                .enthalpy = "BTU/lbm"
                .entropy = "BTU/[lbm.R]"
                .excessEnthalpy = "BTU/lbm"
                .excessEntropy = "BTU/[lbm.R]"
                .fugacity = "lbf/ft2"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "BTU/[lbm.R]"
                .heatCapacityCv = "BTU/[lbm.R]"
                .jouleThomsonCoefficient = "R/[lbf/ft2]"
                .logFugacityCoefficient = "-"
                .massflow = "lbm/h"
                .massfraction = "-"
                .molarflow = "lbmol/h"
                .molarfraction = "-"
                .molecularWeight = "lbm/lbmol"
                .pressure = "lbf/ft2"
                .speedOfSound = "ft/s"
                .temperature = "R"
                .thermalConductivity = "BTU/[ft.h.R]"
                .viscosity = "lbm/[ft.h]"
                .idealGasHeatCapacity = "BTU/[lbm.R]"
                .surfaceTension = "lbf/in"
                .thermalConductivityOfLiquid = "BTU/[ft.h.R]"
                .thermalConductivityOfVapor = "BTU/[ft.h.R]"
                .vaporPressure = "lbf/ft2"
                .viscosityOfLiquid = "lbm/[ft.h]"
                .viscosityOfVapor = "lbm/[ft.h]"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "lbf/in"
                .volumetricFlow = "ft3/s"
                .cinematic_viscosity = "ft2/s"
                .heatflow = "BTU/h"
                .head = "ft"
                .deltaP = "lbf/ft2"
                .deltaT = "R."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class CGS

        Inherits Units

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
                .activity = "atm"
                .activityCoefficient = "-"
                .compressibility = "atm-1"
                .compressibilityFactor = "-"
                .density = "g/cm3"
                .enthalpy = "cal/g"
                .entropy = "cal/[g.C]"
                .excessEnthalpy = "cal/g"
                .excessEntropy = "cal/[g.C]"
                .fugacity = "atm"
                .fugacityCoefficient = "-"
                .heatCapacityCp = "cal/[g.C]"
                .heatCapacityCv = "cal/[g.C]"
                .jouleThomsonCoefficient = "C/atm"
                .logFugacityCoefficient = "-"
                .massflow = "g/s"
                .massfraction = "-"
                .molarflow = "mol/s"
                .molarfraction = "-"
                .molecularWeight = "g/mol"
                .pressure = "atm"
                .speedOfSound = "cm/s"
                .temperature = "C"
                .thermalConductivity = "cal/[cm.s.C]"
                .viscosity = "cP"
                .idealGasHeatCapacity = "cal/[g.C]"
                .surfaceTension = "dyn/cm"
                .thermalConductivityOfLiquid = "cal/[cm.s.C]"
                .thermalConductivityOfVapor = "cal/[cm.s.C]"
                .vaporPressure = "atm"
                .viscosityOfLiquid = "cP"
                .viscosityOfVapor = "cP"
                .tpmp_kvalue = "-"
                .tpmp_logKvalue = "-"
                .tpmp_surfaceTension = "dyn/cm2"
                .volumetricFlow = "cm3/s"
                .cinematic_viscosity = "cSt"
                .heatflow = "kcal/h"
                .head = "m"
                .deltaT = "C."
                .deltaP = "atm"

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class Converter

        Public Shared Function ConvertToSI(ByVal units As String, ByVal value As Double)

            Select Case units

                Case "ft3/bbl"
                    Return value * 0.177295
                Case "g"
                    Return value / 1000
                Case "lb"
                    Return value / 2.20462
                Case "m/s"
                    Return value
                Case "cm/s"
                    Return value / 100
                Case "mm/s"
                    Return value / 1000
                Case "km/h"
                    Return value / 3.6
                Case "ft/h"
                    Return value / 11811
                Case "ft/min"
                    Return value / 196.85
                Case "ft/s"
                    Return value / 3.28084
                Case "in/s"
                    Return value / 39.3701

                Case "kPa"
                    Return value / 0.001
                Case "ftH2O"
                    Return value / 0.000334
                Case "inH2O"
                    Return value / 0.00401463
                Case "inHg"
                    Return value / 0.000295301
                Case "lbf/ft2"
                    Return value / 0.0208854
                Case "mbar"
                    Return value / 0.01
                Case "mH2O"
                    Return value / 0.000101972
                Case "mmH2O"
                    Return value / 0.101972
                Case "mmHg"
                    Return value / 0.00750064
                Case "MPa"
                    Return value / 0.000001
                Case "psi"
                    Return value / 0.000145038
                Case "bar"
                    Return value * 100000
                Case "kPag"
                    Return (value + 101.325) * 1000
                Case "barg"
                    Return (value + 1.01325) * 100000
                Case "kgf/cm2g"
                    Return (value + 1.033) * 101325 / 1.033
                Case "psig"
                    Return (value + 14.696) * 6894.8

                Case "kg/d"
                    Return value / 60 / 60 / 24
                Case "kg/min"
                    Return value / 60
                Case "lb/h"
                    Return value / 7936.64
                Case "lb/min"
                    Return value / 132.277
                Case "lb/s"
                    Return value / 2.20462

                Case "mol/h"
                    Return value / 3600
                Case "mol/d"
                    Return value / 3600 / 24
                Case "kmol/s"
                    Return value * 1000
                Case "kmol/h"
                    Return value * 1000 / 3600
                Case "kmol/d"
                    Return value * 1000 / 3600 / 24

                Case "kmol/[kg.s]"
                    Return value * 1000
                Case "kmol/[kg.min.]"
                    Return value / 60 * 1000
                Case "kmol/[kg.h]"
                    Return value / 60 / 60 * 1000
                Case "mol/[kg.s]"
                    Return value
                Case "mol/[kg.min.]"
                    Return value / 60
                Case "mol/[kg.h]"
                    Return value / 60 / 60
                Case "lbmol/[lbm.h]"
                    Return value * 453.59237 * 2.20462 / 60 / 60

                Case "bbl/h"
                    Return value / 22643.3
                Case "bbl/d"
                    Return value / 543440
                Case "ft3/min"
                    Return value / 35.3147 / 60
                Case "ft3/d"
                    Return value / 35.3147 / 60 / 60 / 24
                Case "ft3/s"
                    Return value / 35.3147
                Case "gal[UK]/h"
                    Return value / 791889
                Case "gal[UK]/s"
                    Return value / 219.969
                Case "gal[US]/h"
                    Return value / 951019
                Case "gal[US]/min"
                    Return value / 15850.3
                Case "L/h"
                    Return value / 3600000.0
                Case "L/min"
                    Return value / 60000
                Case "L/s"
                    Return value / 1000
                Case "m3/d"
                    Return value / 3600 / 24

                Case "BTU/h"
                    Return value / 3412.14
                Case "BTU/s"
                    Return value / 0.947817
                Case "cal/s"
                    Return value / 238.846
                Case "HP"
                    Return value / 1.35962
                Case "kcal/h"
                    Return value / 859.845
                Case "kJ/h"
                    Return value / 3600
                Case "kJ/d"
                    Return value / 3600 / 24
                Case "MW"
                    Return value / 0.001
                Case "W"
                    Return value / 1000

                Case "BTU/lb"
                    Return value / 0.429923
                Case "cal/g"
                    Return value / 0.238846
                Case "kcal/kg"
                    Return value / 0.238846

                Case "kJ/kmol"
                    Return value
                Case "cal/mol"
                    Return value * 0.0041868 * 1000
                Case "BTU/lbmol"
                    Return value * 1.05506 * 1000
                Case "kJ/[kmol.K]"
                    Return value * 1
                Case "cal/[mol.°C]"
                    Return value * 0.0041868 * 1000
                Case "cal/[mol.C]"
                    Return value * 0.0041868 * 1000
                Case "BTU/[lbmol.R]"
                    Return value * 1.05506 * 1000

                Case "K.m2/W"                           'fouling factor
                    Return value
                Case "C.cm2.s/cal"
                    Return value * 0.000023885
                Case "ft2.h.F/BTU"
                    Return value * 0.17611

                Case "m/kg"
                    Return value
                Case "ft/lb"
                    Return value / 3.28084 * 2.20462
                Case "cm/g"
                    Return value * 10
                Case "m-1"
                    Return value
                Case "ft-1"
                    Return value * 3.28084
                Case "cm-1"
                    Return value * 100

                Case "m2"                               'area
                    Return value
                Case "cm2"
                    Return value / 10000.0
                Case "ft2"
                    Return value / 10.7639
                Case "h"                                'tempo
                    Return value * 3600
                Case "s"
                    Return value
                Case "min."
                    Return value * 60
                Case "ft3"                              'volume
                    Return value / 35.3147
                Case "m3"
                    Return value
                Case "cm3"
                    Return value / 1000000.0
                Case "L"
                    Return value / 1000.0
                Case "cm3/mol"                          'volume molar'
                    Return value / 1000.0
                Case "m3/kmol"
                    Return value
                Case "m3/mol"
                    Return value * 1000
                Case "ft3/lbmol"
                    Return value / 35.3147 / 1000
                Case "mm"                               'comprimento'
                    Return value / 1000
                Case "in.", "in"
                    Return value / 39.3701
                Case "dyn"                              'força
                    Return value / 100000
                Case "N"
                    Return value
                Case "lbf"
                    Return value / 0.224809
                Case "mol/L"                            'conc molar
                    Return value
                Case "kmol/m3"
                    Return value
                Case "mol/cm3"
                    Return value * 1000000.0 / 1000
                Case "mol/mL"
                    Return value * 1000000.0 / 1000
                Case "lbmol/ft3"
                    Return value * 35.3147 * 1000
                Case "g/L"                              'conc mássica
                    Return value
                Case "kg/m3"
                    Return value
                Case "g/cm3"
                    Return value * 1000000.0 / 1000
                Case "g/mL"
                    Return value * 1000000.0 / 1000
                Case "m2/s"                              'k.visc
                    Return value
                Case "ft/s2"
                    Return value / 3.28084
                Case "cm2/s"
                    Return value / 10000.0
                Case "W/[m2.K]"                              'HTC
                    Return value
                Case "BTU/[ft2.h.R]"
                    Return value / 0.17611
                Case "cal/[cm.s.°C]"
                    Return value / 0.0000238846
                Case "cal/[cm.s.C]"
                    Return value / 0.0000238846
                Case "m3/kg"                                'vol especif
                    Return value
                Case "ft3/lbm"
                    Return value * 0.062428
                Case "cm3/g"
                    Return value / 1000
                Case "kmol/[m3.s]"
                    Return value * 1000                            'taxa reacao
                Case "kmol/[m3.min.]"
                    Return value / 60 * 1000
                Case "kmol/[m3.h]"
                    Return value / 3600 * 1000
                Case "mol/[m3.s]"
                    Return value
                Case "mol/[m3.min.]"
                    Return value / 60
                Case "mol/[m3.h]"
                    Return value / 3600
                Case "mol/[L.s]"
                    Return value / 1000
                Case "mol/[L.min.]"
                    Return value / 60 / 1000
                Case "mol/[L.h]"
                    Return value / 3600 / 1000
                Case "mol/[cm3.s]"
                    Return value * 1000000.0
                Case "mol/[cm3.min.]"
                    Return value / 60 * 1000000.0
                Case "mol/[cm3.h]"
                    Return value / 3600 * 1000000.0
                Case "lbmol.[ft3.h]"
                    Return value / 3600 * 35.3147 * 1000
                Case "°C"                               'temperatura e demais
                    Return value + 273.15
                Case "C"                               'temperatura e demais
                    Return value + 273.15
                Case "°C."
                    Return value
                Case "C."
                    Return value
                Case "atm"
                    Return value * 101325
                Case "g/s"
                    Return value / 1000
                Case "mol/s"
                    Return value
                Case "kmol/s"
                    Return value * 1000
                Case "cal/g"
                    Return value / 0.238846
                Case "g/cm3"
                    Return value * 1000
                Case "dyn/cm"
                    Return value * 0.001
                Case "dyn/cm2"
                    Return value * 0.001
                Case "cal/[cm.s.°C]"
                    Return value / 0.00238846
                Case "cal/[cm.s.C]"
                    Return value / 0.00238846
                Case "cm3/s"
                    Return value * 0.000001
                Case "cal/[g.°C]"
                    Return value / 0.238846
                Case "cal/[g.C]"
                    Return value / 0.238846
                Case "cSt"
                    Return value / 1000000.0
                Case "mm2/s"
                    Return value / 1000000.0
                Case "Pa.s"
                    Return value
                Case "cP"
                    Return value / 1000
                Case "kcal/h"
                    Return value / 859.845
                Case "m"
                    Return value
                Case "R"
                    Return value / 1.8
                Case "R."
                    Return value / 1.8
                Case "lbf/ft2"
                    Return value / 0.0208854
                Case "lbm/h"
                    Return value / 7936.64
                Case "lbmol/h"
                    Return value * 453.59237 / 3600
                Case "BTU/lbm"
                    Return value / 0.429923
                Case "lbm/ft3"
                    Return value / 0.062428
                Case "lbf/in"
                    Return value / 0.00571015
                Case "BTU/[ft.h.R]"
                    Return value / 0.577789
                Case "ft3/s"
                    Return value / 35.3147
                Case "BTU/[lbm.R]"
                    Return value / 0.238846
                Case "ft2/s"
                    Return value / 10.7639
                Case "lbm/[ft.s]"
                    Return value / 0.671969
                Case "BTU/h"
                    Return value / 3412.14
                Case "ft"
                    Return value / 3.28084
                    'Personalizados
                Case "kgf/cm2_a"
                    Return value * 101325 / 1.033
                Case "kgf/cm2"
                    Return value * 101325 / 1.033
                Case "kgf/cm2_g"
                    Return (value + 1.033) * 101325 / 1.033
                Case "kg/h"
                    Return value / 3600
                Case "kg/d"
                    Return value / 3600 / 24
                Case "m3/h"
                    Return value / 3600
                Case "m3/d"
                    Return value / 3600 / 24
                Case "m3/d @ BR", "m3/d @ 20 C, 1 atm"
                    Return value / (24.055 * 3600 * 24 / 1000)
                Case "m3/d @ CNTP"
                    Return value / (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ NC", "m3/d @ 0 C, 1 atm"
                    Return value / (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ SC", "m3/d @ 15.56 C, 1 atm"
                    Return value / (23.69 * 3600 * 24 / 1000)
                Case "ft3/d @ 60 F, 14.7 psia"
                    Return value / (23.69 * 3600 * 24 / 1000) / 35.3147
                Case "ft3/d @ 0 C, 1 atm"
                    Return value / (22.71 * 3600 * 24 / 1000) / 35.3147
                Case "°F"
                    Return (value - 32) * 5 / 9 + 273.15
                Case "°F."
                    Return value / 1.8
                Case "F"
                    Return (value - 32) * 5 / 9 + 273.15
                Case "F."
                    Return value / 1.8
                Case "cm"
                    Return value / 100
                Case "cal/[mol.°C]"
                    Return value * 238.846 / 1000
                Case "cal/[mol.C]"
                    Return value * 238.846 / 1000
                Case "BTU/[lbmol.R]"
                    Return value * 1.8 * 0.947817
                Case Else
                    Return value
            End Select

        End Function

        Public Shared Function ConvertFromSI(ByVal units As String, ByVal value As Double)

            Select Case units

                Case "ft3/bbl"
                    Return value / 0.177295
                Case "g"
                    Return value * 1000
                Case "lb"
                    Return value * 2.20462
                Case "m/s"
                    Return value
                Case "cm/s"
                    Return value * 100
                Case "mm/s"
                    Return value * 1000
                Case "km/h"
                    Return value * 3.6
                Case "ft/h"
                    Return value * 11811
                Case "ft/min"
                    Return value * 196.85
                Case "ft/s"
                    Return value * 3.28084
                Case "in/s"
                    Return value * 39.3701

                Case "kPa"
                    Return value * 0.001
                Case "ftH2O"
                    Return value * 0.000334
                Case "inH2O"
                    Return value * 0.00401463
                Case "inHg"
                    Return value * 0.000295301
                Case "lbf/ft2"
                    Return value * 0.0208854
                Case "mbar"
                    Return value * 0.01
                Case "mH2O"
                    Return value * 0.000101972
                Case "mmH2O"
                    Return value * 0.101972
                Case "mmHg"
                    Return value * 0.00750064
                Case "MPa"
                    Return value * 0.000001
                Case "psi"
                    Return value * 0.000145038
                Case "bar"
                    Return value / 100000
                Case "kPag"
                    Return (value - 101325) / 1000
                Case "barg"
                    Return (value - 101325) / 100000
                Case "kgf/cm2g"
                    Return (value - 101325) / 101325 * 1.033
                Case "psig"
                    Return (value - 101325) * 0.000145038

                Case "kg/d"
                    Return value * 60 * 60 * 24
                Case "kg/min"
                    Return value * 60
                Case "lb/h"
                    Return value * 7936.64
                Case "lb/min"
                    Return value * 132.277
                Case "lb/s"
                    Return 2.20462

                Case "bbl/h"
                    Return value * 22643.3
                Case "bbl/d"
                    Return value * 543440
                Case "ft3/min"
                    Return value * 35.3147 * 60
                Case "ft3/d"
                    Return value * 35.3147 * 60 * 60 * 24
                Case "ft3/s"
                    Return value * 35.3147
                Case "gal[UK]/h"
                    Return value * 791889
                Case "gal[UK]/s"
                    Return value * 219.969
                Case "gal[US]/h"
                    Return value * 951019
                Case "gal[US]/min"
                    Return value * 15850.3
                Case "L/h"
                    Return value * 3600000.0
                Case "L/min"
                    Return value * 60000
                Case "L/s"
                    Return value * 1000
                Case "m3/d"
                    Return value * 3600 * 24

                Case "mol/h"
                    Return value * 3600
                Case "mol/d"
                    Return value * 3600 * 24
                Case "kmol/s"
                    Return value / 1000
                Case "kmol/h"
                    Return value / 1000 * 3600
                Case "kmol/d"
                    Return value / 1000 * 3600 * 24

                Case "kmol/[kg.s]"
                    Return value / 1000
                Case "kmol/[kg.min.]"
                    Return value * 60 / 1000
                Case "kmol/[kg.h]"
                    Return value * 60 * 60 / 1000
                Case "mol/[kg.s]"
                    Return value
                Case "mol/[kg.min.]"
                    Return value * 60
                Case "mol/[kg.h]"
                    Return value * 60 * 60
                Case "lbmol/[lbm.h]"
                    Return value * 453.59237 * 2.20462 * 60 * 60

                Case "BTU/h"
                    Return value * 3412.14
                Case "BTU/s"
                    Return value * 0.947817
                Case "cal/s"
                    Return value * 238.846
                Case "HP"
                    Return value * 1.35962
                Case "kcal/h"
                    Return value * 859.845
                Case "kJ/h"
                    Return value * 3600
                Case "kJ/d"
                    Return value * 3600 * 24
                Case "MW"
                    Return value * 0.001
                Case "W"
                    Return value * 1000

                Case "BTU/lb"
                    Return value * 0.429923
                Case "cal/g"
                    Return value * 0.238846
                Case "kcal/kg"
                    Return value * 0.238846

                Case "kJ/kmol"
                    Return value
                Case "cal/mol"
                    Return value / 0.0041868 / 1000
                Case "BTU/lbmol"
                    Return value / 1.05506 / 1000
                Case "kJ/[kmol.K]"
                    Return value
                Case "cal/[mol.°C]"
                    Return value / 0.0041868 / 1000
                Case "cal/[mol.C]"
                    Return value / 0.0041868 / 1000
                Case "BTU/[lbmol.R]"
                    Return value / 1.05506 / 1000

                Case "K.m2/W"                           'fouling factor
                    Return value
                Case "C.cm2.s/cal"
                    Return value / 0.000023885
                Case "ft2.h.F/BTU"
                    Return value / 0.17611

                Case "m/kg"
                    Return value
                Case "ft/lb"
                    Return value * 3.28084 / 2.20462
                Case "cm/g"
                    Return value / 10
                Case "m-1"
                    Return value
                Case "ft-1"
                    Return value / 3.28084
                Case "cm-1"
                    Return value / 100

                Case "m2"                               'area
                    Return value
                Case "cm2"
                    Return value * 10000.0
                Case "ft2"
                    Return value * 10.7639
                Case "h"                                'tempo
                    Return value / 3600
                Case "s"
                    Return value
                Case "min."
                    Return value / 60
                Case "ft3"                              'volume
                    Return value * 35.3147
                Case "m3"
                    Return value
                Case "cm3"
                    Return value * 1000000.0
                Case "L"
                    Return value * 1000.0
                Case "cm3/mol"                          'volume molar'
                    Return value * 1000.0
                Case "m3/kmol"
                    Return value
                Case "m3/mol"
                    Return value / 1000
                Case "ft3/lbmol"
                    Return value * 35.3147 * 1000
                Case "mm"                               'comprimento'
                    Return value * 1000
                Case "in.", "in"
                    Return value * 39.3701
                Case "dyn"                              'força
                    Return value * 100000
                Case "N"
                    Return value
                Case "lbf"
                    Return value * 0.224809
                Case "mol/L"                            'conc molar
                    Return value
                Case "kmol/m3"
                    Return value
                Case "mol/cm3"
                    Return value / 1000000.0 * 1000
                Case "mol/mL"
                    Return value / 1000000.0 * 1000
                Case "lbmol/ft3"
                    Return value * 35.3147 * 1000
                Case "g/L"                              'conc mássica
                    Return value
                Case "kg/m3"
                    Return value
                Case "g/cm3"
                    Return value / 1000000.0 * 1000
                Case "g/mL"
                    Return value / 1000000.0 * 1000
                Case "lbm/ft3"
                    Return value * 0.062428
                Case "m2/s"                              'k.visc
                    Return value
                Case "ft/s2"
                    Return value * 3.28084
                Case "cm2/s"
                    Return value * 10000.0
                Case "W/[m2.K]"                              'HTC
                    Return value
                Case "BTU/[ft2.h.R]"
                    Return value * 0.17611
                Case "cal/[cm.s.°C]"
                    Return value * 0.0000238846
                Case "cal/[cm.s.C]"
                    Return value * 0.0000238846
                Case "m3/kg"                                'vol especif
                    Return value
                Case "ft3/lbm"
                    Return value / 0.062428
                Case "cm3/g"
                    Return value * 1000
                Case "kmol/[m3.s]"
                    Return value / 1000                            'taxa reacao
                Case "kmol/[m3.min.]"
                    Return value * 60 / 1000
                Case "kmol/[m3.h]"
                    Return value * 3600 / 1000
                Case "mol/[m3.s]"
                    Return value
                Case "mol/[m3.min.]"
                    Return value * 60
                Case "mol/[m3.h]"
                    Return value * 3600
                Case "mol/[L.s]"
                    Return value * 1000
                Case "mol/[L.min.]"
                    Return value * 60 * 1000
                Case "mol/[L.h]"
                    Return value * 3600 * 1000
                Case "mol/[cm3.s]"
                    Return value / 1000000.0
                Case "mol/[cm3.min.]"
                    Return value * 60 / 1000000.0
                Case "mol/[cm3.h]"
                    Return value * 3600 / 1000000.0
                Case "lbmol.[ft3.h]"
                    Return value * 3600 / 35.3147 / 1000
                Case "°C"                               'temperatura e demais
                    Return value - 273.15
                Case "C"                               'temperatura e demais
                    Return value - 273.15
                Case "°C."
                    Return value
                Case "C."
                    Return value
                Case "atm"
                    Return value / 101325
                Case "g/s"
                    Return value * 1000
                Case "mol/s"
                    Return value
                Case "cal/g"
                    Return value * 0.238846
                Case "g/cm3"
                    Return value / 1000
                Case "dyn/cm"
                    Return value / 0.001
                Case "dyn/cm2"
                    Return value / 0.001
                Case "cal/[cm.s.°C]"
                    Return value * 0.00238846
                Case "cal/[cm.s.C]"
                    Return value * 0.00238846
                Case "cm3/s"
                    Return value / 0.000001
                Case "cal/[g.°C]"
                    Return value * 0.238846
                Case "cal/[g.C]"
                    Return value * 0.238846
                Case "cSt"
                    Return value * 1000000.0
                Case "mm2/s"
                    Return value * 1000000.0
                Case "Pa.s"
                    Return value
                Case "cP"
                    Return value * 1000
                Case "kcal/h"
                    Return value * 859.845
                Case "R"
                    Return value * 1.8
                Case "R."
                    Return value * 1.8
                Case "lbf/ft2"
                    Return value * 0.0208854
                Case "lbm/h"
                    Return value * 7936.64
                Case "lbmol/h"
                    Return value / 453.59237 * 3600
                Case "BTU/lbm"
                    Return value * 0.429923
                Case "lbf/in"
                    Return value * 0.00571015
                Case "BTU/[ft.h.R]"
                    Return value * 0.577789
                Case "ft3/s"
                    Return value * 35.3147
                Case "BTU/[lbm.R]"
                    Return value * 0.238846
                Case "ft2/s"
                    Return value * 10.7639
                Case "lbm/[ft.s]"
                    Return value * 0.671969
                Case "BTU/h"
                    Return value * 3412.14
                Case "ft"
                    Return value * 3.28084
                    'Personalizados
                Case "kgf/cm2_a"
                    Return value / 101325 * 1.033
                Case "kgf/cm2"
                    Return value / 101325 * 1.033
                Case "kgf/cm2_g"
                    Return value * 1.033 / 101325 - 1.033
                Case "kg/h"
                    Return value * 3600
                Case "kg/d"
                    Return value * 3600 * 24
                Case "m3/h"
                    Return value * 3600
                Case "m3/d"
                    Return value * 3600 * 24
                Case "m3/d @ BR", "m3/d @ 20 C, 1 atm"
                    Return value * (24.055 * 3600 * 24 / 1000)
                Case "m3/d @ CNTP"
                    Return value * (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ NC", "m3/d @ 0 C, 1 atm"
                    Return value * (22.71 * 3600 * 24 / 1000)
                Case "m3/d @ SC", "m3/d @ 15.56 C, 1 atm"
                    Return value * (23.69 * 3600 * 24 / 1000)
                Case "ft3/d @ 60 F, 14.7 psia"
                    Return value * (23.69 * 3600 * 24 / 1000) * 35.3147
                Case "ft3/d @ 0 C, 1 atm"
                    Return value * (22.71 * 3600 * 24 / 1000) * 35.3147
                Case "°F"
                    Return (value - 273.15) * 9 / 5 + 32
                Case "°F."
                    Return value * 1.8
                Case "F"
                    Return (value - 273.15) * 9 / 5 + 32
                Case "F."
                    Return value * 1.8
                Case "cm"
                    Return value * 100
                Case "cal/[mol.°C]"
                    Return value / 238.846 * 1000
                Case "cal/[mol.C]"
                    Return value / 238.846 * 1000
                Case "BTU/[lbmol.R]"
                    Return value / 1.8 / 0.947817
                Case Else
                    Return value
            End Select

        End Function

        Public Shared Function Convert(fromunit As String, tounit As String, value As Double) As Double

            Dim sival As Double = ConvertToSI(fromunit, value)
            Return Double.Parse(ConvertFromSI(tounit, sival))

        End Function

        Public Sub New()

        End Sub

    End Class

End Namespace
