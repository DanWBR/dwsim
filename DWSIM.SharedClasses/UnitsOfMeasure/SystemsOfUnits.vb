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

Namespace SystemsOfUnits

    <System.Serializable()> Public Class Units

        Implements Interfaces.ICustomXMLSerialization

        Implements Interfaces.IUnitsOfMeasure

        Public Sub New()

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData
            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData
            Return XMLSerializer.XMLSerializer.Serialize(Me)
        End Function

        Public Property accel As String Implements Interfaces.IUnitsOfMeasure.accel

        Public Property activity As String Implements Interfaces.IUnitsOfMeasure.activity

        Public Property activityCoefficient As String Implements Interfaces.IUnitsOfMeasure.activityCoefficient

        Public Property area As String Implements Interfaces.IUnitsOfMeasure.area

        Public Property cakeresistance As String Implements Interfaces.IUnitsOfMeasure.cakeresistance

        Public Property cinematic_viscosity As String Implements Interfaces.IUnitsOfMeasure.cinematic_viscosity

        Public Property compressibility As String Implements Interfaces.IUnitsOfMeasure.compressibility

        Public Property compressibilityFactor As String Implements Interfaces.IUnitsOfMeasure.compressibilityFactor

        Public Property deltaP As String Implements Interfaces.IUnitsOfMeasure.deltaP

        Public Property deltaT As String Implements Interfaces.IUnitsOfMeasure.deltaT

        Public Property density As String Implements Interfaces.IUnitsOfMeasure.density

        Public Property diameter As String Implements Interfaces.IUnitsOfMeasure.diameter

        Public Property distance As String Implements Interfaces.IUnitsOfMeasure.distance

        Public Property enthalpy As String Implements Interfaces.IUnitsOfMeasure.enthalpy

        Public Property entropy As String Implements Interfaces.IUnitsOfMeasure.entropy

        Public Property excessEnthalpy As String Implements Interfaces.IUnitsOfMeasure.excessEnthalpy

        Public Property excessEntropy As String Implements Interfaces.IUnitsOfMeasure.excessEntropy

        Public Property force As String Implements Interfaces.IUnitsOfMeasure.force

        Public Property foulingfactor As String Implements Interfaces.IUnitsOfMeasure.foulingfactor

        Public Property fugacity As String Implements Interfaces.IUnitsOfMeasure.fugacity

        Public Property fugacityCoefficient As String Implements Interfaces.IUnitsOfMeasure.fugacityCoefficient

        Public Property gor As String Implements Interfaces.IUnitsOfMeasure.gor

        Public Property head As String Implements Interfaces.IUnitsOfMeasure.head

        Public Property heat_transf_coeff As String Implements Interfaces.IUnitsOfMeasure.heat_transf_coeff

        Public Property heatCapacityCp As String Implements Interfaces.IUnitsOfMeasure.heatCapacityCp

        Public Property heatCapacityCv As String Implements Interfaces.IUnitsOfMeasure.heatCapacityCv

        Public Property heatflow As String Implements Interfaces.IUnitsOfMeasure.heatflow

        Public Property idealGasHeatCapacity As String Implements Interfaces.IUnitsOfMeasure.idealGasHeatCapacity

        Public Property jouleThomsonCoefficient As String Implements Interfaces.IUnitsOfMeasure.jouleThomsonCoefficient

        Public Property kvalue As String Implements Interfaces.IUnitsOfMeasure.kvalue

        Public Property logFugacityCoefficient As String Implements Interfaces.IUnitsOfMeasure.logFugacityCoefficient

        Public Property logKvalue As String Implements Interfaces.IUnitsOfMeasure.logKvalue

        Public Property mass As String Implements Interfaces.IUnitsOfMeasure.mass

        Public Property mass_conc As String Implements Interfaces.IUnitsOfMeasure.mass_conc

        Public Property massflow As String Implements Interfaces.IUnitsOfMeasure.massflow

        Public Property massfraction As String Implements Interfaces.IUnitsOfMeasure.massfraction

        Public Property mediumresistance As String Implements Interfaces.IUnitsOfMeasure.mediumresistance

        Public Property molar_conc As String Implements Interfaces.IUnitsOfMeasure.molar_conc

        Public Property molar_enthalpy As String Implements Interfaces.IUnitsOfMeasure.molar_enthalpy

        Public Property molar_entropy As String Implements Interfaces.IUnitsOfMeasure.molar_entropy

        Public Property molar_volume As String Implements Interfaces.IUnitsOfMeasure.molar_volume

        Public Property molarflow As String Implements Interfaces.IUnitsOfMeasure.molarflow

        Public Property molarfraction As String Implements Interfaces.IUnitsOfMeasure.molarfraction

        Public Property molecularWeight As String Implements Interfaces.IUnitsOfMeasure.molecularWeight

        Public Property Name As String = "" Implements Interfaces.IUnitsOfMeasure.Name

        Public Property pdp_boilingPointTemperature As String Implements Interfaces.IUnitsOfMeasure.pdp_boilingPointTemperature

        Public Property pdp_meltingTemperature As String Implements Interfaces.IUnitsOfMeasure.pdp_meltingTemperature

        Public Property pressure As String Implements Interfaces.IUnitsOfMeasure.pressure

        Public Property reac_rate As String Implements Interfaces.IUnitsOfMeasure.reac_rate

        Public Property reac_rate_heterog As String Implements Interfaces.IUnitsOfMeasure.reac_rate_heterog

        Public Property spec_vol As String Implements Interfaces.IUnitsOfMeasure.spec_vol

        Public Property speedOfSound As String Implements Interfaces.IUnitsOfMeasure.speedOfSound

        Public Property surfaceTension As String Implements Interfaces.IUnitsOfMeasure.surfaceTension

        Public Property temperature As String Implements Interfaces.IUnitsOfMeasure.temperature

        Public Property thermalConductivity As String Implements Interfaces.IUnitsOfMeasure.thermalConductivity

        Public Property thermalConductivityOfLiquid As String Implements Interfaces.IUnitsOfMeasure.thermalConductivityOfLiquid

        Public Property thermalConductivityOfVapor As String Implements Interfaces.IUnitsOfMeasure.thermalConductivityOfVapor

        Public Property thickness As String Implements Interfaces.IUnitsOfMeasure.thickness

        Public Property time As String Implements Interfaces.IUnitsOfMeasure.time

        Public Property vaporPressure As String Implements Interfaces.IUnitsOfMeasure.vaporPressure

        Public Property velocity As String Implements Interfaces.IUnitsOfMeasure.velocity

        Public Property viscosity As String Implements Interfaces.IUnitsOfMeasure.viscosity

        Public Property viscosityOfLiquid As String Implements Interfaces.IUnitsOfMeasure.viscosityOfLiquid

        Public Property viscosityOfVapor As String Implements Interfaces.IUnitsOfMeasure.viscosityOfVapor

        Public Property volume As String Implements Interfaces.IUnitsOfMeasure.volume

        Public Property volumetricFlow As String Implements Interfaces.IUnitsOfMeasure.volumetricFlow

        Public Property diffusivity As String Implements IUnitsOfMeasure.diffusivity

        Public Function GetUnitSet(measureID As Enums.UnitOfMeasure) As List(Of String) Implements IUnitsOfMeasure.GetUnitSet

            Dim units As New List(Of String)

            Select Case measureID
                Case Enums.UnitOfMeasure.temperature
                    units.AddRange(New String() {"K", "R", "C", "F"})
                Case Enums.UnitOfMeasure.pressure
                    units.AddRange(New String() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
                Case Enums.UnitOfMeasure.massflow
                    units.AddRange(New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"})
                Case Enums.UnitOfMeasure.molarflow
                    units.AddRange(New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})
                Case Enums.UnitOfMeasure.volumetricFlow
                    units.AddRange(New String() {"m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "L/h", "L/min", "L/s"})
                Case Enums.UnitOfMeasure.enthalpy
                    units.AddRange(New String() {"kJ/kg", "cal/g", "BTU/lbm", "kcal/kg"})
                Case Enums.UnitOfMeasure.entropy
                    units.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
                Case Enums.UnitOfMeasure.molecularWeight
                    units.AddRange(New String() {"kg/kmol", "g/mol", "lbm/lbmol"})
                Case Enums.UnitOfMeasure.surfaceTension
                    units.AddRange(New String() {"N/m", "dyn/cm", "lbf/in"})
                Case Enums.UnitOfMeasure.density
                    units.AddRange(New String() {"kg/m3", "g/cm3", "lbm/ft3"})
                Case Enums.UnitOfMeasure.heatCapacityCp
                    units.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
                Case Enums.UnitOfMeasure.thermalConductivity
                    units.AddRange(New String() {"W/[m.K]", "cal/[cm.s.C]", "BTU/[ft.h.R]"})
                Case Enums.UnitOfMeasure.cinematic_viscosity, Enums.UnitOfMeasure.diffusivity
                    units.AddRange(New String() {"m2/s", "cSt", "ft2/s", "mm2/s", "cm2/s"})
                Case Enums.UnitOfMeasure.viscosity
                    units.AddRange(New String() {"kg/[m.s]", "Pa.s", "cP", "lbm/[ft.h]"})
                Case Enums.UnitOfMeasure.deltaP
                    units.AddRange(New String() {"Pa", "atm", "lbf/ft2", "kgf/cm2", "kPa", "bar", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi"})
                Case Enums.UnitOfMeasure.deltaT
                    units.AddRange(New String() {"C.", "K.", "F.", "R."})
                Case Enums.UnitOfMeasure.distance
                    units.AddRange(New String() {"m", "ft", "cm"})
                Case Enums.UnitOfMeasure.heatflow
                    units.AddRange(New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"})
                Case Enums.UnitOfMeasure.time
                    units.AddRange(New String() {"s", "min.", "h"})
                Case Enums.UnitOfMeasure.volume
                    units.AddRange(New String() {"m3", "cm3", "L", "ft3"})
                Case Enums.UnitOfMeasure.molar_volume
                    units.AddRange(New String() {"m3/kmol", "cm3/mmol", "ft3/lbmol"})
                Case Enums.UnitOfMeasure.area
                    units.AddRange(New String() {"m2", "cm2", "ft2"})
                Case Enums.UnitOfMeasure.diameter
                    units.AddRange(New String() {"mm", "in"})
                Case Enums.UnitOfMeasure.force
                    units.AddRange(New String() {"N", "dyn", "kgf", "lbf"})
                Case Enums.UnitOfMeasure.heat_transf_coeff
                    units.AddRange(New String() {"W/[m2.K]", "cal/[cm2.s.C]", "BTU/[ft2.h.R]"})
                Case Enums.UnitOfMeasure.accel
                    units.AddRange(New String() {"m/s2", "cm/s2", "ft/s2"})
                Case Enums.UnitOfMeasure.spec_vol
                    units.AddRange(New String() {"m3/kg", "cm3/g", "ft3/lbm"})
                Case Enums.UnitOfMeasure.molar_conc
                    units.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"})
                Case Enums.UnitOfMeasure.mass_conc
                    units.AddRange(New String() {"kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
                Case Enums.UnitOfMeasure.reac_rate
                    units.AddRange(New String() {"kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol/[ft3.h]"})
                Case Enums.UnitOfMeasure.molar_enthalpy
                    units.AddRange(New String() {"kJ/kmol", "cal/mol", "BTU/lbmol", "J/mol"})
                Case Enums.UnitOfMeasure.molar_entropy
                    units.AddRange(New String() {"kJ/[kmol.K]", "cal/[mol.C]", "BTU/[lbmol.R]"})
                Case Enums.UnitOfMeasure.velocity, Enums.UnitOfMeasure.speedOfSound
                    units.AddRange(New String() {"m/s", "cm/s", "mm/s", "km/h", "ft/h", "ft/min", "ft/s", "in/s"})
                Case Enums.UnitOfMeasure.foulingfactor
                    units.AddRange(New String() {"K.m2/W", "C.cm2.s/cal", "ft2.h.F/BTU"})
                Case Enums.UnitOfMeasure.cakeresistance
                    units.AddRange(New String() {"m/kg", "ft/lbm", "cm/g"})
                Case Enums.UnitOfMeasure.mediumresistance
                    units.AddRange(New String() {"m-1", "cm-1", "ft-1"})
                Case Enums.UnitOfMeasure.mass
                    units.AddRange(New String() {"kg", "g", "lb"})
                Case Enums.UnitOfMeasure.jouleThomsonCoefficient
                    units.AddRange(New String() {"K/Pa", "F/psi", "C/atm"})
                Case Enums.UnitOfMeasure.compressibility
                    units.AddRange(New String() {"1/Pa", "1/atm", "1/kPa", "1/bar", "1/MPa", "1/psi"})
                Case Enums.UnitOfMeasure.reac_rate_heterog
                    units.AddRange(New String() {"kmol/[kg.s]", "kmol/[kg.min.]", "kmol/[kg.h]", "mol/[kg.s]", "mol/[kg.min.]", "mol/[kg.h]", "lbmol/[lbm.h]"})
            End Select

            Return units

        End Function

        Public Function GetUnitType(unit As String) As Enums.UnitOfMeasure Implements IUnitsOfMeasure.GetUnitType

            Select Case unit
                Case "K", "R", "C", "F"
                    Return Enums.UnitOfMeasure.temperature
                Case "Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"
                    Return Enums.UnitOfMeasure.pressure
                Case "g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"
                    Return Enums.UnitOfMeasure.massflow
                Case "mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"
                    Return Enums.UnitOfMeasure.molarflow
                Case "m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "L/h", "L/min", "L/s"
                    Return Enums.UnitOfMeasure.volumetricFlow
                Case "kJ/kg", "cal/g", "BTU/lbm", "kcal/kg"
                    Return Enums.UnitOfMeasure.enthalpy
                Case "kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"
                    Return Enums.UnitOfMeasure.entropy
                Case "kg/kmol", "g/mol", "lbm/lbmol"
                    Return Enums.UnitOfMeasure.molecularWeight
                Case "N/m", "dyn/cm", "lbf/in"
                    Return Enums.UnitOfMeasure.surfaceTension
                Case "kg/m3", "g/cm3", "lbm/ft3"
                    Return Enums.UnitOfMeasure.density
                Case "kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"
                    Return Enums.UnitOfMeasure.heatCapacityCp
                Case "W/[m.K]", "cal/[cm.s.C]", "BTU/[ft.h.R]"
                    Return Enums.UnitOfMeasure.thermalConductivity
                Case "m2/s", "cSt", "ft2/s", "mm2/s", "cm2/s"
                    Return Enums.UnitOfMeasure.cinematic_viscosity
                Case "kg/[m.s]", "Pa.s", "cP", "lbm/[ft.h]"
                    Return Enums.UnitOfMeasure.viscosity
                Case "Pa", "atm", "lbf/ft2", "kgf/cm2", "kPa", "bar", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi"
                    Return Enums.UnitOfMeasure.deltaP
                Case "C.", "K.", "F.", "R."
                    Return Enums.UnitOfMeasure.deltaT
                Case "m", "ft", "cm"
                    Return Enums.UnitOfMeasure.distance
                Case "kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"
                    Return Enums.UnitOfMeasure.heatflow
                Case "s", "min.", "h"
                    Return Enums.UnitOfMeasure.time
                Case "m3", "cm3", "L", "ft3"
                    Return Enums.UnitOfMeasure.volume
                Case "m3/kmol", "cm3/mmol", "ft3/lbmol"
                    Return Enums.UnitOfMeasure.molar_volume
                Case "m2", "cm2", "ft2"
                    Return Enums.UnitOfMeasure.area
                Case "mm", "in"
                    Return Enums.UnitOfMeasure.diameter
                Case "N", "dyn", "kgf", "lbf"
                    Return Enums.UnitOfMeasure.force
                Case "W/[m2.K]", "cal/[cm2.s.C]", "BTU/[ft2.h.R]"
                    Return Enums.UnitOfMeasure.heat_transf_coeff
                Case "m/s2", "cm/s2", "ft/s2"
                    Return Enums.UnitOfMeasure.accel
                Case "m3/kg", "cm3/g", "ft3/lbm"
                    Return Enums.UnitOfMeasure.spec_vol
                Case "kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"
                    Return Enums.UnitOfMeasure.molar_conc
                Case "kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"
                    Return Enums.UnitOfMeasure.mass_conc
                Case "kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol/[ft3.h]"
                    Return Enums.UnitOfMeasure.reac_rate
                Case "kJ/kmol", "cal/mol", "BTU/lbmol"
                    Return Enums.UnitOfMeasure.molar_enthalpy
                Case "kJ/[kmol.K]", "cal/[mol.C]", "BTU/[lbmol.R]"
                    Return Enums.UnitOfMeasure.molar_entropy
                Case "m/s", "cm/s", "mm/s", "km/h", "ft/h", "ft/min", "ft/s", "in/s"
                    Return Enums.UnitOfMeasure.velocity
                Case "K.m2/W", "C.cm2.s/cal", "ft2.h.F/BTU"
                    Return Enums.UnitOfMeasure.foulingfactor
                Case "m/kg", "ft/lbm", "cm/g"
                    Return Enums.UnitOfMeasure.cakeresistance
                Case "m-1", "cm-1", "ft-1"
                    Return Enums.UnitOfMeasure.mediumresistance
                Case "kg", "g", "lb"
                    Return Enums.UnitOfMeasure.mass
                Case "K/Pa", "F/psi", "C/atm"
                    Return Enums.UnitOfMeasure.jouleThomsonCoefficient
                Case "1/Pa", "1/atm", "1/kPa", "1/bar", "1/MPa", "1/psi"
                    Return Enums.UnitOfMeasure.compressibility
                Case "kmol/[kg.s]", "kmol/[kg.min.]", "kmol/[kg.h]", "mol/[kg.s]", "mol/[kg.min.]", "mol/[kg.h]", "lbmol/[lbm.h]"
                    Return Enums.UnitOfMeasure.reac_rate_heterog
                Case Else
                    Return Enums.UnitOfMeasure.none
            End Select

        End Function


    End Class

    <System.Serializable()> Public Class SI

        Inherits Units

        Public Sub New()

            With Me

                .Name = "SI"
                .jouleThomsonCoefficient = "K/Pa"
                .diffusivity = "m2/s"
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
                .compressibility = "1/Pa"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
                .heatflow = "kW"
                .head = "m"
                .deltaP = "Pa"
                .deltaT = "K."

            End With

        End Sub

    End Class

    <System.Serializable()> Public Class SIUnits_Custom1

        Inherits Units

        Public Sub New()

            With Me

                .Name = "C1"
                .jouleThomsonCoefficient = "K/Pa"
                .diffusivity = "m2/s"

                .accel = "m2/s"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol/[m3.s]"
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
                .compressibility = "1/Pa"
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
                .pressure = "kgf/cm2"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
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

                .Name = "C2"
                .jouleThomsonCoefficient = "K/Pa"

                .diffusivity = "m2/s"
                .accel = "m2/s"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol/[m3.s]"
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
                .compressibility = "1/Pa"
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
                .pressure = "kgf/cm2"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
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

                .Name = "C3"
                .jouleThomsonCoefficient = "K/Pa"

                .diffusivity = "m2/s"
                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol/[m3.s]"
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
                .compressibility = "1/Pa"
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
                .pressure = "kgf/cm2"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
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

                .Name = "C4"
                .jouleThomsonCoefficient = "K/Pa"

                .diffusivity = "m2/s"
                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol/[m3.s]"
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
                .compressibility = "1/Pa"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
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

                .Name = "C5"

                .diffusivity = "m2/s"
                .jouleThomsonCoefficient = "K/Pa"
                .accel = "m/s2"
                .area = "m2"
                .diameter = "mm"
                .distance = "m"
                .force = "N"
                .heat_transf_coeff = "W/[m2.K]"
                .mass_conc = "kg/m3"
                .molar_conc = "kmol/m3"
                .molar_volume = "m3/kmol"
                .reac_rate = "kmol/[m3.h]"
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
                .compressibility = "1/Pa"
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
                .temperature = "C"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "N/m"
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

                .Name = "ENG"

                .diffusivity = "ft2/s"

                .jouleThomsonCoefficient = "F/psi"
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
                .reac_rate = "lbmol/[ft3.h]"
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
                .compressibility = "1/psi"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "lbf/in"
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

                .Name = "CGS"
                .diffusivity = "cm2/s"

                .jouleThomsonCoefficient = "C/atm"
                .accel = "cm/s2"
                .area = "cm2"
                .diameter = "mm"
                .distance = "cm"
                .force = "dyn"
                .heat_transf_coeff = "cal/[cm2.s.C]"
                .mass_conc = "g/cm3"
                .molar_conc = "mol/cm3"
                .molar_volume = "cm3/mol"
                .reac_rate = "mol/[cm3.s]"
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
                .compressibility = "1/atm"
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
                .kvalue = "-"
                .logKvalue = "-"
                .surfaceTension = "dyn/cm2"
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

        Public Shared Function ConvertArrayToSI(ByVal units As String, ByVal values As Double()) As Double()
            Dim newarr As New List(Of Double)
            For Each d In values
                newarr.Add(ConvertToSI(units, d))
            Next
            Return newarr.ToArray
        End Function

        Public Shared Function ConvertArrayFromSI(ByVal units As String, ByVal values As Double()) As Double()
            Dim newarr As New List(Of Double)
            For Each d In values
                newarr.Add(ConvertFromSI(units, d))
            Next
            Return newarr.ToArray
        End Function

        Public Shared Function ConvertToSI(ByVal units As String, ByVal value As Double) As Double

            Select Case units

                Case "K/Pa"
                    Return value
                Case "C/atm"
                    Return value * 101325
                Case "F/psi"
                    Return value / 1.8 * 0.000145038

                Case "1/kPa"
                    Return value * 0.001
                Case "1/MPa"
                    Return value * 0.000001
                Case "1/psi"
                    Return value * 0.000145038
                Case "1/bar"
                    Return value / 100000
                Case "1/atm"
                    Return value / 101325
            
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

                Case "g"
                    Return value / 1000
                Case "lb", "lbm"
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

                Case "kJ/kmol", "J/mol"
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
                Case "ft/lb", "ft/lbm"
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
                    Return value / 35.3147 / 1000 / 2.20462

                Case "mm"                               'comprimento'
                    Return value
                Case "in.", "in"
                    Return value / 39.3701 * 1000


                Case "dyn"                              'forca
                    Return value / 100000
                Case "N"
                    Return value
                Case "lbf"
                    Return value / 0.224809
                Case "mol/L"                            'conc molar
                    Return value * 1000
                Case "kmol/m3"
                    Return value
                Case "mol/cm3"
                    Return value * 1000000.0 / 1000
                Case "mol/mL"
                    Return value * 1000000.0 / 1000
                Case "lbmol/ft3"
                    Return value * 35.3147 * 1000 / 2.20462
                Case "g/L"                              'conc massica
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
                    Return value * 1000
                Case "mol/[L.min.]"
                    Return value / 60 * 1000
                Case "mol/[L.h]"
                    Return value / 3600 * 1000
                Case "mol/[cm3.s]"
                    Return value * 1000000.0
                Case "mol/[cm3.min.]"
                    Return value / 60 * 1000000.0
                Case "mol/[cm3.h]"
                    Return value / 3600 * 1000000.0
                Case "lbmol/[ft3.h]"
                    Return value / 3600 * 35.3147 * 1000 / 2.20462
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
                Case "lbm /[ft.h]"
                    Return value / 2419.09
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

        Public Shared Function ConvertFromSI(ByVal units As String, ByVal value As Double) As Double

            Select Case units

                Case "K/Pa"
                    Return value
                Case "C/atm"
                    Return value / 101325
                Case "F/psi"
                    Return value * 1.8 / 0.000145038

                Case "1/kPa"
                    Return value / 0.001
                Case "1/MPa"
                    Return value / 0.000001
                Case "1/psi"
                    Return value / 0.000145038
                Case "1/bar"
                    Return value * 100000
                Case "1/atm"
                    Return value * 101325

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
                    Return value * 2.20462

                Case "g"
                    Return value * 1000
                Case "lb", "lbm"
                    Return value * 2.20462

                    Return value
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

                Case "kJ/kmol", "J/mol"
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
                Case "ft/lb", "ft/lbm"
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
                    Return value * 35.3147 * 1000 * 2.20462

                Case "mm"                               'comprimento'
                    Return value
                Case "in.", "in"
                    Return value * 39.3701 / 1000


                Case "dyn"                              'forca
                    Return value * 100000
                Case "N"
                    Return value
                Case "lbf"
                    Return value * 0.224809
                Case "mol/L"                            'conc molar
                    Return value / 1000
                Case "kmol/m3"
                    Return value
                Case "mol/cm3"
                    Return value / 1000000.0 * 1000
                Case "mol/mL"
                    Return value / 1000000.0 * 1000
                Case "lbmol/ft3"
                    Return value / 35.3147 / 1000 * 2.20462
                Case "g/L"                              'conc massica
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
                    Return value / 1000
                Case "mol/[L.min.]"
                    Return value * 60 / 1000
                Case "mol/[L.h]"
                    Return value * 3600 / 1000
                Case "mol/[cm3.s]"
                    Return value / 1000000.0
                Case "mol/[cm3.min.]"
                    Return value * 60 / 1000000.0
                Case "mol/[cm3.h]"
                    Return value * 3600 / 1000000.0
                Case "lbmol/[ft3.h]"
                    Return value * 3600 / 35.3147 / 1000 * 2.20462
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
                Case "lbm /[ft.h]"
                    Return value * 2419.09
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
