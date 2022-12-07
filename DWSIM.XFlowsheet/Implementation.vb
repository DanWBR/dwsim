Imports DWSIM.XFlowsheet.Interfaces

Namespace Implementation

    Public Class DefaultParameterDefinitions

        Public Shared Function GetDefaultParameterNames(otype As ObjType) As List(Of String)

            Dim plist As New List(Of String)

            Select Case otype
                Case ObjType.MaterialStream
                    plist.AddRange({"Temperature", "Pressure", "MassEnthalpy", "MassEntropy", "VaporFraction", "OverallMolarFlow", "OverallMolarComposition", "FlashSpec"})
                Case ObjType.EnergyStream
                    plist.AddRange({"EnergyFlow"})
                Case ObjType.Mixer
                Case ObjType.Splitter
                    plist.AddRange({"SplitRatios"})
                Case ObjType.Valve
                    plist.AddRange({"PressureDecrease"})
                Case ObjType.Pump
                    plist.AddRange({"PressureIncrease", "Efficiency"})
                Case ObjType.SeparatorVessel
                Case ObjType.HeatExchanger
                    plist.AddRange({"ExchangeArea", "OverallHTC", "HeatDuty", "Efficiency"})
                Case ObjType.Heater
                    plist.AddRange({"HeatDuty", "Efficiency"})
                Case ObjType.Cooler
                    plist.AddRange({"HeatDuty", "Efficiency"})
                Case ObjType.Compressor
                    plist.AddRange({"PressureIncrease", "AdiabaticEfficiency"})
                Case ObjType.Expander
                    plist.AddRange({"PressureDecrease", "AdiabaticEfficiency"})
            End Select

            Return plist

        End Function

        Public Shared Function GetDefaultParameterTypes(otype As ObjType) As List(Of ParamValueType)

            Dim plist As New List(Of ParamValueType)

            Select Case otype
                Case ObjType.MaterialStream
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double, ParamValueType.Type_Double,
                                   ParamValueType.Type_Double, ParamValueType.Type_Double, ParamValueType.Type_Double,
                                   ParamValueType.Type_ListDouble, ParamValueType.Type_String})
                Case ObjType.EnergyStream
                    plist.AddRange({ParamValueType.Type_Double})
                Case ObjType.Mixer
                Case ObjType.Splitter
                    plist.AddRange({ParamValueType.Type_ListDouble})
                Case ObjType.Valve
                    plist.AddRange({ParamValueType.Type_Double})
                Case ObjType.Pump
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double})
                Case ObjType.SeparatorVessel
                Case ObjType.HeatExchanger
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double, ParamValueType.Type_Double, ParamValueType.Type_Double})
                Case ObjType.Heater
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double})
                Case ObjType.Cooler
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double})
                Case ObjType.Compressor
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double})
                Case ObjType.Expander
                    plist.AddRange({ParamValueType.Type_Double, ParamValueType.Type_Double})
            End Select

            Return plist

        End Function

        Public Shared Function GetDefaultParameterUnitsOfMeasureType(otype As ObjType) As List(Of UnitOfMeasure)

            Dim plist As New List(Of UnitOfMeasure)

            Select Case otype
                Case ObjType.MaterialStream
                    plist.AddRange({UnitOfMeasure.Temperature, UnitOfMeasure.Pressure, UnitOfMeasure.MassEnthalpy, UnitOfMeasure.MassEntropy,
                                   UnitOfMeasure.None, UnitOfMeasure.MolarFlow, UnitOfMeasure.None, UnitOfMeasure.None})
                Case ObjType.EnergyStream
                    plist.AddRange({UnitOfMeasure.HeatFlow})
                Case ObjType.Mixer
                Case ObjType.Splitter
                    plist.AddRange({UnitOfMeasure.None})
                Case ObjType.Valve
                    plist.AddRange({UnitOfMeasure.DeltaP})
                Case ObjType.Pump
                    plist.AddRange({UnitOfMeasure.DeltaP})
                Case ObjType.SeparatorVessel
                Case ObjType.HeatExchanger
                    plist.AddRange({UnitOfMeasure.Area, UnitOfMeasure.HeatTransfCoeff, UnitOfMeasure.HeatFlow, UnitOfMeasure.None})
                Case ObjType.Heater
                    plist.AddRange({UnitOfMeasure.HeatFlow, UnitOfMeasure.None})
                Case ObjType.Cooler
                    plist.AddRange({UnitOfMeasure.HeatFlow, UnitOfMeasure.None})
                Case ObjType.Compressor
                    plist.AddRange({UnitOfMeasure.DeltaP, UnitOfMeasure.None})
                Case ObjType.Expander
                    plist.AddRange({UnitOfMeasure.DeltaP, UnitOfMeasure.None})
            End Select

            Return plist

        End Function

    End Class

    Public Class DefaultImplementations

        Public Class Flowsheet

            Implements IFlowsheet

            Public Property ID As String = "" Implements IFlowsheet.ID

            Public Property Name As String = "" Implements IFlowsheet.Name

            Public Property Description As String = "" Implements IFlowsheet.Description

            Public Property PFDObjects As New List(Of IPFDObject) Implements IFlowsheet.PFDObjects

            Public Property SimulationObjects As New List(Of ISimulationObject) Implements IFlowsheet.SimulationObjects

            Public Property PropertyPackages As New List(Of IPropertyPackage) Implements IFlowsheet.PropertyPackages

            Public Property Compounds As New List(Of String) Implements IFlowsheet.Compounds

            Public Property Parameters As New List(Of IParameter) Implements IFlowsheet.Parameters

            Public Property DisplayedUnitsOfMeasure As UnitOfMeasureSet = UnitOfMeasureSet.SI Implements IFlowsheet.DisplayedUnitsOfMeasure

        End Class

        Public Class Parameter

            Implements IParameter

            Public Property ID As String Implements IParameter.ID

            Public Property Name As String Implements IParameter.Name

            Public Property Description As String Implements IParameter.Description

            Public Property Value As Object Implements IParameter.Value

            Public Property ValueType As ParamValueType Implements IParameter.ValueType

        End Class

        Public Class PFDObject

            Implements IPFDObject

            Public Property ID As String Implements IPFDObject.ID

            Public Property Name As String Implements IPFDObject.Name

            Public Property Description As String Implements IPFDObject.Description

            Public Property X As Double Implements IPFDObject.X

            Public Property Y As Double Implements IPFDObject.Y

            Public Property Width As Double Implements IPFDObject.Width

            Public Property Height As Double Implements IPFDObject.Height

            Public Property ObjectType As ObjType Implements IPFDObject.ObjectType

            Public Property Ports As New List(Of IConnectionPort) Implements IPFDObject.Ports

            Public Property HasAssociatedSimulationObject As Boolean Implements IPFDObject.HasAssociatedSimulationObject

            Public Property AssociatedSimulationObjectID As String Implements IPFDObject.AssociatedSimulationObjectID

        End Class

        Public Class ConnectionPort

            Implements IConnectionPort

            Public Property ID As String Implements IConnectionPort.ID

            Public Property Name As String Implements IConnectionPort.Name

            Public Property Description As String Implements IConnectionPort.Description

            Public Property IsConnected As Boolean = False Implements IConnectionPort.IsConnected

            Public Property IsInput As Boolean = False Implements IConnectionPort.IsInput

            Public Property IsOutput As Boolean = False Implements IConnectionPort.IsOutput

            Public Property IsEnergyPort As Boolean = False Implements IConnectionPort.IsEnergyPort

            Public Property RelativeX As Double Implements IConnectionPort.RelativeX

            Public Property RelativeY As Double Implements IConnectionPort.RelativeY

            Public Property Index As Integer Implements IConnectionPort.Index

            Public Property ConnectedToObjectID As String Implements IConnectionPort.ConnectedToObjectID

            Public Property ConnectedToObjectPortIndex As Integer Implements IConnectionPort.ConnectedToObjectPortIndex

        End Class

        Public Class SimulationObject

            Implements ISimulationObject

            Public Property ID As String Implements ISimulationObject.ID

            Public Property Name As String Implements ISimulationObject.Name

            Public Property Description As String Implements ISimulationObject.Description

            Public Property PropertyPackageID As String Implements ISimulationObject.PropertyPackageID

            Public Property PFDObjectID As String Implements ISimulationObject.PFDObjectID

            Public Property ObjectType As ObjType Implements ISimulationObject.ObjectType

            Public Property Parameters As New List(Of IParameter) Implements ISimulationObject.Parameters

            Public Property ExtendedParameters As New List(Of IParameter) Implements ISimulationObject.ExtendedParameters

        End Class

        Public Class PropertyPackage

            Implements IPropertyPackage

            Public Property ID As String Implements IPropertyPackage.ID

            Public Property Name As String Implements IPropertyPackage.Name

            Public Property Description As String Implements IPropertyPackage.Description

            Public Property Model As PropPackageModel Implements IPropertyPackage.Model

            Public Property Parameters As New List(Of IParameter) Implements IPropertyPackage.Parameters

        End Class

        Public Class UnitsOfMeasure

            Implements IUnitsOfMeasure

            Public Property accel As String = "" Implements IUnitsOfMeasure.Accel

            Public Property area As String = "" Implements IUnitsOfMeasure.Area

            Public Property cakeresistance As String = "" Implements IUnitsOfMeasure.Cakeresistance

            Public Property kinematic_viscosity As String = "" Implements IUnitsOfMeasure.Kinematic_viscosity

            Public Property compressibility As String = "" Implements IUnitsOfMeasure.Compressibility

            Public Property deltaP As String = "" Implements IUnitsOfMeasure.DeltaP

            Public Property deltaT As String = "" Implements IUnitsOfMeasure.DeltaT

            Public Property density As String = "" Implements IUnitsOfMeasure.Density

            Public Property diameter As String = "" Implements IUnitsOfMeasure.Diameter

            Public Property distance As String = "" Implements IUnitsOfMeasure.Distance

            Public Property massenthalpy As String = "" Implements IUnitsOfMeasure.MassEnthalpy

            Public Property massentropy As String = "" Implements IUnitsOfMeasure.MassEntropy

            Public Property force As String = "" Implements IUnitsOfMeasure.Force

            Public Property foulingfactor As String = "" Implements IUnitsOfMeasure.Foulingfactor

            Public Property head As String = "" Implements IUnitsOfMeasure.Head

            Public Property heat_transf_coeff As String = "" Implements IUnitsOfMeasure.Heat_transf_coeff

            Public Property heatCapacity As String = "" Implements IUnitsOfMeasure.HeatCapacity

            Public Property heatflow As String = "" Implements IUnitsOfMeasure.Heatflow

            Public Property jouleThomsonCoefficient As String = "" Implements IUnitsOfMeasure.JouleThomsonCoefficient

            Public Property mass As String = "" Implements IUnitsOfMeasure.Mass

            Public Property mass_conc As String = "" Implements IUnitsOfMeasure.Mass_conc

            Public Property massflow As String = "" Implements IUnitsOfMeasure.MassFlow

            Public Property mediumresistance As String = "" Implements IUnitsOfMeasure.Mediumresistance

            Public Property molar_conc As String = "" Implements IUnitsOfMeasure.Molar_conc

            Public Property molar_enthalpy As String = "" Implements IUnitsOfMeasure.Molar_enthalpy

            Public Property molar_entropy As String = "" Implements IUnitsOfMeasure.Molar_entropy

            Public Property molar_volume As String = "" Implements IUnitsOfMeasure.Molar_volume

            Public Property molarflow As String = "" Implements IUnitsOfMeasure.Molarflow

            Public Property molecularWeight As String = "" Implements IUnitsOfMeasure.MolecularWeight

            Public Property Name As String = "" Implements IUnitsOfMeasure.Name

            Public Property pressure As String = "" Implements IUnitsOfMeasure.Pressure

            Public Property reac_rate As String = "" Implements IUnitsOfMeasure.Reac_rate

            Public Property reac_rate_heterog As String = "" Implements IUnitsOfMeasure.Reac_rate_heterog

            Public Property spec_vol As String = "" Implements IUnitsOfMeasure.Spec_vol

            Public Property speedOfSound As String = "" Implements IUnitsOfMeasure.SpeedOfSound

            Public Property surfaceTension As String = "" Implements IUnitsOfMeasure.SurfaceTension

            Public Property temperature As String = "" Implements IUnitsOfMeasure.Temperature

            Public Property thermalConductivity As String = "" Implements IUnitsOfMeasure.ThermalConductivity

            Public Property thickness As String = "" Implements IUnitsOfMeasure.Thickness

            Public Property time As String = "" Implements IUnitsOfMeasure.Time

            Public Property velocity As String = "" Implements IUnitsOfMeasure.Velocity

            Public Property dynamicviscosity As String = "" Implements IUnitsOfMeasure.Dynamic_Viscosity

            Public Property volume As String = "" Implements IUnitsOfMeasure.Volume

            Public Property volumetricFlow As String = "" Implements IUnitsOfMeasure.VolumetricFlow

            Public Property diffusivity As String = "" Implements IUnitsOfMeasure.Diffusivity

            Public Property conductance As String = "[kg/s]/[Pa^0.5]" Implements IUnitsOfMeasure.Conductance

            Public Property ID As String = "" Implements IUnitsOfMeasure.ID

            Public Property Description As String = "" Implements IUnitsOfMeasure.Description

            Public Function GetUnitSet(measureID As UnitOfMeasure) As List(Of String) Implements IUnitsOfMeasure.GetUnitSet

                Dim units As New List(Of String)

                Select Case measureID
                    Case UnitOfMeasure.Temperature
                        units.AddRange(New String() {"K", "R", "C", "F"})
                    Case UnitOfMeasure.Pressure
                        units.AddRange(New String() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
                    Case UnitOfMeasure.MassFlow
                        units.AddRange(New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min",
                               "lb/s", "lb/h", "lb/d", "Mg/s", "Mg/h", "Mg/d"})
                    Case UnitOfMeasure.MolarFlow
                        units.AddRange(New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm",
                               "ft3/d @ 60 f, 14.7 psia", "ft3/d @ 0 C, 1 atm",
                               "MMSCFD", "SCFD", "SCFM",
                               "Mm3/d @ BR", "Mm3/d @ SC", "Mm3/d @ NC"})
                    Case UnitOfMeasure.VolumetricFlow
                        units.AddRange(New String() {"m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/min", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "gal[US]/s", "L/h", "L/min", "L/s"})
                    Case UnitOfMeasure.MassEnthalpy
                        units.AddRange(New String() {"kJ/kg", "cal/g", "BTU/lbm", "kcal/kg"})
                    Case UnitOfMeasure.MassEntropy
                        units.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
                    Case UnitOfMeasure.MolecularWeight
                        units.AddRange(New String() {"kg/kmol", "g/mol", "lbm/lbmol"})
                    Case UnitOfMeasure.SurfaceTension
                        units.AddRange(New String() {"N/m", "dyn/cm", "lbf/in"})
                    Case UnitOfMeasure.Density
                        units.AddRange(New String() {"kg/m3", "g/cm3", "lbm/ft3"})
                    Case UnitOfMeasure.HeatCapacity
                        units.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
                    Case UnitOfMeasure.ThermalConductivity
                        units.AddRange(New String() {"W/[m.K]", "cal/[cm.s.C]", "BTU/[ft.h.R]"})
                    Case UnitOfMeasure.KinematicViscosity, UnitOfMeasure.Diffusivity
                        units.AddRange(New String() {"m2/s", "cSt", "ft2/s", "mm2/s", "cm2/s"})
                    Case UnitOfMeasure.DynamicViscosity
                        units.AddRange(New String() {"kg/[m.s]", "Pa.s", "cP", "lbm/[ft.h]"})
                    Case UnitOfMeasure.DeltaP
                        units.AddRange(New String() {"Pa", "atm", "lbf/ft2", "kgf/cm2", "kgf/cm2_g", "kPa", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
                    Case UnitOfMeasure.DeltaT
                        units.AddRange(New String() {"C.", "K.", "F.", "R."})
                    Case UnitOfMeasure.Distance
                        units.AddRange(New String() {"m", "ft", "cm"})
                    Case UnitOfMeasure.HeatFlow
                        units.AddRange(New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W",
                               "BTU/d", "MMBTU/d", "MMBTU/h", "kcal/s", "kcal/h", "kcal/d"})
                    Case UnitOfMeasure.Time
                        units.AddRange(New String() {"s", "min.", "h"})
                    Case UnitOfMeasure.Volume
                        units.AddRange(New String() {"m3", "cm3", "L", "ft3", "bbl", "gal[US]", "gal[UK]"})
                    Case UnitOfMeasure.MolarVolume
                        units.AddRange(New String() {"m3/kmol", "cm3/mmol", "ft3/lbmol"})
                    Case UnitOfMeasure.Area
                        units.AddRange(New String() {"m2", "cm2", "ft2"})
                    Case UnitOfMeasure.Head
                        units.AddRange(New String() {"m", "ft", "cm"})
                    Case UnitOfMeasure.Diameter
                        units.AddRange(New String() {"mm", "in"})
                    Case UnitOfMeasure.Force
                        units.AddRange(New String() {"N", "dyn", "kgf", "lbf"})
                    Case UnitOfMeasure.HeatTransfCoeff
                        units.AddRange(New String() {"W/[m2.K]", "cal/[cm2.s.C]", "BTU/[ft2.h.R]"})
                    Case UnitOfMeasure.Accel
                        units.AddRange(New String() {"m/s2", "cm/s2", "ft/s2"})
                    Case UnitOfMeasure.SpecificVol
                        units.AddRange(New String() {"m3/kg", "cm3/g", "ft3/lbm"})
                    Case UnitOfMeasure.MolarConc
                        units.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"})
                    Case UnitOfMeasure.MassConc
                        units.AddRange(New String() {"kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
                    Case UnitOfMeasure.ReacRate
                        units.AddRange(New String() {"kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol/[ft3.h]"})
                    Case UnitOfMeasure.MolarEnthalpy
                        units.AddRange(New String() {"kJ/kmol", "cal/mol", "BTU/lbmol", "J/mol"})
                    Case UnitOfMeasure.MolarEntropy
                        units.AddRange(New String() {"kJ/[kmol.K]", "cal/[mol.C]", "BTU/[lbmol.R]"})
                    Case UnitOfMeasure.Velocity, UnitOfMeasure.SpeedOfSound
                        units.AddRange(New String() {"m/s", "cm/s", "mm/s", "km/h", "ft/h", "ft/min", "ft/s", "in/s"})
                    Case UnitOfMeasure.Foulingfactor
                        units.AddRange(New String() {"K.m2/W", "C.cm2.s/cal", "ft2.h.F/BTU"})
                    Case UnitOfMeasure.CakeResistance
                        units.AddRange(New String() {"m/kg", "ft/lbm", "cm/g"})
                    Case UnitOfMeasure.Mediumresistance
                        units.AddRange(New String() {"m-1", "cm-1", "ft-1"})
                    Case UnitOfMeasure.Mass
                        units.AddRange(New String() {"kg", "g", "lb"})
                    Case UnitOfMeasure.JouleThomsonCoefficient
                        units.AddRange(New String() {"K/Pa", "F/psi", "C/atm"})
                    Case UnitOfMeasure.Compressibility
                        units.AddRange(New String() {"1/Pa", "1/atm", "1/kPa", "1/bar", "1/MPa", "1/psi"})
                    Case UnitOfMeasure.ReacRateHeterog
                        units.AddRange(New String() {"kmol/[kg.s]", "kmol/[kg.min.]", "kmol/[kg.h]", "mol/[kg.s]", "mol/[kg.min.]", "mol/[kg.h]", "lbmol/[lbm.h]"})
                    Case UnitOfMeasure.Conductance
                        units.AddRange(New String() {"[kg/s]/[Pa^0.5]", "[lbm/h]/[psi^0.5]", "[kg/h]/[atm^0.5]", "[kg/h]/[bar^0.5]", "[kg/h]/[[kgf/cm2]^0.5]"})
                End Select

                Return units

            End Function

            Public Function GetUnitType(unit As String) As UnitOfMeasure Implements IUnitsOfMeasure.GetUnitType

                Select Case unit
                    Case "K", "R", "C", "F"
                        Return UnitOfMeasure.Temperature
                    Case "Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig", "psia"
                        Return UnitOfMeasure.Pressure
                    Case "g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s", "lb/h", "lb/d", "Mg/s", "Mg/h", "Mg/d"
                        Return UnitOfMeasure.MassFlow
                    Case "mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 f, 14.7 psia", "ft3/d @ 0 C, 1 atm"
                        Return UnitOfMeasure.MolarFlow
                    Case "m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/min", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "gal[US]/s", "L/h", "L/min", "L/s",
                 "ft3/d @ 60 f, 14.7 psia", "ft3/d @ 0 C, 1 atm",
                "MMSCFD", "SCFD", "SCFM",
                "Mm3/d @ BR", "Mm3/d @ SC", "Mm3/d @ SC"
                        Return UnitOfMeasure.VolumetricFlow
                    Case "kJ/kg", "cal/g", "BTU/lbm", "kcal/kg"
                        Return UnitOfMeasure.MassEnthalpy
                    Case "kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"
                        Return UnitOfMeasure.MassEntropy
                    Case "kg/kmol", "g/mol", "lbm/lbmol"
                        Return UnitOfMeasure.MolecularWeight
                    Case "N/m", "dyn/cm", "lbf/in"
                        Return UnitOfMeasure.SurfaceTension
                    Case "kg/m3", "g/cm3", "lbm/ft3"
                        Return UnitOfMeasure.Density
                    Case "kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"
                        Return UnitOfMeasure.HeatCapacity
                    Case "W/[m.K]", "cal/[cm.s.C]", "BTU/[ft.h.R]"
                        Return UnitOfMeasure.ThermalConductivity
                    Case "m2/s", "cSt", "ft2/s", "mm2/s", "cm2/s"
                        Return UnitOfMeasure.KinematicViscosity
                    Case "kg/[m.s]", "Pa.s", "cP", "lbm/[ft.h]"
                        Return UnitOfMeasure.DynamicViscosity
                    Case "Pa", "atm", "lbf/ft2", "kgf/cm2", "kPa", "bar", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi"
                        Return UnitOfMeasure.DeltaP
                    Case "C.", "K.", "f.", "R."
                        Return UnitOfMeasure.DeltaT
                    Case "m", "ft", "cm"
                        Return UnitOfMeasure.Distance
                    Case "kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W",
                 "BTU/d", "MMBTU/d", "MMBTU/h", "kcal/s", "kcal/h", "kcal/d"
                        Return UnitOfMeasure.HeatFlow
                    Case "s", "min.", "h"
                        Return UnitOfMeasure.Time
                    Case "m3", "cm3", "L", "ft3", "bbl", "gal[US]", "gal[UK]"
                        Return UnitOfMeasure.Volume
                    Case "m3/kmol", "cm3/mmol", "ft3/lbmol"
                        Return UnitOfMeasure.MolarVolume
                    Case "m2", "cm2", "ft2"
                        Return UnitOfMeasure.Area
                    Case "mm", "in"
                        Return UnitOfMeasure.Diameter
                    Case "N", "dyn", "kgf", "lbf"
                        Return UnitOfMeasure.Force
                    Case "W/[m2.K]", "cal/[cm2.s.C]", "BTU/[ft2.h.R]"
                        Return UnitOfMeasure.HeatTransfCoeff
                    Case "m/s2", "cm/s2", "ft/s2"
                        Return UnitOfMeasure.Accel
                    Case "m3/kg", "cm3/g", "ft3/lbm"
                        Return UnitOfMeasure.SpecificVol
                    Case "kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"
                        Return UnitOfMeasure.MolarConc
                    Case "kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"
                        Return UnitOfMeasure.MassConc
                    Case "kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol/[ft3.h]"
                        Return UnitOfMeasure.ReacRate
                    Case "kJ/kmol", "cal/mol", "BTU/lbmol"
                        Return UnitOfMeasure.MolarEnthalpy
                    Case "kJ/[kmol.K]", "cal/[mol.C]", "BTU/[lbmol.R]"
                        Return UnitOfMeasure.MolarEntropy
                    Case "m/s", "cm/s", "mm/s", "km/h", "ft/h", "ft/min", "ft/s", "in/s"
                        Return UnitOfMeasure.Velocity
                    Case "K.m2/W", "C.cm2.s/cal", "ft2.h.F/BTU"
                        Return UnitOfMeasure.Foulingfactor
                    Case "m/kg", "ft/lbm", "cm/g"
                        Return UnitOfMeasure.CakeResistance
                    Case "m-1", "cm-1", "ft-1"
                        Return UnitOfMeasure.Mediumresistance
                    Case "kg", "g", "lb"
                        Return UnitOfMeasure.Mass
                    Case "K/Pa", "F/psi", "C/atm"
                        Return UnitOfMeasure.JouleThomsonCoefficient
                    Case "1/Pa", "1/atm", "1/kPa", "1/bar", "1/MPa", "1/psi"
                        Return UnitOfMeasure.Compressibility
                    Case "kmol/[kg.s]", "kmol/[kg.min.]", "kmol/[kg.h]", "mol/[kg.s]", "mol/[kg.min.]", "mol/[kg.h]", "lbmol/[lbm.h]"
                        Return UnitOfMeasure.ReacRateHeterog
                    Case "[kg/s]/[Pa^0.5]", "[lbm/h]/[psi^0.5]", "[kg/h]/[atm^0.5]", "[kg/h]/[bar^0.5]", "[kg/h]/[[kgf/cm2]^0.5]"
                        Return UnitOfMeasure.Conductance
                    Case Else
                        Return UnitOfMeasure.None
                End Select

            End Function

            Public Function GetCurrentUnits(measureID As UnitOfMeasure) As String Implements IUnitsOfMeasure.GetCurrentUnits

                Dim units As New List(Of String)

                Select Case measureID
                    Case UnitOfMeasure.Temperature
                        Return temperature
                    Case UnitOfMeasure.Pressure
                        Return pressure
                    Case UnitOfMeasure.MassFlow
                        Return massflow
                    Case UnitOfMeasure.MolarFlow
                        Return molarflow
                    Case UnitOfMeasure.VolumetricFlow
                        Return volumetricFlow
                    Case UnitOfMeasure.MassEnthalpy
                        Return massenthalpy
                    Case UnitOfMeasure.MassEntropy
                        Return massentropy
                    Case UnitOfMeasure.MolecularWeight
                        Return molecularWeight
                    Case UnitOfMeasure.SurfaceTension
                        Return surfaceTension
                    Case UnitOfMeasure.Density
                        Return density
                    Case UnitOfMeasure.HeatCapacity
                        Return heatCapacity
                    Case UnitOfMeasure.ThermalConductivity
                        Return thermalConductivity
                    Case UnitOfMeasure.KinematicViscosity, UnitOfMeasure.Diffusivity
                        Return kinematic_viscosity
                    Case UnitOfMeasure.DynamicViscosity
                        Return dynamicviscosity
                    Case UnitOfMeasure.DeltaP
                        Return deltaP
                    Case UnitOfMeasure.DeltaT
                        Return deltaT
                    Case UnitOfMeasure.Distance
                        Return distance
                    Case UnitOfMeasure.HeatFlow
                        Return heatflow
                    Case UnitOfMeasure.Time
                        Return time
                    Case UnitOfMeasure.Volume
                        Return volume
                    Case UnitOfMeasure.MolarVolume
                        Return molar_volume
                    Case UnitOfMeasure.Area
                        Return area
                    Case UnitOfMeasure.Diameter
                        Return diameter
                    Case UnitOfMeasure.Force
                        Return force
                    Case UnitOfMeasure.HeatTransfCoeff
                        Return heat_transf_coeff
                    Case UnitOfMeasure.Accel
                        Return accel
                    Case UnitOfMeasure.SpecificVol
                        Return spec_vol
                    Case UnitOfMeasure.MolarConc
                        Return molar_conc
                    Case UnitOfMeasure.MassConc
                        Return mass_conc
                    Case UnitOfMeasure.ReacRate
                        Return reac_rate
                    Case UnitOfMeasure.MolarEnthalpy
                        Return molar_enthalpy
                    Case UnitOfMeasure.MolarEntropy
                        Return molar_entropy
                    Case UnitOfMeasure.Velocity, UnitOfMeasure.SpeedOfSound
                        Return velocity
                    Case UnitOfMeasure.Foulingfactor
                        Return foulingfactor
                    Case UnitOfMeasure.CakeResistance
                        Return cakeresistance
                    Case UnitOfMeasure.Mediumresistance
                        Return mediumresistance
                    Case UnitOfMeasure.Mass
                        Return mass
                    Case UnitOfMeasure.JouleThomsonCoefficient
                        Return jouleThomsonCoefficient
                    Case UnitOfMeasure.Compressibility
                        Return compressibility
                    Case UnitOfMeasure.ReacRateHeterog
                        Return reac_rate_heterog
                    Case UnitOfMeasure.Conductance
                        Return conductance
                    Case Else
                        Return ""
                End Select

            End Function

        End Class

        Public Class SI

            Inherits UnitsOfMeasure

            Public Sub New()

                With Me

                    .ID = "SI"
                    .Name = "SI"
                    .jouleThomsonCoefficient = "K/Pa"
                    .diffusivity = "m2/s"
                    .accel = "m2/s"
                    .area = "m2"
                    .conductance = "[kg/s]/[Pa^0.5]"
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

                    .compressibility = "1/Pa"
                    .density = "kg/m3"
                    .massenthalpy = "kJ/kg"
                    .massentropy = "kJ/[kg.K]"
                    .heatCapacity = "kJ/[kg.K]"
                    .jouleThomsonCoefficient = "K/Pa"
                    .massflow = "kg/s"
                    .molarflow = "mol/s"
                    .molecularWeight = "kg/kmol"
                    .pressure = "Pa"
                    .speedOfSound = "m/s"
                    .temperature = "K"
                    .thermalConductivity = "W/[m.K]"
                    .dynamicviscosity = "Pa.s"
                    .volumetricFlow = "m3/s"
                    .kinematic_viscosity = "m2/s"
                    .surfaceTension = "N/m"
                    .surfaceTension = "N/m"
                    .heatflow = "kW"
                    .head = "m"
                    .deltaP = "Pa"
                    .deltaT = "K."

                End With

            End Sub

        End Class

    End Class

End Namespace
