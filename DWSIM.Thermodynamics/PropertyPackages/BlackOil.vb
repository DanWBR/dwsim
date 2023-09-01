'    Black Oil Property Package 
'    Copyright 2015 Daniel Wagner O. de Medeiros
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


Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.MathOps.MathEx
Imports System.Linq
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable()> Public Class BlackOilPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Protected bop As New Auxiliary.BlackOilProperties

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

        End Sub

        Public Sub New()

            MyBase.New()

            Me.IsConfigurable = False

            Me._packagetype = PropertyPackages.PackageType.Specialized

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Thermal_Conductivity = "Black-Oil Correlation"
                .Vapor_Viscosity = "Black-Oil Correlation"
                .Vapor_Enthalpy_Entropy_CpCv = "Black-Oil Correlation"
                .Vapor_Density = "Black-Oil Correlation"
                .Liquid_Fugacity = "Vapor Pressure"
                .Liquid_Enthalpy_Entropy_CpCv = "Black-Oil Correlation"
                .Liquid_ThermalConductivity = "Black-Oil Correlation"
                .Liquid_Viscosity = "Black-Oil Correlation"
            End With

        End Sub

        Public Overrides Sub ConfigParameters()

        End Sub

        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            Get
                FlashAlgorithm = New Auxiliary.FlashAlgorithms.BlackOil
                Return FlashAlgorithm
            End Get
        End Property

        Public Function CalcBOFluid(Vxw As Double(), constprops As List(Of Interfaces.ICompoundConstantProperties)) As BlackOilFluid
            Dim bof As New BlackOilFluid
            Dim i As Integer = 0
            For Each c In constprops
                If c.Molar_Weight = 0.0# Then
                    c.Molar_Weight = (c.BO_GOR * c.BO_SGG) * bop.VaporMolecularWeight(c.BO_SGG) +
                                     (c.BO_SGO * 1000 * (100 - c.BO_BSW) / 100 + 1000 * c.BO_BSW / 100) * bop.LiquidMolecularWeight(c.BO_SGO, c.BO_BSW)
                    c.Molar_Weight /= (c.BO_GOR * c.BO_SGG) + (c.BO_SGO * 1000 * (100 - c.BO_BSW) / 100 + 1000 * c.BO_BSW / 100)
                End If
                With bof
                    .SGO += Vxw(i) * c.BO_SGO
                    .SGG += Vxw(i) * c.BO_SGO
                    .BSW += Vxw(i) * c.BO_BSW
                    .GOR += Vxw(i) * c.BO_GOR
                    .t1 += Vxw(i) * c.BO_OilViscTemp1
                    .t2 += Vxw(i) * c.BO_OilViscTemp2
                    .v1 += Vxw(i) * c.BO_OilVisc1
                    .v2 += Vxw(i) * c.BO_OilVisc2
                    .PNA_A += Vxw(i) * c.BO_PNA_A
                    .PNA_N += Vxw(i) * c.BO_PNA_N
                    .PNA_P += Vxw(i) * c.BO_PNA_P
                End With
                i += 1
            Next
            Return bof
        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Vapor), DW_GetConstantProperties())

            Return bop.VaporDensity(T, P, bof.SGG)

        End Function

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    state = "V"
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    state = "L"
                Case Phase.Solid
                    state = "S"
            End Select

            Select Case phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Me.AUX_MMM(phase)

            Select Case phase

                Case Phase.Vapor

                    Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Vapor), DW_GetConstantProperties())

                    Select Case [property].ToLower
                        Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                            CalcAdditionalPhaseProperties(phaseID)
                        Case "compressibilityfactor"
                            result = 1 / (bop.VaporDensity(T, P, bof.SGG) * 1000 / bop.VaporMolecularWeight(bof.SGG)) / 8.314 / T * P
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                        Case "heatcapacity", "heatcapacitycp"
                            result = bop.VaporCp(T, P, bof.SGG, bof.SGO)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Case "heatcapacitycv"
                            result = bop.VaporCv(T, P, bof.SGG, bof.SGO)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                        Case "enthalpy", "enthalpynf"
                            result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                        Case "entropy", "entropynf"
                            result = Me.DW_CalcEntropy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                        Case "excessenthalpy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                        Case "excessentropy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                        Case "enthalpyf"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = 0.0#
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = 0.0#
                        Case "entropyf"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = 0.0#
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = 0.0#
                        Case "viscosity"
                            result = bop.VaporViscosity(T, P, bof.SGG)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                        Case "thermalconductivity"
                            result = bop.VaporThermalConductivity(T, P, bof.SGG, bof.SGO)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                        Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                            Me.DW_CalcCompFugCoeff(phase)
                        Case "volume", "density"
                            result = bop.VaporDensity(T, P, bof.SGG)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                        Case "surfacetension"
                            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                        Case Else
                            Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
                    End Select

                Case Phase.Liquid1

                    Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Liquid1), DW_GetConstantProperties())

                    Select Case [property].ToLower
                        Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound"
                            CalcAdditionalPhaseProperties(phaseID)
                        Case "compressibilityfactor"
                            result = 1 / (bop.LiquidDensity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW) * 1000 / bop.LiquidMolecularWeight(bof.SGO, bof.BSW)) / 8.314 / T * P
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                        Case "heatcapacity", "heatcapacitycp"
                            result = bop.LiquidCp(T, P, bof.SGG, bof.SGO, bof.BSW)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Case "heatcapacitycv"
                            result = bop.LiquidCv(T, P, bof.SGG, bof.SGO, bof.BSW)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                        Case "enthalpy", "enthalpynf"
                            result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                        Case "entropy", "entropynf"
                            result = Me.DW_CalcEntropy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                        Case "excessenthalpy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                        Case "excessentropy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                        Case "enthalpyf"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = 0.0#
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = 0.0#
                        Case "entropyf"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = 0.0#
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = 0.0#
                        Case "viscosity"
                            result = bop.LiquidViscosity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW, bof.v1, bof.t1, bof.v2, bof.t2)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                        Case "thermalconductivity"
                            result = bop.LiquidThermalConductivity(T, P, bof.SGO, bof.BSW)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                        Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                            Me.DW_CalcCompFugCoeff(phase)
                        Case "volume", "density"
                            result = bop.LiquidDensity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                        Case "surfacetension"
                            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                        Case Else
                            Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
                    End Select

            End Select


        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

            Dim result As Double

            Dim T, P As Double
            Dim composition As Object = Nothing
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
            End Select

            If phaseID > 0 Then

                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                If Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault > 0 Then
                    result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)

            End If

            If phaseID = 2 Then

                Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Vapor), DW_GetConstantProperties())

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = bop.VaporMolecularWeight(bof.SGG)

                result = 1 / (bop.VaporDensity(T, P, bof.SGG) * 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight) / 8.314 / T * P
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = bop.VaporCp(T, P, bof.SGG, bof.SGO)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                result = bop.VaporCv(T, P, bof.SGG, bof.SGO)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.DW_CalcEntropy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = 0.0#
                result = bop.VaporViscosity(T, P, bof.SGG)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                result = bop.VaporThermalConductivity(T, P, bof.SGG, bof.SGO)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                result = bop.VaporDensity(T, P, bof.SGG)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 3 Then

                Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Liquid1), DW_GetConstantProperties())

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = bop.LiquidMolecularWeight(bof.SGO, bof.BSW)

                result = 1 / (bop.LiquidDensity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW) * 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight) / 8.314 / T * P
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = bop.LiquidCp(T, P, bof.SGG, bof.SGO, bof.BSW)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                result = bop.LiquidCv(T, P, bof.SGG, bof.SGO, bof.BSW)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.DW_CalcEntropy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = 0.0#
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = 0.0#
                result = bop.LiquidViscosity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW, bof.v1, bof.t1, bof.v2, bof.t2)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                result = bop.LiquidThermalConductivity(T, P, bof.SGO, bof.BSW)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                result = bop.LiquidDensity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()

            Else

                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

        End Sub

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As PropertyPackages.Phase, ByVal Phase2 As PropertyPackages.Phase)

            Dim result As Double

            Dim T, P As Double
            Dim composition1 As Object = Nothing
            Dim composition2 As Object = Nothing

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            result = 1
            Me.CurrentMaterialStream.Phases(0).Properties.kvalue = result
            result = 0
            Me.CurrentMaterialStream.Phases(0).Properties.logKvalue = result

            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = DW_CalcTensaoSuperficial_ISOL(Phase.Liquid1, T, P)

        End Sub

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidDensity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporDensity(T, P, bof.SGG)
            End If
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidViscosity(T, P, bof.SGO, bof.SGG, bof.GOR, bof.BSW, bof.v1, bof.t1, bof.v2, bof.t2)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporViscosity(T, P, bof.SGG)
            End If
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidCp(T, P, bof.SGG, bof.SGO, bof.BSW)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporCp(T, P, bof.SGG, bof.SGO)
            End If
        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidThermalConductivity(T, P, bof.SGO, bof.BSW)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporThermalConductivity(T, P, bof.SGG, bof.SGO)
            End If
        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidMolecularWeight(bof.SGO, bof.BSW)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporMolecularWeight(bof.SGG)
            End If
        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double

            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase.Mixture), DW_GetConstantProperties())
            Return bop.VaporPressure(T, bof.SGO, bof.BSW)

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            Else
                Return False
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim bof As BlackOilFluid = CalcBOFluid(Vx, DW_GetConstantProperties())

            If DirectCast(Vx, Double()).Sum > 0.0# Then
                Select Case st
                    Case State.Liquid
                        Return bop.LiquidEnthalpy(T, P, bof.SGO, bof.SGG, bof.BSW)
                    Case State.Vapor
                        Return bop.VaporEnthalpy(T, P, bof.SGG, bof.SGO)
                End Select
            Else
                Return 0.0#
            End If

        End Function

        Public Overrides Function DW_CalcKvalue(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double) As Double()
            Return New Double() {1.0#, 1.0#}
        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcBubP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim bof As BlackOilFluid = CalcBOFluid(Vx, DW_GetConstantProperties())
            Return New Object() {bop.VaporPressure(T, bof.SGO, bof.BSW)}
        End Function

        Public Overrides Function DW_CalcBubT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {0.0#}
        End Function

        Public Overrides Function DW_CalcDewP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim bof As BlackOilFluid = CalcBOFluid(Vx, DW_GetConstantProperties())
            Return New Object() {bop.VaporPressure(T, bof.SGO, bof.BSW)}
        End Function

        Public Overrides Function DW_CalcDewT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Return New Object() {0.0#}
        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase1), DW_GetConstantProperties())
            If Phase1 = Phase.Liquid Then
                Return bop.LiquidCv(T, P, bof.SGG, bof.SGO, bof.BSW)
            ElseIf Phase1 = Phase.Vapor Then
                Return bop.VaporCv(T, P, bof.SGG, bof.SGO)
            End If
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

        End Sub

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return DW_CalcEnthalpy(Vx, T, P, st) / T
        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each s As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(s.ConstantProperties)
            Next

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then

                Dim Tc As Double() = Me.RET_VTC()

                For i = 0 To n
                    fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                Next

            Else

                For i = 0 To n
                    fugcoeff(i) = 1.0#
                Next

            End If

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

        Public Overrides Function AUX_PVAPi(sub1 As String, T As Double) As Double

            Dim comp As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = sub1).SingleOrDefault

            Return bop.VaporPressure(T, comp.ConstantProperties.BO_SGO, comp.ConstantProperties.BO_BSW)

        End Function

        Public Function DW_CalcXY(T As Double, P As Double) As Double()

            Dim cprops = DW_GetConstantProperties()
            Dim Xv As New List(Of Double)

            For Each cprop In cprops

                Dim API, WOR, GORss As Double

                Dim Tf, Trank, Ppsia As Double

                Tf = (T - 273.15) * 9 / 5 + 32
                Trank = Tf + 459.67
                Ppsia = P * 0.000145038

                API = 141.5 / cprop.BO_SGO - 131.5

                WOR = cprop.BO_BSW / (100 - cprop.BO_BSW)

                GORss = cprop.BO_GOR * 5.6738

                Dim SGdg, Rs, Pb As Double

                SGdg = cprop.BO_SGG

                Rs = SGdg * (((Ppsia) / 18.2 + 1.4) * 10 ^ (0.0125 * API - 0.00091 * Tf)) ^ 1.2048

                Pb = 18.2 * ((GORss / SGdg) ^ (1 / 1.2048) * 10 ^ (0.00091 * Tf - 0.0125 * API) - 1.4)

                Dim Bo, Bos, Boss, C, SGfg100 As Double

                Bos = 0.9759 + 0.00012 * (Rs * (SGdg / cprop.BO_SGO) ^ 0.5 + 1.25 * Tf) ^ 1.2

                Dim Tsep, Psep As Double

                Tsep = Tf
                Psep = Ppsia

                SGfg100 = cprop.BO_SGG * (1 + 0.00005912 * API * Tsep * Math.Log(Psep / 114.7) / Math.Log(10))

                C = 0.0001 * (2.81 * cprop.BO_GOR + 3.1 * Tf + 171 / SGfg100 - 118 * SGfg100 - 1102)

                Boss = Bos * (Pb / Ppsia) ^ C

                If Ppsia < Pb Then Bo = Bos Else Bo = Boss

                Dim Z, Zant, Tpc, Ppc, rhopr, Ppr, Tpr As Double

                Ppc = 677 + 15 * cprop.BO_SGG - 37.5 * cprop.BO_SGG ^ 2
                Tpc = 168 + 325 * cprop.BO_SGG - 12.5 * cprop.BO_SGG ^ 2

                Dim A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, C1, C2, C3 As Double

                A1 = 0.3265
                A2 = -1.07
                A3 = -0.5339
                A4 = 0.01569
                A5 = -0.05165
                A6 = 0.5475
                A7 = -0.7361
                A8 = 0.1844
                A9 = 0.1056
                A10 = 0.6134
                A11 = 0.721

                Ppr = Ppsia / Ppc
                Tpr = Trank / Tpc

                Z = 1

                Dim cnt As Integer = 0

                Do

                    rhopr = 0.27 * Ppr / (Z * Tpr)

                    C1 = A1 + A2 / Tpr + A3 / Tpr ^ 3 + A4 / Tpr ^ 4 + A5 / Tpr ^ 5
                    C2 = A6 + A7 / Tpr + A8 / Tpr ^ 2
                    C3 = A7 / Tpr + A8 / Tpr ^ 2

                    Zant = Z
                    Z = 1 + C1 * rhopr + C2 * rhopr ^ 2 - A9 * C3 * rhopr ^ 5 + A10 * (1 + A11 * rhopr ^ 2) * (rhopr ^ 2 / Tpr ^ 3) * Math.Exp(-A11 * rhopr ^ 2)

                    cnt += 1

                Loop Until Math.Abs(Z - Zant) < 0.0001 Or cnt > 1000

                Dim Bg As Double

                Bg = 0.02827 * Z * Trank / Ppsia

                Dim rhoo, rhog, rhoa, rhoo0, rhog0, rhoa0 As Double

                rhoa0 = 997
                rhog0 = cprop.BO_SGG * 1.22
                rhoo0 = cprop.BO_SGO * 997

                rhoo = (rhoo0 + rhog0 * Rs / 5.6738) / Bo

                rhog = rhog0 / Bg

                Dim Bw As Double

                A1 = 0.9911 + 0.0000635 * Tf + 0.00000085 * Tf ^ 2
                A2 = -0.000001093 - 0.000000003497 * Tf + 0.00000000000457 * Tf ^ 2
                A3 = -0.00000000005 + 0.0000000000006429 * Tf - 0.00000000000000143 * Tf ^ 2

                Bw = A1 + A2 * Ppsia + A3 * Ppsia ^ 2

                rhoa = rhoa0 / Bw

                Dim denom, xg, xo, xw As Double

                denom = rhog0 * cprop.BO_GOR + rhoo0 + rhoa0 * WOR
                xg = (rhog * Bg * (GORss - Rs) / 5.6738) / denom
                xw = rhoa * Bw * WOR / denom
                xo = 1 - xg - xw
                If xo < 0.0# Then xo = 0.0#

                Xv.Add(xg)

            Next

            Return Xv.ToArray

        End Function

        Public Overrides Function AUX_MMM(Vz() As Double, Optional state As String = "") As Double

            Dim bof As BlackOilFluid = CalcBOFluid(Vz, DW_GetConstantProperties())

            If state = "L" Then
                Return bop.LiquidMolecularWeight(bof.SGO, bof.BSW)
            Else
                Return bop.VaporMolecularWeight(bof.SGG)
            End If

        End Function

        Public Overrides Function AUX_MMM(Phase As Phase) As Double

            Dim bof As BlackOilFluid = CalcBOFluid(RET_VMOL(Phase), DW_GetConstantProperties())

            Select Case Phase
                Case PropertyPackages.Phase.Vapor
                    Return bop.VaporMolecularWeight(bof.SGG)
                Case PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Liquid1, PropertyPackages.Phase.Liquid2, PropertyPackages.Phase.Liquid3
                    Return bop.LiquidMolecularWeight(bof.SGO, bof.BSW)
                Case PropertyPackages.Phase.Mixture
                    Dim val As Double = 0.0#
                    Dim subst As Interfaces.ICompound
                    For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                        val += subst.MoleFraction.GetValueOrDefault * subst.ConstantProperties.Molar_Weight
                    Next
                    Return val
                Case Else
                    Return 0.0#
            End Select

        End Function

        Public Overrides Function AUX_CONVERT_MOL_TO_MASS(subst As String, phasenumber As Integer) As Double

            Dim mol_x_mm As Double
            Dim sub1 As Interfaces.ICompound

            For Each sub1 In Me.CurrentMaterialStream.Phases(phasenumber).Compounds.Values
                If phasenumber = 2 Then
                    mol_x_mm += sub1.MoleFraction.GetValueOrDefault * bop.VaporMolecularWeight(sub1.ConstantProperties.BO_SGG)
                ElseIf phasenumber = 3 Then
                    mol_x_mm += sub1.MoleFraction.GetValueOrDefault * bop.LiquidMolecularWeight(sub1.ConstantProperties.BO_SGO, sub1.ConstantProperties.BO_BSW)
                Else
                    mol_x_mm += sub1.MoleFraction.GetValueOrDefault * bop.LiquidMolecularWeight(sub1.ConstantProperties.BO_SGG, sub1.ConstantProperties.BO_BSW)
                End If
            Next

            sub1 = Me.CurrentMaterialStream.Phases(phasenumber).Compounds(subst)

            If mol_x_mm <> 0.0# Then
                If phasenumber = 2 Then
                    Return sub1.MoleFraction.GetValueOrDefault * bop.VaporMolecularWeight(sub1.ConstantProperties.BO_SGG) / mol_x_mm
                ElseIf phasenumber = 3 Then
                    Return sub1.MoleFraction.GetValueOrDefault * bop.LiquidMolecularWeight(sub1.ConstantProperties.BO_SGO, sub1.ConstantProperties.BO_BSW) / mol_x_mm
                Else
                    Return sub1.MoleFraction.GetValueOrDefault * bop.LiquidMolecularWeight(sub1.ConstantProperties.BO_SGO, sub1.ConstantProperties.BO_BSW) / mol_x_mm
                End If
            Else
                Return 0.0#
            End If

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_Z", "Compressibility Factor", "Compressibility Factor Calculation Routine")

            IObj?.SetCurrent()

            Dim bof As BlackOilFluid = CalcBOFluid(Vx, DW_GetConstantProperties())

            Dim val = 1 / (bop.VaporDensity(T, P, bof.SGG) * 1000 / AUX_MMM(Vx)) / 8.314 / T * P

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Compressibility Factor: {0}", val))

            IObj?.Close()

            Return val

        End Function

    End Class

End Namespace