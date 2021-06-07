'    Steam Tables 2 Property Package 
'    Copyright 2021 Daniel Wagner O. de Medeiros
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

'Imports DWSIM.SimulationObjects
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.MathOps.MathEx
Imports System.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(SteamTables2PropertyPackage.ClassId)>
    <System.Serializable()> Public Class SteamTables2PropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "170D6E8A-8890-4bf9-B7A0-E4A3FDBFD589"

        Private stat As Integer

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            Me.SupportedComponents.Add(15)
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

            IsConfigurable = False

        End Sub

        Public Overrides Function GetModel() As Object

            Return New SteamProperties.StmProp()

        End Function

        Public Overrides Sub AddDefaultCompounds(compnames() As String)

            MyBase.AddDefaultCompounds(New String() {"Water"})

        End Sub

        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            Get
                If CurrentMaterialStream IsNot Nothing Then
                    Select Case CurrentMaterialStream.ForcePhase
                        Case ForcedPhase.None
                            Return New Auxiliary.FlashAlgorithms.SteamTables
                        Case ForcedPhase.GlobalDef
                            If CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase = ForcedPhase.None Then
                                Return New Auxiliary.FlashAlgorithms.UniversalFlash() With {.FlashSettings = FlashSettings}
                            Else
                                Return New Auxiliary.FlashAlgorithms.ForcedPhaseFlash() With {.ForcePhase = CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase}
                            End If
                        Case Else
                            Return New Auxiliary.FlashAlgorithms.ForcedPhaseFlash() With {.ForcePhase = CurrentMaterialStream.ForcePhase}
                    End Select
                Else
                    Return New Auxiliary.FlashAlgorithms.SteamTables
                End If
            End Get
        End Property

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim steam As New SteamProperties.StmProp

            Dim Tsat As Double = steam.Tsat(P / 1000, stat, 0)
            If T < Tsat Then
                Return 1 / steam.vgt(T, stat, 0)
            Else
                Return 1 / steam.vpt(P / 1000, T, stat, 0)
            End If

        End Function

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim steam As New SteamProperties.StmProp

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim sta As State = State.Vapor

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    sta = State.Vapor
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
                    sta = State.Liquid
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

            Select Case [property].ToLower
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
                Case "compressibilityfactor"
                    result = 1 / (1 / steam.vpt(P / 1000, T, stat, 0) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    result = GetCp(P, T, sta)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    result = GetCv(P, T, sta)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = steam.hpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = steam.spt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                Case "excessentropy"
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = steam.hpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = steam.spt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    result = steam.mupt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    result = steam.kpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    result = 1 / steam.vpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

            Dim steam As New SteamProperties.StmProp

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcPhaseProps", ComponentName & String.Format(" (Phase Properties - {0})", [Enum].GetName(Phase.GetType, Phase)), "Property Package Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase properties of the currently associated Material Stream.")

            IObj?.Paragraphs.Add("Specified Phase: " & [Enum].GetName(Phase.GetType, Phase))

            Dim result As Double

            Dim T, Tsat, P As Double
            Dim composition As Object = Nothing
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Tsat = steam.Tsat(P / 1000, stat, 0)

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
                result = result * Me.AUX_MMM(PropertyPackages.Phase.Mixture) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
            End If

            If phaseID = 3 Then

                If Math.Abs(T - Tsat) < 0.01 Then

                    result = 1 / steam.vft(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / ((1 / steam.vft(T, stat, 0)) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = GetCp(P, T, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = GetCv(P, T, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = steam.kft(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = steam.muft(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                Else

                    result = 1 / steam.vpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (1 / steam.vpt(P / 1000, T, stat, 0) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = GetCp(P, T, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = GetCv(P, T, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = steam.kpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = steam.mupt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                End If

            ElseIf phaseID = 2 Then

                If Math.Abs(T - Tsat) < 0.01 Then

                    result = 1 / steam.vgt(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (1 / steam.vgt(T, stat, 0) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = GetCp(P, T, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = GetCv(P, T, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = steam.kgt(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = steam.mugt(T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                Else

                    result = 1 / steam.vpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (1 / steam.vpt(P / 1000, T, stat, 0) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = GetCp(P, T, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = GetCv(P, T, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = steam.kpt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = steam.mupt(P / 1000, T, stat, 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                End If

            ElseIf phaseID = 1 Then

                IObj?.SetCurrent
                DW_CalcLiqMixtureProps()

            Else

                IObj?.SetCurrent
                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(PropertyPackages.Phase.Mixture) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

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
            Dim Tr = T / 647.13
            result = 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = result

        End Sub

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            Else
                Return False
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim steam As New SteamProperties.StmProp

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEnthalpy", "Steam Tables Enthalpy", "Property Package Enthalpy Calculation Routine")

            IObj?.SetCurrent()

            Dim Tsat As Double = steam.Tsat(P / 1000, stat, 0)
            Dim Tcrit As Double = 374.0 + 273.15
            Dim Tbound = 1073.15
            Dim Tmin = 273.15
            Select Case st
                Case State.Liquid
                    If T > Tsat Then
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.2
                        x2 = Tmin + (Tsat - Tmin) * 0.4
                        x3 = Tmin + (Tsat - Tmin) * 0.6
                        x4 = Tmin + (Tsat - Tmin) * 0.8
                        x5 = Tmin + (Tsat - Tmin) * 0.9
                        p1 = steam.hft(x1, stat, 0)
                        p2 = steam.hft(x2, stat, 0)
                        p3 = steam.hft(x3, stat, 0)
                        p4 = steam.hft(x4, stat, 0)
                        p5 = steam.hft(x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return steam.hft(T, stat, 0)
                    Else
                        Return steam.hpt(P / 1000, T, stat, 0)
                    End If
                Case State.Vapor
                    If T > Tbound Then
                        CurrentMaterialStream.Flowsheet.ShowMessage(String.Format("Steam Tables Enthalpy Calculation: Temperature outside valid range ({0} > {1}), extrapolating calculated values...", T, Tbound), Interfaces.IFlowsheet.MessageType.Warning)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tcrit + (Tbound - Tcrit) * 0.2
                        x2 = Tcrit + (Tbound - Tcrit) * 0.4
                        x3 = Tcrit + (Tbound - Tcrit) * 0.6
                        x4 = Tcrit + (Tbound - Tcrit) * 0.8
                        x5 = Tcrit + (Tbound - Tcrit) * 0.9
                        p1 = steam.hpt(P / 1000, x1, stat, 0)
                        p2 = steam.hpt(P / 1000, x2, stat, 0)
                        p3 = steam.hpt(P / 1000, x3, stat, 0)
                        p4 = steam.hpt(P / 1000, x4, stat, 0)
                        p5 = steam.hpt(P / 1000, x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return steam.hgt(T, stat, 0)
                    ElseIf T > Tsat Then
                        Return steam.hpt(P / 1000, T, stat, 0)
                    Else
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tsat + (Tcrit - Tsat) * 0.2
                        x2 = Tsat + (Tcrit - Tsat) * 0.4
                        x3 = Tsat + (Tcrit - Tsat) * 0.6
                        x4 = Tsat + (Tcrit - Tsat) * 0.8
                        x5 = Tsat + (Tcrit - Tsat) * 0.9
                        p1 = steam.hgt(x1, stat, 0)
                        p2 = steam.hgt(x2, stat, 0)
                        p3 = steam.hgt(x3, stat, 0)
                        p4 = steam.hgt(x4, stat, 0)
                        p5 = steam.hgt(x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    End If
                Case Else
                    Return steam.hpt(P / 1000, T, stat, 0)
            End Select
        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.hpt(P / 1000, T, stat, 0) - Me.RET_Hid(298.15, T, Vx)
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

        End Sub

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim steam As New SteamProperties.StmProp

            Dim Tsat As Double = steam.Tsat(P / 1000, stat, 0)
            Dim Tcrit As Double = 374.0 + 273.15
            Dim Tmin As Double = 273.15
            Dim Tbound As Double = 1073.15
            Select Case st
                Case State.Liquid
                    If T > Tsat Then
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.2
                        x2 = Tmin + (Tsat - Tmin) * 0.4
                        x3 = Tmin + (Tsat - Tmin) * 0.6
                        x4 = Tmin + (Tsat - Tmin) * 0.8
                        x5 = Tmin + (Tsat - Tmin) * 0.9
                        p1 = steam.sft(x1, stat, 0)
                        p2 = steam.sft(x2, stat, 0)
                        p3 = steam.sft(x3, stat, 0)
                        p4 = steam.sft(x4, stat, 0)
                        p5 = steam.sft(x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return steam.sft(T, stat, 0)
                    Else
                        Return steam.spt(P / 1000, T, stat, 0)
                    End If
                Case State.Vapor
                    If T > Tbound Then
                        CurrentMaterialStream.Flowsheet.ShowMessage(String.Format("Steam Tables Entropy Calculation: Temperature outside valid range ({0} > {1}), extrapolating calculated values...", T, Tbound), Interfaces.IFlowsheet.MessageType.Warning)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tcrit + (Tbound - Tcrit) * 0.2
                        x2 = Tcrit + (Tbound - Tcrit) * 0.4
                        x3 = Tcrit + (Tbound - Tcrit) * 0.6
                        x4 = Tcrit + (Tbound - Tcrit) * 0.8
                        x5 = Tcrit + (Tbound - Tcrit) * 0.9
                        p1 = steam.spt(P / 1000, x1, stat, 0)
                        p2 = steam.spt(P / 1000, x2, stat, 0)
                        p3 = steam.spt(P / 1000, x3, stat, 0)
                        p4 = steam.spt(P / 1000, x4, stat, 0)
                        p5 = steam.spt(P / 1000, x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return steam.sgt(T, stat, 0)
                    ElseIf T > Tsat Then
                        Return steam.spt(P / 1000, T, stat, 0)
                    Else
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tsat + (Tcrit - Tsat) * 0.2
                        x2 = Tsat + (Tcrit - Tsat) * 0.4
                        x3 = Tsat + (Tcrit - Tsat) * 0.6
                        x4 = Tsat + (Tcrit - Tsat) * 0.8
                        x5 = Tsat + (Tcrit - Tsat) * 0.9
                        p1 = steam.sgt(x1, stat, 0)
                        p2 = steam.sgt(x2, stat, 0)
                        p3 = steam.sgt(x3, stat, 0)
                        p4 = steam.sgt(x4, stat, 0)
                        p5 = steam.sgt(x5, stat, 0)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    End If
                Case Else
                    Return steam.spt(P / 1000, T, stat, 0)
            End Select
        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.spt(P / 1000, T, stat, 0) - Me.RET_Sid(298.15, T, P, Vx)
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()
            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then
                For i = 0 To n
                    fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                Next
            Else
                For i = 0 To n
                    fugcoeff(i) = 1
                Next
            End If
            Return fugcoeff
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim steam As New SteamProperties.StmProp
            Return 1 / ((1 / steam.vpt(P / 1000, T, stat, 0)) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P

        End Function

        Public Overrides Function AUX_PVAPi(index As Integer, T As Double) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.Psat(T, stat, 0) * 1000
        End Function

        Public Overrides Function AUX_PVAPi(sub1 As ICompoundConstantProperties, T As Double) As Object
            Dim steam As New SteamProperties.StmProp
            Return steam.Psat(T, stat, 0) * 1000
        End Function

        Public Overrides Function AUX_PVAPi(sub1 As String, T As Double) As Object
            Dim steam As New SteamProperties.StmProp
            Return steam.Psat(T, stat, 0) * 1000
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, index As Integer) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.Tsat(PVAP / 1000, stat, 0)
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, subst As ICompoundConstantProperties) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.Tsat(PVAP / 1000, stat, 0)
        End Function

        Public Overrides Function AUX_TSATi(PVAP As Double, subst As String) As Double
            Dim steam As New SteamProperties.StmProp
            Return steam.Tsat(PVAP / 1000, stat, 0)
        End Function

        Private Function GetCp(P As Double, T As Double, st As State) As Double

            Dim steam As New SteamProperties.StmProp
            Dim region = steam.SubRegion(P / 1000, T)
            If region > 3 Then
                Return steam.cppt(P / 1000, T, stat, 0)
            Else
                Dim steam0 As New IAPWS_IF97()
                Dim Tsat = steam.Tsat(P / 1000, stat, 0)
                If Math.Abs(T - Tsat) < 0.01 Then
                    Select Case st
                        Case State.Liquid
                            Return steam0.cpSatLiqTW(T)
                        Case State.Vapor
                            Return steam0.cpSatVapTW(T)
                    End Select
                ElseIf T > Tsat Then
                    Select Case st
                        Case State.Liquid
                            Return steam0.cpSatLiqTW(T)
                        Case State.Vapor
                            Return steam0.cpW(T, P / 100000)
                    End Select
                Else
                    Select Case st
                        Case State.Liquid
                            Return steam0.cpW(T, P / 100000)
                        Case State.Vapor
                            Return steam0.cpSatVapTW(T)
                    End Select
                End If
            End If

        End Function

        Private Function GetCv(P As Double, T As Double, st As State) As Double

            Dim steam As New SteamProperties.StmProp
            Dim region = steam.SubRegion(P / 1000, T)
            If region > 3 Then
                Return steam.cppt(P / 1000, T, stat, 0)
            Else
                Dim steam0 As New IAPWS_IF97()
                Dim Tsat = steam.Tsat(P / 1000, stat, 0)
                If Math.Abs(T - Tsat) < 0.01 Then
                    Select Case st
                        Case State.Liquid
                            Return steam0.cvSatLiqTW(T)
                        Case State.Vapor
                            Return steam0.cvSatVapTW(T)
                    End Select
                ElseIf T > Tsat Then
                    Select Case st
                        Case State.Liquid
                            Return steam0.cvSatLiqTW(T)
                        Case State.Vapor
                            Return steam0.cvW(T, P / 100000)
                    End Select
                Else
                    Select Case st
                        Case State.Liquid
                            Return steam0.cvW(T, P / 100000)
                        Case State.Vapor
                            Return steam0.cvSatVapTW(T)
                    End Select
                End If
            End If

        End Function

    End Class

End Namespace

