'    Raoult`s Law Property Package 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

'Imports CAPEOPEN_PD.CAPEOPEN
'Imports DWSIM.SimulationObjects
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.PropertyPackages

Imports System.Math

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(RaoultPropertyPackage.ClassId)>
    <System.Serializable()> Public Class RaoultPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "407A13EC-1C55-462a-AEA4-9709B11367B0"

        Private m_props As New PropertyPackages.Auxiliary.PROPS
        Private m_id As New PropertyPackages.Auxiliary.Ideal
        Public m_lk As New PropertyPackages.Auxiliary.LeeKesler

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Overrides ReadOnly Property DisplayName As String = "Ideal (Raoult's Law)"

        Public Overrides ReadOnly Property DisplayDescription As String =
            "Property Package that uses Raoult's Law to calculate K-values. Recommended for gases and hydrocarbons at low pressures."

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

            EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.Ideal

        End Sub

        Public Sub New()

            MyBase.New()

            EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.Ideal

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.VaporPressure

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Enthalpy_Entropy_CpCv = "Ideal Gas Cp"
                .Vapor_Density = "Peng-Robinson EOS"
                .Liquid_Fugacity = "Vapor Pressure / Henry's Constant"
                .Liquid_Enthalpy_Entropy_CpCv = "Ideal Gas Cp + Enthalpy of Vaporization"
            End With

        End Sub

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfigPropertyPackage() With {._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfigPropertyPackage() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

#Region "    DWSIM Functions"

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_CPm(Phase1, T)
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.m_id.H_RA_MIX("L", T, P, RET_VMOL(Phase.Liquid), RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Liquid), Me.RET_VHVAP(T))
            HV = Me.m_id.H_RA_MIX("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Vapor), Me.RET_VHVAP(T))
            HM = Me.CurrentMaterialStream.Phases(1).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV

            Dim ent_massica = HM
            Dim flow = Me.CurrentMaterialStream.Phases(0).Properties.massflow
            Return ent_massica * flow

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_CONDTL(T)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_CONDTG(T, P)
            End If
            Return Nothing
        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQDENS(T)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPDENS(T, P)
            ElseIf Phase1 = Phase.Mixture Then
                Return Me.CurrentMaterialStream.Phases(1).Properties.volumetric_flow.GetValueOrDefault * Me.AUX_LIQDENS(T) / Me.CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault + Me.CurrentMaterialStream.Phases(2).Properties.volumetric_flow.GetValueOrDefault * Me.AUX_VAPDENS(T, P) / Me.CurrentMaterialStream.Phases(0).Properties.volumetric_flow.GetValueOrDefault
            End If
        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_MMM(Phase1)
        End Function

        Public Overrides Sub DW_CalcOverallProps()
            MyBase.DW_CalcOverallProps()
        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = "", pstate As State

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    state = "V"
                    pstate = PropertyPackages.State.Vapor
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    state = "L"
                    pstate = PropertyPackages.State.Liquid
                Case Phase.Solid
                    state = "S"
                    pstate = PropertyPackages.State.Solid
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
                    result = 0.0#
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    If state = "L" Then
                        result = Me.AUX_LIQCPm(T, phaseID)
                    Else
                        result = Me.AUX_CPm(phase, T)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    If state = "L" Then
                        result = Me.AUX_LIQCPm(T, phaseID)
                    Else
                        result = Me.AUX_CPm(phase, T) - 8.314 / Me.AUX_MMM(phase)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = m_id.H_RA_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0, Me.RET_VHVAP(T))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = m_id.S_RA_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0, Me.RET_VHVAP(T))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = m_id.H_RA_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase), Me.RET_VHVAP(T))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = m_id.S_RA_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase), Me.RET_VHVAP(T))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    If state = "L" Then
                        result = Me.AUX_LIQVISCm(T, P, phaseID)
                    Else
                        result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T, phaseID)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If state = "L" Then
                        result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                    Else
                        result = Me.AUX_VAPDENS(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcPhaseProps", ComponentName & String.Format(" (Phase Properties - {0})", [Enum].GetName(Phase.GetType, Phase)), "Property Package Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase properties of the currently associated Material Stream.")

            IObj?.Paragraphs.Add("Specified Phase: " & [Enum].GetName(Phase.GetType, Phase))

            Dim result As Double
            Dim resultObj As Object
            Dim dwpl As Phase

            Dim T, P As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
                Case PropertyPackages.Phase.Mixture
                    phaseID = 0
                    dwpl = PropertyPackages.Phase.Mixture
                Case PropertyPackages.Phase.Vapor
                    phaseID = 2
                    dwpl = PropertyPackages.Phase.Vapor
                Case PropertyPackages.Phase.Liquid1
                    phaseID = 3
                    dwpl = PropertyPackages.Phase.Liquid1
                Case PropertyPackages.Phase.Liquid2
                    phaseID = 4
                    dwpl = PropertyPackages.Phase.Liquid2
                Case PropertyPackages.Phase.Liquid3
                    phaseID = 5
                    dwpl = PropertyPackages.Phase.Liquid3
                Case PropertyPackages.Phase.Liquid
                    phaseID = 1
                    dwpl = PropertyPackages.Phase.Liquid
                Case PropertyPackages.Phase.Aqueous
                    phaseID = 6
                    dwpl = PropertyPackages.Phase.Aqueous
                Case PropertyPackages.Phase.Solid
                    phaseID = 7
                    dwpl = PropertyPackages.Phase.Solid
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
                IObj?.SetCurrent
                Me.DW_CalcCompVolFlow(phaseID)
                IObj?.SetCurrent
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                IObj?.SetCurrent
                result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                result = P * (Me.AUX_MMM(Phase) / 1000.0 / result) / (8.314 * T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                IObj?.SetCurrent
                result = DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                IObj?.SetCurrent
                result = DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                IObj?.SetCurrent
                resultObj = Me.m_id.CpCv("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Me.AUX_LIQCPm(T, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp.GetValueOrDefault
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                IObj?.SetCurrent
                result = Me.AUX_CONDTL(T, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                IObj?.SetCurrent
                result = Me.AUX_LIQVISCm(T, P, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                IObj?.SetCurrent
                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                IObj?.SetCurrent
                result = DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                IObj?.SetCurrent
                result = DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = 1
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                IObj?.SetCurrent
                result = Me.AUX_CPm(PropertyPackages.Phase.Vapor, T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Me.AUX_CPm(Phase.Vapor, T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp.GetValueOrDefault - 8.314 / Me.AUX_MMM(Phase.Vapor)
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                IObj?.SetCurrent
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                IObj?.SetCurrent
                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 7 Then

                result = Me.AUX_SOLIDDENS
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                IObj?.SetCurrent
                result = Me.DW_CalcSolidEnthalpy(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result / T
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 0.0# 'result
                IObj?.SetCurrent
                result = Me.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Phase.Solid), constprops)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                IObj?.SetCurrent
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = 0.0# 'result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = 1.0E+20
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = 1.0E+20
            ElseIf phaseID = 1 Then

                IObj?.SetCurrent
                DW_CalcLiqMixtureProps()

            Else

                IObj?.SetCurrent
                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            Else
                'result = Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow.GetValueOrDefault / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                'Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Phase.Liquid), Me.RET_VPC(Phase.Liquid), Me.RET_VW(Phase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_SURFTM(T)
        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As PropertyPackages.Phase, ByVal Phase2 As PropertyPackages.Phase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T, P)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            End If
            Return Nothing
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            Return True

        End Function

        Function dfidRbb_H(ByVal Rbb, ByVal Kb0, ByVal Vz, ByVal Vu, ByVal sum_Hvi0, ByVal DHv, ByVal DHl, ByVal HT) As Double

            Dim i As Integer = 0
            Dim n = Vz.Length - 1

            Dim Vpbb2(n), L2, V2, Kb2 As Double

            i = 0
            Dim sum_pi2 = 0
            Dim sum_eui_pi2 = 0
            Do
                Vpbb2(i) = Vz(i) / (1 - Rbb + Kb0 * Rbb * Exp(Vu(i)))
                sum_pi2 += Vpbb2(i)
                sum_eui_pi2 += Exp(Vu(i)) * Vpbb2(i)
                i = i + 1
            Loop Until i = n + 1
            Kb2 = sum_pi2 / sum_eui_pi2

            L2 = (1 - Rbb) * sum_pi2
            V2 = 1 - L2

            Return L2 * (DHv - DHl) - sum_Hvi0 - DHv + HT * Me.AUX_MMM(Vz)

        End Function

        Function dfidRbb_S(ByVal Rbb, ByVal Kb0, ByVal Vz, ByVal Vu, ByVal sum_Hvi0, ByVal DHv, ByVal DHl, ByVal ST) As Double

            Dim i As Integer = 0
            Dim n = Vz.Length - 1

            Dim Vpbb2(n), L, V As Double

            i = 0
            Dim sum_pi2 = 0
            Dim sum_eui_pi2 = 0
            Do
                Vpbb2(i) = Vz(i) / (1 - Rbb + Kb0 * Rbb * Exp(Vu(i)))
                sum_pi2 += Vpbb2(i)
                sum_eui_pi2 += Exp(Vu(i)) * Vpbb2(i)
                i = i + 1
            Loop Until i = n + 1

            L = (1 - Rbb) * sum_pi2
            V = 1 - L

            Return L * (DHv - DHl) - sum_Hvi0 - DHv + ST * Me.AUX_MMM(Vz)

        End Function

#End Region

#Region "    Extras"

#End Region

#Region "    Metodos Numericos"

        Public Function IntegralSimpsonCp(ByVal a As Double,
                 ByVal b As Double,
                 ByVal Epsilon As Double, ByVal subst As String) As Double

            Dim Result As Double
            Dim switch As Boolean = False
            Dim h As Double
            Dim s As Double
            Dim s1 As Double
            Dim s2 As Double
            Dim s3 As Double
            Dim x As Double
            Dim tm As Double

            If a > b Then
                switch = True
                tm = a
                a = b
                b = tm
            ElseIf Abs(a - b) < 0.01 Then
                Return 0
            End If

            s2 = 1.0#
            h = b - a
            s = Me.AUX_CPi(subst, a) + Me.AUX_CPi(subst, b)
            Do
                s3 = s2
                h = h / 2.0#
                s1 = 0.0#
                x = a + h
                Do
                    s1 = s1 + 2.0# * Me.AUX_CPi(subst, x)
                    x = x + 2.0# * h
                Loop Until Not x < b
                s = s + s1
                s2 = (s + s1) * h / 3.0#
                x = Abs(s3 - s2) / 15.0#
            Loop Until Not x > Epsilon
            Result = s2

            If switch Then Result = -Result

            IntegralSimpsonCp = Result

        End Function

        Public Function IntegralSimpsonCp_T(ByVal a As Double,
         ByVal b As Double,
         ByVal Epsilon As Double, ByVal subst As String) As Double

            'Cp = A + B*T + C*T^2 + D*T^3 + E*T^4 where Cp in kJ/kg-mol , T in K 


            Dim Result As Double
            Dim h As Double
            Dim s As Double
            Dim s1 As Double
            Dim s2 As Double
            Dim s3 As Double
            Dim x As Double
            Dim tm As Double
            Dim switch As Boolean = False

            If a > b Then
                switch = True
                tm = a
                a = b
                b = tm
            ElseIf Abs(a - b) < 0.01 Then
                Return 0
            End If

            s2 = 1.0#
            h = b - a
            s = Me.AUX_CPi(subst, a) / a + Me.AUX_CPi(subst, b) / b
            Do
                s3 = s2
                h = h / 2.0#
                s1 = 0.0#
                x = a + h
                Do
                    s1 = s1 + 2.0# * Me.AUX_CPi(subst, x) / x
                    x = x + 2.0# * h
                Loop Until Not x < b
                s = s + s1
                s2 = (s + s1) * h / 3.0#
                x = Abs(s3 - s2) / 15.0#
            Loop Until Not x > Epsilon
            Result = s2

            If switch Then Result = -Result

            IntegralSimpsonCp_T = Result

        End Function

#End Region

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If OverrideEnthalpyCalculation Then

                Return EnthalpyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                If EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.LeeKesler Then
                    EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.Ideal
                End If

                Dim H As Double

                If st = State.Liquid Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0, 1 'Ideal
                            H = Me.RET_Hid(298.15, T, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                        Case 2 'Excess
                            Throw New Exception("Raoult's Law Property Package: Excess Enthalpy/Entropy/Cp calculation mode is not supported.")
                        Case 3 'Experimental Liquid
                            H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                    End Select
                ElseIf st = State.Vapor Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0, 1 'Ideal
                            H = Me.RET_Hid(298.15, T, Vx)
                        Case 2 'Excess
                            H = Me.RET_Hid(298.15, T, Vx)
                        Case 3 'Experimental Liquid
                            H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    End Select
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        H = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties)
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0, 1 'Ideal
                                H = Me.RET_Hid(298.15, T, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                            Case 2
                                Throw New Exception("Raoult's Law Property Package: Excess Enthalpy/Entropy/Cp calculation mode is not supported.")
                            Case 3, 4 'Experimental Liquid
                                H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                        End Select
                    End If
                End If

                Return H

            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim H As Double

            If st = State.Liquid Then
                H = Me.m_id.H_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T))
            ElseIf st = State.Vapor Then
                H = Me.m_id.H_RA_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T))
            ElseIf st = State.Solid Then
                H = Me.m_id.H_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T)) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_CPm(Phase1, T) - 8.314
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

            Select Case phase
                Case Phase.Liquid
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(1).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                    Next
                Case Phase.Aqueous
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(6).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                    Next
                Case Phase.Liquid1
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                    Next
                Case Phase.Liquid2
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(4).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                    Next
                Case Phase.Liquid3
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(5).Compounds.Values
                        subst.PartialVolume = subst.ConstantProperties.Molar_Weight / AUX_LIQDENSi(subst, T)
                    Next
                Case Phase.Vapor
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                        subst.PartialVolume = 8.314 * T / P
                    Next
            End Select

        End Sub

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double
            Dim val As Double
            Dim Z As Double = 1
            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Phase.Vapor)

            Return val
        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If OverrideEntropyCalculation Then

                Return EntropyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                If EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.LeeKesler Then
                    EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.Ideal
                End If

                Dim S As Double

                If st = State.Liquid Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0, 1 'Ideal
                            S = Me.RET_Sid(298.15, T, P, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                        Case 2 'Excess
                            Throw New Exception("Raoult's Law Property Package: Excess Enthalpy/Entropy/CP calculation mode is not supported.")
                        Case 3, 4 'Experimental Liquid
                            S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                    End Select
                ElseIf st = State.Vapor Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0, 1 'Ideal
                            S = Me.RET_Sid(298.15, T, P, Vx)
                        Case 2 'Excess
                            S = Me.RET_Sid(298.15, T, P, Vx)
                        Case 3, 4 'Experimental Liquid
                            S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                    End Select
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        S = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties) / T
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0, 1 'Ideal
                                S = Me.RET_Sid(298.15, T, P, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                            Case 2 'Excess
                                Throw New Exception("Raoult's Law Property Package: Excess Enthalpy/Entropy/Cp calculation mode is not supported.")
                            Case 3, 4 'Experimental Liquid
                                S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                        End Select
                    End If
                End If

                Return S

            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                S = Me.m_id.S_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T))
            ElseIf st = State.Vapor Then
                S = Me.m_id.S_RA_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T))
            ElseIf st = State.Solid Then
                S = Me.m_id.S_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T)) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcFugCoeff", "Fugacity Coefficient", "Property Package Fugacity Coefficient Calculation Routine")

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add(String.Format("<h2>Input Parameters</h2>"))

            IObj?.Paragraphs.Add(String.Format("Temperature: {0} K", T))
            IObj?.Paragraphs.Add(String.Format("Pressure: {0} Pa", P))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", DirectCast(Vx, Double()).ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", [Enum].GetName(st.GetType, st)))

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then
                Dim Tc As Double() = Me.RET_VTC()
                For i = 0 To n
                    If T / Tc(i) >= 1.0 Then
                        IObj?.SetCurrent()
                        If UseHenryConstants And HasHenryConstants(RET_VNAMES(i)) Then
                            Dim hc = AUX_KHenry(RET_VNAMES(i), T)
                            IObj?.Paragraphs.Add(String.Format("Henry's Constant (H) @ {0} K: {1} Pa", T, hc))
                            fugcoeff(i) = hc / P
                        Else
                            fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                        End If
                    Else
                        IObj?.SetCurrent()
                        fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                    End If
                Next
            Else
                For i = 0 To n
                    fugcoeff(i) = 1
                Next
            End If

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Fugacity Coefficients: {0}", fugcoeff.ToMathArrayString))

            IObj?.Close()

            Return fugcoeff

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Return 1.0

        End Function

    End Class

End Namespace
