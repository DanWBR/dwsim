'    Peng-Robinson-Stryjek-Vera 2 w/ Margules Mixing Rules Property Package 
'    Copyright 2012 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports System.Math
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages.Auxiliary.FlashAlgorithms
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica

Namespace DWSIM.SimulationObjects.PropertyPackages

    <System.Serializable()> Public Class PRSV2PropertyPackage

        Inherits DWSIM.SimulationObjects.PropertyPackages.PropertyPackage

        Private m_props As New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.PROPS
        Public m_pr As New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.PRSV2

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            MyBase.New()

            'With Me.Parameters
            '    .Add("PP_USE_EOS_LIQDENS", 0)
            'End With

            Me.IsConfigurable = True
            Me.ConfigForm = New FormConfigPP
            Me._packagetype = PropertyPackages.PackageType.EOS

        End Sub

        Public Overrides Sub ReconfigureConfigForm()
            MyBase.ReconfigureConfigForm()
            Me.ConfigForm = New FormConfigPRSV2
        End Sub

        Public Overrides Sub ConfigParameters()
            m_par = New System.Collections.Generic.Dictionary(Of String, Double)
            With Me.Parameters
                .Clear()
                .Add("PP_PHFILT", 0.001)
                .Add("PP_PSFILT", 0.001)
                .Add("PP_PHFELT", 0.001)
                .Add("PP_PSFELT", 0.001)
                .Add("PP_PHFMEI", 50)
                .Add("PP_PSFMEI", 50)
                .Add("PP_PHFMII", 100)
                .Add("PP_PSFMII", 100)
                .Add("PP_PTFMEI", 100)
                .Add("PP_PTFMII", 100)
                .Add("PP_PTFILT", 0.001)
                .Add("PP_PTFELT", 0.001)
                .Add("PP_FLASHALGORITHM", 2)
                .Add("PP_FLASHALGORITHMFASTMODE", 1)
                .Add("PP_IDEAL_MIXRULE_LIQDENS", 0)
                .Add("PP_USEEXPLIQDENS", 0)
                .Add("PP_USE_EOS_LIQDENS", 0)
                .Add("PP_IDEAL_VAPOR_PHASE_FUG", 1)
                .Add("PP_ENTH_CP_CALC_METHOD", 1)
                .Item("PP_IDEAL_MIXRULE_LIQDENS") = 1
                .Item("PP_USEEXPLIQDENS") = 1
            End With
        End Sub

#Region "    DWSIM Functions"

        Public Overrides Function DW_CalcCp_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Select Case fase1
                Case Fase.Liquid
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Liquid), RET_VTC, RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Aqueous), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid1
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Liquid1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid2
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Liquid2), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid3
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Liquid3), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Fase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
            End Select
        End Function

        Public Overrides Function DW_CalcEnergiaMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.m_pr.H_PR_MIX("L", T, P, RET_VMOL(Fase.Liquid), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Fase.Liquid))
            HV = Me.m_pr.H_PR_MIX("V", T, P, RET_VMOL(Fase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Fase.Vapor))
            HM = Me.CurrentMaterialStream.Fases(1).SPMProperties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Fases(2).SPMProperties.massfraction.GetValueOrDefault * HV

            Dim ent_massica = HM
            Dim flow = Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow
            Return ent_massica * flow

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            If fase1 = Fase.Liquid Then
                Return Me.AUX_CONDTL(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_CONDTG(T, P)
            End If
        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double
            If fase1 = Fase.Liquid Then
                Return Me.AUX_LIQDENS(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_VAPDENS(T, P)
            ElseIf fase1 = Fase.Mixture Then
                Return Me.CurrentMaterialStream.Fases(1).SPMProperties.volumetric_flow.GetValueOrDefault * Me.AUX_LIQDENS(T) / Me.CurrentMaterialStream.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault + Me.CurrentMaterialStream.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault * Me.AUX_VAPDENS(T, P) / Me.CurrentMaterialStream.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault
            End If
        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_MMM(fase1)
        End Function

        Public Overrides Sub DW_CalcOverallProps()
            MyBase.DW_CalcOverallProps()
        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Fase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case phase
                Case Fase.Vapor
                    state = "V"
                Case Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3, Fase.Aqueous
                    state = "L"
                Case Fase.Solid
                    state = "S"
            End Select

            Select Case phase
                Case PropertyPackages.Fase.Mixture
                    phaseID = 0
                Case PropertyPackages.Fase.Vapor
                    phaseID = 2
                Case PropertyPackages.Fase.Liquid1
                    phaseID = 3
                Case PropertyPackages.Fase.Liquid2
                    phaseID = 4
                Case PropertyPackages.Fase.Liquid3
                    phaseID = 5
                Case PropertyPackages.Fase.Liquid
                    phaseID = 1
                Case PropertyPackages.Fase.Aqueous
                    phaseID = 6
                Case PropertyPackages.Fase.Solid
                    phaseID = 7
            End Select

            Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = Me.AUX_MMM(phase)

            Select Case [property].ToLower
                Case "compressibilityfactor"
                    result = m_pr.Z_PR(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, state)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    resultObj = Me.m_pr.CpCvR(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = resultObj(1)
                Case "heatcapacitycv"
                    resultObj = Me.m_pr.CpCvR(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = resultObj(2)
                Case "enthalpy", "enthalpynf"
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase))
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase))
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase))
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase))
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropyF = result
                Case "viscosity"
                    If state = "L" Then
                        result = Me.AUX_LIQVISCm(T)
                    Else
                        result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                Case "thermalconductivity"
                    If state = "L" Then
                        result = Me.AUX_CONDTL(T)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If state = "L" Then
                        result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                    Else
                        result = Me.AUX_VAPDENS(T, P)
                    End If
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                Case "surfacetension"
                    Me.CurrentMaterialStream.Fases(0).TPMProperties.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal fase As DWSIM.SimulationObjects.PropertyPackages.Fase)

            Dim result As Double
            Dim resultObj As Object
            Dim dwpl As Fase

            Dim T, P As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case fase
                Case PropertyPackages.Fase.Mixture
                    phaseID = 0
                    dwpl = PropertyPackages.Fase.Mixture
                Case PropertyPackages.Fase.Vapor
                    phaseID = 2
                    dwpl = PropertyPackages.Fase.Vapor
                Case PropertyPackages.Fase.Liquid1
                    phaseID = 3
                    dwpl = PropertyPackages.Fase.Liquid1
                Case PropertyPackages.Fase.Liquid2
                    phaseID = 4
                    dwpl = PropertyPackages.Fase.Liquid2
                Case PropertyPackages.Fase.Liquid3
                    phaseID = 5
                    dwpl = PropertyPackages.Fase.Liquid3
                Case PropertyPackages.Fase.Liquid
                    phaseID = 1
                    dwpl = PropertyPackages.Fase.Liquid
                Case PropertyPackages.Fase.Aqueous
                    phaseID = 6
                    dwpl = PropertyPackages.Fase.Aqueous
                Case PropertyPackages.Fase.Solid
                    phaseID = 7
                    dwpl = PropertyPackages.Fase.Solid
            End Select

            If phaseID > 0 Then

                overallmolarflow = Me.CurrentMaterialStream.Fases(0).SPMProperties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molarflow = result
                result = result * Me.AUX_MMM(fase) / 1000
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massflow = result
                result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(fase) / 1000 / Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(fase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                    Dim val As Double
                    val = m_pr.Z_PR(T, P, RET_VMOL(fase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "L")
                    val = 1 / (8.314 * val * T / P)
                    val = val * Me.AUX_MMM(dwpl) / 1000
                    result = val
                Else
                    result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                End If

                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result

                result = Me.m_pr.H_PR_MIX("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, dwpl))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                result = Me.m_pr.S_PR_MIX("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, dwpl))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                result = Me.m_pr.Z_PR(T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "L")
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                resultObj = Me.m_pr.CpCvR("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = resultObj(1)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = resultObj(2)
                result = Me.AUX_MMM(fase)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                result = Me.AUX_CONDTL(T)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                result = Me.AUX_LIQVISCm(T)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value

            ElseIf phaseID = 2 Then

                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                result = Me.m_pr.H_PR_MIX("V", T, P, RET_VMOL(fase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, fase.Vapor))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                result = Me.m_pr.S_PR_MIX("V", T, P, RET_VMOL(fase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, fase.Vapor))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                result = Me.m_pr.Z_PR(T, P, RET_VMOL(fase.Vapor), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "V")
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                result = Me.AUX_CPm(PropertyPackages.Fase.Vapor, T)
                resultObj = Me.m_pr.CpCvR("V", T, P, RET_VMOL(PropertyPackages.Fase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(PropertyPackages.Fase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = resultObj(1)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = resultObj(2)
                result = Me.AUX_MMM(fase)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault, Me.AUX_MMM(fase))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()


            Else

                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(fase) / 1000 / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            Else
                'result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massflow.GetValueOrDefault / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                'Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            End If


        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Fase.Liquid), Me.RET_VPC(Fase.Liquid), Me.RET_VW(Fase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_SURFTM(T)
        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal fase2 As DWSIM.SimulationObjects.PropertyPackages.Fase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Fases(0).TPMProperties.surfaceTension = Me.AUX_SURFTM(T)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            If fase1 = Fase.Liquid Then
                Return Me.AUX_LIQVISCm(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Fase.Vapor))
            End If
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As ClassesBasicasTermodinamica.ConstantProperties) As Boolean

            Return True

        End Function

        Public Function RET_KIJ(ByVal id1 As String, ByVal id2 As String) As Double
            If Me.m_pr.InteractionParameters.ContainsKey(id1.ToLower) Then
                If Me.m_pr.InteractionParameters(id1.ToLower).ContainsKey(id2.ToLower) Then
                    Return m_pr.InteractionParameters(id1.ToLower)(id2.ToLower).kij
                Else
                    If Me.m_pr.InteractionParameters.ContainsKey(id2.ToLower) Then
                        If Me.m_pr.InteractionParameters(id2.ToLower).ContainsKey(id1.ToLower) Then
                            Return m_pr.InteractionParameters(id2.ToLower)(id1.ToLower).kij
                        Else
                            Return 0
                        End If
                    Else
                        Return 0
                    End If
                End If
            Else
                Return 0
            End If
        End Function

        Public Function RET_KIJ2(ByVal id1 As String, ByVal id2 As String) As Double
            If Me.m_pr.InteractionParameters.ContainsKey(id1.ToLower) Then
                If Me.m_pr.InteractionParameters(id1.ToLower).ContainsKey(id2.ToLower) Then
                    Return m_pr.InteractionParameters(id1.ToLower)(id2.ToLower).kji
                Else
                    If Me.m_pr.InteractionParameters.ContainsKey(id2.ToLower) Then
                        If Me.m_pr.InteractionParameters(id2.ToLower).ContainsKey(id1.ToLower) Then
                            Return m_pr.InteractionParameters(id2.ToLower)(id1.ToLower).kji
                        Else
                            Return 0
                        End If
                    Else
                        Return 0
                    End If
                End If
            Else
                Return 0
            End If
        End Function

        Public Function RET_KAPPA1() As Double()

            Dim val(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                If m_pr._data.ContainsKey(cp.Nome.ToLower) Then
                    val(i) = m_pr._data(cp.Nome.ToLower).kappa1
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_KAPPA2() As Double()

            Dim val(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                If m_pr._data.ContainsKey(cp.Nome.ToLower) Then
                    val(i) = m_pr._data(cp.Nome.ToLower).kappa2
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_KAPPA3() As Double()

            Dim val(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                If m_pr._data.ContainsKey(cp.Nome.ToLower) Then
                    val(i) = m_pr._data(cp.Nome.ToLower).kappa3
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Overrides Function RET_VKij() As Double(,)

            Dim val(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1, Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            i = 0
            For Each cp As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                l = 0
                For Each cp2 As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                    val(i, l) = Me.RET_KIJ(cp.Nome, cp2.Nome)
                    l = l + 1
                Next
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_VKij2() As Double(,)

            Dim val(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1, Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            i = 0
            For Each cp As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                l = 0
                For Each cp2 As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                    val(i, l) = Me.RET_KIJ2(cp2.Nome, cp.Nome)
                    l = l + 1
                Next
                i = i + 1
            Next

            Return val

        End Function

#End Region

#Region "    Métodos Numéricos"

        Public Function IntegralSimpsonCp(ByVal a As Double, _
                 ByVal b As Double, _
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

        Public Function IntegralSimpsonCp_T(ByVal a As Double, _
         ByVal b As Double, _
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

            Dim H As Double

            If st = State.Liquid Then
                H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
            Else
                H = Me.m_pr.H_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim H As Double

            If st = State.Liquid Then
                H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            Else
                H = Me.m_pr.H_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim S As Double

            If st = State.Liquid Then
                S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
            Else
                S = Me.m_pr.S_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            Else
                S = Me.m_pr.S_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double
            Select Case fase1
                Case Fase.Liquid
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid1
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid2
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid3
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(fase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
            End Select
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Fase, ByVal T As Double, ByVal P As Double)

            Dim partvol As New Object
            Dim key As String = "0"
            Dim i As Integer = 0

            If Not Me.Parameters.ContainsKey("PP_USE_EOS_LIQDENS") Then Me.Parameters.Add("PP_USE_EOS_LIQDENS", 0)

            Select Case phase
                Case Fase.Liquid
                    key = "1"
                    If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T)))
                        Next
                    End If
                Case Fase.Aqueous
                    key = "6"
                    If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T)))
                        Next
                    End If
                Case Fase.Liquid1
                    key = "3"
                    If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T)))
                        Next
                    End If
                Case Fase.Liquid2
                    key = "4"
                    If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T)))
                        Next
                    End If
                Case Fase.Liquid3
                    key = "5"
                    If CInt(Me.Parameters("PP_USE_EOS_LIQDENS")) = 1 Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T)))
                        Next
                    End If
                Case Fase.Vapor
                    partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "V", 0.01)
                    key = "2"
            End Select

            i = 0
            For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(key).Componentes.Values
                subst.PartialVolume = partvol(i)
                i += 1
            Next

        End Sub

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double
            Dim val As Double
            Dim Z As Double = Me.m_pr.Z_PR(T, P, RET_VMOL(Fase.Vapor), RET_VKij, RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "V")
            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Fase.Vapor)
            Return val
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            DWSIM.App.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            DWSIM.App.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            DWSIM.App.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim lnfug As Object

            If st = State.Liquid Then
                lnfug = m_pr.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, "L")
            Else
                lnfug = m_pr.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, "V")
            End If

            Dim n As Integer = UBound(lnfug)
            Dim i As Integer
            Dim fugcoeff(n) As Double

            For i = 0 To n
                fugcoeff(i) = Exp(lnfug(i))
            Next

            DWSIM.App.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

    End Class

End Namespace

