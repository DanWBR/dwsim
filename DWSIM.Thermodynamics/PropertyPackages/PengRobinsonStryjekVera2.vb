'    Peng-Robinson-Stryjek-Vera 2 w/ Margules Mixing Rules Property Package 
'    Copyright 2012-2024 Daniel Wagner O. de Medeiros
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
Imports System.Math
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable()> Public Class PRSV2PropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Private m_props As New PropertyPackages.Auxiliary.PROPS
        Public m_pr As New PropertyPackages.Auxiliary.PRSV2
        Private m_id As New PropertyPackages.Auxiliary.Ideal

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Overrides ReadOnly Property DisplayName As String = "Peng-Robinson-Strÿjek-Vera 2 (Margules Mixing Rule)"

        Public Overrides ReadOnly Property DisplayDescription As String = "Modified version of the PR EOS that improves predictions for slightly polar mixtures."

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            MyBase.New()

            'With Me.Parameters
            '    .Add("PP_USE_EOS_LIQDENS", 0)
            'End With

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.EOS

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Peng-Robinson-Strÿjek-Vera EOS"
                .Vapor_Enthalpy_Entropy_CpCv = "Peng-Robinson-Strÿjek-Vera EOS"
                .Vapor_Density = "Peng-Robinson-Strÿjek-Vera EOS"
                .Liquid_Fugacity = "Peng-Robinson-Strÿjek-Vera EOS"
                .Liquid_Enthalpy_Entropy_CpCv = "Peng-Robinson-Strÿjek-Vera EOS"
                .Liquid_Density = "Peng-Robinson-Strÿjek-Vera EOS / Experimental / Rackett / COSTALD"
            End With

        End Sub

        Public Overrides Function GetModel() As Object
            Return m_pr
        End Function

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfigPRSV2() With {._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfigPRSV2() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Function GetEditingForm() As System.Windows.Forms.Form

            Return New FormConfigPRSV2() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

        Public Overrides Sub ConfigParameters()

        End Sub

#Region "    DWSIM Functions"

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Liquid), RET_VTC, RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                    Else
                        Return AUX_CPm(Phase1, T)
                    End If
                Case Phase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Aqueous), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid1
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Liquid1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                    Else
                        Return AUX_CPm(Phase1, T)
                    End If
                Case Phase.Liquid2
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Liquid2), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                    Else
                        Return AUX_CPm(Phase1, T)
                    End If
                Case Phase.Liquid3
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Liquid3), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                    Else
                        Return AUX_CPm(Phase1, T)
                    End If
                Case Phase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
            End Select
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.m_pr.H_PR_MIX("L", T, P, RET_VMOL(Phase.Liquid), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Liquid))
            HV = Me.m_pr.H_PR_MIX("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Vapor))
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
            Else
                Return 0.0#
            End If
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

            Select Case [property].ToLower
                Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    CalcAdditionalPhaseProperties(phaseID)
                Case "compressibilityfactor"
                    result = m_pr.Z_PR(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, state)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    resultObj = Me.m_pr.CpCvR(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                Case "heatcapacitycv"
                    resultObj = Me.m_pr.CpCvR(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
                Case "enthalpy", "enthalpynf"
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.m_pr.H_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.m_pr.S_PR_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase))
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
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)

                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                result = Me.m_pr.H_PR_MIX("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, dwpl))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.m_pr.S_PR_MIX("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, dwpl))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.m_pr.Z_PR(T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "L")
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    resultObj = Me.m_pr.CpCvR("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
                Else
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = AUX_CPm(dwpl, T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = AUX_CPm(dwpl, T)
                End If
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                result = Me.AUX_CONDTL(T, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                result = Me.AUX_LIQVISCm(T, P, phaseID)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                result = Me.m_pr.H_PR_MIX("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, Phase.Vapor))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.m_pr.S_PR_MIX("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, Phase.Vapor))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.m_pr.Z_PR(T, P, RET_VMOL(Phase.Vapor), RET_VKij, RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "V")
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = Me.AUX_CPm(PropertyPackages.Phase.Vapor, T)
                resultObj = Me.m_pr.CpCvR("V", T, P, RET_VMOL(PropertyPackages.Phase.Vapor), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(PropertyPackages.Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()


            Else

                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            Else
                'result = Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow.GetValueOrDefault / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                'Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If


        End Sub

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Phase.Liquid), Me.RET_VPC(Phase.Liquid), Me.RET_VW(Phase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_SURFTM(T)
        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T, P)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            Else
                Return 0.0#
            End If
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

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

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If m_pr._data.ContainsKey(cp.Name.ToLower) Then
                    val(i) = m_pr._data(cp.Name.ToLower).kappa1
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_KAPPA2() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If m_pr._data.ContainsKey(cp.Name.ToLower) Then
                    val(i) = m_pr._data(cp.Name.ToLower).kappa2
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_KAPPA3() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            i = 0
            For Each cp As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If m_pr._data.ContainsKey(cp.Name.ToLower) Then
                    val(i) = m_pr._data(cp.Name.ToLower).kappa3
                Else
                    val(i) = 0.0#
                End If
                i = i + 1
            Next

            Return val

        End Function

        Public Overrides Function RET_VKij() As Double(,)

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            i = 0
            For Each cp As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                l = 0
                For Each cp2 As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    val(i, l) = Me.RET_KIJ(cp.Name, cp2.Name)
                    l = l + 1
                Next
                i = i + 1
            Next

            Return val

        End Function

        Public Function RET_VKij2() As Double(,)

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            i = 0
            For Each cp As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                l = 0
                For Each cp2 As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    val(i, l) = Me.RET_KIJ2(cp2.Name, cp.Name)
                    l = l + 1
                Next
                i = i + 1
            Next

            Return val

        End Function

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

            Dim H As Double

            If st = State.Liquid Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                Else
                    H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                End If
            ElseIf st = State.Vapor Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    H = Me.m_pr.H_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                Else
                    H = RET_Hid_FromLiqCp(Vx, T, P)
                End If
            ElseIf st = State.Solid Then
                If SolidPhaseEnthalpy_UsesCp Then
                    H = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties)
                Else
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx)) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    Else
                        H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    End If
                End If
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim H As Double

            If st = State.Liquid Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                Else
                    H = Me.m_id.H_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                End If
            ElseIf st = State.Vapor Then
                H = Me.m_pr.H_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Solid Then
                H = Me.m_pr.H_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim S As Double

            If st = State.Liquid Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                Else
                    S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                End If
            ElseIf st = State.Vapor Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    S = Me.m_pr.S_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                Else
                    S = RET_Sid_FromLiqCp(Vx, T, P)
                End If
            ElseIf st = State.Solid Then
                If SolidPhaseEnthalpy_UsesCp Then
                    S = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties) / T
                Else
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx)) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                    Else
                        S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                    End If
                End If
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                    S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                Else
                    S = Me.m_id.S_RA_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0, Me.RET_VHVAP(T)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                End If
            ElseIf st = State.Vapor Then
                S = Me.m_pr.S_PR_MIX("V", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Solid Then
                S = Me.m_pr.S_PR_MIX("L", T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, RET_VMM, 0) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid1
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid2
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid3
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
            End Select
            Return 0.0#
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

            Dim partvol As New Object
            Dim key As String = "0"
            Dim i As Integer = 0

            Select Case phase
                Case Phase.Liquid
                    key = "1"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Aqueous
                    key = "6"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid1
                    key = "3"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid2
                    key = "4"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid3
                    key = "5"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Vapor
                    partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "V", 0.01)
                    key = "2"
                Case PropertyPackages.Phase.Solid
                    partvol = RET_NullVector()
            End Select

            i = 0
            For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                subst.PartialVolume = partvol(i)
                i += 1
            Next

        End Sub

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double
            Dim val As Double
            Dim Z As Double = Me.m_pr.Z_PR(T, P, RET_VMOL(Phase.Vapor), RET_VKij, RET_VKij2(), RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "V")
            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Phase.Vapor)
            Return val
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

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

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

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

            Dim val As Double
            If state = PhaseName.Liquid Then
                val = m_pr.Z_PR(T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "L")
            Else
                val = m_pr.Z_PR(T, P, Vx, RET_VKij(), RET_VKij2, RET_KAPPA1, RET_KAPPA2, RET_KAPPA3, RET_VTC, RET_VPC, RET_VW, "V")
            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Compressibility Factor: {0}", val))

            IObj?.Close()

            Return val

        End Function

    End Class

End Namespace

