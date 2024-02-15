'    LIQUAC2 Property Package 
'    Copyright 2013-2022 Daniel Wagner O. de Medeiros
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

Imports System.Math
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(LIQUAC2PropertyPackage.ClassId)>
    <System.Serializable()> Public Class LIQUAC2PropertyPackage

        Inherits PropertyPackages.ElectrolyteBasePropertyPackage

        Public Shadows Const ClassId As String = "c7c433ac-382d-460a-9ecc-a9e6d247a227"

        Private m_props As New PropertyPackages.Auxiliary.PROPS
        Public m_liquac As New PropertyPackages.Auxiliary.LIQUAC2
        Private m_id As New PropertyPackages.Auxiliary.Ideal

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

        End Sub

        Public Sub New()

            MyBase.New()

        End Sub

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then

            Else
                Dim f As New FormConfigLIQUAC() With {._form = Flowsheet, ._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigLIQUAC() With {._form = Flowsheet, ._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

#Region "    DWSIM Functions"

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
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3
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
                    result = 0.0#
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    If phase = Phase.Solid Then
                        result = Me.AUX_SOLIDCP(RET_VMAS(phase), constprops, T)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    ElseIf phase = Phase.Vapor Then
                        resultObj = m_id.CpCv(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                    Else
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = Me.m_elec.HeatCapacityCp(T, RET_VMOL(phase), constprops)
                    End If
                Case "heatcapacitycv"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    If phase = Phase.Solid Then
                        result = Me.AUX_SOLIDCP(RET_VMAS(phase), constprops, T)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    Else
                        resultObj = m_id.CpCv(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = Me.m_elec.HeatCapacityCp(T, RET_VMOL(phase), constprops)
                    End If
                Case "enthalpy", "enthalpynf"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    If phase = Phase.Solid Then
                        result = Me.m_elec.SolidEnthalpy(T, RET_VMOL(phase), constprops)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                        result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    ElseIf phase = Phase.Vapor Then
                        result = Me.m_elec.LiquidEnthalpy(T, RET_VMOL(phase), constprops, Me.m_liquac.GAMMA_MR(T + 0.1, RET_VMOL(phase), constprops), Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), False)
                        result += Me.RET_HVAPM(RET_VMAS(phase), T)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                        result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    Else
                        result = Me.m_elec.LiquidEnthalpy(T, RET_VMOL(phase), constprops, Me.m_liquac.GAMMA_MR(T + 0.1, RET_VMOL(phase), constprops), Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), False)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    End If
                Case "entropy", "entropynf"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    If phase = Phase.Solid Then
                        result = 0.0#
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                        result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    ElseIf phase = Phase.Vapor Then
                        result = m_id.S_RA_MIX(state, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase), Me.RET_VHVAP(T))
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                        result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    Else
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = 0.0#
                    End If
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
                Case "osmoticcoefficient"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.osmoticCoefficient = Me.m_elec.OsmoticCoeff(RET_VMOL(phase), Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), constprops)
                Case "freezingpoint"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.freezingPoint = Me.m_elec.FreezingPointDepression(RET_VMOL(phase), Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), constprops)(0)
                Case "freezingpointdepression"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.freezingPointDepression = Me.m_elec.FreezingPointDepression(RET_VMOL(phase), Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), constprops)(1)
                Case "ph"
                    Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                    For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        constprops.Add(su.ConstantProperties)
                    Next
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.pH = Me.m_elec.pH(RET_VMOL(phase), T, Me.m_liquac.GAMMA_MR(T, RET_VMOL(phase), constprops), constprops)
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
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)
            End If

            If phaseID = 3 Then

                Me.DW_CalcProp("osmoticcoefficient", PropertyPackages.Phase.Liquid1)
                Me.DW_CalcProp("freezingpoint", PropertyPackages.Phase.Liquid1)
                Me.DW_CalcProp("freezingpointdepression", PropertyPackages.Phase.Liquid1)
                Me.DW_CalcProp("ph", PropertyPackages.Phase.Liquid1)

            End If

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 1 / (8.314 * result * 1000 / Me.AUX_MMM(Phase) * T / P)
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.m_elec.HeatCapacityCp(T, RET_VMOL(dwpl), constprops)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
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
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = 1
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = Me.AUX_CPm(PropertyPackages.Phase.Vapor, T)
                resultObj = Me.m_id.CpCv("V", T, P, RET_VMOL(PropertyPackages.Phase.Vapor), RET_VKij(), RET_VMAS(PropertyPackages.Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
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

            ElseIf phaseID = 7 Then

                result = Me.AUX_SOLIDDENS
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = 1
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 0.0# 'result
                result = Me.AUX_SOLIDCP(RET_VMAS(Phase), constprops, T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = 0.0# 'result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = 0.0# 'result
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = 0.0# 'result
                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(Phase))
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = 0.0# 'result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = 0.0# 'result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

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

        Public Overrides Sub DW_CalcSolidPhaseProps()

            Dim result As Double
            Dim dwpl As Phase

            Dim T, P As Double
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            phaseID = 7
            dwpl = PropertyPackages.Phase.Solid

            If phaseID > 0 Then
                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase.Solid) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase.Solid) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase.Solid)
            End If

            result = Me.AUX_SOLIDDENS
            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
            result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
            result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
            result = 1
            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = 0.0# 'result
            result = Me.AUX_SOLIDCP(RET_VMAS(Phase.Solid), DW_GetConstantProperties, T)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
            result = Me.AUX_MMM(Phase.Solid)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = 0.0# 'result
            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = 0.0# 'result
            result = Me.AUX_CONDTG(T, P)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = 0.0# 'result
            result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(Phase.Solid))
            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = 0.0# 'result
            Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = 0.0# 'result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

        End Sub

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim H As Double

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next

            Select Case st
                Case State.Liquid
                    H = Me.RET_Hid(298.15, T, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - Me.m_elec.LiquidEnthalpy(T, Vx, constprops, Me.m_liquac.GAMMA_MR(T + 0.1, Vx, constprops), Me.m_liquac.GAMMA_MR(T, Vx, constprops), False)
                Case State.Solid
                    H = Me.RET_Hid(298.15, T, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - Me.m_elec.LiquidEnthalpy(T, Vx, constprops, Me.m_liquac.GAMMA_MR(T + 0.1, Vx, constprops), Me.m_liquac.GAMMA_MR(T, Vx, constprops), False) - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - Me.m_elec.SolidEnthalpy(T, Vx, constprops)
                Case State.Vapor
                    H = Me.RET_Hid(298.15, T, Vx)
            End Select

            Return H

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Select Case st
                Case State.Liquid
                    Return Me.RET_Sid(298.15, T, P, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                Case State.Solid
                    Return Me.RET_Sid(298.15, T, P, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                Case State.Vapor
                    Return Me.RET_Sid(298.15, T, P, Vx)
            End Select

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Calculator.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            Calculator.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            Calculator.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim n As Integer = Vx.Length - 1
            Dim lnfug(n), ativ(n) As Double
            Dim fugcoeff(n) As Double
            Dim i As Integer

            Dim Tc As Double() = Me.RET_VTC()

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each s As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(s.ConstantProperties)
            Next

            If st = State.Liquid Then

                Dim wtotal As Double = 0
                Dim mtotal As Double = 0
                Dim molality(n) As Double

                i = 0
                Do
                    If constprops(i).Name = "Water" Or constprops(i).Name = "Monoethanolamine" Or constprops(i).Name = "Diethanolamine" Then
                        wtotal += Vx(i) * constprops(i).Molar_Weight / 1000
                    End If
                    mtotal += Vx(i)
                    i += 1
                Loop Until i = n + 1

                Dim Xsolv As Double = 1

                i = 0
                Do
                    molality(i) = Vx(i) / wtotal
                    i += 1
                Loop Until i = n + 1

                ativ = Me.m_liquac.GAMMA_MR(T, Vx, constprops)

                For i = 0 To n
                    If constprops(i).IsIon Then
                        fugcoeff(i) = molality(i) * ativ(i)
                    ElseIf constprops(i).IsSalt Then
                        fugcoeff(i) = molality(i) * ativ(i)
                    Else
                        If T / Tc(i) >= 1 Then
                            If constprops(i).Name = "Hydrogen sulfide" Then
                                fugcoeff(i) = Exp(358.138 - 13236.8 / T - 55.0551 * Log(T) + 0.059565 * T) / P
                            ElseIf constprops(i).Name = "Carbon dioxide" Then
                                fugcoeff(i) = Exp(170.7126 - 8477.711 / T - 21.95743 * Log(T) + 0.005781 * T) / P
                            Else
                                fugcoeff(i) = AUX_KHenry(Me.RET_VNAMES(i), T) / P
                            End If
                        Else
                            fugcoeff(i) = ativ(i) * Me.AUX_PVAPi(i, T) / (P)
                        End If
                    End If
                Next
            ElseIf st = State.Vapor Then
                For i = 0 To n
                    If constprops(i).IsIon Then
                        fugcoeff(i) = 10000000000.0
                    ElseIf constprops(i).IsSalt Then
                        fugcoeff(i) = 10000000000.0
                    Else
                        fugcoeff(i) = 1.0#
                    End If
                Next
            ElseIf st = State.Solid Then
                For i = 0 To n
                    If constprops(i).TemperatureOfFusion <> 0 Then
                        fugcoeff(i) = Exp(-constprops(i).EnthalpyOfFusionAtTf / (0.00831447 * T) * (1 - T / constprops(i).TemperatureOfFusion))
                    Else
                        fugcoeff(i) = 1.0#
                    End If
                Next
            End If
            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

        Public Overrides Function DW_CalcKvalue(Vx As Double(), Vy As Double(), T As Double, P As Double, Optional type As String = "LV") As Double()

            Dim val0 As Double() = MyBase.DW_CalcKvalue(Vx, Vy, T, P, type)

            Dim cprops = Me.DW_GetConstantProperties

            Dim i As Integer = 0
            For Each cp In cprops
                If cp.IsIon Or cp.IsSalt Or cp.IsHydratedSalt Then val0(i) = 0.0000000001
                i += 1
            Next

            Return val0

        End Function

        Public Overrides Function AUX_PVAPi(ByVal sub1 As String, ByVal T As Double) As Double

            Dim cprops = Me.DW_GetConstantProperties.Where(Function(x) x.Name = sub1).FirstOrDefault

            If cprops.IsIon Or cprops.IsSalt Then
                Return 0.0001
            Else
                Return MyBase.AUX_PVAPi(sub1, T)
            End If

        End Function

#End Region

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

    End Class

End Namespace
