'    Reaktoro Property Package
'    Copyright 2020-2024 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Thermodynamics
Imports DWSIM.ExtensionMethods
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports System.Windows.Forms

<System.Runtime.InteropServices.Guid(ReaktoroPropertyPackage.ClassId)>
<System.Serializable()> Public Class ReaktoroPropertyPackage

    Inherits ElectrolyteBasePropertyPackage

    Public Shadows Const ClassId As String = "c7c54cac-387d-460a-9ecc-a9e6d247a227"

    Private m_props As New Auxiliary.PROPS
    Private m_id As New Auxiliary.Ideal

    Public Sub New(ByVal comode As Boolean)

        MyBase.New(comode)

    End Sub

    Public Sub New()

        ComponentName = "Reaktoro (Aqueous Electrolytes)"
        ComponentDescription = "Reaktoro is a computational framework developed in C++ and Python that implements numerical methods for modeling chemically reactive processes governed by either chemical equilibrium, chemical kinetics, or a combination of both."

        IsConfigurable = True

    End Sub

    Public Overrides Function ReturnInstance(typename As String) As Object

        Return New ReaktoroPropertyPackage()

    End Function

    Public Overrides Sub DisplayEditingForm()

        Dim f As New FormConfig
        f.Show()

    End Sub

    Public Overrides Function GetEditingForm() As Form

        Dim f As New FormConfig
        Return f

    End Function

#Region "    DWSIM Functions"

    Private ReaktoroFlashInstance As ReaktoroFlash

    Public Overrides ReadOnly Property FlashBase As FlashAlgorithm
        Get
            If ReaktoroFlashInstance Is Nothing Then
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                ReaktoroFlashInstance = New ReaktoroFlash With {.proppack = Me, .CompoundProperties = constprops}
            End If
            Return ReaktoroFlashInstance
        End Get
    End Property

#Region "    CalcEquilibrium Override"

    Public Overrides Sub DW_CalcEquilibrium(spec1 As FlashSpec, spec2 As FlashSpec)

        Me.CurrentMaterialStream.AtEquilibrium = False

        Dim P, T, H, S, xv, xl, xs, M, W As Double
        Dim result As Dictionary(Of String, Object)
        Dim subst As ICompound
        Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count
        Dim i As Integer = 0

        'for TVF/PVF/PH/PS flashes
        xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
        H = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
        S = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault

        Me.DW_ZerarPhaseProps(Phase.Vapor)
        Me.DW_ZerarPhaseProps(Phase.Liquid)
        Me.DW_ZerarPhaseProps(Phase.Liquid1)
        Me.DW_ZerarPhaseProps(Phase.Liquid2)
        Me.DW_ZerarPhaseProps(Phase.Liquid3)
        Me.DW_ZerarPhaseProps(Phase.Aqueous)
        Me.DW_ZerarPhaseProps(Phase.Solid)
        Me.DW_ZerarComposicoes(Phase.Vapor)
        Me.DW_ZerarComposicoes(Phase.Liquid)
        Me.DW_ZerarComposicoes(Phase.Liquid1)
        Me.DW_ZerarComposicoes(Phase.Liquid2)
        Me.DW_ZerarComposicoes(Phase.Liquid3)
        Me.DW_ZerarComposicoes(Phase.Aqueous)
        Me.DW_ZerarComposicoes(Phase.Solid)

        Dim constprops As New List(Of ICompoundConstantProperties)
        For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
            constprops.Add(su.ConstantProperties)
        Next

        Me.m_elec = New Auxiliary.Electrolyte
        If Me.ReaktoroFlashInstance Is Nothing Then Me.ReaktoroFlashInstance = New ReaktoroFlash
        Me.ReaktoroFlashInstance.CompoundProperties = constprops
        Me.ReaktoroFlashInstance.proppack = Me

        Select Case spec1

            Case FlashSpec.T

                Select Case spec2

                    Case FlashSpec.P

                        T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                        P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                        result = Me.ReaktoroFlashInstance.Flash_PT(RET_VMOL(Phase.Mixture), T, P)

                        xl = result("LiquidPhaseMoleFraction")
                        xv = result("VaporPhaseMoleFraction")
                        xs = result("SolidPhaseMoleFraction")

                        Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                        Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                        Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                        M = result("MoleSum")
                        W = Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault

                        Dim Vnf = result("MixtureMoleFlows")

                        Dim MW = Me.AUX_MMM(Vnf)

                        Me.CurrentMaterialStream.Phases(0).Properties.molarflow = M * W / MW * 1000

                        'i = 0
                        'For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        '    subst.MoleFraction = Vnf(i) / M
                        '    i += 1
                        'Next
                        'For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        '    subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 0)
                        'Next

                        Dim Vx = result("LiquidPhaseMolarComposition")
                        Dim Vy = result("VaporPhaseMolarComposition")
                        Dim Vs = result("SolidPhaseMolarComposition")

                        Dim ACL = result("LiquidPhaseActivityCoefficients")

                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MoleFraction = Vx(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = ACL(i)
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MoleFraction = Vs(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MoleFraction = Vy(i)
                            subst.FugacityCoeff = 1.0#
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                        Next

                        Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(4).Properties.massfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))

                        Dim HM, HV, HL, HS As Double

                        If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                        If xs <> 0 Then HS = Me.DW_CalcEnthalpy(Vs, T, P, State.Solid)
                        If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                        HM = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV

                        H = HM

                        Dim SM, SV, SL, SS As Double

                        If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                        If xs <> 0 Then SS = Me.DW_CalcEntropy(Vs, T, P, State.Solid)
                        If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                        SM = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV

                        S = SM

                    Case FlashSpec.H

                        Throw New Exception(Calculator.GetLocalString("PropPack_FlashTHNotSupported"))

                    Case FlashSpec.S

                        Throw New Exception(Calculator.GetLocalString("PropPack_FlashTSNotSupported"))

                    Case FlashSpec.VAP

                        Throw New Exception(Calculator.GetLocalString("PropPack_FlashTVNotSupported"))

                End Select

            Case FlashSpec.P

                Select Case spec2

                    Case FlashSpec.H

                        T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                        H = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
                        P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                        result = Me.ReaktoroFlashInstance.Flash_PH(RET_VMOL(Phase.Mixture), P, H, T)

                        T = result("Temperature")

                        xl = result("LiquidPhaseMoleFraction")
                        xv = result("VaporPhaseMoleFraction")
                        xs = result("SolidPhaseMoleFraction")

                        Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                        Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                        Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                        M = result("MoleSum")
                        W = Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault

                        Dim Vnf = result("MixtureMoleFlows")

                        Dim MW = Me.AUX_MMM(Vnf)

                        Me.CurrentMaterialStream.Phases(0).Properties.molarflow = M * W / MW * 1000

                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                            subst.MoleFraction = Vnf(i) / M
                            i += 1
                        Next

                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 0)
                        Next

                        Dim Vx = result("LiquidPhaseMolarComposition")
                        Dim Vy = result("VaporPhaseMolarComposition")
                        Dim Vs = result("SolidPhaseMolarComposition")

                        Dim ACL = result("LiquidPhaseActivityCoefficients")

                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MoleFraction = Vx(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = ACL(i)
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MoleFraction = Vs(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MoleFraction = Vy(i)
                            subst.FugacityCoeff = 1.0#
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                        Next

                        Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(4).Properties.massfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))

                        Dim SM, SV, SL, SS As Double

                        If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                        If xs <> 0 Then SS = Me.DW_CalcEntropy(Vs, T, P, State.Solid)
                        If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                        SM = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV

                        S = SM

                    Case FlashSpec.S

                        Throw New Exception(Calculator.GetLocalString("PropPack_FlashPSNotSupported"))

                    Case FlashSpec.VAP

                        T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                        P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                        result = Me.ReaktoroFlashInstance.Flash_PV(RET_VMOL(Phase.Mixture), P, xv, T)

                        T = result("Temperature")

                        xl = result("LiquidPhaseMoleFraction")
                        xv = result("VaporPhaseMoleFraction")
                        xs = result("SolidPhaseMoleFraction")

                        Me.CurrentMaterialStream.Phases(3).Properties.molarfraction = xl
                        Me.CurrentMaterialStream.Phases(4).Properties.molarfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(2).Properties.molarfraction = xv
                        Me.CurrentMaterialStream.Phases(7).Properties.molarfraction = xs

                        M = result("MoleSum")
                        W = Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault

                        'Dim Vnf = result("MixtureMoleFlows")

                        'MW = Me.AUX_MMM(Vnf)

                        Me.CurrentMaterialStream.Phases(0).Properties.molarflow *= M '* W / MW * 1000

                        'i = 0
                        'For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                        '    subst.MoleFraction = Vnf(i) / M
                        '    i += 1
                        'Next
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 0)
                        Next

                        Dim Vx = result("LiquidPhaseMolarComposition")
                        Dim Vy = result("VaporPhaseMolarComposition")
                        Dim Vs = result("SolidPhaseMolarComposition")

                        Dim ACL = result("LiquidPhaseActivityCoefficients")

                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MoleFraction = Vx(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = ACL(i)
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(3).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 3)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MoleFraction = Vs(i)
                            subst.FugacityCoeff = 0
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 7)
                        Next
                        i = 0
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MoleFraction = Vy(i)
                            subst.FugacityCoeff = 1.0#
                            subst.ActivityCoeff = 0
                            subst.PartialVolume = 0
                            subst.PartialPressure = 0
                            i += 1
                        Next
                        For Each subst In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.MassFraction = Me.AUX_CONVERT_MOL_TO_MASS(subst.Name, 2)
                        Next

                        Me.CurrentMaterialStream.Phases(3).Properties.massfraction = xl * Me.AUX_MMM(Phase.Liquid1) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(4).Properties.massfraction = 0.0#
                        Me.CurrentMaterialStream.Phases(7).Properties.massfraction = xs * Me.AUX_MMM(Phase.Solid) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))
                        Me.CurrentMaterialStream.Phases(2).Properties.massfraction = xv * Me.AUX_MMM(Phase.Vapor) / (xl * Me.AUX_MMM(Phase.Liquid1) + xs * Me.AUX_MMM(Phase.Solid) + xv * Me.AUX_MMM(Phase.Vapor))

                        Dim SM, SV, SL, SS As Double

                        If xl <> 0 Then SL = Me.DW_CalcEntropy(Vx, T, P, State.Liquid)
                        If xs <> 0 Then SS = Me.DW_CalcEntropy(Vs, T, P, State.Solid)
                        If xv <> 0 Then SV = Me.DW_CalcEntropy(Vy, T, P, State.Vapor)
                        SM = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * SL + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * SS + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * SV

                        S = SM

                        Dim HM, HV, HL, HS As Double

                        If xl <> 0 Then HL = Me.DW_CalcEnthalpy(Vx, T, P, State.Liquid)
                        If xs <> 0 Then HS = Me.DW_CalcEnthalpy(Vs, T, P, State.Solid)
                        If xv <> 0 Then HV = Me.DW_CalcEnthalpy(Vy, T, P, State.Vapor)
                        HM = Me.CurrentMaterialStream.Phases(3).Properties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Phases(7).Properties.massfraction.GetValueOrDefault * HS + Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault * HV

                        H = HM

                End Select

        End Select

        Dim summf As Double = 0, sumwf As Double = 0
        For Each pi As PhaseInfo In Me.PhaseMappings.Values
            If Not pi.PhaseLabel = "Disabled" Then
                summf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault
                sumwf += Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction.GetValueOrDefault
            End If
        Next
        If Abs(summf - 1) > 0.000001 Then
            For Each pi As PhaseInfo In Me.PhaseMappings.Values
                If Not pi.PhaseLabel = "Disabled" Then
                    If Not Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.molarfraction = 1 - summf
                        Me.CurrentMaterialStream.Phases(pi.DWPhaseIndex).Properties.massfraction = 1 - sumwf
                    End If
                End If
            Next
        End If

        With Me.CurrentMaterialStream

            .Phases(0).Properties.temperature = T
            .Phases(0).Properties.pressure = P
            .Phases(0).Properties.enthalpy = H
            .Phases(0).Properties.entropy = S

        End With

        Me.CurrentMaterialStream.AtEquilibrium = True


    End Sub

#End Region


    Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

        Dim result As Double = 0.0#
        Dim resultObj As Object = Nothing
        Dim phaseID As Integer = -1
        Dim state As String = ""

        Dim actcalc As New ActivityCoefficients

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
            Case Phase.Mixture
                phaseID = 0
            Case Phase.Vapor
                phaseID = 2
            Case Phase.Liquid1
                phaseID = 3
            Case Phase.Liquid2
                phaseID = 4
            Case Phase.Liquid3
                phaseID = 5
            Case Phase.Liquid
                phaseID = 1
            Case Phase.Aqueous
                phaseID = 6
            Case Phase.Solid
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
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
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
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
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
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                If phase = Phase.Solid Then
                    result = Me.m_elec.SolidEnthalpy(T, RET_VMOL(phase), constprops)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                ElseIf phase = Phase.Vapor Then
                    result = Me.m_elec.LiquidEnthalpy(T, RET_VMOL(phase), constprops, actcalc.Calculate(RET_VMOL(phase), T + 0.1, P, Me), actcalc.Calculate(RET_VMOL(phase), T, P, Me), False)
                    result += Me.RET_HVAPM(RET_VMAS(phase), T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Else
                    result = Me.m_elec.LiquidEnthalpy(T, RET_VMOL(phase), constprops, actcalc.Calculate(RET_VMOL(phase), T + 0.1, P, Me), actcalc.Calculate(RET_VMOL(phase), T, P, Me), False)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                End If
            Case "entropy", "entropynf"
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
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
                    result = Me.AUX_LIQVISCm(T, P)
                Else
                    result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(phase))
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
            Case "thermalconductivity"
                If state = "L" Then
                    result = Me.AUX_CONDTL(T)
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
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                Me.CurrentMaterialStream.Phases(phaseID).Properties.osmoticCoefficient = Me.m_elec.OsmoticCoeff(RET_VMOL(phase), actcalc.Calculate(RET_VMOL(phase), T, P, Me), constprops)
            Case "miac"
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                Me.CurrentMaterialStream.Phases(phaseID).Properties.mean_ionic_acitivty_coefficient = Me.m_elec.MIAC(RET_VMOL(phase), RET_VMOL(Phase.Solid), T, constprops)
            Case "freezingpoint"
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                Me.CurrentMaterialStream.Phases(phaseID).Properties.freezingPoint = Me.m_elec.FreezingPointDepression(RET_VMOL(phase), actcalc.Calculate(RET_VMOL(phase), T, P, Me), constprops)(0)
            Case "freezingpointdepression"
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                Me.CurrentMaterialStream.Phases(phaseID).Properties.freezingPointDepression = Me.m_elec.FreezingPointDepression(RET_VMOL(phase), actcalc.Calculate(RET_VMOL(phase), T, P, Me), constprops)(1)
            Case "ph"
                Dim constprops As New List(Of ICompoundConstantProperties)
                For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                Me.CurrentMaterialStream.Phases(phaseID).Properties.pH = Me.m_elec.pH(RET_VMOL(phase), T, actcalc.Calculate(RET_VMOL(phase), T, P, Me), constprops)
            Case Else
                Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
        End Select

    End Sub

    Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As Phase)

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
            Case Phase.Mixture
                phaseID = 0
                dwpl = Phase.Mixture
            Case Phase.Vapor
                phaseID = 2
                dwpl = Phase.Vapor
            Case Phase.Liquid1
                phaseID = 3
                dwpl = Phase.Liquid1
            Case Phase.Liquid2
                phaseID = 4
                dwpl = Phase.Liquid2
            Case Phase.Liquid3
                phaseID = 5
                dwpl = Phase.Liquid3
            Case Phase.Liquid
                phaseID = 1
                dwpl = Phase.Liquid
            Case Phase.Aqueous
                phaseID = 6
                dwpl = Phase.Aqueous
            Case Phase.Solid
                phaseID = 7
                dwpl = Phase.Solid
        End Select

        If phaseID > 0 Then
            overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
            phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
            result = overallmolarflow * phasemolarfrac
            Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
            result = result * Me.AUX_MMM(Phase) / 1000
            Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
            result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(0).Properties.massflow.GetValueOrDefault
            Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
            Me.DW_CalcCompVolFlow(phaseID)
            Me.DW_CalcCompFugCoeff(Phase)
        End If

        If phaseID = 3 Then

            Me.DW_CalcProp("osmoticcoefficient", Phase.Liquid1)
            Me.DW_CalcProp("freezingpoint", Phase.Liquid1)
            Me.DW_CalcProp("freezingpointdepression", Phase.Liquid1)
            Me.DW_CalcProp("ph", Phase.Liquid1)
            Me.DW_CalcProp("miac", Phase.Liquid1)

        End If

        Dim constprops As New List(Of ICompoundConstantProperties)
        For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
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
            result = Me.AUX_CONDTL(T)
            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
            result = Me.AUX_LIQVISCm(T, P)
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
            result = Me.AUX_CPm(Phase.Vapor, T)
            resultObj = Me.m_id.CpCv("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VMAS(Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
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
        dwpl = Phase.Solid

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

        Dim constprops As New List(Of ICompoundConstantProperties)
        For Each su As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
            constprops.Add(su.ConstantProperties)
        Next

        Dim actcalc As New ActivityCoefficients

        Select Case st
            Case State.Liquid
                H = Me.RET_Hid(298.15, T, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - Me.m_elec.LiquidEnthalpy(T, Vx, constprops, actcalc.Calculate(Vx, T + 0.1, P, Me), actcalc.Calculate(Vx, T, P, Me), False)
            Case State.Solid
                H = Me.CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties())
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
                Return Me.CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties()) / T
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

        Dim constprops As New List(Of ICompoundConstantProperties)
        For Each s As ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
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

            ativ = New ActivityCoefficients().Calculate(Vx, T, P, Me)

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

    Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

        Return 1.0

    End Function

End Class

