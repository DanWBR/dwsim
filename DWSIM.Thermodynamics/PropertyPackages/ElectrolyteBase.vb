'    Electrolyte Property Package Base Class 
'    Copyright 2013-2014 Daniel Wagner O. de Medeiros
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
Imports System.Xml.Linq
Imports System.Linq

Imports DWSIM.MathOps.MathEx
Imports DWSIM.MathOps.MathEx.Common
Imports Ciloci.Flee
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable()> Public MustInherit Class ElectrolyteBasePropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Private m_props As New PropertyPackages.Auxiliary.PROPS
        Public m_elec As New PropertyPackages.Auxiliary.Electrolyte
        Private m_id As New PropertyPackages.Auxiliary.Ideal

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Property ReactionSet As String = "DefaultSet"
        Public Property MaxIterations As Integer = 200
        Public Property Tolerance As Double = 0.0000001

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.Electrolytes
            Me.IsElectrolytePP = True

        End Sub

        Public Sub New()

            MyBase.New()

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.Electrolytes
            Me.IsElectrolytePP = True

        End Sub

        ''' <summary>
        ''' Returns the FlashAlgorithm object instance for this property package.
        ''' </summary>
        ''' <value></value>
        ''' <returns>A FlashAlgorithm object to be used in flash calculations.</returns>
        ''' <remarks></remarks>
        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm

            Get

                Return New Auxiliary.FlashAlgorithms.ElectrolyteSVLE With {
                    .CompoundProperties = DW_GetConstantProperties(),
                    .ReactionSet = ReactionSet,
                    .Tolerance = Tolerance,
                    .MaximumIterations = MaxIterations}

            End Get

        End Property


#Region "    DWSIM Functions"

        Public Overrides Function AUX_LIQDENS(T As Double, Optional P As Double = 0.0, Optional Pvp As Double = 0.0, Optional phaseid As Integer = 3, Optional FORCE_EOS As Boolean = False) As Double

            Dim phase As Phase

            Select Case phaseid
                Case 1
                    phase = Phase.Liquid
                Case 3
                    phase = Phase.Liquid1
                Case 4
                    phase = Phase.Liquid2
                Case 5
                    phase = Phase.Liquid3
                Case 6
                    phase = Phase.Aqueous
            End Select

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next

            Return Me.m_elec.LiquidDensity(RET_VMOL(phase), T, constprops)

        End Function

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            If state = PhaseName.Liquid Then
                Dim result = m_elec.LiquidDensity(Vx, T, DW_GetConstantProperties())
                Return 1 / (8.314 * result * 1000 / Me.AUX_MMM(Vx) * T / P)
            Else
                Return 1.0
            End If


        End Function


        Public Function RET_KIJ(ByVal id1 As String, ByVal id2 As String) As Double
            Return 0
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

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Aqueous
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid1
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid2
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid3
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Vapor
                    Return Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
            End Select
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Return 0

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_CONDTL(T)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_CONDTG(T, P)
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

        Public Overrides Sub DW_CalcCompFugCoeff(ByVal f As Phase)

            Dim fc As Object
            Dim vmol As Object = Me.RET_VMOL(f)
            Dim P, T As Double
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            Select Case f
                Case Phase.Vapor
                    fc = Me.DW_CalcFugCoeff(vmol, T, P, State.Vapor)
                Case Else
                    fc = Me.DW_CalcFugCoeff(vmol, T, P, State.Liquid)
            End Select
            Dim i As Integer = 0
            For Each subs As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(f)).Compounds.Values
                subs.FugacityCoeff = fc(i)
                i += 1
            Next

        End Sub

        Public Overrides Sub DW_CalcOverallProps()
            MyBase.DW_CalcOverallProps()
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
        End Function
        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            ElseIf comp.IsHYPO = 1 Then
                Return False
            Else
                Return False
            End If

        End Function

        Function dfidRbb_H(ByVal Rbb, ByVal Kb0, ByVal Vz, ByVal Vu, ByVal sum_Hvi0, ByVal DHv, ByVal DHl, ByVal HT) As Double

            Dim i As Integer = 0
            Dim n = Vz.Length - 1

            Dim Vpbb2(n), L2, V2, Kb2 As Double

            i = 0
            Dim sum_pi2 = 0.0#
            Dim sum_eui_pi2 = 0.0#
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
            Dim sum_pi2 = 0.0#
            Dim sum_eui_pi2 = 0.0#
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

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Aqueous
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid1
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid2
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid3
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Vapor
                    Return Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
            End Select
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
                        subst.PartialVolume = subst.MoleFraction.GetValueOrDefault * 8.314 * T / P
                    Next
            End Select

        End Sub

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double
            Dim val As Double
            Dim Z As Double = 1.0#
            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Phase.Vapor)
            Return val
        End Function

#End Region

#Region "    Auxiliary Functions"

        Function RET_VQ() As Object

            Dim subst As Interfaces.ICompound
            Dim VQ(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VQ(i) = subst.ConstantProperties.UNIQUAC_Q
                i += 1
            Next

            Return VQ

        End Function

        Function RET_VR() As Object

            Dim subst As Interfaces.ICompound
            Dim VR(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                VR(i) = subst.ConstantProperties.UNIQUAC_R
                i += 1
            Next

            Return VR

        End Function

#End Region

#Region "    CalcEquilibrium Override"

        Public Overrides Sub DW_CalcEquilibrium(spec1 As FlashSpec, spec2 As FlashSpec)

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim P, T, H, S, xv, xl, xs, M, W As Double
            Dim result As Dictionary(Of String, Object)
            Dim subst As Interfaces.ICompound
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

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(su.ConstantProperties)
            Next

            Me.m_elec = New Auxiliary.Electrolyte
            Dim ElectrolyteFlash = New Auxiliary.FlashAlgorithms.ElectrolyteSVLE
            ElectrolyteFlash.CompoundProperties = constprops
            ElectrolyteFlash.ReactionSet = ReactionSet
            ElectrolyteFlash.Tolerance = Tolerance
            ElectrolyteFlash.MaximumIterations = MaxIterations
            ElectrolyteFlash.proppack = Me

            Select Case spec1

                Case FlashSpec.T

                    Select Case spec2

                        Case FlashSpec.P

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            result = ElectrolyteFlash.Flash_PT(RET_VMOL(Phase.Mixture), T, P)

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

                            result = ElectrolyteFlash.Flash_PH(RET_VMOL(Phase.Mixture), P, H, T)

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

                            'i = 0
                            'For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                            '    subst.MoleFraction = Vnf(i) / M
                            '    i += 1
                            'Next

                            'For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
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

                            result = ElectrolyteFlash.Flash_PV(RET_VMOL(Phase.Mixture), P, xv, T)

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
                            'For Each subst In Me.CurrentMaterialStream.Phases(7).Compounds.Values
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

#Region "    XML Load/Save Override"

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Dim xel0 As XElement = (From xelv As XElement In data Where xelv.Name = "ElectrolyteFlash_ReactionSetID").SingleOrDefault
            If Not xel0 Is Nothing Then ReactionSet = xel0.Value

            Dim xel2 As XElement = (From xelv As XElement In data Where xelv.Name = "ElectrolyteFlash_Tolerance").SingleOrDefault
            If Not xel2 Is Nothing Then Tolerance = xel2.Value

            Dim xel3 As XElement = (From xelv As XElement In data Where xelv.Name = "ElectrolyteFlash_MaximumIterations").SingleOrDefault
            If Not xel3 Is Nothing Then MaxIterations = xel3.Value

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("ElectrolyteFlash_ReactionSetID", ReactionSet))
                .Add(New XElement("ElectrolyteFlash_Tolerance", Tolerance))
                .Add(New XElement("ElectrolyteFlash_MaximumIterations", MaxIterations))
            End With

            Return elements

        End Function

#End Region

    End Class

End Namespace
