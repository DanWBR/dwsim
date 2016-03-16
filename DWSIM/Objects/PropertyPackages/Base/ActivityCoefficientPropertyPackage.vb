'    Activity Coefficient Property Package Base Class
'    Copyright 2008-2015 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DWSIM.SimulationObjects.Streams
Imports DWSIM.DWSIM.SimulationObjects
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports System.Math
Imports CapeOpen
Imports System.Runtime.InteropServices.ComTypes
Imports iop = System.Runtime.InteropServices
Imports DWSIM.Interfaces2
Imports System.Xml.Serialization
Imports System.Runtime.Serialization.Formatters
Imports System.Threading.Tasks
Imports DWSIM.DWSIM.MathEx

Namespace DWSIM.SimulationObjects.PropertyPackages

    <System.Serializable> Public Class ActivityCoefficientPropertyPackage

        Inherits PropertyPackage

        Friend m_pr As New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.PengRobinson
        Friend m_lk As New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LeeKesler

        Friend m_act As DWSIM.SimulationObjects.PropertyPackages.Auxiliary.IActivityCoefficientBase

#Region "Initialization"

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
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

        Public Overrides Function SupportsComponent(ByVal comp As ClassesBasicasTermodinamica.ConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            ElseIf comp.IsHYPO = 1 Then
                Return True
            Else
                Return True
            End If

        End Function

#End Region

#Region "Functions to Calculate Isolated Properties"

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double
            Dim val As Double
            Dim Z As Double = m_pr.Z_PR(T, P, RET_VMOL(Fase.Vapor), RET_VKij, RET_VTC, RET_VPC, RET_VW, "V")
            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Fase.Vapor)
            Return val
        End Function

        Public Overloads Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Fase, ByVal T As Double, ByVal P As Double)
            Select Case phase
                Case Fase.Liquid
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(1).Componentes.Values
                        subst.PartialVolume = 1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T))
                    Next
                Case Fase.Aqueous
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(6).Componentes.Values
                        subst.PartialVolume = 1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T))
                    Next
                Case Fase.Liquid1
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(3).Componentes.Values
                        subst.PartialVolume = 1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T))
                    Next
                Case Fase.Liquid2
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(4).Componentes.Values
                        subst.PartialVolume = 1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T))
                    Next
                Case Fase.Liquid3
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(5).Componentes.Values
                        subst.PartialVolume = 1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Nome, T))
                    Next
                Case Fase.Vapor
                    Dim partvol As New Object
                    Dim i As Integer = 0
                    partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "V", 0.0001)
                    i = 0
                    For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(2).Componentes.Values
                        subst.PartialVolume = partvol(i)
                        i += 1
                    Next
            End Select
        End Sub

        Public Function RET_KIJ(ByVal id1 As String, ByVal id2 As String) As Double
            If Me.m_pr.InteractionParameters.ContainsKey(id1) Then
                If Me.m_pr.InteractionParameters(id1).ContainsKey(id2) Then
                    Return m_pr.InteractionParameters(id1)(id2).kij
                Else
                    If Me.m_pr.InteractionParameters.ContainsKey(id2) Then
                        If Me.m_pr.InteractionParameters(id2).ContainsKey(id1) Then
                            Return m_pr.InteractionParameters(id2)(id1).kij
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

        Public Overrides Function DW_CalcCp_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Select Case fase1
                Case Fase.Liquid
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Aqueous
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid1
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid2
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Liquid3
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Fase.Vapor
                    Return Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
            End Select
        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double
            Select Case fase1
                Case Fase.Liquid
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Aqueous
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid1
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid2
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Liquid3
                    Return Auxiliary.PROPS.CpCvR("L", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Fase.Vapor
                    Return Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(fase1), RET_VKij(), RET_VMAS(fase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
            End Select
        End Function

        Public Overrides Function DW_CalcEnergiaMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.DW_CalcEnthalpy(RET_VMOL(Fase.Liquid), T, P, State.Liquid)
            HV = Me.DW_CalcEnthalpy(RET_VMOL(Fase.Vapor), T, P, State.Vapor)
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

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Fase.Liquid), Me.RET_VPC(Fase.Liquid), Me.RET_VW(Fase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_SURFTM(T)
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

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            If fase1 = Fase.Liquid Then
                Return Me.AUX_LIQVISCm(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Fase.Vapor))
            End If
        End Function

#End Region

#Region "Main Property Routines"

        Public Function GetArguments() As Object

            If TypeOf Me Is NRTLPropertyPackage Then
                Return DirectCast(Me, NRTLPropertyPackage).RET_VNAMES
            ElseIf TypeOf Me Is SourWaterPropertyPackage Then
                Return DirectCast(Me, SourWaterPropertyPackage).RET_VNAMES
            ElseIf TypeOf Me Is UNIQUACPropertyPackage Then
                Return New Object() {DirectCast(Me, UNIQUACPropertyPackage).RET_VNAMES, DirectCast(Me, UNIQUACPropertyPackage).RET_VQ, DirectCast(Me, UNIQUACPropertyPackage).RET_VR}
            ElseIf TypeOf Me Is COSMOSACPropertyPackage Then
                Return DirectCast(Me, COSMOSACPropertyPackage).RET_VCSACIDS
            ElseIf TypeOf Me Is MODFACPropertyPackage Then
                Return New Object() {DirectCast(Me, MODFACPropertyPackage).RET_VQ, DirectCast(Me, MODFACPropertyPackage).RET_VR, DirectCast(Me, MODFACPropertyPackage).RET_VEKI}
            ElseIf TypeOf Me Is NISTMFACPropertyPackage Then
                Return New Object() {DirectCast(Me, NISTMFACPropertyPackage).RET_VQ, DirectCast(Me, NISTMFACPropertyPackage).RET_VR, DirectCast(Me, NISTMFACPropertyPackage).RET_VEKI}
            ElseIf TypeOf Me Is UNIFACPropertyPackage Then
                Return New Object() {DirectCast(Me, UNIFACPropertyPackage).RET_VQ, DirectCast(Me, UNIFACPropertyPackage).RET_VR, DirectCast(Me, UNIFACPropertyPackage).RET_VEKI}
            ElseIf TypeOf Me Is UNIFACLLPropertyPackage Then
                Return New Object() {DirectCast(Me, UNIFACLLPropertyPackage).RET_VQ, DirectCast(Me, UNIFACLLPropertyPackage).RET_VR, DirectCast(Me, UNIFACLLPropertyPackage).RET_VEKI}
            Else
                Return Nothing
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim H As Double

            If st = State.Liquid Then
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                    Case 1 'Ideal
                        H = Me.RET_Hid_L(298.15, T, Vx)
                    Case 2 'Excess
                        H = Me.RET_Hid_L(298.15, T, Vx) + Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)
                End Select
            Else
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                    Case 1 'Ideal
                        H = Me.RET_Hid_L(298.15, T, Vx) + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    Case 2 'Excess
                        H = Me.RET_Hid_L(298.15, T, Vx) + Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx) + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                End Select
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim H As Double

            If st = State.Liquid Then
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        H = 0.0#
                    Case 2 'Excess
                        H = Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)
                End Select
            Else
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        H = Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    Case 2 'Excess
                        H = Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx) + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                End Select
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim S As Double

            If st = State.Liquid Then
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                    Case 1 'Ideal
                        S = Me.RET_Hid_L(298.15, T, Vx) / T
                    Case 2 'Excess
                        S = (Me.RET_Hid_L(298.15, T, Vx) + Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)) / T
                End Select
            Else
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                    Case 1 'Ideal
                        S = Me.RET_Hid_L(298.15, T, Vx) / T + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                    Case 2 'Excess
                        S = (Me.RET_Hid_L(298.15, T, Vx) + Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)) / T + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                End Select
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        S = 0.0#
                    Case 2 'Excess
                        S = (Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)) / T
                End Select
            Else
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        S = Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                    Case 2 'Excess
                        S = (Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)) / T + Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                End Select
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            DWSIM.App.WriteToConsole(Me.ComponentName & " fugacity coefficient calculation for phase '" & st.ToString & "' requested at T = " & T & " K and P = " & P & " Pa.", 2)
            DWSIM.App.WriteToConsole("Compounds: " & Me.RET_VNAMES.ToArrayString, 2)
            DWSIM.App.WriteToConsole("Mole fractions: " & Vx.ToArrayString(), 2)

            Dim prn As New PropertyPackages.ThermoPlugs.PR

            Dim n As Integer = UBound(Vx)
            Dim lnfug(n), ativ(n) As Double
            Dim fugcoeff(n) As Double
            Dim i As Integer

            Dim Tc As Object = Me.RET_VTC()
            Dim Tr As Double
            If st = State.Liquid Then
                ativ = Me.m_act.CalcActivityCoefficients(T, Vx, Me.GetArguments())
                For i = 0 To n
                    Tr = T / Tc(i)
                    If Tr >= 1.02 Then
                        lnfug(i) = Math.Log(AUX_KHenry(Me.RET_VNAMES(i), T) / P)
                    ElseIf Tr < 0.98 Then
                        lnfug(i) = Math.Log(ativ(i) * Me.AUX_PVAPi(i, T) / (P))
                    Else 'do interpolation at proximity of critical point
                        Dim a2 As Double = AUX_KHenry(Me.RET_VNAMES(i), 1.02 * Tc(i))
                        Dim a1 As Double = ativ(i) * Me.AUX_PVAPi(i, 0.98 * Tc(i))
                        lnfug(i) = Math.Log(((Tr - 0.98) / (1.02 - 0.98) * (a2 - a1) + a1) / P)
                    End If
                Next
            Else
                If Not Me.Parameters.ContainsKey("PP_IDEAL_VAPOR_PHASE_FUG") Then Me.Parameters.Add("PP_IDEAL_VAPOR_PHASE_FUG", 0)
                If Me.Parameters("PP_IDEAL_VAPOR_PHASE_FUG") = 1 Then
                    For i = 0 To n
                        lnfug(i) = 0.0#
                    Next
                Else
                    lnfug = prn.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, "V")
                End If
            End If

            For i = 0 To n
                fugcoeff(i) = Exp(lnfug(i))
            Next

            DWSIM.App.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Fase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = "", pstate As State

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case phase
                Case Fase.Vapor
                    state = "V"
                    pstate = PropertyPackages.State.Vapor
                Case Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3, Fase.Aqueous
                    state = "L"
                    pstate = PropertyPackages.State.Liquid
                Case Fase.Solid
                    state = "S"
                    pstate = PropertyPackages.State.Solid
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
                    result = Me.m_lk.Z_LK(state, T / Me.AUX_TCM(phase), P / Me.AUX_PCM(phase), Me.AUX_WM(phase))(0)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    If state = "V" Then
                        resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        result = resultObj(1)
                    Else
                        Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                            Case 0 'LK
                                resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                                result = resultObj(1)
                            Case 1 'Ideal
                                result = Me.AUX_LIQCPm(T, phaseID)
                            Case 2 'Excess
                                result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(phase), Me.GetArguments()) / Me.AUX_MMM(phase)
                        End Select
                    End If
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                Case "heatcapacitycv"
                    If state = "V" Then
                        resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        result = resultObj(2)
                    Else
                        Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                            Case 0 'LK
                                resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                                result = resultObj(2)
                            Case 1 'Ideal
                                result = Me.AUX_LIQCPm(T, phaseID)
                            Case 2 'Excess
                                result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(phase), Me.GetArguments()) / Me.AUX_MMM(phase)
                        End Select
                    End If
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
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
                If Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow.GetValueOrDefault > 0 Then
                    result = phasemolarfrac * overallmolarflow * Me.AUX_MMM(fase) / 1000 / Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massfraction = result
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(fase)
            End If

            If phaseID = 3 Or phaseID = 4 Or phaseID = 5 Or phaseID = 6 Then

                If TypeOf Me Is SourWaterPropertyPackage Then
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.pH = New Auxiliary.Electrolyte().pH(RET_VMOL(dwpl), T, Me.DW_GetConstantProperties)
                End If

                result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                result = Me.m_lk.Z_LK("L", T / Me.AUX_TCM(dwpl), P / Me.AUX_PCM(dwpl), Me.AUX_WM(dwpl))(0)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                Select Case Me.Parameters("PP_ENTH_CP_CALC_METHOD")
                    Case 0 'LK
                        resultObj = Me.m_lk.CpCvR_LK("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = resultObj(1)
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = resultObj(2)
                    Case 1 'Ideal
                        result = Me.AUX_LIQCPm(T, phaseID)
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                    Case 2 'Excess
                        result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(dwpl), Me.GetArguments()) / Me.AUX_MMM(dwpl)
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                        Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                End Select
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
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                result = m_pr.Z_PR(T, P, RET_VMOL(fase.Vapor), RET_VKij, RET_VTC, RET_VPC, RET_VW, "V")
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                result = Me.AUX_CPm(PropertyPackages.Fase.Vapor, T)
                resultObj = Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(PropertyPackages.Fase.Vapor), RET_VKij(), RET_VMAS(PropertyPackages.Fase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
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

            ElseIf phaseID = 7 Then

                result = Me.AUX_SOLIDDENS
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                Dim constprops As New List(Of ConstantProperties)
                For Each su As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                    constprops.Add(su.ConstantProperties)
                Next
                result = Me.DW_CalcSolidEnthalpy(T, RET_VMOL(PropertyPackages.Fase.Solid), constprops)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result / T
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = 0.0# 'result
                result = Me.DW_CalcSolidHeatCapacityCp(T, RET_VMOL(PropertyPackages.Fase.Solid), constprops)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                result = Me.AUX_MMM(fase)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = 0.0# 'result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = 1.0E+20
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = 1.0E+20

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()

            Else

                DW_CalcOverallProps()


            End If

            If phaseID > 0 Then
                If Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault > 0 And overallmolarflow > 0 Then
                    result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(fase) / 1000 / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            Else
                'result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massflow.GetValueOrDefault / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                'Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            End If

        End Sub

#End Region

    End Class

End Namespace
