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

Imports System.Math
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable> Public MustInherit Class ActivityCoefficientPropertyPackage

        Inherits PropertyPackage

        Public m_pr As New PropertyPackages.Auxiliary.PengRobinson
        Public m_lk As New PropertyPackages.Auxiliary.LeeKesler

        Public m_act As PropertyPackages.Auxiliary.IActivityCoefficientBase

#Region "Initialization"

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

            EnthalpyEntropyCpCvCalculationMode = EnthalpyEntropyCpCvCalcMode.Excess

            LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.COSTALD

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal / PR EOS"
                .Vapor_Enthalpy_Entropy_CpCv = "Ideal / Lee-Kesler / Excess / Experimental"
                .Vapor_Density = "Ideal / PR EOS"
                .Liquid_Fugacity = "Activity Coefficient + Poynting + Vapor Pressure / Henry's Constant"
                .Liquid_Enthalpy_Entropy_CpCv = "Ideal / Lee-Kesler / Excess / Experimental"
            End With

        End Sub

        Public Overrides Sub ConfigParameters()

        End Sub

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            ElseIf comp.IsHYPO = 1 Then
                Return True
            Else
                Return True
            End If

        End Function

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfigPropertyPackage() With {._form = Me.Flowsheet, ._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfigPropertyPackage() With {._form = Me.Flowsheet, ._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Function GetEditingForm() As Form

            Return New FormConfigPropertyPackage() With {._form = Me.Flowsheet, ._pp = Me, ._comps = Flowsheet.SelectedCompounds}

        End Function

#End Region

#Region "Functions to Calculate Isolated Properties"

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_VAPDENS", "Vapor Phase Density", "Vapor Phase Density Calculation Routine")

            IObj?.SetCurrent()

            Dim val As Double
            Dim Z As Double = 1.0#
            If VaporPhaseFugacityCalculationMode = VaporPhaseFugacityCalcMode.Ideal Then
                IObj?.Paragraphs.Add("Ideal Gas Vapor Phase assumption is enabled.")
                Z = 1.0#
            Else
                IObj?.Paragraphs.Add("Real Gas Vapor Phase assumption is enabled.")
                Z = m_pr.Z_PR(T, P, RET_VMOL(Phase.Vapor), RET_VKij, RET_VTC, RET_VPC, RET_VW, "V")
            End If

            IObj?.Paragraphs.Add("<h2>Intermediate Calculations</h2>")
            IObj?.Paragraphs.Add(String.Format("Vapor Phase Compressibility Factor: {0}", val))

            val = P / (Z * 8.314 * T) / 1000 * AUX_MMM(Phase.Vapor)

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Vapor Phase Density: {0} kg/m3", val))

            IObj?.Close()

            Return val

        End Function

        Public Overloads Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

            Dim i As Integer

            For j As Integer = 1 To 7

                If j <> 2 Then

                    Dim Vx = RET_VMOL(RET_PHASECODE(j))
                    Dim n As Integer = Vx.Length - 1

                    Dim constprop = DW_GetConstantProperties()

                    Dim ativ(n), poy1(n), poy2(n), vex(n) As Double

                    ativ = m_act.CalcActivityCoefficients(T, Vx, Me.GetArguments())

                    Dim P2 As Double = P + 1

                    Dim Psati, vli As Double
                    For i = 0 To n
                        vli = 1 / AUX_LIQDENSi(constprop(i), T) * constprop(i).Molar_Weight
                        If Double.IsNaN(vli) Then
                            vli = 1 / AUX_LIQDENSi(constprop(i), constprop(i).Normal_Boiling_Point) * constprop(i).Molar_Weight
                        End If
                        Psati = AUX_PVAPi(i, T)
                        poy1(i) = Math.Exp(vli * Abs(P - Psati) / (8314.47 * T))
                        poy2(i) = Math.Exp(vli * Abs(P2 - Psati) / (8314.47 * T))
                        vex(i) = (Log(poy2(i)) - Log(poy1(i))) * 8.314 * T * 1000 'm3/kmol
                    Next

                    i = 0
                    For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(j).Compounds.Values
                        subst.PartialVolume = vex(i)
                        i += 1
                    Next

                Else

                    If VaporPhaseFugacityCalculationMode = VaporPhaseFugacityCalcMode.Ideal Then
                        Dim vapdens = AUX_VAPDENS(T, P)
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.PartialVolume = subst.ConstantProperties.Molar_Weight / vapdens
                        Next
                    Else
                        Dim partvol As New Object
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(2), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "V", 0.0001)
                        i = 0
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(2).Compounds.Values
                            subst.PartialVolume = partvol(i)
                            i += 1
                        Next
                    End If

                End If

            Next

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
            Return 0.0#
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
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Liquid), T, P, State.Liquid)
            HV = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, State.Vapor)
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

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Auxiliary.PROPS.Pvp_leekesler(T, Me.RET_VTC(Phase.Liquid), Me.RET_VPC(Phase.Liquid), Me.RET_VW(Phase.Liquid))
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.AUX_SURFTM(T)
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

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.AUX_LIQVISCm(T, P)
            ElseIf Phase1 = Phase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Phase.Vapor))
            Else
                Return 0.0#
            End If
        End Function

#End Region

#Region "Main Property Routines"

        Public Overrides Sub AddDefaultCompounds(compnames() As String)

            If TypeOf Me Is SourWaterPropertyPackage Then
                MyBase.AddDefaultCompounds(compnames)
            Else
                MyBase.AddDefaultCompounds(New String() {"Water", "Ethanol"})
            End If

        End Sub

        Public Overridable Function GetArguments() As Object

            If TypeOf Me Is NRTLPropertyPackage Then
                Return DirectCast(Me, NRTLPropertyPackage).RET_VNAMES
            ElseIf TypeOf Me Is SourWaterPropertyPackage Then
                Return DirectCast(Me, SourWaterPropertyPackage).RET_VNAMES
            ElseIf TypeOf Me Is UNIQUACPropertyPackage Then
                Return New Object() {DirectCast(Me, UNIQUACPropertyPackage).RET_VNAMES, DirectCast(Me, UNIQUACPropertyPackage).RET_VQ, DirectCast(Me, UNIQUACPropertyPackage).RET_VR}
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

            If OverrideEnthalpyCalculation Then

                Return EnthalpyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEnthalpy", "Enthalpy", "Property Package Enthalpy Calculation Routine")

                IObj?.SetCurrent()

                Dim H As Double

                If st = State.Liquid Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0 'LK
                            H = Me.m_lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                        Case 1 'Ideal
                            H = Me.RET_Hid(298.15, T, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                        Case 2 'Excess
                            Dim Hex = Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)
                            If Double.IsNaN(Hex) Then
                                If Flowsheet IsNot Nothing Then
                                    Flowsheet.ShowMessage(Tag + ": " + Flowsheet.GetTranslatedString("Error calculating excess enthalpy from activity coefficients. If this error persists, try changing the Enthalpy Calculation Mode in Property Package settings"), Interfaces.IFlowsheet.MessageType.Warning)
                                    Flowsheet.ShowMessage(Tag + ": " + Flowsheet.GetTranslatedString("Assuming excess enthalpy = 0"), Interfaces.IFlowsheet.MessageType.Warning)
                                    Hex = 0.0
                                Else
                                    Throw New Exception("Error calculating excess enthalpy from activity coefficients. If this error persists, try changing the Enthalpy Calculation Mode in Property Package settings.")
                                End If
                            End If
                            H = Me.RET_Hid(298.15, T, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - Hex - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                        Case 3 'Experimental Liquid
                            H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                    End Select
                ElseIf st = State.Vapor Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0 'LK
                            H = Me.m_lk.H_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx))
                        Case 1 'Ideal
                            H = Me.RET_Hid(298.15, T, Vx)
                        Case 2 'Excess
                            H = Me.RET_Hid(298.15, T, Vx)
                        Case 3 'Experimental Liquid
                            H = RET_Hid_FromLiqCp(Vx, T, P)
                    End Select
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        H = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties)
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0 'LK
                                H = Me.m_lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Vx)) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                            Case 1, 2 'Ideal
                                H = Me.RET_Hid(298.15, T, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                            Case 3 'Experimental Liquid
                                H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                        End Select
                    End If
                End If

                IObj?.Close()

                Return H

            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim H As Double

            If st = State.Liquid Then
                Select Case EnthalpyEntropyCpCvCalculationMode
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        H = 0.0#
                    Case 2 'Excess
                        H = Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)
                End Select
            Else
                Select Case EnthalpyEntropyCpCvCalculationMode
                    Case 0 'LK
                        H = Me.m_lk.H_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        H = 0.0#
                    Case 2 'Excess
                        H = 0.0#
                End Select
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If OverrideEntropyCalculation Then

                Return EntropyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEntropy", "Entropy", "Entropy Calculation Routine")

                IObj?.SetCurrent()

                Dim S As Double

                If st = State.Liquid Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0 'LK
                            S = Me.m_lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                        Case 1 'Ideal
                            S = Me.RET_Sid(298.15, T, P, Vx) - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                        Case 2 'Excess
                            Dim gammaex = Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)
                            If Double.IsNaN(gammaex) Then Throw New Exception("Error calculating excess enthalpy from activity coefficients. If this error persists, try changing the Enthalpy Calculation Mode in Property Package settings.")
                            S = Me.RET_Sid(298.15, T, P, Vx) - gammaex / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                        Case 3 'Experimental Liquid
                            S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                    End Select
                ElseIf st = State.Vapor Then
                    Select Case EnthalpyEntropyCpCvCalculationMode
                        Case 0 'LK
                            S = Me.m_lk.S_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx))
                        Case 1 'Ideal
                            S = Me.RET_Sid(298.15, T, P, Vx)
                        Case 2 'Excess
                            S = Me.RET_Sid(298.15, T, P, Vx)
                        Case 3 'Experimental Liquid
                            S = RET_Sid_FromLiqCp(Vx, T, P)
                    End Select
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        S = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties) / T
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0 'LK
                                S = Me.m_lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Sid(298.15, T, P, Vx)) - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                            Case 1 'Ideal
                                S = Me.RET_Sid(298.15, T, P, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                            Case 2 'Excess
                                S = Me.RET_Sid(298.15, T, P, Vx) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T - Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx) / T - Me.RET_HVAPM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T - Me.RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                            Case 3 'Experimental Liquid
                                S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                        End Select
                    End If
                End If

                IObj?.Close()

                Return S

            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                Select Case EnthalpyEntropyCpCvCalculationMode
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        S = 0.0#
                    Case 2 'Excess
                        S = (Me.m_act.CalcExcessEnthalpy(T, Vx, Me.GetArguments()) / Me.AUX_MMM(Vx)) / T
                End Select
            Else
                Select Case EnthalpyEntropyCpCvCalculationMode
                    Case 0 'LK
                        S = Me.m_lk.S_LK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
                    Case 1 'Ideal
                        S = 0.0#
                    Case 2 'Excess
                        S = 0.0#
                End Select
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
            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", RET_VNAMES.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Mole Fractions: {0}", DirectCast(Vx, Double()).ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("State: {0}", [Enum].GetName(st.GetType, st)))

            If Not ActivityCoefficientModels_IgnoreMissingInteractionParameters Then
                CheckMissingInteractionParameters(Vx)
            End If

            Dim n As Integer = Vx.Length - 1
            Dim lnfug(n), ativ(n) As Double
            Dim fugcoeff(n), poy(n) As Double
            Dim i As Integer

            Dim constprop = Me.DW_GetConstantProperties

            Dim Tc As Double() = Me.RET_VTC()
            Dim Tr As Double
            If st = State.Liquid Then

                If LiquidFugacity_UsePoyntingCorrectionFactor Then
                    IObj?.Paragraphs.Add(String.Format("<h2>Poynting Correction</h2>"))
                    IObj?.Paragraphs.Add(String.Format("Poynting Correction Factor calculation is enabled."))
                    IObj?.Paragraphs.Add("The Poynting factor is a correction factor for the liquid phase vapor pressure. 
                                        Unless pressures are very high, the Poynting factor is usually small and the exponential term is near 1.")
                    IObj?.Paragraphs.Add("<m>{\ln {\frac {f}{f_{\mathrm {sat} }}}={\frac {V_{\mathrm {m} }}{RT}}\int _{P_{\mathrm {sat} }}^{P}dp={\frac {V\left(P-P_{\mathrm {sat} }\right)}{RT}}.}</m>")
                    IObj?.Paragraphs.Add("This fraction is known as the Poynting correction factor. Using <mi>f_{sat}=\phi_{sat} P_{sat}</mi>, where <mi>\phi_{sat}</mi> is the fugacity coefficient,")
                    IObj?.Paragraphs.Add("<m>f=\varphi _{\mathrm {sat} }P_{\mathrm {sat} }\exp \left({\frac {V\left(P-P_{\mathrm {sat} }\right)}{RT}}\right).</m>")
                    Dim Psati, vli As Double
                    For i = 0 To n
                        If T < 0.98 * Tc(i) Then
                            IObj?.SetCurrent()
                            IObj?.Paragraphs.Add(String.Format("<b>{0}</b>", constprop(i).Name))
                            vli = 1 / AUX_LIQDENSi(constprop(i), T) * constprop(i).Molar_Weight
                            If Double.IsNaN(vli) Then
                                IObj?.SetCurrent()
                                vli = 1 / AUX_LIQDENSi(constprop(i), constprop(i).Normal_Boiling_Point) * constprop(i).Molar_Weight
                            End If
                            IObj?.Paragraphs.Add(String.Format("Molar Volume (V) @ {0} K: {1} m3/kmol", T, vli))
                            IObj?.SetCurrent()
                            Psati = AUX_PVAPi(i, T)
                            IObj?.Paragraphs.Add(String.Format("Vapor Pressure (Psat) @ {0} K: {1} Pa", T, Psati))
                            poy(i) = Math.Exp(vli * Abs(P - Psati) / (8314.47 * T))
                            IObj?.Paragraphs.Add(String.Format("Poynting Correction Factor: {0}", poy(i)))
                        End If
                    Next
                Else
                    For i = 0 To n
                        poy(i) = 1.0#
                    Next
                End If

                IObj?.SetCurrent()
                IObj?.Paragraphs.Add(String.Format("<h2>Activity Coefficients</h2>"))
                ativ = Me.m_act.CalcActivityCoefficients(T, Vx, Me.GetArguments())

                IObj?.Paragraphs.Add(String.Format("Calculated Activity Coefficients: {0}", ativ.ToMathArrayString))

                IObj?.Paragraphs.Add(String.Format("<h2>Fugacity Coefficients</h2>"))

                For i = 0 To n
                    Tr = T / Tc(i)
                    IObj?.Paragraphs.Add(String.Format("<b>{0}</b>", constprop(i).Name))
                    IObj?.Paragraphs.Add("Reduced Temperature (<mi>T_r=T/T_c</mi>): " & Tr.ToString)
                    If Tr >= 1.02 Then
                        IObj?.SetCurrent()
                        IObj?.Paragraphs.Add("<m>f_i = H_i/P</m>")
                        If UseHenryConstants And HasHenryConstants(constprop(i).Name) Then
                            Dim hc = AUX_KHenry(constprop(i).Name, T)
                            IObj?.Paragraphs.Add(String.Format("Henry's Constant (H) @ {0} K: {1} Pa", T, hc))
                            lnfug(i) = Log(hc / P)
                        Else
                            lnfug(i) = Log(AUX_PVAPi(i, T) / (P))
                        End If
                    ElseIf Tr < 0.98 Then
                        IObj?.Paragraphs.Add("<m>f_i = \gamma_i Poy_i P_{sat_i}/P</m>")
                        IObj?.Paragraphs.Add(String.Format("Activity Coefficient: {0}", ativ(i)))
                        IObj?.Paragraphs.Add(String.Format("Vapor Pressure (Psat) @ {0} K: {1} Pa", T, Me.AUX_PVAPi(i, T)))
                        IObj?.Paragraphs.Add(String.Format("Poynting Correction Factor: {0}", poy(i)))
                        lnfug(i) = Log(ativ(i) * Me.AUX_PVAPi(i, T) / (P)) + Log(poy(i))
                        IObj?.Paragraphs.Add(String.Format("Fugacity Coefficient: {0}", Exp(lnfug(i))))
                    Else 'do interpolation at proximity of critical point
                        IObj?.SetCurrent()
                        Dim a2 As Double = AUX_KHenry(Me.RET_VNAMES(i), 1.02 * Tc(i))
                        Dim a1 As Double = ativ(i) * Me.AUX_PVAPi(i, 0.98 * Tc(i))
                        If Not Double.IsNaN(a1) Then
                            lnfug(i) = Math.Log(((Tr - 0.98) / (1.02 - 0.98) * (a2 - a1) + a1) / P)
                        Else
                            lnfug(i) = Log(a2 / P)
                        End If
                    End If
                Next

            Else

                If VaporPhaseFugacityCalculationMode = VaporPhaseFugacityCalcMode.Ideal Then
                    For i = 0 To n
                        lnfug(i) = 0.0#
                    Next
                Else
                    Dim prn As New PropertyPackages.ThermoPlugs.PR
                    IObj?.SetCurrent()
                    lnfug = prn.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, 1)
                End If

            End If

            For i = 0 To n
                fugcoeff(i) = Exp(lnfug(i))
            Next

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            IObj?.Paragraphs.Add(String.Format("<h2>Results</h2>"))

            IObj?.Paragraphs.Add(String.Format("Fugacity Coefficients: {0}", fugcoeff.ToMathArrayString))

            IObj?.Close()

            Return fugcoeff

        End Function

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
                    result = Me.m_lk.Z_LK(state, T / Me.AUX_TCM(phase), P / Me.AUX_PCM(phase), Me.AUX_WM(phase))(0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    If state = "V" Then
                        resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        result = resultObj(1)
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0 'LK
                                resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                                result = resultObj(1)
                            Case 1, 3 'Ideal/Experimental
                                result = Me.AUX_LIQCPm(T, phaseID)
                            Case 2 'Excess
                                result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(phase), Me.GetArguments()) / Me.AUX_MMM(phase)
                        End Select
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    If state = "V" Then
                        resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        result = resultObj(2)
                    Else
                        Select Case EnthalpyEntropyCpCvCalculationMode
                            Case 0 'LK
                                resultObj = Me.m_lk.CpCvR_LK(state, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                                result = resultObj(2)
                            Case 1, 3 'Ideal/Experimental
                                result = Me.AUX_LIQCPm(T, phaseID)
                            Case 2 'Excess
                                result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(phase), Me.GetArguments()) / Me.AUX_MMM(phase)
                        End Select
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, pstate)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, pstate)
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

            IObj?.SetCurrent

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

                If TypeOf Me Is SourWaterPropertyPackage Then
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.pH = New Auxiliary.Electrolyte().pH(RET_VMOL(dwpl), T, Me.DW_GetConstantProperties)
                End If

                IObj?.SetCurrent

                result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                IObj?.SetCurrent
                result = Me.m_lk.Z_LK("L", T / Me.AUX_TCM(dwpl), P / Me.AUX_PCM(dwpl), Me.AUX_WM(dwpl))(0)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                IObj?.SetCurrent
                Select Case EnthalpyEntropyCpCvCalculationMode
                    Case 0 'LK
                        resultObj = Me.m_lk.CpCvR_LK("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
                    Case 1, 3 'Ideal/Experimental
                        result = Me.AUX_LIQCPm(T, phaseID)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    Case 2 'Excess
                        result = Me.AUX_LIQCPm(T, phaseID) + Me.m_act.CalcExcessHeatCapacity(T, RET_VMOL(dwpl), Me.GetArguments()) / Me.AUX_MMM(dwpl)
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                End Select
                result = Me.AUX_MMM(Phase)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                IObj?.SetCurrent
                result = Me.AUX_CONDTL(T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                IObj?.SetCurrent
                result = Me.AUX_LIQVISCm(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 2 Then

                IObj?.SetCurrent
                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                IObj?.SetCurrent
                result = m_pr.Z_PR(T, P, RET_VMOL(Phase.Vapor), RET_VKij, RET_VTC, RET_VPC, RET_VW, "V")
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                IObj?.SetCurrent
                result = Me.AUX_CPm(PropertyPackages.Phase.Vapor, T)
                IObj?.SetCurrent
                resultObj = Auxiliary.PROPS.CpCvR("V", T, P, RET_VMOL(PropertyPackages.Phase.Vapor), RET_VKij(), RET_VMAS(PropertyPackages.Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
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

                IObj?.SetCurrent
                result = Me.AUX_SOLIDDENS
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Solid)
                IObj?.SetCurrent
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Solid)
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
                If Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault > 0 And overallmolarflow > 0 Then
                    result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Else
                    result = 0
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            Else
                'result = Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow.GetValueOrDefault / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                'Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

        End Sub

#End Region

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_Z", "Compressibility Factor", "Compressibility Factor Calculation Routine")

            IObj?.SetCurrent()

            Dim TCM As Double = RET_VTC().MultiplyY(Vx).Sum
            Dim PCM As Double = RET_VPC().MultiplyY(Vx).Sum
            Dim WM As Double = RET_VW().MultiplyY(Vx).Sum

            Dim val As Double
            If state = PhaseName.Liquid Then
                val = P / (Me.AUX_LIQDENS(T, Vx, P) * 8.314 * T) / 1000 * AUX_MMM(Vx)
            Else
                val = P / (Me.AUX_VAPDENS(T, P) * 8.314 * T) / 1000 * AUX_MMM(Vx)
            End If

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Compressibility Factor: {0}", val))

            IObj?.Close()

            Return val

        End Function

        Public MustOverride Function CheckMissingInteractionParameters(Vx As Double()) As Boolean

    End Class

End Namespace
