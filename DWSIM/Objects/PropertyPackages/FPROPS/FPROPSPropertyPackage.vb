'    FPROPS Property Package
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


' FPROPS
' Copyright (C) 2011 - Carnegie Mellon University
'
' ASCEND is free software; you can redistribute it and/or modify
' it under the terms of the GNU General Public License as published by
' the Free Software Foundation; either version 2 of the License, or
' (at your option) any later version.
'
' ASCEND is distributed in the hope that it will be useful,
' but WITHOUT ANY WARRANTY; without even the implied warranty of
' MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
' GNU General Public License for more details.
'
' You should have received a copy of the GNU General Public License
' along with ASCEND; if not, write to the Free Software
' Foundation, Inc., 51 Franklin St, Fifth Floor,
' Boston, MA  02110-1301  USA
'/

Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports System.Math
Imports DWSIM.DWSIM.MathEx
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages.Auxiliary.FPROPS
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports System.Runtime.InteropServices

Namespace DWSIM.SimulationObjects.PropertyPackages


    <System.Runtime.InteropServices.Guid(FPROPSPropertyPackage.ClassId)> _
    <System.Serializable()> Public Class FPROPSPropertyPackage

        Inherits DWSIM.SimulationObjects.PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "D1CFA6ED-8050-41bf-A2B9-B7D75947E880"

        Protected fprops As New Auxiliary.FPROPS.FPROPS
        Public m_uni As New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.UNIQUAC

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            MyBase.New()

            With Me.Parameters
                .Item("PP_IDEAL_MIXRULE_LIQDENS") = 1
                .Item("PP_FLASHALGORITHM") = 0
                .Item("PP_USEEXPLIQDENS") = 0
                .Add("PP_USE_EOS_LIQDENS", 1)
            End With

            Me.IsConfigurable = True
            Me.ConfigForm = New FormConfigFPROPS
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

        End Sub

        Public Overrides Sub ReconfigureConfigForm()
            MyBase.ReconfigureConfigForm()
            Me.ConfigForm = New FormConfigFPROPS
        End Sub


#Region "    DWSIM Functions"

        Public Overrides Function AUX_LIQDENS(ByVal T As Double, Optional ByVal P As Double = 0.0, Optional ByVal Pvp As Double = 0.0, Optional ByVal phaseid As Integer = 3, Optional ByVal FORCE_EOS As Boolean = False) As Double

            Return CalcRho(RET_VMAS(RET_PHASECODE(phaseid)), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Liquid)

        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Return CalcRho(RET_VMAS(Fase.Vapor), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Vapor)

        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Fase, ByVal T As Double, ByVal P As Double)

            For Each subst As ClassesBasicasTermodinamica.Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                subst.PartialVolume = 0.0#
            Next

        End Sub

        Public Overrides Function DW_CalcCp_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            Dim fstate As State

            Select Case fase1
                Case Fase.Aqueous, Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3
                    fstate = State.Liquid
                Case Fase.Vapor
                    fstate = State.Vapor
            End Select

            Return FPROPS_CalcCp(RET_VMAS(fase1), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            Dim fstate As State

            Select Case fase1
                Case Fase.Aqueous, Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3
                    fstate = State.Liquid
                Case Fase.Vapor
                    fstate = State.Vapor
            End Select

            Return FPROPS_CalcCv(RET_VMAS(fase1), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)

        End Function

        Public Overrides Function DW_CalcEnergiaMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = FPROPS_CalcH(RET_VMAS(Fase.Liquid), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Liquid)
            HV = FPROPS_CalcH(RET_VMAS(Fase.Vapor), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Vapor)
            HM = Me.CurrentMaterialStream.Fases(1).SPMProperties.massfraction.GetValueOrDefault * HL + Me.CurrentMaterialStream.Fases(2).SPMProperties.massfraction.GetValueOrDefault * HV

            Dim ent_massica = HM
            Dim flow = Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow
            Return ent_massica * flow

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return FPROPS_CalcH(AUX_CONVERT_MOL_TO_MASS(Vx), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), st)

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return FPROPS_CalcH(AUX_CONVERT_MOL_TO_MASS(Vx), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), st) - RET_Hid(298.15, T, Fase.Mixture)

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return FPROPS_CalcS(AUX_CONVERT_MOL_TO_MASS(Vx), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), st)

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return FPROPS_CalcS(AUX_CONVERT_MOL_TO_MASS(Vx), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), st) - RET_Sid(298.15, T, P, Fase.Mixture)

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            If m_uni Is Nothing Then m_uni = New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.UNIQUAC

            Dim n As Integer = UBound(Vx)
            Dim lnfug(n), ativ(n) As Double
            Dim fugcoeff(n) As Double
            Dim i As Integer

            Dim Tc As Object = Me.RET_VTC()

            If st = State.Liquid Then
                ativ = Me.m_uni.GAMMA_MR(T, Vx, Me.RET_VNAMES, Me.RET_VQ, Me.RET_VR)
                For i = 0 To n
                    If T / Tc(i) >= 1 Then
                        lnfug(i) = Math.Log(AUX_KHenry(Me.RET_VNAMES(i), T) / P)
                    Else
                        lnfug(i) = Math.Log(ativ(i) * Me.AUX_PVAPi(i, T) / (P))
                    End If
                Next
            Else
                For i = 0 To n
                    lnfug(i) = 0.0#
                Next
            End If

            For i = 0 To n
                fugcoeff(i) = Exp(lnfug(i))
            Next

            Return fugcoeff

        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            If fase1 = Fase.Liquid Then
                Return Me.AUX_CONDTL(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_CONDTG(T, P)
            End If

        End Function

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double, Optional ByVal Pvp As Double = 0.0) As Double

            Dim fstate As State

            Select Case fase1
                Case Fase.Aqueous, Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3
                    fstate = State.Liquid
                Case Fase.Vapor
                    fstate = State.Vapor
            End Select

            Return CalcRho(RET_VMAS(fase1), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)

        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_MMM(fase1)

        End Function

        Public Overrides Sub DW_CalcPhaseProps(ByVal fase As Fase)

            Dim result As Double
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


                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = AUX_LIQDENS(T, P, 0.0#, phaseID)

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result

                result = FPROPS_CalcZ(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result

                result = FPROPS_CalcCp(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result

                result = FPROPS_CalcCv(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Liquid)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result

                result = Me.AUX_MMM(fase)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid) * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result

                result = Me.AUX_CONDTL(T)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result

                result = Me.AUX_LIQVISCm(T)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result

                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value

            ElseIf phaseID = 2 Then

                result = Me.AUX_VAPDENS(T, P)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result

                result = FPROPS_CalcZ(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result

                result = FPROPS_CalcCp(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result

                result = FPROPS_CalcCv(RET_VMAS(dwpl), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), State.Vapor)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result

                result = Me.AUX_MMM(fase)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result

                result = Me.DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result

                result = Me.DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor) * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result

                result = Me.AUX_CONDTG(T, P)
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result

                result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault, Me.AUX_MMM(fase))
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()


            Else

                DW_CalcOverallProps()

            End If


            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(fase) / 1000 / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            End If


        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Fase)

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""
            Dim fstate As State = PropertyPackages.State.Solid

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case phase
                Case Fase.Vapor
                    state = "V"
                    fstate = PropertyPackages.State.Vapor
                Case Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3, Fase.Aqueous
                    state = "L"
                    fstate = PropertyPackages.State.Liquid
                Case Fase.Solid
                    state = "S"
                    fstate = PropertyPackages.State.Solid
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
                    result = FPROPS_CalcZ(RET_VMAS(phase), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    result = FPROPS_CalcCp(RET_VMAS(phase), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                Case "heatcapacitycv"
                    result = FPROPS_CalcCv(RET_VMAS(phase), T, P, GetFluidStruct(GetFpropsFluidID(RET_VNAMES())), fstate)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = result * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = result * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                Case "excessenthalpy"
                    result = Me.DW_CalcEnthalpyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.DW_CalcEntropyDeparture(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEnthalpy(RET_VMOL(phase), T, P, phase)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF = result + entF
                    result = result * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = 0.0#
                    result = Me.DW_CalcEntropy(RET_VMOL(phase), T, P, phase)
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

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double

            Return 0.0#

        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            Return Me.AUX_SURFTM(T)

        End Function

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal fase1 As Fase, ByVal fase2 As Fase)

            Dim T As Double

            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            Me.CurrentMaterialStream.Fases(0).TPMProperties.surfaceTension = Me.AUX_SURFTM(T)

        End Sub

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double

            If fase1 = Fase.Liquid Then
                Return Me.AUX_LIQVISCm(T)
            ElseIf fase1 = Fase.Vapor Then
                Return Me.AUX_VAPVISCm(T, Me.AUX_VAPDENS(T, P), Me.AUX_MMM(Fase.Vapor))
            End If

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As ClassesBasicasTermodinamica.ConstantProperties) As Boolean

            Return True

        End Function

#End Region

#Region "    Auxiliary Functions"

        Shared Function SupportsCompound(compname As String) As Boolean

            Select Case compname
                Case "Ammonia", "Nitrogen", "Hydrogen", "Water", "Carbon dioxide", "Methane", "Carbon monoxide", "Ethanol", "Acetone", "Carbonyl sulfide", "N-decane",
                    "Hydrogen sulfide", "Isopentane", "Krypton", "Neopentane", "Nitrous oxide", "N-nonane", "Sulfur dioxide", "Toluene", "Xenon", "1-butene", "Cis-2-butene",
                    "Isobutene","Trans-2-butene","Dimethyl ether","Ethane","Isobutane"
                    Return True
                Case Else
                    Return False
            End Select

        End Function

        Public Function GetFpropsFluidID(ByVal names() As String) As String()

            Dim fpnames As New ArrayList

            For Each name As String In names

                Select Case name
                    Case "Ammonia"
                        fpnames.Add("ammonia")
                    Case "Nitrogen"
                        fpnames.Add("nitrogen")
                    Case "Hydrogen"
                        fpnames.Add("hydrogen")
                    Case "Water"
                        fpnames.Add("water")
                    Case "Carbon dioxide"
                        fpnames.Add("carbondioxide")
                    Case "Methane"
                        fpnames.Add("methane")
                    Case "Carbon monoxide"
                        fpnames.Add("carbonmonoxide")
                    Case "Ethanol"
                        fpnames.Add("ethanol")
                    Case "Acetone"
                        fpnames.Add("acetone")
                    Case "Carbonyl sulfide"
                        fpnames.Add("carbonylsulfide")
                    Case "N-decane"
                        fpnames.Add("decane")
                    Case "Hydrogen sulfide"
                        fpnames.Add("hydrogensulfide")
                    Case "Isopentane"
                        fpnames.Add("isopentane")
                    Case "Krypton"
                        fpnames.Add("krypton")
                    Case "Neopentane"
                        fpnames.Add("neopentane")
                    Case "Nitrous oxide"
                        fpnames.Add("nitrousoxide")
                    Case "N-nonane"
                        fpnames.Add("nonane")
                    Case "Sulfur dioxide"
                        fpnames.Add("sulfurdioxide")
                    Case "Toluene"
                        fpnames.Add("toluene")
                    Case "Xenon"
                        fpnames.Add("xenon")
                    Case "1-butene"
                        fpnames.Add("butane")
                    Case "Cis-2-butene"
                        fpnames.Add("cisbutene")
                    Case "Isobutene"
                        fpnames.Add("isobutene")
                    Case "Trans-2-butene"
                        fpnames.Add("transbutene")
                    Case "Dimethyl ether"
                        fpnames.Add("dimethylether")
                    Case "Ethane"
                        fpnames.Add("ethane")
                    Case "Isobutane"
                        fpnames.Add("isobutane")
                    Case Else
                        Throw New ArgumentOutOfRangeException(name, "Error: compound '" & name & "' is not supported by this version of FPROPS.")
                        Return Nothing
                End Select

            Next

            Return fpnames.ToArray(Type.GetType("System.String"))

        End Function

        'Public Function GetDominantCompound(ByVal Vz As Double()) As String

        '    Dim i As Integer = 0
        '    Dim maxZ As Double = Vz(0)
        '    Dim name As String = ""

        '    For Each s As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
        '        If s.FracaoMolar.GetValueOrDefault >= maxZ Then
        '            name = s.Nome
        '            maxZ = Vz(i)
        '        End If
        '        i += 1
        '    Next

        '    Return name

        'End Function

        'Public Function GetDominantCompound() As String()

        '    Dim i As Integer = 0
        '    Dim maxZ As Double = 0.0#
        '    Dim name As String = ""

        '    For Each s As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values
        '        If s.FracaoMolar.GetValueOrDefault >= maxZ Then
        '            name = s.Nome
        '            maxZ = s.FracaoMolar.GetValueOrDefault
        '        End If
        '        i += 1
        '    Next

        '    Return name

        'End Function

        Public Function GetFluidStruct(ByVal fluidnames() As String) As PureFluid_struct()

            DWSIM.App.CheckParallelPInvoke()

            Dim mystruct(fluidnames.Length - 1) As PureFluid_struct

            For i As Integer = 0 To fluidnames.Length - 1
                Dim data As System.IntPtr = fprops.fprops_fluid(fluidnames(i), Nothing)
                mystruct(i) = Marshal.PtrToStructure(data, Type.GetType("DWSIM.DWSIM.SimulationObjects.PropertyPackages.Auxiliary.FPROPS.PureFluid_struct"))
            Next

            Return mystruct

        End Function

        Private Function FPROPS_CalcCp(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim result As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                result += Vw(i) * fprops.fprops_cp(T, CalcRho(T, P, fluidstruct(i), fluidstate), fluidstruct(i), myerr) / 1000 'kJ/kg
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_cp, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct(i).name & ", state = " & fluidstate.ToString & "]")
            Next


            Return result

        End Function

        Private Function FPROPS_CalcCv(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim result As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                result += Vw(i) * fprops.fprops_cv(T, CalcRho(T, P, fluidstruct(i), fluidstate), fluidstruct(i), myerr) / 1000 'kJ/kg
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_cv, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct(i).name & ", state = " & fluidstate.ToString & "]")
            Next

            Return result

        End Function

        Private Function FPROPS_CalcH(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim result As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                result += Vw(i) * fprops.fprops_h(T, CalcRho(T, P, fluidstruct(i), fluidstate), fluidstruct(i), myerr) / 1000 'kJ/kg
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_h, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct(i).name & ", state = " & fluidstate.ToString & "]")
            Next

            Return result

        End Function

        Private Function FPROPS_CalcS(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim result As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                result += Vw(i) * fprops.fprops_s(T, CalcRho(T, P, fluidstruct(i), fluidstate), fluidstruct(i), myerr) / 1000 'kJ/kg.K
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_s, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct(i).name & ", state = " & fluidstate.ToString & "]")
            Next

            Return result

        End Function

        Private Function FPROPS_CalcZ(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim _rho As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                _rho += Vw(i) / CalcRho(T, P, fluidstruct(i), fluidstate)
            Next

            Dim rho As Double = 1 / _rho

            Dim result As Double = P / (rho * 8.314 * T) / 1000 * AUX_MMM(Fase.Mixture)

            Return result

        End Function

        Private Function CalcRho(ByVal T As Double, ByVal P As Double, ByVal fluidstruct As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim f1, f2, x1, dfdx As Double, cnt As Integer
            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR
            Dim satt, rhof, rhog As Double

            If fluidstate = State.Liquid Then
                x1 = 1000
            Else
                x1 = 1
            End If

            cnt = 0

            f1 = 1000

            While Abs(f1) >= 0.00001

                f1 = P - fprops.fprops_p(T, x1, fluidstruct, myerr)
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_p, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct.name & ", state = " & fluidstate.ToString & "]")
                f2 = P - fprops.fprops_p(T, x1 * 1.01, fluidstruct, myerr)
                If myerr <> FpropsError_enum.FPROPS_NO_ERROR Then Throw New Exception("FPROPS returned an error: " & myerr & " [@ fprops_p, T = " & T & " K, P = " & P & " Pa, fluid = " & fluidstruct.name & ", state = " & fluidstate.ToString & "]")
                dfdx = (f2 - f1) / (0.01 * x1)

                If Abs(dfdx) < 0.0000000001 Or Double.IsNaN(f1) Or Double.IsInfinity(f1) Then
                    DWSIM.App.WriteToConsole("FPROPS WARNING: compound: " & fluidstruct.name & ", state: " & fluidstate.ToString, 2)
                    DWSIM.App.WriteToConsole("FPROPS WARNING: unable to calculate density at P = " & P & " Pa and T = " & T & " K", 2)
                    fprops.fprops_sat_p(P, satt, rhof, rhog, fluidstruct)
                    If fluidstate = State.Liquid Then
                        DWSIM.App.WriteToConsole("FPROPS WARNING: returning calculated density @ saturation temperature (" & satt & " K => " & rhof & " kg/m3", 2)
                        Return rhof
                    Else
                        DWSIM.App.WriteToConsole("FPROPS WARNING: returning calculated density @ saturation temperature (" & satt & " K => " & rhog & " kg/m3", 2)
                        Return rhog
                    End If
                End If

                x1 = x1 - f1 / dfdx
                cnt += 1

                If cnt > 500 Then Exit While

            End While

            Return x1

        End Function

        Private Function CalcRho(ByVal Vw As Array, ByVal T As Double, ByVal P As Double, ByVal fluidstruct() As PureFluid_struct, ByVal fluidstate As State) As Double

            DWSIM.App.CheckParallelPInvoke()

            Dim myerr As FpropsError_enum = FpropsError_enum.FPROPS_NO_ERROR

            Dim _rho As Double = 0

            For i As Integer = 0 To Vw.Length - 1
                _rho += Vw(i) / CalcRho(T, P, fluidstruct(i), fluidstate)
            Next

            Dim rho As Double = 1 / _rho

            Return rho

        End Function

#End Region

#Region "    UNIQUAC Auxiliary Functions"

        Function RET_VQ() As Object

            Dim subst As DWSIM.ClassesBasicasTermodinamica.Substancia
            Dim VQ(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                VQ(i) = subst.ConstantProperties.UNIQUAC_Q
                i += 1
            Next

            Return VQ

        End Function

        Function RET_VR() As Object

            Dim subst As DWSIM.ClassesBasicasTermodinamica.Substancia
            Dim VR(Me.CurrentMaterialStream.Fases(0).Componentes.Count - 1) As Double
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Fases(0).Componentes.Values
                VR(i) = subst.ConstantProperties.UNIQUAC_R
                i += 1
            Next

            Return VR

        End Function

#End Region

    End Class

End Namespace
