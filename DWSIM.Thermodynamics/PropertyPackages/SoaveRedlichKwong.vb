'    SRK Property Package 
'    Copyright 2008 Daniel Wagner O. de Medeiros
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
'
'Imports CAPEOPEN_PD.CAPEOPEN
'Imports DWSIM.SimulationObjects
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.Math
Imports System.Threading.Tasks
Imports System.Linq
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(SRKPropertyPackage.ClassId)>
    <System.Serializable()> Public Class SRKPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "920D043C-F640-4cca-B301-228E773D4E35"

        Private KijMatrix As Double(,)

        Public m_pr As New PropertyPackages.Auxiliary.SRK
        Private m_id As New PropertyPackages.Auxiliary.Ideal

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Overrides ReadOnly Property DisplayName As String = "Soave-Redlich-Kwong"

        Public Overrides ReadOnly Property DisplayDescription As String =
            "Property Package that uses the Soave-Redlich-Kwong Cubic Equation of State. Recommended for use with hydrocarbons and non-condensables at high pressures."

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
            'With Me.Parameters
            '    .Add("PP_USE_EOS_LIQDENS", 0)
            '    .Add("PP_USE_EOS_VOLUME_SHIFT", 0)
            'End With
        End Sub

        Public Sub New()

            MyBase.New()

            'With Me.Parameters
            '    .Add("PP_USE_EOS_LIQDENS", 0)
            '    .Add("PP_USE_EOS_VOLUME_SHIFT", 0)
            'End With

            Me.IsConfigurable = True
            Me._packagetype = PropertyPackages.PackageType.EOS

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Soave-Redlich-Kwong EOS"
                .Vapor_Enthalpy_Entropy_CpCv = "Soave-Redlich-Kwong EOS"
                .Vapor_Density = "Soave-Redlich-Kwong EOS"
                .Liquid_Fugacity = "Soave-Redlich-Kwong EOS"
                .Liquid_Enthalpy_Entropy_CpCv = "Soave-Redlich-Kwong EOS"
                .Liquid_Density = "Soave-Redlich-Kwong EOS (+VT) / Experimental / Rackett / COSTALD"
            End With

        End Sub

        Public Overrides Function GetModel() As Object
            Return m_pr
        End Function

        Public Overrides Sub DisplayEditingForm()

            If GlobalSettings.Settings.CAPEOPENMode Then
                Dim f As New FormConfigPropertyPackage() With {._pp = Me, ._comps = _selectedcomps.ToDictionary(Of String, Interfaces.ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)}
                f.ShowDialog()
            Else
                Dim f As New FormConfigPropertyPackage() With {._pp = Me, ._comps = Flowsheet.SelectedCompounds}
                f.ShowDialog()
            End If

        End Sub

        Public Overrides Sub ConfigParameters()

        End Sub

#Region "    DWSIM Functions"

        Public Overrides Function CalcJouleThomsonCoefficient(p As Interfaces.IPhase) As Double

            Dim T, P0 As Double
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    If p.Properties.compressibilityFactor Is Nothing Then DW_CalcProp("compressibilityfactor", Phase.Vapor)
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Vapor)
                    Return m_pr.JT_SRK(p.Properties.compressibilityFactor.GetValueOrDefault, T, P0, RET_VMOL(Phase.Vapor), RET_VMM, RET_VZC, RET_VTC, RET_VPC,
                                      p.Properties.heatCapacityCp.GetValueOrDefault, RET_VW)
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    If p.Properties.compressibilityFactor Is Nothing Then DW_CalcProp("compressibilityfactor", Phase.Liquid1)
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid1)
                    Return m_pr.JT_SRK(p.Properties.compressibilityFactor.GetValueOrDefault, T, P0, RET_VMOL(Phase.Liquid1), RET_VMM, RET_VZC, RET_VTC, RET_VPC,
                                      p.Properties.heatCapacityCp.GetValueOrDefault, RET_VW)
                Case "Liquid2"
                    If p.Properties.compressibilityFactor Is Nothing Then DW_CalcProp("compressibilityfactor", Phase.Liquid2)
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid2)
                    Return m_pr.JT_SRK(p.Properties.compressibilityFactor.GetValueOrDefault, T, P0, RET_VMOL(Phase.Liquid2), RET_VMM, RET_VZC, RET_VTC, RET_VPC,
                                      p.Properties.heatCapacityCp.GetValueOrDefault, RET_VW)
                Case "Liquid3"
                    If p.Properties.compressibilityFactor Is Nothing Then DW_CalcProp("compressibilityfactor", Phase.Liquid3)
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Liquid3)
                    Return m_pr.JT_SRK(p.Properties.compressibilityFactor.GetValueOrDefault, T, P0, RET_VMOL(Phase.Liquid3), RET_VMM, RET_VZC, RET_VTC, RET_VPC,
                            p.Properties.heatCapacityCp.GetValueOrDefault, RET_VW)
                Case "Aqueous"
                    If p.Properties.compressibilityFactor Is Nothing Then DW_CalcProp("compressibilityfactor", Phase.Aqueous)
                    If p.Properties.heatCapacityCp Is Nothing Then DW_CalcProp("heatCapacityCp", Phase.Aqueous)
                    Return m_pr.JT_SRK(p.Properties.compressibilityFactor.GetValueOrDefault, T, P0, RET_VMOL(Phase.Aqueous), RET_VMM, RET_VZC, RET_VTC, RET_VPC,
                  p.Properties.heatCapacityCp.GetValueOrDefault, RET_VW)
                Case "Solid"
                    Return 0.0#
            End Select

        End Function


        Public Function AUX_CM(ByVal Vx As Object) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            Dim i As Integer = 0
            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val += Vx(i) * AUX_Ci(subst.ConstantProperties) * Me.m_pr.bi(0.08664, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure)
                i += 1
            Next

            Return val

        End Function

        Public Function AUX_CM(ByVal Phase As Phase) As Double

            Dim val As Double
            Dim subst As Interfaces.ICompound

            For Each subst In Me.CurrentMaterialStream.Phases(Me.RET_PHASEID(Phase)).Compounds.Values
                val += subst.MoleFraction.GetValueOrDefault * AUX_Ci(subst.ConstantProperties) * Me.m_pr.bi(0.08664, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure)
            Next

            Return val

        End Function

        Private Function AUX_Ci(cprop As Interfaces.ICompoundConstantProperties) As Double

            If cprop.SRK_Volume_Translation_Coefficient = 0.0 Then
                Select Case cprop.Name.ToLower
                    Case "nitrogen"
                        Return -0.0079
                    Case "carbon dioxide"
                        Return 0.0833
                    Case "hydrogen sulfide"
                        Return 0.0466
                    Case "methane"
                        Return 0.0234
                    Case "ethane"
                        Return 0.0605
                    Case "propane"
                        Return 0.0825
                    Case "isobutane"
                        Return 0.083
                    Case "n-butane"
                        Return 0.0975
                    Case "isopentane"
                        Return 0.1022
                    Case "n-pentane"
                        Return 0.1209
                    Case "n-hexane"
                        Return 0.1467
                    Case "n-heptane"
                        Return 0.1554
                    Case "n-octane"
                        Return 0.1794
                    Case "n-nonane"
                        Return 0.1868
                    Case "n-decane"
                        Return 0.208
                    Case Else
                        Return 0.0
                End Select
            Else
                Return cprop.SRK_Volume_Translation_Coefficient
            End If

        End Function

        Public Function RET_VS() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.SRK_Volume_Translation_Coefficient
                i += 1
            Next

            Return val

        End Function

        Public Function RET_VC() As Double()

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim subst As Interfaces.ICompound
            Dim i As Integer = 0

            For Each subst In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                val(i) = subst.ConstantProperties.SRK_Volume_Translation_Coefficient * Me.m_pr.bi(0.08664, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure)
                i += 1
            Next

            Return val

        End Function

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
            ElseIf Me.m_pr.InteractionParameters.ContainsKey(id2) Then
                If Me.m_pr.InteractionParameters(id2).ContainsKey(id1) Then
                    Return m_pr.InteractionParameters(id2)(id1).kij
                Else
                    Return 0
                End If
            End If
        End Function

        Public Overrides Function RET_VKij() As Double(,)

            If KijMatrix.Length = 0 Then

                Dim vn As String() = RET_VNAMES()
                Dim n As Integer = vn.Length - 1

                Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
                Dim i As Integer = 0
                Dim l As Integer = 0

                For i = 0 To n
                    For l = 0 To n
                        val(i, l) = Me.RET_KIJ(vn(i), vn(l))
                    Next
                Next

                Return val

            Else

                Return KijMatrix

            End If

        End Function

        Private Sub SetKijMatrix()

            Dim vn As String() = RET_VNAMES()
            Dim n As Integer = vn.Length - 1

            Dim val(Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1, Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1) As Double
            Dim i As Integer = 0
            Dim l As Integer = 0

            For i = 0 To n
                For l = 0 To n
                    val(i, l) = Me.RET_KIJ(vn(i), vn(l))
                Next
            Next

            KijMatrix = val

        End Sub

        Public Overrides Sub RunPostMaterialStreamSetRoutine()

            If AreModelParametersDirty Or KijMatrix Is Nothing OrElse KijMatrix.Length = 0 Or Not Settings.LockModelParameters Then
                SetKijMatrix()
                AreModelParametersDirty = False
            End If

        End Sub

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid1
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid2
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Liquid3
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
                Case Phase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(1)
            End Select
            Return 0.0#
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double

            Dim HM, HV, HL As Double

            HL = Me.m_pr.H_SRK_MIX("L", T, P, RET_VMOL(Phase.Liquid), RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Liquid))
            HV = Me.m_pr.H_SRK_MIX("V", T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Me.RET_Hid(298.15, T, Phase.Vapor))
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
            Return 0.0#
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
            Dim pstate As State
            Dim sstate As String = ""

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case phase
                Case Phase.Vapor
                    pstate = State.Vapor
                    sstate = "V"
                Case Phase.Liquid, Phase.Liquid1, Phase.Liquid2, Phase.Liquid3, Phase.Aqueous
                    pstate = State.Liquid
                    sstate = "L"
                Case Phase.Solid
                    pstate = State.Solid
                    sstate = "S"
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
                    result = m_pr.Z_SRK(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC, RET_VPC, RET_VW, sstate)
                    If LiquidDensity_UsePenelouxVolumeTranslation Then
                        result -= Me.AUX_CM(phase) / 8.314 / T * P
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    resultObj = Me.m_pr.CpCvR(sstate, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                Case "heatcapacitycv"
                    resultObj = Me.m_pr.CpCvR(sstate, T, P, RET_VMOL(phase), RET_VKij(), RET_VMAS(phase), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
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
                    result = Me.m_pr.H_SRK_MIX(sstate, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = result
                Case "excessentropy"
                    result = Me.m_pr.S_SRK_MIX(sstate, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), 0)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = result
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.m_pr.H_SRK_MIX(sstate, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Hid(298.15, T, phase))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.m_pr.S_SRK_MIX(sstate, T, P, RET_VMOL(phase), RET_VKij, RET_VTC(), RET_VPC(), RET_VW(), RET_VMM(), Me.RET_Sid(298.15, T, P, phase))
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                Case "viscosity"
                    If sstate = "L" Then
                        result = Me.AUX_LIQVISCm(T, P, phaseID)
                    Else
                        result = Me.AUX_VAPVISCm(T, Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault, Me.AUX_MMM(phase))
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Case "thermalconductivity"
                    If sstate = "L" Then
                        result = Me.AUX_CONDTL(T, phaseID)
                    Else
                        result = Me.AUX_CONDTG(T, P)
                    End If
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    If sstate = "L" Then
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

                If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                    Dim val As Double
                    val = m_pr.Z_SRK(T, P, RET_VMOL(Phase), RET_VKij(), RET_VTC, RET_VPC, RET_VW, "L")
                    val = (8.314 * val * T / P)
                    If LiquidDensity_UsePenelouxVolumeTranslation Then
                        val -= Me.AUX_CM(Phase)
                    End If
                    val = 1 / val * Me.AUX_MMM(dwpl) / 1000
                    result = val
                Else
                    result = Me.AUX_LIQDENS(T, P, 0.0#, phaseID, False)
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result

                result = DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Liquid)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.m_pr.Z_SRK(T, P, RET_VMOL(dwpl), RET_VKij(), RET_VTC, RET_VPC, RET_VW, "L")
                If LiquidDensity_UsePenelouxVolumeTranslation Then
                    result -= Me.AUX_CM(dwpl) / 8.314 / T * P
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                resultObj = Me.m_pr.CpCvR("L", T, P, RET_VMOL(dwpl), RET_VKij(), RET_VMAS(dwpl), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = resultObj(1)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = resultObj(2)
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
                result = DW_CalcEnthalpy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = DW_CalcEntropy(RET_VMOL(dwpl), T, P, State.Vapor)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.m_pr.Z_SRK(T, P, RET_VMOL(Phase.Vapor), RET_VKij, RET_VTC, RET_VPC, RET_VW, "V")
                If LiquidDensity_UsePenelouxVolumeTranslation Then
                    result -= Me.AUX_CM(dwpl) / 8.314 / T * P
                End If
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = Me.AUX_CPm(PropertyPackages.Phase.Vapor, T)
                resultObj = Me.m_pr.CpCvR("V", T, P, RET_VMOL(PropertyPackages.Phase.Vapor), RET_VKij(), RET_VMAS(PropertyPackages.Phase.Vapor), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())
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
            Return 0.0#
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            Return True

            'If Me.SupportedComponents.Contains(comp.ID) Then
            '    Return True
            'ElseIf comp.IsPF = 1 Then
            '    Return True
            'ElseIf comp.IsHYPO = 1 Then
            '    Return True
            'Else
            '    Return False
            'End If

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

        Public Overrides Function CalcIsothermalCompressibility(p As Interfaces.IPhase) As Double

            Dim T, P0 As Double
            T = CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case p.Name
                Case "Mixture"
                    Return 0.0#
                Case "Vapor"
                    Return ThermoPlug.CalcIsothermalCompressibility(RET_VMOL(Phase.Vapor), P0, T, Me, "SRK")
                Case "OverallLiquid"
                    Return 0.0#
                Case "Liquid1"
                    Return ThermoPlug.CalcIsothermalCompressibility(RET_VMOL(Phase.Liquid1), P0, T, Me, "SRK")
                Case "Liquid2"
                    Return ThermoPlug.CalcIsothermalCompressibility(RET_VMOL(Phase.Liquid2), P0, T, Me, "SRK")
                Case "Liquid3"
                    Return ThermoPlug.CalcIsothermalCompressibility(RET_VMOL(Phase.Liquid3), P0, T, Me, "SRK")
                Case "Aqueous"
                    Return ThermoPlug.CalcIsothermalCompressibility(RET_VMOL(Phase.Aqueous), P0, T, Me, "SRK")
                Case "Solid"
                    Return 0.0#
            End Select
        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If OverrideEnthalpyCalculation Then

                Return EnthalpyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEnthalpy", "SRK EOS Enthalpy", "SRK Enthalpy Calculation Routine")

                IObj?.SetCurrent()

                Dim H As Double

                Dim Hid As Double = Me.RET_Hid(298.15, T, Vx)

                IObj?.SetCurrent()

                If st = State.Liquid Then
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        H = Me.m_pr.H_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Hid)
                    Else
                        H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P)
                    End If
                ElseIf st = State.Vapor Then
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        H = Me.m_pr.H_SRK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Hid)
                    Else
                        H = RET_Hid_FromLiqCp(Vx, T, P)
                    End If
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        H = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties)
                    Else
                        If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                            H = Me.m_pr.H_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Hid) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
                        Else
                            H = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T)
                        End If
                    End If
                End If

                IObj?.Close()

                If Double.IsNaN(H) Then H = 0.0

                Return H

            End If

        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim H As Double

            If st = State.Liquid Then
                H = Me.m_pr.H_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Vapor Then
                H = Me.m_pr.H_SRK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Solid Then
                H = Me.m_pr.H_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
            End If

            Return H

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If OverrideEntropyCalculation Then

                Return EntropyCalculationOverride.Invoke(Vx, T, P, st, Me)

            Else

                Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEntropy", "SRK EOS Entropy", "Property Package Entropy Calculation Routine")

                IObj?.SetCurrent()

                Dim S As Double

                Dim Sid As Double = Me.RET_Sid(298.15, T, P, Vx)

                IObj?.SetCurrent()

                If st = State.Liquid Then
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        S = Me.m_pr.S_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Sid)
                    Else
                        S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                    End If
                ElseIf st = State.Vapor Then
                    If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                        S = Me.m_pr.S_SRK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Sid)
                    Else
                        S = RET_Sid_FromLiqCp(Vx, T, P)
                    End If
                ElseIf st = State.Solid Then
                    If SolidPhaseEnthalpy_UsesCp Then
                        S = CalcSolidEnthalpyFromCp(T, Vx, DW_GetConstantProperties) / T
                    Else
                        If LiquidEnthalpyEntropyCpCvCalculationMode_EOS = LiquidEnthalpyEntropyCpCvCalcMode_EOS.EOS Then
                            S = Me.m_pr.S_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, Sid) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                        Else
                            S = AUX_INT_CPDTm_L(298.15, T, Me.AUX_CONVERT_MOL_TO_MASS(Vx)) / T - RET_HFUSM(Me.AUX_CONVERT_MOL_TO_MASS(Vx), T) / T + P / 1000 / Me.AUX_LIQDENS(T, Vx, P) / T
                        End If
                    End If
                End If

                IObj?.Close()

                If Double.IsNaN(S) Then S = 0.0

                Return S

            End If

        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Dim S As Double

            If st = State.Liquid Then
                S = Me.m_pr.S_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Vapor Then
                S = Me.m_pr.S_SRK_MIX("V", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0)
            ElseIf st = State.Solid Then
                S = Me.m_pr.S_SRK_MIX("L", T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, RET_VMM, 0) - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
            End If

            Return S

        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Select Case Phase1
                Case Phase.Liquid
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Aqueous
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid1
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid2
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Liquid3
                    Return Me.m_pr.CpCvR("L", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
                Case Phase.Vapor
                    Return Me.m_pr.CpCvR("V", T, P, RET_VMOL(Phase1), RET_VKij(), RET_VMAS(Phase1), RET_VTC(), RET_VPC(), RET_VCP(T), RET_VMM(), RET_VW(), RET_VZRa())(2)
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
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Aqueous
                    key = "6"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid1
                    key = "3"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid2
                    key = "4"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Liquid3
                    key = "5"
                    If LiquidDensityCalculationMode_Subcritical = LiquidDensityCalcMode.EOS Then
                        partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "L", 0.01)
                    Else
                        partvol = New ArrayList
                        For Each subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(key).Compounds.Values
                            partvol.Add(1 / 1000 * subst.ConstantProperties.Molar_Weight / Auxiliary.PROPS.liq_dens_rackett(T, subst.ConstantProperties.Critical_Temperature, subst.ConstantProperties.Critical_Pressure, subst.ConstantProperties.Acentric_Factor, subst.ConstantProperties.Molar_Weight, subst.ConstantProperties.Z_Rackett, P, Me.AUX_PVAPi(subst.Name, T)))
                        Next
                    End If
                Case Phase.Vapor
                    partvol = Me.m_pr.CalcPartialVolume(T, P, RET_VMOL(phase), RET_VKij(), RET_VTC(), RET_VPC(), RET_VW(), RET_VTB(), "V", 0.01)
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
            val = m_pr.Z_SRK(T, P, RET_VMOL(Phase.Vapor), RET_VKij(), RET_VTC, RET_VPC, RET_VW, "V")
            val = (8.314 * val * T / P)
            If LiquidDensity_UsePenelouxVolumeTranslation Then
                val -= Me.AUX_CM(Phase.Vapor)
            End If
            val = 1 / val * Me.AUX_MMM(Phase.Vapor) / 1000
            Return val
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcFugCoeff", "SRK EOS Fugacity Coefficient", "Property Package Fugacity Coefficient Calculation Routine")

            IObj?.SetCurrent()

            Dim srkn As New PropertyPackages.ThermoPlugs.SRK

            Dim lnfug As Double()

            If st = State.Liquid Then
                lnfug = srkn.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, 0)
            Else
                lnfug = srkn.CalcLnFug(T, P, Vx, Me.RET_VKij, Me.RET_VTC, Me.RET_VPC, Me.RET_VW, Nothing, 1)
            End If

            IObj?.Close()

            Return lnfug.ExpY

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Function ReturnCriticalPoints() As ArrayList

            Dim cpc As New Utilities.TCP.Methods_SRK
            Dim i, j, k, l As Integer
            Dim n As Integer = Me.CurrentMaterialStream.Phases(0).Compounds.Count - 1
            Dim Vz(n) As Double
            Dim comp As BaseClasses.Compound
            i = 0
            For Each comp In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                Vz(i) += comp.MoleFraction.GetValueOrDefault
                i += 1
            Next
            i = 0
            Do
                If Vz(i) = 0 Then j += 1
                i = i + 1
            Loop Until i = n + 1
            Dim VTc(n), Vpc(n), Vw(n), VVc(n), VKij(n, n) As Double
            Dim Vm2(UBound(Vz) - j), VPc2(UBound(Vz) - j), VTc2(UBound(Vz) - j), VVc2(UBound(Vz) - j), Vw2(UBound(Vz) - j), VKij2(UBound(Vz) - j, UBound(Vz) - j) As Double
            VTc = Me.RET_VTC()
            Vpc = Me.RET_VPC()
            VVc = Me.RET_VVC()
            Vw = Me.RET_VW()
            VKij = Me.RET_VKij
            i = 0
            k = 0
            Do
                If Vz(i) <> 0 Then
                    Vm2(k) = Vz(i)
                    VTc2(k) = VTc(i)
                    VPc2(k) = Vpc(i)
                    VVc2(k) = VVc(i)
                    Vw2(k) = Vw(i)
                    j = 0
                    l = 0
                    Do
                        If Vz(l) <> 0 Then
                            VKij2(k, j) = VKij(i, l)
                            j = j + 1
                        End If
                        l = l + 1
                    Loop Until l = n + 1
                    k = k + 1
                End If
                i = i + 1
            Loop Until i = n + 1

            Dim TCR, PCR, VCR, real As Double

            Dim CP, CP1 As New ArrayList()

            If n > 0 Then
                CP1 = cpc.CRITPT_PR(Vm2, VTc2, VPc2, VVc2, Vw2, VKij2)
                If CP1.Count > 0 Then
                    Dim cp0 As Object() = CP1(0)
                    TCR = cp0(0)
                    PCR = cp0(1)
                    VCR = cp0(2)
                    real = 1.0#
                Else
                    TCR = Me.AUX_TCM(Phase.Mixture)
                    PCR = Me.AUX_PCM(Phase.Mixture)
                    VCR = Me.AUX_VCM(Phase.Mixture)
                    real = 0.0#
                End If
            Else
                TCR = Me.AUX_TCM(Phase.Mixture)
                PCR = Me.AUX_PCM(Phase.Mixture)
                VCR = Me.AUX_VCM(Phase.Mixture)
                real = 0.0#
            End If

            CP.Add(New Double() {TCR, PCR, VCR, real})

            Return CP

        End Function

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "AUX_Z", "Compressibility Factor", "Compressibility Factor Calculation Routine")

            IObj?.SetCurrent()

            Dim val As Double
            If state = PhaseName.Liquid Then
                val = m_pr.Z_SRK(T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, "L")
            Else
                val = m_pr.Z_SRK(T, P, Vx, RET_VKij(), RET_VTC, RET_VPC, RET_VW, "V")
            End If

            val = (8.314 * val * T / P)
            If LiquidDensity_UsePenelouxVolumeTranslation Then
                val -= Me.AUX_CM(Vx)
            End If
            val = P * val / (8.314 * T)

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Compressibility Factor: {0}", val))

            IObj?.Close()

            Return val

        End Function


    End Class

End Namespace


