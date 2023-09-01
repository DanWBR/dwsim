'    Seawater Property Package 
'    Copyright 2015 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.MathOps.MathEx
Imports System.Linq
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Serializable()> Public Class SeawaterPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Protected m_iapws97 As New IAPWS_IF97

        Protected SIA As New Seawater

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Sub New(ByVal comode As Boolean)

            MyBase.New(comode)

        End Sub

        Public Sub New()

            MyBase.New()

            Me.SupportedComponents.Add(15)

            Me.IsConfigurable = True

            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Thermal_Conductivity = "IAPWS-08 Seawater"
                .Vapor_Viscosity = "IAPWS-08 Seawater"
                .Vapor_Enthalpy_Entropy_CpCv = "IAPWS-08 Seawater"
                .Vapor_Density = "IAPWS-08 Seawater"
                .Liquid_Fugacity = "Vapor Pressure / Henry's Constant"
                .Liquid_Enthalpy_Entropy_CpCv = "IAPWS-08 Seawater"
                .Liquid_ThermalConductivity = "IAPWS-08 Seawater"
                .Liquid_Viscosity = "IAPWS-08 Seawater"
            End With

        End Sub

        Public Overrides Function GetModel() As Object
            Return SIA
        End Function

        Public Overrides Sub ConfigParameters()

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

        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            Get
                Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
                For Each su As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                    constprops.Add(su.ConstantProperties)
                Next
                FlashAlgorithm = New Auxiliary.FlashAlgorithms.Seawater With {.CompoundProperties = constprops}
                Return FlashAlgorithm
            End Get
        End Property

        Public Overrides Function AUX_IS_SINGLECOMP(Phase As Phase) As Boolean
            Return False
        End Function

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Return Me.SIA.vap_density_si(T, P)

        End Function

        Public Overrides Sub AddDefaultCompounds(compnames() As String)

            MyBase.AddDefaultCompounds(New String() {"Water", "Salt"})

        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

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

            Select Case phase
                Case Phase.Vapor

                    Select Case [property].ToLower
                        Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                            CalcAdditionalPhaseProperties(phaseID)
                        Case "compressibilityfactor"
                            result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / 18) / 8.314 / T * P
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                        Case "heatcapacity", "heatcapacitycp"
                            result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Case "heatcapacitycv"
                            result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                        Case "enthalpy", "enthalpynf"
                            result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                        Case "entropy", "entropynf"
                            result = Me.DW_CalcEntropy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                        Case "excessenthalpy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                        Case "excessentropy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                        Case "enthalpyf"
                            Dim entF As Double = Me.AUX_HFm25(phase)
                            result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                        Case "entropyf"
                            Dim entF As Double = Me.AUX_SFm25(phase)
                            result = Me.DW_CalcEntropy(RET_VMOL(Phase.Vapor), T, P, PropertyPackages.State.Vapor)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                        Case "viscosity"
                            result = Me.m_iapws97.viscW(T, P / 100000)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                        Case "thermalconductivity"
                            result = Me.m_iapws97.thconW(T, P / 100000)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                        Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                            Me.DW_CalcCompFugCoeff(phase)
                        Case "volume", "density"
                            result = Me.m_iapws97.densW(T, P / 100000)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                        Case "surfacetension"
                            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                        Case Else
                            Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
                    End Select

                Case Phase.Liquid1

                    Dim salinity As Double = CalcSalinity()

                    Select Case [property].ToLower
                        Case "isothermalcompressibility", "bulkmodulus", "joulethomsoncoefficient", "speedofsound", "internalenergy", "gibbsenergy", "helmholtzenergy"
                            CalcAdditionalPhaseProperties(phaseID)
                        Case "compressibilityfactor"
                            result = 1 / (Me.SIA.sea_density_si(salinity, T, P) * 1000 / 18) / 8.314 / T * P
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                        Case "heatcapacity", "heatcapacitycp"
                            result = Me.SIA.sea_cp_si(salinity, T, P) / 1000
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                        Case "heatcapacitycv"
                            result = Me.SIA.sea_cp_si(salinity, T, P) / 1000
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                        Case "enthalpy", "enthalpynf"
                            result = Me.DW_CalcEnthalpy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                        Case "entropy", "entropynf"
                            result = Me.DW_CalcEntropy(RET_VMOL(Phase.Liquid1), T, P, PropertyPackages.State.Liquid)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                        Case "excessenthalpy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEnthalpy = 0.0#
                        Case "excessentropy"
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.excessEntropy = 0.0#
                        Case "enthalpyf"
                            Dim entF As Double = Me.AUX_HFm25(phase)
                            result = Me.m_iapws97.enthalpyW(T, P / 100000)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                        Case "entropyf"
                            Dim entF As Double = Me.AUX_SFm25(phase)
                            result = Me.m_iapws97.entropyW(T, P / 100000)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                            result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                        Case "viscosity"
                            result = Me.SIA.sea_viscosity(salinity, T)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                        Case "thermalconductivity"
                            result = Me.SIA.sea_thermalcond(salinity, T)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                        Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                            Me.DW_CalcCompFugCoeff(phase)
                        Case "volume", "density"
                            result = Me.SIA.sea_density_si(salinity, T, P)
                            Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                        Case "surfacetension"
                            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)
                        Case Else
                            Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                            ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
                    End Select

            End Select


        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

            Dim result As Double

            Dim T, P As Double
            Dim composition As Object = Nothing
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Select Case Phase
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

            If phaseID > 0 Then

                overallmolarflow = Me.CurrentMaterialStream.Phases(0).Properties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molarflow = result
                result = result * Me.AUX_MMM(Phase) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result

                Dim xl1 = Me.CurrentMaterialStream.Phases(3).Properties.molarfraction.GetValueOrDefault
                Dim xl2 = Me.CurrentMaterialStream.Phases(4).Properties.molarfraction.GetValueOrDefault
                Dim xv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
                Dim xs = Me.CurrentMaterialStream.Phases(7).Properties.molarfraction.GetValueOrDefault

                Dim Vx1 = Me.CurrentMaterialStream.GetPhaseComposition(3)
                Dim Vx2 = Me.CurrentMaterialStream.GetPhaseComposition(4)
                Dim Vv = Me.CurrentMaterialStream.GetPhaseComposition(2)
                Dim Vs = Me.CurrentMaterialStream.GetPhaseComposition(7)

                Dim Vp = Me.CurrentMaterialStream.GetPhaseComposition(phaseID)
                Dim xp = Me.CurrentMaterialStream.Phases(phaseID).Properties.molarfraction.GetValueOrDefault

                Dim wp = xp * AUX_MMM(Vp) / (xl1 * AUX_MMM(Vx1) + xl2 * AUX_MMM(Vx2) + xv * AUX_MMM(Vv) + xs * AUX_MMM(Vs))

                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = wp
                Me.DW_CalcCompVolFlow(phaseID)
                Me.DW_CalcCompFugCoeff(Phase)

            End If

            Dim Tsat As Double = Me.m_iapws97.tSatW(P / 100000)

            If phaseID = 2 Then

                result = Me.m_iapws97.densW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                result = Me.m_iapws97.enthalpyW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.m_iapws97.entropyW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / 18) / 8.314 / T * P
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = 18
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                result = Me.m_iapws97.thconW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                result = Me.m_iapws97.viscW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

            ElseIf phaseID = 3 Then

                Dim salinity As Double = CalcSalinity()

                Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = Me.AUX_MMM(PropertyPackages.Phase.Liquid1)
                result = 1 / (Me.SIA.sea_density_si(salinity, T, P) * 1000 / 18) / 8.314 / T * P
                Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                result = Me.SIA.sea_cp_si(salinity, T, P) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                result = Me.SIA.sea_cp_si(salinity, T, P) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                result = Me.SIA.sea_enthalpy_si(salinity, T, P) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                result = Me.SIA.sea_entropy_si(salinity, T, P) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                Dim entF As Double = Me.AUX_HFm25(PropertyPackages.Phase.Liquid1)
                result = Me.m_iapws97.enthalpyW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF = result + entF
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpyF = result
                entF = Me.AUX_SFm25(PropertyPackages.Phase.Liquid1)
                result = Me.m_iapws97.entropyW(T, P / 100000)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF = result + entF
                result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropyF = result
                result = Me.SIA.sea_viscosity(salinity, T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                result = Me.SIA.sea_thermalcond(salinity, T)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                Me.DW_CalcCompFugCoeff(PropertyPackages.Phase.Liquid1)
                result = Me.SIA.sea_density_si(salinity, T, P)
                Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity.GetValueOrDefault / result
                Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = Me.AUX_SURFTM(T)

            ElseIf phaseID = 1 Then

                DW_CalcLiqMixtureProps()

            Else

                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(Phase) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

        End Sub

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal Phase1 As PropertyPackages.Phase, ByVal Phase2 As PropertyPackages.Phase)

            Dim result As Double

            Dim T, P As Double
            Dim composition1 As Object = Nothing
            Dim composition2 As Object = Nothing

            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            result = 1
            Me.CurrentMaterialStream.Phases(0).Properties.kvalue = result
            result = 0
            Me.CurrentMaterialStream.Phases(0).Properties.logKvalue = result

            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = DW_CalcTensaoSuperficial_ISOL(Phase.Liquid1, T, P)

        End Sub

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.m_iapws97.densW(T, P / 100000)
            ElseIf Phase1 = Phase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.densSatVapTW(T)
                Else
                    Return Me.m_iapws97.densW(T, P / 100000)
                End If
            ElseIf Phase1 = Phase.Mixture Then
                Return Me.m_iapws97.densW(T, P / 100000)
            End If
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return SIA.sea_surfacetension(CalcSalinity, T)
        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

            Dim salinity As Double = salt.MassFraction.GetValueOrDefault / water.MassFraction.GetValueOrDefault

            If Phase1 = Phase.Liquid Then
                Return Me.SIA.sea_viscosity(salinity, T)
            ElseIf Phase1 = Phase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.viscSatVapTW(T)
                Else
                    Return Me.m_iapws97.viscW(T, P / 100000)
                End If
            End If
        End Function

        Public Overrides Function DW_CalcEnergyFlowMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double
            Dim ent_massica = Me.m_iapws97.enthalpyW(T, P / 100000)
            Dim flow = Me.CurrentMaterialStream.Phases(0).Properties.massflow
            Return ent_massica * flow
        End Function

        Public Overrides Function DW_CalcCp_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.SIA.sea_cp_si(CalcSalinity, T, P)
        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.m_iapws97.thconW(T, P / 100000)
            ElseIf Phase1 = Phase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.thconSatVapTW(T)
                Else
                    Return Me.m_iapws97.thconW(T, P / 100000)
                End If
            End If
        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            Return 18
        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

            Dim salinity As Double = salt.MassFraction.GetValueOrDefault / water.MassFraction.GetValueOrDefault

            Return Me.SIA.sea_vaporpressure(salinity, T)

        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            Else
                Return False
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            If DirectCast(Vx, Double()).Sum > 0.0# Then
                Select Case st
                    Case State.Solid
                        Return Me.SIA.sea_enthalpy_si(CalcSalinity(Vx), T, P) / 1000 - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
                    Case State.Liquid
                        Return Me.SIA.sea_enthalpy_si(CalcSalinity(Vx), T, P) / 1000
                    Case State.Vapor
                        Return Me.SIA.sea_enthalpy_si(CalcSalinity(Vx), T, P) / 1000 + Me.RET_HVAPM(AUX_CONVERT_MOL_TO_MASS(Vx), T)
                End Select
            Else
                Return 0.0#
            End If

        End Function

        Public Overrides Function DW_CalcKvalue(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double) As Double()
            Return New Double() {1.0#, 1.0#}
        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return Me.DW_CalcEnthalpy(Vx, T, P, st) - Me.RET_Hid(298.15, T, Vx)
        End Function

        Public Overrides Function DW_CalcBubP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.SIA.sea_vaporpressure(CalcSalinity(Vx), T)}
        End Function

        Public Overrides Function DW_CalcBubT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Return New Object() {Me.AUX_TSATi(P, water.Name)}
        End Function

        Public Overrides Function DW_CalcDewP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.SIA.sea_vaporpressure(CalcSalinity(Vx), T)}
        End Function

        Public Overrides Function DW_CalcDewT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Return New Object() {Me.AUX_TSATi(P, water.Name)}
        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.SIA.sea_cp_si(CalcSalinity, T, P)
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

        End Sub

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            If DirectCast(Vx, Double()).Sum > 0.0# Then
                Select Case st
                    Case State.Solid
                        Return Me.SIA.sea_entropy_si(CalcSalinity(Vx), T, P) / 1000 - Me.RET_HFUSM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                    Case State.Liquid
                        Return Me.SIA.sea_entropy_si(CalcSalinity, T, P) / 1000
                    Case State.Vapor
                        Return Me.SIA.sea_entropy_si(CalcSalinity(Vx), T, P) / 1000 + Me.RET_HVAPM(AUX_CONVERT_MOL_TO_MASS(Vx), T) / T
                End Select
            Else
                Return 0.0#
            End If
        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Return DW_CalcEntropy(Vx, T, P, st) - Me.RET_Sid(298.15, T, P, Vx)

        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            Dim constprops As New List(Of Interfaces.ICompoundConstantProperties)
            For Each s As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                constprops.Add(s.ConstantProperties)
            Next

            Dim n As Integer = Vx.Length - 1
            Dim i, j As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then

                Dim Tc As Double() = Me.RET_VTC()

                For i = 0 To n

                    If Tc(i) = 0.0# Then

                        Dim wtotal As Double = 0
                        Dim mtotal As Double = 0
                        Dim molality(n) As Double

                        For j = 0 To n
                            If constprops(j).Name = "Water" Then
                                wtotal += Vx(j) * constprops(j).Molar_Weight / 1000
                            End If
                            mtotal += Vx(j)
                        Next

                        Dim Xsolv As Double = 1

                        For j = 0 To n
                            molality(i) = Vx(i) / wtotal
                        Next

                        fugcoeff(i) = molality(i) * 0.665 'salt activity coefficient

                    ElseIf T / Tc(i) >= 1 Then
                        fugcoeff(i) = AUX_KHenry(Me.RET_VNAMES(i), T) / P
                    Else
                        fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                    End If

                Next

            Else
                For i = 0 To n
                    fugcoeff(i) = 1.0#
                Next
            End If

            Calculator.WriteToConsole("Result: " & fugcoeff.ToArrayString(), 2)

            Return fugcoeff

        End Function

        Public Overrides Function AUX_PVAPi(sub1 As String, T As Double) As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water.Name = sub1 Then

                Return Me.SIA.sea_vaporpressure(CalcSalinity, T)

            ElseIf salt.Name = sub1 Then

                Return 0.0#

            Else

                Return MyBase.AUX_PVAPi(sub1, T)

            End If

        End Function

        Public Function VaporPressure(Vx As Double(), T As Double) As Double

            If Vx.Sum > 0.0000000002 Then
                Return Me.SIA.sea_vaporpressure(CalcSalinity(Vx), T)
            Else
                Return Me.m_iapws97.pSatW(T) * 100000
            End If

        End Function

        Public Function SaturationTemperature(Vx As Double(), P As Double) As Double

            Dim pvapt As Double
            Return MathNet.Numerics.RootFinding.Brent.FindRoot(Function(T)
                                                                   pvapt = VaporPressure(Vx, T)
                                                                   If Double.IsNaN(pvapt) Or Double.IsNaN(P) Then
                                                                       Throw New Exception(String.Format("Error calculation vapor pressure."))
                                                                   End If
                                                                   Return pvapt - P
                                                               End Function, 273.15, 1000)

        End Function

        Public Function CalcSalinity(Vx As Double()) As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

            Dim idw As Integer = 0
            Dim ids As Integer = 0

            Dim i As Integer = 0
            For Each s As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If s.Name = water.Name Then idw = i
                If s.Name = salt.Name Then ids = i
                i += 1
            Next

            Dim vxw As Double() = Me.AUX_CONVERT_MOL_TO_MASS(Vx)

            Dim salinity As Double = vxw(ids) / vxw(idw)

            If Double.IsInfinity(salinity) Then salinity = 0.0#

            If Not IgnoreSalinityLimit Then
                If salinity > Seawater.sal_smax Then
                    'If Me.CurrentMaterialStream.Flowsheet IsNot Nothing Then
                    '    Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & "/" & New StackFrame(1).GetMethod.Name & "(): maximum salinity exceeded (" & Format(salinity, "0.00") & " kg/kg). Using upper limit value (" & Format(Seawater.sal_smax, "0.00") & " kg/kg).", Interfaces.IFlowsheet.MessageType.Warning)
                    'End If
                    salinity = Seawater.sal_smax
                End If
            End If


            Return salinity

        End Function

        Public Function CalcSalinity() As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault
            Dim salt As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.Name = "Salt").SingleOrDefault

            If water Is Nothing Then Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            If salt Is Nothing Then Throw New Exception("Salt compound not found. Please setup your simulation accordingly.")

            Dim salinity As Double = salt.MassFraction.GetValueOrDefault / water.MassFraction.GetValueOrDefault

            If Double.IsInfinity(salinity) Then salinity = 0.0#

            If Not IgnoreSalinityLimit Then
                If salinity > Seawater.sal_smax Then
                    If Me.CurrentMaterialStream.Flowsheet IsNot Nothing Then
                        Me.CurrentMaterialStream.Flowsheet.ShowMessage(Me.ComponentName & "/" & New StackFrame(1).GetMethod.Name & "(): maximum salinity exceeded (" & Format(salinity, "0.00") & " kg/kg). Using upper limit value (" & Format(Seawater.sal_smax, "0.00") & " kg/kg).", Interfaces.IFlowsheet.MessageType.Warning)
                    End If
                    salinity = Seawater.sal_smax
                End If
            End If

            Return salinity

        End Function

        Function TemperatureOfFusion(Vxl As Double(), T As Double) As Double

            Dim Tnfp, DHm, DT, Td As Double

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            Tnfp = 273.15
            DHm = 6.00174

            Dim idw As Integer = 0

            Dim i As Integer = 0
            For Each s As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values
                If s.Name = water.Name Then idw = i
                i += 1
            Next

            DT = 0.00831447 * Tnfp ^ 2 / DHm * Math.Log(Vxl(idw))
            Td = Tnfp - DT

            Return Td

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Return 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / 18) / 8.314 / T * P

        End Function

    End Class

End Namespace