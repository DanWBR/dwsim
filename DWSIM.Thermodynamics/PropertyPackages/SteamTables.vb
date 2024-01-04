'    Steam Tables Property Package 
'    Copyright 2008-2014 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.MathOps.MathEx
Imports DWSIM.Interfaces.Enums

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(SteamTablesPropertyPackage.ClassId)>
    <System.Serializable()> Public Class SteamTablesPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "170D6E8A-8880-4bf9-B7A0-E4A3FDBFD589"

        Friend m_iapws97 As New IAPWS_IF97

        Public Overrides ReadOnly Property Popular As Boolean = True

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            Me.SupportedComponents.Add(15)
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

            IsConfigurable = False

            With PropertyMethodsInfo
                .Vapor_Fugacity = "Ideal"
                .Vapor_Thermal_Conductivity = "IAPWS-IF97 Steam Tables"
                .Vapor_Viscosity = "IAPWS-IF97 Steam Tables"
                .Vapor_Enthalpy_Entropy_CpCv = "IAPWS-IF97 Steam Tables"
                .Vapor_Density = "IAPWS-IF97 Steam Tables"
                .Liquid_Fugacity = "Vapor Pressure / Henry's Constant"
                .Liquid_Enthalpy_Entropy_CpCv = "IAPWS-IF97 Steam Tables"
                .Liquid_ThermalConductivity = "IAPWS-IF97 Steam Tables"
                .Liquid_Viscosity = "IAPWS-IF97 Steam Tables"
            End With

        End Sub
        Public Overrides Function GetModel() As Object
            Return m_iapws97
        End Function

        Public Overrides Sub AddDefaultCompounds(compnames() As String)

            MyBase.AddDefaultCompounds(New String() {"Water"})

        End Sub

        Public Overrides Sub RunPostMaterialStreamSetRoutine()

            If Not CurrentMaterialStream.Phases(0).Compounds.Keys.Contains("Water") Then
                Throw New Exception("Steam Tables Property Package is meant to be used with Water streams only.")
            End If

        End Sub

        Private Sub CheckStream()

            If Not CurrentMaterialStream.Phases(0).Compounds.Keys.Contains("Water") Then
                Throw New Exception("Steam Tables Property Package is meant to be used with Water streams only.")
            Else
                If Not CurrentMaterialStream.Phases(0).Compounds("Water").MoleFraction.GetValueOrDefault() > 0.99 Then
                    Throw New Exception("Stream has Water but it is not the only compound with a significant amount.")
                End If
            End If

        End Sub




        Public Overrides ReadOnly Property FlashBase() As Auxiliary.FlashAlgorithms.FlashAlgorithm
            Get
                If CurrentMaterialStream IsNot Nothing Then
                    Select Case CurrentMaterialStream.ForcePhase
                        Case ForcedPhase.None
                            Return New Auxiliary.FlashAlgorithms.SteamTables
                        Case ForcedPhase.GlobalDef
                            If CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase = ForcedPhase.None Then
                                Return New Auxiliary.FlashAlgorithms.SteamTables
                            Else
                                Return New Auxiliary.FlashAlgorithms.ForcedPhaseFlash() With {.ForcePhase = CurrentMaterialStream.Flowsheet.FlowsheetOptions.ForceStreamPhase}
                            End If
                        Case Else
                            Return New Auxiliary.FlashAlgorithms.ForcedPhaseFlash() With {.ForcePhase = CurrentMaterialStream.ForcePhase}
                    End Select
                Else
                    Return New Auxiliary.FlashAlgorithms.SteamTables
                End If
            End Get
        End Property

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Dim Tsat As Double = m_iapws97.tSatW(P / 100000)
            If T < Tsat Then
                Return Me.m_iapws97.densSatVapTW(T)
            Else
                Return Me.m_iapws97.densW(T, P / 100000)
            End If

        End Function

        Public Overrides Sub DW_CalcEquilibrium(ByVal spec1 As FlashSpec, ByVal spec2 As FlashSpec)

            CheckStream()

            If ShouldBypassEquilibriumCalculation() Then
                MyBase.DW_CalcEquilibrium(spec1, spec2)
                Exit Sub
            End If

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEquilibrium", ComponentName & " (Phase Equilibria)", "Property Package Equilibrium Calculation Routine")

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

            Dim wkey As String = water.Name

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim P, T, H, S, vf, lf, Psat, Hv, Hl, Sv, Sl As Double

            Dim brentsolverP As New BrentOpt.Brent
            brentsolverP.DefineFuncDelegate(AddressOf EnthalpyPx)

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf EnthalpyTx)

            'for TVF/PVF/PH/PS flashes
            H = Me.CurrentMaterialStream.Phases(0).Properties.enthalpy.GetValueOrDefault
            S = Me.CurrentMaterialStream.Phases(0).Properties.entropy.GetValueOrDefault
            vf = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault

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

            Select Case spec1

                Case FlashSpec.T

                    Select Case spec2

                        Case FlashSpec.P

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            With Me.m_iapws97
                                Psat = .pSatW(T)
                                If T > 273.15 And Psat = -1 Then Psat = 1.0E+20
                                If P / 100000 > Psat Then
                                    vf = 0
                                Else
                                    vf = 1
                                End If
                                H = .enthalpyW(T, P / 100000)
                                S = .entropyW(T, P / 100000)
                            End With
                            lf = 1 - vf

                        Case FlashSpec.H

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

                            With Me.m_iapws97
                                Hl = .enthalpySatLiqTW(T)
                                Hv = .enthalpySatVapTW(T)
                                Sl = .entropySatLiqTW(T)
                                Sv = .entropySatVapTW(T)
                                If H < Hl Then
                                    vf = 0
                                ElseIf H > Hv Then
                                    vf = 1
                                Else
                                    vf = (H - Hl) / (Hv - Hl)
                                End If
                                S = vf * Sv + (1 - vf) * Sl
                                P = .pSatW(T) * 100000

                                If vf <> 0 And vf <> 1 Then
                                    P = .pSatW(T)
                                Else
                                    LoopVarF = H
                                    LoopVarX = T
                                    P = brentsolverP.BrentOpt(0.001, 2000, 100, 0.0001, 1000, Nothing)
                                End If
                                P = P * 100000

                            End With
                            lf = 1 - vf

                        Case FlashSpec.S

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

                            With Me.m_iapws97
                                Hl = .enthalpySatLiqTW(T)
                                Hv = .enthalpySatVapTW(T)
                                Sl = .entropySatLiqTW(T)
                                Sv = .entropySatVapTW(T)
                                If S < Sl Then
                                    vf = 0
                                ElseIf S > Sv Then
                                    vf = 1
                                Else
                                    vf = (S - Sl) / (Sv - Sl)
                                End If
                                H = vf * Hv + (1 - vf) * Hl

                                If vf <> 0 And vf <> 1 Then
                                    P = .pSatW(T)
                                Else
                                    LoopVarF = H
                                    LoopVarX = T
                                    P = brentsolverP.BrentOpt(0.001, 2000, 100, 0.0001, 1000, Nothing)
                                End If
                                P = P * 100000

                            End With
                            lf = 1 - vf

                        Case FlashSpec.VAP

                            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault

                            With Me.m_iapws97
                                Hl = .enthalpySatLiqTW(T)
                                Hv = .enthalpySatVapTW(T)
                                Sl = .entropySatLiqTW(T)
                                Sv = .entropySatVapTW(T)
                                H = vf * Hv + (1 - vf) * Hl
                                S = vf * Sv + (1 - vf) * Sl
                                P = .pSatW(T) * 100000
                            End With
                            lf = 1 - vf

                    End Select

                Case FlashSpec.P

                    Select Case spec2

                        Case FlashSpec.H

                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            With Me.m_iapws97

                                Dim Tsat = .tSatW(P / 100000)
                                Dim Tcrit = 647.1

                                If Tsat > Tcrit Then

                                    'supercritical

                                    vf = 1.0

                                    T = brentsolverT.BrentOpt2(273.15, 2000, 5, 0.001, 100, Function(Tx)

                                                                                                Return H - .enthalpyW(Tx, P / 100000)

                                                                                            End Function)

                                    Dim dens = .densW(T, P / 100000)

                                    If dens > 322.0 Then
                                        vf = 0.0
                                    Else
                                        vf = 1.0
                                    End If

                                    S = .entropyW(T, P / 100000)

                                Else

                                    Hl = .enthalpySatLiqPW(P / 100000)
                                    Hv = .enthalpySatVapPW(P / 100000)
                                    Sl = .entropySatLiqPW(P / 100000)
                                    Sv = .entropySatVapPW(P / 100000)
                                    If H < Hl Then
                                        vf = 0
                                    ElseIf H > Hv Then
                                        vf = 1
                                    Else
                                        vf = (H - Hl) / (Hv - Hl)
                                    End If
                                    S = vf * Sv + (1 - vf) * Sl

                                    If vf <> 0 And vf <> 1 Then
                                        T = .tSatW(P / 100000)
                                    Else
                                        LoopVarF = H
                                        LoopVarX = P / 100000
                                        T = brentsolverT.BrentOpt(273.15, 2000, 100, 0.0001, 1000, Nothing)
                                    End If

                                End If

                            End With

                            lf = 1 - vf

                        Case FlashSpec.S

                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            With Me.m_iapws97

                                Dim Tsat = .tSatW(P / 100000)
                                Dim Tcrit = 647.1

                                If Tsat > Tcrit Then

                                    'supercritical

                                    T = brentsolverT.BrentOpt2(273.15, 2000, 5, 0.001, 100, Function(Tx)

                                                                                                Return S - .entropyW(Tx, P / 100000)

                                                                                            End Function)

                                    Dim dens = .densW(T, P / 100000)

                                    If dens > 322.0 Then
                                        vf = 0.0
                                    Else
                                        vf = 1.0
                                    End If

                                    H = .enthalpyW(T, P / 100000)

                                Else

                                    Hl = .enthalpySatLiqPW(P / 100000)
                                    Hv = .enthalpySatVapPW(P / 100000)
                                    Sl = .entropySatLiqPW(P / 100000)
                                    Sv = .entropySatVapPW(P / 100000)
                                    If S < Sl Then
                                        vf = 0
                                    ElseIf S > Sv Then
                                        vf = 1
                                    Else
                                        vf = (S - Sl) / (Sv - Sl)
                                    End If
                                    H = vf * Hv + (1 - vf) * Hl

                                    If vf <> 0 And vf <> 1 Then
                                        T = .tSatW(P / 100000)
                                    Else
                                        LoopVarF = H
                                        LoopVarX = P / 100000
                                        T = brentsolverT.BrentOpt(273.15, 2000, 100, 0.0001, 1000, Nothing)
                                    End If

                                End If

                            End With

                            lf = 1 - vf

                        Case FlashSpec.VAP

                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            With Me.m_iapws97
                                Hl = .enthalpySatLiqPW(P / 100000)
                                Hv = .enthalpySatVapPW(P / 100000)
                                Sl = .entropySatLiqPW(P / 100000)
                                Sv = .entropySatVapPW(P / 100000)
                                H = vf * Hv + (1 - vf) * Hl
                                S = vf * Sv + (1 - vf) * Sl
                                T = .tSatW(P / 100000)
                            End With
                            lf = 1 - vf

                    End Select

            End Select

FINAL:

            With Me.CurrentMaterialStream
                .Phases(0).Properties.temperature = T
                .Phases(0).Properties.pressure = P
                .Phases(0).Properties.enthalpy = H
                .Phases(0).Properties.entropy = S
                .Phases(0).Properties.molarfraction = 1
                .Phases(3).Properties.molarfraction = lf
                .Phases(2).Properties.molarfraction = vf
                .Phases(0).Properties.massfraction = 1
                .Phases(3).Properties.massfraction = lf
                .Phases(2).Properties.massfraction = vf
                .Phases(0).Compounds(wkey).MoleFraction = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).MoleFraction = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FugacityCoeff = 1
                If lf = 0 Then .Phases(3).Compounds(wkey).MoleFraction = 0
                If vf > 0 Then .Phases(2).Compounds(wkey).MoleFraction = 1
                If vf > 0 Then .Phases(2).Compounds(wkey).FugacityCoeff = 1
                If vf = 0 Then .Phases(2).Compounds(wkey).MoleFraction = 0
                .Phases(0).Compounds(wkey).MassFraction = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).MassFraction = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FugacityCoeff = 1
                If lf = 0 Then .Phases(3).Compounds(wkey).MassFraction = 0
                If vf > 0 Then .Phases(2).Compounds(wkey).MassFraction = 1
                If vf > 0 Then .Phases(2).Compounds(wkey).FugacityCoeff = 1
                If vf = 0 Then .Phases(2).Compounds(wkey).MassFraction = 0

                If lf = 0 Then
                    With .Phases(3).Properties
                        .activity = 0
                        .activityCoefficient = 0
                        .compressibility = 0
                        .compressibilityFactor = 0
                        .density = 0
                        .enthalpy = 0
                        .entropy = 0
                        .excessEnthalpy = 0
                        .excessEntropy = 0
                        .fugacity = 0
                        .fugacityCoefficient = 0
                        .heatCapacityCp = 0
                        .heatCapacityCv = 0
                        .jouleThomsonCoefficient = 0
                        .kinematic_viscosity = 0
                        .logFugacityCoefficient = 0
                        .massflow = 0
                        .massfraction = 0
                        .molarflow = 0
                        .molarfraction = 0
                        .molecularWeight = 0
                        .pressure = 0
                        .speedOfSound = 0
                        .temperature = 0
                        .thermalConductivity = 0
                        .viscosity = 0
                        .volumetric_flow = 0
                    End With
                ElseIf vf = 0 Then
                    With .Phases(2).Properties
                        .activity = 0
                        .activityCoefficient = 0
                        .compressibility = 0
                        .compressibilityFactor = 0
                        .density = 0
                        .enthalpy = 0
                        .entropy = 0
                        .excessEnthalpy = 0
                        .excessEntropy = 0
                        .fugacity = 0
                        .fugacityCoefficient = 0
                        .heatCapacityCp = 0
                        .heatCapacityCv = 0
                        .jouleThomsonCoefficient = 0
                        .kinematic_viscosity = 0
                        .logFugacityCoefficient = 0
                        .massflow = 0
                        .massfraction = 0
                        .molarflow = 0
                        .molarfraction = 0
                        .molecularWeight = 0
                        .pressure = 0
                        .speedOfSound = 0
                        .temperature = 0
                        .thermalConductivity = 0
                        .viscosity = 0
                        .volumetric_flow = 0
                    End With
                End If

            End With

            Me.CurrentMaterialStream.AtEquilibrium = True

            IObj?.Close()

        End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim water As Interfaces.ICompound = (From subst As Interfaces.ICompound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

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
                    result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                Case "heatcapacitycv"
                    result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.m_iapws97.enthalpyW(T, P / 100000)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.m_iapws97.entropyW(T, P / 100000)
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

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

            CheckStream()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()
            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcPhaseProps", ComponentName & String.Format(" (Phase Properties - {0})", [Enum].GetName(Phase.GetType, Phase)), "Property Package Phase Properties Calculation Routine")

            IObj?.Paragraphs.Add("This is the routine responsible for the calculation of phase properties of the currently associated Material Stream.")

            IObj?.Paragraphs.Add("Specified Phase: " & [Enum].GetName(Phase.GetType, Phase))

            Dim result As Double

            Dim T, Tsat, P As Double
            Dim composition As Object = Nothing
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Phases(0).Properties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

            Tsat = m_iapws97.tSatW(P / 100000)

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
                result = result * Me.AUX_MMM(PropertyPackages.Phase.Mixture) / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
            End If

            If phaseID = 3 Then

                If Math.Abs(T - Tsat) < 0.01 Then

                    result = Me.m_iapws97.densSatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatLiqTW(T) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = Me.m_iapws97.thconSatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = Me.m_iapws97.viscSatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                Else

                    result = Me.m_iapws97.densW(T, P / 100000)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Liquid)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
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

                End If

            ElseIf phaseID = 2 Then

                If Math.Abs(T - Tsat) < 0.01 Then

                    result = Me.m_iapws97.densSatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatVapTW(T) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Phases(phaseID).Properties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.molar_entropy = result
                    result = Me.m_iapws97.thconSatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.thermalConductivity = result
                    result = Me.m_iapws97.viscSatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.viscosity = result
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.kinematic_viscosity = result / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.Value

                Else

                    result = Me.m_iapws97.densW(T, P / 100000)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = DW_CalcEnthalpy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = DW_CalcEntropy(RET_VMOL(Phase), T, P, State.Vapor)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = Me.AUX_MMM(PropertyPackages.Phase.Mixture)
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

                End If

            ElseIf phaseID = 1 Then

                IObj?.SetCurrent
                DW_CalcLiqMixtureProps()

            Else

                IObj?.SetCurrent
                DW_CalcOverallProps()

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * Me.AUX_MMM(PropertyPackages.Phase.Mixture) / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Phases(phaseID).Properties.volumetric_flow = result
            End If

            IObj?.Close()

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
            Dim Tr = T / 647.13
            result = 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
            Me.CurrentMaterialStream.Phases(0).Properties.surfaceTension = result

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
            Dim Tr = T / 647.13
            Return 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal Phase1 As PropertyPackages.Phase, ByVal T As Double, ByVal P As Double) As Double
            If Phase1 = Phase.Liquid Then
                Return Me.m_iapws97.viscW(T, P / 100000)
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
            Return Me.m_iapws97.cpW(T, P / 10000)
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
            Return Me.AUX_MMM(PropertyPackages.Phase.Mixture)
        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Me.m_iapws97.pSatW(T) * 100000
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Interfaces.ICompoundConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            Else
                Return False
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "DW_CalcEnthalpy", "Steam Tables Enthalpy", "Property Package Enthalpy Calculation Routine")

            IObj?.SetCurrent()

            Dim Tsat As Double = m_iapws97.tSatW(P / 100000)
            Dim Tcrit As Double = 374.0 + 273.15
            'If Tsat > Tcrit Then Throw New Exception(String.Format("Steam Tables Enthalpy calculation error: calculated Tsat ({0} K) > Tcrit ({1} K) @ P = {2} Pa", Tsat, Tcrit, P))
            Dim Tbound = 1073.15
            Dim Tmin = 273.15
            Select Case st
                Case State.Liquid
                    If T > Tsat Then
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.2
                        x2 = Tmin + (Tsat - Tmin) * 0.4
                        x3 = Tmin + (Tsat - Tmin) * 0.6
                        x4 = Tmin + (Tsat - Tmin) * 0.8
                        x5 = Tmin + (Tsat - Tmin) * 0.9
                        p1 = Me.m_iapws97.enthalpySatLiqTW(x1)
                        p2 = Me.m_iapws97.enthalpySatLiqTW(x2)
                        p3 = Me.m_iapws97.enthalpySatLiqTW(x3)
                        p4 = Me.m_iapws97.enthalpySatLiqTW(x4)
                        p5 = Me.m_iapws97.enthalpySatLiqTW(x5)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return Me.m_iapws97.enthalpySatLiqTW(T)
                    Else
                        Return Me.m_iapws97.enthalpyW(T, P / 100000)
                    End If
                Case State.Vapor
                    If T > Tbound Then
                        CurrentMaterialStream.Flowsheet.ShowMessage(String.Format("Steam Tables Enthalpy Calculation: Temperature outside valid range ({0} > {1}), extrapolating calculated values...", T, Tbound), Interfaces.IFlowsheet.MessageType.Warning)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tcrit + (Tbound - Tcrit) * 0.2
                        x2 = Tcrit + (Tbound - Tcrit) * 0.4
                        x3 = Tcrit + (Tbound - Tcrit) * 0.6
                        x4 = Tcrit + (Tbound - Tcrit) * 0.8
                        x5 = Tcrit + (Tbound - Tcrit) * 0.9
                        p1 = Me.m_iapws97.enthalpyW(x1, P / 100000)
                        p2 = Me.m_iapws97.enthalpyW(x2, P / 100000)
                        p3 = Me.m_iapws97.enthalpyW(x3, P / 100000)
                        p4 = Me.m_iapws97.enthalpyW(x4, P / 100000)
                        p5 = Me.m_iapws97.enthalpyW(x5, P / 100000)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return Me.m_iapws97.enthalpySatVapTW(T)
                    ElseIf T > Tsat Then
                        Return Me.m_iapws97.enthalpyW(T, P / 100000)
                    Else
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tsat + (Tcrit - Tsat) * 0.2
                        x2 = Tsat + (Tcrit - Tsat) * 0.4
                        x3 = Tsat + (Tcrit - Tsat) * 0.6
                        x4 = Tsat + (Tcrit - Tsat) * 0.8
                        x5 = Tsat + (Tcrit - Tsat) * 0.9
                        p1 = Me.m_iapws97.enthalpySatVapTW(x1)
                        p2 = Me.m_iapws97.enthalpySatVapTW(x2)
                        p3 = Me.m_iapws97.enthalpySatVapTW(x3)
                        p4 = Me.m_iapws97.enthalpySatVapTW(x4)
                        p5 = Me.m_iapws97.enthalpySatVapTW(x5)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    End If
                Case Else
                    Return Me.m_iapws97.enthalpyW(T, P / 100000)
            End Select
        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return Me.m_iapws97.enthalpyW(T, P / 100000) - Me.RET_Hid(298.15, T, Vx)
        End Function

        Public Overrides Function DW_CalcBubP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.m_iapws97.pSatW(T) * 1.001}
        End Function

        Public Overrides Function DW_CalcBubT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.m_iapws97.tSatW(P / 100000) * 0.999}
        End Function

        Public Overrides Function DW_CalcDewP(ByVal Vx As System.Array, ByVal T As Double, Optional ByVal Pref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.m_iapws97.pSatW(T) * 0.999}
        End Function

        Public Overrides Function DW_CalcDewT(ByVal Vx As System.Array, ByVal P As Double, Optional ByVal Tref As Double = 0, Optional ByVal K As System.Array = Nothing, Optional ByVal ReuseK As Boolean = False) As Object
            Return New Object() {Me.m_iapws97.tSatW(P / 100000) * 1.001}
        End Function

        Public Overrides Function DW_CalcCv_ISOL(ByVal Phase1 As Phase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.m_iapws97.cvW(T, P / 100000)
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Phase, ByVal T As Double, ByVal P As Double)

        End Sub

        Friend LoopVarF As Double = 0
        Friend LoopVarX As Double = 0

        Public Function EnthalpyTx(ByVal x As Double, ByVal otherargs As Object) As Double

            Dim er As Double = LoopVarF - Me.m_iapws97.enthalpyW(x, LoopVarX)
            Return er

        End Function

        Public Function EntropyTx(ByVal x As Double, ByVal otherargs As Object) As Double

            Dim er As Double = LoopVarF - Me.m_iapws97.entropyW(x, LoopVarX)
            Return er

        End Function

        Public Function EnthalpyPx(ByVal x As Double, ByVal otherargs As Object) As Double

            Return LoopVarF - Me.m_iapws97.enthalpyW(LoopVarX, x)

        End Function

        Public Function EntropyPx(ByVal x As Double, ByVal otherargs As Object) As Double

            Dim er As Double = LoopVarF - Me.m_iapws97.entropyW(x, LoopVarX)
            Return er

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double

            Dim Tsat As Double = m_iapws97.tSatW(P / 100000)
            Dim Tcrit As Double = 374.0 + 273.15
            'If Tsat > Tcrit Then Throw New Exception(String.Format("Steam Tables Entropy calculation error: calculated Tsat ({0} K) > Tcrit ({1} K) @ P = {2} Pa", Tsat, Tcrit, P))
            Dim Tmin As Double = 273.15
            Dim Tbound As Double = 1073.15
            Select Case st
                Case State.Liquid
                    If T > Tsat Then
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tmin + (Tsat - Tmin) * 0.2
                        x2 = Tmin + (Tsat - Tmin) * 0.4
                        x3 = Tmin + (Tsat - Tmin) * 0.6
                        x4 = Tmin + (Tsat - Tmin) * 0.8
                        x5 = Tmin + (Tsat - Tmin) * 0.9
                        p1 = Me.m_iapws97.entropySatLiqTW(x1)
                        p2 = Me.m_iapws97.entropySatLiqTW(x2)
                        p3 = Me.m_iapws97.entropySatLiqTW(x3)
                        p4 = Me.m_iapws97.entropySatLiqTW(x4)
                        p5 = Me.m_iapws97.entropySatLiqTW(x5)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return Me.m_iapws97.entropySatLiqTW(T)
                    Else
                        Return Me.m_iapws97.entropyW(T, P / 100000)
                    End If
                Case State.Vapor
                    If T > Tbound Then
                        CurrentMaterialStream.Flowsheet.ShowMessage(String.Format("Steam Tables Entropy Calculation: Temperature outside valid range ({0} > {1}), extrapolating calculated values...", T, Tbound), Interfaces.IFlowsheet.MessageType.Warning)
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tcrit + (Tbound - Tcrit) * 0.2
                        x2 = Tcrit + (Tbound - Tcrit) * 0.4
                        x3 = Tcrit + (Tbound - Tcrit) * 0.6
                        x4 = Tcrit + (Tbound - Tcrit) * 0.8
                        x5 = Tcrit + (Tbound - Tcrit) * 0.9
                        p1 = Me.m_iapws97.entropyW(x1, P / 100000)
                        p2 = Me.m_iapws97.entropyW(x2, P / 100000)
                        p3 = Me.m_iapws97.entropyW(x3, P / 100000)
                        p4 = Me.m_iapws97.entropyW(x4, P / 100000)
                        p5 = Me.m_iapws97.entropyW(x5, P / 100000)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    ElseIf Math.Abs(T - Tsat) < 0.01 Then
                        Return Me.m_iapws97.entropySatVapTW(T)
                    ElseIf T > Tsat Then
                        Return Me.m_iapws97.entropyW(T, P / 100000)
                    Else
                        Dim x1, x2, x3, x4, x5, p1, p2, p3, p4, p5 As Double
                        x1 = Tsat + (Tcrit - Tsat) * 0.2
                        x2 = Tsat + (Tcrit - Tsat) * 0.4
                        x3 = Tsat + (Tcrit - Tsat) * 0.6
                        x4 = Tsat + (Tcrit - Tsat) * 0.8
                        x5 = Tsat + (Tcrit - Tsat) * 0.9
                        p1 = Me.m_iapws97.entropySatVapTW(x1)
                        p2 = Me.m_iapws97.entropySatVapTW(x2)
                        p3 = Me.m_iapws97.entropySatVapTW(x3)
                        p4 = Me.m_iapws97.entropySatVapTW(x4)
                        p5 = Me.m_iapws97.entropySatVapTW(x5)
                        Return Interpolation.polinterpolation.nevilleinterpolation(New Double() {x1, x2, x3, x4, x5}, New Double() {p1, p2, p3, p4, p5}, 5, T)
                    End If
                Case Else
                    Return Me.m_iapws97.entropyW(T, P / 100000)
            End Select
        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Return Me.m_iapws97.entropyW(T, P / 100000) - Me.RET_Sid(298.15, T, P, Vx)
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()

            CheckStream()

            Dim n As Integer = Vx.Length - 1
            Dim i As Integer
            Dim fugcoeff(n) As Double

            If st = State.Liquid Then
                For i = 0 To n
                    fugcoeff(i) = Me.AUX_PVAPi(i, T) / P
                Next
            Else
                For i = 0 To n
                    fugcoeff(i) = 1
                Next
            End If

            Return fugcoeff

        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function AUX_Z(Vx() As Double, T As Double, P As Double, state As PhaseName) As Double

            Return 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / Me.AUX_MMM(PropertyPackages.Phase.Mixture)) / 8.314 / T * P

        End Function

    End Class

End Namespace

