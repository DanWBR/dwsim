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

'Imports DWSIM.SimulationObjects
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary
Imports DWSIM.Thermodynamics.MathEx
Imports System.Linq
Imports DWSIM.Thermodynamics.BaseClasses

Namespace PropertyPackages

    <System.Runtime.InteropServices.Guid(SteamTablesPropertyPackage.ClassId)> _
<System.Serializable()> Public Class SteamTablesPropertyPackage

        Inherits PropertyPackages.PropertyPackage

        Public Shadows Const ClassId As String = "170D6E8A-8880-4bf9-B7A0-E4A3FDBFD589"

        Protected m_iapws97 As New IAPWS_IF97
        'Protected m_steam67 As New STEAM67

        Public Sub New(ByVal comode As Boolean)
            MyBase.New(comode)
        End Sub

        Public Sub New()

            Me.SupportedComponents.Add(15)
            Me._packagetype = PropertyPackages.PackageType.Miscelaneous

        End Sub

        Public Overrides Function AUX_VAPDENS(ByVal T As Double, ByVal P As Double) As Double

            Return Me.m_iapws97.densW(T, P / 100000)

        End Function

        Public Overrides Sub DW_CalcEquilibrium(ByVal spec1 As PropertyPackages.FlashSpec, ByVal spec2 As PropertyPackages.FlashSpec)

            Dim water As Compound = (From subst As Compound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

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
                                    P = brentsolverP.BrentOpt(0.001, 600, 20, 0.0001, 1000, Nothing)
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
                                    P = brentsolverP.BrentOpt(0.001, 1000, 20, 0.0001, 1000, Nothing)
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
                                    T = brentsolverT.BrentOpt(273.15, 623.15, 20, 0.0001, 1000, Nothing)
                                End If

                            End With
                            lf = 1 - vf

                        Case FlashSpec.S

                            P = Me.CurrentMaterialStream.Phases(0).Properties.pressure.GetValueOrDefault

                            With Me.m_iapws97
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
                                    T = brentsolverT.BrentOpt(273.15, 623.15, 20, 0.0001, 1000, Nothing)
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
                .Phases(0).Compounds(wkey).FracaoMolar = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FracaoMolar = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FugacityCoeff = 1
                If lf = 0 Then .Phases(3).Compounds(wkey).FracaoMolar = 0
                If vf > 0 Then .Phases(2).Compounds(wkey).FracaoMolar = 1
                If vf > 0 Then .Phases(2).Compounds(wkey).FugacityCoeff = 1
                If vf = 0 Then .Phases(2).Compounds(wkey).FracaoMolar = 0
                .Phases(0).Compounds(wkey).FracaoMassica = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FracaoMassica = 1
                If lf > 0 Then .Phases(3).Compounds(wkey).FugacityCoeff = 1
                If lf = 0 Then .Phases(3).Compounds(wkey).FracaoMassica = 0
                If vf > 0 Then .Phases(2).Compounds(wkey).FracaoMassica = 1
                If vf > 0 Then .Phases(2).Compounds(wkey).FugacityCoeff = 1
                If vf = 0 Then .Phases(2).Compounds(wkey).FracaoMassica = 0

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

        End Sub

        Public Overrides Function DW_CalcEquilibrio_ISOL(spec1 As FlashSpec, spec2 As FlashSpec, val1 As Double, val2 As Double, estimate As Double) As Object

            Dim water As Compound = (From subst As Compound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

            Dim P, T, H, S, vf, lf, Psat, Hv, Hl, Sv, Sl As Double

            Dim brentsolverP As New BrentOpt.Brent
            brentsolverP.DefineFuncDelegate(AddressOf EnthalpyPx)

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf EnthalpyTx)

            Select Case spec1

                Case FlashSpec.T

                    Select Case spec2

                        Case FlashSpec.P

                            T = val1
                            P = val2

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

                            T = val1
                            H = val2

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
                                    P = brentsolverP.BrentOpt(0.001, 600, 20, 0.0001, 1000, Nothing)
                                End If
                                P = P * 100000

                            End With
                            lf = 1 - vf

                        Case FlashSpec.S

                            T = val1
                            S = val2

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
                                    P = brentsolverP.BrentOpt(0.001, 1000, 20, 0.0001, 1000, Nothing)
                                End If
                                P = P * 100000

                            End With
                            lf = 1 - vf

                        Case FlashSpec.VAP

                            T = val1
                            vf = val2

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

                            P = val1
                            H = val2

                            With Me.m_iapws97
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
                                    T = brentsolverT.BrentOpt(273.15, 623.15, 20, 0.0001, 1000, Nothing)
                                End If

                            End With
                            lf = 1 - vf

                        Case FlashSpec.S

                            P = val1
                            S = val2

                            With Me.m_iapws97
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
                                    T = brentsolverT.BrentOpt(273.15, 623.15, 20, 0.0001, 1000, Nothing)
                                End If

                            End With
                            lf = 1 - vf

                        Case FlashSpec.VAP

                            P = val1
                            vf = val2

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

            Return New Object() {lf, vf, T, P, H, S, 1, 1, New Double() {1.0#}, New Double() {1.0#}, New Object() {lf, vf, New Double() {1.0#}, New Double() {1.0#}, 0, 0.0#, Me.RET_NullVector, 0.0#, Me.RET_NullVector}}

        End Function

        'Public Overrides Sub DW_CalcOverallProps()

        '    'frações molares/mássicas das Phases
        '    Dim xmv, xml, xwv, xwl As Double

        '    xmv = Me.CurrentMaterialStream.Phases(2).Properties.molarfraction.GetValueOrDefault
        '    xml = Me.CurrentMaterialStream.Phases(1).Properties.molarfraction.GetValueOrDefault
        '    xwv = Me.CurrentMaterialStream.Phases(2).Properties.massfraction.GetValueOrDefault
        '    xwl = Me.CurrentMaterialStream.Phases(1).Properties.massfraction.GetValueOrDefault


        'End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Phase)

            Dim water As Compound = (From subst As Compound In Me.CurrentMaterialStream.Phases(0).Compounds.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

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
                    Me.CurrentMaterialStream.Phases(0).Properties2.surfaceTension = Me.AUX_SURFTM(T)
                Case Else
                    Dim ex As Exception = New CapeOpen.CapeThrmPropertyNotAvailableException
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "CalcSinglePhaseProp/CalcTwoPhaseProp/CalcProp", ex.GetHashCode)
            End Select

        End Sub

        Public Overrides Sub DW_CalcPhaseProps(ByVal Phase As PropertyPackages.Phase)

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
                result = result * 18 / 1000
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massflow = result
                result = phasemolarfrac
                Me.CurrentMaterialStream.Phases(phaseID).Properties.massfraction = result
            End If

            Dim Tsat As Double = Me.m_iapws97.tSatW(P / 100000)

            If Math.Abs(T - Tsat) < 0.001 Then

                If phaseID = 3 Then

                    result = Me.m_iapws97.densSatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = Me.m_iapws97.enthalpySatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.m_iapws97.entropySatLiqTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatLiqTW(T) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = 18
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

                ElseIf phaseID = 2 Then

                    result = Me.m_iapws97.densSatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.density = result
                    result = Me.m_iapws97.enthalpySatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.enthalpy = result
                    result = Me.m_iapws97.entropySatVapTW(T)
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatVapTW(T) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Phases(phaseID).Properties.heatCapacityCv = result
                    result = 18
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

                ElseIf phaseID = 1 Then

                    DW_CalcLiqMixtureProps()


                Else

                    DW_CalcOverallProps()

                End If

            Else

                If phaseID = 3 Or phaseID = 2 Then

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


                ElseIf phaseID = 1 Then

                    DW_CalcLiqMixtureProps()


                Else

                    DW_CalcOverallProps()

                End If

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * 18 / 1000 / Me.CurrentMaterialStream.Phases(phaseID).Properties.density.GetValueOrDefault
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
            Me.CurrentMaterialStream.Phases(0).Properties2.kvalue = result
            result = 0
            Me.CurrentMaterialStream.Phases(0).Properties2.logKvalue = result
            Dim Tr = T / 647.13
            result = 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
            Me.CurrentMaterialStream.Phases(0).Properties2.surfaceTension = result

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
            Return 18
        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Me.m_iapws97.pSatW(T) * 100000
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As Thermodynamics.BaseClasses.ConstantProperties) As Boolean

            If Me.SupportedComponents.Contains(comp.ID) Then
                Return True
            Else
                Return False
            End If

        End Function

        Public Overrides Function DW_CalcEnthalpy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Select Case st
                Case State.Liquid
                    Return Me.m_iapws97.enthalpySatLiqTW(T)
                Case State.Vapor
                    Return Me.m_iapws97.enthalpySatVapTW(T)
                Case Else
                    Return Me.m_iapws97.enthalpyW(T, P / 100000)
            End Select
        End Function

        Public Overrides Function DW_CalcKvalue(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double) As Double()
            Return New Double() {1.0#}
        End Function

        Public Overrides Function DW_CalcEnthalpyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Select Case st
                Case State.Liquid
                    Return Me.m_iapws97.enthalpySatLiqTW(T) - Me.RET_Hid(298.15, T, Vx)
                Case State.Vapor
                    Return Me.m_iapws97.enthalpySatVapTW(T) - Me.RET_Hid(298.15, T, Vx)
                Case Else
                    Return Me.m_iapws97.enthalpyW(T, P / 100000) - Me.RET_Hid(298.15, T, Vx)
            End Select
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

        Dim LoopVarF As Double = 0
        Dim LoopVarX As Double = 0

        Public Function EnthalpyTx(ByVal x As Double, ByVal otherargs As Object) As Double

            Dim er As Double = LoopVarF - Me.m_iapws97.enthalpyW(x, LoopVarX)
            Return er

        End Function

        Public Function EnthalpyPx(ByVal x As Double, ByVal otherargs As Object) As Double

            Return LoopVarF - Me.m_iapws97.enthalpyW(LoopVarX, x)

        End Function

        Public Overrides Function DW_CalcEntropy(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Select Case st
                Case State.Liquid
                    Return Me.m_iapws97.entropySatLiqTW(T)
                Case State.Vapor
                    Return Me.m_iapws97.entropySatVapTW(T)
                Case Else
                    Return Me.m_iapws97.entropyW(T, P / 100000)
            End Select
        End Function

        Public Overrides Function DW_CalcEntropyDeparture(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double
            Select Case st
                Case State.Liquid
                    Return Me.m_iapws97.entropySatLiqTW(T) - Me.RET_Sid(298.15, T, P, Vx)
                Case State.Vapor
                    Return Me.m_iapws97.entropySatVapTW(T) - Me.RET_Sid(298.15, T, P, Vx)
                Case Else
                    Return Me.m_iapws97.entropyW(T, P / 100000) - Me.RET_Sid(298.15, T, P, Vx)
            End Select
        End Function

        Public Overrides Function DW_CalcFugCoeff(ByVal Vx As System.Array, ByVal T As Double, ByVal P As Double, ByVal st As State) As Double()
            Return New Double() {1.0#}
        End Function

    End Class

End Namespace

