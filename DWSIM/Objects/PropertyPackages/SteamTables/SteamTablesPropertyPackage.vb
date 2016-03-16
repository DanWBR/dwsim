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
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages.Auxiliary
Imports DWSIM.DWSIM.MathEx
Imports System.Linq
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica

Namespace DWSIM.SimulationObjects.PropertyPackages

    <System.Runtime.InteropServices.Guid(SteamTablesPropertyPackage.ClassId)> _
<System.Serializable()> Public Class SteamTablesPropertyPackage

        Inherits DWSIM.SimulationObjects.PropertyPackages.PropertyPackage

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

        Public Overrides Sub DW_CalcEquilibrium(ByVal spec1 As DWSIM.SimulationObjects.PropertyPackages.FlashSpec, ByVal spec2 As DWSIM.SimulationObjects.PropertyPackages.FlashSpec)

            Dim water As Substancia = (From subst As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

            Dim wkey As String = water.Nome

            Me.CurrentMaterialStream.AtEquilibrium = False

            Dim P, T, H, S, vf, lf, Psat, Hv, Hl, Sv, Sl As Double

            Dim brentsolverP As New BrentOpt.Brent
            brentsolverP.DefineFuncDelegate(AddressOf EnthalpyPx)

            Dim brentsolverT As New BrentOpt.Brent
            brentsolverT.DefineFuncDelegate(AddressOf EnthalpyTx)

            'for TVF/PVF/PH/PS flashes
            H = Me.CurrentMaterialStream.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
            S = Me.CurrentMaterialStream.Fases(0).SPMProperties.entropy.GetValueOrDefault
            vf = Me.CurrentMaterialStream.Fases(2).SPMProperties.molarfraction.GetValueOrDefault

            Me.DW_ZerarPhaseProps(Fase.Vapor)
            Me.DW_ZerarPhaseProps(Fase.Liquid)
            Me.DW_ZerarPhaseProps(Fase.Liquid1)
            Me.DW_ZerarPhaseProps(Fase.Liquid2)
            Me.DW_ZerarPhaseProps(Fase.Liquid3)
            Me.DW_ZerarPhaseProps(Fase.Aqueous)
            Me.DW_ZerarPhaseProps(Fase.Solid)
            Me.DW_ZerarComposicoes(Fase.Vapor)
            Me.DW_ZerarComposicoes(Fase.Liquid)
            Me.DW_ZerarComposicoes(Fase.Liquid1)
            Me.DW_ZerarComposicoes(Fase.Liquid2)
            Me.DW_ZerarComposicoes(Fase.Liquid3)
            Me.DW_ZerarComposicoes(Fase.Aqueous)
            Me.DW_ZerarComposicoes(Fase.Solid)

            Select Case spec1

                Case FlashSpec.T

                    Select Case spec2

                        Case FlashSpec.P

                            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
                            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

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

                            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault

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

                            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault

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

                            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault

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

                            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

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

                            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

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

                            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

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
                .Fases(0).SPMProperties.temperature = T
                .Fases(0).SPMProperties.pressure = P
                .Fases(0).SPMProperties.enthalpy = H
                .Fases(0).SPMProperties.entropy = S
                .Fases(0).SPMProperties.molarfraction = 1
                .Fases(3).SPMProperties.molarfraction = lf
                .Fases(2).SPMProperties.molarfraction = vf
                .Fases(0).SPMProperties.massfraction = 1
                .Fases(3).SPMProperties.massfraction = lf
                .Fases(2).SPMProperties.massfraction = vf
                .Fases(0).Componentes(wkey).FracaoMolar = 1
                If lf > 0 Then .Fases(3).Componentes(wkey).FracaoMolar = 1
                If lf > 0 Then .Fases(3).Componentes(wkey).FugacityCoeff = 1
                If lf = 0 Then .Fases(3).Componentes(wkey).FracaoMolar = 0
                If vf > 0 Then .Fases(2).Componentes(wkey).FracaoMolar = 1
                If vf > 0 Then .Fases(2).Componentes(wkey).FugacityCoeff = 1
                If vf = 0 Then .Fases(2).Componentes(wkey).FracaoMolar = 0
                .Fases(0).Componentes(wkey).FracaoMassica = 1
                If lf > 0 Then .Fases(3).Componentes(wkey).FracaoMassica = 1
                If lf > 0 Then .Fases(3).Componentes(wkey).FugacityCoeff = 1
                If lf = 0 Then .Fases(3).Componentes(wkey).FracaoMassica = 0
                If vf > 0 Then .Fases(2).Componentes(wkey).FracaoMassica = 1
                If vf > 0 Then .Fases(2).Componentes(wkey).FugacityCoeff = 1
                If vf = 0 Then .Fases(2).Componentes(wkey).FracaoMassica = 0

                If lf = 0 Then
                    With .Fases(3).SPMProperties
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
                    With .Fases(2).SPMProperties
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

            Dim water As Substancia = (From subst As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

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

        '    'frações molares/mássicas das fases
        '    Dim xmv, xml, xwv, xwl As Double

        '    xmv = Me.CurrentMaterialStream.Fases(2).SPMProperties.molarfraction.GetValueOrDefault
        '    xml = Me.CurrentMaterialStream.Fases(1).SPMProperties.molarfraction.GetValueOrDefault
        '    xwv = Me.CurrentMaterialStream.Fases(2).SPMProperties.massfraction.GetValueOrDefault
        '    xwl = Me.CurrentMaterialStream.Fases(1).SPMProperties.massfraction.GetValueOrDefault


        'End Sub

        Public Overrides Sub DW_CalcProp(ByVal [property] As String, ByVal phase As Fase)

            Dim water As Substancia = (From subst As Substancia In Me.CurrentMaterialStream.Fases(0).Componentes.Values Select subst Where subst.ConstantProperties.CAS_Number = "7732-18-5").SingleOrDefault

            If water Is Nothing Then
                Throw New Exception("Water compound not found. Please setup your simulation accordingly.")
            End If

            Dim result As Double = 0.0#
            Dim resultObj As Object = Nothing
            Dim phaseID As Integer = -1
            Dim state As String = ""

            Dim T, P As Double
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case phase
                Case Fase.Vapor
                    state = "V"
                Case Fase.Liquid, Fase.Liquid1, Fase.Liquid2, Fase.Liquid3
                    state = "L"
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
                    result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                Case "heatcapacity", "heatcapacitycp"
                    result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                Case "heatcapacitycv"
                    result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                Case "enthalpy", "enthalpynf"
                    result = Me.m_iapws97.enthalpyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                Case "entropy", "entropynf"
                    result = Me.m_iapws97.entropyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                Case "excessenthalpy"
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEnthalpy = 0.0#
                Case "excessentropy"
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.excessEntropy = 0.0#
                Case "enthalpyf"
                    Dim entF As Double = Me.AUX_HFm25(phase)
                    result = Me.m_iapws97.enthalpyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF = result + entF
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpyF.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpyF = result
                Case "entropyf"
                    Dim entF As Double = Me.AUX_SFm25(phase)
                    result = Me.m_iapws97.entropyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropyF = result + entF
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropyF.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropyF = result
                Case "viscosity"
                    result = Me.m_iapws97.viscW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                Case "thermalconductivity"
                    result = Me.m_iapws97.thconW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                Case "fugacity", "fugacitycoefficient", "logfugacitycoefficient", "activity", "activitycoefficient"
                    Me.DW_CalcCompFugCoeff(phase)
                Case "volume", "density"
                    result = Me.m_iapws97.densW(T, P / 100000)
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

            Dim T, P As Double
            Dim composition As Object = Nothing
            Dim phasemolarfrac As Double = Nothing
            Dim overallmolarflow As Double = Nothing

            Dim phaseID As Integer
            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            Select Case fase
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

            If phaseID > 0 Then
                overallmolarflow = Me.CurrentMaterialStream.Fases(0).SPMProperties.molarflow.GetValueOrDefault
                phasemolarfrac = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molarfraction.GetValueOrDefault
                result = overallmolarflow * phasemolarfrac
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molarflow = result
                result = result * 18 / 1000
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massflow = result
                result = phasemolarfrac
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.massfraction = result
            End If

            Dim Tsat As Double = Me.m_iapws97.tSatW(P / 100000)

            If Math.Abs(T - Tsat) < 0.001 Then

                If phaseID = 3 Then

                    result = Me.m_iapws97.densSatLiqTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                    result = Me.m_iapws97.enthalpySatLiqTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.m_iapws97.entropySatLiqTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatLiqTW(T) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatLiqTW(T) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                    result = 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                    result = Me.m_iapws97.thconSatLiqTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                    result = Me.m_iapws97.viscSatLiqTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value

                ElseIf phaseID = 2 Then

                    result = Me.m_iapws97.densSatVapTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                    result = Me.m_iapws97.enthalpySatVapTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.m_iapws97.entropySatVapTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = 1 / (Me.m_iapws97.densSatVapTW(T) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                    result = Me.m_iapws97.cpSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                    result = Me.m_iapws97.cvSatVapTW(T) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                    result = 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                    result = Me.m_iapws97.thconSatVapTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                    result = Me.m_iapws97.viscSatVapTW(T)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value

                ElseIf phaseID = 1 Then

                    DW_CalcLiqMixtureProps()


                Else

                    DW_CalcOverallProps()

                End If

            Else

                If phaseID = 3 Or phaseID = 2 Then

                    result = Me.m_iapws97.densW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density = result
                    result = Me.m_iapws97.enthalpyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy = result
                    result = Me.m_iapws97.entropyW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy = result
                    result = 1 / (Me.m_iapws97.densW(T, P / 100000) * 1000 / 18) / 8.314 / T * P
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.compressibilityFactor = result
                    result = Me.m_iapws97.cpW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCp = result
                    result = Me.m_iapws97.cvW(T, P / 100000) '* 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.heatCapacityCv = result
                    result = 18
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.enthalpy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_enthalpy = result
                    result = Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.entropy.GetValueOrDefault * Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molecularWeight.GetValueOrDefault
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.molar_entropy = result
                    result = Me.m_iapws97.thconW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.thermalConductivity = result
                    result = Me.m_iapws97.viscW(T, P / 100000)
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.viscosity = result
                    Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.kinematic_viscosity = result / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.Value


                ElseIf phaseID = 1 Then

                    DW_CalcLiqMixtureProps()


                Else

                    DW_CalcOverallProps()

                End If

            End If

            If phaseID > 0 Then
                result = overallmolarflow * phasemolarfrac * 18 / 1000 / Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.density.GetValueOrDefault
                Me.CurrentMaterialStream.Fases(phaseID).SPMProperties.volumetric_flow = result
            End If

        End Sub

        Public Overrides Sub DW_CalcTwoPhaseProps(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal fase2 As DWSIM.SimulationObjects.PropertyPackages.Fase)

            Dim result As Double

            Dim T, P As Double
            Dim composition1 As Object = Nothing
            Dim composition2 As Object = Nothing

            T = Me.CurrentMaterialStream.Fases(0).SPMProperties.temperature.GetValueOrDefault
            P = Me.CurrentMaterialStream.Fases(0).SPMProperties.pressure.GetValueOrDefault

            result = 1
            Me.CurrentMaterialStream.Fases(0).TPMProperties.kvalue = result
            result = 0
            Me.CurrentMaterialStream.Fases(0).TPMProperties.logKvalue = result
            Dim Tr = T / 647.13
            result = 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
            Me.CurrentMaterialStream.Fases(0).TPMProperties.surfaceTension = result

        End Sub

        Public Overrides Function DW_CalcMassaEspecifica_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double, Optional ByVal pvp As Double = 0) As Double
            If fase1 = Fase.Liquid Then
                Return Me.m_iapws97.densW(T, P / 100000)
            ElseIf fase1 = Fase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.densSatVapTW(T)
                Else
                    Return Me.m_iapws97.densW(T, P / 100000)
                End If
            ElseIf fase1 = Fase.Mixture Then
                Return Me.m_iapws97.densW(T, P / 100000)
            End If
        End Function

        Public Overrides Function DW_CalcTensaoSuperficial_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Dim Tr = T / 647.13
            Return 0.18548 * (1 - Tr) ^ (2.717 - 3.554 * Tr + 2.047 * Tr ^ 2)
        End Function

        Public Overrides Function DW_CalcViscosidadeDinamica_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            If fase1 = Fase.Liquid Then
                Return Me.m_iapws97.viscW(T, P / 100000)
            ElseIf fase1 = Fase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.viscSatVapTW(T)
                Else
                    Return Me.m_iapws97.viscW(T, P / 100000)
                End If
            End If
        End Function

        Public Overrides Function DW_CalcEnergiaMistura_ISOL(ByVal T As Double, ByVal P As Double) As Double
            Dim ent_massica = Me.m_iapws97.enthalpyW(T, P / 100000)
            Dim flow = Me.CurrentMaterialStream.Fases(0).SPMProperties.massflow
            Return ent_massica * flow
        End Function

        Public Overrides Function DW_CalcCp_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.m_iapws97.cpW(T, P / 10000)
        End Function

        Public Overrides Function DW_CalcK_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            If fase1 = Fase.Liquid Then
                Return Me.m_iapws97.thconW(T, P / 100000)
            ElseIf fase1 = Fase.Vapor Then
                If Me.m_iapws97.pSatW(T) / 100000 = P Then
                    Return Me.m_iapws97.thconSatVapTW(T)
                Else
                    Return Me.m_iapws97.thconW(T, P / 100000)
                End If
            End If
        End Function

        Public Overrides Function DW_CalcMM_ISOL(ByVal fase1 As DWSIM.SimulationObjects.PropertyPackages.Fase, ByVal T As Double, ByVal P As Double) As Double
            Return 18
        End Function

        Public Overrides Function DW_CalcPVAP_ISOL(ByVal T As Double) As Double
            Return Me.m_iapws97.pSatW(T) * 100000
        End Function

        Public Overrides Function SupportsComponent(ByVal comp As ClassesBasicasTermodinamica.ConstantProperties) As Boolean

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

        Public Overrides Function DW_CalcCv_ISOL(ByVal fase1 As Fase, ByVal T As Double, ByVal P As Double) As Double
            Return Me.m_iapws97.cvW(T, P / 100000)
        End Function

        Public Overrides Sub DW_CalcCompPartialVolume(ByVal phase As Fase, ByVal T As Double, ByVal P As Double)

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

