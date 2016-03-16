'    Conversion Reactor Calculation Routines 
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

Imports Microsoft.MSDN.Samples.GraphicObjects
Imports DWSIM.DWSIM.ClassesBasicasTermodinamica
Imports Ciloci.Flee
Imports System.Math
Imports System.Linq
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.Reactors

    <System.Serializable()> Public Class Reactor_Conversion

        Inherits Reactor

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.New()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
            Me.ShowQuickTable = False

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim form As FormFlowsheet = Me.FlowSheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_Conversion
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_Conversion
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_Conversion
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_Conversion
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedeenerg17"))
            End If

            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)

            Dim ConversionLimiter As New Dictionary(Of String, Double)
         
            Me.Reactions.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0

            'check active reactions (conversion only) in the reaction set
            For Each rxnsb As ReactionSetBase In form.Options.ReactionSets(Me.ReactionSetID).Reactions.Values
                If form.Options.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Conversion And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                End If
            Next

            'order reactions
            Dim i As Integer
            i = 0
            Dim maxrank As Integer = 0
            For Each rxnsb As ReactionSetBase In form.Options.ReactionSets(Me.ReactionSetID).Reactions.Values
                If rxnsb.Rank > maxrank And Me.Reactions.Contains(rxnsb.ReactionID) Then maxrank = rxnsb.Rank
            Next

            'ordering of parallel reactions

            i = 0
            Dim arr As New ArrayList
            Do
                arr = New ArrayList
                For Each rxnsb As ReactionSetBase In form.Options.ReactionSets(Me.ReactionSetID).Reactions.Values
                    If rxnsb.Rank = i And Me.Reactions.Contains(rxnsb.ReactionID) Then arr.Add(rxnsb.ReactionID)
                Next
                If arr.Count > 0 Then Me.ReactionsSequence.Add(i, arr)
                i = i + 1
            Loop Until i = maxrank + 1

            Dim ims As DWSIM.SimulationObjects.Streams.MaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Clone
            Dim pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage = Me.PropertyPackage
            Dim ppr As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()

            ims.SetFlowsheet(Me.FlowSheet)

            pp.CurrentMaterialStream = ims
            ppr.CurrentMaterialStream = ims

            Dim DN As New Dictionary(Of String, Double)
            Dim N0 As New Dictionary(Of String, Double)
            Dim N As New Dictionary(Of String, Double)

            Dim X, scBC, nBC, DHr, Hid_r, Hid_p, Hr, Hp, Tin, Pin, Pout, W As Double
            Dim BC As String = ""
            Dim tmp As Object
         
            Tin = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault
            Pin = ims.Fases(0).SPMProperties.pressure.GetValueOrDefault
            W = ims.Fases(0).SPMProperties.massflow.GetValueOrDefault
            Pout = ims.Fases(0).SPMProperties.pressure.GetValueOrDefault - Me.DeltaP.GetValueOrDefault
            ims.Fases(0).SPMProperties.pressure = Pout

            'Reactants Enthalpy (kJ/kg * kg/s = kW)

            Hr = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

            Dim rxn As Reaction

            'loop through reactions

            For Each ar As ArrayList In Me.ReactionsSequence.Values

                ConversionLimiter.Clear()

                'calculate conversion limiters

                i = 0
                Do

                    'process reaction i

                    rxn = form.Options.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    'initial mole flows
                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        Select Case rxn.ReactionPhase
                            Case PhaseName.Liquid
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(1).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(1).SPMProperties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Fases(1).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(1).SPMProperties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                            Case PhaseName.Vapor
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(2).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(2).SPMProperties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Fases(2).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(2).SPMProperties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                            Case PhaseName.Mixture
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(0).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(0).SPMProperties.molarflow.GetValueOrDefault)
                                    N.Add(sb.CompName, N0(sb.CompName))
                                Else
                                    N0(sb.CompName) = ims.Fases(0).Componentes(sb.CompName).FracaoMolar.GetValueOrDefault * ims.Fases(0).SPMProperties.molarflow.GetValueOrDefault
                                    'N(sb.CompName) = N0(sb.CompName)
                                End If
                        End Select

                    Next

                    nBC = N0(rxn.BaseReactant)

                    rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                    rxn.ExpContext.Imports.AddType(GetType(System.Math))
                    rxn.ExpContext.Variables.Clear()
                    rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    rxn.ExpContext.Variables.Add("T", ims.Fases(0).SPMProperties.temperature.GetValueOrDefault)

                    rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                    X = rxn.Expr.Evaluate / 100

                    If X < 0 Or X > 1 Then Throw New ArgumentOutOfRangeException("Conversion Expression", "The conversion expression for reaction " & rxn.Name & " results in a value that is out of the valid range (0 to 100%).")

                    'verify if the calculated conversion is reachable

                
                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If sb.StoichCoeff < 1 Then
                            If Not ConversionLimiter.ContainsKey(sb.CompName) Then ConversionLimiter.Add(sb.CompName, 0.0#)
                            Dim amount As Double = Abs(X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC)
                            ConversionLimiter(sb.CompName) += amount / N0(sb.CompName)
                       End If
                    Next

                    i += 1

                Loop Until i = ar.Count

                i = 0
                DHr = 0
                Hid_r = 0
                Hid_p = 0
                Do

                    'process reaction i
                    rxn = form.Options.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    nBC = N0(rxn.BaseReactant)

                    rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                    rxn.ExpContext.Imports.AddType(GetType(System.Math))
                    rxn.ExpContext.Variables.Clear()
                    rxn.ExpContext.Variables.Add("T", ims.Fases(0).SPMProperties.temperature.GetValueOrDefault)

                    rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                    X = rxn.Expr.Evaluate / 100.0#

                    If X < 0.0# Or X > 1.0# Then

                        Throw New ArgumentOutOfRangeException("Conversion Expression", "The conversion expression for reaction " & rxn.Name & " results in a value that is out of the valid range (0 to 100%).")

                    Else

                        If ConversionLimiter.Values.Max > 1.0# Then X /= ConversionLimiter.Values.Max

                        If Not Me.Conversions.ContainsKey(rxn.ID) Then
                            Me.Conversions.Add(rxn.ID, X)
                        Else
                            Me.Conversions(rxn.ID) = X
                        End If

                        'delta mole flows

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            If Not DN.ContainsKey(sb.CompName) Then
                                DN.Add(sb.CompName, -X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC)
                            Else
                                DN(sb.CompName) = -X * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC
                            End If
                        Next

                        'final mole flows

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            N(sb.CompName) += DN(sb.CompName)
                        Next

                        'Ideal Gas Reactants Enthalpy (kJ/kg * kg/s = kW)
                        Hid_r += 0 'ppr.RET_Hid(298.15, ims.Fases(0).SPMProperties.temperature.GetValueOrDefault, PropertyPackages.Fase.Mixture) * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                        'update mole flows/fractions
                        Dim Nall(ims.Fases(0).Componentes.Count - 1), Nsum As Double
                        For Each sb As ReactionStoichBase In rxn.Components.Values

                            Nsum = 0
                            For Each s2 As Substancia In ims.Fases(0).Componentes.Values
                                If rxn.Components.ContainsKey(s2.Nome) Then
                                    Nsum += N(s2.Nome)
                                Else
                                    Nsum += ims.Fases(0).Componentes(s2.Nome).FracaoMolar.GetValueOrDefault * ims.Fases(0).SPMProperties.molarflow.GetValueOrDefault
                                End If
                            Next

                            For Each s3 As Substancia In ims.Fases(0).Componentes.Values
                                If rxn.Components.ContainsKey(s3.Nome) Then
                                    s3.FracaoMolar = N(s3.Nome) / Nsum
                                Else
                                    s3.FracaoMolar = ims.Fases(0).Componentes(s3.Nome).FracaoMolar.GetValueOrDefault * ims.Fases(0).SPMProperties.molarflow.GetValueOrDefault / Nsum
                                End If
                            Next

                            ims.Fases(0).SPMProperties.molarflow = Nsum

                            Dim mmm As Double = 0

                            For Each s3 As Substancia In ims.Fases(0).Componentes.Values
                                mmm += s3.FracaoMolar.GetValueOrDefault * s3.ConstantProperties.Molar_Weight
                            Next

                            For Each s3 As Substancia In ims.Fases(0).Componentes.Values
                                s3.FracaoMassica = s3.FracaoMolar.GetValueOrDefault * s3.ConstantProperties.Molar_Weight / mmm
                            Next

                        Next

                        'Ideal Gas Products Enthalpy (kJ/kg * kg/s = kW)
                        Hid_p += 0 'ppr.RET_Hid(298.15, ims.Fases(0).SPMProperties.temperature.GetValueOrDefault, PropertyPackages.Fase.Mixture) * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                    End If

                    'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                    DHr += rxn.ReactionHeat * Abs(DN(rxn.BaseReactant)) / 1000

                    i += 1

                Loop Until i = ar.Count

                'do a flash calc (calculate final temperature/enthalpy)

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name).Energia.GetValueOrDefault

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = Me.DeltaQ.GetValueOrDefault + Hr + Hid_p - Hid_r - DHr
                        Hp = Hp / W

                        tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, Pout, Hp, Tin)
                        Dim Tout As Double = tmp(2)
                        Me.DeltaT = Tout - Tin

                        ims.Fases(0).SPMProperties.temperature = Tout

                        With pp

                            'Calcular corrente de matéria com T e P
                            .DW_CalcVazaoMolar()
                            .DW_CalcEquilibrium(DWSIM.SimulationObjects.PropertyPackages.FlashSpec.T, DWSIM.SimulationObjects.PropertyPackages.FlashSpec.P)
                            If ims.Fases(1).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            End If
                            If ims.Fases(2).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            End If
                            .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Mixture)
                            .DW_CalcOverallProps()
                            .DW_CalcTwoPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid, DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            .DW_CalcVazaoVolumetrica()

                        End With

                    Case OperationMode.Isothermic

                        With pp

                            'Calcular corrente de matéria com T e P
                            .DW_CalcVazaoMolar()
                            .DW_CalcEquilibrium(DWSIM.SimulationObjects.PropertyPackages.FlashSpec.T, DWSIM.SimulationObjects.PropertyPackages.FlashSpec.P)
                            If ims.Fases(1).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            End If
                            If ims.Fases(2).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            End If
                            .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Mixture)
                            .DW_CalcOverallProps()
                            .DW_CalcTwoPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid, DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            .DW_CalcVazaoVolumetrica()

                        End With

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Me.DeltaQ.GetValueOrDefault + DHr + Hid_r + Hp - Hr - Hid_p

                        Me.DeltaT = 0

                    Case OperationMode.OutletTemperature

                        Dim Tout As Double = Me.OutletTemperature

                        Me.DeltaT = Tout - Tin

                        ims.Fases(0).SPMProperties.temperature = Tout

                        With pp

                            'Calcular corrente de matéria com T e P
                            .DW_CalcVazaoMolar()
                            .DW_CalcEquilibrium(DWSIM.SimulationObjects.PropertyPackages.FlashSpec.T, DWSIM.SimulationObjects.PropertyPackages.FlashSpec.P)
                            If ims.Fases(1).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid)
                            End If
                            If ims.Fases(2).SPMProperties.molarfraction.GetValueOrDefault > 0 Then
                                .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            Else
                                .DW_ZerarPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            End If
                            .DW_CalcPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Mixture)
                            .DW_CalcOverallProps()
                            .DW_CalcTwoPhaseProps(DWSIM.SimulationObjects.PropertyPackages.Fase.Liquid, DWSIM.SimulationObjects.PropertyPackages.Fase.Vapor)
                            .DW_CalcVazaoVolumetrica()

                        End With

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                        'Heat (kW)
                        Me.DeltaQ = Me.DeltaQ.GetValueOrDefault + DHr + Hid_r + Hp - Hr - Hid_p

                End Select

            Next

            'Copy results to upstream MS
            Dim xl, xv, xs, T, P, H, S, wtotalx, wtotaly, wtotalS As Double
            Dim nc As Integer = ims.Fases(0).Componentes.Count - 1
            tmp = pp.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P, ims.Fases(0).SPMProperties.temperature.GetValueOrDefault, ims.Fases(0).SPMProperties.pressure.GetValueOrDefault, 0)

            Dim Vx(nc), Vy(nc), Vs(nc), Vwx(nc), Vwy(nc), Vws(nc) As Double
            xl = tmp(0)
            xv = tmp(1)
            xs = tmp(13)
            T = tmp(2)
            P = tmp(3)
            H = tmp(4)
            S = tmp(5)
            Vx = tmp(8)
            Vy = tmp(9)
            Vs = tmp(14)


            Dim j As Integer = 0

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint
            cp = Me.GraphicObject.InputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedFrom.Name)
                Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                i = 0
                For Each comp In ms.Fases(0).Componentes.Values
                    wtotalx += Vx(i) * comp.ConstantProperties.Molar_Weight
                    wtotaly += Vy(i) * comp.ConstantProperties.Molar_Weight
                    wtotalS += Vs(i) * comp.ConstantProperties.Molar_Weight
                    i += 1
                Next
                i = 0
                For Each comp In ms.Fases(0).Componentes.Values
                    If wtotalx > 0 Then Vwx(i) = Vx(i) * comp.ConstantProperties.Molar_Weight / wtotalx
                    If wtotaly > 0 Then Vwy(i) = Vy(i) * comp.ConstantProperties.Molar_Weight / wtotaly
                    If wtotalS > 0 Then Vws(i) = Vs(i) * comp.ConstantProperties.Molar_Weight / wtotalS
                    i += 1
                Next
            End If

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = T
                    .Fases(0).SPMProperties.pressure = P
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = Vy(j)
                        comp.FracaoMassica = Vwy(j)
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = W * (wtotaly * xv / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Fases(0).SPMProperties.massfraction = (wtotaly * xv / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Fases(0).SPMProperties.molarfraction = 1
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = T
                    .Fases(0).SPMProperties.pressure = P
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = (Vx(j) * xl + Vs(j) * xs) / (xl + xs)
                        comp.FracaoMassica = (Vwx(j) * wtotalx + Vws(j) * wtotalS) / (wtotalx + wtotalS)
                        j += 1
                    Next
                    j = 0
                    .Fases(0).SPMProperties.massflow = W * ((wtotalx * xl + wtotalS * xs) / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Fases(0).SPMProperties.massfraction = ((wtotalx * xl + wtotalS * xs) / (wtotaly * xv + wtotalx * xl + wtotalS * xs))
                    .Fases(0).SPMProperties.molarfraction = 1
                End With
            End If

            'Corrente de energia - atualizar valor da potência (kJ/s)
            With form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
                .Energia = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

            'Call function to calculate flowsheet
            With objargs
                .Calculado = True
                .Nome = Me.Nome
                .Tag = Me.GraphicObject.Tag
                .Tipo = TipoObjeto.RCT_Conversion
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            'If Not Me.GraphicObject.InputConnectors(0).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac10"))
            'If Not Me.GraphicObject.OutputConnectors(0).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac11"))
            'If Not Me.GraphicObject.OutputConnectors(1).IsAttached Then Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac11"))

            Dim form As Global.DWSIM.FormFlowsheet = Me.Flowsheet

            'Dim ems As DWSIM.SimulationObjects.Streams.MaterialStream = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
            'Dim W As Double = ems.Fases(0).SPMProperties.massflow.GetValueOrDefault
            Dim j As Integer = 0

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.temperature = Nothing
                    .Fases(0).SPMProperties.pressure = Nothing
                    .Fases(0).SPMProperties.enthalpy = Nothing
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    j = 0
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = 0
                        comp.FracaoMassica = 0
                        j += 1
                    Next
                    .Fases(0).SPMProperties.massflow = Nothing
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.RCT_Conversion
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub QTFillNodeItems()

        End Sub

        Public Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida1, saida2, energ As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida1 = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida1 = ""
                End If
                If Me.GraphicObject.OutputConnectors(1).IsAttached = True Then
                    saida2 = Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
                Else
                    saida2 = ""
                End If
                If Me.GraphicObject.InputConnectors(1).IsAttached = True Then
                    energ = Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
                Else
                    energ = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Correntedeentrada"), ent, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Saidadevapor"), saida1, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Saidadelquido"), saida2, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIOutputMSSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("Correntedeenergia"), energ, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .CustomEditor = New DWSIM.Editors.Streams.UIInputESSelector
                End With

                .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem1"), FlowSheet.Options.ReactionSets(Me.ReactionSetID).Name, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem1Help"), True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Reactors.UIReactionSetSelector
                    .IsDropdownResizable = True
                End With

                .Item.Add(DWSIM.App.GetLocalString("RConvPGridItem2"), Me, "ReactorOperationMode", False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RConvPGridItem2Help"), True)
                With .Item(.Item.Count - 1)
                    .IsBrowsable = False
                End With

                Dim valor As Double

                If Me.ReactorOperationMode = OperationMode.OutletTemperature Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_temperature, Me.OutletTemperature), FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("HeaterCoolerOutletTemperature"), su.spmp_temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), "", True)
                    With .Item(.Item.Count - 1)
                        .Tag = New Object() {FlowSheet.Options.NumberFormat, su.spmp_temperature, "T"}
                        .CustomEditor = New DWSIM.Editors.Generic.UIUnitConverter
                    End With
                End If

                valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.spmp_deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.Calculated Then

                    .Item.Add(FT(DWSIM.App.GetLocalString("DeltaT2"), su.spmp_deltaT), Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("Diferenadetemperatur"), True)
                    With .Item(.Item.Count - 1)
                        .DefaultValue = Nothing
                        .DefaultType = GetType(Nullable(Of Double))
                    End With

                    .Item.Add(FT(DWSIM.App.GetLocalString("RConvPGridItem3"), su.spmp_heatflow), Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ.GetValueOrDefault), FlowSheet.Options.NumberFormat), True, DWSIM.App.GetLocalString("Resultados3"), "", True)
                    With .Item(.Item.Count - 1)
                        .DefaultValue = Nothing
                        .DefaultType = GetType(Nullable(Of Double))
                    End With

                    'CustomPropertyCollection
                    Dim m As New PropertyGridEx.CustomPropertyCollection()
                    For Each dbl As KeyValuePair(Of String, Double) In Me.Conversions
                        valor = Format(dbl.Value * 100, FlowSheet.Options.NumberFormat)
                        m.Add(FlowSheet.Options.Reactions(dbl.Key).Name, valor, False, DWSIM.App.GetLocalString("ReacoesConversoes"), DWSIM.App.GetLocalString("RConvPGridItem4Help"), True)
                        m.Item(m.Count - 1).IsReadOnly = True
                        m.Item(m.Count - 1).DefaultValue = Nothing
                        m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next

                    .Item.Add(DWSIM.App.GetLocalString("ReacoesConversoes"), m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RConvPGridItem3Help"), True)
                    With .Item(.Item.Count - 1)
                        .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With

                End If

            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As Double = 0
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0    Pressure Drop
                    value = Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault)

            End Select

            Return value

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As SimulationObjects_BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                    For i = 0 To 0
                        proplist.Add("PROP_CR_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 0
                        proplist.Add("PROP_CR_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_CR_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As DWSIM.SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    Me.DeltaP = Conversor.ConverterParaSI(su.spmp_deltaP, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SistemasDeUnidades.Unidades = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SistemasDeUnidades.UnidadesSI
            Dim cv As New DWSIM.SistemasDeUnidades.Conversor
            Dim value As String = ""
            Dim propidx As Integer = CInt(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_HT_0	Pressure Drop
                    value = su.spmp_deltaP

            End Select

            Return value
        End Function
    End Class

End Namespace
