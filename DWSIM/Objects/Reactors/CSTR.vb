'    CSTR Calculation Routines 
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
Imports DWSIM.DWSIM.MathEx
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports System.Linq

Namespace DWSIM.SimulationObjects.Reactors

    <System.Serializable()> Public Class Reactor_CSTR

        Inherits Reactor

        Protected m_vol As Double
        Protected m_isotemp As Double

        Dim C0 As Dictionary(Of String, Double)
        Dim C As Dictionary(Of String, Double)
        Dim Ri As Dictionary(Of String, Double)
        Dim Kf, Kr As ArrayList
        Dim DN As Dictionary(Of String, Double)
        Dim N00 As Dictionary(Of String, Double)
        Dim Rxi As Dictionary(Of String, Double)
        Dim DHRi As Dictionary(Of String, Double)

        Dim activeAL As Integer = 0

        <System.NonSerialized()> Dim form As FormFlowsheet
        <System.NonSerialized()> Dim ims As DWSIM.SimulationObjects.Streams.MaterialStream
        <System.NonSerialized()> Dim pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage
        <System.NonSerialized()> Dim ppr As New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()

        Public Property IsothermalTemperature() As Double
            Get
                Return m_isotemp
            End Get
            Set(ByVal value As Double)
                m_isotemp = value
            End Set
        End Property

        Public Property Volume() As Double
            Get
                Return m_vol
            End Get
            Set(ByVal value As Double)
                m_vol = value
            End Set
        End Property

        Public Property CatalystAmount As Double = 0.0#

        Public Sub New()

            MyBase.New()

            'Me.FillNodeItems()
            'Me.QTFillNodeItems()

            'N00 = New Dictionary(Of String, Double)
            'DN = New Dictionary(Of String, Double)
            'C0 = New Dictionary(Of String, Double)
            'C = New Dictionary(Of String, Double)
            'Ri = New Dictionary(Of String, Double)
            'Rxi = New Dictionary(Of String, Double)
            'DHRi = New Dictionary(Of String, Double)
            'Kf = New ArrayList
            'Kr = New ArrayList

        End Sub

        Public Sub New(ByVal nome As String, ByVal descricao As String)

            MyBase.new()
            Me.m_ComponentName = nome
            Me.m_ComponentDescription = descricao
            Me.FillNodeItems()
            Me.QTFillNodeItems()
   
            N00 = New Dictionary(Of String, Double)
            DN = New Dictionary(Of String, Double)
            C0 = New Dictionary(Of String, Double)
            C = New Dictionary(Of String, Double)
            Ri = New Dictionary(Of String, Double)
            Rxi = New Dictionary(Of String, Double)
            DHRi = New Dictionary(Of String, Double)
            Kf = New ArrayList
            Kr = New ArrayList

        End Sub

        Public Sub ODEFunc(ByVal y As Double(), ByRef dy As Double())

            Dim conv As New SistemasDeUnidades.Conversor

            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim scBC As Double = 0
            Dim BC As String = ""

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As New Dictionary(Of String, Double)

            'loop through reactions
            Dim rxn As Reaction
            Dim ar As ArrayList = Me.ReactionsSequence(activeAL)

            i = 0
            Do
                'process reaction i
                rxn = form.Options.Reactions(ar(i))
                For Each sb As ReactionStoichBase In rxn.Components.Values
                    Ri(sb.CompName) = 0
                Next
                i += 1
            Loop Until i = ar.Count

            i = 0
            Do

                'process reaction i
                rxn = form.Options.Reactions(ar(i))
                BC = rxn.BaseReactant
                scBC = rxn.Components(BC).StoichCoeff

                j = 1
                For Each sb As ReactionStoichBase In rxn.Components.Values

                    C(sb.CompName) = y(j)
                    j = j + 1

                Next

                Dim T As Double = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault

                Dim rx As Double = 0.0#

                convfactors = Me.GetConvFactors(rxn, ims)

                If rxn.ReactionType = ReactionType.Kinetic Then

                    Dim kxf As Double = rxn.A_Forward * Exp(-rxn.E_Forward / (8.314 * T))
                    Dim kxr As Double = rxn.A_Reverse * Exp(-rxn.E_Reverse / (8.314 * T))

                    If T < rxn.Tmin Or T > rxn.Tmax Then
                        kxf = 0.0#
                        kxr = 0.0#
                    End If

                    Dim rxf As Double = 1.0#
                    Dim rxr As Double = 1.0#

                    'kinetic expression

                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        rxf *= (C(sb.CompName) * convfactors(sb.CompName)) ^ sb.DirectOrder
                        rxr *= (C(sb.CompName) * convfactors(sb.CompName)) ^ sb.ReverseOrder
                    Next

                    rx = kxf * rxf - kxr * rxr
                    Rxi(rxn.ID) = Conversor.ConverterParaSI(rxn.VelUnit, rx)

                    Kf(i) = kxf
                    Kr(i) = kxr

                ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then

                    Dim numval, denmval As Double

                    rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                    rxn.ExpContext.Imports.AddType(GetType(System.Math))

                    rxn.ExpContext.Variables.Clear()
                    rxn.ExpContext.Variables.Add("T", ims.Fases(0).SPMProperties.temperature.GetValueOrDefault)
                    rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                    Dim ir As Integer = 1
                    Dim ip As Integer = 1

                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If sb.StoichCoeff < 0 Then
                            rxn.ExpContext.Variables.Add("R" & ir.ToString, C(sb.CompName) * convfactors(sb.CompName))
                            ir += 1
                        ElseIf sb.StoichCoeff > 0 Then
                            rxn.ExpContext.Variables.Add("P" & ip.ToString, C(sb.CompName) * convfactors(sb.CompName))
                            ip += 1
                        End If
                    Next

                    rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationNumerator)

                    numval = rxn.Expr.Evaluate

                    rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationDenominator)

                    denmval = rxn.Expr.Evaluate

                    rx = Conversor.ConverterParaSI(rxn.VelUnit, numval / denmval)

                End If

                For Each sb As ReactionStoichBase In rxn.Components.Values

                    If rxn.ReactionType = ReactionType.Kinetic Then
                        Ri(sb.CompName) += rx * sb.StoichCoeff / rxn.Components(BC).StoichCoeff * Me.Volume
                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then
                        Ri(sb.CompName) += rx * sb.StoichCoeff / rxn.Components(BC).StoichCoeff * Me.CatalystAmount
                    End If

                Next

                i += 1

            Loop Until i = ar.Count

            j = 1
            For Each kv As KeyValuePair(Of String, Double) In Ri

                dy(j) = -kv.Value
                j += 1

            Next

        End Sub

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim conv As New SistemasDeUnidades.Conversor

            If Ri Is Nothing Then Ri = New Dictionary(Of String, Double)
            If Rxi Is Nothing Then Rxi = New Dictionary(Of String, Double)
            If DHRi Is Nothing Then DHRi = New Dictionary(Of String, Double)
            If DN Is Nothing Then DN = New Dictionary(Of String, Double)
            If N00 Is Nothing Then N00 = New Dictionary(Of String, Double)
            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ComponentConversions Is Nothing Then Me.m_componentconversions = New Dictionary(Of String, Double)

            form = Me.FlowSheet

            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_CSTR
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                'Call function to calculate flowsheet
                With objargs
                    .Calculado = False
                    .Nome = Me.Nome
                    .Tipo = TipoObjeto.RCT_CSTR
                End With
                CalculateFlowsheet(FlowSheet, objargs, Nothing)
                Throw New Exception(DWSIM.App.GetLocalString("Nohcorrentedematriac15"))
            End If

            ims = form.Collections.CLCS_MaterialStreamCollection(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name).Clone
            pp = Me.PropertyPackage
            ppr = New DWSIM.SimulationObjects.PropertyPackages.RaoultPropertyPackage()

            ims.SetFlowsheet(Me.FlowSheet)

            Me.Reactions.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.ComponentConversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0
            Me.DN.Clear()

            Dim kcount, hcount As Integer

            'check active reactions (kinetic and heterogeneous only) in the reaction set
            kcount = 0
            hcount = 0
            For Each rxnsb As ReactionSetBase In form.Options.ReactionSets(Me.ReactionSetID).Reactions.Values
                If form.Options.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Kinetic And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    kcount += 1
                ElseIf form.Options.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Heterogeneous_Catalytic And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    hcount += 1
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

            pp.CurrentMaterialStream = ims
            ppr.CurrentMaterialStream = ims

            Dim N0 As New Dictionary(Of String, Double)
            Dim N As New Dictionary(Of String, Double)
            Dim DNT As New Dictionary(Of String, Double)

            C = New Dictionary(Of String, Double)
            C0 = New Dictionary(Of String, Double)

            Kf = New ArrayList(kcount)
            Kr = New ArrayList(kcount)

            Dim scBC, DHr, Hid_r, Hid_p, Hr, Hr0, Hp, Tin, Tin0, Pin, Pout, T As Double
            Dim BC As String = ""
            Dim tmp As Object
            Dim maxXarr As New ArrayList

            Select Case Me.ReactorOperationMode

                Case OperationMode.Isothermic

                    'reactants Enthalpy before temperature change (kJ/kg * kg/s = kW)

                    Hr0 = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault
                    Tin0 = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault

                    If Me.IsothermalTemperature = 0.0# Then Me.IsothermalTemperature = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault

            End Select

            Tin = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault
            Pin = ims.Fases(0).SPMProperties.pressure.GetValueOrDefault
            Pout = ims.Fases(0).SPMProperties.pressure.GetValueOrDefault - Me.DeltaP.GetValueOrDefault
            ims.Fases(0).SPMProperties.pressure = Pout

            'Reactants Enthalpy (kJ/kg * kg/s = kW)
            Hr = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

            N00.Clear()

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As New Dictionary(Of String, Double)

            'loop through reactions
            Dim rxn As Reaction
            For Each ar As ArrayList In Me.ReactionsSequence.Values

                i = 0
                DHr = 0
                Hid_r = 0
                Hid_p = 0

                Do

                    'process reaction i
                    rxn = form.Options.Reactions(ar(i))

                    'initial mole flows
                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        Select Case rxn.ReactionPhase
                            Case PhaseName.Liquid
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(3).Componentes(sb.CompName).MolarFlow.GetValueOrDefault)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = ims.Fases(3).Componentes(sb.CompName).MolarFlow.GetValueOrDefault
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Fases(3).SPMProperties.volumetric_flow.GetValueOrDefault
                                End If
                            Case PhaseName.Vapor
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(2).Componentes(sb.CompName).MolarFlow.GetValueOrDefault)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = ims.Fases(2).Componentes(sb.CompName).MolarFlow.GetValueOrDefault
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Fases(2).SPMProperties.volumetric_flow.GetValueOrDefault
                                End If
                            Case PhaseName.Mixture
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, ims.Fases(0).Componentes(sb.CompName).MolarFlow.GetValueOrDefault)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = ims.Fases(0).Componentes(sb.CompName).MolarFlow.GetValueOrDefault
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Fases(0).SPMProperties.volumetric_flow.GetValueOrDefault
                                End If
                        End Select

                    Next

                    i += 1

                Loop Until i = ar.Count

                Ri.Clear()
                Rxi.Clear()
                DHRi.Clear()

                i = 0
                Do

                    Dim rx As Double = 0.0#

                    'process reaction i

                    rxn = form.Options.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        C(sb.CompName) = C0(sb.CompName)

                    Next

                    T = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault

                    convfactors = Me.GetConvFactors(rxn, ims)

                    If rxn.ReactionType = ReactionType.Kinetic Then

                        Dim kxf As Double = rxn.A_Forward * Exp(-rxn.E_Forward / (8.314 * T))
                        Dim kxr As Double = rxn.A_Reverse * Exp(-rxn.E_Reverse / (8.314 * T))

                        Dim rxf As Double = 1.0#
                        Dim rxr As Double = 1.0#

                        'kinetic expression

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            rxf *= (C(sb.CompName) * convfactors(sb.CompName)) ^ sb.DirectOrder
                            rxr *= (C(sb.CompName) * convfactors(sb.CompName)) ^ sb.ReverseOrder
                        Next

                        rx = Conversor.ConverterParaSI(rxn.VelUnit, kxf * rxf - kxr * rxr)

                        If Kf.Count - 1 <= i Then
                            Kf.Add(kxf)
                            Kr.Add(kxf)
                        Else
                            Kf(i) = kxf
                            Kr(i) = kxr
                        End If

                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then

                        Dim numval, denmval As Double

                        rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                        rxn.ExpContext.Imports.AddType(GetType(System.Math))
                        rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                        rxn.ExpContext.Variables.Clear()
                        rxn.ExpContext.Variables.Add("T", ims.Fases(0).SPMProperties.temperature.GetValueOrDefault)

                        Dim ir As Integer = 1
                        Dim ip As Integer = 1

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            If sb.StoichCoeff < 0 Then
                                rxn.ExpContext.Variables.Add("R" & ir.ToString, C(sb.CompName) * convfactors(sb.CompName))
                                ir += 1
                            ElseIf sb.StoichCoeff > 0 Then
                                rxn.ExpContext.Variables.Add("P" & ip.ToString, C(sb.CompName) * convfactors(sb.CompName))
                                ip += 1
                            End If
                        Next

                        rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationNumerator)

                        numval = rxn.Expr.Evaluate

                        rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationDenominator)

                        denmval = rxn.Expr.Evaluate

                        rx = Conversor.ConverterParaSI(rxn.VelUnit, numval / denmval)

                    End If

                    If Not Rxi.ContainsKey(rxn.ID) Then
                        Rxi.Add(rxn.ID, rx)
                    Else
                        Rxi(rxn.ID) = rx
                    End If

                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        If Not Ri.ContainsKey(sb.CompName) Then
                            Ri.Add(sb.CompName, 0)
                        End If

                    Next

                    rx = Conversor.ConverterParaSI(rxn.VelUnit, rx)

                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        Ri(sb.CompName) += rx * sb.StoichCoeff / rxn.Components(BC).StoichCoeff

                    Next

                    i += 1

                Loop Until i = ar.Count

                'SOLVE ODEs

                Me.activeAL = Me.ReactionsSequence.IndexOfValue(ar)

                Dim vc(C.Count) As Double
                'vc(1) = Me.Volume
                i = 1
                For Each d As Double In C.Values
                    vc(i) = d
                    i = i + 1
                Next

                Dim bs As New MathEx.ODESolver.bulirschstoer
                bs.DefineFuncDelegate(AddressOf ODEFunc)
                bs.solvesystembulirschstoer(0.0#, 1.0#, vc, Ri.Count, 0.05, 0.000001, True)

                If Double.IsNaN(vc.Sum) Then Throw New Exception(DWSIM.App.GetLocalString("PFRMassBalanceError"))

                C.Clear()
                i = 1
                For Each sb As KeyValuePair(Of String, Double) In C0
                    C(sb.Key) = CDbl(vc(i))
                    i = i + 1
                Next

                i = 0
                Do

                    'process reaction i
                    rxn = form.Options.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        ''comp. conversions
                        If Not Me.ComponentConversions.ContainsKey(sb.CompName) Then
                            Me.ComponentConversions.Add(sb.CompName, 0)
                        End If

                    Next

                    i += 1

                Loop Until i = ar.Count

                For Each sb As String In Me.ComponentConversions.Keys
                    If N0(sb) <> 0.0# Then N(sb) = N0(sb) * (1 - (C0(sb) - C(sb)) / C0(sb)) Else N(sb) = 0.0#
                Next

                For Each sb As String In Me.ComponentConversions.Keys
                    If Not DN.ContainsKey(sb) Then
                        DN.Add(sb, N(sb) - N0(sb))
                    Else
                        DN(sb) = N(sb) - N0(sb)
                    End If
                Next

                For Each sb As String In Me.ComponentConversions.Keys
                    If Not DNT.ContainsKey(sb) Then
                        DNT.Add(sb, DN(sb))
                    Else
                        DNT(sb) += DN(sb)
                    End If
                Next

                'Ideal Gas Reactants Enthalpy (kJ/kg * kg/s = kW)
                Hid_r += 0 'ppr.RET_Hid(298.15, ims.Fases(0).SPMProperties.temperature.GetValueOrDefault, PropertyPackages.Fase.Mixture) * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                'update mole flows/fractions
                Dim Nsum As Double = 0

                'compute new mole flows
                'Nsum = ims.Fases(0).SPMProperties.molarflow.GetValueOrDefault
                For Each s2 As Substancia In ims.Fases(0).Componentes.Values
                    If DN.ContainsKey(s2.Nome) Then
                        Nsum += N(s2.Nome)
                    Else
                        Nsum += s2.MolarFlow.GetValueOrDefault
                    End If
                Next
                For Each s2 As Substancia In ims.Fases(0).Componentes.Values
                    If DN.ContainsKey(s2.Nome) Then
                        s2.FracaoMolar = (ims.Fases(0).Componentes(s2.Nome).MolarFlow.GetValueOrDefault + DN(s2.Nome)) / Nsum
                        s2.MolarFlow = ims.Fases(0).Componentes(s2.Nome).MolarFlow.GetValueOrDefault + DN(s2.Nome)
                    Else
                        s2.FracaoMolar = ims.Fases(0).Componentes(s2.Nome).MolarFlow.GetValueOrDefault / Nsum
                        s2.MolarFlow = ims.Fases(0).Componentes(s2.Nome).MolarFlow.GetValueOrDefault
                    End If
                Next

                ims.Fases(0).SPMProperties.molarflow = Nsum

                Dim mmm As Double = 0
                Dim mf As Double = 0
                For Each s3 As Substancia In ims.Fases(0).Componentes.Values
                    mmm += s3.FracaoMolar.GetValueOrDefault * s3.ConstantProperties.Molar_Weight
                Next
                For Each s3 As Substancia In ims.Fases(0).Componentes.Values
                    s3.FracaoMassica = s3.FracaoMolar.GetValueOrDefault * s3.ConstantProperties.Molar_Weight / mmm
                    s3.MassFlow = s3.FracaoMassica.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault
                    mf += s3.MassFlow.GetValueOrDefault
                Next

                'Ideal Gas Products Enthalpy (kJ/kg * kg/s = kW)
                Hid_p += 0 'ppr.RET_Hid(298.15, ims.Fases(0).SPMProperties.temperature.GetValueOrDefault, PropertyPackages.Fase.Mixture) * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault

                Dim NS As New Dictionary(Of String, Double)

                i = 0
                Do

                    'process reaction i
                    rxn = form.Options.Reactions(ar(i))

                    If NS.ContainsKey(rxn.BaseReactant) Then
                        NS(rxn.BaseReactant) += Rxi(rxn.ID)
                    Else
                        NS.Add(rxn.BaseReactant, Rxi(rxn.ID))
                    End If

                    i += 1

                Loop Until i = ar.Count

                i = 0
                Do

                    'process reaction i
                    rxn = form.Options.Reactions(ar(i))

                    'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                    Select Case Me.ReactorOperationMode
                        Case OperationMode.Adiabatic
                            DHr = rxn.ReactionHeat * Abs(DN(rxn.BaseReactant)) / 1000 * Rxi(rxn.ID) / Ri(rxn.BaseReactant)
                        Case OperationMode.Isothermic
                            DHr += rxn.ReactionHeat * Abs(DN(rxn.BaseReactant)) / 1000 * Rxi(rxn.ID) / Ri(rxn.BaseReactant)
                    End Select

                    DHRi.Add(rxn.ID, DHr)

                    i += 1

                Loop Until i = ar.Count

                ' comp. conversions
                For Each sb As Substancia In ims.Fases(0).Componentes.Values
                    If Me.ComponentConversions.ContainsKey(sb.Nome) Then
                        Me.ComponentConversions(sb.Nome) += -DNT(sb.Nome) / N00(sb.Nome)
                    End If
                Next

                'do a flash calc (calculate final temperature/enthalpy)

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = form.Collections.CLCS_EnergyStreamCollection(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name).Energia.GetValueOrDefault

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = Me.DeltaQ.GetValueOrDefault + Hr + Hid_p - Hid_r - DHr

                        tmp = Me.PropertyPackage.DW_CalcEquilibrio_ISOL(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H, Pout, Hp / ims.Fases(0).SPMProperties.massflow.GetValueOrDefault, Tin)
                        Dim Tout As Double = tmp(2)
                        Me.DeltaT = Tout - Tin

                        ims.Fases(0).SPMProperties.temperature = Tout

                    Case OperationMode.Isothermic

                    Case OperationMode.OutletTemperature

                        Tin = Me.OutletTemperature

                        Me.DeltaT = Tin - Tin0

                        ims.Fases(0).SPMProperties.temperature = Tin

                End Select

                ims.Calculate(True, True)

            Next

            If Me.ReactorOperationMode = OperationMode.Isothermic Or Me.ReactorOperationMode = OperationMode.OutletTemperature Then

                'Products Enthalpy (kJ/kg * kg/s = kW)
                Hp = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault * ims.Fases(0).SPMProperties.massflow.GetValueOrDefault
                'Heat (kW)
                Me.DeltaQ = DHr + Hp - Hr0
                Me.DeltaT = Tin - Tin0

            End If

            Dim ms As DWSIM.SimulationObjects.Streams.MaterialStream
            Dim cp As ConnectionPoint
            Dim mtotal, wtotal As Double

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.Collections.CLCS_MaterialStreamCollection(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Fases(0).SPMProperties.massflow = ims.Fases(0).SPMProperties.massflow.GetValueOrDefault
                    .Fases(0).SPMProperties.massfraction = 1
                    .Fases(0).SPMProperties.temperature = ims.Fases(0).SPMProperties.temperature.GetValueOrDefault
                    .Fases(0).SPMProperties.pressure = ims.Fases(0).SPMProperties.pressure.GetValueOrDefault
                    .Fases(0).SPMProperties.enthalpy = ims.Fases(0).SPMProperties.enthalpy.GetValueOrDefault
                    Dim comp As DWSIM.ClassesBasicasTermodinamica.Substancia
                    mtotal = 0
                    wtotal = 0
                    For Each comp In .Fases(0).Componentes.Values
                        mtotal += ims.Fases(0).Componentes(comp.Nome).FracaoMolar.GetValueOrDefault
                        wtotal += ims.Fases(0).Componentes(comp.Nome).FracaoMassica.GetValueOrDefault
                    Next
                    For Each comp In .Fases(0).Componentes.Values
                        comp.FracaoMolar = ims.Fases(0).Componentes(comp.Nome).FracaoMolar.GetValueOrDefault / mtotal
                        comp.FracaoMassica = ims.Fases(0).Componentes(comp.Nome).FracaoMassica.GetValueOrDefault / wtotal
                        comp.MassFlow = comp.FracaoMassica.GetValueOrDefault * .Fases(0).SPMProperties.massflow.GetValueOrDefault
                        comp.MolarFlow = comp.FracaoMolar.GetValueOrDefault * .Fases(0).SPMProperties.molarflow.GetValueOrDefault
                    Next
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
                .Tipo = TipoObjeto.RCT_CSTR
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Function DeCalculate() As Integer

            Dim form As Global.DWSIM.FormFlowsheet = Me.FlowSheet

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


            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
            With objargs
                .Calculado = False
                .Nome = Me.Nome
                .Tipo = TipoObjeto.RCT_CSTR
            End With

            form.CalculationQueue.Enqueue(objargs)

        End Function

        Public Overrides Sub QTFillNodeItems()

            With Me.QTNodeTableItems

                .Clear()

                .Add(0, New DWSIM.Outros.NodeItem("Delta-P", "", "", 0, 0, ""))
                .Add(1, New DWSIM.Outros.NodeItem("Delta-T", "", "", 1, 0, ""))
                .Add(2, New DWSIM.Outros.NodeItem("Delta-Q", "", "", 2, 0, ""))

            End With

        End Sub

        Public Overrides Sub UpdatePropertyNodes(ByVal su As SistemasDeUnidades.Unidades, ByVal nf As String)

            Me.ShowQuickTable = True

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            If Me.QTNodeTableItems Is Nothing Then
                Me.QTNodeTableItems = New System.Collections.Generic.Dictionary(Of Integer, DWSIM.Outros.NodeItem)
                Me.QTFillNodeItems()
            End If

            With Me.QTNodeTableItems

                Dim valor As String

                If Me.DeltaP.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(0).Value = valor
                .Item(0).Unit = su.spmp_deltaP

                If Me.DeltaT.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_deltaT, Me.DeltaT), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(1).Value = valor
                .Item(1).Unit = su.spmp_deltaT

                If Me.DeltaQ.HasValue Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_heatflow, Me.DeltaQ), nf)
                Else
                    valor = DWSIM.App.GetLocalString("NC")
                End If
                .Item(2).Value = valor
                .Item(2).Unit = su.spmp_heatflow

            End With

        End Sub

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SistemasDeUnidades.Unidades)

            Dim Conversor As New DWSIM.SistemasDeUnidades.Conversor

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                Dim ent, saida, energ As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida = ""
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

                .Item.Add(DWSIM.App.GetLocalString("Correntedesada"), saida, False, DWSIM.App.GetLocalString("Conexes1"), "", True)
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

                If Me.ReactorOperationMode = OperationMode.Isothermic Then
                    valor = Format(Conversor.ConverterDoSI(su.spmp_temperature, Me.IsothermalTemperature), FlowSheet.Options.NumberFormat)
                    .Item.Add(FT(DWSIM.App.GetLocalString("RSCTRIsothermalTemperature"), su.spmp_temperature), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RCSTRPGridItem1Help"), True)
                End If

                valor = Format(Conversor.ConverterDoSI(su.mass, Me.CatalystAmount), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("CSTRCatalystAmount"), su.mass), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("CSTRCatalystAmountDesc"), True)

                valor = Format(Conversor.ConverterDoSI(su.spmp_deltaP, Me.DeltaP.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("Quedadepresso"), su.spmp_deltaP), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("Quedadepressoaplicad6"), True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                valor = Format(Conversor.ConverterDoSI(su.volume, Me.Volume), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetLocalString("RCSTRPGridItem1"), su.volume), valor, False, DWSIM.App.GetLocalString("Parmetrosdeclculo2"), DWSIM.App.GetLocalString("RCSTRPGridItem1Help"), True)
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
                    For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                        valor = Format(dbl.Value * 100, FlowSheet.Options.NumberFormat)
                        If dbl.Value >= 0 Then
                            m.Add(DWSIM.App.GetComponentName(dbl.Key), valor, False, DWSIM.App.GetLocalString("ComponentesConversoes"), DWSIM.App.GetLocalString("RCSTRPGridItem3Help"), True)
                            m.Item(m.Count - 1).IsReadOnly = True
                            m.Item(m.Count - 1).DefaultValue = Nothing
                            m.Item(m.Count - 1).DefaultType = GetType(Nullable(Of Double))
                        End If
                    Next

                    .Item.Add(DWSIM.App.GetLocalString("ComponentesConversoes") & " (%)", m, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString("RCSTRPGridItem2Help"), True)
                    With .Item(.Item.Count - 1)
                        .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With

                    'CustomPropertyCollection
                    Dim m2 As New PropertyGridEx.CustomPropertyCollection()
                    For Each dbl As KeyValuePair(Of String, Double) In Rxi
                        m2.Add(form.Options.Reactions(dbl.Key).Name, Format(dbl.Value / C(form.Options.Reactions(dbl.Key).BaseReactant), FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                        m2.Item(m2.Count - 1).IsReadOnly = True
                        m2.Item(m2.Count - 1).DefaultValue = Nothing
                        m2.Item(m2.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next

                    .Item.Add(FT(DWSIM.App.GetLocalString("ReactionExtents"), su.spmp_molarflow), m2, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                    With .Item(.Item.Count - 1)
                        .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With

                    'CustomPropertyCollection
                    Dim m3 As New PropertyGridEx.CustomPropertyCollection()
                    For Each dbl As KeyValuePair(Of String, Double) In Rxi
                        m3.Add(form.Options.Reactions(dbl.Key).Name, Format(dbl.Value / C(form.Options.Reactions(dbl.Key).BaseReactant) / Me.Volume, FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("ReactionExtents"), DWSIM.App.GetLocalString(""), True)
                        m3.Item(m3.Count - 1).IsReadOnly = True
                        m3.Item(m3.Count - 1).DefaultValue = Nothing
                        m3.Item(m3.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next

                    .Item.Add(FT(DWSIM.App.GetLocalString("ReactionRates"), su.reac_rate), m3, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
                    With .Item(.Item.Count - 1)
                        .IsReadOnly = True
                        .IsBrowsable = True
                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                    End With

                    'CustomPropertyCollection
                    Dim m4 As New PropertyGridEx.CustomPropertyCollection()
                    For Each dbl As KeyValuePair(Of String, Double) In DHRi
                        m4.Add(form.Options.Reactions(dbl.Key).Name, Format(Conversor.ConverterDoSI(su.spmp_heatflow, dbl.Value), FlowSheet.Options.NumberFormat), False, DWSIM.App.GetLocalString("ReactionHeats"), DWSIM.App.GetLocalString(""), True)
                        m4.Item(m4.Count - 1).IsReadOnly = True
                        m4.Item(m4.Count - 1).DefaultValue = Nothing
                        m4.Item(m4.Count - 1).DefaultType = GetType(Nullable(Of Double))
                    Next

                    .Item.Add(FT(DWSIM.App.GetLocalString("ReactionHeats"), su.spmp_heatflow), m4, True, DWSIM.App.GetLocalString("Resultados3"), DWSIM.App.GetLocalString(""), True)
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
                    'PROP_HT_0	Pressure Drop
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
                        proplist.Add("PROP_CS_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 0
                        proplist.Add("PROP_CS_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_CS_" + CStr(i))
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


