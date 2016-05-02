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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics.BaseClasses
Imports Ciloci.Flee
Imports System.Math
Imports System.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics
Imports DWSIM.MathOps

Namespace Reactors

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
        Dim Rxi As New Dictionary(Of String, Double)
        Dim RxiT As New Dictionary(Of String, Double)
        Dim DHRi As New Dictionary(Of String, Double)

        Dim activeAL As Integer = 0

        <System.NonSerialized()> Dim form As IFlowsheet
        <System.NonSerialized()> Dim ims As MaterialStream
        <System.NonSerialized()> Dim pp As PropertyPackages.PropertyPackage
        <System.NonSerialized()> Dim ppr As New PropertyPackages.RaoultPropertyPackage()

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

            '
            '

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

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description



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

        Private Property ResidenceTime As Double

        Public Sub ODEFunc(ByVal y As Double(), ByRef dy As Double())

            Dim conv As New SystemsOfUnits.Converter

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
                rxn = FlowSheet.Reactions(ar(i))
                For Each sb As ReactionStoichBase In rxn.Components.Values
                    Ri(sb.CompName) = 0
                Next
                i += 1
            Loop Until i = ar.Count

            i = 0
            Do

                'process reaction i
                rxn = FlowSheet.Reactions(ar(i))
                BC = rxn.BaseReactant
                scBC = rxn.Components(BC).StoichCoeff

                j = 1
                For Each sb As ReactionStoichBase In rxn.Components.Values

                    C(sb.CompName) = y(j)
                    j = j + 1

                Next

                Dim T As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault

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
                    Rxi(rxn.ID) = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, rx)

                    Kf(i) = kxf
                    Kr(i) = kxr

                ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then

                    Dim numval, denmval As Double

                    rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                    rxn.ExpContext.Imports.AddType(GetType(System.Math))

                    rxn.ExpContext.Variables.Clear()
                    rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)
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

                    rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, numval / denmval)

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

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim conv As New SystemsOfUnits.Converter

            If Ri Is Nothing Then Ri = New Dictionary(Of String, Double)
            If Rxi Is Nothing Then Rxi = New Dictionary(Of String, Double)
            If DHRi Is Nothing Then DHRi = New Dictionary(Of String, Double)
            If DN Is Nothing Then DN = New Dictionary(Of String, Double)
            If N00 Is Nothing Then N00 = New Dictionary(Of String, Double)
            If Me.Conversions Is Nothing Then Me.m_conversions = New Dictionary(Of String, Double)
            If Me.ComponentConversions Is Nothing Then Me.m_componentconversions = New Dictionary(Of String, Double)

            form = Me.FlowSheet

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            End If

            ims = DirectCast(form.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Clone
            pp = Me.PropertyPackage
            ppr = New PropertyPackages.RaoultPropertyPackage()

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
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Kinetic And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    kcount += 1
                ElseIf FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Heterogeneous_Catalytic And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    hcount += 1
                End If
            Next

            'order reactions
            Dim i As Integer
            i = 0
            Dim maxrank As Integer = 0
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If rxnsb.Rank > maxrank And Me.Reactions.Contains(rxnsb.ReactionID) Then maxrank = rxnsb.Rank
            Next

            'ordering of parallel reactions
            i = 0
            Dim arr As New ArrayList
            Do
                arr = New ArrayList
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
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
            Dim tmp As IFlashCalculationResult
            Dim maxXarr As New ArrayList

            Select Case Me.ReactorOperationMode

                Case OperationMode.Isothermic

                    'reactants Enthalpy before temperature change (kJ/kg * kg/s = kW)

                    Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault
                    Tin0 = ims.Phases(0).Properties.temperature.GetValueOrDefault

                    If Me.IsothermalTemperature = 0.0# Then Me.IsothermalTemperature = ims.Phases(0).Properties.temperature.GetValueOrDefault

            End Select

            Tin = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Pin = ims.Phases(0).Properties.pressure.GetValueOrDefault
            Pout = ims.Phases(0).Properties.pressure.GetValueOrDefault - Me.DeltaP.GetValueOrDefault
            ims.Phases(0).Properties.pressure = Pout

            'Reactants Enthalpy (kJ/kg * kg/s = kW)
            Hr = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

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
                    rxn = FlowSheet.Reactions(ar(i))

                    Dim m0 As Double = 0.0#

                    'initial mole flows
                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        Select Case rxn.ReactionPhase
                            Case PhaseName.Liquid
                                m0 = ims.Phases(3).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                If m0 = 0.0# Then m0 = 0.0000000001
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, m0)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = m0
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault
                                End If
                            Case PhaseName.Vapor
                                m0 = ims.Phases(2).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                If m0 = 0.0# Then m0 = 0.0000000001
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, m0)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = m0
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault
                                End If
                            Case PhaseName.Mixture
                                m0 = ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                If m0 = 0.0# Then m0 = 0.0000000001
                                If Not N0.ContainsKey(sb.CompName) Then
                                    N0.Add(sb.CompName, m0)
                                    N00.Add(sb.CompName, N0(sb.CompName))
                                    N.Add(sb.CompName, N0(sb.CompName))
                                    C0.Add(sb.CompName, N0(sb.CompName) / ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault)
                                Else
                                    N0(sb.CompName) = m0
                                    N(sb.CompName) = N0(sb.CompName)
                                    C0(sb.CompName) = N0(sb.CompName) / ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault
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

                    rxn = FlowSheet.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        C(sb.CompName) = C0(sb.CompName)

                    Next

                    T = ims.Phases(0).Properties.temperature.GetValueOrDefault

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

                        rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, kxf * rxf - kxr * rxr)

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
                        rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)

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

                        rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, numval / denmval)

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

                    rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, rx)

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

                If Double.IsNaN(vc.Sum) Then Throw New Exception(FlowSheet.GetTranslatedString("PFRMassBalanceError"))

                C.Clear()
                i = 1
                For Each sb As KeyValuePair(Of String, Double) In C0
                    C(sb.Key) = Convert.ToDouble(vc(i))
                    i = i + 1
                Next

                i = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))
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
                Hid_r += 0 'ppr.RET_Hid(298.15, ims.Phases(0).Properties.temperature.GetValueOrDefault, PropertyPackages.Phase.Mixture) * ims.Phases(0).Properties.massflow.GetValueOrDefault

                'update mole flows/fractions
                Dim Nsum As Double = 0

                'compute new mole flows
                'Nsum = ims.Phases(0).Properties.molarflow.GetValueOrDefault
                For Each s2 As Compound In ims.Phases(0).Compounds.Values
                    If DN.ContainsKey(s2.Name) Then
                        Nsum += N(s2.Name)
                    Else
                        Nsum += s2.MolarFlow.GetValueOrDefault
                    End If
                Next
                For Each s2 As Compound In ims.Phases(0).Compounds.Values
                    If DN.ContainsKey(s2.Name) Then
                        s2.MoleFraction = (ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault + DN(s2.Name)) / Nsum
                        s2.MolarFlow = ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault + DN(s2.Name)
                    Else
                        s2.MoleFraction = ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault / Nsum
                        s2.MolarFlow = ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault
                    End If
                Next

                ims.Phases(0).Properties.molarflow = Nsum

                Dim mmm As Double = 0
                Dim mf As Double = 0
                For Each s3 As Compound In ims.Phases(0).Compounds.Values
                    mmm += s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight
                Next
                For Each s3 As Compound In ims.Phases(0).Compounds.Values
                    s3.MassFraction = s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight / mmm
                    s3.MassFlow = s3.MassFraction.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault
                    mf += s3.MassFlow.GetValueOrDefault
                Next

                'Ideal Gas Products Enthalpy (kJ/kg * kg/s = kW)
                Hid_p += 0 'ppr.RET_Hid(298.15, ims.Phases(0).Properties.temperature.GetValueOrDefault, PropertyPackages.Phase.Mixture) * ims.Phases(0).Properties.massflow.GetValueOrDefault

                Dim NS As New Dictionary(Of String, Double)

                i = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))

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
                    rxn = FlowSheet.Reactions(ar(i))

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
                For Each sb As Compound In ims.Phases(0).Compounds.Values
                    If Me.ComponentConversions.ContainsKey(sb.Name) Then
                        Me.ComponentConversions(sb.Name) += -DNT(sb.Name) / N00(sb.Name)
                    End If
                Next

                'do a flash calc (calculate final temperature/enthalpy)

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        Me.DeltaQ = GetInletEnergyStream(1).EnergyFlow.GetValueOrDefault

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = Me.DeltaQ.GetValueOrDefault + Hr + Hid_p - Hid_r - DHr

                        tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, Pout, Hp / ims.Phases(0).Properties.massflow.GetValueOrDefault, Tin)
                        Dim Tout As Double = tmp.CalculatedTemperature
                        Me.DeltaT = Tout - Tin

                        ims.Phases(0).Properties.temperature = Tout

                    Case OperationMode.Isothermic

                    Case OperationMode.OutletTemperature

                        Tin = Me.OutletTemperature

                        Me.DeltaT = Tin - Tin0

                        ims.Phases(0).Properties.temperature = Tin

                End Select

                ims.Calculate(True, True)

            Next

            RxiT.Clear()
            DHRi.Clear()

            For Each ar As ArrayList In Me.ReactionsSequence.Values

                i = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))

                    RxiT.Add(rxn.ID, (N(rxn.BaseReactant) - N00(rxn.BaseReactant)) / rxn.Components(rxn.BaseReactant).StoichCoeff / 1000)
                    DHRi.Add(rxn.ID, rxn.ReactionHeat * RxiT(rxn.ID) * rxn.Components(rxn.BaseReactant).StoichCoeff / 1000)

                    i += 1

                Loop Until i = ar.Count

            Next

            ResidenceTime = Volume / ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            If Me.ReactorOperationMode = OperationMode.Isothermic Or Me.ReactorOperationMode = OperationMode.OutletTemperature Then

                'Products Enthalpy (kJ/kg * kg/s = kW)
                Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault
                'Heat (kW)
                Me.DeltaQ = DHr + Hp - Hr0
                Me.DeltaT = Tin - Tin0

            End If

            Dim ms As MaterialStream
            Dim cp As ConnectionPoint
            Dim mtotal, wtotal As Double

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.massflow = ims.Phases(0).Properties.massflow.GetValueOrDefault
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.temperature = ims.Phases(0).Properties.temperature.GetValueOrDefault
                    .Phases(0).Properties.pressure = ims.Phases(0).Properties.pressure.GetValueOrDefault
                    .Phases(0).Properties.enthalpy = ims.Phases(0).Properties.enthalpy.GetValueOrDefault
                    Dim comp As BaseClasses.Compound
                    mtotal = 0
                    wtotal = 0
                    For Each comp In .Phases(0).Compounds.Values
                        mtotal += ims.Phases(0).Compounds(comp.Name).MoleFraction.GetValueOrDefault
                        wtotal += ims.Phases(0).Compounds(comp.Name).MassFraction.GetValueOrDefault
                    Next
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction.GetValueOrDefault / mtotal
                        comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction.GetValueOrDefault / wtotal
                        comp.MassFlow = comp.MassFraction.GetValueOrDefault * .Phases(0).Properties.massflow.GetValueOrDefault
                        comp.MolarFlow = comp.MoleFraction.GetValueOrDefault * .Phases(0).Properties.molarflow.GetValueOrDefault
                    Next
                End With
            End If

            'Corrente de EnergyFlow - atualizar valor da potencia (kJ/s)
            With GetInletEnergyStream(1)
                .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer = 0

            Dim ms As MaterialStream
            Dim cp As ConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = form.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .Phases(0).Properties.temperature = Nothing
                    .Phases(0).Properties.pressure = Nothing
                    .Phases(0).Properties.enthalpy = Nothing
                    Dim comp As BaseClasses.Compound
                    j = 0
                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = 0
                        comp.MassFraction = 0
                        j += 1
                    Next
                    .Phases(0).Properties.massflow = Nothing
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.molarfraction = 1
                    .GraphicObject.Calculated = False
                End With
            End If

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                Case 1
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.ResidenceTime)
                Case 2
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.volume, Me.Volume)
                Case 3
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                Case 4
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
            End Select

            Return value
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_CS_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_CS_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 4
                        proplist.Add("PROP_CS_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    Me.ResidenceTime = SystemsOfUnits.Converter.ConvertToSI(su.time, propval)
                Case 2
                    Me.Volume = SystemsOfUnits.Converter.ConvertToSI(su.volume, propval)
                Case 3
                    Me.DeltaT = SystemsOfUnits.Converter.ConvertToSI(su.deltaT, propval)
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    value = su.deltaP
                Case 1
                    value = su.time
                Case 2
                    value = su.volume
                Case 3
                    value = su.deltaT
                Case 4
                    value = su.heatflow
            End Select

            Return value
        End Function

        Public Overrides Sub DisplayEditForm()

        End Sub

        Public Overrides Sub UpdateEditForm()

        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.re_cstr_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                Return "Modelo de um CSTR, suporta reações Cinéticas e Catalíticas Heterogêneas"
            Else
                Return "CSTR model, supports Kinetic and HetCat reactions"
            End If
        End Function

        Public Overrides Function GetDisplayName() As String
            If GlobalSettings.Settings.CurrentCulture = "pt-BR" Then
                Return "Continous Stirred Tank Reactor (CSTR)"
            Else
                Return "Continous Stirred Tank Reactor (CSTR)"
            End If
        End Function
    End Class

End Namespace


