'    CSTR Calculation Routines 
'    Copyright 2008-2016 Daniel Wagner O. de Medeiros; Gregor Reichert
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

        Public Enum EReactorMode
            SingleOutlet
            TwoOutlets
        End Enum

        Protected m_vol As Double
        Protected m_headspace As Double
        Protected m_isotemp As Double

        Dim C0 As Dictionary(Of String, Double)
        Dim C As Dictionary(Of String, Double)
        Dim Ri As Dictionary(Of String, Double)
        Dim Kf, Kr As ArrayList
        Dim N00, N0 As Dictionary(Of String, Double)
        Dim Rxi As New Dictionary(Of String, Double)
        Public RxiT As New Dictionary(Of String, Double)
        Public DHRi As New Dictionary(Of String, Double)

        Dim activeAL As Integer = 0

        <NonSerialized> <Xml.Serialization.XmlIgnore> Dim f As EditingForm_ReactorCSTR

        <System.NonSerialized()> Dim ims As MaterialStream

        Public Property ResidenceTimeL As Double = 0.0# 'Liquid/Solid residence time
        Public Property ResidenceTimeV As Double = 0.0# 'Vapour residence time

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

        Public Property Headspace() As Double
            Get
                Return m_headspace
            End Get
            Set(ByVal value As Double)
                m_headspace = value
            End Set
        End Property

        Public Property CatalystAmount As Double = 0.0#

        Public Sub New()

            MyBase.New()

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

            N00 = New Dictionary(Of String, Double)
            N0 = New Dictionary(Of String, Double)
            C0 = New Dictionary(Of String, Double)
            C = New Dictionary(Of String, Double)
            Ri = New Dictionary(Of String, Double)
            Rxi = New Dictionary(Of String, Double)
            DHRi = New Dictionary(Of String, Double)
            Kf = New ArrayList
            Kr = New ArrayList

        End Sub

        Public Overrides Function CloneXML() As Object
            Return New Reactor_CSTR().LoadData(Me.SaveData)
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_CSTR)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Function ODEFunc(ByVal x As Double, ByVal y As Double()) As Double()

            'this function calculates the change (dy) of compound mole amounts (y) for the current volume (x).
            'x = m3
            'y and dy = mol/s

            Dim conv As New SystemsOfUnits.Converter

            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim scBC As Double = 0
            Dim BC As String = ""

            'calculate concentrations in mol/m3 = mol/s * s/m3
            j = 0
            For Each s As String In N00.Keys
                C(s) = y(j) * ResidenceTimeL / Volume
                j = j + 1
            Next

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As New Dictionary(Of String, Double)

            'loop through reactions
            Dim rxn As Reaction
            Dim ar = Me.ReactionsSequence(activeAL)

            i = 0
            Do
                'process reaction i
                rxn = FlowSheet.Reactions(ar(i))
                For Each sb As ReactionStoichBase In rxn.Components.Values
                    'initialize reaction rate
                    Ri(sb.CompName) = 0.0#
                Next
                i += 1
            Loop Until i = ar.Count

            i = 0
            Do

                'process reaction i
                rxn = FlowSheet.Reactions(ar(i))
                BC = rxn.BaseReactant
                scBC = rxn.Components(BC).StoichCoeff

                Dim T As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault

                Dim rx As Double = 0.0#

                convfactors = Me.GetConvFactors(rxn, ims)

                If rxn.ReactionType = ReactionType.Kinetic Then

                    'calculate reaction constants
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

                    'calculate reaction rate
                    rx = kxf * rxf - kxr * rxr

                    'convert to internal SI units
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

                    'calculate reaction rate & convert to internal SI units
                    rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, numval / denmval)

                    Rxi(rxn.ID) = rx

                End If

                'calculate total compound consumption/generation rates in mol/m3/s
                For Each sb As ReactionStoichBase In rxn.Components.Values
                    If rxn.ReactionType = ReactionType.Kinetic Then
                        Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / rxn.Components(BC).StoichCoeff
                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then
                        Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / rxn.Components(BC).StoichCoeff * Me.CatalystAmount
                    End If

                Next

                i += 1

            Loop Until i = ar.Count

            Dim dy(Ri.Count - 1) As Double

            'calculate compound mole flows change

            j = 0
            For Each kv As KeyValuePair(Of String, Double) In Ri
                dy(j) = -kv.Value * x 'x = volume
                j += 1
            Next

            FlowSheet.CheckStatus()

            'return dNi = dy

            Return dy

        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)
            NewCalculate(args)
            'OldCalculate(args)
        End Sub
        Public Sub NewCalculate(Optional ByVal args As Object = Nothing)

            'ims-stream (internal material stream) to be used during internal calculations
            'Clone inlet stream as initial estimation
            ims = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Clone

            PropertyPackage.CurrentMaterialStream = ims
            ims.SetPropertyPackage(PropertyPackage)
            ims.SetFlowsheet(Me.FlowSheet)
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

            N0 = New Dictionary(Of String, Double) 'inlet mole flows
            C0 = New Dictionary(Of String, Double) 'inlet mole concentrations
            C = New Dictionary(Of String, Double) 'current mole concentrations
            Ri = New Dictionary(Of String, Double) 'compound consumption rates

            DHRi = New Dictionary(Of String, Double) 'reaction heats
            Kf = New ArrayList 'K forward reaction
            Kr = New ArrayList 'K reverse reaction
            Rxi = New Dictionary(Of String, Double)
            Dim BC As String = "" 'Base Component of Reaction
            Dim ErrCode As String

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As New Dictionary(Of String, Double)

            'scBC = stoichiometric coeff of the base comp
            'DHr = reaction heat (total)
            'Hr = reaction enthalpy
            'Hr0 = initial reactant enthalpy
            'Hp = products enthalpy
            Dim scBC, DHr, Hr, Hr0, Hp, T, T0, P, P0, W, Q, QL, QS, QV, Qr, Rx, IErr, OErr As Double
            Dim ReactorMode As EReactorMode = EReactorMode.SingleOutlet
            Dim RP As Integer 'Reaction phase ID
            Dim dT, MaxChange As Double 'Time step in seconds; MaxChange = max relative change of a component
            Dim i, NIter As Integer
            Dim NC As Integer = ims.Phases(0).Compounds.Count 'Number of components
            Dim CompNames(NC - 1) As String 'ComponentNames
            Dim MM(NC - 1) As Double 'Mol masses of components
            Dim LC(NC - 1) As Double 'Composition of last iteration
            Dim Nin(NC - 1), Nout(NC - 1), NReac(NC - 1), Mout(NC - 1), RComp(NC - 1), dB(NC - 1), dN(NC - 1), dRT(NC - 1), Y(NC - 1), M(NC - 1) As Double
            Dim TR(NC - 1) As Double

            m_conversions = New Dictionary(Of String, Double) 'Conversion of reactions
            m_componentconversions = New Dictionary(Of String, Double) 'Conversion of components

            Dim conv As New SystemsOfUnits.Converter
            Dim rxn As Reaction

            'Check streams beeing attached
            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedeenerg17"))
            End If

            'Check Reaction type, Base components and reaction volume
            If FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Count = 0 Then Throw New Exception("No reaction defined")
            ErrCode = "No kinetic or catalytic reaction found"
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                rxn = FlowSheet.Reactions(rxnsb.ReactionID)
                If (rxn.ReactionType = ReactionType.Kinetic Or rxn.ReactionType = ReactionType.Heterogeneous_Catalytic) And rxnsb.IsActive Then
                    BC = rxn.BaseReactant
                    If BC = "" Then
                        ErrCode = "No Base Component defined for reaction " & rxn.Name
                        Exit For
                    End If

                    'Check if reaction has a volume 
                    If (rxn.ReactionPhase = PhaseName.Liquid And Volume <= 0) Or
                        (rxn.ReactionPhase = PhaseName.Vapor And Headspace <= 0) Or
                        (rxn.ReactionPhase = PhaseName.Mixture And Volume + Headspace <= 0) Then
                        ErrCode = "No reactor volume defined"
                    Else
                        ErrCode = ""
                    End If
                    Exit For
                End If
            Next
            If ErrCode <> "" Then Throw New Exception(ErrCode)

            'Check reactor mode.
            'Mode 0 (without vapour outlet):    Complete output is leaving through Outlet Stream 1. Phase volumes for reactions depend on phase volumetric fractions.
            '                                   Residence time liquid (and vapour) is Volume (without head space) divided by total volume flow.
            'Mode 1 (with vapour outlet):       Vapour phase reactions take place in headspace volume only. Reactor volume is volume of liquid+solid phases
            '                                   Residence time is calculated for reactor volume and headspace separately
            If Me.GraphicObject.OutputConnectors(1).IsAttached Then ReactorMode = EReactorMode.TwoOutlets

            'Initialisations
            Q = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault 'Mixture
            QV = ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault 'Vapour
            QL = ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault 'Liquid
            QS = ims.Phases(7).Properties.volumetric_flow.GetValueOrDefault 'Solid

            If QL > 0 Then
                ResidenceTimeL = Volume / QL
            Else
                ResidenceTimeL = 0
            End If
            If ReactorMode = EReactorMode.TwoOutlets And QV > 0 Then
                ResidenceTimeV = Headspace / QV
            Else
                ResidenceTimeV = ResidenceTimeL
            End If
            dT = ResidenceTimeL / 10 'initial time step

            CompNames = ims.PropertyPackage.RET_VNAMES 'Component names
            MM = ims.PropertyPackage.RET_VMM 'Component molar wheights
            LC = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture) 'Component molar fractions

            'Calculate initial inventory of components in reactor
            For i = 0 To NC - 1
                Nin(i) = ims.Phases(0).Compounds(CompNames(i)).MolarFlow
                Nout(i) = Nin(i)
                If ReactorMode = EReactorMode.SingleOutlet Then
                    NReac(i) = Me.Volume * ims.Phases(0).Compounds(CompNames(i)).Molarity 'global composition; headspace is ignored
                Else
                    NReac(i) = Me.Headspace * ims.Phases(2).Compounds(CompNames(i)).Molarity 'vapour in headspace
                    If QL + QS > 0 Then NReac(i) += Volume * QL / (QL + QS) * ims.Phases(1).Compounds(CompNames(i)).Molarity 'liqud phase
                    If QL + QS > 0 Then NReac(i) += Volume * QS / (QL + QS) * ims.Phases(7).Compounds(CompNames(i)).Molarity 'liqud phase
                End If
            Next

            P0 = ims.Phases(0).Properties.pressure.GetValueOrDefault
            P = P0 - DeltaP.GetValueOrDefault
            ims.Phases(0).Properties.pressure = P

            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Select Case Me.ReactorOperationMode
                Case OperationMode.Isothermic, OperationMode.Adiabatic
                    T = ims.Phases(0).Properties.temperature.GetValueOrDefault
                Case OperationMode.OutletTemperature
                    T = Me.OutletTemperature
            End Select

            W = ims.Phases(0).Properties.massflow.GetValueOrDefault
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W 'reactands enthalpy

            IErr = 1

            '====================================================
            '==== Start of iteration loop =======================
            '====================================================
            'Assume output composition as input composition initially
            'Then do molar balances for every component for small time steps until composition becomes stable
            'Time step: Moles in reactor(t+dT)= Moles in reactor (t) + Feed - consumption (composition,temperature) - Output(composition) 

            Do

                'initialise component reaction rates
                Ri.Clear()
                For Each comp In ims.Phases(0).Compounds.Values
                    Ri.Add(comp.Name, 0)
                Next

                RxiT.Clear()

                Q = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault 'Mixture
                QV = ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault 'Vapour
                QL = ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault 'Liquid
                QS = ims.Phases(7).Properties.volumetric_flow.GetValueOrDefault 'Solid

                If ReactorMode = EReactorMode.SingleOutlet Then
                    ResidenceTimeL = Volume / Q
                    ResidenceTimeV = ResidenceTimeL
                Else
                    If QL + QS > 0 Then
                        ResidenceTimeL = Volume / (QL + QS)
                    Else
                        ResidenceTimeL = 0
                    End If

                    If QV > 0 Then
                        ResidenceTimeV = Headspace / QV
                    Else
                        ResidenceTimeV = 0
                    End If
                End If

                DHr = 0.0# 'Enthalpy change due to reactions
                DHRi.Clear()

                'Run through all reactions of Reaction Set to calculate component consumption/production rates at actual
                'composition and temperature.
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values

                    If rxnsb.IsActive = False Then Continue For
                    rxn = FlowSheet.Reactions(rxnsb.ReactionID)
                    If rxn.ReactionType <> ReactionType.Kinetic And rxn.ReactionType <> ReactionType.Heterogeneous_Catalytic Then Continue For

                    BC = rxn.BaseReactant
                    scBC = Math.Abs(rxn.Components(BC).StoichCoeff)

                    convfactors = Me.GetConvFactors(rxn, ims)

                    'Read component concentrations in phase of reaction 

                    C.Clear()
                    For Each comp As Compound In ims.Phases(RP).Compounds.Values
                        C.Add(comp.Name, comp.Molarity) 'C: mol/m³
                    Next

                    'Qr = reaction volume where actual reaction takes place
                    Select Case rxn.ReactionPhase
                        Case PhaseName.Liquid
                            RP = 1 'reacting phase ID = overall Liquid
                            If ReactorMode = EReactorMode.SingleOutlet Then
                                Qr = QL / Q * Me.Volume
                            Else
                                Qr = QL / (QL + QS) * Me.Volume
                            End If

                        Case PhaseName.Vapor
                            RP = 2
                            If ReactorMode = EReactorMode.SingleOutlet Then
                                Qr = QV / Q * Me.Volume
                            Else
                                Qr = Me.Headspace
                            End If

                        Case PhaseName.Mixture
                            RP = 0
                            Qr = Me.Volume + Me.Headspace

                        Case PhaseName.Solid
                            RP = 7
                            If ReactorMode = EReactorMode.SingleOutlet Then
                                Qr = QS / Q * Me.Volume
                            Else
                                Qr = QS / (QL + QS) * Me.Volume
                            End If

                    End Select

                    If rxn.ReactionType = ReactionType.Kinetic Then

                        'calculate reaction constants
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

                        'calculate total reaction rate (units of reaction definition)
                        Rx = kxf * rxf - kxr * rxr

                        'convert to internal SI units - RxiT: mol/s
                        Rxi(rxn.ID) = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, Rx)
                        RxiT.Add(rxn.ID, Rxi(rxn.ID) / scBC * Qr)

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

                        'calculate reaction rate & convert to internal SI units
                        Rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, numval / denmval)

                        Rxi(rxn.ID) = Rx
                        RxiT.Add(rxn.ID, Rxi(rxn.ID) / scBC)

                    End If

                    'calculate total compound production rates - Ri: mol/s
                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If rxn.ReactionType = ReactionType.Kinetic Then
                            Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / scBC * Qr
                        ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then
                            Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / scBC * CatalystAmount
                        End If
                    Next

                    'calculate heat of reaction
                    'Reaction Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                    'kW (kJ/s) = kJ/kmol * mol/s * 0.001 mol/kmol
                    Hr = rxn.ReactionHeat * RxiT(rxn.ID) * 0.001 / scBC
                    DHRi.Add(rxn.ID, Hr)
                    DHr += Hr 'Total Heat released 

                Next

                'Calculate composition of mixture in reactor
                'Doing molar balance, calculate new component inventory of reactor after time step
                Ri.Values.CopyTo(RComp, 0)
                dB = Nin.AddY(RComp).SubtractY(Nout) 'Balance dB: mol/s

                'Limit time step for fast reactions
                For i = 0 To NC - 1
                    If dB(i) < 0 Then
                        'reaction step is limited to consumption of 80% of component amount in reactor
                        dRT(i) = -NReac(i) / dB(i) * 0.8
                        If dRT(i) < dT Then dT = dRT(i)
                    Else
                        dRT(i) = 0
                    End If
                Next

                'amount of component change due to reactions during time step dN: mol
                dN = dB.MultiplyConstY(dT)
                TR = dN.DivideY(NReac.AddConstY(0.001)) 'Calculate relative consumption of components
                NReac = NReac.AddY(dN) 'calculate new inventory of components in reactor

                'Time step is over! Now calculate new compositions
                Y = NReac.NormalizeY 'molar fraction
                M = Y.MultiplyY(MM).NormalizeY 'mass fraction
                Mout = M.MultiplyConstY(W) 'total components massflow
                Nout = Mout.DivideY(MM).MultiplyConstY(1000) 'total components molar flow

                'Update iteration variables
                OErr = IErr
                IErr = Y.SubtractY(LC).AbsSumY 'Calculate difference to last composition
                Y.CopyTo(LC, 0) 'Save composition as last composition (LC) for next iteration
                NIter += 1

                'Accelerate calculations by increasing time step up to residence time as maximum

                MaxChange = -TR.MinY
                If MaxChange < 0.3 Then
                    dT *= 1.2
                End If
                If dT > 0.2 * ResidenceTimeL Then dT = 0.2 * ResidenceTimeL

                'Transfer calculation results to IMS-stream and recalculate stream
                i = 0
                For Each Comp In ims.Phases(0).Compounds
                    Comp.Value.MoleFraction = Y(i)
                    Comp.Value.MassFraction = M(i)
                    i += 1
                Next

                ims.Phases(0).Properties.molarflow = Nout.SumY
                ims.Phases(0).Properties.temperature = T
                ims.Phases(0).Properties.pressure = P

                Select Case Me.ReactorOperationMode
                    Case OperationMode.Adiabatic
                        ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                        Hp = Hr0 - DHr 'Products Enthalpy (kJ/kg * kg/s = kW)
                        ims.Phases(0).Properties.enthalpy = Hp / W

                    Case Else
                        ims.SpecType = StreamSpec.Temperature_and_Pressure
                End Select

                ims.Calculate(True, True)

                T = ims.Phases(0).Properties.temperature 'read temperature -> PH-Flash

                If NIter > 100 Then Throw New Exception(FlowSheet.GetTranslatedString("Nmeromximodeiteraesa3"))

            Loop Until IErr < 0.000001 Or MaxChange < 0.0001 'repeat until composition is constant

            '====================================================
            '==== Transfer information to output stream =========
            '====================================================
out:        Dim ms1, ms2 As MaterialStream

            If ReactorMode = EReactorMode.SingleOutlet Then 'only a single outlet
                ms1 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                With ms1
                    .SpecType = ims.SpecType
                    .Phases(0).Properties.massflow = ims.Phases(0).Properties.massflow.GetValueOrDefault
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    .Phases(0).Properties.enthalpy = ims.Phases(0).Properties.enthalpy.GetValueOrDefault

                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = ims.Phases(0).Compounds(comp.Name).MoleFraction.GetValueOrDefault
                        comp.MassFraction = ims.Phases(0).Compounds(comp.Name).MassFraction.GetValueOrDefault
                        comp.MassFlow = comp.MassFraction.GetValueOrDefault * .Phases(0).Properties.massflow.GetValueOrDefault
                        comp.MolarFlow = comp.MoleFraction.GetValueOrDefault * .Phases(0).Properties.molarflow.GetValueOrDefault
                    Next

                    .Phases(0).Properties.pressure = P
                End With
            Else
                'two outlets: Liquid+Solid->Outlet1 / Vapour->Outlet2
                ms1 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                ms2 = FlowSheet.SimulationObjects(Me.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)

                'Vapour outlet
                With ms2
                    .SpecType = ims.SpecType
                    .Phases(0).Properties.massflow = ims.Phases(2).Properties.massflow.GetValueOrDefault
                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    .Phases(0).Properties.enthalpy = ims.Phases(2).Properties.enthalpy.GetValueOrDefault

                    For Each comp In .Phases(0).Compounds.Values
                        comp.MoleFraction = ims.Phases(2).Compounds(comp.Name).MoleFraction.GetValueOrDefault
                        comp.MassFraction = ims.Phases(2).Compounds(comp.Name).MassFraction.GetValueOrDefault
                        comp.MassFlow = comp.MassFraction.GetValueOrDefault * .Phases(2).Properties.massflow.GetValueOrDefault
                        comp.MolarFlow = comp.MoleFraction.GetValueOrDefault * .Phases(2).Properties.molarflow.GetValueOrDefault
                    Next

                    .Phases(0).Properties.pressure = P
                End With

                'Liquid/Solid outlet
                With ms1
                    .SpecType = ims.SpecType
                    .Phases(0).Properties.massflow = ims.Phases(0).Properties.massflow.GetValueOrDefault - ims.Phases(2).Properties.massflow.GetValueOrDefault
                    .Phases(0).Properties.molarflow = ims.Phases(0).Properties.molarflow.GetValueOrDefault - ims.Phases(2).Properties.molarflow.GetValueOrDefault

                    .Phases(0).Properties.massfraction = 1
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Hp = ims.Phases(3).Properties.enthalpy.GetValueOrDefault * ims.Phases(3).Properties.massflow.GetValueOrDefault
                    Hp += ims.Phases(7).Properties.enthalpy.GetValueOrDefault * ims.Phases(7).Properties.massflow.GetValueOrDefault
                    Hp /= .Phases(0).Properties.massflow
                    .Phases(0).Properties.enthalpy = Hp

                    For Each comp In .Phases(0).Compounds.Values
                        comp.MassFlow = ims.Phases(0).Compounds(comp.Name).MassFlow - ims.Phases(2).Compounds(comp.Name).MassFlow
                        comp.MolarFlow = ims.Phases(0).Compounds(comp.Name).MolarFlow - ims.Phases(2).Compounds(comp.Name).MolarFlow
                        comp.MoleFraction = comp.MolarFlow / ms1.Phases(0).Properties.molarflow
                        comp.MassFraction = comp.MassFlow / ms1.Phases(0).Properties.massflow
                    Next

                    .Phases(0).Properties.pressure = P
                End With
            End If

            If ReactorMode = EReactorMode.SingleOutlet Then
                ResidenceTimeL = Volume / Q
                ResidenceTimeV = ResidenceTimeL
            Else
                If QL + QS > 0 Then
                    ResidenceTimeL = Volume / (QL + QS)
                Else
                    ResidenceTimeL = 0
                End If

                If QV > 0 Then
                    ResidenceTimeV = Headspace / QV
                Else
                    ResidenceTimeV = 0
                End If
            End If

            DeltaT = T - T0

            'Calculate component conversions
            For i = 0 To NC - 1
                If Nin(i) > 0 And Nin(i) > Nout(i) Then
                    ComponentConversions(CompNames(i)) = (Nin(i) - Nout(i)) / Nin(i)
                Else
                    ComponentConversions(CompNames(i)) = 0
                End If

            Next

            '====================================================
            '==== Transfer information to energy stream =========
            '====================================================
            'Energy stream (KW = KJ/s)

            'Products Enthalpy (kJ/kg * kg/s = kW)
            Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W
            DeltaQ = DHr + Hp - Hr0 'Energy stream to reactor

            Dim estr As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
            With estr
                .EnergyFlow = DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With
        End Sub
        Public Sub OldCalculate(Optional ByVal args As Object = Nothing)

            'initial mole flows
            N00 = New Dictionary(Of String, Double)
            'initial mole concentrations
            C0 = New Dictionary(Of String, Double)
            'current mole concentrations
            C = New Dictionary(Of String, Double)
            'compound change rates
            Ri = New Dictionary(Of String, Double)
            'reaction heats
            DHRi = New Dictionary(Of String, Double)
            Kf = New ArrayList
            Kr = New ArrayList
            Rxi = New Dictionary(Of String, Double)

            m_conversions = New Dictionary(Of String, Double)
            m_componentconversions = New Dictionary(Of String, Double)

            Dim conv As New SystemsOfUnits.Converter
            Dim rxn As Reaction

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedeenerg17"))
            End If

            Dim N0 As New Dictionary(Of String, Double)
            Dim N As New Dictionary(Of String, Double)
            N00.Clear()

            Dim i, ec As Integer

            'scBC = stoichiometric coeff of the base comp
            'DHr = reaction heat (total)
            'Hr = reactant enthalpy
            'Hr0 = initial reactant enthalpy
            'Hp = products enthalpy
            Dim scBC, DHr, Hr, Hr0, Hp, T, T0, P, P0, W, Qf, Q, m0 As Double

            'base compound
            Dim BC As String = ""
            Dim tmp As IFlashCalculationResult

            Dim Tout0, Tout As Double

            Dim errt As Double = 1.0#

            ec = 0

            'temperature convergence loop
            While errt > 0.01

                'clone inlet stream
                ims = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Clone

                Ri.Clear()
                Rxi.Clear()
                DHRi.Clear()
                m_conversions.Clear()
                m_componentconversions.Clear()

                C0 = New Dictionary(Of String, Double)
                C = New Dictionary(Of String, Double)()

                DHr = 0.0#
                Hr = 0.0#
                Hp = 0.0#

                N0.Clear()
                N.Clear()

                Me.Reactions.Clear()
                Me.ReactionsSequence.Clear()
                Me.Conversions.Clear()
                Me.ComponentConversions.Clear()
                Me.DeltaQ = 0
                Me.DeltaT = 0

                'check active reactions (kinetic and heterogeneous only) in the reaction set
                'check if there are multiple reactions on different phases (unsupported)

                Dim rxp As PhaseName = PhaseName.Mixture

                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                    rxn = FlowSheet.Reactions(rxnsb.ReactionID)
                    If rxn.ReactionType = ReactionType.Kinetic And rxnsb.IsActive Then
                        Me.Reactions.Add(rxnsb.ReactionID)
                        If rxp = PhaseName.Mixture Then rxp = rxn.ReactionPhase
                        If rxp <> rxn.ReactionPhase Then
                            Throw New Exception(FlowSheet.GetTranslatedString("MultipleReactionPhasesNotSupported"))
                        End If
                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic And rxnsb.IsActive Then
                        Me.Reactions.Add(rxnsb.ReactionID)
                        If rxp = PhaseName.Mixture Then rxp = rxn.ReactionPhase
                        If rxp <> rxn.ReactionPhase Then
                            Throw New Exception(FlowSheet.GetTranslatedString("MultipleReactionPhasesNotSupported"))
                        End If
                    End If
                Next

                W = ims.Phases(0).Properties.massflow.GetValueOrDefault
                Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W

                PropertyPackage.CurrentMaterialStream = ims
                ims.SetPropertyPackage(PropertyPackage)
                ims.SetFlowsheet(Me.FlowSheet)
                ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

                If Tout <> 0.0# Then
                    ims.Phases(0).Properties.temperature = Tout
                    ims.Calculate(True, True)
                End If

                'order reactions
                i = 0
                Dim maxrank As Integer = 0
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                    If rxnsb.Rank > maxrank And Me.Reactions.Contains(rxnsb.ReactionID) Then maxrank = rxnsb.Rank
                Next

                'ordering of parallel reactions
                i = 0
                Dim arr As New List(Of String)
                Do
                    arr = New List(Of String)
                    For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                        If rxnsb.Rank = i And Me.Reactions.Contains(rxnsb.ReactionID) Then arr.Add(rxnsb.ReactionID)
                    Next
                    If arr.Count > 0 Then Me.ReactionsSequence.Add(arr)
                    i = i + 1
                Loop Until i = maxrank + 1

                PropertyPackage.CurrentMaterialStream = ims

                T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault
                P0 = ims.Phases(0).Properties.pressure.GetValueOrDefault

                'conversion factors for different basis other than molar concentrations
                Dim convfactors As New Dictionary(Of String, Double)

                RxiT.Clear()
                DHRi.Clear()

                ims.Phases(0).Properties.pressure = P0 - DeltaP.GetValueOrDefault

                If ReactorOperationMode = OperationMode.OutletTemperature Then
                    ims.Phases(0).Properties.temperature = OutletTemperature
                End If

                C = New Dictionary(Of String, Double)
                C0 = New Dictionary(Of String, Double)

                Kf = New ArrayList(Me.Reactions.Count)
                Kr = New ArrayList(Me.Reactions.Count)

                T = ims.Phases(0).Properties.temperature.GetValueOrDefault
                P = ims.Phases(0).Properties.pressure.GetValueOrDefault

                'Reactants Enthalpy (kJ/kg * kg/s = kW)
                Hr = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W

                'loop through reactions
                For Each ar In Me.ReactionsSequence

                    DHr = 0

                    Q = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault()

                    i = 0
                    Do

                        'process reaction i
                        rxn = FlowSheet.Reactions(ar(i))

                        If Me.Reactions.Count > 0 Then
                            Select Case FlowSheet.Reactions(Me.Reactions(0)).ReactionPhase
                                Case PhaseName.Vapor
                                    Qf = ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault()
                                Case PhaseName.Liquid
                                    Qf = ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault()
                                Case PhaseName.Mixture
                                    Qf = ims.Phases(3).Properties.volumetric_flow.GetValueOrDefault() +
                                        ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault()
                            End Select
                        End If

                        ResidenceTimeL = Volume / Q

                        'initial mole flows & concentrations

                        For Each sb As ReactionStoichBase In rxn.Components.Values

                            Select Case rxn.ReactionPhase
                                Case PhaseName.Liquid
                                    m0 = ims.Phases(3).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                Case PhaseName.Vapor
                                    m0 = ims.Phases(2).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                Case PhaseName.Mixture
                                    m0 = ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                            End Select

                            If m0 = 0.0# Then m0 = 0.0000000001

                            If Not N00.ContainsKey(sb.CompName) Then N00.Add(sb.CompName, m0)

                            N0(sb.CompName) = m0
                            N(sb.CompName) = N0(sb.CompName)
                            C0(sb.CompName) = N0(sb.CompName) / Qf

                        Next

                        Kf.Add(0.0#)
                        Kr.Add(0.0#)

                        i += 1

                    Loop Until i = ar.Count

                    Ri.Clear()
                    Rxi.Clear()

                    Me.activeAL = Me.ReactionsSequence.IndexOf(ar)

                    'solve ODEs

                    Dim vc(N.Count - 1), vc0(N.Count - 1), vcf(N.Count - 1) As Double
                    i = 0
                    For Each d As Double In N0.Values
                        vc(i) = d
                        vc0(i) = vc(i)
                        i = i + 1
                    Next

                    Dim odesolver = New DotNumerics.ODE.OdeImplicitRungeKutta5()
                    'initialize ODE solver with a pointer to the ODE function
                    odesolver.InitializeODEs(AddressOf ODEFunc, N.Count)
                    'solve ODE
                    'first arg: vc = initial mole flows (mol/s)
                    'second arg: initial value of V
                    'third arg: V step
                    'fourth arg: final value of V
                    'fifth arg: function to be called on solver finish, actually gets the final mole flows (y) and stores them in vc
                    odesolver.Solve(vc, 0.0#, 0.05 * Volume, Volume, Sub(x As Double, y As Double()) vc = y)

                    If Double.IsNaN(vc.Sum) Then Throw New Exception(FlowSheet.GetTranslatedString("PFRMassBalanceError"))

                    'update equilibrium concentrations (mol/m3)
                    C.Clear()
                    i = 0
                    For Each sb As KeyValuePair(Of String, Double) In C0
                        C(sb.Key) = vc(i) * ResidenceTimeL / (Volume)
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

                    i = 0
                    For Each sb As String In Me.ComponentConversions.Keys
                        N(sb) = vc(i)
                        i += 1
                    Next

                    DHRi.Clear()

                    i = 0
                    DHr = 0.0#
                    Do

                        'process reaction i
                        rxn = FlowSheet.Reactions(ar(i))

                        'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                        'kW (kJ/s) = kJ/kmol * 1 mol/s * 0.001 kmol/mol * mol/m3/s / mol/m3/s
                        DHr += rxn.ReactionHeat * (N00(rxn.BaseReactant) - N(rxn.BaseReactant)) / 1000 * Rxi(rxn.ID) / Ri(rxn.BaseReactant)

                        i += 1

                    Loop Until i = ar.Count

                    'update mole flows/fractions
                    Dim Nsum As Double = 0

                    'compute new mole flows
                    'Nsum = ims.Phases(0).Properties.molarflow.GetValueOrDefault
                    For Each s2 As Compound In ims.Phases(0).Compounds.Values
                        If N.ContainsKey(s2.Name) Then
                            Nsum += N(s2.Name)
                        Else
                            Nsum += s2.MolarFlow.GetValueOrDefault
                        End If
                    Next
                    For Each s2 As Compound In ims.Phases(0).Compounds.Values
                        If N.ContainsKey(s2.Name) Then
                            s2.MoleFraction = N(s2.Name) / Nsum
                            s2.MolarFlow = N(s2.Name)
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

                    'do a flash calc (calculate final temperature/enthalpy)

                    Me.PropertyPackage.CurrentMaterialStream = ims

                    Select Case Me.ReactorOperationMode

                        Case OperationMode.Adiabatic

                            Me.DeltaQ = 0.0#

                            'Products Enthalpy (kJ/kg * kg/s = kW)
                            Hp = Hr0 - DHr

                            tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P, Hp / W, Tout)
                            Tout0 = Tout
                            Tout = tmp.CalculatedTemperature

                            errt = Abs(Tout - Tout0)

                            ims.Phases(0).Properties.temperature = Tout
                            ims.Phases(0).Properties.enthalpy = Hp / W
                            T = Tout

                            ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                        Case OperationMode.Isothermic

                            errt = 1.0E-20

                            ims.SpecType = StreamSpec.Temperature_and_Pressure

                        Case OperationMode.OutletTemperature

                            errt = 1.0E-20

                            DeltaT = OutletTemperature - T0

                            ims.Phases(0).Properties.temperature = T0 + DeltaT

                            T = ims.Phases(0).Properties.temperature.GetValueOrDefault

                            ims.SpecType = StreamSpec.Temperature_and_Pressure

                    End Select

                    ims.Calculate(True, True)

                Next

                ec += 1

            End While

            ' comp. conversions
            For Each sb As Compound In ims.Phases(0).Compounds.Values
                If Me.ComponentConversions.ContainsKey(sb.Name) Then
                    Me.ComponentConversions(sb.Name) += (N00(sb.Name) - N(sb.Name)) / N00(sb.Name)
                End If
            Next

            RxiT.Clear()
            DHRi.Clear()

            For Each ar In Me.ReactionsSequence

                i = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))

                    'calculate reaction rate in terms of base compound
                    ' kmol/s = mol * 0.001 kmol/mol * mol/m3/s / mol/m3/s
                    RxiT.Add(rxn.ID, (N(rxn.BaseReactant) - N00(rxn.BaseReactant)) / rxn.Components(rxn.BaseReactant).StoichCoeff / 1000 * Rxi(rxn.ID) / Ri(rxn.BaseReactant))

                    'calculate heat for reaction i
                    'kW (kJ/s) = kJ/kmol * kmol/s
                    DHRi.Add(rxn.ID, rxn.ReactionHeat * RxiT(rxn.ID))

                    i += 1

                Loop Until i = ar.Count

            Next

            If Me.ReactorOperationMode = OperationMode.Isothermic Then

                'Products Enthalpy (kJ/kg * kg/s = kW)
                Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                DeltaQ = DHRi.Values.Sum + Hp - Hr

                DeltaT = 0.0#

            ElseIf Me.ReactorOperationMode = OperationMode.OutletTemperature Then

                'Products Enthalpy (kJ/kg * kg/s = kW)
                Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                DeltaQ = DHRi.Values.Sum + Hp - Hr

                DeltaT = OutletTemperature - T0

            ElseIf ReactorOperationMode = OperationMode.Adiabatic Then

                DeltaT = Tout - T0

            End If

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint
            Dim mtotal, wtotal As Double

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .SpecType = ims.SpecType
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
            Dim estr As Streams.EnergyStream = FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Name)
            With estr
                .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                .GraphicObject.Calculated = True
            End With

        End Sub

        Public Overrides Sub DeCalculate()

            Dim j As Integer = 0

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
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

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx
                    Case 0
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                    Case 1
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.ResidenceTimeL)
                    Case 2
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.volume, Me.Volume)
                    Case 3
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                    Case 4
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
                End Select

                Return value
            End If
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
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

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                Case 1
                    Me.ResidenceTimeL = SystemsOfUnits.Converter.ConvertToSI(su.time, propval)
                Case 2
                    Me.Volume = SystemsOfUnits.Converter.ConvertToSI(su.volume, propval)
                Case 3
                    Me.DeltaT = SystemsOfUnits.Converter.ConvertToSI(su.deltaT, propval)
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
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
            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ReactorCSTR With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReactorCSTR With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.re_cstr_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CSTR_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CSTR_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            str.AppendLine("Reactor: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Calculation Parameters")
            str.AppendLine()
            str.AppendLine("    Calculation Mode: " & ReactorOperationMode.ToString)
            str.AppendLine("    Reactor Volume: " & SystemsOfUnits.Converter.ConvertFromSI(su.volume, Me.Volume).ToString(numberformat, ci) & " " & su.volume)
            str.AppendLine("    Pressure Drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                Case OperationMode.Isothermic
                    str.AppendLine("    Heat Added/Removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
                Case OperationMode.OutletTemperature
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Heat Added/Removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
            End Select
            str.AppendLine("    Residence Time: " & SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.ResidenceTimeL).ToString(numberformat, ci) & " " & su.time)
            str.AppendLine()
            str.AppendLine("Reaction Extents")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.RxiT
                str.AppendLine("    " & Me.GetFlowsheet.Reactions(dbl.Key).Name & ": " & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, dbl.Value).ToString(numberformat, ci) & " " & su.molarflow)
            Next
            str.AppendLine()
            str.AppendLine("Reaction Rates")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.RxiT
                str.AppendLine("    " & Me.GetFlowsheet.Reactions(dbl.Key).Name & ": " & SystemsOfUnits.Converter.ConvertFromSI(su.reac_rate, (dbl.Value / Me.Volume)).ToString(numberformat, ci) & " " & su.reac_rate)
            Next
            str.AppendLine()
            str.AppendLine("Reaction Heats")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.DHRi
                str.AppendLine("    " & Me.GetFlowsheet.Reactions(dbl.Key).Name & ": " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, dbl.Value).ToString(numberformat, ci) & " " & su.heatflow)
            Next
            str.AppendLine()
            str.AppendLine("Compound Conversions")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                If dbl.Value > 0 Then
                    str.AppendLine("    " & dbl.Key & ": " & (dbl.Value * 100).ToString(numberformat, ci) & "%")
                End If
            Next
            Return str.ToString

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            If p.Equals("Calculation Mode") Then
                Return "Select the calculation mode of this reactor."
            ElseIf p.Equals("Pressure Drop") Then
                Return "Enter the desired pressure drop for this reactor."
            ElseIf p.Equals("Outlet Temperature") Then
                Return "If you chose 'Outlet Temperature' as the calculation mode, enter the desired value. If you chose a different calculation mode, this parameter will be calculated."
            ElseIf p.Equals("Reactor Volume") Then
                Return "Define the active volume of this reactor."
            ElseIf p.Equals("Catalyst Amount") Then
                Return "Enter the amount of catalyst in the reactor (for HetCat reactions only)."
            Else
                Return p
            End If
        End Function

    End Class

End Namespace


