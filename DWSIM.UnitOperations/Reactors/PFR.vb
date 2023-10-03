'    PFR Calculation Routines 
'    Copyright 2008-2022 Daniel Wagner O. de Medeiros
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


Imports DWSIM.Thermodynamics.BaseClasses
Imports Ciloci.Flee
Imports System.Math
Imports System.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.Thermodynamics
Imports DWSIM.ExtensionMethods
Imports OxyPlot
Imports OxyPlot.Axes
Imports DWSIM.UnitOperations.Streams
Imports System.Xml.Serialization

Namespace Reactors

    <System.Serializable()> Public Class ProfileItem

        Public Property Compound As String = ""
        Public Property MassFlow As Double = 0.0
        Public Property MolarFlow As Double = 0.0
        Public Property MassFraction As Double = 0.0
        Public Property MolarFraction As Double = 0.0
        Public Property MolarConcentration As Double = 0.0
        Public Property MassConcentration As Double = 0.0

    End Class

    <System.Serializable()> Public Class Reactor_PFR

        Inherits Reactor

        Public Enum SizingType
            Length = 0
            Diameter = 1
        End Enum

        Public Property ReactorSizingType As SizingType = SizingType.Length

        Private _IObj As InspectorItem

        Dim C0 As Dictionary(Of String, Double)
        Dim C As Dictionary(Of String, Double)

        Dim Ri As Dictionary(Of String, Double)

        Dim Kf, Kr As ArrayList

        Dim N00 As Dictionary(Of String, Double)

        Public Rxi As New Dictionary(Of String, Double)
        Public RxiT As New Dictionary(Of String, Double)
        Public DHRi As New Dictionary(Of String, Double)
        Public DHRT As New List(Of Double)

        Public points As ArrayList

        Public Property Profile As List(Of Tuple(Of Double, Double, Double, List(Of ProfileItem)))

        Dim activeAL As Integer = 0

        <System.NonSerialized()> Dim ims As MaterialStream

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = True


        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ReactorPFR

        Private VolumeFraction As Double = 1.0

        Public Property NumberOfTubes As Integer = 1

        Public Property Length As Double = 1.0

        Public Property Diameter As Double = 0.1

        Public Property Volume As Double = 1.0

        Public Property dV As Double = 0.01

        Public Property CatalystLoading As Double = 0.0#

        Public Property CatalystVoidFraction As Double = 0.0#

        Public Property CatalystParticleDiameter As Double = 0.0#

        Public Property ResidenceTime As Double = 0.0#

        Public Property UseUserDefinedPressureDrop As Boolean = False

        Public Property UserDefinedPressureDrop As Double = 0.0


        Public Sub New()

            MyBase.New()

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

            N00 = New Dictionary(Of String, Double)
            C0 = New Dictionary(Of String, Double)
            C = New Dictionary(Of String, Double)
            Ri = New Dictionary(Of String, Double)
            DHRi = New Dictionary(Of String, Double)
            Kf = New ArrayList
            Kr = New ArrayList
            Rxi = New Dictionary(Of String, Double)

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Reactor_PFR()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_PFR)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Function LoadData(data As List(Of XElement)) As Boolean

            AccumulationStreams = New List(Of MaterialStream)
            Dim ael = (From xel As XElement In data Select xel Where xel.Name = "AccumulationStreams").FirstOrDefault
            If Not ael Is Nothing Then
                For Each xel In ael.Elements
                    Dim as1 As New MaterialStream()
                    as1.LoadData(xel.Elements.ToList)
                    AccumulationStreams.Add(as1)
                Next
            End If
            Return MyBase.LoadData(data)

        End Function

        Public Overrides Function SaveData() As List(Of XElement)

            Dim elements As List(Of XElement) = MyBase.SaveData()

            If AccumulationStreams IsNot Nothing Then
                Dim astr As New XElement("AccumulationStreams")
                elements.Add(astr)
                For Each mstream In AccumulationStreams
                    elements.Add(New XElement("AccumulationStream", mstream.SaveData()))
                Next
            End If

            Return elements

        End Function

        Public Function ODEFunc(ByVal x As Double, ByVal y As Double()) As Double()

            _IObj?.SetCurrent

            Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj2, "", "ODEFunc", "ODE solver for reactor concentrations", "", True)

            IObj2?.SetCurrent

            IObj2?.Paragraphs.Add("<h2>Input Vars</h2>")

            IObj2?.Paragraphs.Add(String.Format("Volume Step: {0}", x))
            IObj2?.Paragraphs.Add(String.Format("Compound Mole Flows: {0} mol/s", y.ToMathArrayString))

            IObj2?.Paragraphs.Add("<h2>Intermediate Calcs</h2>")

            Dim conv As New SystemsOfUnits.Converter

            Dim i As Integer
            Dim j As Integer
            Dim scBC As Double
            Dim BC As String

            Dim Qf As Double

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

            j = 0
            For Each s As String In N00.Keys
                If y(j) < 0.0 Then
                    C(s) = 0.0
                Else
                    C(s) = y(j) / Qf
                End If
                j = j + 1
            Next

            IObj2?.Paragraphs.Add(String.Format("Compound Concentrations: {0} mol/m3", C.Values.ToArray.ToMathArrayString))

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As Dictionary(Of String, Double)

            'loop through reactions
            Dim rxn As Reaction
            Dim ar = Me.ReactionsSequence(activeAL)

            i = 0
            Do
                'process reaction i
                rxn = FlowSheet.Reactions(ar(i))
                For Each sb As ReactionStoichBase In rxn.Components.Values
                    Ri(sb.CompName) = 0.0
                Next
                i += 1
            Loop Until i = ar.Count

            i = 0
            Do

                'process reaction i
                rxn = FlowSheet.Reactions(ar(i))
                BC = rxn.BaseReactant
                scBC = rxn.Components(BC).StoichCoeff

                IObj2?.Paragraphs.Add(String.Format("Reaction ID: {0}", rxn.Name))

                Dim T As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault
                Dim P As Double = ims.Phases(0).Properties.pressure.GetValueOrDefault

                IObj2?.Paragraphs.Add(String.Format("T: {0} K", T))

                Dim rx As Double

                convfactors = Me.GetConvFactors(rxn, ims)

                Dim cvar As Double

                If rxn.ReactionKinetics = ReactionKinetics.Expression Then

                    If rxn.ReactionType = ReactionType.Kinetic Then

                        'calculate reaction constants

                        Dim kxf, kxr As Double

                        If rxn.ReactionKinFwdType = ReactionKineticType.Arrhenius Then

                            kxf = rxn.A_Forward * Exp(-SystemsOfUnits.Converter.Convert(rxn.E_Forward_Unit, "J/mol", rxn.E_Forward) / (8.314 * T))

                        Else

                            rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                            rxn.ExpContext.Imports.AddType(GetType(System.Math))

                            rxn.ExpContext.Variables.Clear()
                            rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)
                            rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                            rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.ReactionKinFwdExpression)

                            kxf = rxn.Expr.Evaluate

                        End If

                        If rxn.ReactionKinRevType = ReactionKineticType.Arrhenius Then

                            kxr = rxn.A_Reverse * Exp(-SystemsOfUnits.Converter.Convert(rxn.E_Reverse_Unit, "J/mol", rxn.E_Reverse) / (8.314 * T))

                        Else

                            rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                            rxn.ExpContext.Imports.AddType(GetType(System.Math))

                            rxn.ExpContext.Variables.Clear()
                            rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)
                            rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                            rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.ReactionKinRevExpression)

                            kxr = rxn.Expr.Evaluate

                        End If

                        If T < rxn.Tmin Or T > rxn.Tmax Then
                            kxf = 0.0#
                            kxr = 0.0#
                        End If

                        Dim rxf As Double = 1.0#
                        Dim rxr As Double = 1.0#

                        'kinetic expression

                        For Each sb As ReactionStoichBase In rxn.Components.Values
                            cvar = C(sb.CompName) * convfactors(sb.CompName)
                            rxf *= cvar ^ sb.DirectOrder
                            rxr *= cvar ^ sb.ReverseOrder
                        Next

                        rx = kxf * rxf - kxr * rxr

                        IObj2?.Paragraphs.Add(String.Format("Reaction Rate: {0} {1}", rx, rxn.VelUnit))

                        Rxi(rxn.ID) = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, rx)

                        Kf(i) = kxf
                        Kr(i) = kxr

                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then

                        If T < rxn.Tmin Or T > rxn.Tmax Then

                            rx = 0.0

                        Else

                            Dim numval, denmval As Double

                            rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                            rxn.ExpContext.Imports.AddType(GetType(System.Math))
                            rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture

                            rxn.ExpContext.Variables.Clear()
                            rxn.ExpContext.Variables.Add("T", T)

                            Dim ir As Integer = 1
                            Dim ip As Integer = 1
                            Dim ine As Integer = 1

                            For Each sb As ReactionStoichBase In rxn.Components.Values
                                cvar = C(sb.CompName) * convfactors(sb.CompName)
                                If sb.StoichCoeff < 0 Then
                                    IObj2?.Paragraphs.Add(String.Format("R{0} ({1}): {2} {3}", ir.ToString, sb.CompName, cvar, rxn.ConcUnit))
                                    rxn.ExpContext.Variables.Add("R" & ir.ToString, cvar)
                                    ir += 1
                                ElseIf sb.StoichCoeff > 0 Then
                                    IObj2?.Paragraphs.Add(String.Format("P{0} ({1}): {2} {3}", ip.ToString, sb.CompName, cvar, rxn.ConcUnit))
                                    rxn.ExpContext.Variables.Add("P" & ip.ToString, cvar)
                                    ip += 1
                                Else
                                    IObj2?.Paragraphs.Add(String.Format("N{0} ({1}): {2} {3}", ine.ToString, sb.CompName, cvar, rxn.ConcUnit))
                                    rxn.ExpContext.Variables.Add("N" & ine.ToString, cvar)
                                    ine += 1
                                End If
                            Next

                            rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationNumerator)

                            numval = rxn.Expr.Evaluate

                            rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.RateEquationDenominator)

                            denmval = rxn.Expr.Evaluate

                            IObj2?.Paragraphs.Add(String.Format("Numerator Expression: {0}", rxn.RateEquationNumerator))
                            IObj2?.Paragraphs.Add(String.Format("Numerator Value: {0}", numval))
                            IObj2?.Paragraphs.Add(String.Format("Denominator Expression: {0}", rxn.RateEquationDenominator))
                            IObj2?.Paragraphs.Add(String.Format("Denominator Value: {0}", denmval))

                            rx = numval / denmval

                        End If

                        IObj2?.Paragraphs.Add(String.Format("Reaction Rate: {0} {1}", rx, rxn.VelUnit))

                        Rxi(rxn.ID) = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, rx)

                    End If

                Else

                    ' python script
                    Dim ir As Integer = 1
                    Dim ip As Integer = 1
                    Dim ine As Integer = 1

                    Dim vars As New Dictionary(Of String, Double)
                    Dim amounts As New Dictionary(Of String, Double)

                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If sb.StoichCoeff < 0 Then
                            vars.Add("R" & ir.ToString, C(sb.CompName) * convfactors(sb.CompName))
                            ir += 1
                        ElseIf sb.StoichCoeff > 0 Then
                            vars.Add("P" & ip.ToString, C(sb.CompName) * convfactors(sb.CompName))
                            ip += 1
                        ElseIf sb.StoichCoeff = 0 Then
                            vars.Add("N" & ine.ToString, C(sb.CompName) * convfactors(sb.CompName))
                            ine += 1
                        End If
                        amounts.Add(sb.CompName, C(sb.CompName) * convfactors(sb.CompName))
                    Next

                    Dim r = ProcessAdvancedKineticReactionRate(rxn.ScriptTitle, Me, rxn, T, P, vars, amounts)

                    'calculate reaction rate & convert to internal SI units
                    rx = SystemsOfUnits.Converter.ConvertToSI(rxn.VelUnit, r)

                    Rxi(rxn.ID) = rx

                End If

                For Each sb As ReactionStoichBase In rxn.Components.Values

                    If rxn.ReactionType = ReactionType.Kinetic Then
                        Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / rxn.Components(BC).StoichCoeff
                    ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic Then
                        Ri(sb.CompName) += Rxi(rxn.ID) * sb.StoichCoeff / rxn.Components(BC).StoichCoeff * Me.CatalystLoading
                    End If

                Next

                i += 1

            Loop Until i = ar.Count

            Dim dy(Ri.Count - 1) As Double

            j = 0
            For Each kv As KeyValuePair(Of String, Double) In Ri
                dy(j) = -kv.Value
                j += 1
            Next

            IObj2?.Paragraphs.Add("<h2>Results</h2>")

            IObj2?.Paragraphs.Add(String.Format("Compound Mole Flow Variation: {0} mol/[m3.s]", dy.ToMathArrayString))

            IObj2?.Close()

            If Double.IsNaN(dy.Sum) Then Throw New Exception("PFR ODE solver failed to find a solution.")

            FlowSheet.CheckStatus()

            Return dy

        End Function

        Public Overrides Sub CreateDynamicProperties()

            AddDynamicProperty("Max Sections", "Maximum number of sections to divide the PFR length in during dynamic calculations.", 20, UnitOfMeasure.none, 1.GetType())
            AddDynamicProperty("Reset Contents", "Empties the PFR's content on the next run.", False, UnitOfMeasure.none, True.GetType())

        End Sub

        Public Property AccumulationStreams As New List(Of MaterialStream)

        Public Overrides Sub RunDynamicModel()

            Select Case Me.ReactorOperationMode

                Case OperationMode.OutletTemperature, OperationMode.Isothermic

                    Throw New Exception("This calculation mode is not supported while in Dynamic Mode.")

            End Select

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator

            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            If integrator.RealTime Then timestep = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

            Dim ims1 As MaterialStream = GetInletMaterialStream(0)

            Dim oms1 As MaterialStream = GetOutletMaterialStream(0)

            Dim es As EnergyStream = GetInletEnergyStream(1)

            ResidenceTime = Volume / ims1.GetVolumetricFlow()

            If ResidenceTime < timestep Then

                Throw New Exception("The residence time of the fluid in the PFR is lower than the current Integrator Step. Please correct this issue and try again.")

            End If

            Dim MaxSections = GetDynamicProperty("Max Sections")

            Dim NumberOfSections As Integer = ResidenceTime / timestep

            If NumberOfSections > MaxSections Then NumberOfSections = MaxSections

            Dim Reset As Boolean = GetDynamicProperty("Reset Contents")

            Dim MustReset As Boolean = CDbl(NumberOfSections) / CDbl(AccumulationStreams.Count) - 1.0 > 0.001

            If Reset Or MustReset Then
                AccumulationStreams = New List(Of MaterialStream)
                SetDynamicProperty("Reset Contents", 0)
                FlowSheet.ShowMessage(GraphicObject.Tag + ": Resetting contents...", IFlowsheet.MessageType.Warning)
            End If

            If AccumulationStreams.Count = 0 Then

                For i As Integer = 0 To NumberOfSections - 1

                    AccumulationStreams.Add(ims1.CloneXML)

                Next

            Else

                AccumulationStreams.Insert(0, ims1.CloneXML)

                AccumulationStreams.Remove(AccumulationStreams.Last)

                For Each astr In AccumulationStreams

                    astr.SetFlowsheet(FlowSheet)
                    astr.PropertyPackage.CurrentMaterialStream = astr
                    astr.Calculate()
                    If astr.GetMassFlow <= 0.0 Then astr.SetMassFlow(0.0)

                Next

            End If

            ' Calculate Temperature

            Dim Qval, Ha, Wa As Double

            If es IsNot Nothing Then Qval = es.EnergyFlow.GetValueOrDefault / NumberOfSections

            For i As Integer = 0 To NumberOfSections - 1

                Dim astr = AccumulationStreams(i)

                astr.SetFlowsheet(FlowSheet)

                Ha = astr.GetMassEnthalpy
                Wa = astr.GetMassFlow

                If Qval <> 0.0 Then

                    If Wa > 0 Then

                        astr.SetMassEnthalpy(Ha + Qval * timestep / Wa)

                        astr.SpecType = StreamSpec.Pressure_and_Enthalpy

                        astr.PropertyPackage = PropertyPackage
                        astr.PropertyPackage.CurrentMaterialStream = astr

                        If integrator.ShouldCalculateEquilibrium Then

                            astr.Calculate(True, True)

                        End If

                    End If

                End If

            Next

            For i As Integer = 0 To NumberOfSections - 1

                Calculate(AccumulationStreams(i))

            Next

            For i As Integer = NumberOfSections - 1 To 1 Step -1

                AccumulationStreams(i).AssignFromPhase(PhaseLabel.Mixture, AccumulationStreams(i - 1), True)
                AccumulationStreams(i).SetFlowsheet(FlowSheet)
                AccumulationStreams(i).PropertyPackage.CurrentMaterialStream = AccumulationStreams(i)
                AccumulationStreams(i).Calculate()

            Next

            oms1.AssignFromPhase(PhaseLabel.Mixture, AccumulationStreams.Last, False)

            AccumulationStream = Nothing

            'update profile

            points = New ArrayList

            Dim j As Integer = 1

            'Volume = PI * Diameter ^ 2 / 4 * Length
            If ReactorSizingType = SizingType.Length Then
                Diameter = (4 * Volume / NumberOfTubes / Length / PI) ^ 0.5
            Else
                Length = 4 * Volume / NumberOfTubes / PI / Diameter ^ 2
            End If

            For Each astr In AccumulationStreams

                'add data to array

                Dim tmparr(C.Count + 2) As Double

                tmparr(0) = j / AccumulationStreams.Count * Length

                Dim i As Integer = 1
                For Each kvp In C
                    tmparr(i) = astr.Phases(0).Compounds(kvp.Key).MolarFlow.GetValueOrDefault() / astr.GetVolumetricFlow()
                    i = i + 1
                Next

                tmparr(i) = astr.GetTemperature
                tmparr(i + 1) = astr.GetPressure

                Me.points.Add(tmparr)

                j += 1

            Next

            OutletTemperature = AccumulationStreams.Last.GetTemperature()

            DeltaT = OutletTemperature - ims1.GetTemperature()

            DeltaP = AccumulationStreams.Last.GetPressure() - ims1.GetPressure()

            DeltaQ = (AccumulationStreams.Last.GetMassEnthalpy() - ims1.GetMassEnthalpy()) * ims1.GetMassFlow()

            ' comp. conversions

            For Each sb As Compound In ims1.Phases(0).Compounds.Values
                If ComponentConversions.ContainsKey(sb.Name) AndAlso N00(sb.Name) > 0 Then
                    Dim n0 = ims1.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    Dim nf = AccumulationStreams.Last.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    ComponentConversions(sb.Name) = Abs(n0 - nf) / n0
                End If
            Next

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            'this reduces the volume step once a negative moalr amount is found by the ODE solver
            If Calculate_Internal(1.0, args) Then
                'reduce the volume step by a factor of 10
                If Calculate_Internal(0.1, args) Then
                    'at this stage, if we still keep getting negative molar amounts, 
                    'we stop solving the ODE and accept the solution since the error will be very small (hopefully)
                    Calculate_Internal(0.05, args)
                End If
            End If

        End Sub

        Public Function Calculate_Internal(dVF As Double, Optional ByVal args As Object = Nothing) As Boolean

            If dV = 0.0 Then dV = 0.01

            Dim negative As Boolean = False

            Dim negativeflag As Boolean = False

            Dim dynamics As Boolean = False

            If args IsNot Nothing Then
                dynamics = True
                AccumulationStream = args
            End If

            Dim deltaV, deltaV0 As Double

            If dynamics Then
                deltaV = 1.0 / AccumulationStreams.Count
            Else
                deltaV = dV * dVF
            End If
            deltaV0 = deltaV

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("To run a simulation of a reactor, the user needs to define the chemical reactions which will take place in the reactor.</p>
                                This Is done through the&nbsp;<span style='font-weight bold;'>Reactions Manager, </span>accessible through <span style='font-weight: bold;'>Simulation Settings &gt; Basis &gt; Open Chemical Reactions Manager</span> or <span style='font-weight: bold;'>Tools &gt; Reactions Manager</span> menus (see separate documentation).<br><br>Reactions can be of&nbsp;<span style='font-weight: bold;'>Equilibrium</span>,<span style='font-weight: bold;'>&nbsp;Conversion</span>,<span style='font-weight: bold;'>&nbsp;Kinetic</span> or&nbsp;<span style='font-weight: bold;'>Heterogeneous Catalytic</span> types. One or more reactions can be&nbsp;combined to define
                                            a&nbsp;<span style='font-weight bold;'>Reaction Set</span>. The reactors then 'see' the reactions through the reaction sets.
                                <br><br><span style ='font-weight bold; font-style: italic;'>Equilibrium</span>
                                Reactions are defined by an equilibrium constant (K). The source Of
                                Information for the equilibrium constant can be a direct gibbs energy
                                calculation, an expression defined by the user Or a constant value.
                                Equilibrium Reactions can be used in Equilibrium And Gibbs reactors.<br><br><span style='font-weight bold; font-style: italic;'>Conversion</span>
                                            Reactions are defined by the amount of a base compound which Is
                                consumed in the reaction. This amount can be a fixed value Or a
                                Function of() the system temperature. Conversion reactions are supported
                                by the Conversion reactor.<br><br><span style='font-style: italic;'>Kinetic</span> reactions are reactions defined by a kinetic expression. These reactions are supported by the PFR and CSTR reactors. <br><br><span style='font-style: italic;'>Heterogeneous Catalytic</span> reactions&nbsp;in DWSIM must obey the <span style='font-style: italic;'>Langmuir&#8211;Hinshelwood</span> 
                                            mechanism, where compounds react over a solid catalyst surface. In this 
                                model, Reaction rates are a function of catalyst amount (i.e. mol/kg 
                                cat.s). These Reactions are supported by the PFR And CStr reactors.<p>")

            N00 = New Dictionary(Of String, Double)
            C0 = New Dictionary(Of String, Double)
            C = New Dictionary(Of String, Double)
            Ri = New Dictionary(Of String, Double)
            DHRi = New Dictionary(Of String, Double)
            DHRT = New List(Of Double)
            Kf = New ArrayList
            Kr = New ArrayList
            Rxi = New Dictionary(Of String, Double)

            Profile = New List(Of Tuple(Of Double, Double, Double, List(Of ProfileItem)))()

            Dim conv As New SystemsOfUnits.Converter
            Dim rxn As Reaction

            m_conversions = New Dictionary(Of String, Double)
            m_componentconversions = New Dictionary(Of String, Double)

            If Not dynamics Then points = New ArrayList

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.InputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedeenerg17"))
            End If

            ims = DirectCast(FlowSheet.SimulationObjects(Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name), MaterialStream).Clone

            If dynamics Then ims = AccumulationStream

            ims.SetPropertyPackage(PropertyPackage)
            PropertyPackage.CurrentMaterialStream = ims
            ims.SetFlowsheet(Me.FlowSheet)
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

            If Not dynamics Then ResidenceTime = Volume / ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

            Me.Reactions.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.ComponentConversions.Clear()
            Me.DeltaQ = 0.0#
            Me.DeltaT = 0.0#

            'update profile
            Dim clist As New List(Of ProfileItem)
            For Each comp In ims.Phases(0).Compounds.Values
                Dim pitem As New ProfileItem
                With pitem
                    .Compound = comp.Name
                    .MassFlow = comp.MassFlow.GetValueOrDefault()
                    .MolarFlow = comp.MolarFlow.GetValueOrDefault()
                    .MassFraction = comp.MassFraction.GetValueOrDefault()
                    .MolarFraction = comp.MoleFraction.GetValueOrDefault()
                    .MassConcentration = .MassFlow / ims.GetVolumetricFlow()
                    .MolarConcentration = .MolarFlow / ims.GetVolumetricFlow()
                End With
                clist.Add(pitem)
            Next

            Profile.Add(New Tuple(Of Double, Double, Double, List(Of ProfileItem))(0, ims.GetTemperature(), ims.GetPressure(), clist))

            'check active reactions (kinetic and heterogeneous only) in the reaction set
            'check if there are multiple reactions on different phases (unsupported)

            Dim rxp As PhaseName = PhaseName.Mixture

            Dim hasHetCatReaction As Boolean = False

            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                rxn = FlowSheet.Reactions(rxnsb.ReactionID)
                If Not rxn.Components.ContainsKey(rxn.BaseReactant) Then
                    Throw New Exception("No base reactant defined for reaction '" + rxn.Name + "'.")
                End If
                If rxn.ReactionType = ReactionType.Kinetic And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
                    If rxp = PhaseName.Mixture Then rxp = rxn.ReactionPhase
                    If rxp <> rxn.ReactionPhase Then
                        Throw New Exception(FlowSheet.GetTranslatedString("MultipleReactionPhasesNotSupported"))
                    End If
                ElseIf rxn.ReactionType = ReactionType.Heterogeneous_Catalytic And rxnsb.IsActive Then
                    hasHetCatReaction = True
                    Me.Reactions.Add(rxnsb.ReactionID)
                    If rxp = PhaseName.Mixture Then rxp = rxn.ReactionPhase
                    If rxp <> rxn.ReactionPhase Then
                        Throw New Exception(FlowSheet.GetTranslatedString("MultipleReactionPhasesNotSupported"))
                    End If
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
            Dim arr As New List(Of String)
            Do
                arr = New List(Of String)
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                    If rxnsb.Rank = i And Me.Reactions.Contains(rxnsb.ReactionID) Then arr.Add(rxnsb.ReactionID)
                Next
                If arr.Count > 0 Then Me.ReactionsSequence.Add(arr)
                i = i + 1
            Loop Until i = maxrank + 1

            Dim N0 As New Dictionary(Of String, Double)
            Dim N As New Dictionary(Of String, Double)
            Dim Nnr As New Dictionary(Of String, Double)
            N00.Clear()

            Dim DHr, Hr, Hr0, Hp, T, T0, P, P0, Qf, Q, W As Double
            Dim BC As String = ""
            Dim tmp As IFlashCalculationResult
            Dim maxXarr As New ArrayList

            'Reactants Enthalpy (kJ/kg * kg/s = kW) (ISOTHERMIC)
            W = ims.Phases(0).Properties.massflow.GetValueOrDefault
            Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W
            T0 = ims.Phases(0).Properties.temperature.GetValueOrDefault
            P0 = ims.Phases(0).Properties.pressure.GetValueOrDefault

            'conversion factors for different basis other than molar concentrations
            Dim convfactors As New Dictionary(Of String, Double)

            RxiT.Clear()
            DHRi.Clear()
            DHRT.Clear()

            'do the calculations on each deltaV

            Dim currvol As Double = 0.0#
            Dim prevvol As Double = 0.0#

            Dim nloops As Integer = 1.0 / deltaV

            Dim counter As Integer = 0

            Dim Tant As Double

            Do

                IObj?.SetCurrent

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Calculate", String.Format("PFR Volume Step Calculation (V = {0} m3)", currvol), "", True)

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("This is the calculation routine for convergence of the compound concentrations/amounts at volume step {0}/{1} m3.", currvol, Volume))

                _IObj = IObj2

                C = New Dictionary(Of String, Double)
                C0 = New Dictionary(Of String, Double)

                Kf = New ArrayList(Me.Reactions.Count)
                Kr = New ArrayList(Me.Reactions.Count)

                T = ims.Phases(0).Properties.temperature.GetValueOrDefault
                P = ims.Phases(0).Properties.pressure.GetValueOrDefault

                Q = ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault

                If Me.Reactions.Count > 0 Then
                    Select Case FlowSheet.Reactions(Me.Reactions(0)).ReactionPhase
                        Case PhaseName.Vapor
                            Qf = ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault()
                        Case PhaseName.Liquid
                            Qf = ims.Phases(1).Properties.volumetric_flow.GetValueOrDefault()
                        Case PhaseName.Mixture
                            Qf = ims.Phases(1).Properties.volumetric_flow.GetValueOrDefault() +
                                ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault()
                    End Select
                End If

                'Reactants Enthalpy (kJ/kg * kg/s = kW)
                Hr = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * W

                Ri.Clear()
                Rxi.Clear()

                'loop through reactions
                For Each ar In Me.ReactionsSequence

                    i = 0
                    DHr = 0

                    Do

                        'process reaction i
                        rxn = FlowSheet.Reactions(ar(i))

                        Dim m0 As Double = 0.0#
                        Dim m0nr As Double = 0.0#

                        'initial mole flows
                        For Each sb As ReactionStoichBase In rxn.Components.Values

                            Select Case rxn.ReactionPhase
                                Case PhaseName.Liquid
                                    m0 = ims.Phases(1).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                Case PhaseName.Vapor
                                    m0 = ims.Phases(2).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                                Case PhaseName.Mixture
                                    m0 = ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault
                            End Select

                            If m0 = 0.0# Then m0 = 0.0000000001

                            m0nr = ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault - m0
                            If m0nr < 0.0# Then m0nr = 0.0

                            If Not N0.ContainsKey(sb.CompName) Then
                                N0.Add(sb.CompName, m0)
                                Nnr.Add(sb.CompName, m0nr)
                                N00.Add(sb.CompName, N0(sb.CompName))
                                N.Add(sb.CompName, N0(sb.CompName))
                                C0.Add(sb.CompName, N0(sb.CompName) / Qf)
                            Else
                                N0(sb.CompName) = m0
                                Nnr(sb.CompName) = m0nr
                                N(sb.CompName) = N0(sb.CompName)
                                C0(sb.CompName) = N0(sb.CompName) / Qf
                            End If

                        Next

                        Kf.Add(0.0#)
                        Kr.Add(0.0#)

                        i += 1

                    Loop Until i = ar.Count

                    If points IsNot Nothing AndAlso points.Count = 0 AndAlso Not dynamics Then

                        'add data to array

                        Dim tmparr0(C0.Count + 2) As Double
                        tmparr0(0) = 0.0
                        i = 1
                        For Each d As Double In Me.C0.Values
                            tmparr0(i) = d
                            i = i + 1
                        Next
                        tmparr0(i) = T0
                        tmparr0(i + 1) = P0

                        Me.points.Add(tmparr0)

                    End If

                    'SOLVE ODEs

                    Me.activeAL = Me.ReactionsSequence.IndexOf(ar)

                    Dim vc(N.Count - 1), vc0(N.Count - 1) As Double
                    i = 0
                    For Each d As Double In N.Values
                        vc(i) = d
                        vc0(i) = vc(i)
                        i = i + 1
                    Next

                    'converge temperature

                    Dim odesolver = New DotNumerics.ODE.OdeImplicitRungeKutta5()

                    Dim esolv As IExternalODESolver = Nothing
                    If FlowSheet.ExternalSolvers.ContainsKey(ExternalSolverID) Then
                        esolv = FlowSheet.ExternalSolvers(ExternalSolverID)
                    End If

                    Do

                        deltaV = deltaV0

                        If Not negativeflag Then

                            ' progressively decrease volume step until non-negative concentrations are found.

                            For nncounter = 1 To 30

                                If esolv IsNot Nothing Then
                                    esolv.InitializeODEs(Function(x, y)
                                                             Dim dy = ODEFunc(x, y)
                                                             Return dy
                                                         End Function, N.Count, 0.0, vc0)
                                    IObj2?.SetCurrent
                                    If dynamics Then
                                        esolv.Solve(vc0, 0.0#, 0.01 * deltaV * Volume, deltaV * Volume, Sub(x As Double, y As Double())
                                                                                                            vc = y
                                                                                                        End Sub, 0.00000001)
                                    Else
                                        esolv.Solve(vc0, 0.0#, 0.01 * deltaV * Volume, deltaV * Volume, Sub(x As Double, y As Double())
                                                                                                            vc = y
                                                                                                        End Sub, 0.00000001)
                                    End If
                                Else
                                    odesolver.InitializeODEs(AddressOf ODEFunc, N.Count, 0.0, vc0)
                                    IObj2?.SetCurrent
                                    If dynamics Then
                                        odesolver.Solve(vc0, 0.0#, 0.01 * deltaV * Volume, deltaV * Volume, Sub(x As Double, y As Double())
                                                                                                                vc = y
                                                                                                            End Sub)
                                    Else
                                        odesolver.Solve(vc0, 0.0#, 0.01 * deltaV * Volume, deltaV * Volume, Sub(x As Double, y As Double())
                                                                                                                vc = y
                                                                                                            End Sub)
                                    End If
                                End If

                                ODEFunc(0, vc)

                                If Double.IsNaN(vc.Sum) Then Throw New Exception(FlowSheet.GetTranslatedString("PFRMassBalanceError"))

                                negative = False
                                For i = 0 To vc.Count - 1
                                    If vc(i) < 0.0 Then
                                        If Math.Abs(vc(i)) > 0.000001 Then
                                            negative = True
                                            negativeflag = True
                                        Else
                                            vc(i) = 0.0
                                        End If
                                    End If
                                Next

                                If Not negative Then Exit For

                                deltaV /= 2

                            Next

                            If negative Then
                                Throw New Exception(FlowSheet.GetTranslatedString("PFRMassBalanceError") &
                                                    String.Format(" Error details: ODE solver calculated negative molar flows at volume step {0}/{1} m3.", currvol, Volume))
                            End If

                        Else

                            ODEFunc(0, vc)

                        End If

                        C.Clear()
                        i = 0
                        For Each sb As KeyValuePair(Of String, Double) In C0
                            C(sb.Key) = vc(i) / Qf
                            i = i + 1
                        Next

                        i = 0
                        Do

                            'process reaction i
                            rxn = FlowSheet.Reactions(ar(i))

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

                        DHr = 0.0#
                        For Each sb As Compound In ims.Phases(0).Compounds.Values
                            If N.ContainsKey(sb.Name) Then
                                DHr += sb.ConstantProperties.IG_Enthalpy_of_Formation_25C * sb.ConstantProperties.Molar_Weight * (N(sb.Name) - N0(sb.Name)) / 1000
                            End If
                        Next

                        'update mole flows/fractions
                        Dim Nsum As Double = 0.0#
                        'compute new mole flows
                        For Each s2 As Compound In ims.Phases(0).Compounds.Values
                            If N.ContainsKey(s2.Name) Then
                                Nsum += N(s2.Name) + Nnr(s2.Name)
                            Else
                                Nsum += s2.MolarFlow.GetValueOrDefault
                            End If
                        Next
                        For Each s2 As Compound In ims.Phases(0).Compounds.Values
                            If N.ContainsKey(s2.Name) Then
                                s2.MoleFraction = (N(s2.Name) + Nnr(s2.Name)) / Nsum
                                s2.MolarFlow = N(s2.Name) + Nnr(s2.Name)
                            Else
                                s2.MoleFraction = ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault / Nsum
                                s2.MolarFlow = ims.Phases(0).Compounds(s2.Name).MolarFlow.GetValueOrDefault
                            End If
                        Next

                        ims.Phases(0).Properties.massflow = Nothing
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

                        PropertyPackage.CurrentMaterialStream = ims

                        Tant = T

                        Select Case Me.ReactorOperationMode

                            Case OperationMode.NonIsothermalNonAdiabatic

                                Dim esval = GetInletEnergyStream(1).EnergyFlow.GetValueOrDefault()

                                'Products Enthalpy (kJ/kg * kg/s = kW)

                                Hp = Hr - DHr + esval * deltaV

                                IObj2?.SetCurrent()

                                tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P, Hp / W, T)
                                Dim Tout As Double = tmp.CalculatedTemperature.GetValueOrDefault

                                ims.Phases(0).Properties.temperature = Tout
                                ims.Phases(0).Properties.enthalpy = Hp / W

                                T = Tout

                                ims.SpecType = StreamSpec.Pressure_and_Enthalpy


                            Case OperationMode.Adiabatic

                                Me.DeltaQ = 0.0#

                                'Products Enthalpy (kJ/kg * kg/s = kW)

                                Hp = Hr - DHr

                                IObj2?.SetCurrent()

                                tmp = Me.PropertyPackage.CalculateEquilibrium2(FlashCalculationType.PressureEnthalpy, P, Hp / W, T)
                                Dim Tout As Double = tmp.CalculatedTemperature.GetValueOrDefault

                                Me.DeltaT = Me.DeltaT.GetValueOrDefault + Tout - T

                                ims.Phases(0).Properties.temperature = Tout
                                ims.Phases(0).Properties.enthalpy = Hp / W

                                T = Tout

                                ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                            Case OperationMode.Isothermic

                                ims.SpecType = StreamSpec.Temperature_and_Pressure

                            Case OperationMode.OutletTemperature

                                DeltaT = OutletTemperature - T0

                                ims.Phases(0).Properties.temperature += DeltaT * deltaV

                                T = ims.Phases(0).Properties.temperature.GetValueOrDefault
                                Tant = T

                                ims.SpecType = StreamSpec.Temperature_and_Pressure

                        End Select

                        IObj2?.SetCurrent()
                        ims.PropertyPackage.CurrentMaterialStream = ims
                        ims.Calculate(True, True)

                    Loop Until Abs(T - Tant) < 0.5

                    DHRT.Add(DHr)

                Next

                'Volume = PI * Diameter ^ 2 / 4 * Length
                If ReactorSizingType = SizingType.Length Then
                    Diameter = (4 * Volume / NumberOfTubes / Length / PI) ^ 0.5
                Else
                    Length = 4 * Volume / NumberOfTubes / PI / Diameter ^ 2
                End If

                If Not dynamics Then

                    'update profile
                    clist = New List(Of ProfileItem)
                    For Each comp In ims.Phases(0).Compounds.Values
                        Dim pitem As New ProfileItem
                        With pitem
                            .Compound = comp.Name
                            .MassFlow = comp.MassFlow.GetValueOrDefault()
                            .MolarFlow = comp.MolarFlow.GetValueOrDefault()
                            .MassFraction = comp.MassFraction.GetValueOrDefault()
                            .MolarFraction = comp.MoleFraction.GetValueOrDefault()
                            .MassConcentration = .MassFlow / ims.GetVolumetricFlow()
                            .MolarConcentration = .MolarFlow / ims.GetVolumetricFlow()
                        End With
                        clist.Add(pitem)
                    Next

                    Profile.Add(New Tuple(Of Double, Double, Double, List(Of ProfileItem))((currvol + deltaV * Volume) / Volume * Length, T, P, clist))

                    'add data to array
                    Dim tmparr(C.Count + 2) As Double
                    tmparr(0) = (currvol + deltaV * Volume) / Volume * Length
                    i = 1
                    For Each d As Double In Me.C.Values
                        tmparr(i) = d
                        i = i + 1
                    Next
                    tmparr(i) = T
                    tmparr(i + 1) = P

                    Me.points.Add(tmparr)

                End If

                Dim Qvin, Qlin, eta_v, eta_l, rho_v, rho_l, tens, rho, eta, xv, xl As Double

                With ims
                    rho = .Phases(0).Properties.density.GetValueOrDefault
                    eta = .Phases(0).Properties.viscosity.GetValueOrDefault
                    Qlin = .Phases(3).Properties.volumetric_flow.GetValueOrDefault + .Phases(4).Properties.volumetric_flow.GetValueOrDefault
                    rho_l = .Phases(1).Properties.density.GetValueOrDefault
                    eta_l = .Phases(1).Properties.viscosity.GetValueOrDefault
                    tens = .Phases(0).Properties.surfaceTension.GetValueOrDefault
                    Qvin = .Phases(2).Properties.volumetric_flow.GetValueOrDefault
                    rho_v = .Phases(2).Properties.density.GetValueOrDefault
                    eta_v = .Phases(2).Properties.viscosity.GetValueOrDefault
                    xv = .Phases(2).Properties.massfraction.GetValueOrDefault
                    xl = .Phases(1).Properties.massfraction.GetValueOrDefault
                End With

                Qvin /= NumberOfTubes
                Qlin /= NumberOfTubes

                eta = eta_l * xl + eta_v * xv

                Dim L As Double = deltaV * Length

                If UseUserDefinedPressureDrop Then

                    P -= UserDefinedPressureDrop * deltaV

                Else

                    If Me.CatalystLoading > 0.0 And hasHetCatReaction Then

                        'has catalyst, use Ergun equation for pressure drop in reactor beds

                        Dim vel As Double = (Qlin + Qvin) / (PI * Diameter ^ 2 / 4)
                        Dim dp As Double = Me.CatalystParticleDiameter
                        Dim ev As Double = Me.CatalystVoidFraction

                        Dim pdrop As Double = 150 * eta * L / dp ^ 2 * (1 - ev) ^ 2 / ev ^ 3 * vel + 1.75 * L * rho / dp * (1 - ev) / ev ^ 3 * vel ^ 2

                        P -= pdrop

                    Else

                        'calculate pressure drop using Beggs and Brill correlation

                        Dim resv As Object()
                        Dim fpp As New FlowPackages.BeggsBrill
                        Dim tipofluxo As String, holdup, dpf, dph, dpt As Double

                        resv = fpp.CalculateDeltaP(Diameter, L, 0.0#, 0.000045, Qvin * 24 * 3600, Qlin * 24 * 3600, eta_v * 1000, eta_l * 1000, rho_v, rho_l, tens)

                        tipofluxo = resv(0)
                        holdup = resv(1)
                        dpf = resv(2)
                        dph = resv(3)
                        dpt = resv(4)

                        P -= dpf

                    End If

                End If

                If P < 0 Then Throw New Exception(FlowSheet.GetTranslatedString("PFRNegativePressureError"))

                ims.Phases(0).Properties.pressure = P

                FlowSheet.CheckStatus()

                If dynamics Then Exit Do

                prevvol = currvol
                currvol += deltaV * Volume

                counter += 1

                If currvol + deltaV * Volume > Volume Then
                    deltaV0 = (Volume - currvol) / Volume
                End If

            Loop Until currvol >= Volume

            If Not dynamics Then

                If ReactorOperationMode = OperationMode.OutletTemperature Then

                    ims.Phases(0).Properties.temperature -= DeltaT * deltaV
                    IObj?.SetCurrent()
                    ims.PropertyPackage.CurrentMaterialStream = ims
                    ims.Calculate(True, True)

                End If

                Me.DeltaP = P0 - P

                RxiT.Clear()
                DHRi.Clear()
                DHr = 0.0#

                For Each ar In Me.ReactionsSequence

                    i = 0
                    Do

                        'process reaction i

                        rxn = FlowSheet.Reactions(ar(i))

                        'reactions with the same base compound

                        Dim rset = FlowSheet.Reactions.Values.Where(Function(x) x.BaseReactant = rxn.BaseReactant).Select(Function(x2) x2.ID).ToList

                        Dim totalrxi = Rxi.Where(Function(x) rset.Contains(x.Key)).Select(Function(x2) x2.Value).ToArray.AbsSumY

                        Dim f = Abs(Rxi(rxn.ID)) / totalrxi
                        If Double.IsNaN(f) Or Double.IsInfinity(f) Then f = 1.0#

                        RxiT.Add(rxn.ID, (N(rxn.BaseReactant) - N00(rxn.BaseReactant)) / rxn.Components(rxn.BaseReactant).StoichCoeff / 1000 * f)
                        DHRi.Add(rxn.ID, rxn.ReactionHeat * RxiT(rxn.ID))

                        i += 1

                    Loop Until i = ar.Count

                Next

                'overall reaction heat

                Dim DHrT As Double = 0

                For Each sb As Compound In ims.Phases(0).Compounds.Values
                    If N0.ContainsKey(sb.Name) Then
                        DHrT += sb.ConstantProperties.IG_Enthalpy_of_Formation_25C * sb.ConstantProperties.Molar_Weight * (N(sb.Name) - N00(sb.Name)) / 1000
                    End If
                Next

                If Me.ReactorOperationMode = OperationMode.Isothermic Then

                    'Products Enthalpy (kJ/kg * kg/s = kW)
                    Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                    Me.DeltaQ = DHrT + Hp - Hr0

                    Me.DeltaT = 0.0#

                    OutletTemperature = T0

                ElseIf Me.ReactorOperationMode = OperationMode.NonIsothermalNonAdiabatic Then

                    Me.DeltaQ = GetInletEnergyStream(1).EnergyFlow.GetValueOrDefault()

                    OutletTemperature = ims.GetTemperature()

                    Me.DeltaT = OutletTemperature - T0

                ElseIf Me.ReactorOperationMode = OperationMode.OutletTemperature Then

                    'Products Enthalpy (kJ/kg * kg/s = kW)
                    Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                    'Heat (kW)
                    Me.DeltaQ = DHrT + Hp - Hr0

                    Me.DeltaT = OutletTemperature - T0

                Else

                    OutletTemperature = T

                    Me.DeltaT = OutletTemperature - T0

                End If

                ' comp. conversions
                For Each sb As Compound In ims.Phases(0).Compounds.Values
                    If Me.ComponentConversions.ContainsKey(sb.Name) AndAlso N00(sb.Name) > 0.0000000001 Then
                        Me.ComponentConversions(sb.Name) = Abs(N00(sb.Name) - N(sb.Name)) / N00(sb.Name)
                    End If
                Next

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

                If ReactorOperationMode <> OperationMode.NonIsothermalNonAdiabatic Then
                    'energy stream - update energy flow value (kW)
                    Dim estr = GetInletEnergyStream(1)
                    If estr IsNot Nothing Then
                        With estr
                            .EnergyFlow = DeltaQ.GetValueOrDefault
                            .GraphicObject.Calculated = True
                        End With
                    End If
                End If

            End If

            IObj?.Close()

            Return negative

        End Function

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

                If prop.Contains("_") Then

                    Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                    Select Case propidx
                        Case 0
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)
                        Case 1
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.ResidenceTime)
                        Case 2
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volume, Me.Volume)
                        Case 3
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.Length)
                        Case 4
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.CatalystLoading)
                        Case 5
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.CatalystParticleDiameter)
                        Case 6
                            value = CatalystVoidFraction
                        Case 7
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaT, Me.DeltaT.GetValueOrDefault)
                        Case 8
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault)
                        Case 9
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.diameter, Me.Diameter)
                        Case 10
                            value = NumberOfTubes
                    End Select

                Else

                    Select Case prop
                        Case "Calculation Mode"
                            Select Case ReactorOperationMode
                                Case OperationMode.Adiabatic
                                    Return "Adiabatic"
                                Case OperationMode.Isothermic
                                    Return "Isothermic"
                                Case OperationMode.OutletTemperature
                                    Return "Defined Temperature"
                            End Select
                        Case Else
                            If prop.Contains("Conversion") Then
                                Dim comp = prop.Split(": ")(0)
                                If ComponentConversions.ContainsKey(comp) Then
                                    value = ComponentConversions(comp) * 100
                                Else
                                    value = 0.0
                                End If
                            End If
                            If prop.Contains("Extent") Then
                                Dim rx = prop.Split(": ")(0)
                                Dim rx2 = FlowSheet.Reactions.Values.Where(Function(x) x.Name = rx).FirstOrDefault
                                If rx2 IsNot Nothing AndAlso Rxi.ContainsKey(rx2.ID) Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, RxiT(rx2.ID))
                                Else
                                    value = 0.0
                                End If
                            End If
                            If prop.Contains("Rate") Then
                                Dim rx = prop.Split(": ")(0)
                                Dim rx2 = FlowSheet.Reactions.Values.Where(Function(x) x.Name = rx).FirstOrDefault
                                If rx2 IsNot Nothing AndAlso Rxi.ContainsKey(rx2.ID) Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.reac_rate, RxiT(rx2.ID) / Volume)
                                Else
                                    value = 0.0
                                End If
                            End If
                            If prop.Contains("Heat") Then
                                Dim rx = prop.Split(": ")(0)
                                Dim rx2 = FlowSheet.Reactions.Values.Where(Function(x) x.Name = rx).FirstOrDefault
                                If rx2 IsNot Nothing AndAlso DHRi.ContainsKey(rx2.ID) Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, DHRi(rx2.ID))
                                Else
                                    value = 0.0
                                End If
                            End If
                    End Select

                End If

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
                    For i = 0 To 10
                        proplist.Add("PROP_PF_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 10
                        proplist.Add("PROP_PF_" + CStr(i))
                    Next
                Case PropertyType.ALL, PropertyType.RO
                    For i = 0 To 10
                        proplist.Add("PROP_PF_" + CStr(i))
                    Next
                    proplist.Add("Calculation Mode")
                    For Each item In ComponentConversions
                        proplist.Add(item.Key + ": Conversion")
                    Next
                    For Each dbl As KeyValuePair(Of String, Double) In RxiT
                        proplist.Add(FlowSheet.Reactions(dbl.Key).Name + ": Reaction Extent")
                    Next
                    For Each dbl As KeyValuePair(Of String, Double) In RxiT
                        proplist.Add(FlowSheet.Reactions(dbl.Key).Name + ": Reaction Rate")
                    Next
                    For Each dbl As KeyValuePair(Of String, Double) In DHRi
                        proplist.Add(FlowSheet.Reactions(dbl.Key).Name + ": Reaction Heat")
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
                    Me.ResidenceTime = SystemsOfUnits.Converter.ConvertToSI(su.time, propval)
                Case 2
                    Me.Volume = SystemsOfUnits.Converter.ConvertToSI(su.volume, propval)
                Case 3
                    Me.Length = SystemsOfUnits.Converter.ConvertToSI(su.distance, propval)
                Case 4
                    Me.CatalystLoading = SystemsOfUnits.Converter.ConvertToSI(su.density, propval)
                Case 5
                    Me.CatalystParticleDiameter = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval)
                Case 6
                    CatalystVoidFraction = propval
                Case 7
                    Me.DeltaT = SystemsOfUnits.Converter.ConvertToSI(su.deltaT, propval)
                Case 9
                    Me.Diameter = SystemsOfUnits.Converter.ConvertToSI(su.diameter, propval)
                Case 10
                    NumberOfTubes = propval
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

                If prop.Contains("_") Then

                    Try

                        Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                        Select Case propidx
                            Case 0
                                value = su.deltaP
                            Case 1
                                value = su.time
                            Case 2
                                value = su.volume
                            Case 3
                                value = su.distance
                            Case 4
                                value = su.density
                            Case 5
                                value = su.diameter
                            Case 6
                                value = ""
                            Case 7
                                value = su.deltaT
                            Case 8
                                value = su.heatflow
                            Case 9
                                value = su.diameter
                            Case 10
                                value = ""
                        End Select

                    Catch ex As Exception

                        Return ""

                    End Try

                Else

                    Select Case prop
                        Case "Calculation Mode"
                            Return ""
                        Case Else
                            If prop.Contains("Conversion") Then value = "%"
                            If prop.Contains("Rate") Then value = su.reac_rate
                            If prop.Contains("Extent") Then value = su.molarflow
                            If prop.Contains("Heat") Then value = su.heatflow
                    End Select

                End If

                Return value

            End If

        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ReactorPFR With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReactorPFR With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
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
            Return My.Resources.pfr
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("PFR_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("PFR_Name")
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
            str.AppendLine("    Reactor Length: " & SystemsOfUnits.Converter.ConvertFromSI(su.distance, Me.Length).ToString(numberformat, ci) & " " & su.distance)
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
            str.AppendLine("    Residence Time: " & SystemsOfUnits.Converter.ConvertFromSI(su.time, Me.ResidenceTime).ToString(numberformat, ci) & " " & su.time)
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

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Plug Flow Reactor '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))


            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn, New String() {"Calculation Mode", ReactorOperationMode.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results"}))

            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Temperature",
                            Me.OutletTemperature.ConvertFromSI(su.temperature).ToString(nf), su.temperature}))
                Case OperationMode.Isothermic
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Heat Added/Removed",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow}))
                Case OperationMode.OutletTemperature
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Outlet Temperature",
                            Me.OutletTemperature.ConvertFromSI(su.temperature).ToString(nf), su.temperature}))
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Heat Added/Removed",
                            Me.DeltaQ.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf), su.heatflow}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn, New String() {"Pressure Drop", SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(nf), su.deltaP}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {"Residence Time",
                    ResidenceTime.ConvertFromSI(su.time).ToString(nf),
                    su.time}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Reaction Extents"}))
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.RxiT
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {Me.GetFlowsheet.Reactions(dbl.Key).Name,
                            (dbl.Value).ConvertFromSI(su.molarflow).ToString(nf), su.molarflow}))
                Next
            End If

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Reaction Rates"}))
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.RxiT
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {Me.GetFlowsheet.Reactions(dbl.Key).Name,
                            (dbl.Value / Volume).ConvertFromSI(su.reac_rate).ToString(nf), su.reac_rate}))
                Next
            End If

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Reaction Heats"}))
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.DHRi
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {Me.GetFlowsheet.Reactions(dbl.Key).Name,
                            (dbl.Value).ConvertFromSI(su.heatflow).ToString(nf), su.heatflow}))
                Next
            End If

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Compound Conversions"}))
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                If dbl.Value >= 0 Then list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {dbl.Key,
                            (dbl.Value * 100).ToString(nf), "%"}))
            Next

            Return list

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
            ElseIf p.Equals("Reactor Length") Then
                Return "Define the active length of this reactor."
            ElseIf p.Equals("Catalyst Loading") Then
                Return "Enter the amount of catalyst per unit volume (for HetCat reactions only)."
            ElseIf p.Equals("Catalyst Diameter") Then
                Return "Enter the diameter of the catalyst sphere (for HetCat reactions only)."
            ElseIf p.Equals("Catalyst Void Fraction") Then
                Return "Enter the void fraction of the catalyst bed in the reactor (for HetCat reactions only)."
            Else
                Return p
            End If
        End Function

        Public Overrides Function GetChartModel(name As String) As Object

            If points Is Nothing Then Return Nothing

            If points.Count = 0 Then Return Nothing

            Dim su = FlowSheet.FlowsheetOptions.SelectedUnitSystem

            Dim model = New PlotModel() With {.Subtitle = name, .Title = GraphicObject.Tag}

            model.TitleFontSize = 11
            model.SubtitleFontSize = 10

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Bottom,
                .FontSize = 10,
                .Title = "Length (" + su.distance + ")"
            })

            model.LegendFontSize = 9
            model.LegendPlacement = LegendPlacement.Outside
            model.LegendOrientation = LegendOrientation.Horizontal
            model.LegendPosition = LegendPosition.BottomCenter
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView

            Dim vx As New List(Of Double)(), vy As New List(Of Double)()
            Dim vya As New List(Of List(Of Double))()
            Dim vn As New List(Of String)()

            For Each obj In points
                vx.Add(DirectCast(obj, Double())(0))
            Next

            Dim j As Integer
            For j = 1 To ComponentConversions.Count + 2
                vy = New List(Of Double)()
                For Each obj In points
                    vy.Add(DirectCast(obj, Double())(j))
                Next
                vya.Add(vy)
            Next
            For Each st In ComponentConversions.Keys
                vn.Add(st)
            Next
            Dim color As OxyColor

            Select Case name

                Case "Temperature Profile"

                    model.Axes.Add(New LinearAxis() With {
                        .MajorGridlineStyle = LineStyle.Dash,
                        .MinorGridlineStyle = LineStyle.Dot,
                        .Position = AxisPosition.Left,
                        .FontSize = 10,
                        .Title = "Temperature (" + su.temperature + ")",
                        .Key = "temp"
                    })

                    color = OxyColors.Blue
                    model.AddLineSeries(SystemsOfUnits.Converter.ConvertArrayFromSI(su.distance, vx.ToArray()), SystemsOfUnits.Converter.ConvertArrayFromSI(su.temperature, vya(ComponentConversions.Count).ToArray()), color)
                    model.Series(model.Series.Count - 1).Title = "Temperature"
                    DirectCast(model.Series(model.Series.Count - 1), OxyPlot.Series.LineSeries).YAxisKey = "temp"

                Case "Pressure Profile"

                    model.Axes.Add(New LinearAxis() With {
                        .MajorGridlineStyle = LineStyle.Dash,
                        .MinorGridlineStyle = LineStyle.Dot,
                        .Position = AxisPosition.Left,
                        .FontSize = 10,
                        .Title = "Pressure (" + su.pressure + ")",
                        .Key = "press"
                    })

                    color = OxyColors.Green
                    model.AddLineSeries(SystemsOfUnits.Converter.ConvertArrayFromSI(su.distance, vx.ToArray()), SystemsOfUnits.Converter.ConvertArrayFromSI(su.pressure, vya(ComponentConversions.Count + 1).ToArray()), color)
                    model.Series(model.Series.Count - 1).Title = "Pressure"
                    DirectCast(model.Series(model.Series.Count - 1), OxyPlot.Series.LineSeries).YAxisKey = "press"

                Case "Concentration Profile"

                    model.Axes.Add(New LinearAxis() With {
                        .MajorGridlineStyle = LineStyle.Dash,
                        .MinorGridlineStyle = LineStyle.Dot,
                        .Position = AxisPosition.Left,
                        .FontSize = 10,
                        .Title = "Concentration (" + su.molar_conc + ")",
                        .Key = "conc"
                    })

                    For j = 0 To vn.Count - 1
                        Select Case j
                            Case 0
                                color = OxyColors.Red
                            Case 1
                                color = OxyColors.Blue
                            Case 2
                                color = OxyColors.Green
                            Case 3
                                color = OxyColors.Yellow
                            Case 4
                                color = OxyColors.Orange
                            Case 5
                                color = OxyColors.Salmon
                            Case 6
                                color = OxyColors.Brown
                            Case 7
                                color = OxyColors.Cyan
                            Case 8
                                color = OxyColors.Purple
                            Case Else
                                color = OxyColor.FromRgb(New Random(j).Next(0, 255), New Random(j + 100).Next(0, 255), New Random(j - 100).Next(0, 255))
                        End Select
                        model.AddLineSeries(SystemsOfUnits.Converter.ConvertArrayFromSI(su.distance, vx.ToArray()), SystemsOfUnits.Converter.ConvertArrayFromSI(su.molar_conc, vya(j).ToArray()), color)
                        model.Series(model.Series.Count - 1).Title = vn(j)
                        DirectCast(model.Series(model.Series.Count - 1), OxyPlot.Series.LineSeries).YAxisKey = "conc"
                    Next

            End Select

            Return model

        End Function

        Public Overrides Function GetChartModelNames() As List(Of String)
            Return New List(Of String)({"Temperature Profile", "Pressure Profile", "Concentration Profile"})
        End Function

    End Class

End Namespace



