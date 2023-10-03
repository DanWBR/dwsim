'    Conversion Reactor Calculation Routines 
'    Copyright 2008-2018 Daniel Wagner O. de Medeiros
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

Imports DotNumerics.Optimization

Namespace Reactors

    <System.Serializable()> Public Class Reactor_Conversion

        Inherits Reactor

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False


        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_ReactorConvEqGibbs

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.New()
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New Reactor_Conversion()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of Reactor_Conversion)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Sub Validate()

            If Not Me.GraphicObject.InputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac16"))
            ElseIf Not Me.GraphicObject.OutputConnectors(0).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            ElseIf Not Me.GraphicObject.OutputConnectors(1).IsAttached Then
                Throw New Exception(FlowSheet.GetTranslatedString("Nohcorrentedematriac15"))
            End If

            If Conversions Is Nothing Then m_conversions = New Dictionary(Of String, Double)

        End Sub

        Private Sub InitVars()

            Me.Reactions.Clear()
            Me.ReactionsSequence.Clear()
            Me.Conversions.Clear()
            Me.DeltaQ = 0
            Me.DeltaT = 0

            'check active reactions (conversion only) in the reaction set
            For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                If FlowSheet.Reactions(rxnsb.ReactionID).ReactionType = ReactionType.Conversion And rxnsb.IsActive Then
                    Me.Reactions.Add(rxnsb.ReactionID)
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
            Dim arr As List(Of String)
            Do
                arr = New List(Of String)
                For Each rxnsb As ReactionSetBase In FlowSheet.ReactionSets(Me.ReactionSetID).Reactions.Values
                    If rxnsb.Rank = i And Me.Reactions.Contains(rxnsb.ReactionID) Then arr.Add(rxnsb.ReactionID)
                Next
                If arr.Count > 0 Then Me.ReactionsSequence.Add(arr)
                i = i + 1
            Loop Until i = maxrank + 1

        End Sub

        Public Overrides Sub DisplayDynamicsEditForm()

            If fd Is Nothing Then
                fd = New DynamicsPropertyEditor With {.SimObject = Me}
                fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                fd.Tag = "ObjectEditor"
                fd.UpdateCallBack = Sub(table)
                                        AddButtonsToDynEditor(table)
                                    End Sub
                Me.FlowSheet.DisplayForm(fd)
            Else
                If fd.IsDisposed Then
                    fd = New DynamicsPropertyEditor With {.SimObject = Me}
                    fd.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
                    fd.Tag = "ObjectEditor"
                    fd.UpdateCallBack = Sub(table)
                                            AddButtonsToDynEditor(table)
                                        End Sub
                    Me.FlowSheet.DisplayForm(fd)
                Else
                    fd.Activate()
                End If
            End If

        End Sub

        Private Sub AddButtonsToDynEditor(table As TableLayoutPanel)

            Dim button1 As New Button With {.Text = FlowSheet.GetTranslatedString("ViewAccumulationStream"),
                .Dock = DockStyle.Bottom, .AutoSize = True, .AutoSizeMode = AutoSizeMode.GrowAndShrink}
            AddHandler button1.Click, Sub(s, e)
                                          AccumulationStream.SetFlowsheet(FlowSheet)
                                          Dim fms As New MaterialStreamEditor With {
                                          .MatStream = AccumulationStream,
                                          .IsAccumulationStream = True,
                                          .Text = Me.GraphicObject.Tag + ": " + FlowSheet.GetTranslatedString("AccumulationStream")}
                                          FlowSheet.DisplayForm(fms)
                                      End Sub

            Dim button2 As New Button With {.Text = FlowSheet.GetTranslatedString("FillWithStream"),
                .Dock = DockStyle.Bottom, .AutoSize = True, .AutoSizeMode = AutoSizeMode.GrowAndShrink}
            AddHandler button2.Click, Sub(s, e)
                                          AccumulationStream.SetFlowsheet(FlowSheet)
                                          Dim fms As New EditingForm_SeparatorFiller With {.Separator = Me}
                                          fms.ShowDialog()
                                      End Sub

            table.Controls.Add(button1)
            table.Controls.Add(button2)
            table.Controls.Add(New Panel())

        End Sub
        Public Overrides Sub CreateDynamicProperties()

            AddDynamicProperty("Operating Pressure (Dynamics)", "Current Operating Pressure", 0, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Liquid Level", "Current Liquid Level", 0, UnitOfMeasure.distance, 1.0.GetType())
            AddDynamicProperty("Volume", "Reactor Volume", 1, UnitOfMeasure.volume, 1.0.GetType())
            AddDynamicProperty("Height", "Available Height for Liquid", 2, UnitOfMeasure.distance, 1.0.GetType())
            AddDynamicProperty("Minimum Pressure", "Minimum Dynamic Pressure for this Unit Operation.", 101325, UnitOfMeasure.pressure, 1.0.GetType())
            AddDynamicProperty("Initialize using Inlet Stream", "Initializes the Reactor's available space with information from the inlet stream, if the vessel content is null.", False, UnitOfMeasure.none, True.GetType())
            AddDynamicProperty("Reset Contents", "Empties the Reactor's space on the next run.", False, UnitOfMeasure.none, True.GetType())
            RemoveDynamicProperty("Reset Content")

        End Sub

        Private prevM, currentM As Double

        Public Overrides Sub RunDynamicModel()

            Dim integratorID = FlowSheet.DynamicsManager.ScheduleList(FlowSheet.DynamicsManager.CurrentSchedule).CurrentIntegrator
            Dim integrator = FlowSheet.DynamicsManager.IntegratorList(integratorID)

            Dim timestep = integrator.IntegrationStep.TotalSeconds

            If integrator.RealTime Then timestep = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

            Dim ims1 As MaterialStream = GetInletMaterialStream(0)

            Dim oms1 As MaterialStream = GetOutletMaterialStream(0)
            Dim oms2 As MaterialStream = GetOutletMaterialStream(1)

            Dim es = GetInletEnergyStream(1)

            Dim Height As Double = GetDynamicProperty("Height")
            Dim Pressure As Double
            Dim Pmin = GetDynamicProperty("Minimum Pressure")
            Dim InitializeFromInlet As Boolean = GetDynamicProperty("Initialize using Inlet Stream")

            Dim Reset As Boolean = GetDynamicProperty("Reset Contents")

            Dim Volume As Double = GetDynamicProperty("Volume")

            If Reset Then
                AccumulationStream = Nothing
                SetDynamicProperty("Reset Contents", 0)
            End If

            If AccumulationStream Is Nothing Then

                If InitializeFromInlet Then

                    AccumulationStream = ims1.CloneXML

                Else

                    AccumulationStream = ims1.Subtract(oms1, timestep)
                    If oms2 IsNot Nothing Then AccumulationStream = AccumulationStream.Subtract(oms2, timestep)

                End If

                Dim density = AccumulationStream.Phases(0).Properties.density.GetValueOrDefault

                AccumulationStream.SetMassFlow(density * Volume)
                AccumulationStream.SpecType = StreamSpec.Temperature_and_Pressure
                AccumulationStream.PropertyPackage = PropertyPackage
                AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream
                AccumulationStream.Calculate()

            Else

                AccumulationStream.SetFlowsheet(FlowSheet)
                AccumulationStream = AccumulationStream.Add(ims1, timestep)
                AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream
                AccumulationStream.Calculate()
                AccumulationStream = AccumulationStream.Subtract(oms1, timestep)
                If oms2 IsNot Nothing Then AccumulationStream = AccumulationStream.Subtract(oms2, timestep)
                If AccumulationStream.GetMassFlow <= 0.0 Then AccumulationStream.SetMassFlow(0.0)

            End If

            AccumulationStream.SetFlowsheet(FlowSheet)

            ' Calculate Temperature

            Dim Qval, Ha, Wa As Double

            Ha = AccumulationStream.GetMassEnthalpy
            Wa = AccumulationStream.GetMassFlow

            If es IsNot Nothing Then Qval = es.EnergyFlow.GetValueOrDefault

            'If Qval <> 0.0 Then

            '    If Wa > 0 Then

            '        AccumulationStream.SetMassEnthalpy(Ha + Qval * timestep / Wa)

            '        AccumulationStream.SpecType = StreamSpec.Pressure_and_Enthalpy

            '        AccumulationStream.PropertyPackage = PropertyPackage
            '        AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream

            '        If integrator.ShouldCalculateEquilibrium Then

            '            AccumulationStream.Calculate(True, True)

            '        End If

            '    End If

            'End If

            'calculate pressure

            Dim M = AccumulationStream.GetMolarFlow()

            Dim Temperature = AccumulationStream.GetTemperature()

            Pressure = AccumulationStream.GetPressure()

            'm3/mol

            prevM = currentM

            currentM = Volume / M

            PropertyPackage.CurrentMaterialStream = AccumulationStream

            Dim LiquidVolume, RelativeLevel As Double

            If AccumulationStream.GetPressure > Pmin Then

                If prevM = 0.0 Or integrator.ShouldCalculateEquilibrium Then

                    Dim result As IFlashCalculationResult

                    result = PropertyPackage.CalculateEquilibrium2(FlashCalculationType.VolumeTemperature, currentM, Temperature, Pressure)

                    Pressure = result.CalculatedPressure

                    LiquidVolume = AccumulationStream.Phases(3).Properties.volumetric_flow.GetValueOrDefault

                    RelativeLevel = LiquidVolume / Volume

                    SetDynamicProperty("Liquid Level", RelativeLevel * Height)

                Else

                    Pressure = currentM / prevM * Pressure

                End If

            Else

                Pressure = Pmin

                LiquidVolume = 0.0

                RelativeLevel = LiquidVolume / Volume

                SetDynamicProperty("Liquid Level", RelativeLevel * Height)

            End If

            AccumulationStream.SetPressure(Pressure)
            AccumulationStream.SpecType = StreamSpec.Temperature_and_Pressure

            AccumulationStream.PropertyPackage = PropertyPackage
            AccumulationStream.PropertyPackage.CurrentMaterialStream = AccumulationStream

            If integrator.ShouldCalculateEquilibrium And Pressure > 0.0 Then

                AccumulationStream.Calculate(True, True)

            End If

            SetDynamicProperty("Operating Pressure", Pressure)

            Calculate(True)

            OutletTemperature = AccumulationStream.GetTemperature()

            DeltaT = OutletTemperature - ims1.GetTemperature()

            DeltaP = AccumulationStream.GetPressure() - ims1.GetPressure()

            DeltaQ = (AccumulationStream.GetMassEnthalpy() - ims1.GetMassEnthalpy()) * ims1.GetMassFlow()

            ' comp. conversions

            For Each sb As Compound In ims1.Phases(0).Compounds.Values
                If ComponentConversions.ContainsKey(sb.Name) > 0 Then
                    Dim n0 = ims1.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    Dim nf = AccumulationStream.Phases(0).Compounds(sb.Name).MolarFlow.GetValueOrDefault()
                    ComponentConversions(sb.Name) = Math.Abs(n0 - nf) / nf
                End If
            Next

        End Sub

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim dynamics As Boolean = False

            If args IsNot Nothing Then dynamics = args

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("To run a simulation of a reactor, the user needs to define the chemical reactions which will take place in the reactor.</p>
                                This is done through the&nbsp;<span style='font-weight bold;'>Reactions Manager, </span>accessible through <span style='font-weight: bold;'>Simulation Settings &gt; Basis &gt; Open Chemical Reactions Manager</span> or <span style='font-weight: bold;'>Tools &gt; Reactions Manager</span> menus (see separate documentation).<br><br>Reactions can be of&nbsp;<span style='font-weight: bold;'>Equilibrium</span>,<span style='font-weight: bold;'>&nbsp;Conversion</span>,<span style='font-weight: bold;'>&nbsp;Kinetic</span> or&nbsp;<span style='font-weight: bold;'>Heterogeneous Catalytic</span> types. One or more reactions can be&nbsp;combined to define
                                            a&nbsp;<span style='font-weight bold;'>Reaction Set</span>. The reactors then 'see' the reactions through the reaction sets.
                                <br><br><span style ='font-weight bold; font-style: italic;'>Equilibrium</span>
                                Reactions are defined by an equilibrium constant (K). The source Of
                                Information for the equilibrium constant can be a direct gibbs energy
                                calculation, an expression defined by the user or a constant value.
                                Equilibrium Reactions can be used in Equilibrium And Gibbs reactors.<br><br><span style='font-weight bold; font-style: italic;'>Conversion</span>
                                            Reactions are defined by the amount of a base compound which Is
                                consumed in the reaction. This amount can be a fixed value Or a
                                Function of() the system temperature. Conversion reactions are supported
                                by the Conversion reactor.<br><br><span style='font-weight bold; font-style: italic;'>Kinetic</span> reactions are reactions defined by a kinetic expression. These reactions are supported by the PFR and CSTR reactors. <br><br><span style='font-weight: bold; font-style: italic;'>Heterogeneous Catalytic</span> reactions&nbsp;in DWSIM must obey the <span style='font-style: italic;'>Langmuir&#8211;Hinshelwood</span> 
                                            mechanism, where compounds react over a solid catalyst surface. In this 
                                model, Reaction rates are a function of catalyst amount (i.e. mol/kg 
                                cat.s). These Reactions are supported by the PFR And CSTR reactors.<p>")

            Validate()

            InitVars()

            Dim ims As MaterialStream

            If dynamics Then
                ims = AccumulationStream.Clone()
            Else
                ims = GetInletMaterialStream(0).Clone()
            End If

            Dim Qin = 0.0

            'energy stream
            If GetInletEnergyStream(1) IsNot Nothing Then
                Qin = GetInletEnergyStream(1).EnergyFlow.GetValueOrDefault()
            End If

            Dim pp = Me.PropertyPackage

            ims.SetFlowsheet(Me.FlowSheet)
            ims.PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

            pp.CurrentMaterialStream = ims
            ims.PropertyPackage = pp

            Dim DN As New Dictionary(Of String, Double)
            Dim N00 As New Dictionary(Of String, Double)
            Dim N0 As New Dictionary(Of String, Double)

            Dim X, scBC, nBC, DHr, Hid_r, Hid_p, Hr, Hp, Tin, Pin, Pout, W As Double
            Dim i As Integer
            Dim BC As String = ""

            Dim tmp As IFlashCalculationResult

            Tin = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Pin = ims.Phases(0).Properties.pressure.GetValueOrDefault
            W = ims.Phases(0).Properties.massflow.GetValueOrDefault
            Pout = ims.Phases(0).Properties.pressure.GetValueOrDefault - Me.DeltaP.GetValueOrDefault
            ims.Phases(0).Properties.pressure = Pout

            Dim ni0(N00.Count - 1), nif(N00.Count - 1) As Double

            Dim cnames = ims.PropertyPackage.RET_VNAMES().ToList()

            ni0 = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).MultiplyConstY(ims.Phases(0).Properties.molarflow.GetValueOrDefault)

            'Reactants Enthalpy (kJ/kg * kg/s = kW)

            Dim Hr0 = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

            Dim rxn As Reaction

            'loop through conversion reaction groups (parallel/sequential) as defined in the reaction set

            IObj?.Paragraphs.Add("<h2>Calculation Procedure</h2>")

            IObj?.Paragraphs.Add("Looping through conversion reaction groups (parallel/sequential) as defined in the reaction set...")

            For Each ar In Me.ReactionsSequence

                pp.CurrentMaterialStream = ims
                ims.PropertyPackage = pp

                DN.Clear()
                N0.Clear()

                IObj?.SetCurrent

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Calculate", String.Format("Reaction Sequence ID #{0}", "Reaction Names:" & ReactionsSequence.IndexOf(ar)), ar.Select(Of String)(Function(ari) FlowSheet.Reactions(ari).Name).ToArray.ToArrayString, True)

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add("This is the calculation routine for a group of reactions as defined in the reaction set.")

                'Reactants Enthalpy (kJ/kg * kg/s = kW)

                Hr = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                IObj2?.Paragraphs.Add(String.Format("Reactants Enthalpy: {0} kJ/kg", Hr))

                pp.CurrentMaterialStream = ims

                i = 0
                Do

                    'process reaction i

                    rxn = FlowSheet.Reactions(ar(i))
                    BC = rxn.BaseReactant

                    If Not rxn.Components.ContainsKey(BC) Then
                        Throw New Exception("No base reactant defined for reaction '" + rxn.Name + "'.")
                    End If

                    scBC = rxn.Components(BC).StoichCoeff

                    'initial mole flows
                    For Each sb As ReactionStoichBase In rxn.Components.Values

                        If Not N00.ContainsKey(sb.CompName) Then
                            N00.Add(sb.CompName, ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault())
                        End If

                        If Not N0.ContainsKey(sb.CompName) Then
                            N0.Add(sb.CompName, ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault())
                        End If

                    Next

                    i += 1

                Loop Until i = ar.Count

                IObj2?.Paragraphs.Add(String.Format("Compounds: {0}", N00.Keys.ToArray.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Initial Mole Flows: {0} mol/s", N00.Values.ToArray.ToMathArrayString))

                IObj2?.Paragraphs.Add("Solving Parallel Reactions with Simplex solver...")

                ' solve parallel reactions with Simplex NL solver

                IObj2?.Paragraphs.Add("Problem Setup: minimize the difference between defined and calculated (final) conversions, subject to all mole flows being equal to or higher than zero.")

                ' problem setup: minimize the difference between defined and calculated (final) conversions, subject to Ni >= 0.

                IObj2?.Paragraphs.Add("This solution scheme for parallel reactions guarantees that the mass balance is preserved, even if the final conversion values aren't reached due to limited reactant amounts.")

                ' this solution scheme for parallel reactions guarantees that the mass balance is preserved, even if the final
                ' conversion values aren't reached due to limited reactant amounts.

                Dim xref(ar.Count - 1), xf(ar.Count - 1), dni(N00.Count - 1) As Double

                Dim splex As New Simplex()
                Dim vars As New List(Of OptSimplexBoundVariable)

                splex.MaxFunEvaluations = 100000
                splex.Tolerance = 1.0E-20

                i = 0
                Do

                    'check defined conversions

                    rxn = FlowSheet.Reactions(ar(i))

                    rxn.ExpContext = New Ciloci.Flee.ExpressionContext
                    rxn.ExpContext.Imports.AddType(GetType(System.Math))
                    rxn.ExpContext.Variables.Clear()
                    rxn.ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    If ReactorOperationMode = OperationMode.OutletTemperature Then
                        rxn.ExpContext.Variables.Add("T", OutletTemperature)
                    Else
                        rxn.ExpContext.Variables.Add("T", ims.Phases(0).Properties.temperature.GetValueOrDefault)
                    End If

                    rxn.Expr = rxn.ExpContext.CompileGeneric(Of Double)(rxn.Expression)
                    X = rxn.Expr.Evaluate / 100

                    If X < 0.0# Or X > 1.0# Then Throw New ArgumentOutOfRangeException("Conversion Expression", "The conversion expression for reaction " & rxn.Name & " results in a value that is out of the valid range (0 to 100%).")

                    'store defined (reference) conversions for simplex solver

                    xref(i) = X

                    'create simplex vars

                    vars.Add(New OptSimplexBoundVariable(xref(i), 0.0000000001, xref(i)))

                    i += 1

                Loop Until i = ar.Count

                'solve parallel reactions 
                'xf = final conversion values

                Dim pen_val As Double = 0.0#
                Dim fval As Double

                xf = splex.ComputeMin(Function(xi)

                                          Dim i2, j2, n2 As Integer, m0 As Double

                                          n2 = xi.Length - 1

                                          dni = ims.PropertyPackage.RET_NullVector()

                                          For i2 = 0 To n2

                                              'process reaction i2

                                              rxn = FlowSheet.Reactions(ar(i2))
                                              BC = rxn.BaseReactant
                                              scBC = rxn.Components(BC).StoichCoeff

                                              Select Case rxn.ReactionPhase
                                                  Case PhaseName.Liquid
                                                      m0 = ims.Phases(1).Properties.molarflow.GetValueOrDefault
                                                      nif = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Liquid).MultiplyConstY(m0)
                                                      nBC = ims.Phases(1).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                                                  Case PhaseName.Vapor
                                                      m0 = ims.Phases(2).Properties.molarflow.GetValueOrDefault
                                                      nif = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Vapor).MultiplyConstY(m0)
                                                      nBC = ims.Phases(2).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                                                  Case PhaseName.Mixture
                                                      m0 = ims.Phases(0).Properties.molarflow.GetValueOrDefault
                                                      nif = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).MultiplyConstY(m0)
                                                      nBC = ims.Phases(0).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                                              End Select

                                              'delta mole flows

                                              For Each sb As ReactionStoichBase In rxn.Components.Values
                                                  j2 = cnames.IndexOf(sb.CompName)
                                                  dni(j2) += -xi(i2) * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC
                                              Next

                                              'calculate final mole amounts

                                              For Each sb As ReactionStoichBase In rxn.Components.Values
                                                  j2 = cnames.IndexOf(sb.CompName)
                                                  nif(j2) += dni(j2)
                                              Next

                                          Next

                                          'calculate a penalty value for the objective function due to negative mole flows

                                          pen_val = 0.0
                                          For Each d In nif
                                              If d < 0.0 Then
                                                  pen_val += Math.Abs(d) * 10000000000.0
                                              End If
                                          Next

                                          fval = xref.SubtractY(xi).AbsSqrSumY

                                          Return fval + pen_val

                                      End Function, vars.ToArray)

                IObj2?.Paragraphs.Add(String.Format("Specified Reaction Conversions: {0}", xref.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("Final Reaction Conversions: {0}", xf.ToMathArrayString))

                ' at this point, the xf vector holds the final conversion values as calculated 
                ' by the simplex solver, and the energy balance can be calculated (again). 

                DHr = 0.0

                i = 0
                Do

                    'process reaction i
                    rxn = FlowSheet.Reactions(ar(i))
                    BC = rxn.BaseReactant
                    scBC = rxn.Components(BC).StoichCoeff

                    Select Case rxn.ReactionPhase
                        Case PhaseName.Liquid
                            nBC = ims.Phases(1).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                        Case PhaseName.Vapor
                            nBC = ims.Phases(2).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                        Case PhaseName.Mixture
                            nBC = ims.Phases(0).Compounds(rxn.BaseReactant).MolarFlow.GetValueOrDefault()
                    End Select

                    If Not Me.Conversions.ContainsKey(rxn.ID) Then
                        Me.Conversions.Add(rxn.ID, xf(i))
                    Else
                        Me.Conversions(rxn.ID) = xf(i)
                    End If

                    'delta mole flows

                    For Each sb As ReactionStoichBase In rxn.Components.Values
                        If Not DN.ContainsKey(sb.CompName) Then
                            DN.Add(sb.CompName, -xf(i) * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC)
                        Else
                            DN(sb.CompName) += -xf(i) * rxn.Components(sb.CompName).StoichCoeff / scBC * nBC
                        End If
                    Next

                    Dim DNbr = -xf(i) * rxn.Components(rxn.BaseReactant).StoichCoeff / scBC * nBC

                    'Heat released (or absorbed) (kJ/s = kW) (Ideal Gas)
                    DHr += rxn.ReactionHeat * Abs(DNbr) / 1000

                    i += 1

                Loop Until i = ar.Count

                'final mole flows

                For Each s1 As Compound In ims.Phases(0).Compounds.Values
                    If DN.ContainsKey(s1.Name) Then
                        N0(s1.Name) += DN(s1.Name)
                        If N0(s1.Name) < 0.0 Then
                            N0(s1.Name) = 0.0
                        End If
                    End If
                Next

                IObj2?.Paragraphs.Add(String.Format("Total Heat of Reaction: {0} kW", DHr))

                'update mole flows/fractions

                Dim Nsum As Double = 0
                For Each s2 As Compound In ims.Phases(0).Compounds.Values
                    If N0.ContainsKey(s2.Name) Then
                        Nsum += N0(s2.Name)
                    Else
                        Nsum += s2.MolarFlow.GetValueOrDefault()
                    End If
                Next

                For Each s3 As Compound In ims.Phases(0).Compounds.Values
                    If N0.ContainsKey(s3.Name) Then
                        s3.MoleFraction = N0(s3.Name) / Nsum
                    Else
                        s3.MoleFraction = s3.MolarFlow.GetValueOrDefault() / Nsum
                    End If
                Next

                ims.Phases(0).Properties.molarflow = Nsum

                Dim mmm As Double = 0

                For Each s3 As Compound In ims.Phases(0).Compounds.Values
                    mmm += s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight
                Next

                For Each s3 As Compound In ims.Phases(0).Compounds.Values
                    s3.MassFraction = s3.MoleFraction.GetValueOrDefault * s3.ConstantProperties.Molar_Weight / mmm
                Next

                'do a flash calc (calculate final temperature/enthalpy)

                pp.CurrentMaterialStream = ims

                Select Case Me.ReactorOperationMode

                    Case OperationMode.Adiabatic

                        ims.SpecType = StreamSpec.Pressure_and_Enthalpy

                        'Products Enthalpy (kJ/kg * kg/s = kW)

                        Hp = (Hr - DHr) / ims.GetMassFlow()

                        ims.SetMassEnthalpy(Hp + Qin / W)

                        ims.Calculate(True, True)

                        Dim Tout As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault
                        Me.DeltaT = Tout - Tin

                        IObj2?.Paragraphs.Add(String.Format("Heat Balance: {0} kW", DeltaQ))

                    Case OperationMode.Isothermic

                        IObj?.SetCurrent()
                        ims.SpecType = StreamSpec.Temperature_and_Pressure
                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        IObj2?.Paragraphs.Add(String.Format("Products Enthalpy: {0} kJ/kg", Hp))

                        'Heat (kW)
                        Me.DeltaQ += DHr

                        Me.DeltaT = 0

                        IObj2?.Paragraphs.Add(String.Format("Heat Balance: {0} kW", DeltaQ))

                    Case OperationMode.OutletTemperature

                        Dim Tout As Double = Me.OutletTemperature

                        Me.DeltaT = Tout - Tin

                        ims.Phases(0).Properties.temperature = Tout
                        ims.SpecType = StreamSpec.Temperature_and_Pressure

                        IObj?.SetCurrent()
                        ims.Calculate(True, True)

                        'Products Enthalpy (kJ/kg * kg/s = kW)
                        Hp = ims.Phases(0).Properties.enthalpy.GetValueOrDefault * ims.Phases(0).Properties.massflow.GetValueOrDefault

                        IObj2?.Paragraphs.Add(String.Format("Products Enthalpy: {0} kJ/kg", Hp))

                        'Heat (kW)
                        Me.DeltaQ += DHr

                        IObj2?.Paragraphs.Add(String.Format("Heat Balance: {0} kW", DeltaQ))

                End Select

                IObj2?.Paragraphs.Add(String.Format("Outlet Temperature: {0} K", OutletTemperature))

                IObj2?.Close()

            Next

            Select Case Me.ReactorOperationMode

                Case OperationMode.Adiabatic

                    Me.DeltaQ = 0.0#

                Case OperationMode.Isothermic, OperationMode.OutletTemperature

                    Me.DeltaQ += Hp + Hid_r - Hr0 - Hid_p

            End Select

            'Compound conversions

            pp.CurrentMaterialStream = ims

            If dynamics Then
                AccumulationStream.Assign(ims)
                AccumulationStream.AssignProps(ims)
            End If

            nif = ims.PropertyPackage.RET_VMOL(PropertyPackages.Phase.Mixture).MultiplyConstY(ims.Phases(0).Properties.molarflow.GetValueOrDefault)

            Me.ComponentConversions.Clear()
            For i = 0 To ni0.Length - 1
                Me.ComponentConversions(cnames(i)) = (ni0(i) - nif(i)) / ni0(i)
            Next

            IObj?.Paragraphs.Add(String.Format("Compounds: {0}", N00.Keys.ToArray.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Initial Mole Flows: {0} mol/s", ni0.ToArray.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final Mole Flows: {0} mol/s", nif.ToArray.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Conversions: {0}", ComponentConversions.Values.Select(Of Double)(Function(d) If(Not Double.IsNaN(d) And Not Double.IsInfinity(d), d, 0.0)).ToArray.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Products Enthalpy: {0} kJ/kg", Hp))

            IObj?.Paragraphs.Add(String.Format("Heat Balance: {0} kW", DeltaQ))

            IObj?.Paragraphs.Add(String.Format("Outlet Temperature: {0} K", OutletTemperature))

            'Copy results to upstream MS
            Dim xl, xv, xs, T, P, H, S, wtotalx, wtotaly, wtotalS, wl, wv, ws As Double
            Dim nc As Integer = ims.Phases(0).Compounds.Count - 1
            pp.CurrentMaterialStream = ims
            IObj?.SetCurrent()

            tmp = pp.CalculateEquilibrium2(FlashCalculationType.PressureTemperature, ims.Phases(0).Properties.pressure.GetValueOrDefault, ims.Phases(0).Properties.temperature.GetValueOrDefault, 0)

            Dim Vx(nc), Vy(nc), Vs(nc), Vwx(nc), Vwy(nc), Vws(nc) As Double
            xl = tmp.GetLiquidPhase1MoleFraction
            xv = tmp.GetVaporPhaseMoleFraction
            xs = tmp.GetSolidPhaseMoleFraction
            wl = tmp.GetLiquidPhase1MassFraction
            wv = tmp.GetVaporPhaseMassFraction
            ws = tmp.GetSolidPhaseMassFraction
            T = tmp.CalculatedTemperature.GetValueOrDefault
            P = tmp.CalculatedPressure.GetValueOrDefault
            H = tmp.CalculatedEnthalpy.GetValueOrDefault
            S = tmp.CalculatedEntropy.GetValueOrDefault
            Vx = tmp.GetLiquidPhase1MoleFractions
            Vy = tmp.GetVaporPhaseMoleFractions
            Vs = tmp.GetSolidPhaseMoleFractions

            Dim Hv As Double

            Dim ids = ims.PropertyPackage.RET_VNAMES().ToList

            Dim ms As MaterialStream
            Dim cp As IConnectionPoint
            cp = Me.GraphicObject.InputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedFrom.Name)
                Dim comp As BaseClasses.Compound
                For Each comp In ms.Phases(0).Compounds.Values
                    wtotalx += Vx(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight
                    wtotaly += Vy(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight
                    wtotalS += Vs(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight
                Next
                For Each comp In ms.Phases(0).Compounds.Values
                    If wtotalx > 0 Then Vwx(ids.IndexOf(comp.Name)) = Vx(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight / wtotalx
                    If wtotaly > 0 Then Vwy(ids.IndexOf(comp.Name)) = Vy(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight / wtotaly
                    If wtotalS > 0 Then Vws(ids.IndexOf(comp.Name)) = Vs(ids.IndexOf(comp.Name)) * comp.ConstantProperties.Molar_Weight / wtotalS
                Next
            End If

            cp = Me.GraphicObject.OutputConnectors(0)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        If xv = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = Vy(ids.IndexOf(comp.Name))
                            comp.MassFraction = Vwy(ids.IndexOf(comp.Name))
                        End If
                    Next
                    .PropertyPackage.CurrentMaterialStream = ms
                    Hv = .PropertyPackage.DW_CalcEnthalpy(ms.GetOverallComposition(), T, P, PropertyPackages.State.Vapor)
                    .Phases(0).Properties.enthalpy = Hv
                    .Phases(0).Properties.massflow = W * wv
                    .Phases(0).Properties.massfraction = 1.0#
                    .Phases(0).Properties.molarfraction = 1.0#
                End With
            End If

            cp = Me.GraphicObject.OutputConnectors(1)
            If cp.IsAttached Then
                ms = FlowSheet.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                With ms
                    .SpecType = StreamSpec.Temperature_and_Pressure
                    .Phases(0).Properties.temperature = T
                    .Phases(0).Properties.pressure = P
                    Dim comp As BaseClasses.Compound
                    For Each comp In .Phases(0).Compounds.Values
                        If (xl + xs) = 0.0# Then
                            comp.MoleFraction = 0.0#
                            comp.MassFraction = 0.0#
                        Else
                            comp.MoleFraction = (Vx(ids.IndexOf(comp.Name)) * xl + Vs(ids.IndexOf(comp.Name)) * xs) / (xl + xs)
                            comp.MassFraction = (Vwx(ids.IndexOf(comp.Name)) * wtotalx + Vws(ids.IndexOf(comp.Name)) * wtotalS) / (wtotalx + wtotalS)
                        End If
                    Next
                    .Phases(0).Properties.enthalpy = (H - Hv * wv) / (1 - wv)
                    .Phases(0).Properties.massflow = W * (1 - wv)
                    .Phases(0).Properties.massfraction = 1.0#
                    .Phases(0).Properties.molarfraction = 1.0#
                End With
            End If

            If ReactorOperationMode <> OperationMode.Adiabatic Then
                'energy stream - update energy flow value (kW)
                If GetInletEnergyStream(1) IsNot Nothing Then
                    'energy stream - update energy flow value (kW)
                    With GetInletEnergyStream(1)
                        .EnergyFlow = Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With
                ElseIf GetOutletEnergyStream(2) IsNot Nothing Then
                    'energy stream - update energy flow value (kW)
                    With GetOutletEnergyStream(2)
                        .EnergyFlow = -Me.DeltaQ.GetValueOrDefault
                        .GraphicObject.Calculated = True
                    End With
                End If
            End If

            IObj?.Close()

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

            cp = Me.GraphicObject.OutputConnectors(1)
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
                            'PROP_HT_0    Pressure Drop
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.DeltaP.GetValueOrDefault)

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
                                If rx2 IsNot Nothing AndAlso Conversions.ContainsKey(rx2.ID) Then
                                    value = Conversions(rx2.ID) * 100
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
                    proplist.Add("Calculation Mode")
                    For Each item In Conversions
                        proplist.Add(FlowSheet.Reactions(item.Key).Name + ": Extent")
                    Next
                    For Each item In ComponentConversions
                        proplist.Add(item.Key + ": Conversion")
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
                    'PROP_HT_0	Pressure Drop
                    Me.DeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)

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
                                'PROP_HT_0	Pressure Drop
                                value = su.deltaP

                        End Select

                        Return value

                    Catch ex As Exception

                        Return ""

                    End Try

                Else

                    Select Case prop
                        Case "Calculation Mode"
                            Return ""
                        Case Else
                            If prop.Contains("Conversion") Then value = "%"
                            If prop.Contains("Extent") Then value = "%"
                    End Select

                End If

                Return value

            End If
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_ReactorConvEqGibbs With {.SimObject = Me}
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
            Return My.Resources.reactor_conversion
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CONV_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CONV_Name")
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
            str.AppendLine("    Calculation mode: " & ReactorOperationMode.ToString)
            str.AppendLine("    Pressure drop: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(numberformat, ci) & " " & su.deltaP)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            Select Case Me.ReactorOperationMode
                Case OperationMode.Adiabatic
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                Case OperationMode.Isothermic
                    str.AppendLine("    Heat added/removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
                Case OperationMode.OutletTemperature
                    str.AppendLine("    Outlet Temperature: " & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.OutletTemperature).ToString(numberformat, ci) & " " & su.temperature)
                    str.AppendLine("    Heat added/removed: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.DeltaQ.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)
            End Select
            str.AppendLine()
            str.AppendLine("Reaction Conversions")
            str.AppendLine()
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.Conversions
                    str.AppendLine("    " & Me.GetFlowsheet.Reactions(dbl.Key).Name & ": " & (dbl.Value * 100).ToString(numberformat, ci) & "%")
                Next
            End If
            str.AppendLine()
            str.AppendLine("Compound Conversions")
            str.AppendLine()
            For Each dbl As KeyValuePair(Of String, Double) In Me.ComponentConversions
                str.AppendLine("    " & dbl.Key & ": " & (dbl.Value * 100).ToString(numberformat, ci) & "%")
            Next
            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Conversion Reactor '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))


            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Calculation Parameters"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn, New String() {"Calculation Mode", ReactorOperationMode.ToString}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn, New String() {"Pressure Drop", SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.DeltaP.GetValueOrDefault).ToString(nf), su.deltaP}))

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

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Reaction Conversions"}))
            If Not Me.Conversions Is Nothing Then
                For Each dbl As KeyValuePair(Of String, Double) In Me.Conversions
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {Me.GetFlowsheet.Reactions(dbl.Key).Name,
                            (dbl.Value * 100).ToString(nf), "%"}))
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
            Else
                Return p
            End If
        End Function

    End Class

End Namespace
