'    Stream Classes
'    Copyright 2008-2011 Daniel Wagner O. de Medeiros
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
Imports CapeOpen
Imports System.Linq
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks
Imports System.Runtime.Serialization
Imports System.Reflection
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports System.Windows.Forms
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Globalization

Namespace Streams

    <System.Serializable()> Public Class MaterialStream

        Inherits UnitOperations.BaseClass

        'CAPE-OPEN 1.0
        Implements ICapeIdentification, ICapeThermoMaterialObject, ICapeThermoCalculationRoutine, ICapeThermoEquilibriumServer, ICapeThermoPropertyPackage, ICapeThermoMaterialTemplate

        'CAPE-OPEN 1.1
        Implements ICapeThermoMaterial, ICapeThermoCompounds, ICapeThermoPhases, ICapeThermoUniversalConstant, ICapeThermoPropertyRoutine, ICapeThermoEquilibriumRoutine, ICapeThermoMaterialContext

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        Implements Interfaces.IMaterialStream

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As MaterialStreamEditor

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public _pp As PropertyPackages.PropertyPackage
        Public _ppid As String = ""

        Protected m_Phases As New Dictionary(Of Integer, IPhase)

        <System.NonSerialized()> Private _flowsheet As Interfaces.IFlowsheet

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Streams

#Region "    XML serialization"

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            Try
                Me._ppid = (From xel As XElement In data Select xel Where xel.Name = "PropertyPackage").SingleOrDefault.Value
            Catch ex As Exception
            End Try

            Dim dataPhases As List(Of XElement) = (From xel As XElement In data Select xel Where xel.Name = "Phases").Elements.ToList

            Me.Phases.Clear()

            For Each xel As XElement In dataPhases
                Dim p As New BaseClasses.Phase(xel.<Name>.Value, "")
                p.LoadData(xel.Elements.ToList)
                Me.Phases.Add(xel.<ID>.Value, p)
            Next

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()

            With elements
                Dim ppid As String = ""
                If _ppid <> "" Then
                    ppid = _ppid
                ElseIf Not _pp Is Nothing Then
                    ppid = _pp.Name
                Else
                    ppid = ""
                End If
                .Add(New XElement("PropertyPackage", ppid))
                .Add(New XElement("Phases"))
                For Each kvp As KeyValuePair(Of Integer, IPhase) In m_Phases
                    .Item(.Count - 1).Add(New XElement("Phase", {New XElement("ID", kvp.Key), DirectCast(kvp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
                Next
            End With

            Return elements

        End Function

#End Region

#Region "    DWSIM Specific"

        Public Overrides Function GetDebugReport() As String

            Me.DebugMode = True
            Me.DebugText = ""

            Try

                Calculate(True, True)

            Catch ex As Exception

                Dim st As New StackTrace(ex, True)
                Dim frame As StackFrame = st.GetFrame(0)
                Dim fileName As String = IO.Path.GetFileName(frame.GetFileName)
                Dim methodName As String = frame.GetMethod().Name
                Dim line As Integer = frame.GetFileLineNumber()

                AppendDebugLine(String.Format("Exception raised from file {0}, at method {1}, line {2}:", fileName, methodName, line))
                AppendDebugLine(ex.Message.ToString)

            Finally

                Me.DebugMode = False

            End Try

            Return DebugText

        End Function

        Public Overrides Sub Validate()

            Dim mytag As String = ""
            If Not Me.GraphicObject Is Nothing Then mytag = Me.GraphicObject.Tag

            'temperature
            If Not Me.Phases(0).Properties.temperature.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidMSSpecValue") & " (stream: " & mytag & ", name: temperature, value: " & Me.Phases(0).Properties.temperature.GetValueOrDefault & ")")
            'pressure
            If Not Me.Phases(0).Properties.pressure.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidMSSpecValue") & " (stream: " & mytag & ", name: pressure, value: " & Me.Phases(0).Properties.pressure.GetValueOrDefault & ")")
            'enthalpy
            If Not Me.Phases(0).Properties.enthalpy.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidMSSpecValue") & " (stream: " & mytag & ", name: enthalpy, value: " & Me.Phases(0).Properties.enthalpy.GetValueOrDefault & ")")
            'entropy
            If Not Me.Phases(0).Properties.entropy.IsValid Then Throw New ArgumentException(Me.FlowSheet.GetTranslatedString("ErrorInvalidMSSpecValue") & " (stream: " & mytag & ", name: entropy, value: " & Me.Phases(0).Properties.entropy.GetValueOrDefault & ")")

        End Sub

        ''' <summary>
        ''' Gets or sets if this stream is at thermodynamic equilbirium or not.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>

        Public Property AtEquilibrium() As Boolean = False Implements IMaterialStream.AtEquilibrium

        Public Function GetPropertyPackageObject() As Object Implements IMaterialStream.GetPropertyPackageObject
            Return PropertyPackage
        End Function

        Public Function GetPropertyPackageObjectCopy() As Object Implements IMaterialStream.GetPropertyPackageObjectCopy
            Return PropertyPackage.Clone
        End Function

        Sub SetPropertyPackage(pp As Object) Implements IMaterialStream.SetPropertyPackageObject
            PropertyPackage = pp
        End Sub

        Sub SetCurrentMaterialStream(ms As Object) Implements IMaterialStream.SetCurrentMaterialStream
            PropertyPackage.CurrentMaterialStream = ms
        End Sub

        ''' <summary>
        ''' Gets or sets the associated Property Package for this stream.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public Shadows Property PropertyPackage() As PropertyPackage
            Get
                If Not _pp Is Nothing Then Return _pp
                If _ppid Is Nothing Then _ppid = ""
                If Not FlowSheet Is Nothing Then
                    If FlowSheet.PropertyPackages.ContainsKey(_ppid) Then
                        Return FlowSheet.PropertyPackages(_ppid)
                    Else
                        For Each pp As PropertyPackages.PropertyPackage In FlowSheet.PropertyPackages.Values
                            _ppid = pp.UniqueID
                            Return pp
                            Exit For
                        Next
                    End If
                Else
                    _ppid = _pp?.UniqueID
                    Return _pp
                End If
                Return Nothing
            End Get
            Set(ByVal value As PropertyPackage)
                If value IsNot Nothing Then
                    _ppid = value.UniqueID
                    _pp = value
                Else
                    _pp = Nothing
                End If

            End Set
        End Property

        Public Sub New()

            MyBase.New()

            Me.Phases.Add(0, New BaseClasses.Phase("Mixture", ""))
            Me.Phases.Add(1, New BaseClasses.Phase(("OverallLiquid"), ""))
            Me.Phases.Add(2, New BaseClasses.Phase(("Vapor"), ""))
            Me.Phases.Add(3, New BaseClasses.Phase(("Liquid1"), ""))
            Me.Phases.Add(4, New BaseClasses.Phase(("Liquid2"), ""))
            Me.Phases.Add(5, New BaseClasses.Phase(("Liquid3"), ""))
            Me.Phases.Add(6, New BaseClasses.Phase(("Aqueous"), ""))
            Me.Phases.Add(7, New BaseClasses.Phase(("Solid"), ""))

            'assign default values for temperature, pressure and mass flow
            Me.Phases(0).Properties.temperature = 298.15
            Me.Phases(0).Properties.pressure = 101325
            Me.Phases(0).Properties.massflow = 1

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String, ByVal flowsheet As IFlowsheet, ByVal proppack As PropertyPackages.PropertyPackage)

            MyBase.CreateNew()

            Me.SetFlowsheet(flowsheet)
            If Me.PropertyPackage Is Nothing Then Me.PropertyPackage = proppack

            Me.ComponentName = name
            Me.ComponentDescription = description

            Me.Phases.Add(0, New BaseClasses.Phase("Mixture", ""))
            Me.Phases.Add(1, New BaseClasses.Phase(("OverallLiquid"), ""))
            Me.Phases.Add(2, New BaseClasses.Phase(("Vapor"), ""))
            Me.Phases.Add(3, New BaseClasses.Phase(("Liquid1"), ""))
            Me.Phases.Add(4, New BaseClasses.Phase(("Liquid2"), ""))
            Me.Phases.Add(5, New BaseClasses.Phase(("Liquid3"), ""))
            Me.Phases.Add(6, New BaseClasses.Phase(("Aqueous"), ""))
            Me.Phases.Add(7, New BaseClasses.Phase(("Solid"), ""))

            'Me.PropertyPackage = FlowSheet.Options.PropertyPackages(0)

            'assign default values for temperature, pressure and mass flow
            Me.Phases(0).Properties.temperature = 298.15
            Me.Phases(0).Properties.pressure = 101325
            Me.Phases(0).Properties.massflow = 1

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            Me.ComponentName = name
            Me.ComponentDescription = description

            Me.Phases.Add(0, New BaseClasses.Phase("Mixture", ""))
            Me.Phases.Add(1, New BaseClasses.Phase(("OverallLiquid"), ""))
            Me.Phases.Add(2, New BaseClasses.Phase(("Vapor"), ""))
            Me.Phases.Add(3, New BaseClasses.Phase(("Liquid1"), ""))
            Me.Phases.Add(4, New BaseClasses.Phase(("Liquid2"), ""))
            Me.Phases.Add(5, New BaseClasses.Phase(("Liquid3"), ""))
            Me.Phases.Add(6, New BaseClasses.Phase(("Aqueous"), ""))
            Me.Phases.Add(7, New BaseClasses.Phase(("Solid"), ""))

            'assign default values for temperature, pressure and mass flow
            Me.Phases(0).Properties.temperature = 298.15
            Me.Phases(0).Properties.pressure = 101325
            Me.Phases(0).Properties.massflow = 1

        End Sub

        ''' <summary>
        ''' Gets the collection of phases in this stream.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overrides ReadOnly Property Phases() As Dictionary(Of Integer, IPhase) Implements IMaterialStream.Phases
            Get
                Return m_Phases
            End Get
        End Property

        Public Function GetPhase(phasename As String) As BaseClasses.Phase
            Select Case phasename
                Case "Vapor"
                    Return Phases(2)
                Case "LiquidMixture", "OverallLiquid", "Líquido (Mistura)"
                    Return Phases(1)
                Case "Liquid1", "Líquido 1"
                    Return Phases(3)
                Case "Liquid2", "Líquido 2"
                    Return Phases(4)
                Case "Liquid3", "Líquido 3"
                    Return Phases(5)
                Case "Aqueous", "Phase Aquosa"
                    Return Phases(6)
                Case "Solid", "Sólida"
                    Return Phases(7)
                Case "Mixture", "Mistura"
                    Return Phases(0)
                Case Else
                    Return Phases(0)
            End Select
        End Function

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)
            Calculate(True, True)
        End Sub

        ''' <summary>
        ''' Calculates equilibrium and/or properties for this stream.
        ''' </summary>
        ''' <param name="equilibrium"></param>
        ''' <param name="properties"></param>
        ''' <remarks></remarks>
        Public Overloads Sub Calculate(equilibrium As Boolean, properties As Boolean)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", "Material Stream Calculation Routine", True)

            IObj?.Paragraphs.Add("The Material Stream Calculation routine is responsible to calculate the phase distribution and its properties according to the specifications.")

            IObj?.Paragraphs.Add("To calculate a Material Stream, DWSIM needs its mixture composition and two specified State Variables (Temperature, Pressure, Enthalpy, Entropy or Vapor Fraction).")

            Dim doparallel As Boolean = Settings.EnableParallelProcessing

            Dim T As Double = Me.Phases(0).Properties.temperature.GetValueOrDefault
            Dim P As Double = Me.Phases(0).Properties.pressure.GetValueOrDefault
            Dim W As Nullable(Of Double) = Me.Phases(0).Properties.massflow
            Dim Q As Nullable(Of Double) = Me.Phases(0).Properties.molarflow
            Dim QV As Nullable(Of Double) = Me.Phases(0).Properties.volumetric_flow
            Dim H As Double = Me.Phases(0).Properties.enthalpy.GetValueOrDefault
            Dim S As Double = Me.Phases(0).Properties.entropy.GetValueOrDefault

            If DebugMode Then AppendDebugLine(String.Format("Calculation spec: {0}", SpecType.ToString))

            IObj?.Paragraphs.Add(String.Format("Calculation Specification: {0}", SpecType.ToString))

            Select Case SpecType
                Case StreamSpec.Pressure_and_Enthalpy
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: P = {0} Pa, H = {1} kJ/kg", P, H))
                    IObj?.Paragraphs.Add(String.Format("Specified State Variables: P = {0} Pa, H = {1} kJ/kg", P, H))
                Case StreamSpec.Pressure_and_Entropy
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: P = {0} Pa, S = {1} kJ/kg.K", P, S))
                    IObj?.Paragraphs.Add(String.Format("Specified State Variables: P = {0} Pa, S = {1} kJ/kg", P, S))
                Case StreamSpec.Pressure_and_VaporFraction
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: P = {0} Pa, VF = {1}", P, Me.Phases(2).Properties.molarfraction.GetValueOrDefault))
                    IObj?.Paragraphs.Add(String.Format("Specified State Variables: P = {0} Pa, VF = {1}", P, Me.Phases(2).Properties.molarfraction.GetValueOrDefault))
                Case StreamSpec.Temperature_and_Pressure
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, P = {1} Pa", T, P))
                    IObj?.Paragraphs.Add(String.Format("Specified State Variables: T = {0} K, P = {1} Pa", T, P))
                Case StreamSpec.Temperature_and_VaporFraction
                    If DebugMode Then AppendDebugLine(String.Format("Input variables: T = {0} K, VF = {1}", T, Me.Phases(2).Properties.molarfraction.GetValueOrDefault))
                    IObj?.Paragraphs.Add(String.Format("Specified State Variables: T = {0} K, VF = {1}", T, Me.Phases(2).Properties.molarfraction.GetValueOrDefault))
            End Select

            Dim subs As BaseClasses.Compound
            Dim comp As Double = 0
            For Each subs In Me.Phases(0).Compounds.Values
                comp += subs.MoleFraction.GetValueOrDefault
            Next

            IObj?.Paragraphs.Add(String.Format("Total Molar Composition: {0}", comp.ToString))

            'update mass fractions, just to make sure they're there.

            CalcOverallCompMassFractions()

            If DebugMode Then AppendDebugLine(String.Format("Checking mixture composition. Sum must be higher than zero. Sum = {0}", comp))

            Dim foption As Integer

            IObj?.Paragraphs.Add(String.Format("The user can specify one of the three flow amounts: Mass, Mole or Volumetric. 
                                                Only one is required to calculate the other two. DWSIM checks for a value in this order: Mass, Mole and Volumetric.
                                                As soon as a value is found, the other two will be calculated automatically."))

            With Me.PropertyPackage

                .CurrentMaterialStream = Me

                If W.HasValue Then
                    If DebugMode Then AppendDebugLine(String.Format("Checking flow definition. Mass flow specified, will calculate molar and volumetric flow."))
                    IObj?.Paragraphs.Add("Checking flow definition... Mass Flow specified, will calculate Molar and Volumetric Flows.")
                    foption = 0
                    .DW_CalcVazaoMolar()
                ElseIf Q.HasValue Then
                    If DebugMode Then AppendDebugLine(String.Format("Checking flow definition. Molar flow specified, will calculate mass and volumetric flow."))
                    IObj?.Paragraphs.Add("Checking flow definition... Molar Flow specified, will calculate Mass and Volumetric Flows.")
                    foption = 1
                    .DW_CalcVazaoMassica()
                ElseIf QV.HasValue Then
                    If DebugMode Then AppendDebugLine(String.Format("Checking flow definition. Volumetric flow specified, will calculate mass and molar flow."))
                    IObj?.Paragraphs.Add("Checking flow definition... Volumetric Flow specified, will calculate Mass and Mole Flows.")
                    foption = 2
                    Me.Phases(0).Properties.molarflow = 1.0#
                    Me.Phases(0).Properties.massflow = 1.0#
                End If

                If DebugMode Then AppendDebugLine(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
                If DebugMode Then AppendDebugLine(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))

                IObj?.Paragraphs.Add(String.Format("Property Package: {0}", Me.PropertyPackage.ComponentName))
                IObj?.Paragraphs.Add(String.Format("Flash Algorithm: {0}", Me.PropertyPackage.FlashBase.GetType.Name))

                If equilibrium And comp > 0.0# Then

                    IObj?.Paragraphs.Add("Phase Equilibria will be calculated using the currently selected Property Package and Flash Algorithm.")

                    IObj?.Paragraphs.Add("To calculate the Phase Equilibria, DWSIM will call the 'DW_CalcEquilibrium' routine from the Property Package instance.")

                    If DebugMode Then AppendDebugLine(String.Format("Calculating phase equilibria..."))

                    If .AUX_IS_SINGLECOMP(PropertyPackages.Phase.Mixture) Then

                        If Not Me.GraphicObject Is Nothing AndAlso Me.GraphicObject.InputConnectors(0).IsAttached AndAlso
                            Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_Recycle Then
                            If DebugMode Then AppendDebugLine(String.Format("Stream is single-compound and attached to the outlet of an unit operation. PH flash equilibrium calculation forced."))
                            IObj?.Paragraphs.Add("<b>WARNING: Stream is single-compound and attached to the outlet of an unit operation. PH flash equilibrium calculation forced.</b>")
                            IObj?.Paragraphs.Add(String.Format("<b>Current State Variables: P = {0} Pa, H = {1} kJ/kg</b>", P, H))
                            .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H)
                        Else
                            Select Case Me.SpecType
                                Case StreamSpec.Temperature_and_Pressure
                                    .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                                Case StreamSpec.Pressure_and_Enthalpy
                                    .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H)
                                Case StreamSpec.Pressure_and_Entropy
                                    .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.S)
                                Case StreamSpec.Pressure_and_VaporFraction
                                    .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.VAP)
                                Case StreamSpec.Temperature_and_VaporFraction
                                    .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP)
                            End Select
                        End If
                    Else
                        Select Case Me.SpecType
                            Case StreamSpec.Temperature_and_Pressure
                                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.P)
                            Case StreamSpec.Pressure_and_Enthalpy
                                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.H)
                            Case StreamSpec.Pressure_and_Entropy
                                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.S)
                            Case StreamSpec.Pressure_and_VaporFraction
                                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.P, PropertyPackages.FlashSpec.VAP)
                            Case StreamSpec.Temperature_and_VaporFraction
                                .DW_CalcEquilibrium(PropertyPackages.FlashSpec.T, PropertyPackages.FlashSpec.VAP)
                        End Select
                    End If

                    If DebugMode Then AppendDebugLine(String.Format("Phase equilibria calculated succesfully."))

                    IObj?.Paragraphs.Add("Phase equilibria calculated succesfully.")

                    IObj?.Paragraphs.Add("Now that the phase distribution was determined successfully, DWSIM will proceed to calculate the properties for each one of them, again using the associated Property Package.")

                End If

                If properties Then

                    If DebugMode Then AppendDebugLine(String.Format("Calculating phase properties..."))

                    If foption = 2 Then

                        IObj?.Paragraphs.Add("Volumetric Flow specified. In this case, DWSIM needs to calculate the overall mixture density first, in order to calculate the Mass Flow with")

                        IObj?.Paragraphs.Add("<math>W = \rho Q</math>")

                        IObj?.SetCurrent()

                        .DW_CalcOverallDensity()

                        IObj?.Paragraphs.Add(String.Format("Specified Volumetric Flow: {0} m3/s", QV.GetValueOrDefault))

                        IObj?.Paragraphs.Add(String.Format("Calculated Mixture Density: {0} kg/m3", Phases(0).Properties.density.GetValueOrDefault))

                        Me.Phases(0).Properties.massflow = QV.GetValueOrDefault * Me.Phases(0).Properties.density.GetValueOrDefault

                        If DebugMode Then AppendDebugLine(String.Format("Calculated mass flow: {0} kg/s", Me.Phases(0).Properties.massflow.GetValueOrDefault))

                        IObj?.Paragraphs.Add(String.Format("Calculated Mass Flow: {0} kg/s", Phases(0).Properties.massflow.GetValueOrDefault))

                        IObj?.SetCurrent()

                        .DW_CalcVazaoMolar()

                        If DebugMode Then AppendDebugLine(String.Format("Calculated molar flow: {0} mol/s.", Me.Phases(0).Properties.molarflow.GetValueOrDefault))

                        IObj?.Paragraphs.Add(String.Format("Calculated Molar Flow: {0} kg/s", Phases(0).Properties.molarflow.GetValueOrDefault))

                    End If

                    IObj?.Paragraphs.Add("Phase Properties will be calculated using the currently selected Property Package.")

                    IObj?.Paragraphs.Add("To calculate the phase properties, DWSIM will call the 'DW_CalcPhaseProps' routine from the Property Package for each present phase.")

                    If doparallel Then

                        Dim task1 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 1'...")
                                                                  .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid1)
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
                                                              End If
                                                          End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Dim task2 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 2'...")
                                                                  .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid2)
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid2)
                                                              End If
                                                          End Sub,
                                                     Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Dim task3 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 3'...")
                                                                  .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid3)
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid3)
                                                              End If
                                                          End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Dim task4 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Aqueous'...")
                                                                  .DW_CalcPhaseProps(PropertyPackages.Phase.Aqueous)
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Aqueous)
                                                              End If
                                                          End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Dim task5 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Solid'...")
                                                                  .DW_CalcSolidPhaseProps()
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Solid)
                                                              End If
                                                          End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Dim task6 = Task.Factory.StartNew(Sub()
                                                              If Me.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                                                                  IObj?.Paragraphs.Add("Calculating properties of Phase 'Vapor'...")
                                                                  .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                                                              Else
                                                                  .DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
                                                              End If
                                                          End Sub,
                                                  Settings.TaskCancellationTokenSource.Token,
                                                  TaskCreationOptions.None,
                                                 Settings.AppTaskScheduler)
                        Task.WaitAll(task1, task2, task3, task4, task5, task6)

                    Else
                        If Me.Phases(3).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 1'...")
                            IObj?.SetCurrent()
                            .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid1)
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
                        End If
                        If Me.Phases(4).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 2'...")
                            IObj?.SetCurrent()
                            .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid2)
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid2)
                        End If
                        If Me.Phases(5).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Liquid 3'...")
                            IObj?.SetCurrent()
                            .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid3)
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid3)
                        End If
                        If Me.Phases(6).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Aqueous'...")
                            IObj?.SetCurrent()
                            .DW_CalcPhaseProps(PropertyPackages.Phase.Aqueous)
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Aqueous)
                        End If
                        If Me.Phases(7).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Solid'...")
                            IObj?.SetCurrent()
                            .DW_CalcSolidPhaseProps()
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Solid)
                        End If
                        If Me.Phases(2).Properties.molarfraction.GetValueOrDefault > 0 Then
                            IObj?.Paragraphs.Add("Calculating properties of Phase 'Vapor'...")
                            IObj?.SetCurrent()
                            .DW_CalcPhaseProps(PropertyPackages.Phase.Vapor)
                        Else
                            .DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
                        End If
                    End If

                    IObj?.Paragraphs.Add("Additional (new) properties like Bulk Modulus and Speed of Sound are calculated in a separate step.")

                    IObj?.SetCurrent()

                    'calculate additional properties
                    .CalcAdditionalPhaseProperties()

                    IObj?.SetCurrent()

                    'process overridden properties (single phase)
                    .ProcessOverridenProperties(PhaseType.SinglePhase)

                    IObj?.SetCurrent()

                    If Me.Phases(2).Properties.molarfraction.GetValueOrDefault >= 0 And Me.Phases(2).Properties.molarfraction.GetValueOrDefault <= 1 Then
                        .DW_CalcPhaseProps(PropertyPackages.Phase.Liquid)
                        .DW_CalcLiqMixtureProps()
                    Else
                        .DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid)
                        .DW_CalcLiqMixtureProps()
                    End If

                    'process overridden properties (liquid mix)
                    .ProcessOverridenProperties(PhaseType.LiquidMixture)

                    If DebugMode Then AppendDebugLine(String.Format("Phase properties calculated successfully."))

                    Select Case foption
                        Case 0, 1
                            IObj?.SetCurrent()
                            .DW_CalcCompMolarFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcCompMassFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcCompVolFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcOverallProps()
                            IObj?.SetCurrent()
                            .DW_CalcTwoPhaseProps(PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Vapor)
                            IObj?.SetCurrent()
                            .DW_CalcVazaoVolumetrica()
                            IObj?.SetCurrent()
                            .DW_CalcKvalue()
                        Case 2
                            'Me.Phases(0).Properties.massflow = QV * Me.Phases(0).Properties.density.GetValueOrDefault
                            '.DW_CalcVazaoMolar()
                            IObj?.SetCurrent()
                            .DW_CalcCompMolarFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcCompMassFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcCompVolFlow(-1)
                            IObj?.SetCurrent()
                            .DW_CalcOverallProps()
                            IObj?.SetCurrent()
                            .DW_CalcTwoPhaseProps(PropertyPackages.Phase.Liquid, PropertyPackages.Phase.Vapor)
                            IObj?.SetCurrent()
                            .DW_CalcKvalue()
                    End Select

                    IObj?.SetCurrent()

                    'process overridden properties (overall)
                    .ProcessOverridenProperties(PhaseType.Overall)

                    IObj?.SetCurrent()

                    'calculate molar concentrations
                    .DW_CalcConcentrations()

                    If DebugMode Then AppendDebugLine(String.Format("Material Stream calculated successfully."))

                End If

                .CurrentMaterialStream = Nothing

            End With

            IObj?.Close()

        End Sub

        ''' <summary>
        ''' Copies basic properties from another stream.
        ''' </summary>
        ''' <param name="ASource">Stream to be used as the source</param>
        ''' <remarks>Properties copied: phase temperature, pressure, enthalpy and mole/mass flows.</remarks>
        Public Sub Assign(ByVal ASource As IMaterialStream)

            Me.AtEquilibrium = ASource.AtEquilibrium

            SpecType = ASource.SpecType

            'Copy properties from the ASource stream.

            Dim i As Integer

            For i = 0 To 7

                If ASource.Phases.ContainsKey(i) Then

                    Phases(i).Properties.temperature = ASource.Phases(i).Properties.temperature.GetValueOrDefault
                    Phases(i).Properties.pressure = ASource.Phases(i).Properties.pressure.GetValueOrDefault
                    Phases(i).Properties.enthalpy = ASource.Phases(i).Properties.enthalpy.GetValueOrDefault

                    For Each comp As BaseClasses.Compound In Phases(i).Compounds.Values
                        comp.MoleFraction = ASource.Phases(i).Compounds(comp.Name).MoleFraction.GetValueOrDefault
                        comp.MassFraction = ASource.Phases(i).Compounds(comp.Name).MassFraction.GetValueOrDefault
                        comp.MassFlow = ASource.Phases(i).Compounds(comp.Name).MassFlow.GetValueOrDefault
                        comp.MolarFlow = ASource.Phases(i).Compounds(comp.Name).MolarFlow.GetValueOrDefault
                        comp.Molality = ASource.Phases(i).Compounds(comp.Name).Molality.GetValueOrDefault
                        comp.Molarity = ASource.Phases(i).Compounds(comp.Name).Molarity.GetValueOrDefault
                        comp.PartialPressure = ASource.Phases(i).Compounds(comp.Name).PartialPressure.GetValueOrDefault
                        comp.PartialVolume = ASource.Phases(i).Compounds(comp.Name).PartialVolume.GetValueOrDefault
                        comp.VolumetricFlow = ASource.Phases(i).Compounds(comp.Name).VolumetricFlow.GetValueOrDefault
                        comp.VolumetricFraction = ASource.Phases(i).Compounds(comp.Name).VolumetricFraction.GetValueOrDefault
                        comp.Kvalue = ASource.Phases(i).Compounds(comp.Name).Kvalue
                        comp.ActivityCoeff = ASource.Phases(i).Compounds(comp.Name).ActivityCoeff.GetValueOrDefault
                        comp.FugacityCoeff = ASource.Phases(i).Compounds(comp.Name).FugacityCoeff.GetValueOrDefault
                    Next

                    Phases(i).Properties.massflow = ASource.Phases(i).Properties.massflow.GetValueOrDefault
                    Phases(i).Properties.molarflow = ASource.Phases(i).Properties.molarflow.GetValueOrDefault

                    Phases(i).Properties.massfraction = ASource.Phases(i).Properties.massfraction.GetValueOrDefault
                    Phases(i).Properties.molarfraction = ASource.Phases(i).Properties.molarfraction.GetValueOrDefault

                End If

            Next

        End Sub

        ''' <summary>
        ''' Copies phase properties from another stream.
        ''' </summary>
        ''' <param name="ASource">Source stream</param>
        ''' <remarks></remarks>
        Public Sub AssignProps(ByVal ASource As IMaterialStream)

            'Copy properties from the ASource stream.

            Dim i As Integer

            For i = 0 To 7

                If ASource.Phases.ContainsKey(i) Then

                    Dim props = ASource.Phases(i).Properties.GetType().GetProperties()

                    For Each p In props
                        p.SetValue(Phases(i).Properties, p.GetValue(ASource.Phases(i).Properties))
                    Next

                End If

            Next

        End Sub

        ''' <summary>
        ''' Clears the basic phase properties of this stream.
        ''' </summary>
        ''' <remarks>Properties cleared: phase temperature, pressure, enthalpy and mole/mass flows/fractions.</remarks>
        Public Sub Clear()

            Dim i As Integer

            For i = 0 To Phases.Count - 1

                Phases(i).Properties.temperature = Nothing
                Phases(i).Properties.pressure = Nothing
                Phases(i).Properties.enthalpy = Nothing
                Phases(i).Properties.molarfraction = Nothing
                Phases(i).Properties.massfraction = Nothing

                'Copy component properties.
                Dim comp As BaseClasses.Compound

                For Each comp In Phases(i).Compounds.Values
                    comp.MoleFraction = Nothing
                    comp.MassFraction = Nothing
                Next

                'Should be define after concentrations?!?!
                Phases(i).Properties.massflow = Nothing
                Phases(i).Properties.molarflow = Nothing
                Phases(i).Properties.volumetric_flow = Nothing

            Next

            AtEquilibrium = False

            If GraphicObject IsNot Nothing Then GraphicObject.Calculated = False

        End Sub

        ''' <summary>
        ''' Clear all calculated props on this stream.
        ''' </summary>
        Public Sub ClearCalculatedProps()
            Me.PropertyPackage.CurrentMaterialStream = Me
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid2)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid3)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Aqueous)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Solid)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Mixture, True)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Vapor)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid1)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid2)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid3)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Aqueous)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Solid)
        End Sub


        ''' <summary>
        ''' Sets the overall molar composition of the mixture.
        ''' </summary>
        ''' <param name="Vx"></param>
        ''' <remarks></remarks>
        Public Sub SetOverallComposition(ByVal Vx As Array) Implements Interfaces.IMaterialStream.SetOverallComposition

            Dim i As Integer = 0
            For Each c As Compound In Me.Phases(0).Compounds.Values
                c.MoleFraction = Vx(i)
                i += 1
            Next

        End Sub

        Public Sub EqualizeOverallComposition()

            For Each c As Compound In Me.Phases(0).Compounds.Values
                c.MoleFraction = 1 / Me.Phases(0).Compounds.Count
            Next

        End Sub

        Public Sub NormalizeOverallMoleComposition()

            Dim mt As Double = 0.0#
            For Each S In Phases(0).Compounds.Values
                mt += S.MoleFraction.GetValueOrDefault
            Next

            For Each S In Phases(0).Compounds.Values
                S.MoleFraction /= mt
            Next

        End Sub

        Public Sub NormalizeOverallMassComposition()

            Dim mt As Double = 0.0#
            For Each S In Phases(0).Compounds.Values
                mt += S.MassFraction.GetValueOrDefault
            Next

            For Each S In Phases(0).Compounds.Values
                S.MassFraction /= mt
            Next

        End Sub

        Public Sub CalcOverallCompMassFractions()

            Dim mol_x_mm As Double
            Dim sub1 As BaseClasses.Compound
            For Each sub1 In Phases(0).Compounds.Values
                mol_x_mm += sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight
            Next
            For Each sub1 In Phases(0).Compounds.Values
                If mol_x_mm > 0.0# Then
                    sub1.MassFraction = sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight / mol_x_mm
                Else
                    sub1.MassFraction = 0.0#
                End If
            Next

        End Sub

        Public Sub CalcOverallCompMoleFractions()

            Dim mol_x_mm As Double
            Dim sub1 As BaseClasses.Compound
            For Each sub1 In Phases(0).Compounds.Values
                mol_x_mm += sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight
            Next
            For Each sub1 In Phases(0).Compounds.Values
                sub1.MoleFraction = sub1.MassFraction.GetValueOrDefault / sub1.ConstantProperties.Molar_Weight / mol_x_mm
            Next

        End Sub

        Public Sub SetPhaseComposition1(ByVal Vx As Array, ByVal phase As Integer) Implements Interfaces.IMaterialStream.SetPhaseComposition
            SetPhaseComposition(Vx, phase)
        End Sub

        ''' <summary>
        ''' Sets the molar composition of a phase.
        ''' </summary>
        ''' <param name="Vx">Molar composition array</param>
        ''' <param name="phase">Phase to set composition of</param>
        ''' <remarks></remarks>
        Public Sub SetPhaseComposition(ByVal Vx As Array, ByVal phase As PropertyPackages.Phase)

            Dim i As Integer = 0, idx As Integer = 0
            Select Case phase
                Case PropertyPackages.Phase.Aqueous
                    idx = 2
                Case PropertyPackages.Phase.Liquid
                    idx = 1
                Case PropertyPackages.Phase.Liquid1
                    idx = 3
                Case PropertyPackages.Phase.Liquid2
                    idx = 4
                Case PropertyPackages.Phase.Liquid3
                    idx = 5
                Case PropertyPackages.Phase.Mixture
                    idx = 0
                Case PropertyPackages.Phase.Solid
                    idx = 7
                Case PropertyPackages.Phase.Vapor
                    idx = 2
            End Select
            For Each c As Compound In Me.Phases(idx).Compounds.Values
                c.MoleFraction = Vx(i)
                i += 1
            Next

        End Sub

        Public Function GetPhaseComposition(phs As Integer) As Double() Implements IMaterialStream.GetPhaseComposition

            Return Phases(phs).Compounds.Values.Select(Function(x) x.MoleFraction.GetValueOrDefault).ToArray

        End Function

        Public Function GetOverallComposition() As Double() Implements IMaterialStream.GetOverallComposition

            Return Phases(0).Compounds.Values.Select(Function(x) x.MoleFraction.GetValueOrDefault).ToArray

        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If val0 Is Nothing Then

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim value As Object = ""
                Dim sname As String = ""

                If prop.StartsWith("PROP_MS") Then

                    Dim propidx As Integer = Integer.Parse(prop.Split("/")(0).Split("_")(2))
                    If prop.Split("/").Length = 2 Then
                        sname = prop.Split("/")(1)
                    End If

                    Select Case propidx

                        Case 0
                            'PROP_MS_0 Temperature
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.Phases(0).Properties.temperature.GetValueOrDefault)
                        Case 1
                            'PROP_MS_1 Pressure
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Phases(0).Properties.pressure.GetValueOrDefault)
                        Case 2
                            'PROP_MS_2	Mass Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(0).Properties.massflow.GetValueOrDefault)
                        Case 3
                            'PROP_MS_3	Molar Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(0).Properties.molarflow.GetValueOrDefault)
                        Case 4
                            'PROP_MS_4	Volumetric Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault)
                        Case 5
                            'PROP_MS_5	Mixture Density
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(0).Properties.density.GetValueOrDefault)
                        Case 6
                            'PROP_MS_6	Mixture Molar Weight
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(0).Properties.molecularWeight.GetValueOrDefault)
                        Case 7
                            'PROP_MS_7	Mixture Specific Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(0).Properties.enthalpy.GetValueOrDefault)
                        Case 8
                            'PROP_MS_8	Mixture Specific Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(0).Properties.entropy.GetValueOrDefault)
                        Case 9
                            'PROP_MS_9	Mixture Molar Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(0).Properties.molar_enthalpy.GetValueOrDefault)
                        Case 10
                            'PROP_MS_10	Mixture Molar Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(0).Properties.molar_entropy.GetValueOrDefault)
                        Case 11
                            'PROP_MS_11	Mixture Thermal Conductivity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(0).Properties.thermalConductivity.GetValueOrDefault)
                        Case 12
                            'PROP_MS_12	Vapor Phase Density
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(2).Properties.density.GetValueOrDefault)
                        Case 13
                            'PROP_MS_13	Vapor Phase Molar Weight
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(2).Properties.molecularWeight.GetValueOrDefault)
                        Case 14
                            'PROP_MS_14	Vapor Phase Specific Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(2).Properties.enthalpy.GetValueOrDefault)
                        Case 15
                            'PROP_MS_15	Vapor Phase Specific Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(2).Properties.entropy.GetValueOrDefault)
                        Case 16
                            'PROP_MS_16	Vapor Phase Molar Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(2).Properties.molar_enthalpy.GetValueOrDefault)
                        Case 17
                            'PROP_MS_17	Vapor Phase Molar Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(2).Properties.molar_entropy.GetValueOrDefault)
                        Case 18
                            'PROP_MS_18	Vapor Phase Thermal Conductivity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(2).Properties.thermalConductivity.GetValueOrDefault)
                        Case 19
                            'PROP_MS_19	Vapor Phase Kinematic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(2).Properties.kinematic_viscosity.GetValueOrDefault)
                        Case 20
                            'PROP_MS_20	Vapor Phase Dynamic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(2).Properties.viscosity.GetValueOrDefault)
                        Case 21
                            'PROP_MS_21	Vapor Phase Heat Capacity (Cp)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(2).Properties.heatCapacityCp.GetValueOrDefault)
                        Case 22
                            'PROP_MS_22	Vapor Phase Heat Capacity Ratio (Cp/Cv)
                            value = Me.Phases(2).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(2).Properties.heatCapacityCv.GetValueOrDefault
                        Case 23
                            'PROP_MS_23	Vapor Phase Mass Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(2).Properties.massflow.GetValueOrDefault)
                        Case 24
                            'PROP_MS_24	Vapor Phase Molar Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(2).Properties.molarflow.GetValueOrDefault)
                        Case 25
                            'PROP_MS_25	Vapor Phase Volumetric Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(2).Properties.volumetric_flow.GetValueOrDefault)
                        Case 26
                            'PROP_MS_26	Vapor Phase Compressibility Factor
                            value = Me.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                        Case 27
                            'PROP_MS_27	Vapor Phase Molar Fraction
                            value = Me.Phases(2).Properties.molarfraction.GetValueOrDefault
                        Case 28
                            'PROP_MS_28	Vapor Phase Mass Fraction
                            value = Me.Phases(2).Properties.massfraction.GetValueOrDefault
                        Case 29
                            'PROP_MS_29	Vapor Phase Volumetric Fraction
                            value = Me.Phases(2).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                        Case 30
                            'PROP_MS_30	Liquid Phase (Mixture) Density
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(1).Properties.density.GetValueOrDefault)
                        Case 31
                            'PROP_MS_31	Liquid Phase (Mixture) Molar Weight
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(1).Properties.molecularWeight.GetValueOrDefault)
                        Case 32
                            'PROP_MS_32	Liquid Phase (Mixture) Specific Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(1).Properties.enthalpy.GetValueOrDefault)
                        Case 33
                            'PROP_MS_33	Liquid Phase (Mixture) Specific Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(1).Properties.entropy.GetValueOrDefault)
                        Case 34
                            'PROP_MS_34	Liquid Phase (Mixture) Molar Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(1).Properties.molar_enthalpy.GetValueOrDefault)
                        Case 35
                            'PROP_MS_35	Liquid Phase (Mixture) Molar Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(1).Properties.molar_entropy.GetValueOrDefault)
                        Case 36
                            'PROP_MS_36	Liquid Phase (Mixture) Thermal Conductivity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(1).Properties.thermalConductivity.GetValueOrDefault)
                        Case 37
                            'PROP_MS_37	Liquid Phase (Mixture) Kinematic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(1).Properties.kinematic_viscosity.GetValueOrDefault)
                        Case 38
                            'PROP_MS_38	Liquid Phase (Mixture) Dynamic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(1).Properties.viscosity.GetValueOrDefault)
                        Case 39
                            'PROP_MS_39	Liquid Phase (Mixture) Heat Capacity (Cp)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(1).Properties.heatCapacityCp.GetValueOrDefault)
                        Case 40
                            'PROP_MS_40	Liquid Phase (Mixture) Heat Capacity Ratio (Cp/Cv)
                            value = Me.Phases(1).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(1).Properties.heatCapacityCv.GetValueOrDefault
                        Case 41
                            'PROP_MS_41	Liquid Phase (Mixture) Mass Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(1).Properties.massflow.GetValueOrDefault)
                        Case 42
                            'PROP_MS_42	Liquid Phase (Mixture) Molar Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(1).Properties.molarflow.GetValueOrDefault)
                        Case 43
                            'PROP_MS_43	Liquid Phase (Mixture) Volumetric Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(1).Properties.volumetric_flow.GetValueOrDefault)
                        Case 44
                            'PROP_MS_44	Liquid Phase (Mixture) Compressibility Factor
                            value = Me.Phases(1).Properties.compressibilityFactor.GetValueOrDefault
                        Case 45
                            'PROP_MS_45	Liquid Phase (Mixture) Molar Fraction
                            value = Me.Phases(1).Properties.molarfraction.GetValueOrDefault
                        Case 46
                            'PROP_MS_46	Liquid Phase (Mixture) Mass Fraction
                            value = Me.Phases(1).Properties.massfraction.GetValueOrDefault
                        Case 47
                            'PROP_MS_47	Liquid Phase (Mixture) Volumetric Fraction
                            value = Me.Phases(1).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                        Case 48
                            'PROP_MS_48	Liquid Phase (1) Density
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(3).Properties.density.GetValueOrDefault)
                        Case 49
                            'PROP_MS_49	Liquid Phase (1) Molar Weight
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(3).Properties.molecularWeight.GetValueOrDefault)
                        Case 50
                            'PROP_MS_50	Liquid Phase (1) Specific Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(3).Properties.enthalpy.GetValueOrDefault)
                        Case 51
                            'PROP_MS_51	Liquid Phase (1) Specific Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(3).Properties.entropy.GetValueOrDefault)
                        Case 52
                            'PROP_MS_52	Liquid Phase (1) Molar Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(3).Properties.molar_enthalpy.GetValueOrDefault)
                        Case 53
                            'PROP_MS_53	Liquid Phase (1) Molar Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(3).Properties.molar_entropy.GetValueOrDefault)
                        Case 54
                            'PROP_MS_54	Liquid Phase (1) Thermal Conductivity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(3).Properties.thermalConductivity.GetValueOrDefault)
                        Case 55
                            'PROP_MS_55	Liquid Phase (1) Kinematic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(3).Properties.kinematic_viscosity.GetValueOrDefault)
                        Case 56
                            'PROP_MS_56	Liquid Phase (1) Dynamic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(3).Properties.viscosity.GetValueOrDefault)
                        Case 57
                            'PROP_MS_57	Liquid Phase (1) Heat Capacity (Cp)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(3).Properties.heatCapacityCp.GetValueOrDefault)
                        Case 58
                            'PROP_MS_58	Liquid Phase (1) Heat Capacity Ratio (Cp/Cv)
                            value = Me.Phases(3).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(3).Properties.heatCapacityCv.GetValueOrDefault
                        Case 59
                            'PROP_MS_59	Liquid Phase (1) Mass Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(3).Properties.massflow.GetValueOrDefault)
                        Case 60
                            'PROP_MS_60	Liquid Phase (1) Molar Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(3).Properties.molarflow.GetValueOrDefault)
                        Case 61
                            'PROP_MS_61	Liquid Phase (1) Volumetric Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(3).Properties.volumetric_flow.GetValueOrDefault)
                        Case 62
                            'PROP_MS_62	Liquid Phase (1) Compressibility Factor
                            value = Me.Phases(3).Properties.compressibilityFactor.GetValueOrDefault
                        Case 63
                            'PROP_MS_63	Liquid Phase (1) Molar Fraction
                            value = Me.Phases(3).Properties.molarfraction.GetValueOrDefault
                        Case 64
                            'PROP_MS_64	Liquid Phase (1) Mass Fraction
                            value = Me.Phases(3).Properties.massfraction.GetValueOrDefault
                        Case 65
                            'PROP_MS_65	Liquid Phase (1) Volumetric Fraction
                            value = Me.Phases(3).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                        Case 66
                            'PROP_MS_66	Liquid Phase (2) Density
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(4).Properties.density.GetValueOrDefault)
                        Case 67
                            'PROP_MS_67	Liquid Phase (2) Molar Weight
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(4).Properties.molecularWeight.GetValueOrDefault)
                        Case 68
                            'PROP_MS_68	Liquid Phase (2) Specific Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(4).Properties.enthalpy.GetValueOrDefault)
                        Case 69
                            'PROP_MS_69	Liquid Phase (2) Specific Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(4).Properties.entropy.GetValueOrDefault)
                        Case 70
                            'PROP_MS_70	Liquid Phase (2) Molar Enthalpy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(4).Properties.molar_enthalpy.GetValueOrDefault)
                        Case 71
                            'PROP_MS_71	Liquid Phase (2) Molar Entropy
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(4).Properties.molar_entropy.GetValueOrDefault)
                        Case 72
                            'PROP_MS_72	Liquid Phase (2) Thermal Conductivity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(4).Properties.thermalConductivity.GetValueOrDefault)
                        Case 73
                            'PROP_MS_73	Liquid Phase (2) Kinematic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(4).Properties.kinematic_viscosity.GetValueOrDefault)
                        Case 74
                            'PROP_MS_74	Liquid Phase (2) Dynamic Viscosity
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(4).Properties.viscosity.GetValueOrDefault)
                        Case 75
                            'PROP_MS_75	Liquid Phase (2) Heat Capacity (Cp)
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(4).Properties.heatCapacityCp.GetValueOrDefault)
                        Case 76
                            'PROP_MS_76	Liquid Phase (2) Heat Capacity Ratio (Cp/Cv)
                            value = Me.Phases(4).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(4).Properties.heatCapacityCv.GetValueOrDefault
                        Case 77
                            'PROP_MS_77	Liquid Phase (2) Mass Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(4).Properties.massflow.GetValueOrDefault)
                        Case 78
                            'PROP_MS_78	Liquid Phase (2) Molar Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(4).Properties.molarflow.GetValueOrDefault)
                        Case 79
                            'PROP_MS_79	Liquid Phase (2) Volumetric Flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(4).Properties.volumetric_flow.GetValueOrDefault)
                        Case 80
                            'PROP_MS_80	Liquid Phase (2) Compressibility Factor
                            value = Me.Phases(4).Properties.compressibilityFactor.GetValueOrDefault
                        Case 81
                            'PROP_MS_81	Liquid Phase (2) Molar Fraction
                            value = Me.Phases(4).Properties.molarfraction.GetValueOrDefault
                        Case 82
                            'PROP_MS_82	Liquid Phase (2) Mass Fraction
                            value = Me.Phases(4).Properties.massfraction.GetValueOrDefault
                        Case 83
                            'PROP_MS_83	Liquid Phase (2) Volumetric Fraction
                            value = Me.Phases(4).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                        Case 84
                            'PROP_MS_84	Aqueous Phase Density
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(6).Properties.density.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 85
                            'PROP_MS_85	Aqueous Phase Molar Weight
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(6).Properties.molecularWeight.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 86
                            'PROP_MS_86	Aqueous Phase Specific Enthalpy
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(6).Properties.enthalpy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 87
                            'PROP_MS_87	Aqueous Phase Specific Entropy
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(6).Properties.entropy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 88
                            'PROP_MS_88	Aqueous Phase Molar Enthalpy
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(6).Properties.molar_enthalpy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 89
                            'PROP_MS_89	Aqueous Phase Molar Entropy
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(6).Properties.molar_entropy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 90
                            'PROP_MS_90	Aqueous Phase Thermal Conductivity
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(6).Properties.thermalConductivity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 91
                            'PROP_MS_91	Aqueous Phase Kinematic Viscosity
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(6).Properties.kinematic_viscosity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 92
                            'PROP_MS_92	Aqueous Phase Dynamic Viscosity
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(6).Properties.viscosity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 93
                            'PROP_MS_93	Aqueous Phase Heat Capacity (Cp)
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(6).Properties.heatCapacityCp.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 94
                            'PROP_MS_94	Aqueous Phase Heat Capacity Ratio (Cp/Cv)
                            If Me.Phases.ContainsKey(6) Then
                                value = Me.Phases(6).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(6).Properties.heatCapacityCv.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 95
                            'PROP_MS_95	Aqueous Phase Mass Flow
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(6).Properties.massflow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 96
                            'PROP_MS_96	Aqueous Phase Molar Flow
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(6).Properties.molarflow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 97
                            'PROP_MS_97	Aqueous Phase Volumetric Flow
                            If Me.Phases.ContainsKey(6) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(6).Properties.volumetric_flow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 98
                            'PROP_MS_98	Aqueous Phase Compressibility Factor
                            If Me.Phases.ContainsKey(6) Then
                                value = Me.Phases(6).Properties.compressibilityFactor.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 99
                            'PROP_MS_99	Aqueous Phase Molar Fraction
                            If Me.Phases.ContainsKey(6) Then
                                value = Me.Phases(6).Properties.molarfraction.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 100
                            'PROP_MS_100	Aqueous Phase Mass Fraction
                            If Me.Phases.ContainsKey(6) Then
                                value = Me.Phases(6).Properties.massfraction.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 101
                            'PROP_MS_101	Aqueous Phase Volumetric Fraction
                            If Me.Phases.ContainsKey(6) Then
                                value = Me.Phases(6).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 131
                            'PROP_MS_131	Solid Phase Density
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.density, Me.Phases(7).Properties.density.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 132
                            'PROP_MS_132	Solid Phase Molar Weight
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molecularWeight, Me.Phases(7).Properties.molecularWeight.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 133
                            'PROP_MS_133	Solid Phase Specific Enthalpy
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.enthalpy, Me.Phases(7).Properties.enthalpy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 134
                            'PROP_MS_134	Solid Phase Specific Entropy
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.entropy, Me.Phases(7).Properties.entropy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 135
                            'PROP_MS_135	Solid Phase Molar Enthalpy
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_enthalpy, Me.Phases(7).Properties.molar_enthalpy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 136
                            'PROP_MS_136	Solid Phase Molar Entropy
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molar_entropy, Me.Phases(7).Properties.molar_entropy.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 137
                            'PROP_MS_137	Solid Phase Thermal Conductivity
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.thermalConductivity, Me.Phases(7).Properties.thermalConductivity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 138
                            'PROP_MS_138	Solid Phase Kinematic Viscosity
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.cinematic_viscosity, Me.Phases(7).Properties.kinematic_viscosity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 139
                            'PROP_MS_139	Solid Phase Dynamic Viscosity
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.viscosity, Me.Phases(7).Properties.viscosity.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 140
                            'PROP_MS_140	Solid Phase Heat Capacity (Cp)
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.heatCapacityCp, Me.Phases(7).Properties.heatCapacityCp.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 141
                            'PROP_MS_141	Solid Phase Heat Capacity Ratio (Cp/Cv)
                            If Me.Phases.ContainsKey(7) Then
                                value = Me.Phases(7).Properties.heatCapacityCp.GetValueOrDefault / Me.Phases(7).Properties.heatCapacityCv.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 142
                            'PROP_MS_142	Solid Phase Mass Flow
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(7).Properties.massflow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 143
                            'PROP_MS_143	Solid Phase Molar Flow
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(7).Properties.molarflow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 144
                            'PROP_MS_144	Solid Phase Volumetric Flow
                            If Me.Phases.ContainsKey(7) Then
                                value = SystemsOfUnits.Converter.ConvertFromSI(su.volumetricFlow, Me.Phases(7).Properties.volumetric_flow.GetValueOrDefault)
                            Else
                                value = 0
                            End If
                        Case 145
                            'PROP_MS_145	Solid Phase Compressibility Factor
                            If Me.Phases.ContainsKey(7) Then
                                value = Me.Phases(7).Properties.compressibilityFactor.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 146
                            'PROP_MS_146	Solid Phase Molar Fraction
                            If Me.Phases.ContainsKey(7) Then
                                value = Me.Phases(7).Properties.molarfraction.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 147
                            'PROP_MS_147	Solid Phase Mass Fraction
                            If Me.Phases.ContainsKey(7) Then
                                value = Me.Phases(7).Properties.massfraction.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 148
                            'PROP_MS_148	Solid Phase Volumetric Fraction
                            If Me.Phases.ContainsKey(7) Then
                                value = Me.Phases(7).Properties.volumetric_flow.GetValueOrDefault / Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault
                            Else
                                value = 0
                            End If
                        Case 103, 111, 112, 113, 114, 115, 150
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 103 Then
                                    value = Me.Phases(0).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 111 Then
                                    value = Me.Phases(2).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 112 Then
                                    value = Me.Phases(1).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 113 Then
                                    value = Me.Phases(3).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 114 Then
                                    value = Me.Phases(4).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 115 Then
                                    value = Me.Phases(5).Compounds(sname).MassFraction.GetValueOrDefault
                                ElseIf propidx = 150 Then
                                    value = Me.Phases(7).Compounds(sname).MassFraction.GetValueOrDefault
                                End If
                            Else
                                value = Double.MinValue
                            End If
                        Case 102, 106, 107, 108, 109, 110, 149
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 102 Then
                                    value = Me.Phases(0).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 106 Then
                                    value = Me.Phases(2).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 107 Then
                                    value = Me.Phases(1).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 108 Then
                                    value = Me.Phases(3).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 109 Then
                                    value = Me.Phases(4).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 110 Then
                                    value = Me.Phases(5).Compounds(sname).MoleFraction.GetValueOrDefault
                                ElseIf propidx = 149 Then
                                    value = Me.Phases(7).Compounds(sname).MoleFraction.GetValueOrDefault
                                End If
                            Else
                                value = Double.MinValue
                            End If
                        Case 104, 116, 117, 118, 119, 151
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 104 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(0).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 116 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(2).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 117 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(1).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 118 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(3).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 119 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(4).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 120 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(5).Compounds(sname).MolarFlow.GetValueOrDefault)
                                ElseIf propidx = 151 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Me.Phases(7).Compounds(sname).MolarFlow.GetValueOrDefault)
                                End If
                            Else
                                value = Double.MinValue
                            End If
                        Case 105, 121, 122, 123, 124, 125, 152
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 105 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(0).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 121 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(2).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 122 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(1).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 123 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(3).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 124 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(4).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 125 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(5).Compounds(sname).MassFlow.GetValueOrDefault)
                                ElseIf propidx = 152 Then
                                    value = SystemsOfUnits.Converter.ConvertFromSI(su.massflow, Me.Phases(7).Compounds(sname).MassFlow.GetValueOrDefault)
                                End If
                            Else
                                value = Double.MinValue
                            End If
                        Case 126
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Phases(0).Properties.bubblePressure.GetValueOrDefault)
                        Case 127
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Phases(0).Properties.dewPressure.GetValueOrDefault)
                        Case 128
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.Phases(0).Properties.bubbleTemperature.GetValueOrDefault)
                        Case 129
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.Phases(0).Properties.dewTemperature.GetValueOrDefault)
                        Case 130
                            If Me.Phases(1).Properties.molarfraction.GetValueOrDefault = 1.0# Then
                                value = "Liquid Only"
                            ElseIf Me.Phases(2).Properties.molarfraction.GetValueOrDefault = 1.0# Then
                                value = "Vapor Only"
                            Else
                                value = "Mixed"
                            End If
                        Case 153
                            value = Me.Phases(3).Properties.pH.GetValueOrDefault
                        Case 154
                            'total energy flow
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Phases(0).Properties.enthalpy.GetValueOrDefault * Phases(0).Properties.massflow.GetValueOrDefault)
                        Case 155
                            'PROP_MS_155	Isothermal Compressibility (Vapor)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(2).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 156
                            'PROP_MS_156	Bulk Modulus (Vapor)	
                            value = cv.ConvertFromSI(su.pressure, Phases(2).Properties.bulk_modulus.GetValueOrDefault)
                        Case 157
                            'PROP_MS_157	Speed of Sound (Vapor)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(2).Properties.speedOfSound.GetValueOrDefault)
                        Case 158
                            'PROP_MS_158	Joule-Thomson Coefficient (Vapor)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(2).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 159
                            'PROP_MS_159	Internal Energy (Vapor)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(2).Properties.internal_energy.GetValueOrDefault)
                        Case 160
                            'PROP_MS_160	Gibbs Free Energy (Vapor)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(2).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 161
                            'PROP_MS_161	Helmholtz Free Energy (Vapor)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(2).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 162
                            'PROP_MS_162	Isothermal Compressibility (Overall Liquid)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(1).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 163
                            'PROP_MS_163	Bulk Modulus (Overall Liquid)	
                            value = cv.ConvertFromSI(su.pressure, Phases(1).Properties.bulk_modulus.GetValueOrDefault)
                        Case 164
                            'PROP_MS_164	Speed of Sound (Overall Liquid)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(1).Properties.speedOfSound.GetValueOrDefault)
                        Case 165
                            'PROP_MS_165	Joule-Thomson Coefficient (Overall Liquid)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(1).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 166
                            'PROP_MS_166	Internal Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(1).Properties.internal_energy.GetValueOrDefault)
                        Case 167
                            'PROP_MS_167	Gibbs Free Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(1).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 168
                            'PROP_MS_168	Helmholtz Free Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(1).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 169
                            'PROP_MS_169	Bulk Modulus (Liquid 1)	
                            value = cv.ConvertFromSI(su.pressure, Phases(3).Properties.bulk_modulus.GetValueOrDefault)
                        Case 170
                            'PROP_MS_170	Speed of Sound (Liquid 1)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(3).Properties.speedOfSound.GetValueOrDefault)
                        Case 171
                            'PROP_MS_171	Joule-Thomson Coefficient (Liquid 1)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(3).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 172
                            'PROP_MS_172	Internal Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(3).Properties.internal_energy.GetValueOrDefault)
                        Case 173
                            'PROP_MS_173	Gibbs Free Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(3).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 174
                            'PROP_MS_174	Helmholtz Free Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(3).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 175
                            'PROP_MS_175	Isothermal Compressibility (Liquid 1)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(3).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 176
                            'PROP_MS_176	Bulk Modulus (Liquid 2)	
                            value = cv.ConvertFromSI(su.pressure, Phases(4).Properties.bulk_modulus.GetValueOrDefault)
                        Case 177
                            'PROP_MS_177	Speed of Sound (Liquid 2)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(4).Properties.speedOfSound.GetValueOrDefault)
                        Case 178
                            'PROP_MS_178	Joule-Thomson Coefficient (Liquid 2)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(4).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 179
                            'PROP_MS_179	Internal Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(4).Properties.internal_energy.GetValueOrDefault)
                        Case 180
                            'PROP_MS_180	Gibbs Free Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(4).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 181
                            'PROP_MS_181	Helmholtz Free Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(4).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 182
                            'PROP_MS_182	Isothermal Compressibility (Liquid 2)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(4).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 183
                            'PROP_MS_183	Bulk Modulus (Liquid 3)	
                            value = cv.ConvertFromSI(su.pressure, Phases(5).Properties.bulk_modulus.GetValueOrDefault)
                        Case 184
                            'PROP_MS_184	Speed of Sound (Liquid 3)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(5).Properties.speedOfSound.GetValueOrDefault)
                        Case 185
                            'PROP_MS_185	Joule-Thomson Coefficient (Liquid 3)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(5).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 186
                            'PROP_MS_186	Internal Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(5).Properties.internal_energy.GetValueOrDefault)
                        Case 187
                            'PROP_MS_187	Gibbs Free Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(5).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 188
                            'PROP_MS_188	Helmholtz Free Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(5).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 189
                            'PROP_MS_189	Isothermal Compressibility (Liquid 3)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(5).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 190
                            'PROP_MS_190	Bulk Modulus (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.pressure, Phases(6).Properties.bulk_modulus.GetValueOrDefault)
                        Case 191
                            'PROP_MS_191	Speed of Sound (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(6).Properties.speedOfSound.GetValueOrDefault)
                        Case 192
                            'PROP_MS_192	Joule-Thomson Coefficient (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(6).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 193
                            'PROP_MS_193	Internal Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(6).Properties.internal_energy.GetValueOrDefault)
                        Case 194
                            'PROP_MS_194	Gibbs Free Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(6).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 195
                            'PROP_MS_195	Helmholtz Free Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(6).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 196
                            'PROP_MS_196	Isothermal Compressibility (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(6).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 198
                            'PROP_MS_198	Bulk Modulus (Solid)	
                            value = cv.ConvertFromSI(su.pressure, Phases(7).Properties.bulk_modulus.GetValueOrDefault)
                        Case 199
                            'PROP_MS_199	Speed of Sound (Solid)	
                            value = cv.ConvertFromSI(su.speedOfSound, Phases(7).Properties.speedOfSound.GetValueOrDefault)
                        Case 200
                            'PROP_MS_200	Joule-Thomson Coefficient (Solid)	
                            value = cv.ConvertFromSI(su.jouleThomsonCoefficient, Phases(7).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                        Case 201
                            'PROP_MS_201	Internal Energy (Solid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(7).Properties.internal_energy.GetValueOrDefault)
                        Case 202
                            'PROP_MS_202	Gibbs Free Energy (Solid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(7).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 203
                            'PROP_MS_203	Helmholtz Free Energy (Solid)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(7).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 204
                            'PROP_MS_204	Isothermal Compressibility (Solid)	
                            value = cv.ConvertFromSI(su.compressibility, Phases(7).Properties.isothermal_compressibility.GetValueOrDefault)
                        Case 205
                            'PROP_MS_205	Internal Energy (Mixture)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(0).Properties.internal_energy.GetValueOrDefault)
                        Case 206
                            'PROP_MS_206	Gibbs Free Energy (Mixture)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(0).Properties.gibbs_free_energy.GetValueOrDefault)
                        Case 207
                            'PROP_MS_207	Helmholtz Free Energy (Mixture)	
                            value = cv.ConvertFromSI(su.enthalpy, Phases(0).Properties.helmholtz_energy.GetValueOrDefault)
                        Case 208
                            'PROP_MS_208	Molar Internal Energy (Mixture)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(0).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 209
                            'PROP_MS_209	Molar Gibbs Free Energy (Mixture)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(0).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 210
                            'PROP_MS_210	Molar Helmholtz Free Energy (Mixture)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(0).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 211
                            'PROP_MS_211	Molar Internal Energy (Vapor)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(2).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 212
                            'PROP_MS_212	Molar Gibbs Free Energy (Vapor)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(2).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 213
                            'PROP_MS_213	Molar Helmholtz Free Energy (Vapor)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(2).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 214
                            'PROP_MS_214	Molar Internal Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(1).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 215
                            'PROP_MS_215	Molar Gibbs Free Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(1).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 216
                            'PROP_MS_216	Molar Helmholtz Free Energy (Overall Liquid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(1).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 217
                            'PROP_MS_217	Molar Internal Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(3).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 218
                            'PROP_MS_218	Molar Gibbs Free Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(3).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 219
                            'PROP_MS_219	Molar Helmholtz Free Energy (Liquid 1)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(3).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 220
                            'PROP_MS_220	Molar Internal Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(4).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 221
                            'PROP_MS_221	Molar Gibbs Free Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(4).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 222
                            'PROP_MS_222	Molar Helmholtz Free Energy (Liquid 2)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(4).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 223
                            'PROP_MS_223	Molar Internal Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(5).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 224
                            'PROP_MS_224	Molar Gibbs Free Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(5).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 225
                            'PROP_MS_225	Molar Helmholtz Free Energy (Liquid 3)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(5).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 226
                            'PROP_MS_226	Molar Internal Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(6).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 227
                            'PROP_MS_227	Molar Gibbs Free Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(6).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 228
                            'PROP_MS_228	Molar Helmholtz Free Energy (Aqueous Phase)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(6).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 229
                            'PROP_MS_229	Molar Internal Energy (Solid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(7).Properties.molar_internal_energy.GetValueOrDefault)
                        Case 230
                            'PROP_MS_230	Molar Gibbs Free Energy (Solid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(7).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Case 231
                            'PROP_MS_231	Molar Helmholtz Free Energy (Solid)	
                            value = cv.ConvertFromSI(su.molar_enthalpy, Phases(7).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Case 232, 233, 234, 235, 236, 237, 238
                            'Molalities
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 232 Then
                                    value = Me.Phases(0).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 233 Then
                                    value = Me.Phases(2).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 234 Then
                                    value = Me.Phases(1).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 235 Then
                                    value = Me.Phases(3).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 236 Then
                                    value = Me.Phases(4).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 237 Then
                                    value = Me.Phases(5).Compounds(sname).Molality.GetValueOrDefault
                                ElseIf propidx = 238 Then
                                    value = Me.Phases(7).Compounds(sname).Molality.GetValueOrDefault
                                End If
                            Else
                                value = Double.MinValue
                            End If
                        Case 239, 240, 241, 242, 243, 244, 245
                            'Molarities
                            If Me.Phases(0).Compounds.ContainsKey(sname) Then
                                If propidx = 239 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(0).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 240 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(2).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 241 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(1).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 242 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(3).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 243 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(4).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 244 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(5).Compounds(sname).Molarity.GetValueOrDefault)
                                ElseIf propidx = 245 Then
                                    value = cv.ConvertFromSI(su.molar_conc, Me.Phases(7).Compounds(sname).Molarity.GetValueOrDefault)
                                End If
                            Else
                                value = Double.MinValue
                            End If
                    End Select

                    Return value

                Else

                    Return ""

                End If

            Else

                Return val0

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()

            Dim i As Integer = 0
            Dim proplist As New List(Of String)

            Dim basecol = MyBase.GetProperties(proptype)

            If basecol.Length > 0 Then proplist.AddRange(basecol)

            Select Case proptype
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For i = 102 To 105
                        For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                            proplist.Add("PROP_MS_" + CStr(i) + "/" + subst.Name)
                        Next
                    Next
                Case PropertyType.RO
                    For i = 5 To 101
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For i = 131 To 148
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                        proplist.Add("PROP_MS_102" + "/" + subst.Name)
                        proplist.Add("PROP_MS_103" + "/" + subst.Name)
                        proplist.Add("PROP_MS_104" + "/" + subst.Name)
                        proplist.Add("PROP_MS_105" + "/" + subst.Name)
                        proplist.Add("PROP_MS_106" + "/" + subst.Name)
                        proplist.Add("PROP_MS_107" + "/" + subst.Name)
                        proplist.Add("PROP_MS_108" + "/" + subst.Name)
                        proplist.Add("PROP_MS_109" + "/" + subst.Name)
                        proplist.Add("PROP_MS_110" + "/" + subst.Name)
                        proplist.Add("PROP_MS_111" + "/" + subst.Name)
                        proplist.Add("PROP_MS_112" + "/" + subst.Name)
                        proplist.Add("PROP_MS_113" + "/" + subst.Name)
                        proplist.Add("PROP_MS_114" + "/" + subst.Name)
                        proplist.Add("PROP_MS_115" + "/" + subst.Name)
                        proplist.Add("PROP_MS_116" + "/" + subst.Name)
                        proplist.Add("PROP_MS_117" + "/" + subst.Name)
                        proplist.Add("PROP_MS_118" + "/" + subst.Name)
                        proplist.Add("PROP_MS_119" + "/" + subst.Name)
                        proplist.Add("PROP_MS_120" + "/" + subst.Name)
                        proplist.Add("PROP_MS_121" + "/" + subst.Name)
                        proplist.Add("PROP_MS_122" + "/" + subst.Name)
                        proplist.Add("PROP_MS_123" + "/" + subst.Name)
                        proplist.Add("PROP_MS_124" + "/" + subst.Name)
                        proplist.Add("PROP_MS_125" + "/" + subst.Name)
                        proplist.Add("PROP_MS_149" + "/" + subst.Name)
                        proplist.Add("PROP_MS_150" + "/" + subst.Name)
                        proplist.Add("PROP_MS_151" + "/" + subst.Name)
                        proplist.Add("PROP_MS_152" + "/" + subst.Name)
                    Next
                    For i = 126 To 130
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    proplist.Add("PROP_MS_153")
                    proplist.Add("PROP_MS_154")
                    For i = 155 To 231
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                        proplist.Add("PROP_MS_232" + "/" + subst.Name)
                        proplist.Add("PROP_MS_233" + "/" + subst.Name)
                        proplist.Add("PROP_MS_234" + "/" + subst.Name)
                        proplist.Add("PROP_MS_235" + "/" + subst.Name)
                        proplist.Add("PROP_MS_236" + "/" + subst.Name)
                        proplist.Add("PROP_MS_238" + "/" + subst.Name)
                        proplist.Add("PROP_MS_239" + "/" + subst.Name)
                        proplist.Add("PROP_MS_240" + "/" + subst.Name)
                        proplist.Add("PROP_MS_241" + "/" + subst.Name)
                        proplist.Add("PROP_MS_242" + "/" + subst.Name)
                        proplist.Add("PROP_MS_243" + "/" + subst.Name)
                        proplist.Add("PROP_MS_244" + "/" + subst.Name)
                        proplist.Add("PROP_MS_245" + "/" + subst.Name)
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    proplist.Add("PROP_MS_7")
                    proplist.Add("PROP_MS_8")
                    proplist.Add("PROP_MS_27")
                    For i = 102 To 105
                        For Each subst As Compound In Me.Phases(0).Compounds.Values
                            proplist.Add("PROP_MS_" + CStr(i) + "/" + subst.Name)
                        Next
                    Next
                Case PropertyType.ALL
                    For i = 0 To 101
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For i = 131 To 148
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For Each subst As ConstantProperties In Me.FlowSheet.SelectedCompounds.Values
                        proplist.Add("PROP_MS_102" + "/" + subst.Name)
                        proplist.Add("PROP_MS_103" + "/" + subst.Name)
                        proplist.Add("PROP_MS_104" + "/" + subst.Name)
                        proplist.Add("PROP_MS_105" + "/" + subst.Name)
                        proplist.Add("PROP_MS_106" + "/" + subst.Name)
                        proplist.Add("PROP_MS_107" + "/" + subst.Name)
                        proplist.Add("PROP_MS_108" + "/" + subst.Name)
                        proplist.Add("PROP_MS_109" + "/" + subst.Name)
                        proplist.Add("PROP_MS_110" + "/" + subst.Name)
                        proplist.Add("PROP_MS_111" + "/" + subst.Name)
                        proplist.Add("PROP_MS_112" + "/" + subst.Name)
                        proplist.Add("PROP_MS_113" + "/" + subst.Name)
                        proplist.Add("PROP_MS_114" + "/" + subst.Name)
                        proplist.Add("PROP_MS_115" + "/" + subst.Name)
                        proplist.Add("PROP_MS_116" + "/" + subst.Name)
                        proplist.Add("PROP_MS_117" + "/" + subst.Name)
                        proplist.Add("PROP_MS_118" + "/" + subst.Name)
                        proplist.Add("PROP_MS_119" + "/" + subst.Name)
                        proplist.Add("PROP_MS_120" + "/" + subst.Name)
                        proplist.Add("PROP_MS_121" + "/" + subst.Name)
                        proplist.Add("PROP_MS_122" + "/" + subst.Name)
                        proplist.Add("PROP_MS_123" + "/" + subst.Name)
                        proplist.Add("PROP_MS_124" + "/" + subst.Name)
                        proplist.Add("PROP_MS_125" + "/" + subst.Name)
                        proplist.Add("PROP_MS_149" + "/" + subst.Name)
                        proplist.Add("PROP_MS_150" + "/" + subst.Name)
                        proplist.Add("PROP_MS_151" + "/" + subst.Name)
                        proplist.Add("PROP_MS_152" + "/" + subst.Name)
                    Next
                    For i = 126 To 130
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    proplist.Add("PROP_MS_153")
                    proplist.Add("PROP_MS_154")
                    For i = 155 To 231
                        proplist.Add("PROP_MS_" + CStr(i))
                    Next
                    For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                        proplist.Add("PROP_MS_232" + "/" + subst.Name)
                        proplist.Add("PROP_MS_233" + "/" + subst.Name)
                        proplist.Add("PROP_MS_234" + "/" + subst.Name)
                        proplist.Add("PROP_MS_235" + "/" + subst.Name)
                        proplist.Add("PROP_MS_236" + "/" + subst.Name)
                        proplist.Add("PROP_MS_238" + "/" + subst.Name)
                        proplist.Add("PROP_MS_239" + "/" + subst.Name)
                        proplist.Add("PROP_MS_240" + "/" + subst.Name)
                        proplist.Add("PROP_MS_241" + "/" + subst.Name)
                        proplist.Add("PROP_MS_242" + "/" + subst.Name)
                        proplist.Add("PROP_MS_243" + "/" + subst.Name)
                        proplist.Add("PROP_MS_244" + "/" + subst.Name)
                        proplist.Add("PROP_MS_245" + "/" + subst.Name)
                    Next
            End Select

            proplist.AddRange(MyBase.GetProperties(proptype))

            Return proplist.ToArray

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If Not prop.StartsWith("PROP_MS") Then

                Return MyBase.SetPropertyValue(prop, propval)

            Else

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim propidx As Integer = Integer.Parse(prop.Split("/")(0).Split("_")(2))
                Dim sname As String = ""
                If prop.Split("/").Length = 2 Then
                    sname = prop.Split("/")(1)
                End If

                Dim prevstream = Me.PropertyPackage.CurrentMaterialStream

                Me.PropertyPackage.CurrentMaterialStream = Me

                Select Case propidx
                    Case 0
                        'PROP_MS_0 Temperature
                        Me.Phases(0).Properties.temperature = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)
                    Case 1
                        'PROP_MS_1 Pressure
                        Me.Phases(0).Properties.pressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                    Case 2
                        'PROP_MS_2	Mass Flow
                        Me.Phases(0).Properties.massflow = SystemsOfUnits.Converter.ConvertToSI(su.massflow, propval)
                        Me.PropertyPackage.DW_CalcVazaoMolar()
                        Me.PropertyPackage.DW_CalcVazaoVolumetrica()
                    Case 3
                        'PROP_MS_3	Molar Flow
                        Me.Phases(0).Properties.molarflow = SystemsOfUnits.Converter.ConvertToSI(su.molarflow, propval)
                        Me.PropertyPackage.DW_CalcVazaoMassica()
                        Me.PropertyPackage.DW_CalcVazaoVolumetrica()
                    Case 4
                        'PROP_MS_4	Volumetric Flow
                        Me.Phases(0).Properties.volumetric_flow = SystemsOfUnits.Converter.ConvertToSI(su.volumetricFlow, propval)
                        Me.Phases(0).Properties.massflow = Me.Phases(0).Properties.volumetric_flow * Me.Phases(0).Properties.density.GetValueOrDefault
                        Me.PropertyPackage.DW_CalcVazaoMolar()
                    Case 7
                        'PROP_MS_7	Specific Enthalpy
                        Me.Phases(0).Properties.enthalpy = SystemsOfUnits.Converter.ConvertToSI(su.enthalpy, propval)
                    Case 8
                        'PROP_MS_8	Specific Entropy
                        Me.Phases(0).Properties.entropy = SystemsOfUnits.Converter.ConvertToSI(su.entropy, propval)
                    Case 27
                        'PROP_MS_27	Molar fraction vapour phase
                        Me.Phases(2).Properties.molarfraction = propval
                    Case 102
                        If Me.Phases(0).Compounds.ContainsKey(sname) Then
                            Me.Phases(0).Compounds(sname).MoleFraction = Convert.ToDouble(propval)
                            Dim mtotal As Double = 0
                            Me.PropertyPackage.DW_CalcCompMolarFlow(0)
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                            Next
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MassFraction = comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight / mtotal
                            Next
                            Me.PropertyPackage.DW_CalcCompMassFlow(0)
                        End If
                    Case 103
                        If Me.Phases(0).Compounds.ContainsKey(sname) Then
                            Me.Phases(0).Compounds(sname).MassFraction = Convert.ToDouble(propval)
                            Dim mtotal As Double = 0
                            Me.PropertyPackage.DW_CalcCompMassFlow(0)
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                mtotal += comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight
                            Next
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MoleFraction = comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / mtotal
                            Next
                            Me.PropertyPackage.DW_CalcCompMolarFlow(0)
                        End If
                    Case 104
                        If Me.Phases(0).Compounds.ContainsKey(sname) Then
                            Me.Phases(0).Compounds(sname).MolarFlow = SystemsOfUnits.Converter.ConvertToSI(su.molarflow, propval)
                            Me.Phases(0).Compounds(sname).MassFlow = SystemsOfUnits.Converter.ConvertToSI(su.molarflow, propval) / 1000 * Me.Phases(0).Compounds(sname).ConstantProperties.Molar_Weight
                            Dim summ As Double = 0
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                summ += comp.MolarFlow
                            Next
                            Me.Phases(0).Properties.molarflow = summ
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MoleFraction = comp.MolarFlow / summ
                            Next
                            Dim mtotal As Double = 0
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                mtotal += comp.MoleFraction.GetValueOrDefault * comp.ConstantProperties.Molar_Weight
                            Next
                            Me.Phases(0).Properties.massflow = mtotal * summ / 1000
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MassFraction = comp.MolarFlow.GetValueOrDefault * Me.Phases(0).Properties.massflow.GetValueOrDefault
                            Next
                        End If
                    Case 105
                        If Me.Phases(0).Compounds.ContainsKey(sname) Then
                            Me.Phases(0).Compounds(sname).MassFlow = SystemsOfUnits.Converter.ConvertToSI(su.massflow, propval)
                            Me.Phases(0).Compounds(sname).MolarFlow = SystemsOfUnits.Converter.ConvertToSI(su.massflow, propval) / Me.Phases(0).Compounds(sname).ConstantProperties.Molar_Weight * 1000
                            Dim mtotal As Double = 0
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                mtotal += comp.MassFlow
                            Next
                            Me.Phases(0).Properties.massflow = mtotal
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MassFraction = comp.MassFlow / mtotal
                            Next
                            Dim summ As Double = 0
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                summ += comp.MassFraction.GetValueOrDefault / comp.ConstantProperties.Molar_Weight / 1000
                            Next
                            Me.Phases(0).Properties.molarflow = mtotal / summ
                            For Each comp As Compound In Me.Phases(0).Compounds.Values
                                comp.MoleFraction = comp.MolarFlow.GetValueOrDefault * Me.Phases(0).Properties.molarflow.GetValueOrDefault
                            Next
                        End If
                End Select

                Me.PropertyPackage.CurrentMaterialStream = prevstream

            End If

            Return True

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String

            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 = "NF" Then

                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim value As String = ""

                If prop.StartsWith("PROP_MS") Then

                    Dim propidx As Integer = Integer.Parse(prop.Split("/")(0).Split("_")(2))

                    Select Case propidx

                        Case 0
                            'PROP_MS_0 Temperature
                            value = su.temperature
                        Case 1
                            'PROP_MS_1 Pressure
                            value = su.pressure
                        Case 2
                            'PROP_MS_2	Mass Flow
                            value = su.massflow
                        Case 3
                            'PROP_MS_3	Molar Flow
                            value = su.molarflow
                        Case 4
                            'PROP_MS_4	Volumetric Flow
                            value = su.volumetricFlow
                        Case 5
                            'PROP_MS_5	Mixture Density
                            value = su.density
                        Case 6
                            'PROP_MS_6	Mixture Molar Weight
                            value = su.molecularWeight
                        Case 7
                            'PROP_MS_7	Mixture Specific Enthalpy
                            value = su.enthalpy
                        Case 8
                            'PROP_MS_8	Mixture Specific Entropy
                            value = su.entropy
                        Case 9
                            'PROP_MS_9	Mixture Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 10
                            'PROP_MS_10	Mixture Molar Entropy
                            value = su.molar_entropy
                        Case 11
                            'PROP_MS_11	Mixture Thermal Conductivity
                            value = su.thermalConductivity
                        Case 12
                            'PROP_MS_12	Vapor Phase Density
                            value = su.density
                        Case 13
                            'PROP_MS_13	Vapor Phase Molar Weight
                            value = su.molecularWeight
                        Case 14
                            'PROP_MS_14	Vapor Phase Specific Enthalpy
                            value = su.enthalpy
                        Case 15
                            'PROP_MS_15	Vapor Phase Specific Entropy
                            value = su.entropy
                        Case 16
                            'PROP_MS_16	Vapor Phase Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 17
                            'PROP_MS_17	Vapor Phase Molar Entropy
                            value = su.molar_entropy
                        Case 18
                            'PROP_MS_18	Vapor Phase Thermal Conductivity
                            value = su.thermalConductivity
                        Case 19
                            'PROP_MS_19	Vapor Phase Kinematic Viscosity
                            value = su.cinematic_viscosity
                        Case 20
                            'PROP_MS_20	Vapor Phase Dynamic Viscosity
                            value = su.viscosity
                        Case 21
                            'PROP_MS_21	Vapor Phase Heat Capacity (Cp)
                            value = su.heatCapacityCp
                        Case 22
                            'PROP_MS_22	Vapor Phase Heat Capacity Ratio (Cp/Cv)
                            value = ""
                        Case 23
                            'PROP_MS_23	Vapor Phase Mass Flow
                            value = su.massflow
                        Case 24
                            'PROP_MS_24	Vapor Phase Molar Flow
                            value = su.molarflow
                        Case 25
                            'PROP_MS_25	Vapor Phase Volumetric Flow
                            value = su.volumetricFlow
                        Case 26
                            'PROP_MS_26	Vapor Phase Compressibility Factor
                            value = ""
                        Case 27
                            'PROP_MS_27	Vapor Phase Molar Fraction
                            value = ""
                        Case 28
                            'PROP_MS_28	Vapor Phase Mass Fraction
                            value = ""
                        Case 29
                            'PROP_MS_29	Vapor Phase Volumetric Fraction
                            value = ""
                        Case 30
                            'PROP_MS_30	Liquid Phase (Mixture) Density
                            value = su.density
                        Case 31
                            'PROP_MS_31	Liquid Phase (Mixture) Molar Weight
                            value = su.molecularWeight
                        Case 32
                            'PROP_MS_32	Liquid Phase (Mixture) Specific Enthalpy
                            value = su.enthalpy
                        Case 33
                            'PROP_MS_33	Liquid Phase (Mixture) Specific Entropy
                            value = su.entropy
                        Case 34
                            'PROP_MS_34	Liquid Phase (Mixture) Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 35
                            'PROP_MS_35	Liquid Phase (Mixture) Molar Entropy
                            value = su.molar_entropy
                        Case 36
                            'PROP_MS_36	Liquid Phase (Mixture) Thermal Conductivity
                            value = su.thermalConductivity
                        Case 37
                            'PROP_MS_37	Liquid Phase (Mixture) Kinematic Viscosity
                            value = su.cinematic_viscosity
                        Case 38
                            'PROP_MS_38	Liquid Phase (Mixture) Dynamic Viscosity
                            value = su.viscosity
                        Case 39
                            'PROP_MS_39	Liquid Phase (Mixture) Heat Capacity (Cp)
                            value = su.heatCapacityCp
                        Case 40
                            'PROP_MS_40	Liquid Phase (Mixture) Heat Capacity Ratio (Cp/Cv)
                            value = ""
                        Case 41
                            'PROP_MS_41	Liquid Phase (Mixture) Mass Flow
                            value = su.massflow
                        Case 42
                            'PROP_MS_42	Liquid Phase (Mixture) Molar Flow
                            value = su.molarflow
                        Case 43
                            'PROP_MS_43	Liquid Phase (Mixture) Volumetric Flow
                            value = su.volumetricFlow
                        Case 44
                            'PROP_MS_44	Liquid Phase (Mixture) Compressibility Factor
                            value = ""
                        Case 45
                            'PROP_MS_45	Liquid Phase (Mixture) Molar Fraction
                            value = ""
                        Case 46
                            'PROP_MS_46	Liquid Phase (Mixture) Mass Fraction
                            value = ""
                        Case 47
                            'PROP_MS_47	Liquid Phase (Mixture) Volumetric Fraction
                            value = ""
                        Case 48
                            'PROP_MS_48	Liquid Phase (1) Density
                            value = su.density
                        Case 49
                            'PROP_MS_49	Liquid Phase (1) Molar Weight
                            value = su.molecularWeight
                        Case 50
                            'PROP_MS_50	Liquid Phase (1) Specific Enthalpy
                            value = su.enthalpy
                        Case 51
                            'PROP_MS_51	Liquid Phase (1) Specific Entropy
                            value = su.entropy
                        Case 52
                            'PROP_MS_52	Liquid Phase (1) Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 53
                            'PROP_MS_53	Liquid Phase (1) Molar Entropy
                            value = su.molar_entropy
                        Case 54
                            'PROP_MS_54	Liquid Phase (1) Thermal Conductivity
                            value = su.thermalConductivity
                        Case 55
                            'PROP_MS_55	Liquid Phase (1) Kinematic Viscosity
                            value = su.cinematic_viscosity
                        Case 56
                            'PROP_MS_56	Liquid Phase (1) Dynamic Viscosity
                            value = su.viscosity
                        Case 57
                            'PROP_MS_57	Liquid Phase (1) Heat Capacity (Cp)
                            value = su.heatCapacityCp
                        Case 58
                            'PROP_MS_58	Liquid Phase (1) Heat Capacity Ratio (Cp/Cv)
                            value = ""
                        Case 59
                            'PROP_MS_59	Liquid Phase (1) Mass Flow
                            value = su.massflow
                        Case 60
                            'PROP_MS_60	Liquid Phase (1) Molar Flow
                            value = su.molarflow
                        Case 61
                            'PROP_MS_61	Liquid Phase (1) Volumetric Flow
                            value = su.volumetricFlow
                        Case 62
                            'PROP_MS_62	Liquid Phase (1) Compressibility Factor
                            value = ""
                        Case 63
                            'PROP_MS_63	Liquid Phase (1) Molar Fraction
                            value = ""
                        Case 64
                            'PROP_MS_64	Liquid Phase (1) Mass Fraction
                            value = ""
                        Case 65
                            'PROP_MS_65	Liquid Phase (1) Volumetric Fraction
                            value = ""
                        Case 66
                            'PROP_MS_66	Liquid Phase (2) Density
                            value = su.density
                        Case 67
                            'PROP_MS_67	Liquid Phase (2) Molar Weight
                            value = su.molecularWeight
                        Case 68
                            'PROP_MS_68	Liquid Phase (2) Specific Enthalpy
                            value = su.enthalpy
                        Case 69
                            'PROP_MS_69	Liquid Phase (2) Specific Entropy
                            value = su.entropy
                        Case 70
                            'PROP_MS_70	Liquid Phase (2) Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 71
                            'PROP_MS_71	Liquid Phase (2) Molar Entropy
                            value = su.molar_entropy
                        Case 72
                            'PROP_MS_72	Liquid Phase (2) Thermal Conductivity
                            value = su.thermalConductivity
                        Case 73
                            'PROP_MS_73	Liquid Phase (2) Kinematic Viscosity
                            value = su.cinematic_viscosity
                        Case 74
                            'PROP_MS_74	Liquid Phase (2) Dynamic Viscosity
                            value = su.viscosity
                        Case 75
                            'PROP_MS_75	Liquid Phase (2) Heat Capacity (Cp)
                            value = su.heatCapacityCp
                        Case 76
                            'PROP_MS_76	Liquid Phase (2) Heat Capacity Ratio (Cp/Cv)
                            value = ""
                        Case 77
                            'PROP_MS_77	Liquid Phase (2) Mass Flow
                            value = su.massflow
                        Case 78
                            'PROP_MS_78	Liquid Phase (2) Molar Flow
                            value = su.molarflow
                        Case 79
                            'PROP_MS_79	Liquid Phase (2) Volumetric Flow
                            value = su.volumetricFlow
                        Case 80
                            'PROP_MS_80	Liquid Phase (2) Compressibility Factor
                            value = ""
                        Case 81
                            'PROP_MS_81	Liquid Phase (2) Molar Fraction
                            value = ""
                        Case 82
                            'PROP_MS_82	Liquid Phase (2) Mass Fraction
                            value = ""
                        Case 83
                            'PROP_MS_83	Liquid Phase (2) Volumetric Fraction
                            value = ""
                        Case 84, 131
                            'PROP_MS_84	    Aqueous Phase Density
                            'PROP_MS_131	Solid Phase Density
                            value = su.density
                        Case 85, 132
                            'PROP_MS_85	    Aqueous Phase Molar Weight
                            'PROP_MS_132	Solid Phase Molar Weight
                            value = su.molecularWeight
                        Case 86, 133
                            'PROP_MS_86	    Aqueous Phase Specific Enthalpy
                            'PROP_MS_133	Solid Phase Specific Enthalpy
                            value = su.enthalpy
                        Case 87, 134
                            'PROP_MS_87	    Aqueous Phase Specific Entropy
                            'PROP_MS_134	Solid Phase Specific Entropy
                            value = su.entropy
                        Case 88, 135
                            'PROP_MS_88	    Aqueous Phase Molar Enthalpy
                            'PROP_MS_135	Solid Phase Molar Enthalpy
                            value = su.molar_enthalpy
                        Case 89, 136
                            'PROP_MS_89	    Aqueous Phase Molar Entropy
                            'PROP_MS_136	Solid Phase Molar Entropy
                            value = su.molar_entropy
                        Case 90, 137
                            'PROP_MS_90	    Aqueous Phase Thermal Conductivity
                            'PROP_MS_137	Solid Phase Thermal Conductivity
                            value = su.thermalConductivity
                        Case 91, 138
                            'PROP_MS_91	    Aqueous Phase Kinematic Viscosity
                            'PROP_MS_138	Solid Phase Kinematic Viscosity
                            value = su.cinematic_viscosity
                        Case 92, 139
                            'PROP_MS_92	    Aqueous Phase Dynamic Viscosity
                            'PROP_MS_139	Solid Phase Dynamic Viscosity
                            value = su.viscosity
                        Case 93, 140
                            'PROP_MS_93	    Aqueous Phase Heat Capacity (Cp)
                            'PROP_MS_140    Solid Phase Heat Capacity (Cp)
                            value = su.heatCapacityCp
                        Case 94, 141
                            'PROP_MS_94	    Aqueous Phase Heat Capacity Ratio (Cp/Cv)
                            'PROP_MS_141	Solid Phase Heat Capacity Ratio (Cp/Cv)
                            value = ""
                        Case 95, 142
                            'PROP_MS_95	    Aqueous Phase Mass Flow
                            'PROP_MS_142	Solid Phase Mass Flow
                            value = su.massflow
                        Case 96, 143
                            'PROP_MS_96	    Aqueous Phase Molar Flow
                            'PROP_MS_143	Solid Phase Molar Flow
                            value = su.molarflow
                        Case 97, 144
                            'PROP_MS_97	    Aqueous Phase Volumetric Flow
                            'PROP_MS_144	Solid Phase Volumetric Flow
                            value = su.volumetricFlow
                        Case 98, 145
                            'PROP_MS_98	    Aqueous Phase Compressibility Factor
                            'PROP_MS_145	Solid Phase Compressibility Factor
                            value = ""
                        Case 99, 146
                            'PROP_MS_99	    Aqueous Phase Molar Fraction
                            'PROP_MS_146    Solid Phase Molar Fraction
                            value = ""
                        Case 100, 147
                            'PROP_MS_100	Aqueous Phase Mass Fraction
                            'PROP_MS_147	Solid Phase Mass Fraction
                            value = ""
                        Case 101, 148
                            'PROP_MS_101	Aqueous Phase Volumetric Fraction
                            'PROP_MS_148	Solid Phase Volumetric Fraction
                            value = ""
                        Case 102, 103, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 149, 150
                            value = ""
                        Case 104, 116, 117, 118, 119, 120, 151
                            value = su.molarflow
                        Case 105, 121, 122, 123, 124, 125, 152
                            value = su.massflow
                        Case 126, 127
                            value = su.pressure
                        Case 128, 129
                            value = su.temperature
                        Case 154
                            value = su.heatflow
                        Case 155
                            'PROP_MS_155	Isothermal Compressibility (Vapor)	
                            value = su.compressibility
                        Case 156
                            'PROP_MS_156	Bulk Modulus (Vapor)	
                            value = su.pressure
                        Case 157
                            'PROP_MS_157	Speed of Sound (Vapor)	
                            value = su.speedOfSound
                        Case 158
                            'PROP_MS_158	Joule-Thomson Coefficient (Vapor)	
                            value = su.jouleThomsonCoefficient
                        Case 159
                            'PROP_MS_159	Internal Energy (Vapor)	
                            value = su.enthalpy
                        Case 160
                            'PROP_MS_160	Gibbs Free Energy (Vapor)	
                            value = su.enthalpy
                        Case 161
                            'PROP_MS_161	Helmholtz Free Energy (Vapor)	
                            value = su.enthalpy
                        Case 162
                            'PROP_MS_162	Isothermal Compressibility (Overall Liquid)	
                            value = su.compressibility
                        Case 163
                            'PROP_MS_163	Bulk Modulus (Overall Liquid)	
                            value = su.pressure
                        Case 164
                            'PROP_MS_164	Speed of Sound (Overall Liquid)	
                            value = su.speedOfSound
                        Case 165
                            'PROP_MS_165	Joule-Thomson Coefficient (Overall Liquid)	
                            value = su.jouleThomsonCoefficient
                        Case 166
                            'PROP_MS_166	Internal Energy (Overall Liquid)	
                            value = su.enthalpy
                        Case 167
                            'PROP_MS_167	Gibbs Free Energy (Overall Liquid)	
                            value = su.enthalpy
                        Case 168
                            'PROP_MS_168	Helmholtz Free Energy (Overall Liquid)	
                            value = su.enthalpy
                        Case 169
                            'PROP_MS_169	Bulk Modulus (Liquid 1)	
                            value = su.pressure
                        Case 170
                            'PROP_MS_170	Speed of Sound (Liquid 1)	
                            value = su.speedOfSound
                        Case 171
                            'PROP_MS_171	Joule-Thomson Coefficient (Liquid 1)	
                            value = su.jouleThomsonCoefficient
                        Case 172
                            'PROP_MS_172	Internal Energy (Liquid 1)	
                            value = su.enthalpy
                        Case 173
                            'PROP_MS_173	Gibbs Free Energy (Liquid 1)	
                            value = su.enthalpy
                        Case 174
                            'PROP_MS_174	Helmholtz Free Energy (Liquid 1)	
                            value = su.enthalpy
                        Case 175
                            'PROP_MS_175	Isothermal Compressibility (Liquid 1)	
                            value = su.compressibility
                        Case 176
                            'PROP_MS_176	Bulk Modulus (Liquid 2)	
                            value = su.pressure
                        Case 177
                            'PROP_MS_177	Speed of Sound (Liquid 2)	
                            value = su.speedOfSound
                        Case 178
                            'PROP_MS_178	Joule-Thomson Coefficient (Liquid 2)	
                            value = su.jouleThomsonCoefficient
                        Case 179
                            'PROP_MS_179	Internal Energy (Liquid 2)	
                            value = su.enthalpy
                        Case 180
                            'PROP_MS_180	Gibbs Free Energy (Liquid 2)	
                            value = su.enthalpy
                        Case 181
                            'PROP_MS_181	Helmholtz Free Energy (Liquid 2)	
                            value = su.enthalpy
                        Case 182
                            'PROP_MS_182	Isothermal Compressibility (Liquid 2)	
                            value = su.compressibility
                        Case 183
                            'PROP_MS_183	Bulk Modulus (Liquid 3)	
                            value = su.pressure
                        Case 184
                            'PROP_MS_184	Speed of Sound (Liquid 3)	
                            value = su.speedOfSound
                        Case 185
                            'PROP_MS_185	Joule-Thomson Coefficient (Liquid 3)	
                            value = su.jouleThomsonCoefficient
                        Case 186
                            'PROP_MS_186	Internal Energy (Liquid 3)	
                            value = su.enthalpy
                        Case 187
                            'PROP_MS_187	Gibbs Free Energy (Liquid 3)	
                            value = su.enthalpy
                        Case 188
                            'PROP_MS_188	Helmholtz Free Energy (Liquid 3)	
                            value = su.enthalpy
                        Case 189
                            'PROP_MS_189	Isothermal Compressibility (Liquid 3)	
                            value = su.compressibility
                        Case 190
                            'PROP_MS_190	Bulk Modulus (Aqueous Phase)	
                            value = su.pressure
                        Case 191
                            'PROP_MS_191	Speed of Sound (Aqueous Phase)	
                            value = su.speedOfSound
                        Case 192
                            'PROP_MS_192	Joule-Thomson Coefficient (Aqueous Phase)	
                            value = su.jouleThomsonCoefficient
                        Case 193
                            'PROP_MS_193	Internal Energy (Aqueous Phase)	
                            value = su.enthalpy
                        Case 194
                            'PROP_MS_194	Gibbs Free Energy (Aqueous Phase)	
                            value = su.enthalpy
                        Case 195
                            'PROP_MS_195	Helmholtz Free Energy (Aqueous Phase)	
                            value = su.enthalpy
                        Case 196
                            'PROP_MS_196	Isothermal Compressibility (Aqueous Phase)	
                            value = su.compressibility
                        Case 198
                            'PROP_MS_198	Bulk Modulus (Solid)	
                            value = su.pressure
                        Case 199
                            'PROP_MS_199	Speed of Sound (Solid)	
                            value = su.speedOfSound
                        Case 200
                            'PROP_MS_200	Joule-Thomson Coefficient (Solid)	
                            value = su.jouleThomsonCoefficient
                        Case 201
                            'PROP_MS_201	Internal Energy (Solid)	
                            value = su.enthalpy
                        Case 202
                            'PROP_MS_202	Gibbs Free Energy (Solid)	
                            value = su.enthalpy
                        Case 203
                            'PROP_MS_203	Helmholtz Free Energy (Solid)	
                            value = su.enthalpy
                        Case 204
                            'PROP_MS_204	Isothermal Compressibility (Solid)	
                            value = su.compressibility
                        Case 205
                            'PROP_MS_205	Internal Energy (Mixture)	
                            value = su.enthalpy
                        Case 206
                            'PROP_MS_206	Gibbs Free Energy (Mixture)	
                            value = su.enthalpy
                        Case 207
                            'PROP_MS_207	Helmholtz Free Energy (Mixture)	
                            value = su.enthalpy
                        Case 208
                            'PROP_MS_208	Molar Internal Energy (Mixture)	
                            value = su.molar_enthalpy
                        Case 209
                            'PROP_MS_209	Molar Gibbs Free Energy (Mixture)	
                            value = su.molar_enthalpy
                        Case 210
                            'PROP_MS_210	Molar Helmholtz Free Energy (Mixture)	
                            value = su.molar_enthalpy
                        Case 211
                            'PROP_MS_211	Molar Internal Energy (Vapor)	
                            value = su.molar_enthalpy
                        Case 212
                            'PROP_MS_212	Molar Gibbs Free Energy (Vapor)	
                            value = su.molar_enthalpy
                        Case 213
                            'PROP_MS_213	Molar Helmholtz Free Energy (Vapor)	
                            value = su.molar_enthalpy
                        Case 214
                            'PROP_MS_214	Molar Internal Energy (Overall Liquid)	
                            value = su.molar_enthalpy
                        Case 215
                            'PROP_MS_215	Molar Gibbs Free Energy (Overall Liquid)	
                            value = su.molar_enthalpy
                        Case 216
                            'PROP_MS_216	Molar Helmholtz Free Energy (Overall Liquid)	
                            value = su.molar_enthalpy
                        Case 217
                            'PROP_MS_217	Molar Internal Energy (Liquid 1)	
                            value = su.molar_enthalpy
                        Case 218
                            'PROP_MS_218	Molar Gibbs Free Energy (Liquid 1)	
                            value = su.molar_enthalpy
                        Case 219
                            'PROP_MS_219	Molar Helmholtz Free Energy (Liquid 1)	
                            value = su.molar_enthalpy
                        Case 220
                            'PROP_MS_220	Molar Internal Energy (Liquid 2)	
                            value = su.molar_enthalpy
                        Case 221
                            'PROP_MS_221	Molar Gibbs Free Energy (Liquid 2)	
                            value = su.molar_enthalpy
                        Case 222
                            'PROP_MS_222	Molar Helmholtz Free Energy (Liquid 2)	
                            value = su.molar_enthalpy
                        Case 223
                            'PROP_MS_223	Molar Internal Energy (Liquid 3)	
                            value = su.molar_enthalpy
                        Case 224
                            'PROP_MS_224	Molar Gibbs Free Energy (Liquid 3)	
                            value = su.molar_enthalpy
                        Case 225
                            'PROP_MS_225	Molar Helmholtz Free Energy (Liquid 3)	
                            value = su.molar_enthalpy
                        Case 226
                            'PROP_MS_226	Molar Internal Energy (Aqueous Phase)	
                            value = su.molar_enthalpy
                        Case 227
                            'PROP_MS_227	Molar Gibbs Free Energy (Aqueous Phase)	
                            value = su.molar_enthalpy
                        Case 228
                            'PROP_MS_228	Molar Helmholtz Free Energy (Aqueous Phase)	
                            value = su.molar_enthalpy
                        Case 229
                            'PROP_MS_229	Molar Internal Energy (Solid)	
                            value = su.molar_enthalpy
                        Case 230
                            'PROP_MS_230	Molar Gibbs Free Energy (Solid)	
                            value = su.molar_enthalpy
                        Case 231
                            'PROP_MS_231	Molar Helmholtz Free Energy (Solid)	
                            value = su.molar_enthalpy
                        Case 232, 233, 234, 235, 236, 237, 238
                            'Molality
                            value = "mol/Kg Water"
                        Case 239, 240, 241, 242, 243, 244, 245
                            'Molality
                            value = su.molar_conc
                        Case Else
                            value = ""
                    End Select

                    Return value

                Else

                    Return ""

                End If

            Else

                Return u0

            End If

        End Function

#End Region

#Region "    CAPE-OPEN ICapeIdentification"

        Public Overrides Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Overrides Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

#End Region

#Region "    CAPE-OPEN 1.0 Methods and Properties"

        ''' <summary>
        ''' Gets a list of properties that have been calculated.
        ''' </summary>
        ''' <returns>Properties for which results are available.</returns>
        ''' <remarks>Not implemented in DWSIM.</remarks>
        Public Function AvailableProps() As Object Implements CapeOpen.ICapeThermoMaterialObject.AvailableProps
            Throw New CapeOpen.CapeNoImplException
        End Function

        ''' <summary>
        ''' This method is responsible for calculating a flash or delegating flash calculations to the associated Property Package or Equilibrium Server.
        ''' </summary>
        ''' <param name="flashType">Flash calculation type.</param>
        ''' <param name="props">Properties to be calculated at equilibrium. UNDEFINED for no properties. If a list, then the 
        ''' property values should be set for each phase present at equilibrium (not including the overall phase).</param>
        ''' <remarks>The CalcEquilibrium method must set on the Material Object the amounts (phaseFraction) and compositions
        ''' (fraction) for all phases present at equilibrium, as well as the temperature and pressure for the overall
        ''' mixture, if not set as part of the calculation specifications. The CalcEquilibrium method must not set on the
        ''' Material Object any other value - in particular it must not set any values for phases that do not exist. See
        ''' 5.2.1 for more information.
        ''' The available list of flashes is given in section 5.6.1.
        ''' It is advised not to combine a flash calculation with a property calculation. Although by the returned error
        ''' one cannot see which has failed, plus the additional arguments to CalcProp (such as calculation type) cannot
        ''' be specified. Advice is to perform a CalcEquilibrium, get the phaseIDs and perform a CalcProp on the
        ''' existing phases.
        ''' The Material Object may or may not delegate this call to a Property Package.</remarks>
        Public Sub CalcEquilibrium(ByVal flashType As String, ByVal props As Object) Implements CapeOpen.ICapeThermoMaterialObject.CalcEquilibrium
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcEquilibrium(Me, flashType, props)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "CalcEquilibrium", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' This method is responsible for doing all property calculations or delegating these calculations to the
        ''' associated Property Package.
        ''' </summary>
        ''' <param name="props">The List of Properties to be calculated.</param>
        ''' <param name="phases">List of phases for which the Properties are to be calculated.</param>
        ''' <param name="calcType">Type of calculation: Mixture Property or Pure Compound Property. For
        ''' partial property, such as fugacity coefficients of compounds in a
        ''' mixture, use “Mixture” CalcType. For pure compound fugacity
        ''' coefficients, use “Pure” CalcType.</param>
        ''' <remarks>"Pure" calctype is not implemented in DWSIM.</remarks>
        Public Sub CalcProp(ByVal props As Object, ByVal phases As Object, ByVal calcType As String) Implements CapeOpen.ICapeThermoMaterialObject.CalcProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcProp(Me, props, phases, calcType)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "CalcProp", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Returns the list of compound IDs of a given Material Object.
        ''' </summary>
        ''' <value></value>
        ''' <returns>Compound IDs</returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property ComponentIds() As Object Implements CapeOpen.ICapeThermoMaterialObject.ComponentIds
            Get
                Dim compids As Object = Nothing
                Dim formulas As Object = Nothing
                Dim mols As Object = Nothing
                Dim cas As Object = Nothing
                Dim nbps As Object = Nothing
                Dim names As Object = Nothing
                Me.PropertyPackage.CurrentMaterialStream = Me
                Try
                    Me.PropertyPackage.GetComponentList(compids, formulas, names, nbps, mols, cas)
                Catch ex As Exception
                    Dim hcode As Integer = 0
                    Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                    If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "ComponentIds", hcode)
                End Try
                Return compids
            End Get
        End Property

        ''' <summary>
        ''' Create a Material Object from the parent Material Template of the current Material Object.
        ''' </summary>
        ''' <returns>The created and initialized Material Object.</returns>
        ''' <remarks></remarks>
        Public Function CreateMaterialObject() As Object Implements CapeOpen.ICapeThermoMaterialObject.CreateMaterialObject
            Return DirectCast(Me.FlowSheet, CapeOpen.ICapeMaterialTemplateSystem).CreateMaterialTemplate("temporary stream")
        End Function

        ''' <summary>
        ''' Creates a duplicate of the current Material Object.
        ''' </summary>
        ''' <returns>The duplicated Material Object.</returns>
        ''' <remarks></remarks>
        Public Function Duplicate() As Object Implements CapeOpen.ICapeThermoMaterialObject.Duplicate
            Dim newmat As MaterialStream = Me.Clone
            newmat._flowsheet = Me._flowsheet
            Return newmat
        End Function

        ''' <summary>
        ''' Retrieve pure compound constants from the Property Package.
        ''' </summary>
        ''' <param name="props">List of pure compound constants</param>
        ''' <param name="compIds">List of compound IDs for which constants are to be retrieved.
        '''UNDEFINED is to be used when the call applied to all compounds in
        '''the Material Object.</param>
        ''' <returns>Compound Constant values returned from the Property Package for the
        ''' specified compounds.</returns>
        ''' <remarks></remarks>
        Public Function GetComponentConstant(ByVal props As Object, ByVal compIds As Object) As Object Implements CapeOpen.ICapeThermoMaterialObject.GetComponentConstant
            Me.PropertyPackage.CurrentMaterialStream = Me
            Dim obj As Object = Nothing
            Try
                obj = Me.PropertyPackage.GetCompoundConstant(props, compIds)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "ComponentIds", hcode)
            End Try
            Return obj
        End Function

        ''' <summary>
        ''' Returns the independent variables of a Material Object. This method is deprecated.
        ''' </summary>
        ''' <param name="indVars">Independent variables to be set</param>
        ''' <returns>Values of independent variables.</returns>
        ''' <remarks>This method should not be used.</remarks>
        Public Function GetIndependentVar(ByVal indVars As Object) As Object Implements CapeOpen.ICapeThermoMaterialObject.GetIndependentVar
            Throw New CapeOpen.CapeNoImplException
        End Function

        ''' <summary>
        ''' Returns number of chemical compounds in Material Object.
        ''' </summary>
        ''' <returns>Number of compounds in the Material Object.</returns>
        ''' <remarks></remarks>
        Public Function GetNumComponents() As Integer Implements CapeOpen.ICapeThermoMaterialObject.GetNumComponents
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.Phases(0).Compounds.Count
        End Function

        ''' <summary>
        ''' This method is responsible for retrieving the results from calculations from the Material Object.
        ''' </summary>
        ''' <param name="property">The Property for which results are requested from the Material Object.</param>
        ''' <param name="phase">The qualified phase for the results.</param>
        ''' <param name="compIds">The qualified compounds for the results. UNDEFINED to specify all
        ''' compounds in the Material Object. For scalar mixture properties such
        ''' as liquid enthalpy, this qualifier must not be specified. Use
        ''' UNDEFINED as place holder.</param>
        ''' <param name="calcType">The qualified type of calculation for the results. (valid Calculation Types: Pure and Mixture)</param>
        ''' <param name="basis">Qualifies the basis of the result (i.e., mass /mole). Use UNDEFINED
        ''' for default or as place holder for property for which basis does not apply (see also 3.3.1).</param>
        ''' <returns>Results vector containing property values in SI units arranged by the defined qualifiers.</returns>
        ''' <remarks></remarks>
        Public Function GetProp(ByVal [property] As String, ByVal phase As String, ByVal compIds As Object, ByVal calcType As String, ByVal basis As String) As Object Implements CapeOpen.ICapeThermoMaterialObject.GetProp

            Me.PropertyPackage.CurrentMaterialStream = Me

            If Not calcType Is Nothing Then
                If calcType = "Pure" Then Throw New CapeLimitedImplException
            End If
            Dim res As New ArrayList
            Dim length As Integer, i As Integer, comps As New ArrayList
            If Not compIds Is Nothing Then
                length = compIds.length
                If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) Is Nothing Then
                    For i = 0 To length - 1
                        comps.Add(compIds(i))
                    Next
                Else
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        For i = 0 To length - 1
                            If kvp.Value = compIds(i) Then comps.Add(kvp.Key)
                        Next
                    Next
                End If
            Else
                If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                    Dim complist As Object = Nothing
                    Me.PropertyPackage.GetComponentList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                    For Each s As String In complist
                        For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                            If kvp.Value = s Then
                                comps.Add(kvp.Key)
                                Exit For
                            End If
                        Next
                    Next
                Else
                    For Each c As Compound In Me.Phases(0).Compounds.Values
                        comps.Add(c.Name)
                    Next
                End If
            End If
            Dim f As Integer = 0
            Dim phs As PropertyPackages.Phase
            Select Case phase.ToLower
                Case "overall"
                    f = 0
                    phs = PropertyPackages.Phase.Mixture
                Case Else
                    For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                        If phase = pi.PhaseLabel Then
                            f = pi.DWPhaseIndex
                            phs = pi.DWPhaseID
                            Exit For
                        End If
                    Next
            End Select
            Select Case [property].ToLower
                Case "compressibilityfactor"
                    res.Add(Me.Phases(f).Properties.compressibilityFactor.GetValueOrDefault)
                Case "heatofvaporization"
                Case "heatcapacity", "heatcapacitycp"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.heatCapacityCp * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.heatCapacityCp * 1000)
                    End Select
                Case "heatcapacitycv"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.heatCapacityCv * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.heatCapacityCv * 1000)
                    End Select
                Case "idealgasheatcapacity"
                    If f = 1 Then
                        res.Add(Me.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Liquid, Me.Phases(0).Properties.temperature * 1000))
                    Else
                        res.Add(Me.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Me.Phases(0).Properties.temperature * 1000))
                    End If
                Case "idealgasenthalpy"
                    If f = 1 Then
                        res.Add(Me.PropertyPackage.RET_Hid(298.15, Me.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Liquid))
                    Else
                        res.Add(Me.PropertyPackage.RET_Hid(298.15, Me.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Vapor))
                    End If
                Case "excessenthalpy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * 1000)
                    End Select
                Case "excessentropy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.excessEntropy.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.excessEntropy.GetValueOrDefault * 1000)
                    End Select
                Case "viscosity"
                    res.Add(Me.Phases(f).Properties.viscosity.GetValueOrDefault)
                Case "thermalconductivity"
                    res.Add(Me.Phases(f).Properties.thermalConductivity.GetValueOrDefault)
                Case "fugacity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault * Me.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault * Me.Phases(0).Properties.pressure.GetValueOrDefault)
                    Next
                Case "activity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).ActivityCoeff.GetValueOrDefault * Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                    Next
                Case "fugacitycoefficient"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).FugacityCoeff)
                    Next
                Case "activitycoefficient"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).ActivityCoeff)
                    Next
                Case "logfugacitycoefficient"
                    For Each c As String In comps
                        res.Add(Math.Log(Me.Phases(f).Compounds(c).FugacityCoeff))
                    Next
                Case "volume"
                    res.Add(Me.Phases(f).Properties.molecularWeight / Me.Phases(f).Properties.density / 1000)
                Case "density"
                    res.Add(Me.Phases(f).Properties.density)
                Case "enthalpy", "enthalpynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_enthalpy.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.enthalpy.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.enthalpy.GetValueOrDefault * 1000)
                    End Select
                Case "entropy", "entropynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_entropy.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                    End Select
                Case "enthalpyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_enthalpyF.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.enthalpyF.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.enthalpyF.GetValueOrDefault * 1000)
                    End Select
                Case "entropyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_entropyF.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.entropyF.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                    End Select
                Case "moles"
                    res.Add(Me.Phases(f).Properties.molarflow.GetValueOrDefault)
                Case "mass"
                    res.Add(Me.Phases(f).Properties.massflow.GetValueOrDefault)
                Case "molecularweight"
                    res.Add(Me.Phases(f).Properties.molecularWeight)
                Case "temperature"
                    res.Add(Me.Phases(0).Properties.temperature.GetValueOrDefault)
                Case "pressure"
                    res.Add(Me.Phases(0).Properties.pressure.GetValueOrDefault)
                Case "flow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault)
                            Next
                        Case "Mass", "mass"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MassFlow.GetValueOrDefault)
                            Next
                    End Select
                Case "fraction", "massfraction", "molarfraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole", ""
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                            Next
                        Case "Mass", "mass"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                            Next
                        Case ""
                            If [property].ToLower.Contains("mole") Then
                                For Each c As String In comps
                                    res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                                Next
                            ElseIf [property].ToLower.Contains("mass") Then
                                For Each c As String In comps
                                    res.Add(Me.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                                Next
                            End If
                    End Select
                Case "concentration"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MassFlow.GetValueOrDefault / Me.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                    Next
                Case "molarity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault / Me.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                    Next
                Case "phasefraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.molarfraction.GetValueOrDefault)
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.massfraction.GetValueOrDefault)
                    End Select
                Case "totalflow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.molarflow.GetValueOrDefault)
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.massflow.GetValueOrDefault)
                    End Select
                Case "kvalue"
                    For Each c As String In comps
                        res.Add(Me.Phases(0).Compounds(c).Kvalue)
                    Next
                Case "logkvalue"
                    For Each c As String In comps
                        res.Add(Me.Phases(0).Compounds(c).lnKvalue)
                    Next
                Case "surfacetension"
                    res.Add(Me.Phases(1).Properties.surfaceTension)
                Case Else
                    Dim ex As New CapeOpen.CapeThrmPropertyNotAvailableException
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "GetProp", hcode)
            End Select

            Dim arr(res.Count - 1) As Double
            Array.Copy(res.ToArray, arr, res.Count)
            Return arr

        End Function

        ''' <summary>
        ''' Returns list of properties that can be calculated by the Material Object.
        ''' </summary>
        ''' <returns>List of all supported properties of the Material Object.</returns>
        ''' <remarks>DWSIM passes this call to the Property Package currently associated to the stream.</remarks>
        Public Function GetPropList() As Object Implements CapeOpen.ICapeThermoMaterialObject.GetPropList
            Dim mylist As Object = Me.PropertyPackage.GetPropList
            Return mylist
        End Function

        ''' <summary>
        ''' Retrieves values of universal constants from the Property Package.
        ''' </summary>
        ''' <param name="props">List of universal constants to be retrieved</param>
        ''' <returns>Values of universal constants</returns>
        ''' <remarks>DWSIM passes this call to the Property Package currently associated to the stream.</remarks>
        Public Function GetUniversalConstant(ByVal props As Object) As Object Implements CapeOpen.ICapeThermoMaterialObject.GetUniversalConstant
            Return Me.PropertyPackage.GetUniversalConstant(Me, props)
        End Function

        ''' <summary>
        ''' It returns the phases existing in the Material Object at that moment.
        ''' </summary>
        ''' <value></value>
        ''' <returns>List of phases</returns>
        ''' <remarks></remarks>
        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property PhaseIds() As Object Implements CapeOpen.ICapeThermoMaterialObject.PhaseIds
            Get
                Dim pl As New ArrayList
                For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                    If Me.Phases.ContainsKey(pi.DWPhaseIndex.ToString) Then
                        If pi.PhaseLabel <> "Disabled" And Me.Phases(pi.DWPhaseIndex).Properties.molarfraction.GetValueOrDefault > 0 Then
                            pl.Add(pi.PhaseLabel)
                        End If
                    End If
                Next
                Dim arr(pl.Count - 1) As String
                Array.Copy(pl.ToArray, arr, pl.Count)
                Return arr
            End Get
        End Property

        ''' <summary>
        ''' Checks to see if a list of given properties can be calculated.
        ''' </summary>
        ''' <param name="props">Properties to check.</param>
        ''' <returns>Returns Boolean List associated to list of properties to be checked.</returns>
        ''' <remarks>Not implemented in DWSIM. As it was unclear from the original specification what PropCheck should exactly be checking, and as the
        ''' argument list does not include a phase specification, implementations vary. It is generally expected that
        ''' PropCheck at least verifies that the Property is available for calculation in the Material Object. However, this
        ''' can also be verified with PropList. It is advised not to use PropCheck.
        ''' The Material Object may or may not delegate this call to a Property Package.</remarks>
        Public Function PropCheck(ByVal props As Object) As Object Implements CapeOpen.ICapeThermoMaterialObject.PropCheck
            Throw New CapeNoImplException
        End Function

        ''' <summary>
        ''' RemoveResults
        ''' </summary>
        ''' <param name="props">Properties to be removed. UNDEFINED to remove all properties.</param>
        ''' <remarks>Not implemented.</remarks>
        Public Sub RemoveResults(ByVal props As Object) Implements CapeOpen.ICapeThermoMaterialObject.RemoveResults
            Throw New CapeOpen.CapeNoImplException
        End Sub

        ''' <summary>
        ''' SetIndependentVar
        ''' </summary>
        ''' <param name="indVars">Sets the independent variable for a given Material Object. This method is deprecated.</param>
        ''' <param name="values">Independent variables to be set</param>
        ''' <remarks>Values of independent variables.</remarks>
        Public Sub SetIndependentVar(ByVal indVars As Object, ByVal values As Object) Implements CapeOpen.ICapeThermoMaterialObject.SetIndependentVar
            Throw New CapeNoImplException
        End Sub

        ''' <summary>
        ''' This method is responsible for setting the values for properties of the Material Object.
        ''' </summary>
        ''' <param name="property">The property for which the values need to be set.</param>
        ''' <param name="phase">Phase for which the property is to be set.</param>
        ''' <param name="compIds">Compounds for which values are to be set. UNDEFINED to specify all
        ''' compounds in the Material Object. For scalar mixture properties such
        ''' as liquid enthalpy, this qualifier should not be specified. Use
        ''' UNDEFINED as place holder.</param>
        ''' <param name="calcType">The calculation type. (valid Calculation Types: Pure and Mixture)</param>
        ''' <param name="basis">Qualifies the basis (mole / mass). See also 3.3.2.</param>
        ''' <param name="values">Values to set for the property.</param>
        ''' <remarks>DWSIM doesn't implement "Pure" calculation type.</remarks>
        Public Sub SetProp(ByVal [property] As String, ByVal phase As String, ByVal compIds As Object, ByVal calcType As String, ByVal basis As String, ByVal values As Object) Implements CapeOpen.ICapeThermoMaterialObject.SetProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            If Not calcType Is Nothing Then
                If calcType = "Pure" Then Throw New CapeLimitedImplException
            End If
            Dim res As New ArrayList
            Dim length As Integer, i As Integer, comps As New ArrayList
            If Not compIds Is Nothing Then
                length = compIds.length
                If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        For i = 0 To length - 1
                            If kvp.Value = compIds(i) Then comps.Add(kvp.Key)
                        Next
                    Next
                Else
                    For i = 0 To length - 1
                        comps.Add(compIds(i))
                    Next
                End If
            Else
                If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                    Dim complist As Object = Nothing
                    Me.PropertyPackage.GetComponentList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                    For Each s As String In complist
                        For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                            If kvp.Value = s Then
                                comps.Add(kvp.Key)
                                Exit For
                            End If
                        Next
                    Next
                Else
                    For Each c As Compound In Me.Phases(0).Compounds.Values
                        comps.Add(c.Name)
                    Next
                End If
            End If
            Dim f As Integer = -1
            Dim phs As PropertyPackages.Phase
            Select Case phase.ToLower
                Case "overall"
                    f = 0
                    phs = PropertyPackages.Phase.Mixture
                Case Else
                    For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                        If phase = pi.PhaseLabel Then
                            f = pi.DWPhaseIndex
                            phs = pi.DWPhaseID
                            Exit For
                        End If
                    Next
            End Select
            Select Case [property].ToLower
                Case "compressibilityfactor"
                    Me.Phases(f).Properties.compressibilityFactor = values(0)
                Case "heatcapacity", "heatcapacitycp"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.heatCapacityCp = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.heatCapacityCp = values(0) / 1000
                    End Select
                Case "heatcapacitycv"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.heatCapacityCv = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.heatCapacityCv = values(0) / 1000
                    End Select
                Case "excessenthalpy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.excessEnthalpy = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.excessEnthalpy = values(0) / 1000
                    End Select
                Case "excessentropy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.excessEntropy = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.excessEntropy = values(0) / 1000
                    End Select
                Case "viscosity"
                    Me.Phases(f).Properties.viscosity = values(0)
                    Me.Phases(f).Properties.kinematic_viscosity = values(0) / Me.Phases(f).Properties.density.GetValueOrDefault
                Case "thermalconductivity"
                    Me.Phases(f).Properties.thermalConductivity = values(0)
                Case "fugacity"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = values(comps.IndexOf(c)) / (Me.Phases(0).Properties.pressure.GetValueOrDefault * Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                        i += 1
                    Next
                Case "fugacitycoefficient"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "activitycoefficient"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).ActivityCoeff = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "logfugacitycoefficient"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = Math.Exp(values(comps.IndexOf(c)))
                        i += 1
                    Next
                Case "density"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.density = values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.density = values(0)
                    End Select
                Case "volume"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.density = 1 / values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.density = 1 / values(0)
                    End Select
                Case "enthalpy", "enthalpynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_enthalpy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.enthalpy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.enthalpy = values(0) / 1000
                            Me.Phases(f).Properties.molar_enthalpy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "entropy", "entropynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_entropy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.entropy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.entropy = values(0) / 1000
                            Me.Phases(f).Properties.molar_entropy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "enthalpyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            Me.Phases(f).Properties.molar_enthalpyF = values(0)
                            If val <> 0.0# Then Me.Phases(f).Properties.enthalpyF = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.enthalpyF = values(0) / 1000
                            Me.Phases(f).Properties.molar_enthalpyF = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "entropyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            Me.Phases(f).Properties.molar_entropyF = values(0)
                            If val <> 0.0# Then Me.Phases(f).Properties.entropyF = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.entropyF = values(0) / 1000
                            Me.Phases(f).Properties.molar_entropyF = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "moles"
                    Me.Phases(f).Properties.molarflow = values(0)
                Case "mass"
                    Me.Phases(f).Properties.massflow = values(0)
                Case "molecularweight"
                    Me.Phases(f).Properties.molecularWeight = values(0)
                Case "temperature"
                    Me.Phases(0).Properties.temperature = values(0)
                Case "pressure"
                    Me.Phases(0).Properties.pressure = values(0)
                Case "flow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            i = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MolarFlow = values(comps.IndexOf(c))
                                i += 1
                            Next
                        Case "Mass", "mass"
                            i = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MassFlow = values(comps.IndexOf(c))
                                i += 1
                            Next
                    End Select
                Case "fraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            i = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MoleFraction = values(comps.IndexOf(c))
                                i += 1
                            Next
                        Case "Mass", "mass"
                            i = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MassFraction = values(comps.IndexOf(c))
                                i += 1
                            Next
                    End Select
                Case "phasefraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molarfraction = values(0)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.massfraction = values(0)
                    End Select
                Case "totalflow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molarflow = values(0)
                            'Me.Phases(f).Properties.massflow = values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.massflow = values(0)
                            'Me.Phases(f).Properties.molarflow = values(0)
                    End Select
                Case "kvalue"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(0).Compounds(c).Kvalue = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "logkvalue"
                    i = 0
                    For Each c As String In comps
                        Me.Phases(0).Compounds(c).lnKvalue = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "surfacetension"
                    Me.Phases(0).Properties.surfaceTension = values(0)
                Case Else
                    Dim ex As New CapeOpen.CapeThrmPropertyNotAvailableException
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialObject", ex.Source, ex.StackTrace, "ComponentIds", hcode)
            End Select
        End Sub

        ''' <summary>
        ''' Checks the validity of the calculation. This method is deprecated.
        ''' </summary>
        ''' <param name="props">The properties for which reliability is checked.</param>
        ''' <returns>Returns the reliability scale of the calculation.</returns>
        ''' <remarks>The ValidityCheck method must not be used, since the ICapeThermoReliability interface is not yet defined.</remarks>
        Public Function ValidityCheck(ByVal props As Object) As Object Implements CapeOpen.ICapeThermoMaterialObject.ValidityCheck
            Return Me.PropertyPackage.ValidityCheck(Me, props)
        End Function

        Public Sub CalcProp1(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String) Implements CapeOpen.ICapeThermoCalculationRoutine.CalcProp
            CalcProp1(materialObject, props, phases, calcType)
        End Sub

        Public Function GetPropList1() As Object Implements CapeOpen.ICapeThermoCalculationRoutine.GetPropList
            Return GetPropList()
        End Function

        Public Function PropCheck1(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoCalculationRoutine.PropCheck
            Return PropCheck1(materialObject, props)
        End Function

        Public Function ValidityCheck1(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoCalculationRoutine.ValidityCheck
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return ValidityCheck(props)
        End Function

        Public Sub CalcEquilibrium2(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.CalcEquilibrium
            Try
                Me.PropertyPackage.CalcEquilibrium(materialObject, flashType, props)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoEquilibriumServer", ex.Source, ex.StackTrace, "ComponentIds", hcode)
            End Try
        End Sub

        Public Sub PropCheck2(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object, ByRef valid As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.PropCheck
            Try
                Me.PropertyPackage.PropCheck1(materialObject, flashType, props, valid)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoEquilibriumServer", ex.Source, ex.StackTrace, "PropCheck", hcode)
            End Try
        End Sub

        Public Sub PropList(ByRef flashType As Object, ByRef props As Object, ByRef phases As Object, ByRef calcType As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.PropList
            Try
                Me.PropertyPackage.PropList(flashType, props, phases, calcType)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoEquilibriumServer", ex.Source, ex.StackTrace, "PropList", hcode)
            End Try
        End Sub

        Public Sub ValidityCheck2(ByVal materialObject As Object, ByVal props As Object, ByRef relList As Object) Implements CapeOpen.ICapeThermoEquilibriumServer.ValidityCheck
            Throw New CapeNoImplException
        End Sub

        Public Sub CalcEquilibrium3(ByVal materialObject As Object, ByVal flashType As String, ByVal props As Object) Implements CapeOpen.ICapeThermoPropertyPackage.CalcEquilibrium
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcEquilibrium(materialObject, flashType, props)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "CalcEquilibrium", hcode)
            End Try
        End Sub

        Public Sub CalcProp2(ByVal materialObject As Object, ByVal props As Object, ByVal phases As Object, ByVal calcType As String) Implements CapeOpen.ICapeThermoPropertyPackage.CalcProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcProp(materialObject, props, phases, calcType)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "CalcProp", hcode)
            End Try
        End Sub

        Public Function GetComponentConstant1(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetComponentConstant
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetComponentConstant(materialObject, props)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetComponentConstant", hcode)
                Return Nothing
            End Try
        End Function

        Public Sub GetComponentList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molWt As Object, ByRef casNo As Object) Implements CapeOpen.ICapeThermoPropertyPackage.GetComponentList
            If Not Me.PropertyPackage Is Nothing Then
                Me.PropertyPackage.CurrentMaterialStream = Me
                Try
                    Me.PropertyPackage.GetComponentList(compIds, formulae, names, boilTemps, molWt, casNo)
                Catch ex As Exception
                    Dim hcode As Integer = 0
                    Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                    If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetComponentList", hcode)
                End Try
            Else
                Try
                    _GetComponentList(compIds, formulae, names, boilTemps, molWt, casNo)
                Catch ex As Exception
                    Dim hcode As Integer = 0
                    Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                    If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetComponentList", hcode)
                End Try
            End If
        End Sub

        Private Sub _GetComponentList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molWt As Object, ByRef casNo As Object)

            Dim ids, formulas, nms, bts, casnos, molws As New ArrayList

            If Settings.CAPEOPENMode Then
                For Each c As ConstantProperties In Me.PropertyPackage._selectedcomps.Values
                    ids.Add(c.Name)
                    formulas.Add(c.Formula)
                    nms.Add(Me.FlowSheet.GetTranslatedString(c.Name))
                    bts.Add(c.Normal_Boiling_Point)
                    If c.CAS_Number <> "" Then casnos.Add(c.CAS_Number) Else casnos.Add(c.Name)
                    molws.Add(c.Molar_Weight)
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    ids.Add(c.ConstantProperties.Name)
                    formulas.Add(c.ConstantProperties.Formula)
                    nms.Add(Me.FlowSheet.GetTranslatedString(c.ConstantProperties.Name))
                    bts.Add(c.ConstantProperties.Normal_Boiling_Point)
                    If c.ConstantProperties.CAS_Number <> "" Then casnos.Add(c.ConstantProperties.CAS_Number) Else casnos.Add(c.ConstantProperties.Name)
                    molws.Add(c.ConstantProperties.Molar_Weight)
                Next
            End If

            Dim _i(ids.Count - 1) As String
            Dim _f(ids.Count - 1) As String
            Dim _n(ids.Count - 1) As String
            Dim _c(ids.Count - 1) As String
            Dim _b(ids.Count - 1) As Double
            Dim _m(ids.Count - 1) As Double

            Array.Copy(ids.ToArray, _i, ids.Count)
            Array.Copy(formulas.ToArray, _f, ids.Count)
            Array.Copy(nms.ToArray, _n, ids.Count)
            Array.Copy(casnos.ToArray, _c, ids.Count)
            Array.Copy(bts.ToArray, _b, ids.Count)
            Array.Copy(molws.ToArray, _m, ids.Count)

            If ids.Count > 0 Then
                compIds = _i
                formulae = _f
                names = _n
                boilTemps = _b
                casNo = _c
                molWt = _m
            Else
                compIds = Nothing
                formulae = Nothing
                names = Nothing
                casNo = Nothing
                boilTemps = Nothing
                molWt = Nothing
            End If

        End Sub

        Public Function GetPhaseList1() As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetPhaseList
            Try
                Return Me.PropertyPackage.GetPhaseList()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetPhaseList", hcode)
                Return Nothing
            End Try
        End Function

        Public Function GetPropList2() As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetPropList
            Try
                Return Me.PropertyPackage.GetPropList()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetPropList", hcode)
                Return Nothing
            End Try
        End Function

        Public Function GetUniversalConstant1(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.GetUniversalConstant
            Return Me.PropertyPackage.GetUniversalConstant(materialObject, props)
        End Function

        Public Function PropCheck3(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.PropCheck
            Try
                Return Me.PropertyPackage.PropCheck(materialObject, props)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "PropCheck", hcode)
                Return Nothing
            End Try
        End Function

        Public Function ValidityCheck3(ByVal materialObject As Object, ByVal props As Object) As Object Implements CapeOpen.ICapeThermoPropertyPackage.ValidityCheck
            Return Me.PropertyPackage.ValidityCheck(materialObject, props)
        End Function

        Public Function CreateMaterialObject1() As Object Implements CapeOpen.ICapeThermoMaterialTemplate.CreateMaterialObject
            Dim mstr As MaterialStream = CreateMaterial()
            Return mstr
        End Function

        Public Sub SetProp1(ByVal [property] As String, ByVal values As Object) Implements CapeOpen.ICapeThermoMaterialTemplate.SetProp
            ThrowCAPEException(New NotImplementedException(), "Not implemented.", "Not implemented.", "ICapeThermoMaterialTemplate", "", "", "", 0)
        End Sub

#End Region

#Region "    CAPE-OPEN 1.1 Thermo & Properties"

        ''' <summary>
        ''' Returns the values of constant Physical Properties for the specified Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid
        ''' identifiers for constant Physical Properties are listed in section 7.5.2.</param>
        ''' <param name="compIds">List of Compound identifiers for which constants are to
        ''' be retrieved. Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <returns>Values of constants for the specified Compounds.</returns>
        ''' <remarks>The GetConstPropList method can be used in order to check which constant Physical
        ''' Properties are available.
        ''' If the number of requested Physical Properties is P and the number of Compounds is C, the
        ''' propvals array will contain C*P variants. The first C variants will be the values for the first
        ''' requested Physical Property (one variant for each Compound) followed by C values of constants
        ''' for the second Physical Property, and so on. The actual type of values returned
        ''' (Double, String, etc.) depends on the Physical Property as specified in section 7.5.2.
        ''' Physical Properties are returned in a fixed set of units as specified in section 7.5.2.
        ''' If the compIds argument is set to UNDEFINED this is a request to return property values for
        ''' all compounds in the component that implements the ICapeThermoCompounds interface
        ''' with the compound order the same as that returned by the GetCompoundList method. For
        ''' example, if the interface is implemented by a Property Package component the property
        ''' request with compIds set to UNDEFINED means all compounds in the Property Package
        ''' rather than all compounds in the Material Object passed to the Property package.
        ''' If any Physical Property is not available for one or more Compounds, then undefined values
        ''' must be returned for those combinations and an ECapeThrmPropertyNotAvailable exception
        ''' must be raised. If the exception is raised, the client should check all the values returned to
        ''' determine which is undefined.</remarks>
        Public Function GetCompoundConstant(ByVal props As Object, ByVal compIds As Object) As Object Implements ICapeThermoCompounds.GetCompoundConstant
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetCompoundConstant(props, compIds)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetCompoundConstant", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Returns the list of all Compounds. This includes the Compound identifiers recognised and extra
        '''information that can be used to further identify the Compounds.
        ''' </summary>
        ''' <param name="compIds">List of Compound identifiers</param>
        ''' <param name="formulae">List of Compound formulae</param>
        ''' <param name="names">List of Compound names.</param>
        ''' <param name="boilTemps">List of boiling point temperatures.</param>
        ''' <param name="molwts">List of molecular weights.</param>
        ''' <param name="casnos">List of Chemical Abstract Service (CAS) Registry numbers.</param>
        ''' <remarks>If any item cannot be returned then the value should be set to UNDEFINED. The same information
        ''' can also be extracted using the GetCompoundConstant method. The equivalences
        ''' between GetCompoundList arguments and Compound constant Physical Properties, as
        ''' specified in section 7.5.2, is given in the table below.
        ''' When the ICapeThermoCompounds interface is implemented by a Material Object, the list
        ''' of Compounds returned is fixed when the Material Object is configured.
        ''' For a Property Package component, the Property Package will normally contain a limited set
        ''' of Compounds selected for a particular application, rather than all possible Compounds that
        ''' could be available to a proprietary Properties System.
        ''' The compIds returned by the GetCompoundList method must be unique within the
        ''' component that implements the ICapeThermoCompounds interface. There is no restriction
        ''' on the length of the strings returned in compIds. However, it should be recognised that a
        ''' PME may restrict the length of Compound identifiers internally. In such a case the PME’s
        ''' CAPE-OPEN socket must maintain a method of mapping the, potentially long, identifiers
        ''' used by a CAPE-OPEN Property package component to the identifiers used within the PME.
        ''' In order to identify the Compounds of a Property Package, the PME, or other client, will use
        ''' the casnos argument rather than the compIds. This is because different PMEs and different
        ''' Property Packages may give different names to the same Compounds and the casnos is
        ''' (almost always) unique. If the casnos is not available (e.g. for petroleum fractions), or not
        ''' unique, the other pieces of information returned by GetCompoundList can be used to
        ''' distinguish the Compounds. It should be noted, however, that for communication with a
        ''' Property Package a client must use the Compound identifiers returned in the compIds
        ''' argument. It is the responsibility of the client to maintain appropriate data structures that
        ''' allow it to reconcile the different Compound identifiers used by different Property Packages
        ''' and any native property system.</remarks>
        Public Sub GetCompoundList(ByRef compIds As Object, ByRef formulae As Object, ByRef names As Object, ByRef boilTemps As Object, ByRef molwts As Object, ByRef casnos As Object) Implements ICapeThermoCompounds.GetCompoundList
            If Not Me.PropertyPackage Is Nothing Then
                Me.PropertyPackage.CurrentMaterialStream = Me
                Try
                    Me.PropertyPackage.GetCompoundList(compIds, formulae, names, boilTemps, molwts, casnos)
                Catch ex As Exception
                    Dim hcode As Integer = 0
                    Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                    If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetComponentList", hcode)
                End Try
            Else
                Try
                    _GetComponentList(compIds, formulae, names, boilTemps, molwts, casnos)
                Catch ex As Exception
                    Dim hcode As Integer = 0
                    Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                    If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyPackage", ex.Source, ex.StackTrace, "GetComponentList", hcode)
                End Try
            End If
        End Sub

        ''' <summary>
        ''' Returns the list of supported constant Physical Properties.
        ''' </summary>
        ''' <returns>List of identifiers for all supported constant Physical Properties. The standard constant property identifiers are listed in section 7.5.2.</returns>
        ''' <remarks>GetConstPropList returns identifiers for all the constant Physical Properties that can be
        ''' retrieved by the GetCompoundConstant method. If no properties are supported,
        ''' UNDEFINED should be returned. The CAPE-OPEN standards do not define a minimum list
        ''' of Physical Properties to be made available by a software component that implements the
        ''' ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return constant
        ''' Physical Property identifiers which do not belong to the list defined in section 7.5.2.
        ''' However, these proprietary identifiers may not be understood by most of the clients of this
        ''' component.</remarks>
        Public Function GetConstPropList() As Object Implements ICapeThermoCompounds.GetConstPropList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetConstPropList()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetConstPropList", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Returns the number of Compounds supported.
        ''' </summary>
        ''' <returns>Number of Compounds supported.</returns>
        ''' <remarks>The number of Compounds returned by this method must be equal to the number of
        ''' Compound identifiers that are returned by the GetCompoundList method of this interface. It
        ''' must be zero or a positive number.</remarks>
        Public Function GetNumCompounds() As Integer Implements ICapeThermoCompounds.GetNumCompounds
            Return Me.Phases(0).Compounds.Count
        End Function

        ''' <summary>
        ''' Returns the values of pressure-dependent Physical Properties for the specified pure Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid identifiers for pressure-dependent 
        ''' Physical Properties are listed in section 7.5.4</param>
        ''' <param name="pressure">Pressure (in Pa) at which Physical Properties are evaluated</param>
        ''' <param name="compIds">List of Compound identifiers for which Physical Properties are to be retrieved. 
        ''' Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <param name="propVals">Property values for the Compounds specified.</param>
        ''' <remarks></remarks>
        Public Sub GetPDependentProperty(ByVal props As Object, ByVal pressure As Double, ByVal compIds As Object, ByRef propVals As Object) Implements ICapeThermoCompounds.GetPDependentProperty
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.GetPDependentProperty(props, pressure, compIds, propVals)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetPDependentProperty", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Returns the list of supported pressure-dependent properties.
        ''' </summary>
        ''' <returns>The list of Physical Property identifiers for all supported pressure-dependent properties. The standard identifiers are listed in section 7.5.4</returns>
        ''' <remarks>GetPDependentPropList returns identifiers for all the pressure-dependent properties that can
        ''' be retrieved by the GetPDependentProperty method. If no properties are supported
        ''' UNDEFINED should be returned. The CAPE-OPEN standards do not define a minimum list
        ''' of Physical Properties to be made available by a software component that implements the
        ''' ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return identifiers
        ''' which do not belong to the list defined in section 7.5.4. However, these proprietary
        ''' identifiers may not be understood by most of the clients of this component.</remarks>
        Public Function GetPDependentPropList() As Object Implements ICapeThermoCompounds.GetPDependentPropList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetPDependentPropList()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetPDependentList", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Returns the values of temperature-dependent Physical Properties for the specified pure Compounds.
        ''' </summary>
        ''' <param name="props">The list of Physical Property identifiers. Valid identifiers for 
        ''' temperature-dependent Physical Properties are listed in section 7.5.3</param>
        ''' <param name="temperature">Temperature (in K) at which properties are evaluated</param>
        ''' <param name="compIds">List of Compound identifiers for which Physical Properties are to be retrieved. 
        ''' Set compIds to UNDEFINED to denote all Compounds in the component that implements the ICapeThermoCompounds interface.</param>
        ''' <param name="propVals">Physical Property values for the Compounds specified.</param>
        ''' <remarks>The GetTDependentPropList method can be used in order to check which Physical
        ''' Properties are available.
        ''' If the number of requested Physical Properties is P and the number of Compounds is C, the
        ''' propvals array will contain C*P values. The first C will be the values for the first requested
        ''' Physical Property followed by C values for the second Physical Property, and so on.
        ''' Properties are returned in a fixed set of units as specified in section 7.5.3.
        ''' If the compIds argument is set to UNDEFINED this is a request to return property values for
        ''' all compounds in the component that implements the ICapeThermoCompounds interface
        ''' with the compound order the same as that returned by the GetCompoundList method. For
        ''' example, if the interface is implemented by a Property Package component the property
        ''' request with compIds set to UNDEFINED means all compounds in the Property Package
        ''' rather than all compounds in the Material Object passed to the Property package.
        ''' If any Physical Property is not available for one or more Compounds, then undefined values
        ''' must be returned for those combinations and an ECapeThrmPropertyNotAvailable exception
        ''' must be raised. If the exception is raised, the client should check all the values returned to
        ''' determine which is undefined.</remarks>
        Public Sub GetTDependentProperty(ByVal props As Object, ByVal temperature As Double, ByVal compIds As Object, ByRef propVals As Object) Implements ICapeThermoCompounds.GetTDependentProperty
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.GetTDependentProperty(props, temperature, compIds, propVals)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetTDependentProperty", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Returns the list of supported temperature-dependent Physical Properties.
        ''' </summary>
        ''' <returns>The list of Physical Property identifiers for all supported temperature-dependent 
        ''' properties. The standard identifiers are listed in section 7.5.3</returns>
        ''' <remarks>GetTDependentPropList returns identifiers for all the temperature-dependent Physical
        ''' Properties that can be retrieved by the GetTDependentProperty method. If no properties are
        ''' supported UNDEFINED should be returned. The CAPE-OPEN standards do not define a
        ''' minimum list of properties to be made available by a software component that implements
        ''' the ICapeThermoCompounds interface.
        ''' A component that implements the ICapeThermoCompounds interface may return identifiers
        ''' which do not belong to the list defined in section 7.5.3. However, these proprietary identifiers
        ''' may not be understood by most of the clients of this component.</remarks>
        Public Function GetTDependentPropList() As Object Implements ICapeThermoCompounds.GetTDependentPropList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetTDependentPropList()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoCompounds", ex.Source, ex.StackTrace, "GetTDependentPropList", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Remove all stored Physical Property values.
        ''' </summary>
        ''' <remarks>ClearAllProps removes all stored Physical Properties that have been set using the
        ''' SetSinglePhaseProp, SetTwoPhaseProp or SetOverallProp methods. This means that any
        ''' subsequent call to retrieve Physical Properties will result in an exception until new values
        ''' have been stored using one of the Set methods. ClearAllProps does not remove the
        ''' configuration information for a Material, i.e. the list of Compounds and Phases.
        ''' Using the ClearAllProps method results in a Material Object that is in the same state as
        ''' when it was first created. It is an alternative to using the CreateMaterial method but it is
        ''' expected to have a smaller overhead in operating system resources.</remarks>
        Public Sub ClearAllProps() Implements ICapeThermoMaterial.ClearAllProps, Interfaces.IMaterialStream.ClearAllProps
            Me.PropertyPackage.CurrentMaterialStream = Me
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Vapor)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid1)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid2)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Liquid3)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Aqueous)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Solid)
            Me.PropertyPackage.DW_ZerarPhaseProps(PropertyPackages.Phase.Mixture)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Vapor)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid1)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid2)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Liquid3)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Aqueous)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Solid)
            Me.PropertyPackage.DW_ZerarComposicoes(PropertyPackages.Phase.Mixture)
            Me.PropertyPackage.CurrentMaterialStream = Nothing
        End Sub

        ''' <summary>
        ''' Copies all the stored non-constant Physical Properties (which have been set using the SetSinglePhaseProp, 
        ''' SetTwoPhaseProp or SetOverallProp) from the source Material Object to the current instance of the Material Object.</summary>
        ''' <param name="source">Source Material Object from which stored properties will be copied.</param>
        ''' <remarks>Before using this method, the Material Object must have been configured with the same
        ''' exact list of Compounds and Phases as the source one. Otherwise, calling the method will
        ''' raise an exception. There are two ways to perform the configuration: through the PME
        ''' proprietary mechanisms and with CreateMaterial. Calling CreateMaterial on a Material
        ''' Object S and subsequently calling CopyFromMaterial(S) on the newly created Material
        ''' Object N is equivalent to the deprecated method ICapeMaterialObject.Duplicate.
        ''' The method is intended to be used by a client, for example a Unit Operation that needs a
        ''' Material Object to have the same state as one of the Material Objects it has been connected
        ''' to. One example is the representation of an internal stream in a distillation column.
        ''' If the Material Object supports the Petroleum Fractions Interface [7] the petroleum fraction
        ''' properties are also copied from the source Material Object to the current instance of the
        ''' Material Object.</remarks>
        Public Sub CopyFromMaterial(ByRef source As Object) Implements ICapeThermoMaterial.CopyFromMaterial

            If Not Marshal.IsComObject(source) Then
                Me.Assign(source)
                Me.AssignProps(source)
            Else
                'get ID
                Dim id As String = CType(source, ICapeIdentification).ComponentDescription
                Dim myms As MaterialStream = Me.FlowSheet.SimulationObjects(id)
                'proceed with copy
                Me.Assign(myms)
                Me.AssignProps(myms)
            End If

        End Sub

        ''' <summary>
        ''' Creates a Material Object with the same configuration as the current Material Object.
        ''' </summary>
        ''' <returns>The Material Object created does not contain any non-constant Physical Property value but
        ''' has the same configuration (Compounds and Phases) as the current Material Object. These
        ''' Physical Property values must be set using SetSinglePhaseProp, SetTwoPhaseProp or
        ''' SetOverallProp. Any attempt to retrieve Physical Property values before they have been set
        ''' will result in an exception.</returns>
        ''' <remarks></remarks>
        Public Function CreateMaterial() As Object Implements ICapeThermoMaterial.CreateMaterial
            Dim mat As Streams.MaterialStream = Me.Clone
            mat.SetFlowsheet(Me.FlowSheet)
            mat.ClearAllProps()
            Return mat
        End Function

        ''' <summary>
        ''' Retrieves non-constant Physical Property values for the overall mixture.
        ''' </summary>
        ''' <param name="property">The identifier of the Physical Property for which values are requested. 
        ''' This must be one of the single-phase Physical Properties or derivatives that can be stored for 
        ''' the overall mixture. The standard identifiers are listed in sections 7.5.5 and 7.6.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties per 
        ''' unit mass or “Mole” for molar properties. Use UNDEFINED as a place holder for a Physical Property 
        ''' for which basis does not apply. See section 7.5.5 for details.</param>
        ''' <param name="results">Results vector containing Physical Property value(s) in SI units.</param>
        ''' <remarks>The Physical Property values returned by GetOverallProp refer to the overall mixture. These
        ''' values are set by calling the SetOverallProp method. Overall mixture Physical Properties are
        ''' not calculated by components that implement the ICapeThermoMaterial interface. The
        ''' property values are only used as input specifications for the CalcEquilibrium method of a
        ''' component that implements the ICapeThermoEquilibriumRoutine interface.
        ''' It is expected that this method will normally be able to provide Physical Property values on
        ''' any basis, i.e. it should be able to convert values from the basis on which they are stored to
        ''' the basis requested. This operation will not always be possible. For example, if the
        ''' molecular weight is not known for one or more Compounds, it is not possible to convert
        ''' between a mass basis and a molar basis.
        ''' Although the result of some calls to GetOverallProp will be a single value, the return type is
        ''' CapeArrayDouble and the method must always return an array even if it contains only a
        ''' single element.</remarks>
        Public Sub GetOverallProp(ByVal [property] As String, ByVal basis As String, ByRef results As Object) Implements ICapeThermoMaterial.GetOverallProp
            Try
                GetSinglePhaseProp([property], "overall", basis, results)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetOverallProp", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Retrieves temperature, pressure and composition for the overall mixture.
        ''' </summary>
        ''' <param name="temperature">Temperature (in K)</param>
        ''' <param name="pressure">Pressure (in Pa)</param>
        ''' <param name="composition">Composition (mole fractions)</param>
        ''' <remarks>This method is provided to make it easier for developers to make efficient use of the CAPEOPEN interfaces. 
        ''' It returns the most frequently requested information from a Material Object in a single call. There is no choice 
        ''' of basis in this method. The composition is always returned as mole fractions.</remarks>
        Public Sub GetOverallTPFraction(ByRef temperature As Double, ByRef pressure As Double, ByRef composition As Object) Implements ICapeThermoMaterial.GetOverallTPFraction
            Try
                Me.GetTPFraction("overall", temperature, pressure, composition)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetOverallTPFraction", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Returns Phase labels for the Phases that are currently present in the Material Object.
        ''' </summary>
        ''' <param name="phaseLabels">The list of Phase labels (identifiers – names) for the Phases present 
        ''' in the Material Object. The Phase labels in the Material Object must be a subset of the labels 
        ''' returned by the GetPhaseList method of the ICapeThermoPhases interface.</param>
        ''' <param name="phaseStatus"></param>
        ''' <remarks>A Phase is ‘present’ in a Material Object (or other component that implements the
        ''' ICapeThermoMaterial interface) if it has been explicitly made present by calling the
        ''' SetPresentPhases method or if any properties have been set by calling the
        ''' SetSinglePhaseProp or SetTwoPhaseProp methods. Even if a Phase is present, it does not
        ''' necessarily imply that any Physical Properties are actually set unless the phaseStatus is
        ''' Cape_AtEquilibrium or Cape_Estimates (see below). Note that calling the SetPresentPhases
        ''' method of the ICapeThermoMaterial interface will cause any phases not specified in its
        ''' phaseLabels list to not present, even if previously present as a result of a SetSingle-
        ''' PhaseProp or SetTwoPhaseProp call.
        ''' If no Phases are present, UNDEFINED should be returned for both the phaseLabels and
        ''' phaseStatus arguments.
        ''' The phaseStatus argument contains as many entries as there are Phase labels. The valid
        ''' settings are listed in the following table:
        ''' 
        ''' Identifier                    Meaning
        ''' Cape_UnknownPhaseStatus       This is the normal setting when a Phase is specified as being available for an Equilibrium Calculation.
        ''' Cape_AtEquilibrium            The Phase has been set as present as a result of an Equilibrium Calculation.
        ''' Cape_Estimates                Estimates of the equilibrium state have been set in the Material Object.
        ''' 
        ''' All the Phases with a status of Cape_AtEquilibrium have values of temperature, pressure,
        ''' composition and Phase fraction set that correspond to an equilibrium state, i.e. equal
        ''' temperature, pressure and fugacities of each Compound. Phases with a Cape_Estimates
        ''' status have values of temperature, pressure, composition and Phase fraction set in the
        ''' Material Object. These values are available for use by an Equilibrium Calculator component
        ''' to initialise an Equilibrium Calculation. The stored values are available but there is no
        ''' guarantee that they will be used.
        ''' GetPresentPhases is intended to be used in several contexts.
        ''' A Property Package, Property Calculator or other PMC may use this method to check
        ''' whether a phase is present in the Material Object prior to requesting and/or calculating
        ''' some properties.
        ''' An Equilibrium Calculator component will use this method to obtain the list of phases to
        ''' consider in an equilibrium calculation or when checking an equilibrium specification
        ''' (see below for more details).
        ''' The method will be used by the PME or PMC to obtain the list of phases present as the
        ''' result of an equilibrium calculation (see below for more details).
        ''' A Unit Operation (or other PMC) will use this method to get the list of phases present at
        ''' an inlet port or during its calculations.
        ''' In the context of Equilibrium Calculations the GetPresentPhases method is intended to work
        ''' in conjunction with the SetPresentPhases method. Together these methods provide a means
        ''' of communication between a PME (or another client) and an Equilibrium Calculator (or
        ''' other component that implements the ICapeThermoEquilibriumRoutine interface). The
        ''' following sequence of operations is envisaged.
        ''' 
        ''' 1. Prior to requesting an Equilibrium Calculation, a PME will use the SetPresentPhases
        ''' method to define a list of Phases that may be considered in the Equilibrium
        ''' Calculation. Typically, this is necessary because an Equilibrium Calculator may be
        ''' capable of handling a large number of Phases but for a particular application, it may
        ''' be known that only certain Phases will be involved. For example, if the complete
        ''' Phase list contains Phases with the following labels (with the obvious interpretation):
        ''' vapour, hydrocarbonLiquid and aqueousLiquid and it is required to model a liquid
        ''' decanter, the present Phases might be set to hydrocarbonLiquid and aqueousLiquid.
        ''' 
        ''' 2. The GetPresentPhases method is then used by the CalcEquilibrium method of the
        ''' ICapeThermoEquilibriumRoutine interface to obtain the list of Phase labels corresponding
        ''' to the Phases that may be present at equilibrium.
        ''' 
        ''' 3. The Equilibrium Calculation determines which Phases actually co-exist at
        ''' equilibrium. This list of Phases may be a sub-set of the Phases considered because
        ''' some Phases may not be present at the prevailing conditions. For example, if the
        ''' amount of water is sufficiently small the aqueousLiquid Phase in the above example
        ''' may not exist because all the water dissolves in the hydrocarbonLiquid Phase.
        ''' 
        ''' 4. The CalcEquilibrium method uses the SetPresentPhases method to indicate the
        ''' Phases present following the equilibrium calculation (and sets the phase properties).
        ''' 
        ''' 5. The PME uses the GetPresentPhases method to find out the Phases present following
        ''' the calculation and it can then use the GetSinglePhaseProp or GetTPFraction
        ''' methods to get the Phase properties.</remarks>
        Public Sub GetPresentPhases(ByRef phaseLabels As Object, ByRef phaseStatus As Object) Implements ICapeThermoMaterial.GetPresentPhases

            Dim pl As New ArrayList, stat As New ArrayList

            If AtEquilibrium Then
                For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                    If pi.PhaseLabel <> "Disabled" Then
                        If Me.Phases(pi.DWPhaseIndex).Properties.molarfraction.HasValue Then
                            pl.Add(pi.PhaseLabel)
                            stat.Add(CapeOpen.CapePhaseStatus.CAPE_ATEQUILIBRIUM)
                        End If
                    End If
                Next
            Else
                For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                    If pi.PhaseLabel <> "Disabled" Then
                        pl.Add(pi.PhaseLabel)
                        stat.Add(CapeOpen.CapePhaseStatus.CAPE_UNKNOWNPHASESTATUS)
                    End If
                Next
            End If

            Dim arr(pl.Count - 1) As String
            Array.Copy(pl.ToArray, arr, pl.Count)
            phaseLabels = arr

            Dim arr2(stat.Count - 1) As CapeOpen.CapePhaseStatus
            Array.Copy(stat.ToArray, arr2, stat.Count)
            phaseStatus = arr2

        End Sub

        ''' <summary>
        ''' Retrieves single-phase non-constant Physical Property values for a mixture.
        ''' </summary>
        ''' <param name="property">The identifier of the Physical Property for which values are requested. 
        ''' This must be one of the single-phase Physical Properties or derivatives. The standard identifiers 
        ''' are listed in sections 7.5.5 and 7.6.</param>
        ''' <param name="phaseLabel">Phase label of the Phase for which the Physical Property is required. 
        ''' The Phase label must be one of the identifiers returned by the GetPresentPhases method of this interface.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties per unit mass or “Mole” 
        ''' for molar properties. Use UNDEFINED as a place holder for a Physical Property for which basis does not apply. See section 
        ''' 7.5.5 for details.</param>
        ''' <param name="results">Results vector (CapeArrayDouble) containing Physical Property value(s) in SI units or CapeInterface 
        ''' (see notes).</param>
        ''' <remarks>The results argument returned by GetSinglePhaseProp is either a CapeArrayDouble that
        ''' contains one or more numerical values, e.g. temperature, or a CapeInterface that may be
        ''' used to retrieve single-phase Physical Properties described by a more complex data
        ''' structure, e.g. distributed properties.
        ''' It is required that a component that implements the ICapeThermoMaterial interface will
        ''' always support the following properties: temperature, pressure, fraction, phaseFraction,
        ''' flow, totalFlow.
        ''' Although the result of some calls to GetSinglePhaseProp may be a single numerical value,
        ''' the return type for numerical values is CapeArrayDouble and in such a case the method must
        ''' return an array even if it contains only a single element.
        ''' A Phase is ‘present’ in a Material if its identifier is returned by the GetPresentPhases
        ''' method. An exception is raised by the GetSinglePhaseProp method if the Phase specified is
        ''' not present. Even if a Phase is present, this does not necessarily mean that any Physical
        ''' Properties are available.
        ''' The Physical Property values returned by GetSinglePhaseProp refer to a single Phase. These
        ''' values may be set by the SetSinglePhaseProp method, which may be called directly, or by
        ''' ICapeThermoPropertyRoutine interface or the CalcEquilibrium method of the
        ''' ICapeThermoEquilibriumRoutine interface. Note: Physical Properties that depend on more
        ''' than one Phase, for example surface tension or K-values, are returned by the
        ''' GetTwoPhaseProp method.
        ''' It is expected that this method will normally be able to provide Physical Property values on
        ''' any basis, i.e. it should be able to convert values from the basis on which they are stored to
        ''' the basis requested. This operation will not always be possible. For example, if the
        ''' molecular weight is not known for one or more Compounds, it is not possible to convert
        ''' from mass fractions or mass flows to mole fractions or molar flows.</remarks>
        Public Sub GetSinglePhaseProp(ByVal [property] As String, ByVal phaseLabel As String, ByVal basis As String, ByRef results As Object) Implements ICapeThermoMaterial.GetSinglePhaseProp

            Me.PropertyPackage.CurrentMaterialStream = Me

            Dim res As New ArrayList
            Dim comps As New ArrayList
            If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                Dim complist As Object = Nothing
                Me.PropertyPackage.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                For Each s As String In complist
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        If kvp.Value = s Then
                            comps.Add(kvp.Key)
                            Exit For
                        End If
                    Next
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next
            End If
            Dim f As Integer = -1
            Dim phs As PropertyPackages.Phase
            Select Case phaseLabel.ToLower
                Case "overall"
                    f = 0
                    phs = PropertyPackages.Phase.Mixture
                Case Else
                    For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                        If phaseLabel = pi.PhaseLabel Then
                            f = pi.DWPhaseIndex
                            phs = pi.DWPhaseID
                            Exit For
                        End If
                    Next
            End Select

            If f = -1 Then
                Dim ex As New CapeOpen.CapeInvalidArgumentException("Invalid Phase ID", New ArgumentException, 0)
                Dim hcode As Integer = 0
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetOverallTPFraction", hcode)
            End If

            Select Case [property].ToLower
                Case "compressibilityfactor"
                    res.Add(Me.Phases(f).Properties.compressibilityFactor.GetValueOrDefault)
                Case "isothermalcompressibility"
                    res.Add(Me.Phases(f).Properties.isothermal_compressibility.GetValueOrDefault)
                Case "bulkmodulus"
                    res.Add(Me.Phases(f).Properties.bulk_modulus.GetValueOrDefault)
                Case "joulethomsoncoefficient"
                    res.Add(Me.Phases(f).Properties.jouleThomsonCoefficient.GetValueOrDefault)
                Case "speedofsound"
                    res.Add(Me.Phases(f).Properties.speedOfSound.GetValueOrDefault)
                Case "heatofvaporization"
                Case "heatcapacity", "heatcapacitycp"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.heatCapacityCp.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.heatCapacityCp.GetValueOrDefault * 1000)
                    End Select
                Case "heatcapacitycv"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.heatCapacityCv.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.heatCapacityCv.GetValueOrDefault * 1000)
                    End Select
                Case "idealgasheatcapacity"
                    If f = 1 Then
                        res.Add(Me.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Liquid, Me.Phases(0).Properties.temperature * 1000))
                    Else
                        res.Add(Me.PropertyPackage.AUX_CPm(PropertyPackages.Phase.Vapor, Me.Phases(0).Properties.temperature * 1000))
                    End If
                Case "idealgasenthalpy"
                    If f = 1 Then
                        res.Add(Me.PropertyPackage.RET_Hid(298.15, Me.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Liquid))
                    Else
                        res.Add(Me.PropertyPackage.RET_Hid(298.15, Me.Phases(0).Properties.temperature.GetValueOrDefault * 1000, PropertyPackages.Phase.Vapor))
                    End If
                Case "excessenthalpy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.excessEnthalpy.GetValueOrDefault * 1000)
                    End Select
                Case "excessentropy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.excessEntropy.GetValueOrDefault * Me.PropertyPackage.AUX_MMM(phs))
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.excessEntropy.GetValueOrDefault * 1000)
                    End Select
                Case "viscosity"
                    res.Add(Me.Phases(f).Properties.viscosity.GetValueOrDefault)
                Case "thermalconductivity"
                    res.Add(Me.Phases(f).Properties.thermalConductivity.GetValueOrDefault)
                Case "fugacity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault * Me.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault * Me.Phases(0).Properties.pressure.GetValueOrDefault)
                    Next
                Case "activity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).ActivityCoeff.GetValueOrDefault * Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                    Next
                Case "fugacitycoefficient"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault)
                    Next
                Case "activitycoefficient"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).ActivityCoeff.GetValueOrDefault)
                    Next
                Case "logfugacitycoefficient"
                    For Each c As String In comps
                        res.Add(Math.Log(Me.Phases(f).Compounds(c).FugacityCoeff.GetValueOrDefault))
                    Next
                Case "volume"
                    If Not GlobalSettings.Settings.CAPEOPENMode Then
                        res.Add(Me.Phases(f).Properties.molecularWeight / Me.Phases(f).Properties.density)
                    Else
                        res.Add(Me.Phases(f).Properties.molecularWeight / Me.Phases(f).Properties.density / 1000)
                    End If
                Case "density"
                    res.Add(Me.Phases(f).Properties.density.GetValueOrDefault)
                Case "enthalpy", "enthalpynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_enthalpy.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.enthalpy.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.enthalpy.GetValueOrDefault * 1000)
                    End Select
                Case "entropy", "entropynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_entropy.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                    End Select
                Case "enthalpyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_enthalpyF.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.enthalpyF.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.enthalpyF.GetValueOrDefault * 1000)
                    End Select
                Case "entropyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                            If val = 0.0# Then
                                res.Add(Me.Phases(f).Properties.molar_entropyF.GetValueOrDefault)
                            Else
                                res.Add(Me.Phases(f).Properties.entropyF.GetValueOrDefault * val)
                            End If
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.entropy.GetValueOrDefault * 1000)
                    End Select
                Case "internalenergy"
                    If basis.Equals("mole") Then
                        Dim val As Double = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                        If val = 0.0# Then
                            res.Add(Me.Phases(f).Properties.molar_internal_energy.GetValueOrDefault)
                        Else
                            res.Add(Me.Phases(f).Properties.internal_energy.GetValueOrDefault * val)
                        End If
                    Else
                        res.Add(Me.Phases(f).Properties.internal_energy.GetValueOrDefault)
                    End If
                Case "gibbsenergy"
                    If basis.Equals("mole") Then
                        Dim val As Double = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                        If val = 0.0# Then
                            res.Add(Me.Phases(f).Properties.molar_gibbs_free_energy.GetValueOrDefault)
                        Else
                            res.Add(Me.Phases(f).Properties.gibbs_free_energy.GetValueOrDefault * val)
                        End If
                    Else
                        res.Add(Me.Phases(f).Properties.gibbs_free_energy.GetValueOrDefault)
                    End If
                Case "helmholtzenergy"
                    If basis.Equals("mole") Then
                        Dim val As Double = Me.Phases(f).Properties.molecularWeight.GetValueOrDefault
                        If val = 0.0# Then
                            res.Add(Me.Phases(f).Properties.molar_helmholtz_energy.GetValueOrDefault)
                        Else
                            res.Add(Me.Phases(f).Properties.helmholtz_energy.GetValueOrDefault * val)
                        End If
                    Else
                        res.Add(Me.Phases(f).Properties.helmholtz_energy.GetValueOrDefault)
                    End If
                Case "moles"
                    res.Add(Me.Phases(f).Properties.molarflow.GetValueOrDefault)
                Case "mass"
                    res.Add(Me.Phases(f).Properties.massflow.GetValueOrDefault)
                Case "molecularweight"
                    res.Add(Me.Phases(f).Properties.molecularWeight.GetValueOrDefault)
                Case "temperature"
                    res.Add(Me.Phases(0).Properties.temperature.GetValueOrDefault)
                Case "pressure"
                    res.Add(Me.Phases(0).Properties.pressure.GetValueOrDefault)
                Case "flow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault)
                            Next
                        Case "Mass", "mass"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MassFlow.GetValueOrDefault)
                            Next
                    End Select
                Case "fraction", "massfraction", "molarfraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                            Next
                        Case "Mass", "mass"
                            For Each c As String In comps
                                res.Add(Me.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                            Next
                        Case ""
                            If [property].ToLower.Contains("mole") Then
                                For Each c As String In comps
                                    res.Add(Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                                Next
                            ElseIf [property].ToLower.Contains("mass") Then
                                For Each c As String In comps
                                    res.Add(Me.Phases(f).Compounds(c).MassFraction.GetValueOrDefault)
                                Next
                            End If
                    End Select
                Case "concentration"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MassFlow.GetValueOrDefault / Me.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                    Next
                Case "molarity"
                    For Each c As String In comps
                        res.Add(Me.Phases(f).Compounds(c).MolarFlow.GetValueOrDefault / Me.Phases(f).Properties.volumetric_flow.GetValueOrDefault)
                    Next
                Case "phasefraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.molarfraction.GetValueOrDefault)
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.massfraction.GetValueOrDefault)
                    End Select
                Case "totalflow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            res.Add(Me.Phases(f).Properties.molarflow.GetValueOrDefault)
                        Case "Mass", "mass"
                            res.Add(Me.Phases(f).Properties.massflow.GetValueOrDefault)
                    End Select
                Case Else
                    Dim ex = New CapeOpen.CapeThrmPropertyNotAvailableException
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetSinglePhaseProp", hcode)
            End Select
            Dim arr(res.Count - 1) As Double
            Array.Copy(res.ToArray, arr, res.Count)
            results = arr
        End Sub

        Public Function GetSinglePhasePropDefaultUnits(ByVal [property] As String, ByVal basis As String) As String

            Dim units As String = ""

            Select Case [property].ToLower
                Case "isothermalcompressibility"
                    units = "1/Pa"
                Case "bulkmodulus"
                    units = "Pa"
                Case "joulethomsoncoefficient"
                    units = "K/Pa"
                Case "speedofsound"
                    units = "m/s"
                Case "heatofvaporization"
                    units = "kJ/mol"
                Case "heatcapacity", "heatcapacitycp", "heatcapacitycv", "idealgasheatcapacity", "idealgasenthalpy",
                     "excessenthalpy", "excessentropy", "enthalpy", "enthalpynf", "entropy", "entropynf", "enthalpyf",
                     "entropyf", "internalenergy", "gibbsenergy", "helmholtzenergy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            units = "J/mol"
                        Case "Mass", "mass"
                            units = "J/kg"
                    End Select
                Case "viscosity"
                    units = "Pa.s"
                Case "thermalconductivity"
                    units = "W/[m.K]"
                Case "fugacity"
                    units = "Pa"
                Case "activity"
                    units = "Pa"
                Case "volume"
                    units = "m3/mol"
                Case "density"
                    units = "kg/m3"
                Case "moles"
                    units = "mol"
                Case "mass"
                    units = "kg"
                Case "molecularweight"
                    units = "kg/kmol"
                Case "temperature"
                    units = "K"
                Case "pressure"
                    units = "Pa"
                Case "flow", "totalflow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                        Case "Mass", "mass"
                    End Select
                Case "concentration"
                    units = "kg/m3"
                Case "molarity"
                    units = "mol/m3"
                Case Else
                    units = ""
            End Select

            Return units

        End Function


        ''' <summary>
        ''' Retrieves temperature, pressure and composition for a Phase.
        ''' </summary>
        ''' <param name="phaseLabel">Phase label of the Phase for which the property is required. The Phase label 
        ''' must be one of the identifiers returned by the GetPresentPhases method of this interface.</param>
        ''' <param name="temperature">Temperature (in K)</param>
        ''' <param name="pressure">Pressure (in Pa)</param>
        ''' <param name="composition">Composition (mole fractions)</param>
        ''' <remarks>This method is provided to make it easier for developers to make efficient use of the CAPEOPEN
        ''' interfaces. It returns the most frequently requested information from a Material
        ''' Object in a single call.
        ''' There is no choice of basis in this method. The composition is always returned as mole
        ''' fractions.
        ''' To get the equivalent information for the overall mixture the GetOverallTPFraction method
        ''' of the ICapeThermoMaterial interface should be used.</remarks>
        Public Sub GetTPFraction(ByVal phaseLabel As String, ByRef temperature As Double, ByRef pressure As Double, ByRef composition As Object) Implements ICapeThermoMaterial.GetTPFraction

            If Me.Phases(0).Properties.temperature Is Nothing Or Me.Phases(0).Properties.pressure Is Nothing Then
                Throw New CapeBadArgumentException("Temperature and/or Pressure not set.", Nothing, 0)
            End If

            temperature = Me.Phases(0).Properties.temperature
            pressure = Me.Phases(0).Properties.pressure

            Dim arr As New ArrayList
            Dim comps As New ArrayList
            If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                Dim complist As Object = Nothing
                Me.PropertyPackage.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                For Each s As String In complist
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        If kvp.Value = s Then
                            comps.Add(kvp.Key)
                            Exit For
                        End If
                    Next
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next
            End If
            Select Case phaseLabel.ToLower
                Case "overall"
                    For Each c As String In comps
                        arr.Add(Me.Phases(0).Compounds(c).MoleFraction.GetValueOrDefault)
                    Next
                Case Else
                    For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                        If phaseLabel = pi.PhaseLabel Then
                            For Each c As String In comps
                                arr.Add(Me.Phases(pi.DWPhaseIndex).Compounds(c).MoleFraction.GetValueOrDefault)
                            Next
                            Exit For
                        End If
                    Next
            End Select
            Dim arr2(arr.Count - 1) As Double
            Array.Copy(arr.ToArray, arr2, arr.Count)
            composition = arr2
        End Sub

        ''' <summary>
        ''' Retrieves two-phase non-constant Physical Property values for a mixture.
        ''' </summary>
        ''' <param name="property">The identifier of the property for which values are requested. This must 
        ''' be one of the two-phase Physical Properties or Physical Property derivatives listed in sections 
        ''' 7.5.6 and 7.6.</param>
        ''' <param name="phaseLabels">List of Phase labels of the Phases for which the property is required. 
        ''' The Phase labels must be two of the identifiers returned by the GetPhaseList method of the Material 
        ''' Object.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties per unit 
        ''' mass or “Mole” for molar properties. Use UNDEFINED as a place holder for a Physical Property for which 
        ''' basis does not apply. See section 7.5.5 for details.</param>
        ''' <param name="results">Results vector (CapeArrayDouble) containing property value(s) in SI units or 
        ''' CapeInterface (see notes).</param>
        ''' <remarks>The results argument returned by GetTwoPhaseProp is either a CapeArrayDouble that
        ''' contains one or more numerical values, e.g. kvalues, or a CapeInterface that may be used to
        ''' retrieve 2-phase Physical Properties described by a more complex data structure, e.g.
        ''' distributed Physical Properties.
        ''' Although the result of some calls to GetTwoPhaseProp may be a single numerical value, the
        ''' return type for numerical values is CapeArrayDouble and in such a case the method must
        ''' return an array even if it contains only a single element.
        ''' A Phase is ‘present’ in a Material if its identifier is returned by the GetPresentPhases
        ''' method. An exception is raised by the GetTwoPhaseProp method if any of the Phases
        ''' specified is not present. Even if all Phases are present, this does not necessarily mean that
        ''' any Physical Properties are available.
        ''' The Physical Property values returned by GetTwoPhaseProp depend on two Phases, for
        ''' example surface tension or K-values. These values may be set by the SetTwoPhaseProp
        ''' method that may be called directly, or by other methods such as the CalcTwoPhaseProp
        ''' method of the ICapeThermoPropertyRoutine interface, or the CalcEquilibrium method of the
        ''' ICapeThermoEquilibriumRoutine interface. Note: Physical Properties that depend on a
        ''' single Phase are returned by the GetSinglePhaseProp method.
        ''' It is expected that this method will normally be able to provide Physical Property values on
        ''' any basis, i.e. it should be able to convert values from the basis on which they are stored to
        ''' the basis requested. This operation will not always be possible. For example, if the
        ''' molecular weight is not known for one or more Compounds, it is not possible to convert
        ''' between a mass basis and a molar basis.
        ''' If a composition derivative is requested this means that the derivatives are returned for both
        ''' Phases in the order in which the Phase labels are specified. The number of values returned
        ''' for a composition derivative will depend on the dimensionality of the property. For example,
        ''' if there are N Compounds then the results vector for the surface tension derivative will
        ''' contain N composition derivative values for the first Phase, followed by N composition
        ''' derivative values for the second Phase. For K-value derivative there will be N2 derivative
        ''' values for the first phase followed by N2 values for the second phase in the order defined in
        ''' 7.6.2.</remarks>
        Public Sub GetTwoPhaseProp(ByVal [property] As String, ByVal phaseLabels As Object, ByVal basis As String, ByRef results As Object) Implements ICapeThermoMaterial.GetTwoPhaseProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            Dim res As New ArrayList
            Dim f As Integer = 0
            Dim comps As New ArrayList
            If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                Dim complist As Object = Nothing
                Me.PropertyPackage.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                For Each s As String In complist
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        If kvp.Value = s Then
                            comps.Add(kvp.Key)
                            Exit For
                        End If
                    Next
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next
            End If
            Select Case [property].ToLower
                Case "kvalue"
                    For Each c As String In comps
                        res.Add(Me.Phases(0).Compounds(c).Kvalue)
                    Next
                Case "logkvalue"
                    For Each c As String In comps
                        res.Add(Me.Phases(0).Compounds(c).lnKvalue)
                    Next
                Case "surfacetension"
                    res.Add(Me.Phases(0).Properties.surfaceTension.GetValueOrDefault)
                Case Else
                    Throw New CapeOpen.CapeThrmPropertyNotAvailableException
            End Select
            Dim arr2(res.Count - 1) As Double
            Array.Copy(res.ToArray, arr2, res.Count)
            results = arr2
        End Sub

        ''' <summary>
        ''' Sets non-constant property values for the overall mixture.
        ''' </summary>
        ''' <param name="property">The identifier of the property for which values are set.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties 
        ''' per unit mass or “Mole” for molar properties. Use UNDEFINED as a place holder for a Physical 
        ''' Property for which basis does not apply.</param>
        ''' <param name="values">Values to set for the property.s</param>
        ''' <remarks>The property values set by SetOverallProp refer to the overall mixture. These values are
        ''' retrieved by calling the GetOverallProp method. Overall mixture properties are not
        ''' calculated by components that implement the ICapeThermoMaterial interface. The property
        ''' values are only used as input specifications for the CalcEquilibrium method of a component
        ''' that implements the ICapeThermoEquilibriumRoutine interface.
        ''' Although some properties set by calls to SetOverallProp will have a single value, the type of
        ''' argument values is CapeArrayDouble and the method must always be called with values as
        ''' an array even if it contains only a single element.</remarks>
        Public Sub SetOverallProp(ByVal [property] As String, ByVal basis As String, ByVal values As Object) Implements ICapeThermoMaterial.SetOverallProp
            Me.SetSinglePhaseProp([property], "Overall", basis, values)
        End Sub

        ''' <summary>
        ''' Allows the PME or the Property Package to specify the list of Phases that are currently present.
        ''' </summary>
        ''' <param name="phaseLabels">The list of Phase labels for the Phases present.
        ''' The Phase labels in the Material Object must be a
        ''' subset of the labels returned by the GetPhaseList
        ''' method of the ICapeThermoPhases interface.</param>
        ''' <param name="phaseStatus"></param>
        ''' <remarks>SetPresentPhases is intended to be used in the following ways:
        ''' * To restrict an Equilibrium Calculation (using the CalcEquilibrium method of a
        ''' component that implements the ICapeThermoEquilibriumRoutine interface) to a subset
        ''' of the Phases supported by the Property Package component;
        ''' * When the component that implements the ICapeThermoEquilibriumRoutine interface
        ''' needs to specify which Phases are present in a Material Object after an Equilibrium
        ''' Calculation has been performed.
        ''' * In the context of dynamic simulations to specify the state of a Material Object that is an
        ''' output of a unit operation. This is the equivalent of calculating equilibrium in steadystate
        ''' simulations.
        ''' If a Phase in the list is already present, its Physical Properties are unchanged by the action of
        ''' this method. Any Phases not in the list when SetPresentPhases is called are removed from
        ''' the Material Object. This means that any Physical Property values that may have been stored
        ''' on the removed Phases are no longer available (i.e. a call to GetSinglePhaseProp or
        ''' GetTwoPhaseProp including this Phase will return an exception). A call to the
        ''' GetPresentPhases method of the Material Object will return the same list as specified by
        ''' SetPresentPhases.
        ''' The phaseStatus argument must contain as many entries as there are Phase labels. The valid
        ''' settings are listed in the following table:
        ''' 
        ''' Identifier                 Meaning
        ''' Cape_UnknownPhaseStatus    This is the normal setting when a Phase is specified as being available for an Equilibrium Calculation.
        ''' Cape_AtEquilibrium         The Phase has been set as present as a result of an Equilibrium Calculation.
        ''' Cape_Estimates             Estimates of the equilibrium state have been set in the Material Object.
        ''' 
        ''' All the Phases with a status of Cape_AtEquilibrium must have properties that correspond to
        ''' an equilibrium state, i.e. equal temperature, pressure and fugacities of each Compound (this
        ''' does not imply that the fugacities are set as a result of the Equilibrium Calculation). The
        ''' Cape_AtEquilibrium status should be set by the CalcEquilibrium method of a component
        ''' that implements the ICapeThermoEquilibriumRoutine interface following a successful
        ''' Equilibrium Calculation. If the temperature, pressure or composition of an equilibrium Phase
        ''' is changed, the Material Object implementation is responsible for resetting the status of the
        ''' Phase to Cape_UnknownPhaseStatus. Other property values stored for that Phase should not
        ''' be affected.
        ''' Phases with an Estimates status must have values of temperature, pressure, composition and
        ''' phase fraction set in the Material Object. These values are available for use by an
        ''' Equilibrium Calculator component to initialise an Equilibrium Calculation. The stored
        ''' values are available but there is no guarantee that they will be used.</remarks>
        Public Sub SetPresentPhases(ByVal phaseLabels As Object, ByVal phaseStatus As Object) Implements ICapeThermoMaterial.SetPresentPhases
            'do nothing, done automatically by CalcEquilibrium
        End Sub

        ''' <summary>
        ''' Sets single-phase non-constant property values for a mixture.
        ''' </summary>
        ''' <param name="property">The identifier of the property for which values are set. This must be 
        ''' one of the single-phase properties or derivatives. The standard identifiers are listed in 
        ''' sections 7.5.5 and 7.6.</param>
        ''' <param name="phaseLabel">Phase label of the Phase for which the property is set. The phase 
        ''' label must be one of the strings returned by the GetPhaseList method of the ICapeThermoPhases 
        ''' interface.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties 
        ''' per unit mass or “Mole” for molar properties. Use UNDEFINED as a place holder for a Physical 
        ''' Property for which basis does not apply. See section 7.5.5 for details.</param>
        ''' <param name="values">Values to set for the property (CapeArrayDouble) or CapeInterface (see notes).</param>
        ''' <remarks>The values argument of SetSinglePhaseProp is either a CapeArrayDouble that contains one
        ''' or more numerical values to be set for a property, e.g. temperature, or a CapeInterface that
        ''' may be used to set single-phase properties described by a more complex data structure, e.g.
        ''' distributed properties.
        ''' It is required that a component that implements the ICapeThermoMaterial interface will
        ''' always support the following properties: temperature, pressure, fraction, phaseFraction,
        ''' flow, totalFlow.
        ''' Although some properties set by calls to SetSinglePhaseProp will have a single numerical
        ''' value, the type of the values argument for numerical values is CapeArrayDouble and in such
        ''' a case the method must be called with values containing an array even if it contains only a
        ''' single element.
        ''' The property values set by SetSinglePhaseProp refer to a single Phase. Properties that depend
        ''' on more than one Phase, for example surface tension or K-values, are set by the
        ''' SetTwoPhaseProp method of the ICapeThermoMaterial Interface.
        ''' To set a property using SetSinglePhaseProp, a phaseLabel identifier should be passed that is
        ''' supported by the Property Package or Material Object, i.e. one that appears in the list
        ''' returned by the GetPhaseList method of the ICapeThermoPhases interface. Setting such a
        ''' property should cause the phase to be present on the Material Object, as if it were specified
        ''' in a call to SetPresentPhases with status Cape_UnknownPhaseStatus. The SetPresentPhases
        ''' method of this interface does not need to be called before calling SetSinglePhaseProp.</remarks>
        Public Sub SetSinglePhaseProp(ByVal [property] As String, ByVal phaseLabel As String, ByVal basis As String, ByVal values As Object) Implements ICapeThermoMaterial.SetSinglePhaseProp

            Dim comps As New ArrayList
            If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                Dim complist As Object = Nothing
                Me.PropertyPackage.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                For Each s As String In complist
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        If kvp.Value = s Then
                            comps.Add(kvp.Key)
                            Exit For
                        End If
                    Next
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next
            End If
            Dim f As Integer = -1
            Dim phs As PropertyPackages.Phase
            Select Case phaseLabel.ToLower
                Case "overall"
                    f = 0
                    phs = PropertyPackages.Phase.Mixture
                Case Else
                    For Each pi As PhaseInfo In Me.PropertyPackage.PhaseMappings.Values
                        If phaseLabel = pi.PhaseLabel Then
                            f = pi.DWPhaseIndex
                            phs = pi.DWPhaseID
                            Exit For
                        End If
                    Next
            End Select

            If f = -1 Then
                Dim ex As New CapeOpen.CapeInvalidArgumentException("Invalid Phase ID", New ArgumentException, 0)
                Dim hcode As Integer = 0
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetOverallTPFraction", hcode)
            End If

            Select Case [property].ToLower
                Case "compressibilityfactor"
                    Me.Phases(f).Properties.compressibilityFactor = values(0)
                Case "isothermalcompressibility"
                    Me.Phases(f).Properties.isothermal_compressibility = values(0)
                Case "bulkmodulus"
                    Me.Phases(f).Properties.bulk_modulus = values(0)
                Case "joulethomsoncoefficient"
                    Me.Phases(f).Properties.jouleThomsonCoefficient = values(0)
                Case "speedofsound"
                    Me.Phases(f).Properties.speedOfSound = values(0)
                Case "compressibilityfactor"
                    Me.Phases(f).Properties.compressibilityFactor = values(0)
                Case "heatcapacity", "heatcapacitycp"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.heatCapacityCp = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.heatCapacityCp = values(0) / 1000
                    End Select
                Case "heatcapacitycv"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.heatCapacityCv = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.heatCapacityCv = values(0) / 1000
                    End Select
                Case "excessenthalpy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.excessEnthalpy = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.excessEnthalpy = values(0) / 1000
                    End Select
                Case "excessentropy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.excessEntropy = values(0) / Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.excessEntropy = values(0) / 1000
                    End Select
                Case "viscosity"
                    Me.Phases(f).Properties.viscosity = values(0)
                    Me.Phases(f).Properties.kinematic_viscosity = values(0) / Me.Phases(f).Properties.density.GetValueOrDefault
                Case "thermalconductivity"
                    Me.Phases(f).Properties.thermalConductivity = values(0)
                Case "fugacity"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = values(comps.IndexOf(c)) / (Me.Phases(0).Properties.pressure.GetValueOrDefault * Me.Phases(f).Compounds(c).MoleFraction.GetValueOrDefault)
                        i += 1
                    Next
                Case "fugacitycoefficient"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "activitycoefficient"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).ActivityCoeff = values(comps.IndexOf(c))
                        i += 1
                    Next
                Case "logfugacitycoefficient"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(f).Compounds(c).FugacityCoeff = Math.Exp(values(comps.IndexOf(c)))
                        i += 1
                    Next
                Case "density"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.density = values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.density = values(0)
                    End Select
                Case "volume"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.density = 1 / values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.density = 1 / values(0)
                    End Select
                Case "enthalpy", "enthalpynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_enthalpy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.enthalpy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.enthalpy = values(0) / 1000
                            Me.Phases(f).Properties.molar_enthalpy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "entropy", "entropynf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_entropy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.entropy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.entropy = values(0) / 1000
                            Me.Phases(f).Properties.molar_entropy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "enthalpyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            Me.Phases(f).Properties.molar_enthalpyF = values(0)
                            If val <> 0.0# Then Me.Phases(f).Properties.enthalpyF = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.enthalpyF = values(0) / 1000
                            Me.Phases(f).Properties.molar_enthalpyF = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "entropyf"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            Me.Phases(f).Properties.molar_entropyF = values(0)
                            If val <> 0.0# Then Me.Phases(f).Properties.entropyF = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.entropyF = values(0) / 1000
                            Me.Phases(f).Properties.molar_entropyF = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "internalenergy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_internal_energy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.internal_energy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.internal_energy = values(0) / 1000
                            Me.Phases(f).Properties.molar_internal_energy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "gibbslenergy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_gibbs_free_energy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.gibbs_free_energy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.gibbs_free_energy = values(0) / 1000
                            Me.Phases(f).Properties.molar_gibbs_free_energy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "helmholtzenergy"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molar_helmholtz_energy = values(0)
                            Dim val = Me.PropertyPackage.AUX_MMM(phs)
                            If val <> 0.0# Then Me.Phases(f).Properties.helmholtz_energy = values(0) / val
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.helmholtz_energy = values(0) / 1000
                            Me.Phases(f).Properties.molar_helmholtz_energy = values(0) * Me.PropertyPackage.AUX_MMM(phs)
                    End Select
                Case "moles"
                    Me.Phases(f).Properties.molarflow = values(0)
                Case "Mass"
                    Me.Phases(f).Properties.massflow = values(0)
                Case "molecularweight"
                    Me.Phases(f).Properties.molecularWeight = values(0)
                Case "temperature"
                    Me.Phases(0).Properties.temperature = values(0)
                Case "pressure"
                    Me.Phases(0).Properties.pressure = values(0)
                Case "flow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim i As Integer = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MolarFlow = values(comps.IndexOf(c))
                                i += 1
                            Next
                        Case "Mass", "mass"
                            Dim i As Integer = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MassFlow = values(comps.IndexOf(c))
                                i += 1
                            Next
                    End Select
                Case "fraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Dim i As Integer = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MoleFraction = values(comps.IndexOf(c))
                                i += 1
                            Next
                        Case "Mass", "mass"
                            Dim i As Integer = 0
                            For Each c As String In comps
                                Me.Phases(f).Compounds(c).MassFraction = values(comps.IndexOf(c))
                                i += 1
                            Next
                    End Select
                Case "phasefraction"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molarfraction = values(0)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.massfraction = values(0)
                    End Select
                Case "totalflow"
                    Select Case basis
                        Case "Molar", "molar", "mole", "Mole"
                            Me.Phases(f).Properties.molarflow = values(0)
                            'Me.Phases(f).Properties.massflow = values(0) / 1000 * Me.PropertyPackage.AUX_MMM(phs)
                        Case "Mass", "mass"
                            Me.Phases(f).Properties.massflow = values(0)
                            'Me.Phases(f).Properties.molarflow = values(0)
                    End Select
                Case Else
                    Dim ex = New CapeOpen.CapeThrmPropertyNotAvailableException
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetSinglePhaseProp", hcode)
            End Select
        End Sub

        ''' <summary>
        ''' Sets two-phase non-constant property values for a mixture.
        ''' </summary>
        ''' <param name="property">The property for which values are set in the Material Object. This 
        ''' must be one of the two-phase properties or derivatives included in sections 7.5.6 and 7.6.</param>
        ''' <param name="phaseLabels">Phase labels of the Phases for which the property is set. The Phase
        ''' labels must be two of the identifiers returned by the GetPhaseList method of the ICapeThermoPhases
        ''' interface.</param>
        ''' <param name="basis">Basis of the results. Valid settings are: “Mass” for Physical Properties per unit
        ''' mass or “Mole” for molar properties. Use UNDEFINED as a place holder for a Physical Property for which 
        ''' basis does not apply. See section 7.5.5 for details.</param>
        ''' <param name="values">Value(s) to set for the property (CapeArrayDouble) or CapeInterface (see notes).</param>
        ''' <remarks>The values argument of SetTwoPhaseProp is either a CapeArrayDouble that contains one or
        ''' more numerical values to be set for a property, e.g. kvalues, or a CapeInterface that may be
        ''' used to set two-phase properties described by a more complex data structure, e.g. distributed
        ''' properties.
        ''' Although some properties set by calls to SetTwoPhaseProp will have a single numerical
        ''' value, the type of the values argument for numerical values is CapeArrayDouble and in such
        ''' a case the method must be called with the values argument containing an array even if it
        ''' contains only a single element.
        ''' The Physical Property values set by SetTwoPhaseProp depend on two Phases, for example
        ''' surface tension or K-values. Properties that depend on a single Phase are set by the
        ''' SetSinglePhaseProp method.
        ''' If a Physical Property with composition derivative is specified, the derivative values will be
        ''' set for both Phases in the order in which the Phase labels are specified. The number of
        ''' values returned for a composition derivative will depend on the property. For example, if
        ''' there are N Compounds then the values vector for the surface tension derivative will contain
        ''' N composition derivative values for the first Phase, followed by N composition derivative
        ''' values for the second Phase. For K-values there will be N2 derivative values for the first
        ''' phase followed by N2 values for the second phase in the order defined in 7.6.2.
        ''' To set a property using SetTwoPhaseProp, phaseLabels identifiers should be passed that are
        ''' supported by the Property Package or Material Object, i.e. one that appears in the list returned by the
        ''' GetPhaseList method of the ICapeThermoPhases interface. Setting such a property should cause the
        ''' phases to be present on the Material Object, as if it were present in a call to SetPresentPhases with
        ''' status Cape_UnknownPhaseStatus. The SetPresentPhases method of this interface does not need to
        ''' be called before calling SetTwoPhaseProp.</remarks>
        Public Sub SetTwoPhaseProp(ByVal [property] As String, ByVal phaseLabels As Object, ByVal basis As String, ByVal values As Object) Implements ICapeThermoMaterial.SetTwoPhaseProp
            Dim comps As New ArrayList
            If TryCast(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage) IsNot Nothing Then
                Dim complist As Object = Nothing
                Me.PropertyPackage.GetCompoundList(complist, Nothing, Nothing, Nothing, Nothing, Nothing)
                For Each s As String In complist
                    For Each kvp As KeyValuePair(Of String, String) In CType(Me.PropertyPackage, PropertyPackages.CAPEOPENPropertyPackage)._mappings
                        If kvp.Value = s Then
                            comps.Add(kvp.Key)
                            Exit For
                        End If
                    Next
                Next
            Else
                For Each c As Compound In Me.Phases(0).Compounds.Values
                    comps.Add(c.Name)
                Next
            End If
            Select Case [property].ToLower
                Case "kvalue"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(0).Compounds(c).Kvalue = values(i)
                        i += 1
                    Next
                Case "logkvalue"
                    Dim i As Integer = 0
                    For Each c As String In comps
                        Me.Phases(0).Compounds(c).lnKvalue = values(i)
                        i += 1
                    Next
                Case "surfacetension"
                    Me.Phases(0).Properties.surfaceTension = values(0)
                Case Else
                    Dim ex = New CapeOpen.CapeThrmPropertyNotAvailableException
                    Dim hcode As Integer = 0
                    ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterial", ex.Source, ex.StackTrace, "GetSinglePhaseProp", hcode)
            End Select
        End Sub

        ''' <summary>
        ''' Returns the number of Phases.
        ''' </summary>
        ''' <returns>The number of Phases supported.</returns>
        ''' <remarks>The number of Phases returned by this method must be equal to the number of Phase labels
        ''' that are returned by the GetPhaseList method of this interface. It must be zero, or a positive
        ''' number.</remarks>
        Public Function GetNumPhases() As Integer Implements ICapeThermoPhases.GetNumPhases
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.GetNumPhases()
        End Function

        ''' <summary>
        ''' Returns information on an attribute associated with a Phase for the purpose of understanding 
        ''' what lies behind a Phase label.
        ''' </summary>
        ''' <param name="phaseLabel">A (single) Phase label. This must be one of the values returned by GetPhaseList method.</param>
        ''' <param name="phaseAttribute">One of the Phase attribute identifiers from the table below.</param>
        ''' <returns>The value corresponding to the Phase attribute identifier – see table below.</returns>
        ''' <remarks>GetPhaseInfo is intended to allow a PME, or other client, to identify a Phase with an arbitrary
        ''' label. A PME, or other client, will need to do this to map stream data into a Material
        ''' Object, or when importing a Property Package. If the client cannot identify the Phase, it can
        ''' ask the user to provide a mapping based on the values of these properties.
        ''' The list of supported Phase attributes is defined in the following table:
        ''' 
        ''' Phase attribute identifier            Supported values
        ''' 
        ''' StateOfAggregation                    One of the following strings:
        '''                                       Vapor
        '''                                       Liquid
        '''                                       Solid
        '''                                       Unknown
        ''' 
        ''' KeyCompoundId                         The identifier of the Compound (compId as returned by GetCompoundList) 
        '''                                       that is expected to be present in highest concentration in the Phase. 
        '''                                       May be undefined in which case UNDEFINED should be returned.
        ''' 
        ''' ExcludedCompoundId                    The identifier of the Compound (compId as returned by
        '''                                       GetCompoundList) that is expected to be present in low or zero
        '''                                       concentration in the Phase. May not be defined in which case
        '''                                       UNDEFINED should be returned.
        ''' 
        ''' DensityDescription                    A description that indicates the density range expected for the Phase.
        '''                                       One of the following strings or UNDEFINED:
        '''                                       Heavy
        '''                                       Light
        ''' 
        ''' UserDescription                       A description that helps the user or PME to identify the Phase.
        '''                                       It can be any string or UNDEFINED.
        ''' 
        ''' TypeOfSolid                           A description that provides more information about a solid Phase. For
        '''                                       Phases with a “Solid” state of aggregation it may be one of the
        '''                                       following standard strings or UNDEFINED:
        '''                                       PureSolid
        '''                                       SolidSolution
        '''                                       HydrateI
        '''                                       HydrateII
        '''                                       HydrateH
        '''                                       Other values may be returned for solid Phases but these may not be
        '''                                       understood by most clients.
        '''                                       For Phases with any other state of aggregation it must be
        '''                                       UNDEFINED.</remarks>
        Public Function GetPhaseInfo(ByVal phaseLabel As String, ByVal phaseAttribute As String) As Object Implements ICapeThermoPhases.GetPhaseInfo
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.GetPhaseInfo(phaseLabel, phaseAttribute)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPhases", ex.Source, ex.StackTrace, "GetPhaseInfo", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Returns Phase labels and other important descriptive information for all the Phases supported.
        ''' </summary>
        ''' <param name="phaseLabels">he list of Phase labels for the Phases supported. A Phase label can 
        ''' be any string but each Phase must have a unique label. If, for some reason, no Phases are 
        ''' supported an UNDEFINED value should be returned for the phaseLabels. The number of Phase labels 
        ''' must also be equal to the number of Phases returned by the GetNumPhases method.</param>
        ''' <param name="stateOfAggregation">The physical State of Aggregation associated with each of the 
        ''' Phases. This must be one of the following strings: ”Vapor”, “Liquid”, “Solid” or “Unknown”. Each 
        ''' Phase must have a single State of Aggregation. The value must not be left undefined, but may be 
        ''' set to “Unknown”.</param>
        ''' <param name="keyCompoundId">The key Compound for the Phase. This must be the Compound identifier 
        ''' (as returned by GetCompoundList), or it may be undefined in which case a UNDEFINED value is returned. 
        ''' The key Compound is an indication of the Compound that is expected to be present in high concentration 
        ''' in the Phase, e.g. water for an aqueous liquid phase. Each Phase can have a single key Compound.</param>
        ''' <remarks>The Phase label allows the phase to be uniquely identified in methods of the ICapeThermo-
        ''' Phases interface and other CAPE-OPEN interfaces. The State of Aggregation and key
        ''' Compound provide a way for the PME, or other client, to interpret the meaning of a Phase
        ''' label in terms of the physical characteristics of the Phase.
        ''' All arrays returned by this method must be of the same length, i.e. equal to the number of
        ''' Phase labels.
        ''' To get further information about a Phase, use the GetPhaseInfo method.</remarks>
        Public Sub GetPhaseList(ByRef phaseLabels As Object, ByRef stateOfAggregation As Object, ByRef keyCompoundId As Object) Implements ICapeThermoPhases.GetPhaseList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.GetPhaseList1(phaseLabels, stateOfAggregation, keyCompoundId)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPhases", ex.Source, ex.StackTrace, "GetPhaseList", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Retrieves the value of a Universal Constant.
        ''' </summary>
        ''' <param name="constantId">Identifier of Universal Constant. The list of constants supported should be 
        ''' obtained by using the GetUniversalConstantList method.</param>
        ''' <returns>Value of Universal Constant. This could be a numeric or a string value. For numeric values 
        ''' the units of measurement are specified in section 7.5.1.</returns>
        ''' <remarks>Universal Constants (often called fundamental constants) are quantities like the gas constant,
        ''' or the Avogadro constant.</remarks>
        Public Function GetUniversalConstant(ByVal constantId As String) As Object Implements ICapeThermoUniversalConstant.GetUniversalConstant
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.GetUniversalConstant1(constantId)
        End Function

        ''' <summary>
        ''' Returns the identifiers of the supported Universal Constants.
        ''' </summary>
        ''' <returns>List of identifiers of Universal Constants. The list of standard identifiers is given in section 7.5.1.</returns>
        ''' <remarks>A component may return Universal Constant identifiers that do not belong to the list defined
        ''' in section 7.5.1. However, these proprietary identifiers may not be understood by most of the
        ''' clients of this component.</remarks>
        Public Function GetUniversalConstantList() As Object Implements ICapeThermoUniversalConstant.GetUniversalConstantList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.GetUniversalConstantList()
        End Function

        ''' <summary>
        ''' This method is used to calculate the natural logarithm of the fugacity coefficients (and
        ''' optionally their derivatives) in a single Phase mixture. The values of temperature, pressure
        ''' and composition are specified in the argument list and the results are also returned through
        ''' the argument list.
        ''' </summary>
        ''' <param name="phaseLabel">Phase label of the Phase for which the properties are to be calculated. 
        ''' The Phase label must be one of the strings returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <param name="temperature">The temperature (K) for the calculation.</param>
        ''' <param name="pressure">The pressure (Pa) for the calculation.</param>
        ''' <param name="moleNumbers">Number of moles of each Compound in the mixture.</param>
        ''' <param name="fFlags">Code indicating whether natural logarithm of the fugacity coefficients and/or derivatives 
        ''' should be calculated (see notes).</param>
        ''' <param name="lnPhi">Natural logarithm of the fugacity coefficients (if requested).</param>
        ''' <param name="lnPhiDT">Derivatives of natural logarithm of the fugacity coefficients w.r.t. temperature (if requested).</param>
        ''' <param name="lnPhiDP">Derivatives of natural logarithm of the fugacity coefficients w.r.t. pressure (if requested).</param>
        ''' <param name="lnPhiDn">Derivatives of natural logarithm of the fugacity coefficients w.r.t. mole numbers (if requested).</param>
        ''' <remarks>This method is provided to allow the natural logarithm of the fugacity coefficient, which is
        ''' the most commonly used thermodynamic property, to be calculated and returned in a highly
        ''' efficient manner.
        ''' The temperature, pressure and composition (mole numbers) for the calculation are specified
        ''' by the arguments and are not obtained from the Material Object by a separate request. Likewise,
        ''' any quantities calculated are returned through the arguments and are not stored in the
        ''' Material Object. The state of the Material Object is not affected by calling this method. It
        ''' should be noted however, that prior to calling CalcAndGetLnPhi a valid Material Object
        ''' must have been defined by calling the SetMaterial method on the
        ''' ICapeThermoMaterialContext interface of the component that implements the
        ''' ICapeThermoPropertyRoutine interface. The compounds in the Material Object must have
        ''' been identified and the number of values supplied in the moleNumbers argument must be
        ''' equal to the number of Compounds in the Material Object.
        ''' The fugacity coefficient information is returned as the natural logarithm of the fugacity
        ''' coefficient. This is because thermodynamic models naturally provide the natural logarithm
        ''' of this quantity and also a wider range of values may be safely returned.
        ''' The quantities actually calculated and returned by this method are controlled by an integer
        ''' code fFlags. The code is formed by summing contributions for the property and each
        ''' derivative required using the enumerated constants eCapeCalculationCode (defined in the
        ''' Thermo version 1.1 IDL) shown in the following table. For example, to calculate log
        ''' fugacity coefficients and their T-derivatives the fFlags argument would be set to
        ''' CAPE_LOG_FUGACITY_COEFFICIENTS + CAPE_T_DERIVATIVE.
        ''' 
        '''                                       code                            numerical value
        ''' no calculation                        CAPE_NO_CALCULATION             0
        ''' log fugacity coefficients             CAPE_LOG_FUGACITY_COEFFICIENTS  1
        ''' T-derivative                          CAPE_T_DERIVATIVE               2
        ''' P-derivative                          CAPE_P_DERIVATIVE               4
        ''' mole number derivatives               CAPE_MOLE_NUMBERS_DERIVATIVES   8
        ''' 
        ''' If CalcAndGetLnPhi is called with fFlags set to CAPE_NO_CALCULATION no property
        ''' values are returned.
        ''' A typical sequence of operations for this method when implemented by a Property Package
        ''' component would be:
        ''' - Check that the phaseLabel specified is valid.
        ''' - Check that the moleNumbers array contains the number of values expected
        ''' (should be consistent with the last call to the SetMaterial method).
        ''' - Calculate the requested properties/derivatives at the T/P/composition specified in
        ''' the argument list.
        ''' - Store values for the properties/derivatives in the corresponding arguments.
        ''' Note that this calculation can be carried out irrespective of whether the Phase actually exists
        ''' in the Material Object.</remarks>
        Public Sub CalcAndGetLnPhi(ByVal phaseLabel As String, ByVal temperature As Double, ByVal pressure As Double, ByVal moleNumbers As Object, ByVal fFlags As Integer, ByRef lnPhi As Object, ByRef lnPhiDT As Object, ByRef lnPhiDP As Object, ByRef lnPhiDn As Object) Implements ICapeThermoPropertyRoutine.CalcAndGetLnPhi
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcAndGetLnPhi(phaseLabel, temperature, pressure, moleNumbers, fFlags, lnPhi, lnPhiDT, lnPhiDP, lnPhiDn)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyRoutine", ex.Source, ex.StackTrace, "CalcAndGetLnPhi", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' CalcSinglePhaseProp is used to calculate properties and property derivatives of a mixture in
        ''' a single Phase at the current values of temperature, pressure and composition set in the
        ''' Material Object. CalcSinglePhaseProp does not perform phase Equilibrium Calculations.
        ''' </summary>
        ''' <param name="props">The list of identifiers for the single-phase properties or derivatives to 
        ''' be calculated. See sections 7.5.5 and 7.6 for the standard identifiers.</param>
        ''' <param name="phaseLabel">Phase label of the Phase for which the properties are to be calculated. 
        ''' The Phase label must be one of the strings returned by the GetPhaseList method on the 
        ''' ICapeThermoPhases interface and the phase must be present in the Material Object.</param>
        ''' <remarks>CalcSinglePhaseProp calculates properties, such as enthalpy or viscosity that are defined for
        ''' a single Phase. Physical Properties that depend on more than one Phase, for example surface
        ''' tension or K-values, are handled by CalcTwoPhaseProp method.
        ''' Components that implement this method must get the input specification for the calculation
        ''' (temperature, pressure and composition) from the associated Material Object and set the
        ''' results in the Material Object.
        ''' Thermodynamic and Physical Properties Components, such as a Property Package or Property
        ''' Calculator, must implement the ICapeThermoMaterialContext interface so that an
        ''' ICapeThermoMaterial interface can be passed via the SetMaterial method.
        ''' The component that implements the ICapeThermoPropertyRoutine interface (e.g. a Property
        ''' Package or Property Calculator) must also implement the ICapeThermoPhases interface so
        ''' that it is possible to get a list of supported phases. The phaseLabel passed to this method
        ''' must be one of the phase labels returned by the GetPhaseList method of the
        ''' ICapeThermoPhases interface and it must also be present in the Material Object, ie. one of
        ''' the phase labels returned by the GetPresentPhases method of the ICapeThermoMaterial
        ''' interface. This latter condition will be satisfied if the phase is made present explicitly by
        ''' calling the SetPresentPhases method or if any phase properties have been set by calling the
        ''' SetSinglePhaseProp or SetTwoPhaseProp methods.
        ''' A typical sequence of operations for CalcSinglePhaseProp when implemented by a Property
        ''' Package component would be:
        ''' - Check that the phaseLabel specified is valid.
        ''' - Use the GetTPFraction method (of the Material Object specified in the last call to the
        ''' SetMaterial method) to get the temperature, pressure and composition of the
        ''' specified Phase.
        ''' - Calculate the properties.
        ''' - Store values for the properties of the Phase in the Material Object using the
        ''' SetSinglePhaseProp method of the ICapeThermoMaterial interface.
        ''' CalcSinglePhaseProp will request the input Property values it requires from the Material
        ''' Object through GetSinglePhaseProp calls. If a requested property is not available, the
        ''' exception raised will be ECapeThrmPropertyNotAvailable. If this error occurs then the
        ''' Property Package can return it to the client, or request a different property. Material Object
        ''' implementations must be able to supply property values using the client’s choice of basis by
        ''' implementing conversion from one basis to another.
        ''' Clients should not assume that Phase fractions and Compound fractions in a Material Object
        ''' are normalised. Fraction values may also lie outside the range 0 to 1. If fractions are not
        ''' normalised, or are outside the expected range, it is the responsibility of the Property Package
        ''' to decide how to deal with the situation.
        ''' It is recommended that properties are requested one at a time in order to simplify error
        ''' handling. However, it is recognised that there are cases where the potential efficiency gains
        ''' of requesting several properties simultaneously are more important. One such example
        ''' might be when a property and its derivatives are required.
        ''' If a client uses multiple properties in a call and one of them fails then the whole call should
        ''' be considered to have failed. This implies that no value should be written back to the Material
        ''' Object by the Property Package until it is known that the whole request can be satisfied.
        ''' It is likely that a PME might request values of properties for a Phase at conditions of temperature,
        ''' pressure and composition where the Phase does not exist (according to the
        ''' mathematical/physical models used to represent properties). The exception
        ''' ECapeThrmPropertyNotAvailable may be raised or an extrapolated value may be returned.
        ''' It is responsibility of the implementer to decide how to handle this circumstance.</remarks>
        Public Sub CalcSinglePhaseProp(ByVal props As Object, ByVal phaseLabel As String) Implements ICapeThermoPropertyRoutine.CalcSinglePhaseProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcSinglePhaseProp(props, phaseLabel)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyRoutine", ex.Source, ex.StackTrace, "CalcSinglePhaseProp", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' CalcTwoPhaseProp is used to calculate mixture properties and property derivatives that depend on
        ''' two Phases at the current values of temperature, pressure and composition set in the Material Object.
        ''' It does not perform Equilibrium Calculations.
        ''' </summary>
        ''' <param name="props">The list of identifiers for properties to be calculated. This must be one or more 
        ''' of the supported two-phase properties and derivatives (as given by the GetTwoPhasePropList method). 
        ''' The standard identifiers for two-phase properties are given in section 7.5.6 and 7.6.</param>
        ''' <param name="phaseLabels">Phase labels of the phases for which the properties are to be calculated. 
        ''' The phase labels must be two of the strings returned by the GetPhaseList method on the ICapeThermoPhases 
        ''' interface and the phases must also be present in the Material Object.</param>
        ''' <remarks>CalcTwoPhaseProp calculates the values of properties such as surface tension or K-values.
        ''' Properties that pertain to a single Phase are handled by the CalcSinglePhaseProp method of
        ''' the ICapeThermoPropertyRoutine interface.Components that implement this method must
        ''' get the input specification for the calculation (temperature, pressure and composition) from
        ''' the associated Material Object and set the results in the Material Object.
        ''' Components such as a Property Package or Property Calculator must implement the
        ''' ICapeThermoMaterialContext interface so that an ICapeThermoMaterial interface can be
        ''' passed via the SetMaterial method.
        ''' The component that implements the ICapeThermoPropertyRoutine interface (e.g. a Property
        ''' Package or Property Calculator) must also implement the ICapeThermoPhases interface so
        ''' that it is possible to get a list of supported phases. The phaseLabels passed to this method
        ''' must be in the list of phase labels returned by the GetPhaseList method of the
        ''' ICapeThermoPhases interface and they must also be present in the Material Object, ie. in the
        ''' list of phase labels returned by the GetPresentPhases method of the ICapeThermoMaterial
        ''' interface. This latter condition will be satisfied if the phases are made present explicitly by
        ''' calling the SetPresentPhases method or if any phase properties have been set by calling the
        ''' SetSinglePhaseProp or SetTwoPhaseProp methods.</remarks>
        Public Sub CalcTwoPhaseProp(ByVal props As Object, ByVal phaseLabels As Object) Implements ICapeThermoPropertyRoutine.CalcTwoPhaseProp
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcTwoPhaseProp(props, phaseLabels)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoPropertyRoutine", ex.Source, ex.StackTrace, "CalcTwoPhaseProp", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Checks whether it is possible to calculate a property with the CalcSinglePhaseProp method for a given Phase.
        ''' </summary>
        ''' <param name="property">The identifier of the property to check. To be valid this must be one of the supported 
        ''' single-phase properties or derivatives (as given by the GetSinglePhasePropList method).</param>
        ''' <param name="phaseLabel">The Phase label for the calculation check. This must be one of the labels 
        ''' returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <returns>Set to True if the combination of property and phaseLabel is supported or False if 
        ''' not supported.</returns>
        ''' <remarks>The result of the check should only depend on the capabilities and configuration
        ''' (Compounds and Phases supported) of the component that implements the
        ''' ICapeThermoPropertyRoutine interface (e.g. a Property Package). It should not depend on
        ''' whether a Material Object has been set nor on the state (temperature, pressure, composition
        ''' etc.), or configuration of a Material Object that might be set.
        ''' It is expected that the PME, or other client, will use this method to check whether the properties
        ''' it requires are supported by the Property Package when the package is imported. If any
        ''' essential properties are not available, the import process should be aborted.
        ''' If either the property or the phaseLabel arguments are not recognised by the component that
        ''' implements the ICapeThermoPropertyRoutine interface this method should return False.</remarks>
        Public Function CheckSinglePhasePropSpec(ByVal [property] As String, ByVal phaseLabel As String) As Boolean Implements ICapeThermoPropertyRoutine.CheckSinglePhasePropSpec
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.CheckSinglePhasePropSpec([property], phaseLabel)
        End Function

        ''' <summary>
        ''' Checks whether it is possible to calculate a property with the CalcTwoPhaseProp method for a given set of Phases.
        ''' </summary>
        ''' <param name="property">The identifier of the property to check. To be valid this must be one of the supported 
        ''' two-phase properties (including derivatives), as given by the GetTwoPhasePropList method.</param>
        ''' <param name="phaseLabels">Phase labels of the Phases for which the properties are to be calculated. The Phase 
        ''' labels must be two of the identifiers returned by the GetPhaseList method on the ICapeThermoPhases interface.</param>
        ''' <returns>Set to True if the combination of property and phaseLabels is supported, or False if not supported.</returns>
        ''' <remarks>The result of the check should only depend on the capabilities and configuration
        ''' (Compounds and Phases supported) of the component that implements the
        ''' ICapeThermoPropertyRoutine interface (e.g. a Property Package). It should not depend on
        ''' whether a Material Object has been set nor on the state (temperature, pressure, composition
        ''' etc.), or configuration of a Material Object that might be set.
        ''' It is expected that the PME, or other client, will use this method to check whether the
        ''' properties it requires are supported by the Property Package when the Property Package is
        ''' imported. If any essential properties are not available, the import process should be aborted.
        ''' If either the property argument or the values in the phaseLabels arguments are not
        ''' recognised by the component that implements the ICapeThermoPropertyRoutine interface
        ''' this method should return False.</remarks>
        Public Function CheckTwoPhasePropSpec(ByVal [property] As String, ByVal phaseLabels As Object) As Boolean Implements ICapeThermoPropertyRoutine.CheckTwoPhasePropSpec
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.CheckTwoPhasePropSpec([property], phaseLabels)
        End Function

        ''' <summary>
        ''' Returns the list of supported non-constant single-phase Physical Properties.
        ''' </summary>
        ''' <returns>List of all supported non-constant single-phase property identifiers. 
        ''' The standard single-phase property identifiers are listed in section 7.5.5.</returns>
        ''' <remarks>A non-constant property depends on the state of the Material Object.
        ''' Single-phase properties, e.g. enthalpy, only depend on the state of one phase.
        ''' GetSinglePhasePropList must return all the single-phase properties that can be calculated by
        ''' CalcSinglePhaseProp. If derivatives can be calculated these must also be returned. The list
        ''' of standard property identifiers in section 7.5.5 also contains properties such as temperature,
        ''' pressure, fraction, phaseFraction, flow and totalFlow that are not usually calculated by the
        ''' CalcSinglePhaseProp method and hence these property identifiers would not be returned by
        ''' GetSinglePhasePropList. These properties would normally be used in calls to the
        ''' Set/GetSinglePhaseProp methods of the ICapeThermoMaterial interface.
        ''' If no single-phase properties are supported this method should return UNDEFINED.
        ''' To get the list of supported two-phase properties, use GetTwoPhasePropList.
        ''' A component that implements this method may return non-constant single-phase property
        ''' identifiers which do not belong to the list defined in section 7.5.5. However, these
        ''' proprietary identifiers may not be understood by most of the clients of this component.</remarks>
        Public Function GetSinglePhasePropList() As Object Implements ICapeThermoPropertyRoutine.GetSinglePhasePropList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.GetSinglePhasePropList()
        End Function

        ''' <summary>
        ''' Returns the list of supported non-constant two-phase properties.
        ''' </summary>
        ''' <returns>List of all supported non-constant two-phase property identifiers. The standard two-phase 
        ''' property identifiers are listed in section 7.5.6.</returns>
        ''' <remarks>A non-constant property depends on the state of the Material Object. Two-phase properties
        ''' are those that depend on more than one co-existing phase, e.g. K-values.
        ''' GetTwoPhasePropList must return all the properties that can be calculated by
        ''' CalcTwoPhaseProp. If derivatives can be calculated, these must also be returned.
        ''' If no two-phase properties are supported this method should return UNDEFINED.
        ''' To check whether a property can be evaluated for a particular set of phase labels use the
        ''' CheckTwoPhasePropSpec method.
        ''' A component that implements this method may return non-constant two-phase property
        ''' identifiers which do not belong to the list defined in section 7.5.6. However, these
        ''' proprietary identifiers may not be understood by most of the clients of this component.
        ''' To get the list of supported single-phase properties, use GetSinglePhasePropList.</remarks>
        Public Function GetTwoPhasePropList() As Object Implements ICapeThermoPropertyRoutine.GetTwoPhasePropList
            Me.PropertyPackage.CurrentMaterialStream = Me
            Return Me.PropertyPackage.GetTwoPhasePropList()
        End Function

        ''' <summary>
        ''' CalcEquilibrium is used to calculate the amounts and compositions of Phases at equilibrium.
        ''' CalcEquilibrium will calculate temperature and/or pressure if these are not among the two
        ''' specifications that are mandatory for each Equilibrium Calculation considered.
        ''' </summary>
        ''' <param name="specification1">First specification for the Equilibrium Calculation. The 
        ''' specification information is used to retrieve the value of the specification from the 
        ''' Material Object. See below for details.</param>
        ''' <param name="specification2">Second specification for the Equilibrium Calculation in 
        ''' the same format as specification1.</param>
        ''' <param name="solutionType">The identifier for the required solution type. The
        ''' standard identifiers are given in the following list:
        ''' Unspecified
        ''' Normal
        ''' Retrograde
        ''' The meaning of these terms is defined below in the notes. Other identifiers may be supported 
        ''' but their interpretation is not part of the CO standard.</param>
        ''' <remarks>The specification1 and specification2 arguments provide the information necessary to
        ''' retrieve the values of two specifications, for example the pressure and temperature, for the
        ''' Equilibrium Calculation. The CheckEquilibriumSpec method can be used to check for
        ''' supported specifications. Each specification variable contains a sequence of strings in the
        ''' order defined in the following table (hence, the specification arguments may have 3 or 4
        ''' items):
        ''' 
        ''' item                        meaning
        ''' 
        ''' property identifier         The property identifier can be any of the identifiers listed in section 7.5.5 but
        '''                             only certain property specifications will normally be supported by any
        '''                             Equilibrium Routine.
        ''' 
        ''' basis                       The basis for the property value. Valid settings for basis are given in section
        '''                             7.4. Use UNDEFINED as a placeholder for a property for which basis does
        '''                             not apply. For most Equilibrium Specifications, the result of the calculation
        '''                             is not dependent on the basis, but, for example, for phase fraction
        '''                             specifications the basis (Mole or Mass) does make a difference.
        ''' 
        ''' phase label                 The phase label denotes the Phase to which the specification applies. It must
        '''                             either be one of the labels returned by GetPresentPhases, or the special value
        '''                             “Overall”.
        ''' 
        ''' compound identifier         The compound identifier allows for specifications that depend on a particular
        '''                             Compound. This item of the specification array is optional and may be
        '''                             omitted. In case of a specification without compound identifier, the array
        '''                             element may be present and empty, or may be absent.
        '''                             The values corresponding to the specifications in the argument list and the overall
        '''                             composition of the mixture must be set in the associated Material Object before a call to
        '''                             CalcEquilibrium.
        ''' 
        ''' Components such as a Property Package or an Equilibrium Calculator must implement the
        ''' ICapeThermoMaterialContext interface, so that an ICapeThermoMaterial interface can be
        ''' passed via the SetMaterial method. It is the responsibility of the implementation of
        ''' CalcEquilibrium to validate the Material Object before attempting a calculation.
        ''' The Phases that will be considered in the Equilibrium Calculation are those that exist in the
        ''' Material Object, i.e. the list of phases specified in a SetPresentPhases call. This provides a
        ''' way for a client to specify whether, for example, a vapour-liquid, liquid-liquid, or vapourliquid-
        ''' liquid calculation is required. CalcEquilibrium must use the GetPresentPhases method
        ''' to retrieve the list of Phases and the associated Phase status flags. The Phase status flags may
        ''' be used by the client to provide information about the Phases, for example whether estimates
        ''' of the equilibrium state are provided. See the description of the GetPresentPhases and
        ''' SetPresentPhases methods of the ICapeThermoMaterial interface for details. When the
        ''' Equilibrium Calculation has been completed successfully, the SetPresentPhases method
        ''' must be used to specify which Phases are present at equilibrium and the Phase status flags
        ''' for the phases should be set to Cape_AtEquilibrium. This must include any Phases that are
        ''' present in zero amount such as the liquid Phase in a dew point calculation.
        ''' Some types of Phase equilibrium specifications may result in more than one solution. A
        ''' common example of this is the case of a dew point calculation. However, CalcEquilibrium
        ''' can provide only one solution through the Material Object. The solutionType argument
        ''' allows the “Normal” or “Retrograde” solution to be explicitly requested. When none of the
        ''' specifications includes a phase fraction, the solutionType argument should be set to
        ''' “Unspecified”.
        ''' 
        ''' CalcEquilibrium must set the amounts (phase fractions), compositions, temperature and
        ''' pressure for all Phases present at equilibrium, as well as the temperature and pressure for the
        ''' overall mixture if not set as part of the calculation specifications. It must not set any other
        ''' values – in particular it must not set any values for phases that are not present.
        ''' 
        ''' As an example, the following sequence of operations might be performed by
        ''' CalcEquilibrium in the case of an Equilibrium Calculation at fixed pressure and temperature:
        ''' 
        ''' - With the ICapeThermoMaterial interface of the supplied Material Object:
        ''' 
        ''' -- Use the GetPresentPhases method to find the list of Phases that the Equilibrium
        ''' Calculation should consider.
        ''' 
        ''' -- With the ICapeThermoCompounds interface of the Material Object use the
        ''' GetCompoundList method to find which Compounds are present.
        ''' 
        ''' -- Use the GetOverallProp method to get the temperature, pressure and composition
        ''' for the overall mixture.
        ''' 
        ''' - Perform the Equilibrium Calculation.
        ''' 
        ''' -- Use SetPresentPhases to specify the Phases present at equilibrium and set the
        ''' Phase status flags to Cape_AtEquilibrium.
        ''' 
        ''' -- Use SetSinglePhaseProp to set pressure, temperature, Phase amount (or Phase
        ''' fraction) and composition for all Phases present.</remarks>
        Public Sub CalcEquilibrium1(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String) Implements ICapeThermoEquilibriumRoutine.CalcEquilibrium
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Me.PropertyPackage.CalcEquilibrium1(specification1, specification2, solutionType)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoEquilibriumRoutine", ex.Source, ex.StackTrace, "CalcEquilibrium", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Checks whether the Property Package can support a particular type of Equilibrium Calculation.
        ''' </summary>
        ''' <param name="specification1">First specification for the Equilibrium Calculation.</param>
        ''' <param name="specification2">Second specification for the Equilibrium Calculation.</param>
        ''' <param name="solutionType">The required solution type.</param>
        ''' <returns>Set to True if the combination of specifications and solutionType is supported 
        ''' for a particular combination of present phases or False if not supported.</returns>
        ''' <remarks>The meaning of the specification1, specification2 and solutionType arguments is the same as
        ''' for the CalcEquilibrium method. If solutionType, specification1 and specification2
        ''' arguments appear valid but the actual specifications are not supported or not recognised a
        ''' False value should be returned.
        ''' The result of the check should depend primarily on the capabilities and configuration
        ''' (compounds and phases supported) of the component that implements the ICapeThermo-
        ''' EquilibriumRoutine interface (egg. a Property package). A component that supports
        ''' calculation specifications for any combination of supported phases is capable of checking
        ''' the specification without any reference to a Material Object. However, it is possible that
        ''' there may be restrictions on the combinations of phases supported in an equilibrium
        ''' calculation. For example a component may support vapor-liquid and liquid-liquid
        ''' calculations but not vapor-liquid-liquid calculations. In general it is therefore a necessary
        ''' prerequisite that a Material Object has been set (using the SetMaterial method of the
        ''' ICapeThermoMaterialContext interface) and that the SetPresentPhases method of the
        ''' ICapeThermoMaterial interface has been called to specify the combination of phases for the
        ''' equilibrium calculation. The result of the check should not depend on the state (temperature,
        ''' pressure, composition etc.) of the Material Object.</remarks>
        Public Function CheckEquilibriumSpec(ByVal specification1 As Object, ByVal specification2 As Object, ByVal solutionType As String) As Boolean Implements ICapeThermoEquilibriumRoutine.CheckEquilibriumSpec
            Me.PropertyPackage.CurrentMaterialStream = Me
            Try
                Return Me.PropertyPackage.CheckEquilibriumSpec(specification1, specification2, solutionType)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoEquilibriumRoutine", ex.Source, ex.StackTrace, "CalcEquilibrium", hcode)
                Return Nothing
            End Try
        End Function

        ''' <summary>
        ''' Allows the client of a component that implements this interface to pass an ICapeThermoMaterial 
        ''' interface to the component, so that it can access the properties of a Material.
        ''' </summary>
        ''' <param name="material">The Material interface.</param>
        ''' <remarks>The SetMaterial method allows a Thermodynamic and Physical Properties component, such
        ''' as a Property Package, to be given the ICapeThermoMaterial interface of a Material Object.
        ''' This interface gives the component access to the description of the Material for which
        ''' Property Calculations or Equilibrium Calculations are required. The component can access
        ''' property values directly using this interface. A client can also use the ICapeThermoMaterial
        ''' interface to query a Material Object for its ICapeThermoCompounds and ICapeThermo-
        ''' Phases interfaces, which provide access to Compound and Phase information, respectively.
        ''' It is envisaged that the SetMaterial method will be used to check that the Material Interface
        ''' supplied is valid and useable. For example, a Property Package may check that there are
        ''' some Compounds in a Material Object and that those Compounds can be identified by the
        ''' Property Package. In addition a Property Package may perform any initialisation that
        ''' depends on the configuration of a Material Object. A Property Calculator component might
        ''' typically use this method to query the Material Object for any required information
        ''' concerning the Compounds.
        ''' Calling the UnsetMaterial method of the ICapeThermoMaterialContext interface has the
        ''' effect of removing the interface set by the SetMaterial method.
        ''' After a call to SetMaterial() has been received, the object implementing the ICapeThermo-
        ''' MaterialContext interface can assume that the number, name and order of compounds for
        ''' that Material Object will remain fixed until the next call to SetMaterial() or UnsetMaterial().</remarks>
        Public Sub SetMaterial(ByVal material As Object) Implements ICapeThermoMaterialContext.SetMaterial
            Try
                Me.PropertyPackage.SetMaterial(material)
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialContext", ex.Source, ex.StackTrace, "SetMaterial", hcode)
            End Try
        End Sub

        ''' <summary>
        ''' Removes any previously set Material interface.
        ''' </summary>
        ''' <remarks>The UnsetMaterial method removes any Material interface previously set by a call to the
        ''' SetMaterial method of the ICapeThermoMaterialContext interface. This means that any
        ''' methods of other interfaces that depend on having a valid Material Interface, for example
        ''' methods of the ICapeThermoPropertyRoutine or ICapeThermoEquilibriumRoutine
        ''' interfaces, should behave in the same way as if the SetMaterial method had never been
        ''' called.
        ''' If UnsetMaterial is called before a call to SetMaterial it has no effect and no exception
        ''' should be raised.</remarks>
        Public Sub UnsetMaterial() Implements ICapeThermoMaterialContext.UnsetMaterial
            Try
                Me.PropertyPackage.UnsetMaterial()
            Catch ex As Exception
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(ex.Message.ToString, ex)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(ex, "Error", ex.Message, "ICapeThermoMaterialContext", ex.Source, ex.StackTrace, "UnsetMaterial", hcode)
            End Try
        End Sub

#End Region

#Region "    CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            If Not PropertyPackage Is Nothing Then
                PropertyPackage.ExceptionLog += Date.Now + ": " + vbCrLf + vbCrLf + ex.ToString + vbCrLf + vbCrLf
            End If

            Throw New CapeComputationException(ex.Message.ToString, ex)

        End Sub

        Private _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
            Get
                Return Me.Name
            End Get
        End Property

        Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
            Get
                Return _code
            End Get
        End Property

        Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
            Get
                Return _description
            End Get
        End Property

        Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
            Get
                Return _interfacename
            End Get
        End Property

        Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
            Get
                Return _moreinfo
            End Get
        End Property

        Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
            Get
                Return _operation
            End Get
        End Property

        Public ReadOnly Property scope() As String Implements CapeOpen.ECapeUser.scope
            Get
                Return _scope
            End Get
        End Property

#End Region

        Public Property InputComposition As Dictionary(Of String, Double) = New Dictionary(Of String, Double) Implements IMaterialStream.InputComposition

        Public Property IsElectrolyteStream As Boolean = False Implements IMaterialStream.IsElectrolyteStream

        Public Property ReferenceSolvent As String = "" Implements IMaterialStream.ReferenceSolvent

        Public Property SpecType As Enums.StreamSpec = Enums.StreamSpec.Temperature_and_Pressure Implements IMaterialStream.SpecType

        Public Property CompositionBasis As CompositionBasis Implements IMaterialStream.CompositionBasis

        Public Function Clone1() As IMaterialStream Implements IMaterialStream.Clone
            Return ShallowClone()
        End Function

        Public Overrides Function Clone() As Object
            Return ShallowClone()
        End Function

        Public Function ShallowClone() As Streams.MaterialStream

            Dim ms As New MaterialStream("", "", FlowSheet, PropertyPackage)
            If Not FlowSheet Is Nothing Then
                FlowSheet.AddCompoundsToMaterialStream(ms)
            Else
                For Each phase As IPhase In ms.Phases.Values
                    For Each comp In Me.Phases(0).Compounds.Values
                        With phase
                            .Compounds.Add(comp.Name, New Compound(comp.Name, ""))
                            .Compounds(comp.Name).ConstantProperties = comp.ConstantProperties
                        End With
                    Next
                Next
            End If
            ms.Assign(Me)
            ms.AssignProps(Me)

            Return ms

        End Function

        Public ReadOnly Property Flowsheet1 As IFlowsheet Implements IMaterialStream.Flowsheet
            Get
                Return FlowSheet
            End Get
        End Property

        Public Sub Validate1() Implements IMaterialStream.Validate
            Validate()
        End Sub

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New MaterialStreamEditor With {.MatStream = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New MaterialStreamEditor With {.MatStream = Me}
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

        Public Property EditorState As String = "{}"

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.stream_mat_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return Calculator.GetLocalString("MSTR_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return Calculator.GetLocalString("MSTR_Name")
        End Function

        Public Overrides Function GetDefaultProperties() As String()
            Return New String() {"PROP_MS_0", "PROP_MS_1", "PROP_MS_2", "PROP_MS_3", "PROP_MS_4", "PROP_MS_9", "PROP_MS_10", "PROP_MS_27", "PROP_MS_130", "PROP_MS_154"}
        End Function

        Public Function GetProcessFlowsheetProperties() As String()

            Dim prefix = "PROP_MS_"

            Dim plist As New List(Of String)

            plist.Add(prefix & "0")
            plist.Add(prefix & "1")
            plist.Add(prefix & "2")
            plist.Add(prefix & "3")
            plist.Add(prefix & "4")
            plist.Add(prefix & "27")
            plist.Add(prefix & "28")
            plist.Add(prefix & "29")
            plist.Add(prefix & "12")
            plist.Add(prefix & "13")
            plist.Add(prefix & "20")
            plist.Add(prefix & "21")
            plist.Add(prefix & "26")
            For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                plist.Add("PROP_MS_106" + "/" + subst.Name)
            Next
            plist.Add(prefix & "63")
            plist.Add(prefix & "64")
            plist.Add(prefix & "65")
            plist.Add(prefix & "48")
            plist.Add(prefix & "49")
            plist.Add(prefix & "56")
            plist.Add(prefix & "57")
            For Each subst As ConstantProperties In FlowSheet.SelectedCompounds.Values
                plist.Add("PROP_MS_108" + "/" + subst.Name)
            Next

            Return plist.ToArray

        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Sub CalcPhaseMassComposition(dwp As PropertyPackages.Phase)

            Dim idx As Integer = 0

            Select Case dwp
                Case PropertyPackages.Phase.Aqueous
                    idx = 2
                Case PropertyPackages.Phase.Liquid
                    idx = 1
                Case PropertyPackages.Phase.Liquid1
                    idx = 3
                Case PropertyPackages.Phase.Liquid2
                    idx = 4
                Case PropertyPackages.Phase.Liquid3
                    idx = 5
                Case PropertyPackages.Phase.Mixture
                    idx = 0
                Case PropertyPackages.Phase.Solid
                    idx = 7
                Case PropertyPackages.Phase.Vapor
                    idx = 2
            End Select

            Dim mol_x_mm As Double

            For Each sub1 In Phases(idx).Compounds.Values
                mol_x_mm += sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight
            Next

            For Each sub1 In Phases(idx).Compounds.Values
                sub1.MassFraction = sub1.MoleFraction.GetValueOrDefault * sub1.ConstantProperties.Molar_Weight / mol_x_mm
            Next

        End Sub

        Private Function GetPhase2(phasename As String) As IPhase Implements IMaterialStream.GetPhase
            Return GetPhase(phasename)
        End Function

        Public Overrides Function CloneXML() As Object
            Dim ms As New MaterialStream("", "", FlowSheet, PropertyPackage)
            FlowSheet.AddCompoundsToMaterialStream(ms)
            ms.Assign(Me)
            ms.AssignProps(Me)
            Return ms
        End Function

        Public Overrides Function CloneJSON() As Object
            Dim settings As New Newtonsoft.Json.JsonSerializerSettings
            settings.ReferenceLoopHandling = Newtonsoft.Json.ReferenceLoopHandling.Ignore
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of MaterialStream)(Newtonsoft.Json.JsonConvert.SerializeObject(Me), settings)
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public ReadOnly Property PhasesArray As IPhase() Implements IMaterialStream.PhasesArray
            Get
                Return Phases.Values.ToArray
            End Get
        End Property

        Public Property FloatingTableAmountBasis As CompositionBasis = CompositionBasis.DefaultBasis Implements IMaterialStream.FloatingTableAmountBasis

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            PropertyPackage.CurrentMaterialStream = Me

            Dim props As String() = PropertyPackage.GetSinglePhasePropList()
            Dim overallprops As String() = PropertyPackage.GetOverallPropList()
            Dim comps As String() = PropertyPackage.RET_VNAMES2(GetFlowsheet().FlowsheetOptions.CompoundOrderingMode)

            Dim units As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem

            Dim results As New CalculationResults

            Dim ny, nl1, nl2, ns, vz(), wz(), vnz(), wnz() As Double, i As Integer

            ny = Phases(2).Properties.molarfraction.GetValueOrDefault
            nl1 = Phases(3).Properties.molarfraction.GetValueOrDefault
            nl2 = Phases(4).Properties.molarfraction.GetValueOrDefault
            ns = Phases(7).Properties.molarfraction.GetValueOrDefault

            results.Data.Add("SPACE00", Nothing)
            results.Data.Add("Stream Temperature", New List(Of Double) From {Me.Phases(0).Properties.temperature.GetValueOrDefault.ConvertFromSI(units.temperature)})
            results.DataUnits.Add("Stream Temperature", GetPropUnits("temperature", "", units))
            results.Data.Add("Stream Pressure", New List(Of Double) From {Me.Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure)})
            results.DataUnits.Add("Stream Pressure", GetPropUnits("pressure", "", units))
            results.Data.Add("Stream Enthalpy", New List(Of Double) From {Me.Phases(0).Properties.enthalpy.GetValueOrDefault.ConvertFromSI(units.enthalpy)})
            results.DataUnits.Add("Stream Enthalpy", GetPropUnits("enthalpy", "", units))
            results.Data.Add("Stream Entropy", New List(Of Double) From {Me.Phases(0).Properties.entropy.GetValueOrDefault.ConvertFromSI(units.entropy)})
            results.DataUnits.Add("Stream Entropy", GetPropUnits("entropy", "", units))
            results.Data.Add("SPACE0010", Nothing)
            results.Data.Add("Vapor Phase Molar Fraction", New List(Of Double) From {Me.Phases(2).Properties.molarfraction.GetValueOrDefault})
            results.DataUnits.Add("Vapor Phase Molar Fraction", "")
            results.Data.Add("Liquid Phase 1 Molar Fraction", New List(Of Double) From {Me.Phases(3).Properties.molarfraction.GetValueOrDefault})
            results.DataUnits.Add("Liquid Phase 1 Molar Fraction", "")
            results.Data.Add("Liquid Phase 2 Molar Fraction", New List(Of Double) From {Me.Phases(4).Properties.molarfraction.GetValueOrDefault})
            results.DataUnits.Add("Liquid Phase 2 Molar Fraction", "")
            results.Data.Add("Solid Phase Molar Fraction", New List(Of Double) From {Me.Phases(7).Properties.molarfraction.GetValueOrDefault})
            results.DataUnits.Add("Solid Phase Molar Fraction", "")


            If (ny > 0.0000000001# And ny < 0.9999999999#) OrElse nl2 > 0.000000001# OrElse ns > 0.0# Then

                results.Data.Add("SPACE00000000", Nothing)
                results.Data.Add("[Overall] Mass Flow", New List(Of Double) From {Me.Phases(0).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow)})
                results.DataUnits.Add("[Overall] Mass Flow", GetPropUnits("flow", "mass", units))
                results.Data.Add("[Overall] Molar Flow", New List(Of Double) From {Me.Phases(0).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow)})
                results.DataUnits.Add("[Overall] Molar Flow", GetPropUnits("flow", "mole", units))
                results.Data.Add("[Overall] Volumetric Flow", New List(Of Double) From {Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow)})
                results.DataUnits.Add("[Overall] Volumetric Flow", units.volumetricFlow)

                vz = GetSinglePhaseProp2("fraction", "mole", "Overall").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Overall").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Overall").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Overall").ConvertFromSI(units.massflow).ToArray

                results.Data.Add("SPACE1", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Overall] " + comp + " Mole Frac", New List(Of Double)({vz(i)}))
                    results.DataUnits.Add("[Overall] " + comp + " Mole Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE2", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Overall] " + comp + " Mass Frac", New List(Of Double)({wz(i)}))
                    results.DataUnits.Add("[Overall] " + comp + " Mass Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE3", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Overall] " + comp + " Mole Flow", New List(Of Double)({vnz(i)}))
                    results.DataUnits.Add("[Overall] " + comp + " Mole Flow", units.molarflow)
                    i += 1
                Next
                results.Data.Add("SPACE4", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Overall] " + comp + " Mass Flow", New List(Of Double)({wnz(i)}))
                    results.DataUnits.Add("[Overall] " + comp + " Mass Flow", units.massflow)
                    i += 1
                Next
                results.Data.Add("SPACE5", Nothing)
                For Each p As String In overallprops
                    results.Data.Add("[Overall] " & TranslateString(p), GetSinglePhaseProp2(p, "mass", "Overall").ConvertFromSI(GetPropUnits(p, "mass", units)))
                    results.DataUnits.Add("[Overall] " & TranslateString(p), GetPropUnits(p, "mass", units))
                Next

            End If

            If ny > 0.00001# Then
                results.Data.Add("SPACE000", Nothing)
                results.Data.Add("[Vapor Phase] Mass Flow", New List(Of Double) From {Me.Phases(2).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow)})
                results.DataUnits.Add("[Vapor Phase] Mass Flow", GetPropUnits("flow", "mass", units))
                results.Data.Add("[Vapor Phase] Molar Flow", New List(Of Double) From {Me.Phases(2).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow)})
                results.DataUnits.Add("[Vapor Phase] Molar Flow", GetPropUnits("flow", "mole", units))
                results.Data.Add("[Vapor Phase] Volumetric Flow", New List(Of Double) From {Me.Phases(2).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow)})
                results.DataUnits.Add("[Vapor Phase] Volumetric Flow", units.volumetricFlow)
                results.Data.Add("[Vapor Phase] Phase Mole Fraction", GetSinglePhaseProp2("phasefraction", "mole", "Vapor"))
                results.DataUnits.Add("[Vapor Phase] Phase Mole Fraction", "")
                results.Data.Add("[Vapor Phase] Phase Mass Fraction", GetSinglePhaseProp2("phasefraction", "mass", "Vapor"))
                results.DataUnits.Add("[Vapor Phase] Phase Mass Fraction", "")

                vz = GetSinglePhaseProp2("fraction", "mole", "Vapor").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Vapor").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Vapor").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Vapor").ConvertFromSI(units.massflow).ToArray

                results.Data.Add("SPACE11", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Vapor Phase] " + comp + " Mole Frac", New List(Of Double)({vz(i)}))
                    results.DataUnits.Add("[Vapor Phase] " + comp + " Mole Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE21", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Vapor Phase] " + comp + " Mass Frac", New List(Of Double)({wz(i)}))
                    results.DataUnits.Add("[Vapor Phase] " + comp + " Mass Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE31", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Vapor Phase] " + comp + " Mole Flow", New List(Of Double)({vnz(i)}))
                    results.DataUnits.Add("[Vapor Phase] " + comp + " Mole Flow", units.molarflow)
                    i += 1
                Next
                results.Data.Add("SPACE41", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Vapor Phase] " + comp + " Mass Flow", New List(Of Double)({wnz(i)}))
                    results.DataUnits.Add("[Vapor Phase] " + comp + " Mass Flow", units.massflow)
                    i += 1
                Next
                results.Data.Add("SPACE51", Nothing)
                For Each p As String In props
                    results.Data.Add("[Vapor Phase] " & TranslateString(p), GetSinglePhaseProp2(p, "mass", "Vapor").ConvertFromSI(GetPropUnits(p, "mass", units)))
                    results.DataUnits.Add("[Vapor Phase] " & TranslateString(p), GetPropUnits(p, "mass", units))
                Next

            End If

            If nl1 > 0.00001# Then
                results.Data.Add("SPACE0000", Nothing)
                results.Data.Add("[Liquid Phase] Mass Flow", New List(Of Double) From {Me.Phases(3).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow)})
                results.DataUnits.Add("[Liquid Phase] Mass Flow", GetPropUnits("flow", "mass", units))
                results.Data.Add("[Liquid Phase] Molar Flow", New List(Of Double) From {Me.Phases(3).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow)})
                results.DataUnits.Add("[Liquid Phase] Molar Flow", GetPropUnits("flow", "mole", units))
                results.Data.Add("[Liquid Phase] Volumetric Flow", New List(Of Double) From {Me.Phases(3).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow)})
                results.DataUnits.Add("[Liquid Phase] Volumetric Flow", units.volumetricFlow)
                results.Data.Add("[Liquid Phase] Phase Mole Fraction", GetSinglePhaseProp2("phasefraction", "mole", "Liquid"))
                results.DataUnits.Add("[Liquid Phase] Phase Mole Fraction", "")
                results.Data.Add("[Liquid Phase] Phase Mass Fraction", GetSinglePhaseProp2("phasefraction", "mass", "Liquid"))
                results.DataUnits.Add("[Liquid Phase] Phase Mass Fraction", "")

                vz = GetSinglePhaseProp2("fraction", "mole", "Liquid").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Liquid").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Liquid").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Liquid").ConvertFromSI(units.massflow).ToArray

                results.Data.Add("SPACE12", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase] " + comp + " Mole Frac", New List(Of Double)({vz(i)}))
                    results.DataUnits.Add("[Liquid Phase] " + comp + " Mole Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE22", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase] " + comp + " Mass Frac", New List(Of Double)({wz(i)}))
                    results.DataUnits.Add("[Liquid Phase] " + comp + " Mass Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE32", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase] " + comp + " Mole Flow", New List(Of Double)({vnz(i)}))
                    results.DataUnits.Add("[Liquid Phase] " + comp + " Mole Flow", units.molarflow)
                    i += 1
                Next
                results.Data.Add("SPACE42", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase] " + comp + " Mass Flow", New List(Of Double)({wnz(i)}))
                    results.DataUnits.Add("[Liquid Phase] " + comp + " Mass Flow", units.massflow)
                    i += 1
                Next
                results.Data.Add("SPACE52", Nothing)
                For Each p As String In props
                    results.Data.Add("[Liquid Phase] " & TranslateString(p), GetSinglePhaseProp2(p, "mass", "Liquid").ConvertFromSI(GetPropUnits(p, "mass", units)))
                    results.DataUnits.Add("[Liquid Phase] " & TranslateString(p), GetPropUnits(p, "mass", units))
                Next

            End If


            If nl2 > 0.00001# Then
                results.Data.Add("SPACE00000", Nothing)
                results.Data.Add("[Liquid Phase 2] Mass Flow", New List(Of Double) From {Me.Phases(4).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow)})
                results.DataUnits.Add("[Liquid Phase 2] Mass Flow", GetPropUnits("flow", "mass", units))
                results.Data.Add("[Liquid Phase 2] Molar Flow", New List(Of Double) From {Me.Phases(4).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow)})
                results.DataUnits.Add("[Liquid Phase 2] Molar Flow", GetPropUnits("flow", "mole", units))
                results.Data.Add("[Liquid Phase 2] Volumetric Flow", New List(Of Double) From {Me.Phases(4).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow)})
                results.DataUnits.Add("[Liquid Phase 2] Volumetric Flow", units.volumetricFlow)
                results.Data.Add("[Liquid Phase 2] Phase Mole Fraction", GetSinglePhaseProp2("phasefraction", "mole", "Liquid2"))
                results.DataUnits.Add("[Liquid Phase 2] Phase Mole Fraction", "")
                results.Data.Add("[Liquid Phase 2] Phase Mass Fraction", GetSinglePhaseProp2("phasefraction", "mass", "Liquid2"))
                results.DataUnits.Add("[Liquid Phase 2] Phase Mass Fraction", "")

                vz = GetSinglePhaseProp2("fraction", "mole", "Liquid2").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Liquid2").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Liquid2").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Liquid2").ConvertFromSI(units.massflow).ToArray

                results.Data.Add("SPACE13", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase 2] " + comp + " Mole Frac", New List(Of Double)({vz(i)}))
                    results.DataUnits.Add("[Liquid Phase 2] " + comp + " Mole Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE23", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase 2] " + comp + " Mass Frac", New List(Of Double)({wz(i)}))
                    results.DataUnits.Add("[Liquid Phase 2] " + comp + " Mass Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE33", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase 2] " + comp + " Mole Flow", New List(Of Double)({vnz(i)}))
                    results.DataUnits.Add("[Liquid Phase 2] " + comp + " Mole Flow", units.molarflow)
                    i += 1
                Next
                results.Data.Add("SPACE43", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Liquid Phase 2] " + comp + " Mass Flow", New List(Of Double)({wnz(i)}))
                    results.DataUnits.Add("[Liquid Phase 2] " + comp + " Mass Flow", units.massflow)
                    i += 1
                Next
                results.Data.Add("SPACE53", Nothing)
                For Each p As String In props
                    results.Data.Add("[Liquid Phase 2] " & TranslateString(p), GetSinglePhaseProp2(p, "mass", "Liquid2").ConvertFromSI(GetPropUnits(p, "mass", units)))
                    results.DataUnits.Add("[Liquid Phase 2] " & TranslateString(p), GetPropUnits(p, "mass", units))
                Next

            End If

            If ns > 0.00001# Then
                results.Data.Add("SPACE0000S", Nothing)
                results.Data.Add("[Solid Phase] Mass Flow", New List(Of Double) From {Me.Phases(7).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow)})
                results.DataUnits.Add("[Solid Phase] Mass Flow", GetPropUnits("flow", "mass", units))
                results.Data.Add("[Solid Phase] Molar Flow", New List(Of Double) From {Me.Phases(7).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow)})
                results.DataUnits.Add("[Solid Phase] Molar Flow", GetPropUnits("flow", "mole", units))
                results.Data.Add("[Solid Phase] Volumetric Flow", New List(Of Double) From {Me.Phases(7).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow)})
                results.DataUnits.Add("[Solid Phase] Volumetric Flow", units.volumetricFlow)
                results.Data.Add("[Solid Phase] Phase Mole Fraction", GetSinglePhaseProp2("phasefraction", "mole", "Solid"))
                results.DataUnits.Add("[Solid Phase] Phase Mole Fraction", "")
                results.Data.Add("[Solid Phase] Phase Mass Fraction", GetSinglePhaseProp2("phasefraction", "mass", "Solid"))
                results.DataUnits.Add("[Solid Phase] Phase Mass Fraction", "")

                vz = GetSinglePhaseProp2("fraction", "mole", "Solid").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Solid").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Solid").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Solid").ConvertFromSI(units.massflow).ToArray

                results.Data.Add("SPACE12S", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Solid Phase] " + comp + " Mole Frac", New List(Of Double)({vz(i)}))
                    results.DataUnits.Add("[Solid Phase] " + comp + " Mole Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE22S", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Solid Phase] " + comp + " Mass Frac", New List(Of Double)({wz(i)}))
                    results.DataUnits.Add("[Solid Phase] " + comp + " Mass Frac", "")
                    i += 1
                Next
                results.Data.Add("SPACE32S", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Solid Phase] " + comp + " Mole Flow", New List(Of Double)({vnz(i)}))
                    results.DataUnits.Add("[Solid Phase] " + comp + " Mole Flow", units.molarflow)
                    i += 1
                Next
                results.Data.Add("SPACE42S", Nothing)
                i = 0
                For Each comp As String In comps
                    results.Data.Add("[Solid Phase] " + comp + " Mass Flow", New List(Of Double)({wnz(i)}))
                    results.DataUnits.Add("[Solid Phase] " + comp + " Mass Flow", units.massflow)
                    i += 1
                Next
                results.Data.Add("SPACE52SS", Nothing)
                For Each p As String In props
                    results.Data.Add("[Solid Phase] " & TranslateString(p), GetSinglePhaseProp2(p, "mass", "Solid").ConvertFromSI(GetPropUnits(p, "mass", units)))
                    results.DataUnits.Add("[Solid Phase] " & TranslateString(p), GetPropUnits(p, "mass", units))
                Next

            End If

            results.TextOutput += "Calculation results for Material Stream " & Me.GraphicObject.Tag & vbCrLf & "Compounds: " & PropertyPackage.RET_VNAMES.ToArrayString & System.Environment.NewLine

            Select Case Me.SpecType
                Case Enums.StreamSpec.Temperature_and_Pressure
                    results.TextOutput += "Specification: Temperature and Pressure" & System.Environment.NewLine
                    results.TextOutput += "Temperature: " & Phases(0).Properties.temperature.GetValueOrDefault.ConvertFromSI(units.temperature).ToString & " " & units.temperature & System.Environment.NewLine
                    results.TextOutput += "Pressure: " & Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure).ToString & " " & units.pressure & System.Environment.NewLine
                Case Enums.StreamSpec.Pressure_and_Enthalpy
                    results.TextOutput += "Specification: Pressure and Enthalpy" & System.Environment.NewLine
                    results.TextOutput += "Pressure: " & Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure).ToString & " " & units.pressure & System.Environment.NewLine
                    results.TextOutput += "Enthalpy: " & Phases(0).Properties.enthalpy.GetValueOrDefault.ConvertFromSI(units.enthalpy).ToString & " " & units.enthalpy & System.Environment.NewLine
                Case Enums.StreamSpec.Pressure_and_Entropy
                    results.TextOutput += "Specification: Pressure and Entropy" & System.Environment.NewLine
                    results.TextOutput += "Pressure: " & Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure).ToString & " " & units.pressure & System.Environment.NewLine
                    results.TextOutput += "Entropy: " & Phases(0).Properties.entropy.GetValueOrDefault.ConvertFromSI(units.entropy).ToString & " " & units.entropy & System.Environment.NewLine
                Case Enums.StreamSpec.Pressure_and_VaporFraction
                    results.TextOutput += "Specification: Pressure and Vapor Fraction" & System.Environment.NewLine
                    results.TextOutput += "Pressure: " & Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure).ToString & " " & units.pressure & System.Environment.NewLine
                    results.TextOutput += "Vapor Fraction: " & Phases(2).Properties.molarfraction.ToString & System.Environment.NewLine
                Case Enums.StreamSpec.Temperature_and_VaporFraction
                    results.TextOutput += "Specification: Temperature and Vapor Fraction" & System.Environment.NewLine
                    results.TextOutput += "Temperature: " & Phases(0).Properties.temperature.GetValueOrDefault.ConvertFromSI(units.temperature).ToString & " " & units.temperature & System.Environment.NewLine
                    results.TextOutput += "Vapor Fraction: " & Phases(2).Properties.molarfraction.ToString & System.Environment.NewLine
            End Select

            results.TextOutput += "Property Package: " & Me.PropertyPackage.ComponentName & System.Environment.NewLine & System.Environment.NewLine

            For Each d As KeyValuePair(Of String, List(Of Double)) In results.Data
                Dim id As String = d.Key
                If d.Key.Length > 36 Then id = d.Key.Substring(0, 36) & "..."
                If d.Key.Contains("SPACE") Then
                    results.TextOutput += System.Environment.NewLine
                Else
                    If d.Key.ToLower.Contains("heat capacity") Or d.Key.ToLower.Contains("enthalpy") Or d.Key.ToLower.Contains("entropy") Then
                        d.Value(0) /= 1000
                        results.TextOutput += (id.PadRight(40) & d.Value.ToArray.ToArrayString(CultureInfo.CurrentUICulture, numberformat).PadRight(20) & results.DataUnits(d.Key)) & System.Environment.NewLine
                    Else
                        results.TextOutput += (id.PadRight(40) & d.Value.ToArray.ToArrayString(CultureInfo.CurrentUICulture, numberformat).PadRight(20) & results.DataUnits(d.Key)) & System.Environment.NewLine
                    End If
                End If
            Next

            Return results.TextOutput

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            PropertyPackage.CurrentMaterialStream = Me

            Dim props As String() = PropertyPackage.GetSinglePhasePropList()
            Dim overallprops As String() = PropertyPackage.GetOverallPropList()
            Dim comps As String() = PropertyPackage.RET_VNAMES2(GetFlowsheet().FlowsheetOptions.CompoundOrderingMode)

            Dim units As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat
            Dim nff = GetFlowsheet().FlowsheetOptions.FractionNumberFormat

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Material Stream '" & Me.GraphicObject.Tag + "'"}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            Select Case Me.SpecType
                Case Enums.StreamSpec.Temperature_and_Pressure
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Specification: Temperature and Pressure"}))
                Case Enums.StreamSpec.Pressure_and_Enthalpy
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Specification: Pressure and Enthalpy"}))
                Case Enums.StreamSpec.Pressure_and_Entropy
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Specification: Pressure and Entropy"}))
                Case Enums.StreamSpec.Pressure_and_VaporFraction
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Specification: Pressure and Vapor Fraction"}))
                Case Enums.StreamSpec.Temperature_and_VaporFraction
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Specification: Temperature and Vapor Fraction"}))
            End Select

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Property Package: " & Me.PropertyPackage.ComponentName}))

            Dim ny, nl1, nl2, ns, vz(), wz(), vnz(), wnz() As Double, i As Integer

            ny = Phases(2).Properties.molarfraction.GetValueOrDefault
            nl1 = Phases(3).Properties.molarfraction.GetValueOrDefault
            nl2 = Phases(4).Properties.molarfraction.GetValueOrDefault
            ns = Phases(7).Properties.molarfraction.GetValueOrDefault

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Main Properties"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Stream Temperature",
                     Me.Phases(0).Properties.temperature.GetValueOrDefault.ConvertFromSI(units.temperature).ToString(nf),
                     units.temperature}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Stream Pressure",
                     Me.Phases(0).Properties.pressure.GetValueOrDefault.ConvertFromSI(units.pressure).ToString(nf),
                     units.pressure}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mixture Enthalpy",
                     Me.Phases(0).Properties.enthalpy.GetValueOrDefault.ConvertFromSI(units.enthalpy).ToString(nf),
                     units.enthalpy}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mixture Entropy",
                     Me.Phases(0).Properties.entropy.GetValueOrDefault.ConvertFromSI(units.entropy).ToString(nf),
                     units.entropy}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Vapor Phase Mole Fraction",
                     Me.Phases(2).Properties.molarfraction.GetValueOrDefault.ToString(nf),
                     ""}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Phase Mole Fractions"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Vapor",
                     ny.ToString(nf),
                     ""}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Liquid 1",
                     nl1.ToString(nf),
                     ""}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Liquid 2",
                     nl2.ToString(nf),
                     ""}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Solid",
                     ns.ToString(nf),
                     ""}))

            If (ny > 0.0000000001# And ny < 0.9999999999#) OrElse nl2 > 0.000000001# OrElse ns > 0.0# Then

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Flows"}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mass Flow",
                     Me.Phases(0).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow).ToString(nf),
                     units.massflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Molar Flow",
                     Me.Phases(0).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow).ToString(nf),
                     units.molarflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Volumetric Flow",
                     Me.Phases(0).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow).ToString(nf),
                     units.volumetricFlow}))

                vz = GetSinglePhaseProp2("fraction", "mole", "Overall").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Overall").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Overall").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Overall").ConvertFromSI(units.massflow).ToArray

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Compound Mole Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Compound Mass Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Compound Mole Flows (" + units.molarflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Compound Mass Flows (" + units.massflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Overall (Mixture) Properties"}))

                For Each p As String In overallprops
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Overall")
                    If propval.Count = 1 Then
                        If propname.ToLower.Contains("heat capacity") Or propname.ToLower.Contains("enthalpy") Or propname.ToLower.Contains("entropy") Then
                            propval(0) /= 1000
                        End If
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {propname,
                    propval(0).ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                    GetPropUnits(p, "mass", units)}))
                    End If
                Next

            End If

            If ny > 0.00001# Then

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Flows"}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mass Flow",
                     Me.Phases(2).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow).ToString(nf),
                     units.massflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Molar Flow",
                     Me.Phases(2).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow).ToString(nf),
                     units.molarflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Volumetric Flow",
                     Me.Phases(2).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow).ToString(nf),
                     units.volumetricFlow}))

                vz = GetSinglePhaseProp2("fraction", "mole", "Vapor").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Vapor").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Vapor").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Vapor").ConvertFromSI(units.massflow).ToArray

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Compound Mole Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Compound Mass Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Compound Mole Flows (" + units.molarflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Compound Mass Flows (" + units.massflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wnz(i).ToString(nff)}))
                    i += 1
                Next

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Vapor")
                    If propval.Count > 1 Then
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label,
                            New String() {"Vapor Phase " + propname.Replace("Coefficient", "Coefficients")}))
                        i = 0
                        For Each item In propval
                            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {comps(i),
                            item.ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                            GetPropUnits(p, "mass", units)}))
                            i += 1
                        Next
                    End If
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Vapor Phase Properties"}))

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Vapor")
                    If propval.Count = 1 Then
                        If propname.ToLower.Contains("heat capacity") Or propname.ToLower.Contains("enthalpy") Or propname.ToLower.Contains("entropy") Then
                            propval(0) /= 1000
                        End If
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {propname,
                    propval(0).ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                    GetPropUnits(p, "mass", units)}))
                    End If

                Next

            End If

            If nl1 > 0.00001# Then

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Flows"}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mass Flow",
                     Me.Phases(3).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow).ToString(nf),
                     units.massflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Molar Flow",
                     Me.Phases(3).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow).ToString(nf),
                     units.molarflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Volumetric Flow",
                     Me.Phases(3).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow).ToString(nf),
                     units.volumetricFlow}))

                vz = GetSinglePhaseProp2("fraction", "mole", "Liquid").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Liquid").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Liquid").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Liquid").ConvertFromSI(units.massflow).ToArray

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Compound Mole Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Compound Mass Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Compound Mole Flows (" + units.molarflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Compound Mass Flows (" + units.massflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wnz(i).ToString(nff)}))
                    i += 1
                Next

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Liquid")
                    If propval.Count > 1 Then
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label,
                            New String() {"Liquid Phase 1 " + propname.Replace("Coefficient", "Coefficients")}))
                        i = 0
                        For Each item In propval
                            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {comps(i),
                            item.ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                            GetPropUnits(p, "mass", units)}))
                            i += 1
                        Next
                    End If
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 1 Properties"}))

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Liquid")
                    If propval.Count = 1 Then
                        If propname.ToLower.Contains("heat capacity") Or propname.ToLower.Contains("enthalpy") Or propname.ToLower.Contains("entropy") Then
                            propval(0) /= 1000
                        End If
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {propname,
                    propval(0).ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                    GetPropUnits(p, "mass", units)}))
                    End If
                Next

            End If


            If nl2 > 0.00001# Then

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Flows"}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mass Flow",
                     Me.Phases(4).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow).ToString(nf),
                     units.massflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Molar Flow",
                     Me.Phases(4).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow).ToString(nf),
                     units.molarflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Volumetric Flow",
                     Me.Phases(4).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow).ToString(nf),
                     units.volumetricFlow}))

                vz = GetSinglePhaseProp2("fraction", "mole", "Liquid2").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Liquid2").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Liquid2").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Liquid2").ConvertFromSI(units.massflow).ToArray

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Compound Mole Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Compound Mass Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Compound Mole Flows (" + units.molarflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Compound Mass Flows (" + units.massflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wnz(i).ToString(nff)}))
                    i += 1
                Next

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Liquid2")
                    If propval.Count > 1 Then
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label,
                            New String() {"Liquid Phase 2 " + propname.Replace("Coefficient", "Coefficients")}))
                        i = 0
                        For Each item In propval
                            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {comps(i),
                            item.ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                            GetPropUnits(p, "mass", units)}))
                            i += 1
                        Next
                    End If
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Liquid Phase 2 Properties"}))

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Liquid2")
                    If propval.Count = 1 Then
                        If propname.ToLower.Contains("heat capacity") Or propname.ToLower.Contains("enthalpy") Or propname.ToLower.Contains("entropy") Then
                            propval(0) /= 1000
                        End If
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {propname,
                    propval(0).ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                    GetPropUnits(p, "mass", units)}))
                    End If
                Next

            End If

            If ns > 0.00001# Then

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Flows"}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Mass Flow",
                     Me.Phases(7).Properties.massflow.GetValueOrDefault.ConvertFromSI(units.massflow).ToString(nf),
                     units.massflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Molar Flow",
                     Me.Phases(7).Properties.molarflow.GetValueOrDefault.ConvertFromSI(units.molarflow).ToString(nf),
                     units.molarflow}))
                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                     New String() {"Volumetric Flow",
                     Me.Phases(7).Properties.volumetric_flow.GetValueOrDefault.ConvertFromSI(units.volumetricFlow).ToString(nf),
                     units.volumetricFlow}))

                vz = GetSinglePhaseProp2("fraction", "mole", "Solid").ToArray
                wz = GetSinglePhaseProp2("fraction", "mass", "Solid").ToArray
                vnz = GetSinglePhaseProp2("flow", "mole", "Solid").ConvertFromSI(units.molarflow).ToArray
                wnz = GetSinglePhaseProp2("flow", "mass", "Solid").ConvertFromSI(units.massflow).ToArray

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Compound Mole Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Compound Mass Fractions"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Compound Mole Flows (" + units.molarflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    vnz(i).ToString(nff)}))
                    i += 1
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Compound Mass Flows (" + units.massflow + ")"}))

                i = 0
                For Each comp As String In comps
                    list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.DoubleColumn,
                    New String() {comp,
                    wnz(i).ToString(nff)}))
                    i += 1
                Next

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Solid")
                    If propval.Count > 1 Then
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label,
                            New String() {"Solid Phase " + propname.Replace("Coefficient", "Coefficients")}))
                        i = 0
                        For Each item In propval
                            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {comps(i),
                            item.ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                            GetPropUnits(p, "mass", units)}))
                            i += 1
                        Next
                    End If
                Next

                list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Solid Phase Properties"}))

                For Each p As String In props
                    Dim propname = TranslateString(p)
                    Dim propval = GetSinglePhaseProp2(p, "mass", "Solid")
                    If propval.Count = 1 Then
                        If propname.ToLower.Contains("heat capacity") Or propname.ToLower.Contains("enthalpy") Or propname.ToLower.Contains("entropy") Then
                            propval(0) /= 1000
                        End If
                        list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                    New String() {propname,
                    propval(0).ConvertFromSI(GetPropUnits(p, "mass", units)).ToString(nf),
                    GetPropUnits(p, "mass", units)}))
                    End If
                Next

            End If

            Return list

        End Function

        Public Function GetSinglePhaseProp2(ByVal prop As String, ByVal basis As String, ByVal phaseLabel As String) As List(Of Double)
            Dim results As Object = Nothing
            Me.GetSinglePhaseProp(prop, phaseLabel, basis, results)
            Return DirectCast(results, Double()).ToList
        End Function

        Public Shared Function GetPropUnits(prop As String, basis As String, su As IUnitsOfMeasure) As String

            If prop.ToLower.Equals("criticalvolume") Then
                Return su.spec_vol
            ElseIf prop.ToLower.Equals("chemicalformula") Then
                Return ""
            ElseIf prop.ToLower.Equals("structureformula") Then
                Return ""
            ElseIf prop.ToLower.Equals("casregistrynumber") Then
                Return ""
            ElseIf prop.ToLower.Equals("temperature") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("boilingpointtemperature") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("criticaltemperature") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("temperatureoffusion") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("enthalpyoffusion") Then
                Return su.molar_enthalpy
            ElseIf prop.ToLower.Equals("normalboilingpoint") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("boilingpointtemperature") Then
                Return su.temperature
            ElseIf prop.ToLower.Equals("pressure") Then
                Return su.pressure
            ElseIf prop.ToLower.Equals("vaporpressure") Then
                Return su.pressure
            ElseIf prop.ToLower.Equals("criticalpressure") Then
                Return su.pressure
            ElseIf prop.ToLower.Equals("flow") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.massflow
                ElseIf basis.ToLower.Equals("mole") Then
                    Return su.molarflow
                Else
                    Return su.volumetricFlow
                End If
            ElseIf prop.ToLower.Equals("compressibilityfactor") Then
                Return ""
            ElseIf prop.ToLower.Equals("excessenthalpy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("enthalpy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("internalenergy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("gibbsenergy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("helmholtzenergy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("enthalpyf") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("enthalpynf") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("heatofvaporization") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("idealgasenthalpy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("idealgasenthalpyofformationat25c") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("idealgasgibbsfreeenergyofformationat25c") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.enthalpy
                Else
                    Return su.molar_enthalpy
                End If
            ElseIf prop.ToLower.Equals("excessentropy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("entropy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("entropyf") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("entropynf") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("idealgasentropy") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("heatcapacitycp") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("idealgasheatcapacity") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("heatcapacityofliquid") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("heatcapacityofsolid") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("heatcapacitycv") Then
                If basis.ToLower.Equals("mass") Then
                    Return su.entropy
                Else
                    Return su.molar_entropy
                End If
            ElseIf prop.ToLower.Equals("isothermalcompressibility") Then
                Return su.compressibility
            ElseIf prop.ToLower.Equals("bulkmodulus") Then
                Return su.pressure
            ElseIf prop.ToLower.Equals("joulethomsoncoefficient") Then
                Return su.jouleThomsonCoefficient
            ElseIf prop.ToLower.Equals("speedofsound") Then
                Return su.speedOfSound
            ElseIf prop.ToLower.Equals("viscosity") Then
                Return su.viscosity
            ElseIf prop.ToLower.Equals("viscosityofliquid") Then
                Return su.viscosity
            ElseIf prop.ToLower.Equals("viscosityofvapor") Then
                Return su.viscosity
            ElseIf prop.ToLower.Equals("thermalconductivity") Then
                Return su.thermalConductivity
            ElseIf prop.ToLower.Equals("thermalconductivityofliquid") Then
                Return su.thermalConductivity
            ElseIf prop.ToLower.Equals("thermalconductivityofvapor") Then
                Return su.thermalConductivity
            ElseIf prop.ToLower.Equals("fugacity") Then
                Return su.pressure
            ElseIf prop.ToLower.Equals("fugacitycoefficient") Then
                Return ""
            ElseIf prop.ToLower.Equals("activitycoefficient") Then
                Return ""
            ElseIf prop.ToLower.Equals("logfugacitycoefficient") Then
                Return ""
            ElseIf prop.ToLower.Equals("volume") Then
                Return su.molar_volume
            ElseIf prop.ToLower.Equals("density") Then
                Return su.density
            ElseIf prop.ToLower.Equals("densityofsolid") Then
                Return su.density
            ElseIf prop.ToLower.Equals("densityofliquid") Then
                Return su.density
            ElseIf prop.ToLower.Equals("molecularweight") Then
                Return su.molecularWeight
            Else
                Return ""
            End If

        End Function

        Public Function TranslateString(s As String) As String

            If s.ToLower.Equals("criticalvolume") Then
                Return "Critical Volume"
            ElseIf s.ToLower.Equals("criticalcompressibilityfactor") Then
                Return "Critical Compressibility Factor"
            ElseIf s.ToLower.Equals("acentricfactor") Then
                Return "Acentric Factor"
            ElseIf s.ToLower.Equals("chemicalformula") Then
                Return "Chemical Formula"
            ElseIf s.ToLower.Equals("structureformula") Then
                Return "Structure formula"
            ElseIf s.ToLower.Equals("casregistrynumber") Then
                Return "CAS Registry Number"
            ElseIf s.ToLower.Equals("temperature") Then
                Return "Temperature"
            ElseIf s.ToLower.Equals("boilingpointtemperature") Then
                Return "Boiling Point Temperature"
            ElseIf s.ToLower.Equals("criticaltemperature") Then
                Return "Critical Temperature"
            ElseIf s.ToLower.Equals("normalboilingpoint") Then
                Return "Normal Boiling Point"
            ElseIf s.ToLower.Equals("boilingpointtemperature") Then
                Return "Boiling Point Temperature"
            ElseIf s.ToLower.Equals("pressure") Then
                Return "Pressure"
            ElseIf s.ToLower.Equals("vaporpressure") Then
                Return "Vapor Pressure"
            ElseIf s.ToLower.Equals("criticalpressure") Then
                Return "Critical Pressure"
            ElseIf s.ToLower.Equals("flow") Then
                Return "Vazão"
            ElseIf s.ToLower.Equals("compressibilityfactor") Then
                Return "Compressibility Factor"
            ElseIf s.ToLower.Equals("excessenthalpy") Then
                Return "Excess Enthalpy"
            ElseIf s.ToLower.Equals("enthalpy") Then
                Return "Enthalpy"
            ElseIf s.ToLower.Equals("heatofvaporization") Then
                Return "Heat of Vaporization"
            ElseIf s.ToLower.Equals("idealgasenthalpy") Then
                Return "Ideal gas Enthalpy"
            ElseIf s.ToLower.Equals("idealgasenthalpyofformationat25c") Then
                Return "Ideal gas Enthalpy of Formation at 25 C"
            ElseIf s.ToLower.Equals("idealgasgibbsfreeenergyofformationat25c") Then
                Return "Ideal Gas Gibbs Free Energy of Formation at 25 C"
            ElseIf s.ToLower.Equals("excessentropy") Then
                Return "Excess Entropy"
            ElseIf s.ToLower.Equals("entropy") Then
                Return "Entropy"
            ElseIf s.ToLower.Equals("idealgasentropy") Then
                Return "Ideal Gas Entropy"
            ElseIf s.ToLower.Equals("heatcapacitycp") Then
                Return "Heat Capacity Cp"
            ElseIf s.ToLower.Equals("idealgasheatcapacity") Then
                Return "Ideal Gas Heat Capacity"
            ElseIf s.ToLower.Equals("heatcapacityofliquid") Then
                Return "Liquid Heat Capacity"
            ElseIf s.ToLower.Equals("heatcapacityofsolid") Then
                Return "Solid Heat Capacity"
            ElseIf s.ToLower.Equals("heatcapacitycv") Then
                Return "Heat Capacity Cv"
            ElseIf s.ToLower.Equals("viscosity") Then
                Return "Viscosity"
            ElseIf s.ToLower.Equals("viscosityofliquid") Then
                Return "Liquid Viscosity"
            ElseIf s.ToLower.Equals("viscosityofvapor") Then
                Return "Vapor Viscosity"
            ElseIf s.ToLower.Equals("thermalconductivity") Then
                Return "Thermal Conductivity"
            ElseIf s.ToLower.Equals("thermalconductivityofliquid") Then
                Return "Liquid Thermal Conductivity"
            ElseIf s.ToLower.Equals("thermalconductivityofvapor") Then
                Return "Vapor Thermal Conductivity"
            ElseIf s.ToLower.Equals("fugacity") Then
                Return "Fugacity"
            ElseIf s.ToLower.Equals("fugacitycoefficient") Then
                Return "Fugacity Coefficient"
            ElseIf s.ToLower.Equals("activitycoefficient") Then
                Return "Activity Coefficient"
            ElseIf s.ToLower.Equals("logfugacitycoefficient") Then
                Return "Log Fugacity Coefficient"
            ElseIf s.ToLower.Equals("volume") Then
                Return "Volume"
            ElseIf s.ToLower.Equals("density") Then
                Return "Density"
            ElseIf s.ToLower.Equals("densityofsolid") Then
                Return "Solid Density"
            ElseIf s.ToLower.Equals("densityofliquid") Then
                Return "Liquid Density"
            ElseIf s.ToLower.Equals("molecularweight") Then
                Return "Molecular Weight"
            ElseIf s.ToLower.Equals("isothermalcompressibility") Then
                Return "Isothermal Compressibility"
            ElseIf s.ToLower.Equals("bulkmodulus") Then
                Return "Bulk Modulus"
            ElseIf s.ToLower.Equals("joulethomsoncoefficient") Then
                Return "Joule Thomson Coefficient"
            ElseIf s.ToLower.Equals("internalenergy") Then
                Return "Internal Energy"
            ElseIf s.ToLower.Equals("gibbsenergy") Then
                Return "Gibbs Free Energy"
            ElseIf s.ToLower.Equals("helmholtzenergy") Then
                Return "Helmholtz Free Energy"
            ElseIf s.ToLower.Equals("speedofsound") Then
                Return "Speed of Sound"
            ElseIf s.ToLower.Equals("smiles") Then
                Return "SMILES"
            ElseIf s.ToLower.Equals("inchi") Then
                Return "InChI"
            ElseIf s.ToLower.Equals("temperatureoffusion") Then
                Return "Temperature of Fusion"
            ElseIf s.ToLower.Equals("enthalpyoffusion") Then
                Return "Enthalpy of Fusion"
            ElseIf s.ToLower.Equals("uniquacr") Then
                Return "UNIQUAC R"
            ElseIf s.ToLower.Equals("uniquacq") Then
                Return "UNIQUAC Q"
            ElseIf s.ToLower.Equals("unifac_info") Then
                Return "Has UNIFAC Structure Info"
            ElseIf s.ToLower.Equals("modfac_info") Then
                Return "Has Modified UNIFAC Structure Info"
            Else
                Return s
            End If

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String

            If p.Equals("Flash Specification") Then
                Return "Select a pair of properties to specify the thermodynamic state of the stream's mixture."
            ElseIf p.Equals("Temperature") Then
                Return "Enter the temperature of the stream if the Flash Spec is T/P or T/VF, otherwise it will be calculated."
            ElseIf p.Equals("Pressure") Then
                Return "Enter the pressure of the stream if the Flash Spec is T/P, P/H, P/S or P/VF, otherwise it will be calculated."
            ElseIf p.Equals("Specific Enthalpy") Then
                Return "Enter the enthalpy of the stream if the Flash Spec is P/H, otherwise it will be calculated."
            ElseIf p.Equals("Specific Entropy") Then
                Return "Enter the entropy of the stream if the Flash Spec is P/S, otherwise it will be calculated."
            ElseIf p.Equals("Mass Flow") Then
                Return "Enter the Mass flow of the stream. Molar and Volumetric ones will be calculated to match this value."
            ElseIf p.Equals("Molar Flow") Then
                Return "Enter the Molar flow of the stream. Mass and Volumetric ones will be calculated to match this value."
            ElseIf p.Equals("Volumetric Flow") Then
                Return "Enter the Volumetric flow of the stream. Molar and Mass ones will be calculated to match this value."
            ElseIf p.Equals("Vapor Phase Mole Fraction (spec)") Then
                Return "If the Flash Spec is T/VF or P/VF, enter the vapor phase mole fraction (quality) of the stream, otherwise it will be calculated."
            Else
                Return p
            End If

        End Function

        ' shortcut functions

        ''' <summary>
        ''' Sets stream temperature.
        ''' </summary>
        ''' <param name="value">Temperature in K</param>
        Public Sub SetTemperature(value As Double)
            Phases(0).Properties.temperature = value
        End Sub

        ''' <summary>
        ''' Sets stream pressure
        ''' </summary>
        ''' <param name="value">Pressure in Pa</param>
        Public Sub SetPressure(value As Double)
            Phases(0).Properties.pressure = value
        End Sub

        ''' <summary>
        ''' Sets stream enthalpy.
        ''' </summary>
        ''' <param name="value">Enthalpy in kJ/kg</param>
        Public Sub SetMassEnthalpy(value As Double)
            Phases(0).Properties.enthalpy = value
        End Sub

        ''' <summary>
        ''' Sets stream entropy.
        ''' </summary>
        ''' <param name="value">Entropy in kJ/[kg.K]</param>
        Public Sub SetMassEntropy(value As Double)
            Phases(0).Properties.entropy = value
        End Sub

        ''' <summary>
        ''' Sets stream mass flow.
        ''' </summary>
        ''' <param name="value">Flow in kg/s</param>
        Public Sub SetMassFlow(value As Double)
            Phases(0).Properties.massflow = value
            Phases(0).Properties.molarflow = Nothing
            Phases(0).Properties.volumetric_flow = Nothing
        End Sub

        ''' <summary>
        ''' Sets stream molar flow.
        ''' </summary>
        ''' <param name="value">Flow in mol/s</param>
        Public Sub SetMolarFlow(value As Double)
            Phases(0).Properties.massflow = Nothing
            Phases(0).Properties.molarflow = value
            Phases(0).Properties.volumetric_flow = Nothing
        End Sub

        ''' <summary>
        ''' Sets stream volumetric flow.
        ''' </summary>
        ''' <param name="value">Flow in m3/s</param>
        Public Sub SetVolumetricFlow(value As Double)
            Phases(0).Properties.massflow = Nothing
            Phases(0).Properties.molarflow = Nothing
            Phases(0).Properties.volumetric_flow = value
        End Sub

        ''' <summary>
        ''' Sets stream flash spec.
        ''' </summary>
        ''' <param name="value">Flash spec (PT, PH, PS, PVF, TVF or PSF).</param>
        Public Sub SetFlashSpec(value As String)
            Select Case value.ToLower
                Case "pt", "tp"
                    SpecType = StreamSpec.Temperature_and_Pressure
                Case "ph"
                    SpecType = StreamSpec.Pressure_and_Enthalpy
                Case "ps"
                    SpecType = StreamSpec.Pressure_and_Entropy
                Case "pvf"
                    SpecType = StreamSpec.Pressure_and_VaporFraction
                Case "tvf"
                    SpecType = StreamSpec.Temperature_and_VaporFraction
                Case "psf"
                    SpecType = StreamSpec.Pressure_and_SolidFraction
            End Select
        End Sub

    End Class

    Public Class CalculationResults

        Public Property Data As Dictionary(Of String, List(Of Double))
        Public Property DataUnits As Dictionary(Of String, String)
        Public Property CompoundData As Dictionary(Of String, Object)
        Public Property TextOutput As String = ""
        Public Property Units As SystemsOfUnits.Units
        Public Property ExceptionResult As Exception
        Public Property NumberFormat As String = "0.####"
        Public Property Language As String = "en"

        Sub New()
            Data = New Dictionary(Of String, List(Of Double))
            DataUnits = New Dictionary(Of String, String)
            CompoundData = New Dictionary(Of String, Object)
            Units = New SystemsOfUnits.SI
        End Sub

    End Class


End Namespace

Namespace Streams.Editors

    Public Class MaterialStreamEditorState

        Public Property MainSelectedTab As Integer = 0
        Public Property InputCompositionBasis As Integer = 0
        Public Property CompoundsSelectedTab As Integer = 0
        Public Property CompoundsAmountBasis As Integer = 0
        Public Property CompoundsProperty As Integer = 0
        Public Property CompoundsAmountSelectedTab As Integer = 0
        Public Property CompoundsPropertySelectedTab As Integer = 0
        Public Property PhasePropsSelectedTab As Integer = 0
        Public Property MainSelectedTab0 As Integer = 0

    End Class

End Namespace