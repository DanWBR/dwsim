Imports System.Globalization
Imports System.Reflection
Imports DWSIM.Interfaces
Imports DWSIM.Drawing.SkiaSharp
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.FlowsheetSolver
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports DWSIM.UnitOperations.SpecialOps
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.UnitOperations.Reactors
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.UnitOperations.Streams
Imports DWSIM.Thermodynamics.Streams
Imports ICSharpCode.SharpZipLib.Zip
Imports System.IO
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables
Imports Python.Runtime
Imports Microsoft.Scripting.Hosting
Imports System.Text
Imports DWSIM.SharedClasses.Flowsheet
Imports System.Dynamic
Imports DWSIM.Interfaces.Enums
Imports DWSIM.GlobalSettings
Imports DWSIM.Thermodynamics.AdvancedEOS
Imports SkiaSharp
Imports System.Text.RegularExpressions
Imports System.Xml

<System.Runtime.InteropServices.ComVisible(True)> Public MustInherit Class FlowsheetBase

    Implements IFlowsheet, IFlowsheetCalculationQueue

    Public Property WeatherProvider As IWeatherProvider = New SharedClasses.WeatherProvider() Implements IFlowsheet.WeatherProvider

    Public Property DynamicMode As Boolean = False Implements IFlowsheet.DynamicMode

    Public Property DynamicsManager As IDynamicsManager = New DynamicsManager.Manager Implements IFlowsheet.DynamicsManager

    Public Property ExtraProperties As New ExpandoObject Implements IFlowsheet.ExtraProperties

    Public WithEvents Options As New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables

    Public UndoStack As New Stack(Of Tuple(Of Enums.SnapshotType, XDocument))
    Public RedoStack As New Stack(Of Tuple(Of Enums.SnapshotType, XDocument))

    Public Property MessagesLog As New List(Of String) Implements IFlowsheet.MessagesLog

    Private FlowsheetSurface As New GraphicsSurface

    Public SensAnalysisCollection As New List(Of Optimization.SensitivityAnalysisCase)

    Public OptimizationCollection As New List(Of Optimization.OptimizationCase)

    Private Shared AvailablePropPacks As New Dictionary(Of String, IPropertyPackage)

    Public Property AvailablePropertyPackages As Dictionary(Of String, IPropertyPackage) Implements IFlowsheet.AvailablePropertyPackages
        Get
            Return AvailablePropPacks
        End Get
        Set(value As Dictionary(Of String, IPropertyPackage))
            AvailablePropPacks = value
        End Set
    End Property

    Public Property AvailableSystemsOfUnits As New List(Of IUnitsOfMeasure) Implements IFlowsheet.AvailableSystemsOfUnits

    Public Property ExternalUnitOperations As New Dictionary(Of String, IExternalUnitOperation)

    Private loaded As Boolean = False

    Private rm, prm As Resources.ResourceManager

    Public LoadSpreadsheetData, SaveSpreadsheetData As Action(Of XDocument)

    Public GetSpreadsheetObjectFunc As Func(Of Object)

    Public RetrieveSpreadsheetData As Func(Of String, List(Of String()))

    Public RetrieveSpreadsheetFormat As Func(Of String, List(Of String()))

    Public Property ScriptKeywordsF As String = ""

    Public Property ScriptKeywordsU As String = ""

    Protected _translatefunction As Func(Of String, String)

    Public Sub AddCompoundsToMaterialStream(ms As IMaterialStream) Implements IFlowsheet.AddCompoundsToMaterialStream
        For Each phase As IPhase In ms.Phases.Values
            For Each comp In Me.Options.SelectedComponents.Values
                With phase
                    .Compounds.Add(comp.Name, New Compound(comp.Name, ""))
                    .Compounds(comp.Name).ConstantProperties = comp
                End With
            Next
        Next
        DirectCast(ms, MaterialStream).EqualizeOverallComposition()
        DirectCast(ms, MaterialStream).CalcOverallCompMassFractions()
    End Sub

    Public Sub AddGraphicObject(obj As IGraphicObject) Implements IFlowsheet.AddGraphicObject
        FlowsheetSurface.DrawingObjects.Add(obj)
        GraphicObjects.Add(obj.Name, obj)
    End Sub

    Public Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, tag As String) As ISimulationObject Implements IFlowsheet.AddObject
        Dim id = Me.AddObjectToSurface(t, xcoord, ycoord, tag)
        Return Me.SimulationObjects(id)
    End Function

    Public Function AddObject(t As Enums.GraphicObjects.ObjectType, xcoord As Integer, ycoord As Integer, id As String, tag As String) As ISimulationObject Implements IFlowsheet.AddObject
        Me.AddObjectToSurface(t, xcoord, ycoord, tag, id)
        Return Me.SimulationObjects(id)
    End Function

    Public Sub AddPropertyPackage(obj As IPropertyPackage) Implements IFlowsheet.AddPropertyPackage
        If obj.UniqueID = "" Then obj.UniqueID = Guid.NewGuid().ToString()
        If obj.Tag = "" Then obj.Tag = obj.GetType().Name
        obj.Flowsheet = Me
        Me.Options.PropertyPackages.Add(obj.UniqueID, obj)
    End Sub

    Public Sub AddSimulationObject(obj As ISimulationObject) Implements IFlowsheet.AddSimulationObject
        SimulationObjects.Add(obj.Name, obj)
    End Sub

    Public Sub CheckStatus() Implements IFlowsheet.CheckStatus
        FlowsheetSolver.FlowsheetSolver.CheckCalculatorStatus()
    End Sub

    Public Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer) Implements IFlowsheet.ConnectObjects
        RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        FlowsheetSurface.ConnectObject(CType(gobjfrom, GraphicObject), CType(gobjto, GraphicObject), fromidx, toidx)
    End Sub

    Public Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject) Implements IFlowsheet.DisconnectObjects
        RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        FlowsheetSurface.DisconnectObject(CType(gobjfrom, GraphicObject), CType(gobjto, GraphicObject), False)
    End Sub

    Public Sub DeleteSelectedObject(ByVal sender As System.Object, ByVal e As System.EventArgs, gobj As IGraphicObject, Optional ByVal confirmation As Boolean = True, Optional ByVal triggercalc As Boolean = False) Implements IFlowsheet.DeleteSelectedObject

        RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)

        If Not gobj Is Nothing Then

            Dim namesel As String = gobj.Name
            If Not gobj.IsConnector Then

                If gobj.IsEnergyStream Then

                    Dim InCon, OutCon As IConnectionPoint
                    For Each InCon In gobj.InputConnectors
                        If InCon.IsAttached = True Then DisconnectObjects(InCon.AttachedConnector.AttachedFrom, gobj)
                    Next
                    For Each OutCon In gobj.OutputConnectors
                        If OutCon.IsAttached = True Then DisconnectObjects(gobj, OutCon.AttachedConnector.AttachedTo)
                    Next

                    'DWSIM

                    Me.SimulationObjects.Remove(namesel)
                    Me.GraphicObjects.Remove(namesel)
                    Me.FlowsheetSurface.DeleteSelectedObject(gobj)

                Else

                    If gobj.ObjectType = ObjectType.GO_Table Or
                        gobj.ObjectType = ObjectType.GO_FloatingTable Or
                        gobj.ObjectType = ObjectType.GO_MasterTable Or
                        gobj.ObjectType = ObjectType.GO_SpreadsheetTable Or
                        gobj.ObjectType = ObjectType.GO_Text Or
                        gobj.ObjectType = ObjectType.GO_HTMLText Or
                        gobj.ObjectType = ObjectType.GO_Button Or
                        gobj.ObjectType = ObjectType.GO_Chart Then

                        Me.FlowsheetSurface.DeleteSelectedObject(gobj)

                    Else

                        Dim obj As ISimulationObject = Me.SimulationObjects(gobj.Name)

                        If gobj.EnergyConnector.IsAttached = True Then DisconnectObjects(gobj, gobj.EnergyConnector.AttachedConnector.AttachedTo)

                        Dim InCon, OutCon As IConnectionPoint
                        For Each InCon In gobj.InputConnectors
                            Try
                                If InCon.IsAttached = True Then DisconnectObjects(InCon.AttachedConnector.AttachedFrom, gobj)
                            Catch ex As Exception

                            End Try
                        Next

                        For Each OutCon In gobj.OutputConnectors
                            Try
                                If OutCon.IsAttached = True Then DisconnectObjects(gobj, OutCon.AttachedConnector.AttachedTo)
                            Catch ex As Exception

                            End Try
                        Next

                        If gobj.ObjectType = ObjectType.OT_Spec Then
                            Dim specobj As Spec = CType(SimulationObjects(namesel), Spec)
                            If Me.SimulationObjects.ContainsKey(specobj.TargetObjectData.ID) Then
                                Me.SimulationObjects(specobj.TargetObjectData.ID).IsSpecAttached = False
                                Me.SimulationObjects(specobj.TargetObjectData.ID).AttachedSpecId = ""
                            End If
                            If Me.SimulationObjects.ContainsKey(specobj.SourceObjectData.ID) Then
                                Me.SimulationObjects(specobj.SourceObjectData.ID).IsSpecAttached = False
                                Me.SimulationObjects(specobj.SourceObjectData.ID).AttachedSpecId = ""
                            End If
                        ElseIf gobj.ObjectType = ObjectType.OT_Adjust Then
                            Dim adjobj As Adjust = CType(SimulationObjects(namesel), Adjust)
                            If Me.SimulationObjects.ContainsKey(adjobj.ManipulatedObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ManipulatedObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ManipulatedObjectData.ID).AttachedAdjustId = ""
                            End If
                            If Me.SimulationObjects.ContainsKey(adjobj.ControlledObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ControlledObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ControlledObjectData.ID).AttachedAdjustId = ""
                            End If
                            If Me.SimulationObjects.ContainsKey(adjobj.ReferencedObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ReferencedObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ReferencedObjectData.ID).AttachedAdjustId = ""
                            End If
                        ElseIf gobj.ObjectType = ObjectType.Controller_PID Then
                            Dim adjobj As PIDController = CType(SimulationObjects(namesel), PIDController)
                            If Me.SimulationObjects.ContainsKey(adjobj.ManipulatedObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ManipulatedObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ManipulatedObjectData.ID).AttachedAdjustId = ""
                            End If
                            If Me.SimulationObjects.ContainsKey(adjobj.ControlledObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ControlledObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ControlledObjectData.ID).AttachedAdjustId = ""
                            End If
                            If Me.SimulationObjects.ContainsKey(adjobj.ReferencedObjectData.ID) Then
                                Me.SimulationObjects(adjobj.ReferencedObjectData.ID).IsAdjustAttached = False
                                Me.SimulationObjects(adjobj.ReferencedObjectData.ID).AttachedAdjustId = ""
                            End If
                        End If

                        Me.SimulationObjects.Remove(namesel)
                        Me.GraphicObjects.Remove(namesel)

                        Me.FlowsheetSurface.DeleteSelectedObject(gobj)

                    End If


                End If

            End If

        End If

    End Sub

    Public MustOverride Sub DisplayForm(form As Object) Implements IFlowsheet.DisplayForm

    Public Property FilePath As String Implements IFlowsheet.FilePath

    Public ReadOnly Property FlowsheetOptions As IFlowsheetOptions Implements IFlowsheet.FlowsheetOptions
        Get
            Return Options
        End Get
    End Property

    Public Function GetFlowsheetGraphicObject(tag As String) As IGraphicObject

        Return SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = tag).FirstOrDefault?.GraphicObject

    End Function

    Public Function GetFlowsheetSimulationObject(tag As String) As ISimulationObject Implements IFlowsheet.GetFlowsheetSimulationObject

        Return SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = tag).FirstOrDefault

    End Function

    Public MustOverride Function GetNewInstance() As IFlowsheet Implements IFlowsheet.GetNewInstance

    Public Function GetSelectedFlowsheetSimulationObject(tag As String) As ISimulationObject Implements IFlowsheet.GetSelectedFlowsheetSimulationObject
        If tag Is Nothing OrElse tag = "" Then
            If FlowsheetSurface.SelectedObject IsNot Nothing Then
                If SimulationObjects.ContainsKey(FlowsheetSurface.SelectedObject.Name) Then Return SimulationObjects(FlowsheetSurface.SelectedObject.Name) Else Return Nothing
            Else
                Return Nothing
            End If
        Else
            Return SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = tag).FirstOrDefault
        End If
    End Function

    Public Function GetSurface() As Object Implements IFlowsheet.GetSurface
        Return FlowsheetSurface
    End Function

    Public Function GetSurfaceControl() As Object Implements IFlowsheet.GetSurfaceControl
        Return Nothing
    End Function

    Public Function GetTranslatedString(text As String) As String Implements IFlowsheet.GetTranslatedString

        If _translatefunction IsNot Nothing Then
            Return _translatefunction.Invoke(text)
        End If

        If rm Is Nothing Then
            rm = New Resources.ResourceManager("DWSIM.FlowsheetBase.Strings", MyBase.GetType.GetTypeInfo.BaseType.GetTypeInfo.Assembly)
        End If

        Dim ttext As String = rm.GetString(text)

        If Not ttext Is Nothing AndAlso Not ttext.Equals(text) Then
            Return ttext
        Else
            If prm Is Nothing Then
                prm = New Resources.ResourceManager("DWSIM.FlowsheetBase.Properties", MyBase.GetType.GetTypeInfo.BaseType.GetTypeInfo.Assembly)
            End If
            Try
                If text.Split("/"c).Length = 2 Then
                    Dim prop As String = text.Split("/"c)(0)
                    Dim ttext2 As String = prm.GetString(prop)
                    If ttext2 Is Nothing Then Return text Else Return ttext2 + " / " + text.Split("/"c)(1)
                Else
                    Dim ttext2 As String = prm.GetString(text)
                    If ttext2 Is Nothing Then Return text Else Return ttext2
                End If
            Catch ex As Exception
                Return text
            End Try
        End If

    End Function

    Public Function GetTranslatedString(text As String, locale As String) As String Implements IFlowsheet.GetTranslatedString
        Return GetTranslatedString(text)
    End Function

    Public Property GraphicObjects As New Dictionary(Of String, IGraphicObject) Implements IFlowsheet.GraphicObjects

    Public Property PropertyPackages As Dictionary(Of String, IPropertyPackage) Implements IFlowsheet.PropertyPackages
        Get
            Return Options.PropertyPackages
        End Get
        Set(value As Dictionary(Of String, IPropertyPackage))
            Options.PropertyPackages = value
        End Set
    End Property

    Public Property Reactions As Dictionary(Of String, IReaction) Implements IFlowsheet.Reactions
        Get
            Return Options.Reactions
        End Get
        Set(value As Dictionary(Of String, IReaction))
            Options.Reactions = value
        End Set
    End Property

    Public Property ReactionSets As Dictionary(Of String, IReactionSet) Implements IFlowsheet.ReactionSets
        Get
            Return Options.ReactionSets
        End Get
        Set(value As Dictionary(Of String, IReactionSet))
            Options.ReactionSets = value
        End Set
    End Property

    Public Property RedirectMessages As Boolean Implements IFlowsheet.RedirectMessages

    Public Sub Solve()

        RequestCalculation()

    End Sub

    Public Sub RequestCalculation(Optional sender As ISimulationObject = Nothing, Optional ChangeCalculationOrder As Boolean = False) Implements IFlowsheet.RequestCalculation

        If Not sender Is Nothing Then

            'Call function to calculate flowsheet
            Dim objargs As New CalculationArgs
            With objargs
                .Calculated = False
                .Tag = sender.GraphicObject.Tag
                .Name = sender.Name
                .ObjectType = sender.GraphicObject.ObjectType
                .Sender = "PropertyGrid"
            End With

            CalculationQueue.Enqueue(objargs)

            Task.Factory.StartNew(Sub()
                                      FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode, , True)
                                  End Sub)

        Else

            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode, ChangeCalcOrder:=ChangeCalculationOrder)

        End If

    End Sub

    Public Sub RequestCalculation2(Wait As Boolean) Implements IFlowsheet.RequestCalculation2

        If Wait Then
            Task.Factory.StartNew(Sub()
                                      FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode)
                                  End Sub)
        Else
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode)
        End If

    End Sub

    Public Sub RequestCalculation3(Obj As ISimulationObject, Wait As Boolean) Implements IFlowsheet.RequestCalculation3

        If Obj Is Nothing Then
            RequestCalculation2(Wait)
        Else
            RequestCalculation(Obj)
        End If

    End Sub

    Public Sub ResetCalculationStatus() Implements IFlowsheet.ResetCalculationStatus

        For Each obj In SimulationObjects.Values
            obj.SetDirtyStatus(True)
            obj.Calculated = False
            obj.GraphicObject.Calculated = False
        Next

    End Sub

    Public Property SelectedCompounds As Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.SelectedCompounds
        Get
            Select Case Options.CompoundOrderingMode
                Case CompoundOrdering.CAS_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.CAS_Number).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.CAS_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.CAS_Number).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.MW_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Molar_Weight).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.MW_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Molar_Weight).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.Name_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Name).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.Name_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Name).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.NBP_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.NBP.GetValueOrDefault).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.NBP_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.NBP.GetValueOrDefault).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.TAG_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Tag).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.TAG_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Tag).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case Else
                    Return Options.SelectedComponents
            End Select
        End Get
        Set(value As Dictionary(Of String, ICompoundConstantProperties))
            Options.SelectedComponents = value
        End Set
    End Property

    Public MustOverride Sub ShowDebugInfo(text As String, level As Integer) Implements IFlowsheet.ShowDebugInfo

    Public MustOverride Sub ShowMessage(text As String, mtype As IFlowsheet.MessageType, Optional ByVal exceptionID As String = "") Implements IFlowsheet.ShowMessage

    Public Property SimulationObjects As New Dictionary(Of String, ISimulationObject) Implements IFlowsheet.SimulationObjects

    Public MustOverride Sub UpdateOpenEditForms() Implements IFlowsheet.UpdateOpenEditForms

    Public MustOverride Sub CloseOpenEditForms() Implements IFlowsheet.CloseOpenEditForms

    Public Property CalculationQueue As New Queue(Of ICalculationArgs) Implements IFlowsheetCalculationQueue.CalculationQueue

    Public Function AddObject(ByVal typename As String, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "",
                              Optional ByVal id As String = "", Optional CreateConnected As Boolean = False) As ISimulationObject

        Select Case typename

            Case "Controller Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Adjust, x, y, tag,,, CreateConnected))

            Case "Specification Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Spec, x, y, tag,,, CreateConnected))

            Case "Recycle Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Recycle, x, y, tag,,, CreateConnected))

            Case "Energy Recycle Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_EnergyRecycle, x, y, tag,,, CreateConnected))

            Case "Stream Mixer"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.NodeIn, x, y, tag,,, CreateConnected))

            Case "Stream Splitter"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.NodeOut, x, y, tag,,, CreateConnected))

            Case "Pump"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Pump, x, y, tag,,, CreateConnected))

            Case "Tank"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Tank, x, y, tag,,, CreateConnected))

            Case "Gas-Liquid Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Vessel, x, y, tag,,, CreateConnected))

            Case "Material Stream"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.MaterialStream, x, y, tag,,, CreateConnected))

            Case "Energy Stream"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.EnergyStream, x, y, tag,,, CreateConnected))

            Case "Compressor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Compressor, x, y, tag,,, CreateConnected))

            Case "Expander (Turbine)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Expander, x, y, tag,,, CreateConnected))

            Case "Heater"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Heater, x, y, tag,,, CreateConnected))

            Case "Cooler"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Cooler, x, y, tag,,, CreateConnected))

            Case "Pipe Segment"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Pipe, x, y, tag,,, CreateConnected))

            Case "Valve"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Valve, x, y, tag,,, CreateConnected))

            Case "Conversion Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Conversion, x, y, tag,,, CreateConnected))

            Case "Equilibrium Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Equilibrium, x, y, tag,,, CreateConnected))

            Case "Gibbs Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Gibbs, x, y, tag,,, CreateConnected))

            Case "Plug-Flow Reactor (PFR)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_PFR, x, y, tag,,, CreateConnected))

            Case "Continuous Stirred Tank Reactor (CSTR)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_CSTR, x, y, tag,,, CreateConnected))

            Case "Heat Exchanger"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.HeatExchanger, x, y, tag,,, CreateConnected))

            Case "Shortcut Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ShortcutColumn, x, y, tag,,, CreateConnected))

            Case "Distillation Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.DistillationColumn, x, y, tag,,, CreateConnected))

            Case "Absorption Column", "Absorption/Extraction Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.AbsorptionColumn, x, y, tag,,, CreateConnected))

            Case "Compound Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ComponentSeparator, x, y, tag,,, CreateConnected))

            Case "Solids Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.SolidSeparator, x, y, tag,,, CreateConnected))

            Case "Filter"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Filter, x, y, tag,,, CreateConnected))

            Case "Orifice Plate"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OrificePlate, x, y, tag,,, CreateConnected))

            Case "Python Script"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.CustomUO, x, y, tag,,, CreateConnected))

            Case "Spreadsheet"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ExcelUO, x, y, tag,,, CreateConnected))

            Case "Flowsheet"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.FlowsheetUO, x, y, tag,,, CreateConnected))

            Case "CAPE-OPEN Unit Operation"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.CapeOpenUO, x, y, tag,,, CreateConnected))

            Case "Digital Gauge"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.DigitalGauge, x, y, tag,,, CreateConnected))

            Case "Analog Gauge"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.AnalogGauge, x, y, tag,,, CreateConnected))

            Case "Level Gauge"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.LevelGauge, x, y, tag,,, CreateConnected))

            Case "PID Controller"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Controller_PID, x, y, tag,,, CreateConnected))

            Case "Python Controller"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Controller_Python, x, y, tag,,, CreateConnected))

            Case "Input Box"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Input, x, y, tag,,, CreateConnected))

            Case "Switch"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Switch, x, y, tag,,, CreateConnected))

            Case "Air Cooler 2"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.AirCooler2, x, y, tag,,, CreateConnected))

            Case "Gibbs Reactor (Reaktoro)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_GibbsReaktoro, x, y, tag,,, CreateConnected))

            Case "Wind Turbine"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.WindTurbine, x, y, tag,,, CreateConnected))

            Case "Hydroelectric Turbine"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.HydroelectricTurbine, x, y, tag,,, CreateConnected))

            Case "Solar Panel"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.SolarPanel, x, y, tag,,, CreateConnected))

            Case "Water Electrolyzer"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.WaterElectrolyzer, x, y, tag,,, CreateConnected))

            Case "PEM Fuel Cell (Amphlett)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.PEMFuelCell, x, y, tag,,, CreateConnected))

            Case Else

                Return Nothing

        End Select

    End Function


    Public Function GetAvailableFlowsheetObjectTypeNames() As Array Implements IFlowsheet.GetAvailableFlowsheetObjectTypeNames

        Dim list As New List(Of String)

        list.AddRange({
         "Controller Block",
         "Specification Block",
         "Recycle Block",
         "Energy Recycle Block",
         "Stream Mixer",
         "Stream Splitter",
         "Pump",
         "Tank",
         "Gas-Liquid Separator",
         "Material Stream",
         "Energy Stream",
         "Compressor",
         "Expander",
         "Heater",
         "Cooler",
         "Pipe Segment",
         "Valve",
         "Conversion Reactor",
         "Equilibrium Reactor",
         "Gibbs Reactor",
         "Plug-Flow Reactor (PFR)",
         "Continuous Stirred Tank Reactor (CSTR)",
         "Heat Exchanger",
         "Shortcut Column",
         "Distillation Column",
         "Absorption Column", "Absorption/Extraction Column",
         "Compound Separator",
         "Solids Separator",
         "Filter",
         "Orifice Plate",
         "Python Script",
         "Spreadsheet",
         "Flowsheet",
         "CAPE-OPEN Unit Operation",
         "Digital Gauge",
         "Analog Gauge",
         "Level Gauge",
         "PID Controller",
         "Python Controller",
         "Input Box",
         "Switch",
         "Air Cooler 2",
         "Gibbs Reactor (Reaktoro)",
         "Wind Turbine",
         "Hydroelectric Turbine",
         "Solar Panel",
         "Water Electrolyzer",
         "PEM Fuel Cell (Amphlett)"})

        list.Sort()

        Return list.ToArray()

    End Function

    Public Function AddFlowsheetObject(typename As String, objname As String) As ISimulationObject Implements IFlowsheet.AddFlowsheetObject

        Select Case typename

            Case "Controller Block"

                Return AddObject(ObjectType.OT_Adjust, 50, 50, objname)

            Case "Specification Block"

                Return AddObject(ObjectType.OT_Spec, 50, 50, objname)

            Case "Recycle Block"

                Return AddObject(ObjectType.OT_Recycle, 50, 50, objname)

            Case "Energy Recycle Block"

                Return AddObject(ObjectType.OT_EnergyRecycle, 50, 50, objname)

            Case "Stream Mixer"

                Return AddObject(ObjectType.NodeIn, 50, 50, objname)

            Case "Stream Splitter"

                Return AddObject(ObjectType.NodeOut, 50, 50, objname)

            Case "Pump"

                Return AddObject(ObjectType.Pump, 50, 50, objname)

            Case "Tank"

                Return AddObject(ObjectType.Tank, 50, 50, objname)

            Case "Gas-Liquid Separator"

                Return AddObject(ObjectType.Vessel, 50, 50, objname)

            Case "Material Stream"

                Return AddObject(ObjectType.MaterialStream, 50, 50, objname)

            Case "Energy Stream"

                Return AddObject(ObjectType.EnergyStream, 50, 50, objname)

            Case "Compressor"

                Return AddObject(ObjectType.Compressor, 50, 50, objname)

            Case "Expander"

                Return AddObject(ObjectType.Expander, 50, 50, objname)

            Case "Heater"

                Return AddObject(ObjectType.Heater, 50, 50, objname)

            Case "Cooler"

                Return AddObject(ObjectType.Cooler, 50, 50, objname)

            Case "Pipe Segment"

                Return AddObject(ObjectType.Pipe, 50, 50, objname)

            Case "Valve"

                Return AddObject(ObjectType.Valve, 50, 50, objname)

            Case "Conversion Reactor"

                Return AddObject(ObjectType.RCT_Conversion, 50, 50, objname)

            Case "Equilibrium Reactor"

                Return AddObject(ObjectType.RCT_Equilibrium, 50, 50, objname)

            Case "Gibbs Reactor"

                Return AddObject(ObjectType.RCT_Gibbs, 50, 50, objname)

            Case "Plug-Flow Reactor (PFR)"

                Return AddObject(ObjectType.RCT_PFR, 50, 50, objname)

            Case "Continuous Stirred Tank Reactor (CSTR)"

                Return AddObject(ObjectType.RCT_CSTR, 50, 50, objname)

            Case "Heat Exchanger"

                Return AddObject(ObjectType.HeatExchanger, 50, 50, objname)

            Case "Shortcut Column"

                Return AddObject(ObjectType.ShortcutColumn, 50, 50, objname)

            Case "Distillation Column"

                Return AddObject(ObjectType.DistillationColumn, 50, 50, objname)

            Case "Absorption Column", "Absorption/Extraction Column"

                Return AddObject(ObjectType.AbsorptionColumn, 50, 50, objname)

            Case "Compound Separator"

                Return AddObject(ObjectType.ComponentSeparator, 50, 50, objname)

            Case "Solids Separator"

                Return AddObject(ObjectType.SolidSeparator, 50, 50, objname)

            Case "Filter"

                Return AddObject(ObjectType.Filter, 50, 50, objname)

            Case "Orifice Plate"

                Return AddObject(ObjectType.OrificePlate, 50, 50, objname)

            Case "Python Script"

                Return AddObject(ObjectType.CustomUO, 50, 50, objname)

            Case "Spreadsheet"

                Return AddObject(ObjectType.ExcelUO, 50, 50, objname)

            Case "Flowsheet"

                Return AddObject(ObjectType.FlowsheetUO, 50, 50, objname)

            Case "CAPE-OPEN Unit Operation"

                Return AddObject(ObjectType.CapeOpenUO, 50, 50, objname)

            Case "Digital Gauge"

                Return AddObject(ObjectType.DigitalGauge, 50, 50, objname)

            Case "Analog Gauge"

                Return AddObject(ObjectType.AnalogGauge, 50, 50, objname)

            Case "Level Gauge"

                Return AddObject(ObjectType.LevelGauge, 50, 50, objname)

            Case "PID Controller"

                Return AddObject(ObjectType.Controller_PID, 50, 50, objname)

            Case "Python Controller"

                Return AddObject(ObjectType.Controller_Python, 50, 50, objname)

            Case "Input Box"

                Return AddObject(ObjectType.Input, 50, 50, objname)

            Case "Switch"

                Return AddObject(ObjectType.Switch, 50, 50, objname)

            Case "Air Cooler 2"

                Return AddObject(ObjectType.AirCooler2, 50, 50, objname)

            Case "Gibbs Reactor (Reaktoro)"

                Return AddObject(ObjectType.RCT_GibbsReaktoro, 50, 50, objname)

            Case "Wind Turbine"

                Return AddObject(ObjectType.WindTurbine, 50, 50, objname)

            Case "Hydroelectric Turbine"

                Return AddObject(ObjectType.HydroelectricTurbine, 50, 50, objname)

            Case "Solar Panel"

                Return AddObject(ObjectType.SolarPanel, 50, 50, objname)

            Case "Water Electrolyzer"

                Return AddObject(ObjectType.WaterElectrolyzer, 50, 50, objname)

            Case "PEM Fuel Cell (Amphlett)"

                Return AddObject(ObjectType.PEMFuelCell, 50, 50, objname)

            Case Else

                Return Nothing

        End Select

    End Function


    Public Function AddObjectToSurface(type As ObjectType, x As Integer, y As Integer,
                                       Optional tag As String = "",
                                       Optional id As String = "",
                                       Optional uoobj As Interfaces.IExternalUnitOperation = Nothing,
                                       Optional CreateConnected As Boolean = False) As String

        RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)

        Dim gObj As GraphicObject = Nothing
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Dim objindex = (SimulationObjects.Values.Where(Function(o) o.GraphicObject.ObjectType = type).Count + 1).ToString()

        Select Case type

            Case ObjectType.External

                Dim myNode As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                myNode.Tag = uoobj.Prefix + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(uoobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, myNode)
                DirectCast(uoobj, Interfaces.ISimulationObject).GraphicObject = myNode
                myNode.CreateConnectors(0, 0)
                SimulationObjects.Add(myNode.Name, uoobj)

            Case ObjectType.Switch

                Dim myGobj As New SwitchGraphic(mpx, mpy, 50, 40)
                myGobj.Tag = "SW-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "SW-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As UnitOperations.UnitOperations.Switch = New UnitOperations.UnitOperations.Switch(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

            Case ObjectType.Input

                Dim myGobj As New InputGraphic(mpx, mpy, 50, 25)
                myGobj.Tag = "IN-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "IN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As Input = New Input(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

                GraphicObjectControlPanelModeEditors.SetInputDelegate(myGobj, myObj)

            Case ObjectType.Controller_PID

                Dim myGobj As New PIDControllerGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "PID-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "PID-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As PIDController = New PIDController(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

                GraphicObjectControlPanelModeEditors.SetPIDDelegate(myGobj, myObj)

            Case ObjectType.Controller_Python

                Dim myGobj As New PythonControllerGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "PC-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "PC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As PythonController = New PythonController(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

            Case ObjectType.LevelGauge

                Dim myGobj As New LevelGaugeGraphic(mpx, mpy, 40, 70)
                myGobj.Tag = "LG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "LG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As LevelGauge = New LevelGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

            Case ObjectType.DigitalGauge

                Dim myGobj As New DigitalGaugeGraphic(mpx, mpy, 40, 20)
                myGobj.Tag = "DG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "DG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As DigitalGauge = New DigitalGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

            Case ObjectType.AnalogGauge

                Dim myGobj As New AnalogGaugeGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "AG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "AG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myGobj)
                Dim myObj As AnalogGauge = New AnalogGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                SimulationObjects.Add(myGobj.Name, myObj)

            Case ObjectType.OT_Adjust

                Dim myNode As New AdjustGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.Tag = "C-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "AJ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Adjust = New Adjust(myNode.Name, "Ajuste")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_Spec

                Dim myNode As New SpecGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.Tag = "SP-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "ES-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Spec = New Spec(myNode.Name, "Especificao")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_Recycle

                Dim myNode As New RecycleGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.Tag = "R-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "REC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Recycle = New Recycle(myNode.Name, "Reciclo")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_EnergyRecycle

                Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.Tag = "ER-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "EREC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As EnergyRecycle = New EnergyRecycle(myNode.Name, "EnergyRecycle")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.NodeIn, ObjectType.Mixer

                Dim myNode As New MixerGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.Tag = "MIX-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "MIST-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myCOMIX As Mixer = New Mixer(myNode.Name, "Misturador")
                myCOMIX.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myCOMIX)

            Case ObjectType.NodeOut, ObjectType.Splitter

                Dim myNodeo As New SplitterGraphic(mpx, mpy, 20, 20)
                myNodeo.LineWidth = 2
                myNodeo.Fill = True
                myNodeo.Tag = "SPL-" + objindex
                If tag <> "" Then myNodeo.Tag = tag
                gObj = myNodeo
                CheckTag(gObj)
                gObj.Name = "DIV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNodeo)
                'OBJETO DWSIM
                Dim myCOSP As UnitOperations.UnitOperations.Splitter = New UnitOperations.UnitOperations.Splitter(myNodeo.Name, "Divisor")
                myCOSP.GraphicObject = myNodeo
                SimulationObjects.Add(myNodeo.Name, myCOSP)

            Case ObjectType.Pump

                Dim myPump As New PumpGraphic(mpx, mpy, 25, 25)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myPump.SetSize(New SKSize(40, 40))
                End If
                myPump.LineWidth = 2
                myPump.Fill = True
                myPump.Tag = "PUMP-" + objindex
                If tag <> "" Then myPump.Tag = tag
                gObj = myPump
                CheckTag(gObj)
                gObj.Name = "BB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myPump)
                'OBJETO DWSIM
                Dim myCOSP As Pump = New Pump(myPump.Name, "Bomba")
                myCOSP.GraphicObject = myPump
                SimulationObjects.Add(myPump.Name, myCOSP)

            Case ObjectType.Tank

                Dim myTank As New TankGraphic(mpx, mpy, 50, 50)
                myTank.LineWidth = 2
                myTank.Fill = True
                myTank.Tag = "TANK-" + objindex
                If tag <> "" Then myTank.Tag = tag
                gObj = myTank
                CheckTag(gObj)
                gObj.Name = "TQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myTank)
                'OBJETO DWSIM
                Dim myCOTK As Tank = New Tank(myTank.Name, "Tanque")
                myCOTK.GraphicObject = myTank
                SimulationObjects.Add(myTank.Name, myCOTK)

            Case ObjectType.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 50)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myVessel.SetSize(New SKSize(70, 60))
                End If
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.Tag = "V-" + objindex
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                CheckTag(gObj)
                gObj.Name = "SEP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myVessel)
                'OBJETO DWSIM
                Dim myCOVESSEL As Vessel = New Vessel(myVessel.Name, "VasoSeparadorGL")
                myCOVESSEL.GraphicObject = myVessel
                SimulationObjects.Add(myVessel.Name, myCOVESSEL)

            Case ObjectType.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 20, 20)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.Tag = objindex
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                CheckTag(gObj)
                gObj.Name = "MAT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As Thermodynamics.Streams.MaterialStream = New Thermodynamics.Streams.MaterialStream(myMStr.Name, "CorrentedeMatria", Me, Nothing)
                myCOMS.GraphicObject = myMStr
                AddCompoundsToMaterialStream(myCOMS)
                SimulationObjects.Add(myCOMS.Name, myCOMS)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 20, 20)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.Tag = "E" + objindex
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                CheckTag(gObj)
                gObj.Name = "EN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOES As EnergyStream = New EnergyStream(myMStr.Name, "CorrentedeEnergia")
                myCOES.GraphicObject = myMStr
                SimulationObjects.Add(myCOES.Name, myCOES)

            Case ObjectType.Compressor

                Dim myComp As New CompressorGraphic(mpx, mpy, 25, 25)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myComp.SetSize(New SKSize(50, 50))
                End If
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.Tag = "C-" + objindex
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                CheckTag(gObj)
                gObj.Name = "COMP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Compressor = New Compressor(myComp.Name, "CompressorAdiabtico")
                myCOCP.GraphicObject = myComp
                SimulationObjects.Add(myComp.Name, myCOCP)

            Case ObjectType.Expander

                Dim myComp As New TurbineGraphic(mpx, mpy, 25, 25)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myComp.SetSize(New SKSize(50, 50))
                End If
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.Tag = "X-" + objindex
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "TURB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Expander = New Expander(myComp.Name, "TurbinaAdiabtica")
                myCOCP.GraphicObject = myComp
                SimulationObjects.Add(myComp.Name, myCOCP)

            Case ObjectType.Cooler

                Dim myCool As New CoolerGraphic(mpx, mpy, 25, 25)
                myCool.LineWidth = 2
                myCool.Fill = True
                myCool.Tag = "CL-" + objindex
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                CheckTag(gObj)
                gObj.Name = "RESF-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCool)
                'OBJETO DWSIM
                Dim myCOCL As Cooler = New Cooler(myCool.Name, "Resfriador")
                myCOCL.GraphicObject = myCool
                SimulationObjects.Add(myCool.Name, myCOCL)

            Case ObjectType.Heater

                Dim myHeat As New HeaterGraphic(mpx, mpy, 25, 25)
                myHeat.LineWidth = 2
                myHeat.Fill = True
                myHeat.Tag = "HT-" + objindex
                If tag <> "" Then myHeat.Tag = tag
                gObj = myHeat
                CheckTag(gObj)
                gObj.Name = "AQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myHeat)
                'OBJETO DWSIM
                Dim myCOCL As Heater = New Heater(myHeat.Name, "Aquecedor")
                myCOCL.GraphicObject = myHeat
                SimulationObjects.Add(myHeat.Name, myCOCL)

            Case ObjectType.Pipe

                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 50, 10)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myPipe.SetSize(New SKSize(50, 20))
                End If
                myPipe.LineWidth = 2
                myPipe.Fill = True
                myPipe.Tag = "PIPE-" + objindex
                If tag <> "" Then myPipe.Tag = tag
                gObj = myPipe
                CheckTag(gObj)
                gObj.Name = "TUB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myPipe)
                'OBJETO DWSIM
                Dim myCOPIPE As Pipe = New Pipe(myPipe.Name, "Tubulao")
                myCOPIPE.GraphicObject = myPipe
                SimulationObjects.Add(myPipe.Name, myCOPIPE)

            Case ObjectType.Valve

                Dim myValve As New ValveGraphic(mpx, mpy, 20, 20)
                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 50, 10)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myValve.SetSize(New SKSize(30, 30))
                End If
                myValve.LineWidth = 2
                myValve.Fill = True
                myValve.Tag = "VALVE-" + objindex
                If tag <> "" Then myValve.Tag = tag
                gObj = myValve
                CheckTag(gObj)
                gObj.Name = "VALV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myValve)
                'OBJETO DWSIM
                Dim myCOVALVE As Valve = New Valve(myValve.Name, "Vlvula")
                myCOVALVE.GraphicObject = myValve
                SimulationObjects.Add(myValve.Name, myCOVALVE)

            Case ObjectType.RCT_Conversion

                Dim myRconv As New ConversionReactorGraphic(mpx, mpy, 50, 50)
                myRconv.LineWidth = 2
                myRconv.Fill = True
                myRconv.Tag = "RCONV-" + objindex
                If tag <> "" Then myRconv.Tag = tag
                gObj = myRconv
                gObj.Name = "RC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRconv)
                'OBJETO DWSIM
                Dim myCORCONV As Reactor_Conversion = New Reactor_Conversion(myRconv.Name, "ReatorConversao")
                myCORCONV.GraphicObject = myRconv
                SimulationObjects.Add(myRconv.Name, myCORCONV)

            Case ObjectType.RCT_Equilibrium

                Dim myReq As New EquilibriumReactorGraphic(mpx, mpy, 50, 50)
                myReq.LineWidth = 2
                myReq.Fill = True
                myReq.Tag = "REQ-" + objindex
                If tag <> "" Then myReq.Tag = tag
                gObj = myReq
                CheckTag(gObj)
                gObj.Name = "RE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myReq)
                'OBJETO DWSIM
                Dim myCOREQ As Reactor_Equilibrium = New Reactor_Equilibrium(myReq.Name, "ReatorEquilibrio")
                myCOREQ.GraphicObject = myReq
                SimulationObjects.Add(myReq.Name, myCOREQ)

            Case ObjectType.RCT_Gibbs

                Dim myRgibbs As New GibbsReactorGraphic(mpx, mpy, 50, 50)
                myRgibbs.LineWidth = 2
                myRgibbs.Fill = True
                myRgibbs.Tag = "RGIBBS-" + objindex
                If tag <> "" Then myRgibbs.Tag = tag
                gObj = myRgibbs
                gObj.Name = "RG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRgibbs)
                'OBJETO DWSIM
                Dim myCORGIBBS As Reactor_Gibbs = New Reactor_Gibbs(myRgibbs.Name, "ReatorGibbs")
                myCORGIBBS.GraphicObject = myRgibbs
                SimulationObjects.Add(myRgibbs.Name, myCORGIBBS)

            Case ObjectType.RCT_CSTR

                Dim myRcstr As New CSTRGraphic(mpx, mpy, 50, 50)
                myRcstr.LineWidth = 2
                myRcstr.Fill = True
                myRcstr.Tag = "CSTR-" + objindex
                If tag <> "" Then myRcstr.Tag = tag
                gObj = myRcstr
                CheckTag(gObj)
                gObj.Name = "CSTR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRcstr)
                'OBJETO DWSIM
                Dim myCORCSTR As Reactor_CSTR = New Reactor_CSTR(myRcstr.Name, "ReatorCSTR")
                myCORCSTR.GraphicObject = myRcstr
                SimulationObjects.Add(myRcstr.Name, myCORCSTR)

            Case ObjectType.RCT_PFR

                Dim myRpfr As New PFRGraphic(mpx, mpy, 70, 20)
                myRpfr.LineWidth = 2
                myRpfr.Fill = True
                myRpfr.Tag = "PFR-" + objindex
                If tag <> "" Then myRpfr.Tag = tag
                gObj = myRpfr
                CheckTag(gObj)
                gObj.Name = "PFR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRpfr)
                'OBJETO DWSIM
                Dim myCOPFR As Reactor_PFR = New Reactor_PFR(myRpfr.Name, "ReatorPFR")
                myCOPFR.GraphicObject = myRpfr
                SimulationObjects.Add(myRpfr.Name, myCOPFR)

            Case ObjectType.HeatExchanger

                Dim myHeatExchanger As New HeatExchangerGraphic(mpx, mpy, 30, 30)
                If FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myHeatExchanger.SetSize(New SKSize(60, 60))
                End If
                myHeatExchanger.LineWidth = 2
                myHeatExchanger.Fill = True
                myHeatExchanger.Tag = "HX-" + objindex
                If tag <> "" Then myHeatExchanger.Tag = tag
                gObj = myHeatExchanger
                CheckTag(gObj)
                gObj.Name = "HE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myHeatExchanger)
                'OBJETO DWSIM
                Dim myCOHE As HeatExchanger = New HeatExchanger(myHeatExchanger.Name, "HeatExchanger")
                myCOHE.GraphicObject = myHeatExchanger
                SimulationObjects.Add(myHeatExchanger.Name, myCOHE)

            Case ObjectType.ShortcutColumn

                Dim mySC As New ShortcutColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.Tag = "SCOL-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "SC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As ShortcutColumn = New ShortcutColumn(mySC.Name, "ShortcutColumn")
                myCOSC.GraphicObject = mySC
                SimulationObjects.Add(mySC.Name, myCOSC)

            Case ObjectType.DistillationColumn

                Dim mySC As New RigorousColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.Tag = "DCOL-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                CheckTag(gObj)
                gObj.Name = "DC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As DistillationColumn = New DistillationColumn(mySC.Name, "DistillationColumn", Me)
                myCOSC.GraphicObject = mySC
                SimulationObjects.Add(mySC.Name, myCOSC)

            Case ObjectType.AbsorptionColumn

                Dim mySC As New AbsorptionColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.Tag = "ABS-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "ABS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As AbsorptionColumn = New AbsorptionColumn(mySC.Name, "AbsorptionColumn", Me)
                myCOSC.GraphicObject = mySC
                SimulationObjects.Add(mySC.Name, myCOSC)

            Case ObjectType.ComponentSeparator

                Dim myCSep As New ComponentSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.Tag = "CS-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                CheckTag(gObj)
                gObj.Name = "CS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As ComponentSeparator = New ComponentSeparator(myCSep.Name, "ComponentSeparator")
                myCOCSEP.GraphicObject = myCSep
                SimulationObjects.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.SolidSeparator

                Dim myCSep As New SolidsSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.Tag = "SS-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "SS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As SolidsSeparator = New SolidsSeparator(myCSep.Name, "SolidsSeparator")
                myCOCSEP.GraphicObject = myCSep
                SimulationObjects.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.Filter

                Dim myCSep As New FilterGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.Tag = "FLT-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                CheckTag(gObj)
                gObj.Name = "FT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As Filter = New Filter(myCSep.Name, "Filter")
                myCOCSEP.GraphicObject = myCSep
                SimulationObjects.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.OrificePlate

                Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25)
                myOPL.LineWidth = 2
                myOPL.Fill = True
                myOPL.Tag = "OP-" + objindex
                If tag <> "" Then myOPL.Tag = tag
                gObj = myOPL
                CheckTag(gObj)
                gObj.Name = "OP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myOPL)
                'OBJETO DWSIM
                Dim myCOOPL As OrificePlate = New OrificePlate(myOPL.Name, "OrificePlate")
                myCOOPL.GraphicObject = myOPL
                SimulationObjects.Add(myOPL.Name, myCOOPL)

            Case ObjectType.CustomUO

                Dim myCUO As New ScriptGraphic(mpx, mpy, 25, 25)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.Tag = "CUSTOM-" + objindex
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                CheckTag(gObj)
                gObj.Name = "UO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As CustomUO = New CustomUO(myCUO.Name, "CustomUnitOp")
                myCOCUO.GraphicObject = myCUO
                SimulationObjects.Add(myCUO.Name, myCOCUO)

            Case ObjectType.ExcelUO

                Dim myEUO As New SpreadsheetGraphic(mpx, mpy, 25, 25)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.Tag = "SHEET-" + objindex
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                CheckTag(gObj)
                gObj.Name = "EXL-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As ExcelUO = New ExcelUO(myEUO.Name, "ExcelUnitOp")
                myCOEUO.GraphicObject = myEUO
                SimulationObjects.Add(myEUO.Name, myCOEUO)

            Case ObjectType.FlowsheetUO

                Dim myEUO As New FlowsheetGraphic(mpx, mpy, 25, 25)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.Tag = "FS-" + objindex
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                CheckTag(gObj)
                gObj.Name = "FS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As UnitOperations.UnitOperations.Flowsheet = New UnitOperations.UnitOperations.Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                myCOEUO.GraphicObject = myEUO
                SimulationObjects.Add(myEUO.Name, myCOEUO)

            Case ObjectType.CapeOpenUO

                Dim myCUO As New CAPEOPENGraphic(mpx, mpy, 40, 40)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.Tag = "CO-" + objindex
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                CheckTag(gObj)
                gObj.Name = "COUO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCUO)
                Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj, False)
                myCOCUO.GraphicObject = myCUO
                SimulationObjects.Add(myCUO.Name, myCOCUO)

            Case ObjectType.AirCooler2

                Dim fsobj = New AirCooler2()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "AC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.EnergyMixer

                Dim fsobj = New EnergyMixer()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "EMIX-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.RCT_GibbsReaktoro

                Dim fsobj = New Reactor_ReaktoroGibbs()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "RGIBBSR-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.WindTurbine

                Dim fsobj = New WindTurbine()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "WT-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.HydroelectricTurbine

                Dim fsobj = New HydroelectricTurbine()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "HYT-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.PEMFuelCell

                Dim fsobj = New PEMFC_Amphlett()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "PEMFC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.SolarPanel

                Dim fsobj = New SolarPanel()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "SP-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

            Case ObjectType.WaterElectrolyzer

                Dim fsobj = New WaterElectrolyzer()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "WELEC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                SimulationObjects.Add(grobj.Name, fsobj)

        End Select

        If Not gObj Is Nothing Then
            gObj.Flowsheet = Me
            gObj.PositionConnectors()
            gObj.Owner = SimulationObjects(gObj.Name)
            SimulationObjects(gObj.Name).SetFlowsheet(Me)
            FlowsheetSurface.AddObject(gObj)
        End If

        If CreateConnected Then
            Try
                If FlowsheetOptions.AddObjectsWithStreams = 1 Then
                    AddConnectedObjects(SimulationObjects(gObj.Name), 1)
                    UpdateInterface()
                End If
                If FlowsheetOptions.AddObjectsWithStreams = 2 Then
                    AddConnectedObjects(SimulationObjects(gObj.Name), 2)
                    AddConnectedObjects(SimulationObjects(gObj.Name), 1)
                    UpdateInterface()
                End If
            Catch ex As Exception
            End Try
        End If

        Return gObj.Name

    End Function

    Public Sub AddConnectedObjects(obj As Interfaces.ISimulationObject, scheme As Integer)

        Dim x = obj.GraphicObject.X
        Dim y = obj.GraphicObject.Y
        Dim w = obj.GraphicObject.Width
        Dim h = obj.GraphicObject.Height

        Dim gobj = obj.GraphicObject

        Dim mstrs = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 500, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream).ToList()
        Dim mstrsI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream And Not o.OutputConnectors(0).IsAttached).ToList()
        Dim mstrsO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream And Not o.InputConnectors(0).IsAttached).ToList()
        Dim estrs = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 500, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream).ToList()
        Dim estrsI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream And Not o.OutputConnectors(0).IsAttached).ToList()
        Dim estrsO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream And Not o.InputConnectors(0).IsAttached).ToList()

        Dim uosI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType <> ObjectType.MaterialStream And
                                                                                             o.ObjectType <> ObjectType.EnergyStream).ToList()
        Dim uosO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType <> ObjectType.MaterialStream And
                                                                                             o.ObjectType <> ObjectType.EnergyStream).ToList()

        mstrs = mstrs.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        mstrsI = mstrsI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        mstrsO = mstrsO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrs = estrs.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrsI = estrsI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrsO = estrsO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        uosI = uosI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        uosO = uosO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()

        Select Case obj.GraphicObject.ObjectType
            Case ObjectType.MaterialStream
                If scheme = 2 And uosI.Count > 0 Then
                    Try : ConnectObject(uosI(0), gobj) : Catch : End Try
                End If
                If scheme = 2 And uosO.Count > 0 Then
                    Try : ConnectObject(gobj, uosO(0)) : Catch : End Try
                End If
            Case ObjectType.EnergyStream
                If scheme = 2 And uosI.Count > 0 Then
                    Try : ConnectObject(uosI(0), gobj) : Catch : End Try
                End If
                If scheme = 2 And uosO.Count > 0 Then
                    Try : ConnectObject(gobj, uosO(0)) : Catch : End Try
                End If
            Case ObjectType.AbsorptionColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    For i = 0 To mstrs.Count
                        If i > 3 Then Exit For
                        If i < 2 Then
                            Try : ConnectObjects(mstrs(i), gobj, 0, i) : Catch : End Try
                        Else
                            Try : ConnectObjects(gobj, mstrs(i - 2), i - 2, 0) : Catch : End Try
                        End If
                    Next
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + h - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Dim m3 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h - 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 0, 0) : Catch : DeleteObject(m2, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m3).GraphicObject, 1, 0) : Catch : DeleteObject(m3, False) : End Try
                End If
            Case ObjectType.Compressor
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrsI.Count > 0 Then
                    If estrsI.Count > 0 Then
                        Try : ConnectObjects(estrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.DistillationColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 10) : Catch : End Try
                    If estrsO.Count > 0 Then Try : ConnectObjects(gobj, estrsO(0), 10, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + 40, ,,, False)
                    Dim e2 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h - 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e2).GraphicObject, gobj, 0, 10) : Catch : DeleteObject(e1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 10, 0) : Catch : DeleteObject(e2, False) : End Try
                End If
            Case ObjectType.Expander, ObjectType.Cooler
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then ConnectObjects(gobj, estrsO(0), 0, 0)
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 0, 0) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.Heater
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.HeatExchanger
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                        Try : ConnectObjects(mstrsI(1), gobj, 0, 1) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y - 40, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m3 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 0, 0) : Catch : DeleteObject(m2, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m3).GraphicObject, 1, 0) : Catch : DeleteObject(m3, False) : End Try
                End If
            Case ObjectType.NodeIn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                        Try : ConnectObjects(mstrsI(1), gobj, 0, 1) : Catch : End Try
                        Try : ConnectObjects(mstrsI(2), gobj, 0, 2) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 0, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
            Case ObjectType.NodeOut
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(2), 2, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
            Case ObjectType.Pipe
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Try : ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 0, 0) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.Pump
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_Conversion
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then ConnectObjects(gobj, estrsO(0), 2, 0)
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 2, 0) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_CSTR, ObjectType.RCT_PFR
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_Equilibrium, ObjectType.RCT_Gibbs
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.ComponentSeparator
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Try : ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + 60, y + h + 60, ,,, False)
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 0, 0) : Catch : DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.ShortcutColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                    If estrsO.Count > 0 Then Try : ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + 40, ,,, False)
                    Dim e2 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h - 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(e2).GraphicObject, gobj, 0, 1) : Catch : DeleteObject(e1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(e1).GraphicObject, 0, 0) : Catch : DeleteObject(e2, False) : End Try
                End If
            Case ObjectType.Tank, ObjectType.Valve, ObjectType.OrificePlate, ObjectType.OT_Recycle
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                End If
            Case ObjectType.Vessel, ObjectType.SolidSeparator, ObjectType.Filter
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, ,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, ,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, ,,, False)
                    Try : ConnectObjects(SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : DeleteObject(m0, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m1).GraphicObject, 0, 0) : Catch : DeleteObject(m1, False) : End Try
                    Try : ConnectObjects(gobj, SimulationObjects(m2).GraphicObject, 1, 0) : Catch : DeleteObject(m2, False) : End Try
                End If
        End Select

    End Sub

    Public Sub ConnectObject(ByRef gObjFrom As GraphicObject, ByRef gObjTo As GraphicObject, Optional ByVal fidx As Integer = -1, Optional ByVal tidx As Integer = -1)

        ConnectObjects(gObjFrom, gObjTo, fidx, tidx)

    End Sub

    Public Sub DeleteObject(ByVal tag As String, Optional ByVal confirmation As Boolean = True)

        Dim gobj As GraphicObject = GetFlowsheetGraphicObject(tag)

        If Not gobj Is Nothing Then
            FlowsheetSurface.SelectedObject = gobj
            DeleteSelectedObject(Me, New EventArgs(), gobj, confirmation)
        ElseIf GraphicObjects.ContainsKey(tag) Then
            gobj = GraphicObjects(tag)
            FlowsheetSurface.SelectedObject = gobj
            DeleteSelectedObject(Me, New EventArgs(), gobj, confirmation)
        End If

    End Sub

    Public Sub CheckTag(obj As IGraphicObject)

        While GetFlowsheetSimulationObject(obj.Tag) IsNot Nothing
            obj.Tag = Regex.Replace(obj.Tag, "\d+", Function(m) (Integer.Parse(m.Value) + 1).ToString(New String("0", m.Value.Length)))
        End While

    End Sub

    Public Sub LoadFromMXML(xdoc As XDocument)

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               SharedClasses.Utility.UpdateElementForMobileXMLLoading_CrossPlatformUI(xel1)
                                           End Sub)

        LoadFromXML(xdoc)

    End Sub

    Public Sub LoadFromXML(xdoc As XDocument) Implements IFlowsheet.LoadFromXML

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        'check version

        Dim sver = New Version("1.0.0.0")

        Try
            sver = New Version(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("BuildVersion").Value)
        Catch ex As Exception
        End Try

        If sver < New Version("5.0.0.0") Then
            Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                                   SharedClasses.Utility.UpdateElement(xel1)
                                               End Sub)
        End If

        'check saved from Classic UI

        Dim savedfromclui As Boolean = True

        Try
            savedfromclui = Boolean.Parse(xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo").Element("SavedFromClassicUI").Value)
        Catch ex As Exception
        End Try

        If savedfromclui Then
            Try
                Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                                       SharedClasses.Utility.UpdateElementForNewUI(xel1)
                                                   End Sub)
            Catch ex As Exception
            End Try
        End If

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try

            Options.LoadData(data)

            FlowsheetSurface.DrawFloatingTable = Options.DisplayFloatingPropertyTables
            FlowsheetSurface.DrawPropertyList = Options.DisplayCornerPropertyList

            If Not AvailableSystemsOfUnits.Contains(Options.SelectedUnitSystem1) Then
                AvailableSystemsOfUnits.Add(Options.SelectedUnitSystem1)
            End If

            If sver < New Version("6.3.0.0") Then
                Options.SkipEquilibriumCalculationOnDefinedStreams = False
            End If

        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties").Elements.ToList

            Try

                ExtraProperties = New ExpandoObject

                If Not data Is Nothing Then
                    For Each xel As XElement In data
                        Try
                            Dim propname = xel.Element("Name").Value
                            Dim proptype = xel.Element("PropertyType").Value
                            Dim assembly1 As Assembly = Nothing
                            For Each assembly In My.Application.Info.LoadedAssemblies
                                If proptype.Contains(assembly.GetName().Name) Then
                                    assembly1 = assembly
                                    Exit For
                                End If
                            Next
                            If assembly1 IsNot Nothing Then
                                Dim ptype As Type = assembly1.GetType(proptype)
                                Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                                DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                            End If
                        Catch ex As Exception
                        End Try
                    Next
                End If

            Catch ex As Exception

                excs.Add(New Exception("Error Loading Dynamic Properties", ex))

            End Try

        End If

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddGraphicObjects(data, excs)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

        For Each xel As XElement In data
            Dim obj As New ConstantProperties
            obj.Name = xel.Element("Name").Value
            If Not AvailableCompounds.ContainsKey(obj.Name) Then AvailableCompounds.Add(obj.Name, obj)
            Options.SelectedComponents.Add(obj.Name, obj)
        Next

        Parallel.ForEach(data, Sub(xel)
                                   Try
                                       Options.SelectedComponents(xel.Element("Name").Value).LoadData(xel.Elements.ToList)
                                   Catch ex As Exception
                                       excs.Add(New Exception("Error Loading Compound Information", ex))
                                   End Try
                               End Sub)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("PortableDTL.DTL.SimulationObjects", "DWSIM.Thermodynamics")
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = Nothing
                If xel.Element("Type").Value.Contains("ThermoC") Then
                    Dim thermockey As String = "ThermoC Bridge"
                    If AvailablePropertyPackages.ContainsKey(thermockey) Then
                        obj = AvailablePropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("The ThermoC bridge library was not found. Please download and install it in order to run this simulation.")
                    End If
                Else
                    Dim ppkey As String = xel.Element("ComponentName").Value
                    If ppkey = "" Then
                        obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackage)
                    Else
                        Dim ptype = xel.Element("Type").Value
                        If ppkey.Contains("1978") And ptype.Contains("PengRobinsonPropertyPackage") Then
                            ptype = ptype.Replace("PengRobinson", "PengRobinson1978")
                        End If
                        If ppkey.Contains("Seawater") Then ppkey = "Seawater IAPWS-08"
                        If AvailablePropertyPackages.ContainsKey(ppkey) Then
                            obj = AvailablePropertyPackages(ppkey).ReturnInstance(ptype)
                        Else
                            Throw New Exception("The " & ppkey & " Property Package library was not found. Please download and install it in order to run this simulation.")
                        End If
                    End If
                End If
                obj.LoadData(xel.Elements.ToList)
                Dim newID As String = Guid.NewGuid.ToString
                If Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                obj.Flowsheet = Me
                Options.PropertyPackages.Add(obj.UniqueID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Property Package Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        Dim objlist As New Concurrent.ConcurrentBag(Of ISimulationObject)

        For Each xel In data
            Try
                Dim id As String = xel.<Name>.Value
                Dim obj As ISimulationObject = Nothing
                If xel.Element("Type").Value.Contains("Streams.MaterialStream") Then
                    obj = New MaterialStream()
                Else
                    Dim uokey As String = xel.Element("ComponentDescription").Value
                    If ExternalUnitOperations.ContainsKey(uokey) Then
                        obj = ExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                    End If
                End If
                Dim gobj As IGraphicObject = (From go As IGraphicObject In
                                    FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                obj.GraphicObject = gobj
                gobj.Owner = obj
                obj.SetFlowsheet(Me)
                If Not gobj Is Nothing Then
                    DirectCast(obj, ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                    If TypeOf obj Is Streams.MaterialStream Then
                        For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                            For Each c As ConstantProperties In Options.SelectedComponents.Values
                                phase.Compounds(c.Name).ConstantProperties = c
                            Next
                        Next
                    ElseIf TypeOf obj Is CapeOpenUO Then
                        If DirectCast(obj, CapeOpenUO)._seluo.Name.ToLower.Contains("chemsep") Then
                            DirectCast(gobj, CAPEOPENGraphic).ChemSep = True
                            If gobj.Height = 40 And gobj.Width = 40 Then
                                gobj.Width = 144
                                gobj.Height = 180
                            End If
                        End If
                    ElseIf TypeOf obj Is Input Then
                        GraphicObjectControlPanelModeEditors.SetInputDelegate(gobj, obj)
                    ElseIf TypeOf obj Is PIDController Then
                        GraphicObjectControlPanelModeEditors.SetPIDDelegate(gobj, obj)
                    End If
                End If
                objlist.Add(obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        AddSimulationObjects(objlist, excs)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        AddTables(data, excs)

        data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

        Options.ReactionSets.Clear()

        For Each xel As XElement In data
            Try
                Dim obj As New ReactionSet()
                obj.LoadData(xel.Elements.ToList)
                Options.ReactionSets.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Set Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

        For Each xel As XElement In data
            Try
                Dim obj As New Reaction()
                obj.LoadData(xel.Elements.ToList)
                Options.Reactions.Add(obj.ID, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Reaction Information", ex))
            End Try
        Next

        If xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions").Elements.ToList

            StoredSolutions.Clear()

            For Each xel As XElement In data
                Try
                    StoredSolutions.Add(xel.@ID, xel.Elements.ToList())
                Catch ex As Exception
                End Try
            Next

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager").Elements.ToList

            Try
                DirectCast(DynamicsManager, ICustomXMLSerialization).LoadData(data)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Dynamics Manager Information", ex))
            End Try

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Optimization.OptimizationCase
                    obj.LoadData(xel.Elements.ToList)
                    OptimizationCollection.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Optimization Case Information", ex))
                End Try
            Next

        End If

        If xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As New Optimization.SensitivityAnalysisCase
                    obj.LoadData(xel.Elements.ToList)
                    SensAnalysisCollection.Add(obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Sensitivity Analysis Case Information", ex))
                End Try
            Next

        End If

        Scripts = New Dictionary(Of String, Interfaces.IScript)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New Script()
                    obj.LoadData(xel.Elements.ToList)
                    Scripts.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Script Item Information", ex))
                End Try
                i += 1
            Next

        End If

        Charts = New Dictionary(Of String, Interfaces.IChart)

        If xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems") IsNot Nothing Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems").Elements.ToList

            Dim i As Integer = 0
            For Each xel As XElement In data
                Try
                    Dim obj As New SharedClasses.Charts.Chart()
                    obj.LoadData(xel.Elements.ToList)
                    Charts.Add(obj.ID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Chart Item Information", ex))
                End Try
                i += 1
            Next

        End If

        If Not Settings.AutomationMode Then
            If LoadSpreadsheetData IsNot Nothing Then LoadSpreadsheetData.Invoke(xdoc)
        End If

        ProcessScripts(Enums.Scripts.EventType.SimulationOpened, Enums.Scripts.ObjectType.Simulation, "")

        If excs.Count > 0 Then
            If Settings.AutomationMode Then
                'throw errors
                Throw New AggregateException(excs)
            Else
                ShowMessage("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", IFlowsheet.MessageType.GeneralError)
                For Each ex As Exception In excs
                    ShowMessage(ex.Message.ToString & ": " & ex.InnerException.ToString, IFlowsheet.MessageType.GeneralError)
                Next
            End If
        Else
            ShowMessage("Data loaded successfully.", IFlowsheet.MessageType.Information)
        End If

    End Sub

    Public Function SaveToMXML() As XDocument

        Dim xdoc = SaveToXML()

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               Try
                                                   SharedClasses.Utility.UpdateElementForMobileXMLSaving_CrossPlatformUI(xel1)
                                               Catch ex As Exception
                                               End Try
                                           End Sub)

        Return xdoc

    End Function

    Public Function SaveToXML() As XDocument Implements IFlowsheet.SaveToXML

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))
        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GeneralInfo"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo")

        If Not DWSIM.GlobalSettings.Settings.AutomationMode Then
            xel.Add(New XElement("BuildVersion", My.Application.Info.Version.ToString))
            xel.Add(New XElement("BuildDate", CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)))
            If GlobalSettings.Settings.RunningPlatform() = GlobalSettings.Settings.Platform.Mac Then
                xel.Add(New XElement("OSInfo", "macOS " + Environment.OSVersion.ToString()))
            Else
                xel.Add(New XElement("OSInfo", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
            End If
        End If
        xel.Add(New XElement("SavedOn", Date.Now))
        xel.Add(New XElement("SavedFromClassicUI", False))

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

        For Each so As ICustomXMLSerialization In SimulationObjects.Values
            DirectCast(so, ISimulationObject).SetFlowsheet(Me)
            xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Settings"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Settings")
        xel.Add(Options.SaveData().ToArray())

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicProperties"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties")

        Dim extraprops = DirectCast(ExtraProperties, IDictionary(Of String, Object))
        For Each item In extraprops
            Try
                xel.Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                       New XElement("PropertyType", item.Value.GetType.ToString),
                                                                       New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
            Catch ex As Exception
            End Try
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

        For Each go As GraphicObject In FlowsheetSurface.DrawingObjects
            If Not go.IsConnector Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

        For Each pp As KeyValuePair(Of String, IPropertyPackage) In Options.PropertyPackages
            Dim createdms As Boolean = False
            If pp.Value.CurrentMaterialStream Is Nothing Then
                Dim ms As New Streams.MaterialStream("", "")
                AddCompoundsToMaterialStream(ms)
                pp.Value.CurrentMaterialStream = ms
                createdms = True
            End If
            xel.Add(New XElement("PropertyPackage", {New XElement("ID", pp.Key), DirectCast(pp.Value, ICustomXMLSerialization).SaveData().ToArray()}))
            If createdms Then pp.Value.CurrentMaterialStream = Nothing
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Compounds"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds")

        For Each cp As ConstantProperties In Options.SelectedComponents.Values
            xel.Add(New XElement("Compound", cp.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ReactionSets"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets")

        For Each pp As KeyValuePair(Of String, IReactionSet) In Options.ReactionSets
            xel.Add(New XElement("ReactionSet", DirectCast(pp.Value, ICustomXMLSerialization).SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Reactions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions")

        For Each pp As KeyValuePair(Of String, IReaction) In Options.Reactions
            xel.Add(New XElement("Reaction", {DirectCast(pp.Value, ICustomXMLSerialization).SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("StoredSolutions"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("StoredSolutions")

        For Each pp As KeyValuePair(Of String, List(Of XElement)) In StoredSolutions
            xel.Add(New XElement("Solution", New XAttribute("ID", pp.Key), pp.Value))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicsManager"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager")

        xel.Add(DirectCast(DynamicsManager, ICustomXMLSerialization).SaveData().ToArray())

        Dim flsconfig As New System.Text.StringBuilder()

        If Not GlobalSettings.Settings.AutomationMode Then

            With flsconfig
                .Append(FlowsheetSurface.Zoom.ToString(ci) & ";")
                .Append("0;")
                .Append("0")
            End With

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("FlowsheetView"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("FlowsheetView")

            xel.Add(flsconfig.ToString)

        End If

        xel = xdoc.Element("DWSIM_Simulation_Data")

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("OptimizationCases"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("OptimizationCases")

        For Each pp As Optimization.OptimizationCase In OptimizationCollection
            xel.Add(New XElement("OptimizationCase", {pp.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SensitivityAnalysis"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SensitivityAnalysis")

        For Each pp As Optimization.SensitivityAnalysisCase In SensAnalysisCollection
            xel.Add(New XElement("SensitivityAnalysisCase", {pp.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ScriptItems"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems")

        For Each scr As Script In Scripts.Values
            xel.Add(New XElement("ScriptItem", scr.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ChartItems"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems")

        For Each ch As SharedClasses.Charts.Chart In Charts.Values
            xel.Add(New XElement("ChartItem", ch.SaveData().ToArray()))
        Next

        If Not GlobalSettings.Settings.AutomationMode Then
            If SaveSpreadsheetData IsNot Nothing Then SaveSpreadsheetData.Invoke(xdoc)
        End If

        Return xdoc

    End Function

    Public Sub RequestSave() Implements IFlowsheet.RequestSave

        Throw New NotImplementedException()

    End Sub

    Public Sub RequestSaveWithDirectory(directory As String) Implements IFlowsheet.RequestSaveWithDirectory

        Throw New NotImplementedException()

    End Sub

    Public Sub RequestSaveWithPath(filepath As String) Implements IFlowsheet.RequestSaveWithPath

        Throw New NotImplementedException()

    End Sub

    Public Function GetProcessData() As List(Of XElement) Implements IFlowsheet.GetProcessData

        Dim dlist As New List(Of XElement)

        For Each so As SharedClasses.UnitOperations.BaseClass In SimulationObjects.Values
            so.SetFlowsheet(Me)
            dlist.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
        Next

        Return dlist

    End Function

    Public Sub LoadProcessData(data As List(Of XElement)) Implements IFlowsheet.LoadProcessData

        For Each xel In data
            Dim id As String = xel.<Name>.Value
            Dim obj = SimulationObjects(id)
            obj.LoadData(xel.Elements.ToList)
            If TypeOf obj Is Streams.MaterialStream Then
                Dim stream = DirectCast(obj, Streams.MaterialStream)
                For Each p In stream.Phases.Values
                    For Each c In p.Compounds.Values
                        c.ConstantProperties = SelectedCompounds(c.Name)
                    Next
                Next
            End If
        Next

    End Sub

    Sub AddGraphicObjects(data As List(Of XElement), excs As Concurrent.ConcurrentBag(Of Exception),
                      Optional ByVal pkey As String = "", Optional ByVal shift As Integer = 0, Optional ByVal reconnectinlets As Boolean = False)

        Dim objcount As Integer, searchtext As String

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("Microsoft.MSDN.Samples.GraphicObjects", "DWSIM.DrawingTools.GraphicObjects")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Ajuste", "OT_Adjust")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Especificacao", "OT_Spec")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Reciclo", "OT_Recycle")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Texto", "GO_Text")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Figura", "GO_Image")
                Dim obj As GraphicObject = Nothing
                Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                If Not t Is Nothing Then obj = CType(Activator.CreateInstance(t), GraphicObject)
                If obj Is Nothing Then
                    If xel.Element("Type").Value.Contains("OxyPlotGraphic") Then
                        obj = CType(Extended.Shared.ReturnInstance(xel.Element("Type").Value.Replace("Shapes", "Charts")), GraphicObject)
                    Else
                        obj = CType(GraphicObject.ReturnInstance(xel.Element("Type").Value), GraphicObject)
                    End If
                End If
                If Not obj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    obj.Name = pkey & obj.Name
                    obj.X += shift
                    obj.Y += shift
                    If pkey <> "" Then
                        searchtext = obj.Tag.Split("("c)(0).Trim()
                        objcount = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Select go Where go.Tag.Equals(obj.Tag)).Count
                        If objcount > 0 Then obj.Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    End If
                    If TypeOf obj Is TableGraphic Then
                        DirectCast(obj, TableGraphic).Flowsheet = Me
                    ElseIf TypeOf obj Is MasterTableGraphic Then
                        DirectCast(obj, MasterTableGraphic).Flowsheet = Me
                    ElseIf TypeOf obj Is SpreadsheetTableGraphic Then
                        DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = Me
                    ElseIf TypeOf obj Is Charts.OxyPlotGraphic Then
                        DirectCast(obj, Charts.OxyPlotGraphic).Flowsheet = Me
                    ElseIf TypeOf obj Is RigorousColumnGraphic Or TypeOf obj Is AbsorptionColumnGraphic Or TypeOf obj Is CAPEOPENGraphic Then
                        obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                        obj.PositionConnectors()
                    ElseIf TypeOf obj Is ExternalUnitOperationGraphic Then
                        Dim euo = ExternalUnitOperations.Values.Where(Function(x) x.Description = obj.Description).FirstOrDefault
                        If euo IsNot Nothing Then
                            obj.Owner = euo
                            DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = obj
                            obj.CreateConnectors(0, 0)
                            obj.Owner = Nothing
                            DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = Nothing
                        Else
                            Try
                                obj.CreateConnectors(0, 0)
                            Catch ex As Exception
                            End Try
                        End If
                    Else
                        If obj.Name = "" Then obj.Name = obj.Tag
                        obj.CreateConnectors(0, 0)
                    End If
                    obj.Flowsheet = Me
                    If Not TypeOf obj Is TableGraphic Then
                        FlowsheetSurface.DrawingObjects.Add(obj)
                        GraphicObjects.Add(obj.Name, obj)
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
            End Try
        Next


        For Each xel As XElement In data
            Try
                Dim id As String = pkey & xel.Element("Name").Value
                If id <> "" Then
                    Dim obj As IGraphicObject = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If obj Is Nothing Then obj = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = xel.Element("Name").Value).SingleOrDefault
                    If Not obj Is Nothing Then
                        If xel.Element("InputConnectors") IsNot Nothing Then
                            Dim i As Integer = 0
                            For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                                If CBool(xel2.@IsAttached) = True Then
                                    obj.InputConnectors(i).ConnectorName = pkey & xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                    obj.InputConnectors(i).Type = CType([Enum].Parse(obj.InputConnectors(i).Type.GetType, xel2.@ConnType), ConType)
                                    If reconnectinlets Then
                                        Dim objFrom As IGraphicObject = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedFromObjID).SingleOrDefault
                                        If Not objFrom Is Nothing Then
                                            If Not objFrom.OutputConnectors(CInt(xel2.@AttachedFromConnIndex)).IsAttached Then
                                                FlowsheetSurface.ConnectObject(CType(objFrom, GraphicObject), CType(obj, GraphicObject), CInt(xel2.@AttachedFromConnIndex), CInt(xel2.@AttachedToConnIndex))
                                            End If
                                        End If
                                    End If
                                End If
                                i += 1
                            Next
                        End If
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
            End Try
        Next

        For Each xel As XElement In data
            Try
                Dim id As String = pkey & xel.Element("Name").Value
                If id <> "" Then
                    Dim obj As IGraphicObject = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If Not obj Is Nothing Then
                        If xel.Element("OutputConnectors") IsNot Nothing Then
                            For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                                If CBool(xel2.@IsAttached) = True Then
                                    Dim objToID = pkey & xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As IGraphicObject = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                        If objTo Is Nothing Then objTo = (From go As IGraphicObject In FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        Dim fromidx As Integer = -1
                                        Dim cp As IConnectionPoint = (From cp2 As IConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|"c)(0) = obj.Name).SingleOrDefault
                                        If cp Is Nothing Then cp = (From cp2 As IConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|"c)(0) = xel2.@AttachedToObjID).SingleOrDefault
                                        If Not cp Is Nothing Then
                                            fromidx = CInt(cp.ConnectorName.Split("|"c)(1))
                                        End If
                                        If Not obj Is Nothing And Not objTo Is Nothing Then FlowsheetSurface.ConnectObject(CType(obj, GraphicObject), CType(objTo, GraphicObject), fromidx, CInt(xel2.@AttachedToConnIndex))
                                    End If
                                End If
                            Next
                        End If
                        If xel.Element("EnergyConnector") IsNot Nothing Then
                            For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                                If CBool(xel2.@IsAttached) = True Then
                                    Dim objToID = pkey & xel2.@AttachedToObjID
                                    If objToID <> "" Then
                                        Dim objTo As IGraphicObject = (From go As IGraphicObject In
                                                                                   FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                        If objTo Is Nothing Then obj = (From go As IGraphicObject In
                                                                                    FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                        If Not obj Is Nothing And Not objTo Is Nothing Then FlowsheetSurface.ConnectObject(CType(obj, GraphicObject), CType(objTo, GraphicObject), -1, CInt(xel2.@AttachedToConnIndex))
                                    End If
                                End If
                            Next
                        End If
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
            End Try
        Next

    End Sub

    Sub AddTables(data As List(Of XElement), excs As Concurrent.ConcurrentBag(Of Exception))

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("Microsoft.MSDN.Samples.GraphicObjects", "DWSIM.DrawingTools.GraphicObjects")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Ajuste", "OT_Adjust")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Especificacao", "OT_Spec")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Reciclo", "OT_Recycle")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Texto", "GO_Text")
                xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Figura", "GO_Image")
                Dim obj As IGraphicObject = Nothing
                Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                If Not t Is Nothing Then obj = CType(Activator.CreateInstance(t), IGraphicObject)
                If obj Is Nothing Then obj = GraphicObject.ReturnInstance(xel.Element("Type").Value)
                If Not obj Is Nothing Then
                    DirectCast(obj, ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                    If TypeOf obj Is TableGraphic Then
                        DirectCast(obj, TableGraphic).Flowsheet = Me
                        If obj.Name = "" Then obj.Name = obj.Tag
                        FlowsheetSurface.DrawingObjects.Add(obj)
                        GraphicObjects.Add(obj.Name, obj)
                    End If
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
            End Try
        Next

    End Sub


    Sub AddSimulationObjects(objlist As Concurrent.ConcurrentBag(Of ISimulationObject), excs As Concurrent.ConcurrentBag(Of Exception))

        For Each obj In objlist
            Try
                SimulationObjects.Add(obj.Name, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Information", ex))
            End Try
        Next

        For Each so As ISimulationObject In SimulationObjects.Values
            Try
                If TryCast(so, Adjust) IsNot Nothing Then
                    Dim so2 As Adjust = CType(so, Adjust)
                    If SimulationObjects.ContainsKey(so2.ManipulatedObjectData.ID) Then
                        so2.ManipulatedObject = CType(SimulationObjects(so2.ManipulatedObjectData.ID), SharedClasses.UnitOperations.BaseClass)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = CType(so2.ManipulatedObject.GraphicObject, GraphicObject)
                    End If
                    If SimulationObjects.ContainsKey(so2.ControlledObjectData.ID) Then
                        so2.ControlledObject = CType(SimulationObjects(so2.ControlledObjectData.ID), SharedClasses.UnitOperations.BaseClass)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = CType(so2.ControlledObject.GraphicObject, GraphicObject)
                    End If
                    If SimulationObjects.ContainsKey(so2.ReferencedObjectData.ID) Then
                        so2.ReferenceObject = CType(SimulationObjects(so2.ReferencedObjectData.ID), SharedClasses.UnitOperations.BaseClass)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = CType(so2.ReferenceObject.GraphicObject, GraphicObject)
                    End If
                End If
                If TryCast(so, Spec) IsNot Nothing Then
                    Dim so2 As Spec = CType(so, Spec)
                    If SimulationObjects.ContainsKey(so2.TargetObjectData.ID) Then
                        so2.TargetObject = CType(SimulationObjects(so2.TargetObjectData.ID), SharedClasses.UnitOperations.BaseClass)
                        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = CType(so2.TargetObject.GraphicObject, GraphicObject)
                    End If
                    If SimulationObjects.ContainsKey(so2.SourceObjectData.ID) Then
                        so2.SourceObject = CType(SimulationObjects(so2.SourceObjectData.ID), SharedClasses.UnitOperations.BaseClass)
                        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = CType(so2.SourceObject.GraphicObject, GraphicObject)
                    End If
                End If
                If TryCast(so, CapeOpenUO) IsNot Nothing Then
                    DirectCast(so, CapeOpenUO).UpdateConnectors2()
                    DirectCast(so, CapeOpenUO).UpdatePortsFromConnectors()
                End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
            End Try
        Next

    End Sub

    Public Property AvailableCompounds As New Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.AvailableCompounds

    Public Sub Initialize() Implements IFlowsheet.Initialize

        AddHandler AppDomain.CurrentDomain.AssemblyResolve, New ResolveEventHandler(AddressOf LoadFromExtensionsFolder)

        FileDatabaseProvider.CreateDatabase()

        DynamicsManager.ToggleDynamicMode = Function()
                                                DynamicMode = Not DynamicMode
                                                Return DynamicMode
                                            End Function

        FlowsheetSurface.DrawPropertyList = Options.DisplayCornerPropertyList
        FlowsheetSurface.DrawFloatingTable = Options.DisplayFloatingPropertyTables

        ReactionSets.Add("DefaultSet", New ReactionSet("DefaultSet", "Default Set", ""))

        AddPropPacks()
        AddExternalUOs()

        Dim addedcomps As New List(Of String)
        Dim casnumbers As New List(Of String)

        Dim tc = TaskHelper.Run(Sub()
                                    Dim csdb As New Databases.ChemSep
                                    Dim cpa() As ConstantProperties
                                    csdb.Load()
                                    cpa = csdb.Transfer()
                                    For Each cp As ConstantProperties In cpa
                                        If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                    Next
                                    Dim cpdb As New Databases.CoolProp
                                    cpdb.Load()
                                    cpa = cpdb.Transfer()
                                    addedcomps = AvailableCompounds.Keys.Select(Function(x) x.ToLower).ToList()
                                    For Each cp As ConstantProperties In cpa
                                        If Not addedcomps.Contains(cp.Name.ToLower) Then AvailableCompounds.Add(cp.Name, cp)
                                    Next
                                    Dim bddb As New Databases.Biodiesel
                                    bddb.Load()
                                    cpa = bddb.Transfer()
                                    addedcomps = AvailableCompounds.Keys.Select(Function(x) x.ToLower).ToList()
                                    For Each cp As ConstantProperties In cpa
                                        If Not addedcomps.Contains(cp.Name.ToLower) Then AvailableCompounds.Add(cp.Name, cp)
                                    Next
                                    Dim chedl As New Databases.ChEDL_Thermo
                                    chedl.Load()
                                    cpa = chedl.Transfer().ToArray()
                                    addedcomps = AvailableCompounds.Keys.Select(Function(x) x.ToLower).ToList()
                                    casnumbers = AvailableCompounds.Values.Select(Function(x) x.CAS_Number).ToList()
                                    For Each cp As ConstantProperties In cpa
                                        If Not addedcomps.Contains(cp.Name.ToLower) And Not addedcomps.Contains(cp.Name) Then
                                            If Not casnumbers.Contains(cp.CAS_Number) Then
                                                If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                            End If
                                        End If
                                    Next
                                    Dim elec As New Databases.Electrolyte
                                    elec.Load()
                                    cpa = elec.Transfer().ToArray()
                                    addedcomps = AvailableCompounds.Keys.Select(Function(x) x.ToLower).ToList()
                                    For Each cp As ConstantProperties In cpa
                                        If Not addedcomps.Contains(cp.Name.ToLower) AndAlso Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                    Next
                                    Dim comps = Databases.UserDB.LoadAdditionalCompounds()
                                    For Each cp As BaseClasses.ConstantProperties In comps
                                        If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                    Next
                                    Using filestr As Stream = Assembly.GetAssembly(elec.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.FoodProp.xml")
                                        Dim fcomps = Databases.UserDB.ReadComps(filestr)
                                        For Each cp As BaseClasses.ConstantProperties In fcomps
                                            cp.CurrentDB = "FoodProp"
                                            If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                        Next
                                    End Using
                                    csdb.Dispose()
                                    cpdb.Dispose()
                                    chedl.Dispose()
                                    AddSystemsOfUnits()
                                    AddDefaultProperties()
                                End Sub)

        If GlobalSettings.Settings.AutomationMode Then tc.Wait()

    End Sub

    Public Sub Reset() Implements IFlowsheet.Reset

        SimulationObjects.Clear()
        GraphicObjects.Clear()
        FlowsheetSurface.DrawingObjects.Clear()
        SelectedCompounds.Clear()
        Options = New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables()
        FlowsheetSurface.Zoom = 1.0#

    End Sub

    Public Property ErrorMessage As String Implements IFlowsheet.ErrorMessage

    Public Function GetDockPanel() As Object Implements IFlowsheet.GetDockPanel
        Return Nothing
    End Function

    Public Function GetFlowsheetBag() As IFlowsheetBag Implements IFlowsheet.GetFlowsheetBag
        Return CType(Me, IFlowsheetBag)
    End Function

    Public Function GetUtility(uttype As Enums.FlowsheetUtility) As IAttachedUtility Implements IFlowsheet.GetUtility
        Return Nothing
    End Function

    Public Property MasterFlowsheet As IFlowsheet Implements IFlowsheet.MasterFlowsheet

    Public Property MasterUnitOp As ISimulationObject Implements IFlowsheet.MasterUnitOp

    Public Property Message As String Implements IFlowsheet.Message

    Public Property MobileCompatibilityMode As Boolean Implements IFlowsheet.MobileCompatibilityMode

    Public MustOverride Sub RunCodeOnUIThread(act As Action) Implements IFlowsheet.RunCodeOnUIThread

    Public MustOverride Sub SetMessageListener(act As Action(Of String, IFlowsheet.MessageType)) Implements IFlowsheet.SetMessageListener

    Public Property Solved As Boolean Implements IFlowsheet.Solved

    Public Sub UpdateSpreadsheet(act As Action) Implements IFlowsheet.UpdateSpreadsheet
        If Not act Is Nothing Then act.Invoke()
    End Sub

    Public MustOverride Sub UpdateInformation() Implements IFlowsheet.UpdateInformation

    Public MustOverride Sub UpdateInterface() Implements IFlowsheet.UpdateInterface

    Public ReadOnly Property UtilityPlugins As Dictionary(Of String, IUtilityPlugin) Implements IFlowsheet.UtilityPlugins
        Get
            Return Nothing
        End Get
    End Property

    Public Sub WriteSpreadsheetVariables(act As Action) Implements IFlowsheet.WriteSpreadsheetVariables
        If Not act Is Nothing Then act.Invoke()
    End Sub

    Public Sub ProcessScripts(ByVal sourceevent As Enums.Scripts.EventType, ByVal sourceobj As Enums.Scripts.ObjectType, ByVal sourceobjname As String) Implements IFlowsheet.ProcessScripts

        For Each scr As Script In Scripts.Values
            If scr.Linked And scr.LinkedEventType = sourceevent And scr.LinkedObjectType = sourceobj And scr.LinkedObjectName = sourceobjname Then
                If scr.LinkedObjectName <> "" Then
                    ShowMessage("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "', linked to '" & SimulationObjects(scr.LinkedObjectName).GraphicObject.Tag & "'...", IFlowsheet.MessageType.Information)
                Else
                    ShowMessage("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "'", IFlowsheet.MessageType.Information)
                End If
                If scr.PythonInterpreter = Enums.Scripts.Interpreter.IronPython Then
                    RunScript_IronPython(scr.ScriptText)
                Else
                    RunScript_PythonNET(scr.ScriptText)
                End If
            End If
        Next

    End Sub


    Public Function LoadZippedXML(pathtofile As String) As XDocument

        Dim pathtosave As String = Path.Combine(My.Computer.FileSystem.SpecialDirectories.Temp, Guid.NewGuid().ToString())

        Directory.CreateDirectory(pathtosave)

        Dim fullname As String = ""
        Dim dbfile As String = ""

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(pathtofile))
            stream.Password = Nothing
            Dim entry As ZipEntry
Label_00CC:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(Path.Combine(pathtosave, Path.GetFileName(entry.Name)))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                Dim extension = Path.GetExtension(entry.Name).ToLower()
                                If extension = ".xml" Then
                                    fullname = Path.Combine(pathtosave, Path.GetFileName(entry.Name))
                                ElseIf extension = ".db" Then
                                    dbfile = Path.Combine(pathtosave, Path.GetFileName(entry.Name))
                                End If
                                GoTo Label_00CC
                            End If
                            stream2.Write(buffer, 0, count)
                        Loop
                    End Using
                End If
                entry = stream.GetNextEntry
            Loop
        End Using

        Dim xdoc = XDocument.Load(fullname)
        LoadFromXML(xdoc)
        FilePath = pathtofile
        Options.FilePath = pathtofile
        If File.Exists(dbfile) Then
            Try
                FileDatabaseProvider.LoadDatabase(dbfile)
            Catch ex As Exception
            Finally
                File.Delete(dbfile)
            End Try
        End If
        File.Delete(fullname)

        Try
            Directory.Delete(pathtosave, True)
        Catch ex As Exception
        End Try

        Return xdoc

    End Function

    Public Shared Function LoadZippedXMLDoc(pathtofile As String) As XDocument

        Dim pathtosave As String = Path.Combine(My.Computer.FileSystem.SpecialDirectories.Temp, Guid.NewGuid().ToString())

        Directory.CreateDirectory(pathtosave)

        Dim fullname As String = ""

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(pathtofile))
            stream.Password = Nothing
            Dim entry As ZipEntry
Label_00CC:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(Path.Combine(pathtosave, Path.GetFileName(entry.Name)))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                Dim extension = Path.GetExtension(entry.Name).ToLower()
                                If extension = ".xml" Then
                                    fullname = Path.Combine(pathtosave, Path.GetFileName(entry.Name))
                                End If
                                GoTo Label_00CC
                            End If
                            stream2.Write(buffer, 0, count)
                        Loop
                    End Using
                End If
                entry = stream.GetNextEntry
            Loop
        End Using

        Dim xdoc = XDocument.Load(fullname)
        File.Delete(fullname)

        Try
            Directory.Delete(pathtosave, True)
        Catch ex As Exception
        End Try

        Return xdoc

    End Function


    Shared Function IsZipFilePasswordProtected(ByVal ZipFile As String) As Boolean
        Using fsIn As New FileStream(ZipFile, FileMode.Open, FileAccess.Read)
            Using zipInStream As New ZipInputStream(fsIn)
                Dim zEntry As ZipEntry = zipInStream.GetNextEntry()
                Return zEntry.IsCrypted
            End Using
        End Using
    End Function

    Public Shared Sub AddPropPacks()

        If AvailablePropPacks.Count > 0 Then Exit Sub

        Dim plist As New Concurrent.BlockingCollection(Of PropertyPackage)

        Dim t1 = TaskHelper.Run(Sub()

                                    Dim CPPP As CoolPropPropertyPackage = New CoolPropPropertyPackage()
                                    CPPP.ComponentName = "CoolProp"
                                    plist.Add(CPPP)

                                    Dim CPIPP As New CoolPropIncompressiblePurePropertyPackage()
                                    CPIPP.ComponentName = "CoolProp (Incompressible Fluids)"
                                    CPIPP.ComponentDescription = "CoolProp (Incompressible Fluids)"
                                    plist.Add(CPIPP)

                                    Dim CPIMPP As New CoolPropIncompressibleMixturePropertyPackage()
                                    CPIMPP.ComponentName = "CoolProp (Incompressible Mixtures)"
                                    CPIMPP.ComponentDescription = "CoolProp (Incompressible Mixtures)"
                                    plist.Add(CPIMPP)

                                    Dim STPP As SteamTablesPropertyPackage = New SteamTablesPropertyPackage()
                                    STPP.ComponentName = "Steam Tables (IAPWS-IF97)"
                                    plist.Add(STPP)

                                    Dim SEAPP As SeawaterPropertyPackage = New SeawaterPropertyPackage()
                                    SEAPP.ComponentName = "Seawater IAPWS-08"
                                    plist.Add(SEAPP)

                                End Sub)

        Dim t2 = TaskHelper.Run(Sub()

                                    Dim PRPP As PengRobinsonPropertyPackage = New PengRobinsonPropertyPackage()
                                    PRPP.ComponentName = "Peng-Robinson (PR)"
                                    plist.Add(PRPP)

                                End Sub)

        Dim t3 = TaskHelper.Run(Sub()

                                    Dim PRSV2PP As PRSV2PropertyPackage = New PRSV2PropertyPackage()
                                    PRSV2PP.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
                                    plist.Add(PRSV2PP)

                                    Dim PRSV2PPVL As PRSV2VLPropertyPackage = New PRSV2VLPropertyPackage()
                                    PRSV2PPVL.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
                                    plist.Add(PRSV2PPVL)

                                End Sub)

        Dim t4 = TaskHelper.Run(Sub()

                                    Dim SRKPP As SRKPropertyPackage = New SRKPropertyPackage()
                                    SRKPP.ComponentName = "Soave-Redlich-Kwong (SRK)"
                                    plist.Add(SRKPP)

                                End Sub)

        Dim t6 = TaskHelper.Run(Sub()

                                    Dim UPP As UNIFACPropertyPackage = New UNIFACPropertyPackage()
                                    UPP.ComponentName = "UNIFAC"
                                    plist.Add(UPP)

                                End Sub)

        Dim t7 = TaskHelper.Run(Sub()

                                    Dim ULLPP As UNIFACLLPropertyPackage = New UNIFACLLPropertyPackage()
                                    ULLPP.ComponentName = "UNIFAC-LL"
                                    plist.Add(ULLPP)

                                End Sub)

        Dim t8 = TaskHelper.Run(Sub()

                                    Dim MUPP As MODFACPropertyPackage = New MODFACPropertyPackage()
                                    MUPP.ComponentName = "Modified UNIFAC (Dortmund)"
                                    plist.Add(MUPP)

                                End Sub)

        Dim t9 = TaskHelper.Run(Sub()

                                    Dim NUPP As NISTMFACPropertyPackage = New NISTMFACPropertyPackage()
                                    NUPP.ComponentName = "Modified UNIFAC (NIST)"
                                    plist.Add(NUPP)

                                End Sub)

        Dim t10 = TaskHelper.Run(Sub()

                                     Dim WPP As WilsonPropertyPackage = New WilsonPropertyPackage()
                                     WPP.ComponentName = "Wilson"
                                     plist.Add(WPP)

                                     Dim NRTLPP As NRTLPropertyPackage = New NRTLPropertyPackage()
                                     NRTLPP.ComponentName = "NRTL"
                                     plist.Add(NRTLPP)

                                     Dim UQPP As UNIQUACPropertyPackage = New UNIQUACPropertyPackage()
                                     UQPP.ComponentName = "UNIQUAC"
                                     plist.Add(UQPP)

                                     Dim CSLKPP As ChaoSeaderPropertyPackage = New ChaoSeaderPropertyPackage()
                                     CSLKPP.ComponentName = "Chao-Seader"
                                     plist.Add(CSLKPP)

                                     Dim GSLKPP As GraysonStreedPropertyPackage = New GraysonStreedPropertyPackage()
                                     GSLKPP.ComponentName = "Grayson-Streed"
                                     plist.Add(GSLKPP)

                                     Dim RPP As RaoultPropertyPackage = New RaoultPropertyPackage()
                                     RPP.ComponentName = "Raoult's Law"
                                     plist.Add(RPP)

                                     Dim LKPPP As LKPPropertyPackage = New LKPPropertyPackage()
                                     LKPPP.ComponentName = "Lee-Kesler-Plöcker"
                                     plist.Add(LKPPP)

                                 End Sub)

        Dim t11 = TaskHelper.Run(Sub()

                                     Dim ISPP As New IdealElectrolytePropertyPackage()
                                     plist.Add(ISPP)

                                     Dim BOPP As BlackOilPropertyPackage = New BlackOilPropertyPackage()
                                     BOPP.ComponentName = "Black Oil"
                                     plist.Add(BOPP)

                                     Dim GERGPP As GERG2008PropertyPackage = New GERG2008PropertyPackage()
                                     plist.Add(GERGPP)

                                     Dim PCSAFTPP As PCSAFT2PropertyPackage = New PCSAFT2PropertyPackage()
                                     plist.Add(PCSAFTPP)

                                 End Sub)

        Dim t12 = TaskHelper.Run(Sub()

                                     Dim PR78PP As PengRobinson1978PropertyPackage = New PengRobinson1978PropertyPackage()
                                     PR78PP.ComponentName = "Peng-Robinson 1978 (PR78)"
                                     plist.Add(PR78PP)

                                 End Sub)

        Dim t13 = TaskHelper.Run(Sub()

                                     Dim PR78Adv As PengRobinson1978AdvancedPropertyPackage = New PengRobinson1978AdvancedPropertyPackage()
                                     plist.Add(PR78Adv)

                                 End Sub)

        Dim t14 = TaskHelper.Run(Sub()

                                     Dim SRKAdv As SoaveRedlichKwongAdvancedPropertyPackage = New SoaveRedlichKwongAdvancedPropertyPackage()
                                     plist.Add(SRKAdv)

                                 End Sub)

        Task.WaitAll(t1, t2, t3, t4, t6, t7, t8, t9, t10, t11, t12, t13, t14)

        For Each pp In plist
            AvailablePropPacks.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName, pp)
        Next

        Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()

        For Each pp In otherpps
            If Not AvailablePropPacks.ContainsKey(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName) Then
                AvailablePropPacks.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName, pp)
            Else
                Console.WriteLine(String.Format("Error adding External Property Package '{0}'. Check the 'ppacks' and 'extenders' folders for duplicate items.", pp.ComponentName))
            End If
        Next

        'Check if DWSIM is running in Portable/Mono mode, if not then load the CAPE-OPEN Wrapper Property Package.
        If Not GlobalSettings.Settings.IsRunningOnMono Then
            Dim COPP As CAPEOPENPropertyPackage = New CAPEOPENPropertyPackage()
            COPP.ComponentName = "CAPE-OPEN"
            AvailablePropPacks.Add(COPP.ComponentName.ToString, COPP)
        End If

    End Sub

    Sub AddExternalUOs()

        Dim otheruos = SharedClasses.Utility.LoadAdditionalUnitOperations()

        Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

        If unitopassembly Is Nothing Then
            unitopassembly = Assembly.Load("DWSIM.UnitOperations")
        End If

        Dim euolist As List(Of Interfaces.IExternalUnitOperation) = SharedClasses.Utility.GetUnitOperations(unitopassembly)

        otheruos.AddRange(euolist)

        For Each uo In otheruos
            If Not ExternalUnitOperations.ContainsKey(uo.Description) Then
                ExternalUnitOperations.Add(uo.Description, uo)
            Else
                Console.WriteLine(String.Format("Error adding External Unit Operation '{0}'. Check the 'unitops' and 'extenders' folders for duplicate items.", uo.Description))
            End If
        Next

    End Sub


    Function GetPropertyPackages(ByVal assmbly As Assembly) As List(Of Interfaces.IPropertyPackage)

        Dim availableTypes As New List(Of Type)()

        Try
            availableTypes.AddRange(assmbly.GetTypes())
        Catch ex As Exception
            Logging.Logger.LogError("Property Package Loading (CPUI)", ex)
        End Try

        Dim ppList As List(Of Type) = availableTypes.FindAll(Function(t) t.GetInterfaces().Contains(GetType(Interfaces.IPropertyPackage)) And Not t.IsAbstract)

        Return ppList.ConvertAll(Of Interfaces.IPropertyPackage)(Function(t As Type) TryCast(Activator.CreateInstance(t), Interfaces.IPropertyPackage))

    End Function

    Private Sub AddSystemsOfUnits()

        With Me.AvailableSystemsOfUnits

            .Add(New SystemsOfUnits.SI)
            .Add(New SystemsOfUnits.SI_ENG)
            .Add(New SystemsOfUnits.CGS)
            .Add(New SystemsOfUnits.English)
            .Add(New SystemsOfUnits.SIUnits_Custom1)
            .Add(New SystemsOfUnits.SIUnits_Custom2)
            .Add(New SystemsOfUnits.SIUnits_Custom3)
            .Add(New SystemsOfUnits.SIUnits_Custom4)
            .Add(New SystemsOfUnits.SIUnits_Custom5)

            Try
                Dim options = New Newtonsoft.Json.JsonSerializerSettings
                options.StringEscapeHandling = Newtonsoft.Json.StringEscapeHandling.EscapeHtml
                Dim userunits = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of SystemsOfUnits.Units))(GlobalSettings.Settings.UserUnits, options)
                Dim dontadd = New String() {"SI", "SI (Enginnering)", "CGS", "ENG", "C1", "C2", "C3", "C4", "C5"}
                For Each unit In userunits
                    If Not dontadd.Contains(unit.Name) Then .Add(unit)
                Next
            Catch ex As Exception

            End Try

        End With

    End Sub

    Sub AddDefaultProperties()

        If Me.FlowsheetOptions.VisibleProperties.Count = 0 Then

            Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
            Dim unitopassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.UnitOperations")).FirstOrDefault

            If calculatorassembly Is Nothing Then
                calculatorassembly = AppDomain.CurrentDomain.Load("DWSIM.Thermodynamics")
            End If
            If unitopassembly Is Nothing Then
                unitopassembly = AppDomain.CurrentDomain.Load("DWSIM.UnitOperations")
            End If

            Dim aTypeList As New List(Of Type)
            aTypeList.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))
            aTypeList.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing And
                                                                   Not x.IsAbstract And x.GetInterface("DWSIM.Interfaces.IExternalUnitOperation") Is Nothing, True, False)))

            For Each item In aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    obj.SetFlowsheet(Me)
                    Me.FlowsheetOptions.VisibleProperties(item.Name) = obj.GetDefaultProperties.ToList
                    obj = Nothing
                End If
            Next

            For Each obj In ExternalUnitOperations.Values
                obj.SetFlowsheet(Me)
                Me.FlowsheetOptions.VisibleProperties(obj.GetType.Name) = DirectCast(obj, ISimulationObject).GetDefaultProperties().ToList()
            Next

        End If

    End Sub

    Public Function GetSpreadsheetData(range As String) As List(Of String()) Implements IFlowsheet.GetSpreadsheetData

        Return RetrieveSpreadsheetData.Invoke(range)

    End Function

    Public Function GetSpreadsheetFormat(range As String) As List(Of String()) Implements IFlowsheet.GetSpreadsheetFormat

        Return RetrieveSpreadsheetFormat.Invoke(range)

    End Function

    Public Property Scripts As New Dictionary(Of String, IScript) Implements IFlowsheet.Scripts

    Public Property Charts As Dictionary(Of String, IChart) = New Dictionary(Of String, IChart) Implements IFlowsheet.Charts

    Public Property StoredSolutions As Dictionary(Of String, List(Of XElement)) = New Dictionary(Of String, List(Of XElement)) Implements IFlowsheet.StoredSolutions

    Public Property ExternalSolvers As Dictionary(Of String, IExternalSolverIdentification) = New Dictionary(Of String, IExternalSolverIdentification) Implements IFlowsheet.ExternalSolvers

    Public Property FileDatabaseProvider As IFileDatabaseProvider = New FileStorage.FileDatabaseProvider Implements IFlowsheet.FileDatabaseProvider

    Public Property WatchItems As List(Of IWatchItem) = New List(Of IWatchItem) Implements IFlowsheet.WatchItems

    Public Sub RunScript(ScriptID As String)
        Dim script = Scripts(ScriptID)
        PythonPreprocessor?.Invoke(script.ScriptText)
        If script.PythonInterpreter = Enums.Scripts.Interpreter.IronPython Then
            RunScript_IronPython(script.ScriptText)
        Else
            RunScript_PythonNET(script.ScriptText)
        End If
    End Sub

    Public Async Sub RunScriptAsync(ScriptID As String)
        Await TaskHelper.Run(Sub() RunScript(ScriptID))
    End Sub

    Private Sub RunScript_IronPython(scripttext As String)

        Dim scope As Microsoft.Scripting.Hosting.ScriptScope
        Dim engine As Microsoft.Scripting.Hosting.ScriptEngine

        Dim opts As New Dictionary(Of String, Object)()
        opts("Frames") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True
        engine = IronPython.Hosting.Python.CreateEngine(opts)

        Dim paths0 = engine.GetSearchPaths().ToList()
        Dim apppath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        paths0.Add(Path.Combine(apppath, "Lib"))
        Try
            engine.SetSearchPaths(paths0)
        Catch ex As Exception
        End Try

        engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
        engine.Runtime.LoadAssembly(GetType(Thermodynamics.BaseClasses.ConstantProperties).Assembly)
        engine.Runtime.LoadAssembly(GetType(Drawing.SkiaSharp.GraphicsSurface).Assembly)
        engine.Runtime.IO.SetOutput(New FlowsheetLogTextStream(Me), UTF8Encoding.UTF8)
        scope = engine.CreateScope()
        scope.SetVariable("Plugins", UtilityPlugins)
        scope.SetVariable("Flowsheet", Me)
        scope.SetVariable("Application", GetApplicationObject)
        If GetSpreadsheetObjectFunc IsNot Nothing Then
            scope.SetVariable("Spreadsheet", GetSpreadsheetObjectFunc.Invoke())
        End If
        Dim Solver As New FlowsheetSolver.FlowsheetSolver
        scope.SetVariable("Solver", Solver)
        For Each obj As ISimulationObject In SimulationObjects.Values
            scope.SetVariable(obj.GraphicObject.Tag.Replace("-", "_"), obj)
        Next
        Dim txtcode As String = scripttext
        Dim source As Microsoft.Scripting.Hosting.ScriptSource = engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)
        Try
            source.Execute(scope)
        Catch ex As Exception
            Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
            ShowMessage("Error running script: " & ops.FormatException(ex).ToString, IFlowsheet.MessageType.GeneralError)
        Finally
            engine.Runtime.Shutdown()
            engine = Nothing
            scope = Nothing
            source = Nothing
        End Try

    End Sub

    Private Sub RunScript_PythonNET(scripttext As String)

        If GlobalSettings.Settings.RunningPlatform <> Settings.Platform.Windows Then

            If Not GlobalSettings.Settings.PythonInitialized Then

                Runtime.PythonDLL = GlobalSettings.Settings.PythonPath
                PythonEngine.Initialize()
                GlobalSettings.Settings.PythonInitialized = True
                PythonEngine.BeginAllowThreads()

            End If

            Using Py.GIL

                Try

                    Dim sys As Object = Py.Import("sys")

                    Dim codeToRedirectOutput As String = "import sys" & vbCrLf + "from io import BytesIO as StringIO" & vbCrLf + "sys.stdout = mystdout = StringIO()" & vbCrLf + "sys.stdout.flush()" & vbCrLf + "sys.stderr = mystderr = StringIO()" & vbCrLf + "sys.stderr.flush()"
                    PythonEngine.RunSimpleString(codeToRedirectOutput)

                    Dim locals As New PyDict()

                    locals.SetItem("Plugins", UtilityPlugins.ToPython)
                    locals.SetItem("Flowsheet", Me.ToPython)
                    Try
                        locals.SetItem("Spreadsheet", (GetSpreadsheetObjectFunc.Invoke()).ToPython)
                    Catch ex As Exception
                    End Try
                    Dim Solver As New FlowsheetSolver.FlowsheetSolver
                    locals.SetItem("Solver", Solver.ToPython)

                    If Not GlobalSettings.Settings.IsRunningOnMono() Then
                        locals.SetItem("Application", GetApplicationObject.ToPython)
                    End If

                    PythonEngine.Exec(scripttext, Nothing, locals)

                    If Not GlobalSettings.Settings.IsRunningOnMono() Then
                        ShowMessage(sys.stdout.getvalue().ToString, IFlowsheet.MessageType.Information)
                    End If

                Catch ex As Exception

                    ShowMessage("Error running script: " & ex.Message.ToString, IFlowsheet.MessageType.GeneralError)

                Finally

                End Try

            End Using

        Else

            GlobalSettings.Settings.InitializePythonEnvironment()

            Using Py.GIL

                Try

                    Dim sys As Object = Py.Import("sys")

                    'If Not GlobalSettings.Settings.IsRunningOnMono() Then
                    Dim codeToRedirectOutput As String = "import sys" & vbCrLf + "from io import BytesIO as StringIO" & vbCrLf + "sys.stdout = mystdout = StringIO()" & vbCrLf + "sys.stdout.flush()" & vbCrLf + "sys.stderr = mystderr = StringIO()" & vbCrLf + "sys.stderr.flush()"
                    PythonEngine.RunSimpleString(codeToRedirectOutput)
                    'End If

                    Dim locals As New PyDict()

                    locals.SetItem("Plugins", UtilityPlugins.ToPython)
                    locals.SetItem("Flowsheet", Me.ToPython)
                    Try
                        locals.SetItem("Spreadsheet", (GetSpreadsheetObjectFunc.Invoke()).ToPython)
                    Catch ex As Exception
                    End Try
                    Dim Solver As New FlowsheetSolver.FlowsheetSolver
                    locals.SetItem("Solver", Solver.ToPython)

                    If Not GlobalSettings.Settings.IsRunningOnMono() Then
                        locals.SetItem("Application", GetApplicationObject.ToPython)
                    End If

                    PythonEngine.Exec(scripttext, Nothing, locals)

                    If Not GlobalSettings.Settings.IsRunningOnMono() Then
                        ShowMessage(sys.stdout.getvalue().ToString, IFlowsheet.MessageType.Information)
                    End If

                Catch ex As Exception

                    ShowMessage("Error running script: " & ex.ToString, IFlowsheet.MessageType.GeneralError)

                Finally

                End Try

            End Using

        End If

    End Sub

    Private Class FlowsheetLogTextStream

        Inherits MemoryStream
        Private target As FlowsheetBase

        Public Sub New(ByVal target As FlowsheetBase)
            Me.target = target
        End Sub

        Public Overrides Sub Write(ByVal buffer As Byte(), ByVal offset As Integer, ByVal count As Integer)
            Dim output As String = Encoding.UTF8.GetString(buffer, offset, count)
            If output.Trim().Length > 0 Then
                Console.WriteLine(output)
                target.ShowMessage(output, IFlowsheet.MessageType.Information)
            End If
        End Sub

    End Class

    Public MustOverride Function GetApplicationObject() As Object Implements IFlowsheet.GetApplicationObject

    Public Function GetFlowsheetSurfaceWidth() As Integer Implements IFlowsheet.GetFlowsheetSurfaceWidth
        Return FlowsheetSurface.Size.Width
    End Function

    Public Function GetFlowsheetSurfaceHeight() As Integer Implements IFlowsheet.GetFlowsheetSurfaceHeight
        Return FlowsheetSurface.Size.Height
    End Function

    Public Function ChangeCalculationOrder(objects As List(Of String)) As List(Of String) Implements IFlowsheet.ChangeCalculationOrder

        Dim olist As List(Of String) = objects

        RunCodeOnUIThread(Sub()
                              Dim frm As New SharedClasses.FormCustomCalcOrder
                              frm.Flowsheet = Me
                              frm.ItemList = objects
                              frm.ShowDialog()
                              olist = frm.NewItemList
                          End Sub)

        Return olist

    End Function

    Public Function GetSpreadsheetObject() As Object Implements IFlowsheet.GetSpreadsheetObject

        Return GetSpreadsheetObjectFunc?.Invoke()

    End Function
    Public Function GetObject(name As String) As ISimulationObject Implements IFlowsheet.GetObject
        Return GetFlowsheetSimulationObject(name)
    End Function

    Public Function GetCompound(name As String) As ICompoundConstantProperties Implements IFlowsheet.GetCompound
        Return AvailableCompounds(name)
    End Function

    Public Function GetPropertyPackage(name As String) As IPropertyPackage Implements IFlowsheet.GetPropertyPackage
        Return PropertyPackages.Values.Where(Function(x) x.Tag = name).FirstOrDefault
    End Function

    Public Function GetReaction(name As String) As IReaction Implements IFlowsheet.GetReaction
        Return Reactions.Values.Where(Function(x) x.Name = name).FirstOrDefault
    End Function

    Public Function GetReactionSet(name As String) As IReactionSet Implements IFlowsheet.GetReactionSet
        Return ReactionSets.Values.Where(Function(x) x.Name = name).FirstOrDefault
    End Function

    Public Sub AutoLayout() Implements IFlowsheet.AutoLayout
        FlowsheetSurface.AutoArrange()
    End Sub

    Public Sub NaturalLayout() Implements IFlowsheet.NaturalLayout
        FlowsheetSurface.ApplyNaturalLayout(FlowsheetSolver.FlowsheetSolver.GetSolvingList(Me, False)(0), 75)
    End Sub

    Public Sub RefreshInterface() Implements IFlowsheet.RefreshInterface
        UpdateInterface()
    End Sub

    Public Sub SetTranslateTextExternalFunction(act As Func(Of String, String)) Implements IFlowsheet.SetTranslateTextExternalFunction
        _translatefunction = act
    End Sub

    Public Function GetScriptText(name As String) As String Implements IFlowsheet.GetScriptText

        Return Scripts.Values.Where(Function(x) x.Title = name).FirstOrDefault().ScriptText

    End Function

    Public Function GetSimulationFilePath() As String Implements IFlowsheet.GetSimulationFilePath

        Return FlowsheetOptions.FilePath

    End Function

    Public Function GetSimulationFileDirectory() As String Implements IFlowsheet.GetSimulationFileDirectory

        Return Path.GetDirectoryName(FlowsheetOptions.FilePath)

    End Function

    Public Overridable Sub ClearLog() Implements IFlowsheet.ClearLog


    End Sub

    Public MustOverride Property SupressMessages As Boolean Implements IFlowsheet.SupressMessages

    Private Sub IFlowsheet_RunScript(name As String) Implements IFlowsheet.RunScript
        Dim script = Scripts.Where(Function(s) s.Value.Title = name).FirstOrDefault()
        RunScript(script.Key)
    End Sub

    Public Property PythonPreprocessor() As Action(Of String) Implements IFlowsheet.PythonPreprocessor

    Public Property AvailableExternalUnitOperations As Dictionary(Of String, IExternalUnitOperation) Implements IFlowsheet.AvailableExternalUnitOperations
        Get
            Return ExternalUnitOperations
        End Get
        Set(value As Dictionary(Of String, IExternalUnitOperation))
            ExternalUnitOperations = value
        End Set
    End Property

    Private Shared Function LoadFromExtensionsFolder(ByVal sender As Object, ByVal args As ResolveEventArgs) As Assembly

        Dim assemblyPath1 As String = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location), New AssemblyName(args.Name).Name + ".dll")
        Dim assemblyPath2 As String = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly.Location), "extenders", New AssemblyName(args.Name).Name + ".dll")

        If Not File.Exists(assemblyPath1) Then
            If Not File.Exists(assemblyPath2) Then
                Return Nothing
            Else
                Dim assembly As Assembly = Assembly.LoadFrom(assemblyPath2)
                Return assembly
            End If
        Else
            Dim assembly As Assembly = Assembly.LoadFrom(assemblyPath1)
            Return assembly
        End If

    End Function

    Public Sub ToggleFlowsheetAnimation() Implements IFlowsheet.ToggleFlowsheetAnimation
        Throw New NotImplementedException()
    End Sub

    Public Function RunCodeOnUIThread2(act As Action) As Task Implements IFlowsheet.RunCodeOnUIThread2
        Throw New NotImplementedException()
    End Function

    Public Function CreateConversionReaction(name As String, description As String, compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                             basecompound As String, reactionphase As String, conversionExpression As String) As IReaction Implements IFlowsheet.CreateConversionReaction

        Dim r As New Reaction()
        r.ReactionType = ReactionType.Conversion
        r.ID = name
        r.Name = name
        r.Description = description
        For Each kvp In compounds_and_stoichcoeffs
            r.Components.Add(kvp.Key, New ReactionStoichBase(kvp.Key, kvp.Value, False, 0, 0))
        Next
        r.Components(basecompound).IsBaseReactant = True
        r.BaseReactant = basecompound
        CalcReactionStoichiometry(r)
        Select Case reactionphase.ToLower()
            Case "mixture"
                r.ReactionPhase = PhaseName.Mixture
            Case "vapor"
                r.ReactionPhase = PhaseName.Vapor
            Case "liquid"
                r.ReactionPhase = PhaseName.Liquid
            Case "solid"
                r.ReactionPhase = PhaseName.Solid
        End Select
        r.Expression = conversionExpression

        Return r

    End Function

    Public Function CreateEquilibriumReaction(name As String, description As String, compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                              basecompound As String, reactionphase As String, basis As String, units As String, Tapproach As Double,
                                              lnKeq_fT As String) As IReaction Implements IFlowsheet.CreateEquilibriumReaction

        Dim r As New Reaction()
        r.ReactionType = ReactionType.Equilibrium
        r.ID = name
        r.Name = name
        r.Description = description
        For Each kvp In compounds_and_stoichcoeffs
            r.Components.Add(kvp.Key, New ReactionStoichBase(kvp.Key, kvp.Value, False, 0, 0))
        Next
        r.Components(basecompound).IsBaseReactant = True
        r.BaseReactant = basecompound
        CalcReactionStoichiometry(r)
        Select Case reactionphase.ToLower()
            Case "mixture"
                r.ReactionPhase = PhaseName.Mixture
            Case "vapor"
                r.ReactionPhase = PhaseName.Vapor
            Case "liquid"
                r.ReactionPhase = PhaseName.Liquid
            Case "solid"
                r.ReactionPhase = PhaseName.Solid
        End Select
        Select Case basis.ToLower()
            Case "activity"
                r.ReactionBasis = ReactionBasis.Activity
            Case "fugacity"
                r.ReactionBasis = ReactionBasis.Fugacity
            Case "molar concentration"
                r.ReactionBasis = ReactionBasis.MolarConc
            Case "mass concentration"
                r.ReactionBasis = ReactionBasis.MassConc
            Case "molar fraction"
                r.ReactionBasis = ReactionBasis.MolarFrac
            Case "mass fraction"
                r.ReactionBasis = ReactionBasis.MassFrac
            Case "partial pressure"
                r.ReactionBasis = ReactionBasis.PartialPress
        End Select
        r.EquilibriumReactionBasisUnits = units
        r.Approach = Tapproach
        If lnKeq_fT <> "" Then r.KExprType = KOpt.Expression Else r.KExprType = KOpt.Gibbs
        r.Expression = lnKeq_fT

        Return r

    End Function

    Public Function CreateKineticReaction(name As String, description As String, compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                          directorders As Dictionary(Of String, Double), reverseorders As Dictionary(Of String, Double),
                                          basecompound As String, reactionphase As String, basis As String, amountunits As String,
                                          rateunits As String, Aforward As Double, Eforward As Double, Areverse As Double, Ereverse As Double,
                                          Expr_forward As String, Expr_reverse As String) As IReaction Implements IFlowsheet.CreateKineticReaction

        Dim r As New Reaction()
        r.ReactionType = ReactionType.Kinetic
        r.ID = name
        r.Name = name
        r.Description = description
        For Each kvp In compounds_and_stoichcoeffs
            r.Components.Add(kvp.Key, New ReactionStoichBase(kvp.Key, kvp.Value, False, directorders(kvp.Key), reverseorders(kvp.Key)))
        Next
        r.Components(basecompound).IsBaseReactant = True
        r.BaseReactant = basecompound
        CalcReactionStoichiometry(r)
        Select Case reactionphase.ToLower()
            Case "mixture"
                r.ReactionPhase = PhaseName.Mixture
            Case "vapor"
                r.ReactionPhase = PhaseName.Vapor
            Case "liquid"
                r.ReactionPhase = PhaseName.Liquid
            Case "solid"
                r.ReactionPhase = PhaseName.Solid
        End Select
        Select Case basis.ToLower()
            Case "activity"
                r.ReactionBasis = ReactionBasis.Activity
            Case "fugacity"
                r.ReactionBasis = ReactionBasis.Fugacity
            Case "molar concentration"
                r.ReactionBasis = ReactionBasis.MolarConc
            Case "mass concentration"
                r.ReactionBasis = ReactionBasis.MassConc
            Case "molar fraction"
                r.ReactionBasis = ReactionBasis.MolarFrac
            Case "mass fraction"
                r.ReactionBasis = ReactionBasis.MassFrac
            Case "partial pressure"
                r.ReactionBasis = ReactionBasis.PartialPress
        End Select
        r.VelUnit = rateunits
        r.ConcUnit = amountunits
        r.A_Forward = Aforward
        r.E_Forward = Eforward
        r.A_Reverse = Areverse
        r.E_Reverse = Ereverse
        If Expr_forward <> "" Then
            r.ReactionKinFwdType = ReactionKineticType.UserDefined
            r.ReactionKinFwdExpression = Expr_forward
        Else
            r.ReactionKinFwdType = ReactionKineticType.Arrhenius
        End If
        If Expr_reverse <> "" Then
            r.ReactionKinRevType = ReactionKineticType.UserDefined
            r.ReactionKinRevExpression = Expr_reverse
        Else
            r.ReactionKinRevType = ReactionKineticType.Arrhenius
        End If

        Return r

    End Function

    Public Function CreateHetCatReaction(name As String, description As String, compounds_and_stoichcoeffs As Dictionary(Of String, Double),
                                         basecompound As String, reactionphase As String, basis As String, amountunits As String,
                                         rateunits As String, numeratorExpression As String, denominatorExpression As String) As IReaction Implements IFlowsheet.CreateHetCatReaction

        Dim r As New Reaction()
        r.ReactionType = ReactionType.Heterogeneous_Catalytic
        r.ID = name
        r.Name = name
        r.Description = description
        For Each kvp In compounds_and_stoichcoeffs
            r.Components.Add(kvp.Key, New ReactionStoichBase(kvp.Key, kvp.Value, False, 0, 0))
        Next
        r.Components(basecompound).IsBaseReactant = True
        r.BaseReactant = basecompound
        CalcReactionStoichiometry(r)
        Select Case reactionphase.ToLower()
            Case "mixture"
                r.ReactionPhase = PhaseName.Mixture
            Case "vapor"
                r.ReactionPhase = PhaseName.Vapor
            Case "liquid"
                r.ReactionPhase = PhaseName.Liquid
            Case "solid"
                r.ReactionPhase = PhaseName.Solid
        End Select
        Select Case basis.ToLower()
            Case "activity"
                r.ReactionBasis = ReactionBasis.Activity
            Case "fugacity"
                r.ReactionBasis = ReactionBasis.Fugacity
            Case "molar concentration"
                r.ReactionBasis = ReactionBasis.MolarConc
            Case "mass concentration"
                r.ReactionBasis = ReactionBasis.MassConc
            Case "molar fraction"
                r.ReactionBasis = ReactionBasis.MolarFrac
            Case "mass fraction"
                r.ReactionBasis = ReactionBasis.MassFrac
            Case "partial pressure"
                r.ReactionBasis = ReactionBasis.PartialPress
        End Select
        r.VelUnit = rateunits
        r.ConcUnit = amountunits
        r.RateEquationNumerator = numeratorExpression
        r.RateEquationDenominator = denominatorExpression

        Return r

    End Function

    Public Function CreateReactionSet(name As String, description As String) As IReactionSet Implements IFlowsheet.CreateReactionSet

        Dim rs As New ReactionSet(name, name, description)
        Return rs

    End Function

    Public Sub AddReaction(reaction As IReaction) Implements IFlowsheet.AddReaction

        Reactions.Add(reaction.ID, reaction)

    End Sub

    Public Sub AddReactionSet(reactionSet As IReactionSet) Implements IFlowsheet.AddReactionSet

        ReactionSets.Add(reactionSet.ID, reactionSet)

    End Sub

    Public Sub AddReactionToSet(reactionID As String, reactionSetID As String, enabled As Boolean, rank As Integer) Implements IFlowsheet.AddReactionToSet

        ReactionSets(reactionSetID).Reactions.Add(reactionID, New ReactionSetBase(reactionID, rank, enabled))

    End Sub

    Public Function GetAvailablePropertyPackages() As List(Of String) Implements IFlowsheet.GetAvailablePropertyPackages

        Return AvailablePropertyPackages.Keys.ToList()

    End Function

    Public Function CreatePropertyPackage(name As String) As IPropertyPackage Implements IFlowsheet.CreatePropertyPackage

        Dim pp = AvailablePropertyPackages(name).Clone()
        pp.Tag = pp.ComponentName
        Return pp

    End Function

    Public Function CreateAndAddPropertyPackage(name As String) As IPropertyPackage Implements IFlowsheet.CreateAndAddPropertyPackage

        Dim pp = AvailablePropertyPackages(name).Clone()
        pp.Tag = pp.ComponentName
        AddPropertyPackage(pp)
        Return pp

    End Function

    Public Function AddCompound(compname As String) As ICompoundConstantProperties Implements IFlowsheet.AddCompound

        Dim c = GetCompound(compname)
        Options.SelectedComponents.Add(c.Name, c)
        If Options.NotSelectedComponents.ContainsKey(c.Name) Then Options.NotSelectedComponents.Remove(c.Name)
        Return c

    End Function

    Private Sub CalcReactionStoichiometry(rc As IReaction)

        Dim hp, hr, bp, br, brsc, gp, gr As Double

        Dim eq As String = ""
        'build reaction equation
        'scan for reactants
        For Each c In rc.Components
            Dim comp = Options.SelectedComponents(c.Key)
            If c.Value.StoichCoeff < 0 Then
                If c.Value.StoichCoeff = -1 Then
                    eq += comp.Formula & " + "
                Else
                    eq += Math.Abs(c.Value.StoichCoeff) & comp.Formula & " + "
                End If
                hr += Math.Abs(c.Value.StoichCoeff) * comp.IG_Enthalpy_of_Formation_25C * comp.Molar_Weight
                br += Math.Abs(c.Value.StoichCoeff) * comp.Molar_Weight
                gr += Math.Abs(c.Value.StoichCoeff) * comp.IG_Gibbs_Energy_of_Formation_25C * comp.Molar_Weight
            End If
        Next
        If eq.Length >= 2 Then eq = eq.Remove(eq.Length - 2, 2)
        eq += "<--> "
        'scan for products
        For Each c In rc.Components
            Dim comp = Options.SelectedComponents(c.Key)
            If c.Value.StoichCoeff > 0 Then
                If c.Value.StoichCoeff = 1 Then
                    eq += comp.Formula & " + "
                Else
                    eq += Math.Abs(c.Value.StoichCoeff) & comp.Formula & " + "
                End If
                hp += Math.Abs(c.Value.StoichCoeff) * comp.IG_Enthalpy_of_Formation_25C * comp.Molar_Weight
                bp += Math.Abs(c.Value.StoichCoeff) * comp.Molar_Weight
                gp += Math.Abs(c.Value.StoichCoeff) * comp.IG_Gibbs_Energy_of_Formation_25C * comp.Molar_Weight
            End If
        Next
        eq = eq.Remove(eq.Length - 2, 2)

        brsc = Math.Abs(rc.Components.Where(Function(c) c.Value.IsBaseReactant).FirstOrDefault().Value.StoichCoeff)

        rc.ReactionHeat = (hp - hr) / brsc
        rc.ReactionGibbsEnergy = (gp - gr) / brsc

        rc.StoichBalance = bp - br
        rc.Equation = eq

    End Sub

    Public Sub SetDirtyStatus() Implements IFlowsheet.SetDirtyStatus

        For Each obj In SimulationObjects.Values
            obj.SetDirtyStatus(True)
        Next

    End Sub

    Public Sub DisplayBrowserWindow(url As String) Implements IFlowsheet.DisplayBrowserWindow
        Throw New NotImplementedException()
    End Sub

    Public Sub DisplayDockableBrowserWindow(url As String) Implements IFlowsheet.DisplayDockableBrowserWindow
        Throw New NotImplementedException()
    End Sub

#Region "    Snapshots"

    Public Sub RegisterSnapshot(stype As SnapshotType, Optional obj As ISimulationObject = Nothing) Implements IFlowsheet.RegisterSnapshot

        If Options.EnabledUndoRedo Then

            Dim sdata = GetSnapshot(stype, obj)
            UndoStack.Push(New Tuple(Of SnapshotType, XDocument)(stype, sdata))
            RedoStack.Clear()

        End If

    End Sub

    Public Function GetSnapshot(stype As SnapshotType, Optional obj As ISimulationObject = Nothing) As XDocument Implements IFlowsheet.GetSnapshot

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim IncludeObjectData, IncludeObjectLayout, RestoreObjects, IncludeCompounds, IncludeReactionSubsystem, IncludePropertyPackages,
            IncludeSpreadsheet, IncludeSimulationSettings, IncludeWindowLayout As Boolean

        Select Case stype
            Case SnapshotType.All
                IncludeCompounds = True
                IncludeObjectData = True
                IncludeObjectLayout = True
                IncludePropertyPackages = True
                IncludeReactionSubsystem = True
                IncludeSimulationSettings = True
                IncludeSpreadsheet = True
                IncludeWindowLayout = True
                RestoreObjects = True
            Case SnapshotType.Compounds
                IncludeCompounds = True
            Case SnapshotType.ObjectAddedOrRemoved
                RestoreObjects = True
            Case SnapshotType.ObjectData
                IncludeObjectData = True
            Case SnapshotType.ObjectLayout
                IncludeObjectLayout = True
            Case SnapshotType.PropertyPackages
                IncludePropertyPackages = True
            Case SnapshotType.ReactionSubsystem
                IncludeReactionSubsystem = True
            Case SnapshotType.SimulationSettings
                IncludeSimulationSettings = True
            Case SnapshotType.Spreadsheet
                IncludeSpreadsheet = True
            Case SnapshotType.WindowLayout
                IncludeWindowLayout = True
        End Select

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))

        If IncludeObjectData Or IncludeObjectLayout Then

            If IncludeObjectData Then

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

                SimulationObjects(obj.Name).SetFlowsheet(Me)
                xel.Add(New XElement("SimulationObject", {SimulationObjects(obj.Name).SaveData().ToArray()}))

            Else 'includeobjectlayout

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

                For Each go As GraphicObject In GraphicObjects.Values
                    If Not go.IsConnector And Not go.ObjectType = ObjectType.GO_FloatingTable Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
                Next

            End If

        Else

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Settings"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("Settings")
            xel.Add(Options.SaveData().ToArray())

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicProperties"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties")

            Dim extraprops = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            For Each item In extraprops
                Try
                    xel.Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                           New XElement("PropertyType", item.Value.GetType.ToString),
                                                                           New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                Catch ex As Exception
                End Try
            Next

            If RestoreObjects Then

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

                For Each go As GraphicObject In GraphicObjects.Values
                    If Not go.IsConnector And Not go.ObjectType = ObjectType.GO_FloatingTable Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
                Next

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

                For Each so As SharedClasses.UnitOperations.BaseClass In SimulationObjects.Values
                    so.SetFlowsheet(Me)
                    xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                Next

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("DynamicsManager"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager")

                xel.Add(DirectCast(DynamicsManager, ICustomXMLSerialization).SaveData().ToArray())

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ScriptItems"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems")

                For Each scr As Script In Scripts.Values
                    xel.Add(New XElement("ScriptItem", scr.SaveData().ToArray()))
                Next

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ChartItems"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems")

                For Each ch As SharedClasses.Charts.Chart In Charts.Values
                    xel.Add(New XElement("ChartItem", ch.SaveData().ToArray()))
                Next

            End If

            If IncludePropertyPackages Then

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

                For Each pp In Options.PropertyPackages
                    Dim createdms As Boolean = False
                    If pp.Value.CurrentMaterialStream Is Nothing Then
                        Dim ms As New Streams.MaterialStream("", "", Me, pp.Value)
                        AddCompoundsToMaterialStream(ms)
                        pp.Value.CurrentMaterialStream = ms
                        createdms = True
                    End If
                    xel.Add(New XElement("PropertyPackage", {New XElement("ID", pp.Key),
                                                             DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
                    If createdms Then pp.Value.CurrentMaterialStream = Nothing
                Next

            End If

            If IncludeCompounds Then

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Compounds"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds")

                For Each cp As ConstantProperties In Options.SelectedComponents.Values
                    xel.Add(New XElement("Compound", cp.SaveData().ToArray()))
                Next

            End If

            If IncludeReactionSubsystem Then

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("ReactionSets"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets")

                For Each pp As KeyValuePair(Of String, Interfaces.IReactionSet) In Options.ReactionSets
                    xel.Add(New XElement("ReactionSet", DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()))
                Next

                xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Reactions"))
                xel = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions")

                For Each pp As KeyValuePair(Of String, Interfaces.IReaction) In Options.Reactions
                    xel.Add(New XElement("Reaction", {DirectCast(pp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()}))
                Next

            End If

            xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("MessagesLog"))
            xel = xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog")

            Dim inner_elements As New List(Of XElement)
            For Each item In MessagesLog
                inner_elements.Add(New XElement("Message", item))
            Next
            xel.Add(inner_elements)

        End If

        Return xdoc

    End Function

    Public Sub RestoreSnapshot(xdata As XDocument, stype As SnapshotType) Implements IFlowsheet.RestoreSnapshot

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = xdata

        Dim data As List(Of XElement) = Nothing

        Select Case stype

            Case SnapshotType.ObjectData, SnapshotType.ObjectLayout

                If stype = SnapshotType.ObjectData Then

                    If xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects") IsNot Nothing Then

                        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

                        For Each xel In data
                            Dim id As String = xel.<Name>.Value
                            Dim obj As ISimulationObject = SimulationObjects(id)
                            obj.LoadData(xel.Elements.ToList)
                            obj.GraphicObject = GraphicObjects(id)
                            obj.SetFlowsheet(Me)
                            If TypeOf obj Is Streams.MaterialStream Then
                                For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                                    For Each c As ConstantProperties In Options.SelectedComponents.Values
                                        phase.Compounds(c.Name).ConstantProperties = c
                                    Next
                                Next
                            ElseIf TypeOf obj Is CapeOpenUO Then
                                If DirectCast(obj, CapeOpenUO)._seluo.Name.ToLower.Contains("chemsep") Then
                                    DirectCast(obj.GraphicObject, Shapes.CAPEOPENGraphic).ChemSep = True
                                    If obj.GraphicObject.Height = 40 And obj.GraphicObject.Width = 40 Then
                                        obj.GraphicObject.Width = 144
                                        obj.GraphicObject.Height = 180
                                    End If
                                End If
                            ElseIf TypeOf obj Is Input Then
                                GraphicObjectControlPanelModeEditors.SetInputDelegate(obj.GraphicObject, obj)
                            ElseIf TypeOf obj Is PIDController Then
                                GraphicObjectControlPanelModeEditors.SetPIDDelegate(obj.GraphicObject, obj)
                            End If
                        Next

                        ResetCalculationStatus()

                    End If

                Else

                    If xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects") IsNot Nothing Then

                        data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

                        'graphic objects

                        For Each xel As XElement In data
                            Dim id As String = xel.<Name>.Value
                            Dim obj As GraphicObject = GraphicObjects(id)
                            obj.LoadData(xel.Elements.ToList)
                            If SimulationObjects.ContainsKey(id) Then
                                obj.Owner = SimulationObjects(id)
                            End If
                            obj.Flowsheet = Me
                        Next

                    End If

                End If

            Case Else

                If xdoc.Element("DWSIM_Simulation_Data").Element("Settings") IsNot Nothing Then

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

                    Try
                        Options.LoadData(data)
                        Options.EnabledUndoRedo = False
                    Catch ex As Exception
                        excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
                    End Try

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects") IsNot Nothing Then

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

                    'graphic objects

                    GraphicObjects.Clear()

                    For Each xel As XElement In data
                        Try
                            xel.Element("Type").Value = xel.Element("Type").Value.Replace("Microsoft.MSDN.Samples.GraphicObjects", "DWSIM.DrawingTools.GraphicObjects")
                            xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Ajuste", "OT_Adjust")
                            xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Especificacao", "OT_Spec")
                            xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("OT_Reciclo", "OT_Recycle")
                            xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Texto", "GO_Text")
                            xel.Element("ObjectType").Value = xel.Element("ObjectType").Value.Replace("GO_Figura", "GO_Image")
                            Dim obj As GraphicObject = Nothing
                            Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                            If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                            If obj Is Nothing Then
                                If xel.Element("Type").Value.Contains("OxyPlotGraphic") Then
                                    obj = CType(Drawing.SkiaSharp.Extended.Shared.ReturnInstance(xel.Element("Type").Value.Replace("Shapes", "Charts")), GraphicObject)
                                Else
                                    obj = CType(GraphicObject.ReturnInstance(xel.Element("Type").Value), GraphicObject)
                                End If
                            End If
                            If Not obj Is Nothing Then
                                obj.LoadData(xel.Elements.ToList)
                                If TypeOf obj Is TableGraphic Then
                                    DirectCast(obj, TableGraphic).Flowsheet = Me
                                ElseIf TypeOf obj Is MasterTableGraphic Then
                                    DirectCast(obj, MasterTableGraphic).Flowsheet = Me
                                ElseIf TypeOf obj Is SpreadsheetTableGraphic Then
                                    DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = Me
                                ElseIf TypeOf obj Is Charts.OxyPlotGraphic Then
                                    DirectCast(obj, Charts.OxyPlotGraphic).Flowsheet = Me
                                ElseIf TypeOf obj Is Shapes.RigorousColumnGraphic Or TypeOf obj Is Shapes.AbsorptionColumnGraphic Or TypeOf obj Is Shapes.CAPEOPENGraphic Then
                                    obj.CreateConnectors(xel.Element("InputConnectors").Elements.Count, xel.Element("OutputConnectors").Elements.Count)
                                    obj.PositionConnectors()
                                ElseIf TypeOf obj Is Shapes.ExternalUnitOperationGraphic Then
                                    Dim euo = AvailableExternalUnitOperations.Values.Where(Function(x) x.Description = obj.Description).FirstOrDefault
                                    If euo IsNot Nothing Then
                                        obj.Owner = euo
                                        DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = obj
                                        obj.CreateConnectors(0, 0)
                                        obj.Owner = Nothing
                                        DirectCast(euo, Interfaces.ISimulationObject).GraphicObject = Nothing
                                    End If
                                Else
                                    If obj.Name = "" Then obj.Name = obj.Tag
                                    obj.CreateConnectors(0, 0)
                                End If
                                obj.Flowsheet = Me
                                GraphicObjects.Add(obj.Name, obj)
                            End If
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Flowsheet Graphic Objects", ex))
                        End Try
                    Next

                    For Each xel As XElement In data
                        Try
                            Dim id As String = xel.Element("Name").Value
                            If id <> "" Then
                                Dim obj As GraphicObject = (From go As GraphicObject In GraphicObjects.Values Where go.Name = id).SingleOrDefault
                                If obj Is Nothing Then obj = (From go As GraphicObject In GraphicObjects.Values Where go.Name = xel.Element("Name").Value).SingleOrDefault
                                If obj IsNot Nothing Then
                                    If xel.Element("InputConnectors") IsNot Nothing Then
                                        Dim i As Integer = 0
                                        For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                                            If xel2.@IsAttached = True Then
                                                obj.InputConnectors(i).ConnectorName = xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                                obj.InputConnectors(i).Type = [Enum].Parse(obj.InputConnectors(i).Type.GetType, xel2.@ConnType)
                                            End If
                                            i += 1
                                        Next
                                    End If
                                End If
                            End If
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
                        End Try
                    Next

                    For Each xel As XElement In data
                        Try
                            Dim id As String = xel.Element("Name").Value
                            If id <> "" Then
                                Dim obj As GraphicObject = (From go As GraphicObject In GraphicObjects.Values Where go.Name = id).SingleOrDefault
                                If obj IsNot Nothing Then
                                    If xel.Element("OutputConnectors") IsNot Nothing Then
                                        For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                                            If xel2.@IsAttached = True Then
                                                Dim objToID = xel2.@AttachedToObjID
                                                If objToID <> "" Then
                                                    Dim objTo As GraphicObject = (From go As GraphicObject In GraphicObjects.Values Where go.Name = objToID).SingleOrDefault
                                                    If objTo Is Nothing Then
                                                        objTo = (From go As GraphicObject In GraphicObjects.Values Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                                    End If
                                                    Dim fromidx As Integer = -1
                                                    Dim cp As ConnectionPoint = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = obj.Name).SingleOrDefault
                                                    If cp Is Nothing Then
                                                        cp = (From cp2 As ConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = xel2.@AttachedToObjID).SingleOrDefault
                                                    End If
                                                    If Not cp Is Nothing Then
                                                        fromidx = cp.ConnectorName.Split("|")(1)
                                                    End If
                                                    If Not obj Is Nothing And Not objTo Is Nothing Then
                                                        ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                                    End If
                                                End If
                                            End If
                                        Next
                                    End If
                                    If xel.Element("EnergyConnector") IsNot Nothing Then
                                        For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                                            If xel2.@IsAttached = True Then
                                                Dim objToID = xel2.@AttachedToObjID
                                                If objToID <> "" Then
                                                    Dim objTo As GraphicObject = (From go As GraphicObject In
                                                                                                GraphicObjects.Values Where go.Name = objToID).SingleOrDefault
                                                    If objTo Is Nothing Then
                                                        obj = (From go As GraphicObject In GraphicObjects.Values Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                                    End If
                                                    If Not obj Is Nothing And Not objTo Is Nothing Then
                                                        ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
                                                    End If
                                                End If
                                            End If
                                        Next
                                    End If
                                End If
                            End If
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Flowsheet Object Connection Information", ex))
                        End Try
                    Next

                    For Each obj In GraphicObjects.Values
                        If obj.ObjectType = ObjectType.GO_SpreadsheetTable Then
                            DirectCast(obj, SpreadsheetTableGraphic).Flowsheet = Me
                        End If
                    Next

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("Compounds") IsNot Nothing Then

                    'compounds

                    Options.SelectedComponents.Clear()

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

                    For Each xel As XElement In data
                        Dim obj As New ConstantProperties
                        obj.Name = xel.Element("Name").Value
                        If Not AvailableCompounds.ContainsKey(obj.Name) Then AvailableCompounds.Add(obj.Name, obj)
                        Options.SelectedComponents.Add(obj.Name, obj)
                    Next

                    Parallel.ForEach(data, Sub(xel)
                                               Try
                                                   Options.SelectedComponents(xel.Element("Name").Value).LoadData(xel.Elements.ToList)
                                               Catch ex As Exception
                                                   excs.Add(New Exception("Error Loading Compound Information", ex))
                                               End Try
                                           End Sub)

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties") IsNot Nothing Then

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicProperties").Elements.ToList

                    Try

                        ExtraProperties = New ExpandoObject

                        If Not data Is Nothing Then
                            For Each xel As XElement In data
                                Try
                                    Dim propname = xel.Element("Name").Value
                                    Dim proptype = xel.Element("PropertyType").Value
                                    Dim assembly1 As Assembly = Nothing
                                    For Each assembly In My.Application.Info.LoadedAssemblies
                                        If proptype.Contains(assembly.GetName().Name) Then
                                            assembly1 = assembly
                                            Exit For
                                        End If
                                    Next
                                    If assembly1 IsNot Nothing Then
                                        Dim ptype As Type = assembly1.GetType(proptype)
                                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                                        DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                                    End If
                                Catch ex As Exception
                                End Try
                            Next
                        End If

                    Catch ex As Exception

                        excs.Add(New Exception("Error Loading Dynamic Properties", ex))

                    End Try

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages") IsNot Nothing Then

                    Options.PropertyPackages.Clear()

                    Dim pp As New PropertyPackages.RaoultPropertyPackage

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

                    For Each xel As XElement In data
                        Try
                            xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                            Dim obj As PropertyPackages.PropertyPackage = Nothing
                            If xel.Element("Type").Value.Contains("ThermoC") Then
                                Dim thermockey As String = "ThermoC Bridge"
                                If AvailablePropertyPackages.ContainsKey(thermockey) Then
                                    obj = AvailablePropertyPackages(thermockey).ReturnInstance(xel.Element("Type").Value)
                                End If
                            Else
                                Dim ppkey As String = xel.Element("ComponentName").Value
                                If ppkey = "" Then
                                    obj = CType(New PropertyPackages.RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), PropertyPackages.PropertyPackage)
                                Else
                                    Dim ptype = xel.Element("Type").Value
                                    If ppkey.Contains("1978") And ptype.Contains("PengRobinsonPropertyPackage") Then
                                        ptype = ptype.Replace("PengRobinson", "PengRobinson1978")
                                    End If
                                    If AvailablePropertyPackages.ContainsKey(ppkey) Then
                                        obj = AvailablePropertyPackages(ppkey).ReturnInstance(ptype)
                                    End If
                                End If
                            End If
                            DirectCast(obj, Interfaces.ICustomXMLSerialization).LoadData(xel.Elements.ToList)
                            Dim newID As String = Guid.NewGuid.ToString
                            If Options.PropertyPackages.ContainsKey(obj.UniqueID) Then obj.UniqueID = newID
                            obj.Flowsheet = Me
                            Options.PropertyPackages.Add(obj.UniqueID, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Property Package Information", ex))
                        End Try
                    Next


                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects") IsNot Nothing Then

                    SimulationObjects.Clear()

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

                    Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

                    Dim fsuocount = (From go As GraphicObject In SimulationObjects.Values Where go.ObjectType = ObjectType.FlowsheetUO).Count

                    For Each xel In data
                        Try
                            Dim id As String = xel.<Name>.Value
                            Dim obj As SharedClasses.UnitOperations.BaseClass = Nothing
                            If xel.Element("Type").Value.Contains("Streams.MaterialStream") Then
                                obj = New Streams.MaterialStream()
                            Else
                                Dim uokey As String = xel.Element("ComponentDescription").Value
                                If AvailableExternalUnitOperations.ContainsKey(uokey) Then
                                    obj = AvailableExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                                Else
                                    obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
                                End If
                            End If
                            Dim gobj As GraphicObject = (From go As GraphicObject In
                                                GraphicObjects.Values Where go.Name = id).SingleOrDefault
                            obj.GraphicObject = gobj
                            gobj.Owner = obj
                            obj.SetFlowsheet(Me)
                            If Not gobj Is Nothing Then
                                obj.LoadData(xel.Elements.ToList)
                                If TypeOf obj Is Streams.MaterialStream Then
                                    For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                                        For Each c As ConstantProperties In Options.SelectedComponents.Values
                                            phase.Compounds(c.Name).ConstantProperties = c
                                        Next
                                    Next
                                ElseIf TypeOf obj Is CapeOpenUO Then
                                    If DirectCast(obj, CapeOpenUO)._seluo.Name.ToLower.Contains("chemsep") Then
                                        DirectCast(gobj, Shapes.CAPEOPENGraphic).ChemSep = True
                                        If gobj.Height = 40 And gobj.Width = 40 Then
                                            gobj.Width = 144
                                            gobj.Height = 180
                                        End If
                                    End If
                                ElseIf TypeOf obj Is Input Then
                                    GraphicObjectControlPanelModeEditors.SetInputDelegate(gobj, obj)
                                ElseIf TypeOf obj Is PIDController Then
                                    GraphicObjectControlPanelModeEditors.SetPIDDelegate(gobj, obj)
                                End If
                            End If
                            objlist.Add(obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                        End Try
                    Next

                    'simulation objects

                    For Each obj In objlist
                        Try
                            Dim id = obj.Name
                            SimulationObjects.Add(id, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Unit Operation Information", ex))
                        End Try
                    Next

                    For Each so As SharedClasses.UnitOperations.BaseClass In SimulationObjects.Values
                        Try
                            If TryCast(so, Adjust) IsNot Nothing Then
                                Dim so2 As Adjust = so
                                If SimulationObjects.ContainsKey(so2.ManipulatedObjectData.ID) Then
                                    so2.ManipulatedObject = SimulationObjects(so2.ManipulatedObjectData.ID)
                                    DirectCast(so2.GraphicObject, Shapes.AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
                                End If
                                If SimulationObjects.ContainsKey(so2.ControlledObjectData.ID) Then
                                    so2.ControlledObject = SimulationObjects(so2.ControlledObjectData.ID)
                                    DirectCast(so2.GraphicObject, Shapes.AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
                                End If
                                If SimulationObjects.ContainsKey(so2.ReferencedObjectData.ID) Then
                                    so2.ReferenceObject = SimulationObjects(so2.ReferencedObjectData.ID)
                                    DirectCast(so2.GraphicObject, Shapes.AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
                                End If
                            End If
                            If TryCast(so, Spec) IsNot Nothing Then
                                Dim so2 As Spec = so
                                If SimulationObjects.ContainsKey(so2.TargetObjectData.ID) Then
                                    so2.TargetObject = SimulationObjects(so2.TargetObjectData.ID)
                                    DirectCast(so2.GraphicObject, Shapes.SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
                                End If
                                If SimulationObjects.ContainsKey(so2.SourceObjectData.ID) Then
                                    so2.SourceObject = SimulationObjects(so2.SourceObjectData.ID)
                                    DirectCast(so2.GraphicObject, Shapes.SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
                                End If
                            End If
                            If TryCast(so, CapeOpenUO) IsNot Nothing Then
                                DirectCast(so, CapeOpenUO).UpdateConnectors2()
                                DirectCast(so, CapeOpenUO).UpdatePortsFromConnectors()
                            End If
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
                        End Try
                    Next

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets") IsNot Nothing Then

                    'reactions

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("ReactionSets").Elements.ToList

                    Options.ReactionSets.Clear()

                    For Each xel As XElement In data
                        Try
                            Dim obj As New ReactionSet()
                            obj.LoadData(xel.Elements.ToList)
                            Options.ReactionSets.Add(obj.ID, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Reaction Set Information", ex))
                        End Try
                    Next

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("Reactions").Elements.ToList

                    Options.Reactions.Clear()

                    For Each xel As XElement In data
                        Try
                            Dim obj As New Reaction()
                            obj.LoadData(xel.Elements.ToList)
                            Options.Reactions.Add(obj.ID, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Reaction Information", ex))
                        End Try
                    Next

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager") IsNot Nothing Then

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("DynamicsManager").Elements.ToList

                    Try
                        DirectCast(DynamicsManager, ICustomXMLSerialization).LoadData(data)
                    Catch ex As Exception
                        excs.Add(New Exception("Error Loading Dynamics Manager Information", ex))
                    End Try

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems") IsNot Nothing Then

                    Scripts = New Dictionary(Of String, Interfaces.IScript)

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("ScriptItems").Elements.ToList

                    Dim i As Integer = 0
                    For Each xel As XElement In data
                        Try
                            Dim obj As New Script()
                            obj.LoadData(xel.Elements.ToList)
                            Scripts.Add(obj.ID, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Script Item Information", ex))
                        End Try
                        i += 1
                    Next

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems") IsNot Nothing Then

                    Charts = New Dictionary(Of String, Interfaces.IChart)

                    data = xdoc.Element("DWSIM_Simulation_Data").Element("ChartItems").Elements.ToList

                    Dim i As Integer = 0
                    For Each xel As XElement In data
                        Try
                            Dim obj As New SharedClasses.Charts.Chart()
                            obj.LoadData(xel.Elements.ToList)
                            Charts.Add(obj.ID, obj)
                        Catch ex As Exception
                            excs.Add(New Exception("Error Loading Chart Item Information", ex))
                        End Try
                        i += 1
                    Next

                End If

                If xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog") IsNot Nothing Then

                    MessagesLog.Clear()

                    Try
                        data = xdoc.Element("DWSIM_Simulation_Data").Element("MessagesLog").Elements.ToList
                        For Each xel As XElement In data
                            MessagesLog.Add(xel.Value)
                        Next
                    Catch ex As Exception
                    End Try
                End If

        End Select

        If excs.Count > 0 Then

        End If

    End Sub

    Private Function GetSnapshotObjectID(xdoc As XDocument) As String

        Return xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.FirstOrDefault().<Name>.Value

    End Function

#End Region

#Region "    Undo/Redo"

    Sub AddUndoRedoAction(act As Interfaces.IUndoRedoAction) Implements IFlowsheet.AddUndoRedoAction

        If Options.EnabledUndoRedo AndAlso Me.MasterFlowsheet Is Nothing Then

            UndoStack.Push(New Tuple(Of SnapshotType, XDocument)(SnapshotType.All, GetSnapshot(SnapshotType.All)))

            RedoStack.Clear()

        End If

    End Sub

    Public Sub ProcessUndo()

        If Options.EnabledUndoRedo AndAlso UndoStack.Count > 0 Then
            Dim xdata = UndoStack.Pop()
            Options.EnabledUndoRedo = False
            Dim current As XDocument = Nothing
            If xdata.Item1 = SnapshotType.ObjectData Then
                Dim objID = GetSnapshotObjectID(xdata.Item2)
                current = GetSnapshot(SnapshotType.ObjectData, SimulationObjects(objID))
            Else
                current = GetSnapshot(xdata.Item1)
            End If
            If xdata.Item1 = SnapshotType.ObjectAddedOrRemoved Or xdata.Item1 = SnapshotType.All Then CloseOpenEditForms()
            RestoreSnapshot(xdata.Item2, xdata.Item1)
            UpdateInterface()
            UpdateOpenEditForms()
            Options.EnabledUndoRedo = True
            RedoStack.Push(New Tuple(Of SnapshotType, XDocument)(xdata.Item1, current))
        End If

    End Sub

    Public Sub ProcessRedo()

        If Options.EnabledUndoRedo AndAlso RedoStack.Count > 0 Then
            Dim xdata = RedoStack.Pop()
            Options.EnabledUndoRedo = False
            Dim current As XDocument = Nothing
            If xdata.Item1 = SnapshotType.ObjectData Then
                Dim objID = GetSnapshotObjectID(xdata.Item2)
                current = GetSnapshot(SnapshotType.ObjectData, SimulationObjects(objID))
            Else
                current = GetSnapshot(xdata.Item1)
            End If
            If xdata.Item1 = SnapshotType.ObjectAddedOrRemoved Or xdata.Item1 = SnapshotType.All Then CloseOpenEditForms()
            RestoreSnapshot(xdata.Item2, xdata.Item1)
            UpdateInterface()
            UpdateOpenEditForms()
            Options.EnabledUndoRedo = True
            UndoStack.Push(New Tuple(Of SnapshotType, XDocument)(xdata.Item1, current))
        End If

    End Sub


#End Region

End Class

