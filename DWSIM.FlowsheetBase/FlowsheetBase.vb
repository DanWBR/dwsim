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

<System.Runtime.InteropServices.ComVisible(True)> Public MustInherit Class FlowsheetBase

    Implements IFlowsheet, IFlowsheetCalculationQueue

    Public Property ExtraProperties As New ExpandoObject Implements IFlowsheet.ExtraProperties

    Public WithEvents Options As New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables

    Private FlowsheetSurface As New GraphicsSurface

    Public SensAnalysisCollection As New List(Of Optimization.SensitivityAnalysisCase)

    Public OptimizationCollection As New List(Of Optimization.OptimizationCase)

    Public Property AvailablePropertyPackages As New Dictionary(Of String, IPropertyPackage) Implements IFlowsheet.AvailablePropertyPackages

    Public Property AvailableFlashAlgorithms As New Dictionary(Of String, IFlashAlgorithm) Implements IFlowsheet.AvailableFlashAlgorithms

    Public Property AvailableSystemsOfUnits As New List(Of IUnitsOfMeasure) Implements IFlowsheet.AvailableSystemsOfUnits

    Public Property ExternalUnitOperations As New Dictionary(Of String, IExternalUnitOperation)

    Private loaded As Boolean = False

    Private rm, prm As Resources.ResourceManager

    Public LoadSpreadsheetData, SaveSpreadsheetData As Action(Of XDocument)

    Public GetSpreadsheetObject As Func(Of Object)

    Public RetrieveSpreadsheetData As Func(Of String, List(Of String()))

    Public Property ScriptKeywordsF As String = ""

    Public Property ScriptKeywordsU As String = ""

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

    Public Sub AddPropertyPackage(obj As IPropertyPackage) Implements IFlowsheet.AddPropertyPackage
        Me.Options.PropertyPackages.Add(obj.UniqueID, obj)
    End Sub

    Public Sub AddSimulationObject(obj As ISimulationObject) Implements IFlowsheet.AddSimulationObject
        SimulationObjects.Add(obj.Name, obj)
    End Sub

    Public Sub CheckStatus() Implements IFlowsheet.CheckStatus
        FlowsheetSolver.FlowsheetSolver.CheckCalculatorStatus()
    End Sub

    Public Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer) Implements IFlowsheet.ConnectObjects
        FlowsheetSurface.ConnectObject(CType(gobjfrom, GraphicObject), CType(gobjto, GraphicObject), fromidx, toidx)
    End Sub

    Public Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject) Implements IFlowsheet.DisconnectObjects
        FlowsheetSurface.DisconnectObject(CType(gobjfrom, GraphicObject), CType(gobjto, GraphicObject), False)
    End Sub

    Public Sub DeleteSelectedObject(ByVal sender As System.Object, ByVal e As System.EventArgs, gobj As IGraphicObject, Optional ByVal confirmation As Boolean = True, Optional ByVal triggercalc As Boolean = False) Implements IFlowsheet.DeleteSelectedObject

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

    Public Function GetFlowsheetSimulationObject(tag As String) As ISimulationObject Implements IFlowsheet.GetFlowsheetSimulationObject

        For Each obj As ISimulationObject In SimulationObjects.Values
            If obj.GraphicObject.Tag = tag Then
                Return obj
            End If
        Next
        Return Nothing

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

    Public Function GetTranslatedString(text As String) As String Implements IFlowsheet.GetTranslatedString

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

    Public Sub RequestCalculation(Optional sender As ISimulationObject = Nothing, Optional ChangeCalculationOrder As Boolean = False) Implements IFlowsheet.RequestCalculation

        If Not sender Is Nothing Then
            FlowsheetSolver.FlowsheetSolver.CalculateObject(Me, sender.Name)
        Else
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode, ChangeCalcOrder:=ChangeCalculationOrder)
        End If

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

    Public Property CalculationQueue As New Queue(Of ICalculationArgs) Implements IFlowsheetCalculationQueue.CalculationQueue

    Public Function AddObject(ByVal typename As String, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "", Optional ByVal id As String = "") As ISimulationObject

        Select Case typename

            Case "Controller Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Adjust, x, y, tag))

            Case "Specification Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Spec, x, y, tag))

            Case "Recycle Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_Recycle, x, y, tag))

            Case "Energy Recycle Block"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OT_EnergyRecycle, x, y, tag))

            Case "Stream Mixer"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.NodeIn, x, y, tag))

            Case "Stream Splitter"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.NodeOut, x, y, tag))

            Case "Centrifugal Pump"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Pump, x, y, tag))

            Case "Tank"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Tank, x, y, tag))

            Case "Gas-Liquid Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Vessel, x, y, tag))

            Case "Material Stream"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.MaterialStream, x, y, tag))

            Case "Energy Stream"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.EnergyStream, x, y, tag))

            Case "Adiabatic Compressor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Compressor, x, y, tag))

            Case "Adiabatic Expander"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Expander, x, y, tag))

            Case "Heater"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Heater, x, y, tag))

            Case "Cooler"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Cooler, x, y, tag))

            Case "Piping Segment"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Pipe, x, y, tag))

            Case "Valve"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Valve, x, y, tag))

            Case "Conversion Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Conversion, x, y, tag))

            Case "Equilibrium Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Equilibrium, x, y, tag))

            Case "Gibbs Reactor"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_Gibbs, x, y, tag))

            Case "Plug-Flow Reactor (PFR)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_PFR, x, y, tag))

            Case "Continuous Stirred Tank Reactor (CSTR)"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.RCT_CSTR, x, y, tag))

            Case "Heat Exchanger"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.HeatExchanger, x, y, tag))

            Case "Shortcut Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ShortcutColumn, x, y, tag))

            Case "Distillation Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.DistillationColumn, x, y, tag))

            Case "Absorption Column"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.AbsorptionColumn, x, y, tag))

            Case "Compound Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ComponentSeparator, x, y, tag))

            Case "Solids Separator"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.SolidSeparator, x, y, tag))

            Case "Filter"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.Filter, x, y, tag))

            Case "Orifice Plate"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.OrificePlate, x, y, tag))

            Case "Python Script"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.CustomUO, x, y, tag))

            Case "Spreadsheet"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.ExcelUO, x, y, tag))

            Case "Flowsheet"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.FlowsheetUO, x, y, tag))

            Case "CAPE-OPEN Unit Operation"

                Return Me.SimulationObjects(AddObjectToSurface(ObjectType.CapeOpenUO, x, y, tag))

            Case Else

                Return Nothing

        End Select

    End Function

    Public Function AddObjectToSurface(ByVal type As ObjectType, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "", Optional ByVal id As String = "", Optional ByVal uoobj As IExternalUnitOperation = Nothing) As String

        Dim gObj As IGraphicObject = Nothing
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Select Case type

            Case ObjectType.External

                Dim myNode As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                myNode.Tag = uoobj.Prefix & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(uoobj, ISimulationObject).Name = gObj.Name
                GraphicObjects.Add(gObj.Name, myNode)
                DirectCast(uoobj, ISimulationObject).GraphicObject = myNode
                myNode.CreateConnectors(0, 0)
                SimulationObjects.Add(myNode.Name, uoobj)

            Case ObjectType.OT_Adjust

                Dim myNode As New AdjustGraphic(mpx, mpy, 40, 40)
                myNode.Tag = "ADJ-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Adjust = New Adjust(myNode.Name, "Ajuste")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_Spec

                Dim myNode As New SpecGraphic(mpx, mpy, 40, 40)
                myNode.Tag = "SPEC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "SPEC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Spec = New Spec(myNode.Name, "Especificao")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_Recycle

                Dim myNode As New RecycleGraphic(mpx, mpy, 40, 40)
                myNode.Tag = "REC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "REC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As Recycle = New Recycle(myNode.Name, "")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.OT_EnergyRecycle

                Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 40, 40)
                myNode.Tag = "EREC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "EREC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myADJ As EnergyRecycle = New EnergyRecycle(myNode.Name, "EnergyRecycle")
                myADJ.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myADJ)

            Case ObjectType.NodeIn

                Dim myNode As New MixerGraphic(mpx, mpy, 40, 40)
                myNode.Tag = "MIX-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "MIX-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNode)
                Dim myCOMIX As Mixer = New Mixer(myNode.Name, "")
                myCOMIX.GraphicObject = myNode
                SimulationObjects.Add(myNode.Name, myCOMIX)

            Case ObjectType.NodeOut

                Dim myNodeo As New SplitterGraphic(mpx, mpy, 40, 40)
                myNodeo.Tag = "SPLT-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myNodeo.Tag = tag
                gObj = myNodeo
                gObj.Name = "DIV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myNodeo)
                'OBJETO DWSIM
                Dim myCOSP As Splitter = New Splitter(myNodeo.Name, "")
                myCOSP.GraphicObject = myNodeo
                SimulationObjects.Add(myNodeo.Name, myCOSP)

            Case ObjectType.Pump

                Dim myPump As New PumpGraphic(mpx, mpy, 40, 40)
                myPump.Tag = "PUMP-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myPump.Tag = tag
                gObj = myPump
                gObj.Name = "PUMP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myPump)
                'OBJETO DWSIM
                Dim myCOSP As Pump = New Pump(myPump.Name, "")
                myCOSP.GraphicObject = myPump
                SimulationObjects.Add(myPump.Name, myCOSP)

            Case ObjectType.Tank

                Dim myTank As New TankGraphic(mpx, mpy, 50, 50)
                myTank.Tag = "TANK-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myTank.Tag = tag
                gObj = myTank
                gObj.Name = "TANK-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myTank)
                'OBJETO DWSIM
                Dim myCOTK As Tank = New Tank(myTank.Name, "Tanque")
                myCOTK.GraphicObject = myTank
                SimulationObjects.Add(myTank.Name, myCOTK)

            Case ObjectType.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 70)
                myVessel.Tag = "V-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                gObj.Name = "V-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myVessel)
                'OBJETO DWSIM
                Dim myCOVESSEL As Vessel = New Vessel(myVessel.Name, "")
                myCOVESSEL.GraphicObject = myVessel
                SimulationObjects.Add(myVessel.Name, myCOVESSEL)

            Case ObjectType.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 30, 30)
                myMStr.Tag = "MS-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "MS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As MaterialStream = New MaterialStream(myMStr.Name, "CorrentedeMatria", Me, Nothing)
                myCOMS.GraphicObject = myMStr
                AddCompoundsToMaterialStream(myCOMS)
                SimulationObjects.Add(myCOMS.Name, myCOMS)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 30, 30)
                myMStr.Tag = "ES-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "ES-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOES As EnergyStream = New EnergyStream(myMStr.Name, "")
                myCOES.GraphicObject = myMStr
                SimulationObjects.Add(myCOES.Name, myCOES)

            Case ObjectType.CompressorExpander

                Dim myComp As New CompressorExpanderGraphic(mpx, mpy, 50, 50)
                myComp.Tag = "CX-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "CX-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Compressor = New Compressor(myComp.Name, "")
                myCOCP.GraphicObject = myComp
                SimulationObjects.Add(myComp.Name, myCOCP)

            Case ObjectType.Compressor

                Dim myComp As New CompressorGraphic(mpx, mpy, 50, 50)
                myComp.Tag = "C-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "C-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Compressor = New Compressor(myComp.Name, "")
                myCOCP.GraphicObject = myComp
                SimulationObjects.Add(myComp.Name, myCOCP)

            Case ObjectType.Expander

                Dim myComp As New TurbineGraphic(mpx, mpy, 50, 50)
                myComp.Tag = "X-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "X-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Expander = New Expander(myComp.Name, "")
                myCOCP.GraphicObject = myComp
                SimulationObjects.Add(myComp.Name, myCOCP)

            Case ObjectType.HeaterCooler

                Dim myCool As New HeaterCoolerGraphic(mpx, mpy, 40, 40)
                myCool.Tag = "HC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                gObj.Name = "HC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCool)
                'OBJETO DWSIM
                Dim myCOCL As Heater = New Heater(myCool.Name, "")
                myCOCL.GraphicObject = myCool
                SimulationObjects.Add(myCool.Name, myCOCL)

            Case ObjectType.Heater

                Dim myCool As New HeaterGraphic(mpx, mpy, 40, 40)
                myCool.Tag = "HT-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                gObj.Name = "HT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCool)
                'OBJETO DWSIM
                Dim myCOCL As Heater = New Heater(myCool.Name, "")
                myCOCL.GraphicObject = myCool
                SimulationObjects.Add(myCool.Name, myCOCL)

            Case ObjectType.Cooler

                Dim myCool As New CoolerGraphic(mpx, mpy, 40, 40)
                myCool.Tag = "CL-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                gObj.Name = "CL-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCool)
                'OBJETO DWSIM
                Dim myCOCL As Cooler = New Cooler(myCool.Name, "")
                myCOCL.GraphicObject = myCool
                SimulationObjects.Add(myCool.Name, myCOCL)

            Case ObjectType.Pipe

                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 80, 20)
                myPipe.Tag = "PIPE-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myPipe.Tag = tag
                gObj = myPipe
                gObj.Name = "PIPE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myPipe)
                'OBJETO DWSIM
                Dim myCOPIPE As Pipe = New Pipe(myPipe.Name, "Tubulao")
                myCOPIPE.GraphicObject = myPipe
                SimulationObjects.Add(myPipe.Name, myCOPIPE)

            Case ObjectType.Valve

                Dim myValve As New ValveGraphic(mpx, mpy, 40, 40)
                myValve.Tag = "VALVE-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myValve.Tag = tag
                gObj = myValve
                gObj.Name = "VALVE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myValve)
                'OBJETO DWSIM
                Dim myCOVALVE As Valve = New Valve(myValve.Name, "")
                myCOVALVE.GraphicObject = myValve
                SimulationObjects.Add(myValve.Name, myCOVALVE)

            Case ObjectType.RCT_Conversion

                Dim myRconv As New ConversionReactorGraphic(mpx, mpy, 50, 50)
                myRconv.Tag = "CR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myRconv.Tag = tag
                gObj = myRconv
                gObj.Name = "CR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRconv)
                'OBJETO DWSIM
                Dim myCORCONV As Reactor_Conversion = New Reactor_Conversion(myRconv.Name, "ReatorConversao")
                myCORCONV.GraphicObject = myRconv
                SimulationObjects.Add(myRconv.Name, myCORCONV)

            Case ObjectType.RCT_Equilibrium

                Dim myReq As New EquilibriumReactorGraphic(mpx, mpy, 50, 50)
                myReq.Tag = "ER-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myReq.Tag = tag
                gObj = myReq
                gObj.Name = "ER-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myReq)
                'OBJETO DWSIM
                Dim myCOREQ As Reactor_Equilibrium = New Reactor_Equilibrium(myReq.Name, "ReatorEquilibrio")
                myCOREQ.GraphicObject = myReq
                SimulationObjects.Add(myReq.Name, myCOREQ)

            Case ObjectType.RCT_Gibbs

                Dim myRgibbs As New GibbsReactorGraphic(mpx, mpy, 50, 50)
                myRgibbs.Tag = "GR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myRgibbs.Tag = tag
                gObj = myRgibbs
                gObj.Name = "GR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRgibbs)
                'OBJETO DWSIM
                Dim myCORGIBBS As Reactor_Gibbs = New Reactor_Gibbs(myRgibbs.Name, "ReatorGibbs")
                myCORGIBBS.GraphicObject = myRgibbs
                SimulationObjects.Add(myRgibbs.Name, myCORGIBBS)

            Case ObjectType.RCT_CSTR

                Dim myRcstr As New CSTRGraphic(mpx, mpy, 50, 50)
                myRcstr.Tag = "CSTR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myRcstr.Tag = tag
                gObj = myRcstr
                gObj.Name = "CSTR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRcstr)
                'OBJETO DWSIM
                Dim myCORCSTR As Reactor_CSTR = New Reactor_CSTR(myRcstr.Name, "ReatorCSTR")
                myCORCSTR.GraphicObject = myRcstr
                SimulationObjects.Add(myRcstr.Name, myCORCSTR)

            Case ObjectType.RCT_PFR

                Dim myRpfr As New PFRGraphic(mpx, mpy, 70, 20)
                myRpfr.Tag = "PFR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myRpfr.Tag = tag
                gObj = myRpfr
                gObj.Name = "PFR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myRpfr)
                'OBJETO DWSIM
                Dim myCOPFR As Reactor_PFR = New Reactor_PFR(myRpfr.Name, "ReatorPFR")
                myCOPFR.GraphicObject = myRpfr
                SimulationObjects.Add(myRpfr.Name, myCOPFR)

            Case ObjectType.HeatExchanger

                Dim myHeatExchanger As New HeatExchangerGraphic(mpx, mpy, 50, 50)
                myHeatExchanger.Tag = "HX-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myHeatExchanger.Tag = tag
                gObj = myHeatExchanger
                gObj.Name = "HX-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myHeatExchanger)
                'OBJETO DWSIM
                Dim myCOHE As HeatExchanger = New HeatExchanger(myHeatExchanger.Name, "")
                myCOHE.GraphicObject = myHeatExchanger
                SimulationObjects.Add(myHeatExchanger.Name, myCOHE)

            Case ObjectType.ShortcutColumn

                Dim mySC As New ShortcutColumnGraphic(mpx, mpy, 144, 180)
                mySC.Tag = "SC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "SC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As ShortcutColumn = New ShortcutColumn(mySC.Name, "")
                myCOSC.GraphicObject = mySC
                SimulationObjects.Add(mySC.Name, myCOSC)

            Case ObjectType.DistillationColumn

                Dim myDC As New RigorousColumnGraphic(mpx, mpy, 144, 180)
                myDC.Tag = "DC-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myDC.Tag = tag
                gObj = myDC
                gObj.Name = "DC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myDC)
                'OBJETO DWSIM
                Dim myCOSC As DistillationColumn = New DistillationColumn(myDC.Name, "DistillationColumn", Me)
                myCOSC.GraphicObject = myDC
                SimulationObjects.Add(myDC.Name, myCOSC)

            Case ObjectType.AbsorptionColumn

                Dim myAC As New AbsorptionColumnGraphic(mpx, mpy, 144, 180)
                myAC.Tag = "ABS-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myAC.Tag = tag
                gObj = myAC
                gObj.Name = "ABS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myAC)
                'OBJETO DWSIM
                Dim myCOSC As AbsorptionColumn = New AbsorptionColumn(myAC.Name, "AbsorptionColumn", Me)
                myCOSC.GraphicObject = myAC
                SimulationObjects.Add(myAC.Name, myCOSC)

            Case ObjectType.ComponentSeparator

                Dim myCSep As New ComponentSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.Tag = "CS-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "CS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As ComponentSeparator = New ComponentSeparator(myCSep.Name, "ComponentSeparator")
                myCOCSEP.GraphicObject = myCSep
                SimulationObjects.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.SolidSeparator

                Dim myCSep As New SolidsSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.Tag = "SS-" & SimulationObjects.Count.ToString("00#")
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
                myCSep.Tag = "FT-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "FT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As Filter = New Filter(myCSep.Name, "Filter")
                myCOCSEP.GraphicObject = myCSep
                SimulationObjects.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.OrificePlate

                Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25)
                myOPL.Tag = "OP-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myOPL.Tag = tag
                gObj = myOPL
                gObj.Name = "OP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myOPL)
                'OBJETO DWSIM
                Dim myCOOPL As OrificePlate = New OrificePlate(myOPL.Name, "OrificePlate")
                myCOOPL.GraphicObject = myOPL
                SimulationObjects.Add(myOPL.Name, myCOOPL)

            Case ObjectType.CustomUO

                Dim myCUO As New ScriptGraphic(mpx, mpy, 25, 25)
                myCUO.Tag = "UO-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                gObj.Name = "UO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As CustomUO = New CustomUO(myCUO.Name, "CustomUnitOp")
                myCOCUO.GraphicObject = myCUO
                SimulationObjects.Add(myCUO.Name, myCOCUO)

            Case ObjectType.ExcelUO

                Dim myEUO As New SpreadsheetGraphic(mpx, mpy, 25, 25)
                myEUO.Tag = "EXL-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                gObj.Name = "EXL-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As ExcelUO = New ExcelUO(myEUO.Name, "ExcelUnitOp")
                myCOEUO.GraphicObject = myEUO
                SimulationObjects.Add(myEUO.Name, myCOEUO)

            Case ObjectType.FlowsheetUO

                Dim myEUO As New FlowsheetGraphic(mpx, mpy, 25, 25)
                myEUO.Tag = "FS-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                gObj.Name = "FS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As Flowsheet = New Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                myCOEUO.GraphicObject = myEUO
                SimulationObjects.Add(myEUO.Name, myCOEUO)

            Case ObjectType.CapeOpenUO

                Dim myCUO As New CAPEOPENGraphic(mpx, mpy, 40, 40)
                myCUO.Tag = "COUO-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                gObj.Name = "COUO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj)
                myCOCUO.GraphicObject = myCUO
                SimulationObjects.Add(myCUO.Name, myCOCUO)

        End Select

        If Not gObj Is Nothing Then
            gObj.Owner = SimulationObjects(gObj.Name)
            SimulationObjects(gObj.Name).SetFlowsheet(Me)
            FlowsheetSurface.AddObject(gObj)
        End If

        Return gObj.Name

    End Function

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
            Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                                   SharedClasses.Utility.UpdateElementForNewUI(xel1)
                                               End Sub)
        End If

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try

            Options.LoadData(data)

            FlowsheetSurface.DrawFloatingTable = Options.DisplayFloatingPropertyTables
            FlowsheetSurface.DrawPropertyList = Options.DisplayCornerPropertyList

            Options.FlashAlgorithms.Clear()

            If Not AvailableSystemsOfUnits.Contains(Options.SelectedUnitSystem1) Then
                AvailableSystemsOfUnits.Add(Options.SelectedUnitSystem1)
            End If

            Dim el As XElement = (From xel As XElement In data Select xel Where xel.Name = "FlashAlgorithms").SingleOrDefault

            If Not el Is Nothing Then
                For Each xel As XElement In el.Elements
                    Dim obj As PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm = CType(New PropertyPackages.RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.FlashAlgorithm)
                    obj.LoadData(xel.Elements.ToList)
                    Options.FlashAlgorithms.Add(obj)
                Next
            Else
                Options.FlashAlgorithms.Add(New Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops() With {.Tag = .Name})
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
                            Dim ptype As Type = Type.GetType(proptype)
                            Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                            DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
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
            Try
                Dim obj As New ConstantProperties
                obj.LoadData(xel.Elements.ToList)
                If Not AvailableCompounds.ContainsKey(obj.Name) Then AvailableCompounds.Add(obj.Name, obj)
                Options.SelectedComponents.Add(obj.Name, obj)
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Compound Information", ex))
            End Try
        Next

        data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

        For Each xel As XElement In data
            Try
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("PortableDTL.DTL.SimulationObjects", "DWSIM.Thermodynamics")
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = Nothing
                If xel.Element("Type").Value.Contains("AdvancedEOS") Then
                    Dim adveoskey As String = "PC-SAFT (with Association Support)"
                    If AvailablePropertyPackages.ContainsKey(adveoskey) Then
                        obj = AvailablePropertyPackages(adveoskey).ReturnInstance(xel.Element("Type").Value)
                    Else
                        Throw New Exception("Advanced EOS Property Package library not found. Please download and install it in order to run this simulation.")
                    End If
                ElseIf xel.Element("Type").Value.Contains("ThermoC") Then
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
                        If AvailablePropertyPackages.ContainsKey(ppkey) Then
                            obj = AvailablePropertyPackages(ppkey).ReturnInstance(xel.Element("Type").Value)
                        Else
                            Throw New Exception("The " & ppkey & " library was not found. Please download and install it in order to run this simulation.")
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
                    obj = CType(New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value), ISimulationObject)
                Else
                    Dim uokey As String = xel.Element("ComponentDescription").Value
                    If ExternalUnitOperations.ContainsKey(uokey) Then
                        RunCodeOnUIThread(Sub()
                                              obj = ExternalUnitOperations(uokey).ReturnInstance(xel.Element("Type").Value)
                                          End Sub)
                    Else
                        obj = CType(UnitOperations.ReturnInstance(xel.Element("Type").Value), ISimulationObject)
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

        If LoadSpreadsheetData IsNot Nothing Then LoadSpreadsheetData.Invoke(xdoc)

        If excs.Count > 0 Then
            ShowMessage("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", IFlowsheet.MessageType.GeneralError)
            For Each ex As Exception In excs
                ShowMessage(ex.Message.ToString & ": " & ex.InnerException.ToString, IFlowsheet.MessageType.GeneralError)
            Next
        Else
            ShowMessage("Data loaded successfully.", IFlowsheet.MessageType.Information)
        End If

    End Sub

    Public Function SaveToMXML() As XDocument

        Dim xdoc = SaveToXML()

        Parallel.ForEach(xdoc.Descendants, Sub(xel1)
                                               SharedClasses.Utility.UpdateElementForMobileXMLSaving_CrossPlatformUI(xel1)
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

        xel.Add(New XElement("BuildVersion", My.Application.Info.Version.ToString))
        xel.Add(New XElement("BuildDate", CType("01/01/2000", DateTime).AddDays(My.Application.Info.Version.Build).AddSeconds(My.Application.Info.Version.Revision * 2)))
        If GlobalSettings.Settings.RunningPlatform() = GlobalSettings.Settings.Platform.Mac Then
            xel.Add(New XElement("OSInfo", "macOS " + Environment.OSVersion.ToString()))
        Else
            xel.Add(New XElement("OSInfo", My.Computer.Info.OSFullName & ", Version " & My.Computer.Info.OSVersion & ", " & My.Computer.Info.OSPlatform & " Platform"))
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

        Dim flsconfig As New System.Text.StringBuilder()

        With flsconfig
            .Append(FlowsheetSurface.Zoom.ToString(ci) & ";")
            .Append("0;")
            .Append("0")
        End With

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("FlowsheetView"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("FlowsheetView")

        xel.Add(flsconfig.ToString)

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

        If SaveSpreadsheetData IsNot Nothing Then SaveSpreadsheetData.Invoke(xdoc)

        Return xdoc

    End Function

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
                        End If
                    Else
                        If obj.Name = "" Then obj.Name = obj.Tag
                        obj.CreateConnectors(0, 0)
                    End If
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

        FlowsheetSurface.DrawPropertyList = Options.DisplayCornerPropertyList
        FlowsheetSurface.DrawFloatingTable = Options.DisplayFloatingPropertyTables

        AddPropPacks()
        AddExternalUOs()
        AddFlashAlgorithms()

        Dim fa As New Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops()
        fa.Tag = fa.Name & " (1)"

        Options.FlashAlgorithms.Add(fa)

        Dim addedcomps As New List(Of String)

        Task.Factory.StartNew(Sub()
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
                                  For Each cp As ConstantProperties In cpa
                                      If Not addedcomps.Contains(cp.Name.ToLower) AndAlso Not AvailableCompounds.ContainsKey(cp.Name) Then
                                          If AvailableCompounds.Values.Where(Function(x) x.CAS_Number = cp.CAS_Number).Count = 0 Then
                                              AvailableCompounds.Add(cp.Name, cp)
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
                                  AddSystemsOfUnits()
                                  AddDefaultProperties()
                              End Sub)

    End Sub

    Public Sub Reset() Implements IFlowsheet.Reset

        SimulationObjects.Clear()
        GraphicObjects.Clear()
        FlowsheetSurface.DrawingObjects.Clear()
        SelectedCompounds.Clear()
        Options = New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables()
        FlowsheetSurface.Zoom = 1.0#

    End Sub

    Public Sub AddUndoRedoAction(action As IUndoRedoAction) Implements IFlowsheet.AddUndoRedoAction

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

        Dim pathtosave As String = My.Computer.FileSystem.SpecialDirectories.Temp + Path.DirectorySeparatorChar
        Dim fullname As String = ""

        'Dim pwd As String = Nothing
        'If IsZipFilePasswordProtected(caminho) Then
        '    Dim fp As New FormPassword
        '    If fp.ShowDialog() = Windows.Forms.DialogResult.OK Then
        '        pwd = fp.tbPassword.Text
        '    End If
        'End If

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(pathtofile))
            stream.Password = Nothing
            Dim entry As ZipEntry
Label_00CC:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(pathtosave + Path.GetFileName(entry.Name))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                fullname = pathtosave + Path.GetFileName(entry.Name)
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
        File.Delete(fullname)

        Return xdoc

    End Function

    Public Shared Function LoadZippedXMLDoc(pathtofile As String) As XDocument

        Dim pathtosave As String = My.Computer.FileSystem.SpecialDirectories.Temp + Path.DirectorySeparatorChar
        Dim fullname As String = ""

        Using stream As ZipInputStream = New ZipInputStream(File.OpenRead(pathtofile))
            stream.Password = Nothing
            Dim entry As ZipEntry
Label_00CC:
            entry = stream.GetNextEntry()
            Do While (Not entry Is Nothing)
                Dim fileName As String = Path.GetFileName(entry.Name)
                If (fileName <> String.Empty) Then
                    Using stream2 As FileStream = File.Create(pathtosave + Path.GetFileName(entry.Name))
                        Dim count As Integer = 2048
                        Dim buffer As Byte() = New Byte(2048) {}
                        Do While True
                            count = stream.Read(buffer, 0, buffer.Length)
                            If (count <= 0) Then
                                fullname = pathtosave + Path.GetFileName(entry.Name)
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

    Sub AddPropPacks()

        Dim CPPP As CoolPropPropertyPackage = New CoolPropPropertyPackage()
        CPPP.ComponentName = "CoolProp"
        AvailablePropertyPackages.Add(CPPP.ComponentName.ToString, CPPP)

        Dim CPIPP As New CoolPropIncompressiblePurePropertyPackage()
        CPIPP.ComponentName = "CoolProp (Incompressible Fluids)"
        CPIPP.ComponentDescription = "CoolProp (Incompressible Fluids)"
        AvailablePropertyPackages.Add(CPIPP.ComponentName.ToString, CPIPP)

        Dim CPIMPP As New CoolPropIncompressibleMixturePropertyPackage()
        CPIMPP.ComponentName = "CoolProp (Incompressible Mixtures)"
        CPIMPP.ComponentDescription = "CoolProp (Incompressible Mixtures)"
        AvailablePropertyPackages.Add(CPIMPP.ComponentName.ToString, CPIMPP)

        Dim SWPP As New SourWaterPropertyPackage()
        SWPP.ComponentName = "Sour Water"
        AvailablePropertyPackages.Add(SWPP.ComponentName.ToString, SWPP)

        Dim STPP As SteamTablesPropertyPackage = New SteamTablesPropertyPackage()
        STPP.ComponentName = "Steam Tables (IAPWS-IF97)"
        AvailablePropertyPackages.Add(STPP.ComponentName.ToString, STPP)

        Dim SEAPP As SeawaterPropertyPackage = New SeawaterPropertyPackage()
        SEAPP.ComponentName = "Seawater IAPWS-08"
        AvailablePropertyPackages.Add(SEAPP.ComponentName.ToString, SEAPP)

        Dim PRPP As PengRobinsonPropertyPackage = New PengRobinsonPropertyPackage()
        PRPP.ComponentName = "Peng-Robinson (PR)"
        AvailablePropertyPackages.Add(PRPP.ComponentName.ToString, PRPP)

        Dim PRSV2PP As PRSV2PropertyPackage = New PRSV2PropertyPackage()
        PRSV2PP.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-M)"
        AvailablePropertyPackages.Add(PRSV2PP.ComponentName.ToString, PRSV2PP)

        Dim PRSV2PPVL As PRSV2VLPropertyPackage = New PRSV2VLPropertyPackage()
        PRSV2PPVL.ComponentName = "Peng-Robinson-Stryjek-Vera 2 (PRSV2-VL)"
        AvailablePropertyPackages.Add(PRSV2PPVL.ComponentName.ToString, PRSV2PPVL)

        Dim SRKPP As SRKPropertyPackage = New SRKPropertyPackage()
        SRKPP.ComponentName = "Soave-Redlich-Kwong (SRK)"
        AvailablePropertyPackages.Add(SRKPP.ComponentName.ToString, SRKPP)

        Dim PRLKPP As PengRobinsonLKPropertyPackage = New PengRobinsonLKPropertyPackage()
        PRLKPP.ComponentName = "Peng-Robinson / Lee-Kesler (PR/LK)"
        AvailablePropertyPackages.Add(PRLKPP.ComponentName.ToString, PRLKPP)

        Dim UPP As UNIFACPropertyPackage = New UNIFACPropertyPackage()
        UPP.ComponentName = "UNIFAC"
        AvailablePropertyPackages.Add(UPP.ComponentName.ToString, UPP)

        Dim ULLPP As UNIFACLLPropertyPackage = New UNIFACLLPropertyPackage()
        ULLPP.ComponentName = "UNIFAC-LL"
        AvailablePropertyPackages.Add(ULLPP.ComponentName.ToString, ULLPP)

        Dim MUPP As MODFACPropertyPackage = New MODFACPropertyPackage()
        MUPP.ComponentName = "Modified UNIFAC (Dortmund)"
        AvailablePropertyPackages.Add(MUPP.ComponentName.ToString, MUPP)

        Dim NUPP As NISTMFACPropertyPackage = New NISTMFACPropertyPackage()
        NUPP.ComponentName = "Modified UNIFAC (NIST)"
        AvailablePropertyPackages.Add(NUPP.ComponentName.ToString, NUPP)

        Dim NRTLPP As NRTLPropertyPackage = New NRTLPropertyPackage()
        NRTLPP.ComponentName = "NRTL"
        AvailablePropertyPackages.Add(NRTLPP.ComponentName.ToString, NRTLPP)

        Dim UQPP As UNIQUACPropertyPackage = New UNIQUACPropertyPackage()
        UQPP.ComponentName = "UNIQUAC"
        AvailablePropertyPackages.Add(UQPP.ComponentName.ToString, UQPP)

        Dim CSLKPP As ChaoSeaderPropertyPackage = New ChaoSeaderPropertyPackage()
        CSLKPP.ComponentName = "Chao-Seader"
        AvailablePropertyPackages.Add(CSLKPP.ComponentName.ToString, CSLKPP)

        Dim GSLKPP As GraysonStreedPropertyPackage = New GraysonStreedPropertyPackage()
        GSLKPP.ComponentName = "Grayson-Streed"
        AvailablePropertyPackages.Add(GSLKPP.ComponentName.ToString, GSLKPP)

        Dim RPP As RaoultPropertyPackage = New RaoultPropertyPackage()
        RPP.ComponentName = "Raoult's Law"
        AvailablePropertyPackages.Add(RPP.ComponentName.ToString, RPP)

        Dim LKPPP As LKPPropertyPackage = New LKPPropertyPackage()
        LKPPP.ComponentName = "Lee-Kesler-Plöcker"
        AvailablePropertyPackages.Add(LKPPP.ComponentName.ToString, LKPPP)

        'Dim EUQPP As ExUNIQUACPropertyPackage = New ExUNIQUACPropertyPackage()
        'EUQPP.ComponentName = "Extended UNIQUAC (Aqueous Electrolytes)"
        'AvailablePropertyPackages.Add(EUQPP.ComponentName.ToString, EUQPP)

        'Dim ENQPP As New ElectrolyteNRTLPropertyPackage()
        'ENQPP.ComponentName = "Electrolyte NRTL (Aqueous Electrolytes)"
        'AvailablePropertyPackages.Add(ENQPP.ComponentName.ToString, ENQPP)

        Dim BOPP As BlackOilPropertyPackage = New BlackOilPropertyPackage()
        BOPP.ComponentName = "Black Oil"
        AvailablePropertyPackages.Add(BOPP.ComponentName.ToString, BOPP)

        Dim otherpps = SharedClasses.Utility.LoadAdditionalPropertyPackages()

        For Each pp In otherpps
            AvailablePropertyPackages.Add(DirectCast(pp, CapeOpen.ICapeIdentification).ComponentName, pp)
        Next

        'Check if DWSIM is running in Portable/Mono mode, if not then load the CAPE-OPEN Wrapper Property Package.
        If Not GlobalSettings.Settings.IsRunningOnMono Then
            Dim COPP As CAPEOPENPropertyPackage = New CAPEOPENPropertyPackage()
            COPP.ComponentName = "CAPE-OPEN"
            AvailablePropertyPackages.Add(COPP.ComponentName.ToString, COPP)
        End If

    End Sub

    Sub AddExternalUOs()

        Dim otheruos = SharedClasses.Utility.LoadAdditionalUnitOperations()

        For Each uo In otheruos
            ExternalUnitOperations.Add(uo.Description, uo)
        Next

    End Sub


    Function GetPropertyPackages(ByVal assmbly As Assembly) As List(Of Interfaces.IPropertyPackage)

        Dim availableTypes As New List(Of Type)()

        Try
            availableTypes.AddRange(assmbly.GetTypes())
        Catch ex As Exception
        End Try

        Dim ppList As List(Of Type) = availableTypes.FindAll(Function(t) t.GetInterfaces().Contains(GetType(Interfaces.IPropertyPackage)) And Not t.IsAbstract)

        Return ppList.ConvertAll(Of Interfaces.IPropertyPackage)(Function(t As Type) TryCast(Activator.CreateInstance(t), Interfaces.IPropertyPackage))

    End Function

    Sub AddFlashAlgorithms()

        Dim calculatorassembly = My.Application.Info.LoadedAssemblies.Where(Function(x) x.FullName.Contains("DWSIM.Thermodynamics,")).FirstOrDefault
        Dim availableTypes As New List(Of Type)()

        availableTypes.AddRange(calculatorassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.IFlashAlgorithm") IsNot Nothing, True, False)))

        For Each item In availableTypes.OrderBy(Function(x) x.Name)
            If Not item.IsAbstract Then
                Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.IFlashAlgorithm)
                If Not obj.InternalUseOnly Then AvailableFlashAlgorithms.Add(obj.Name, obj)
                If obj.Name.Contains("Gibbs") Then
                    Dim obj2 = obj.Clone
                    DirectCast(obj2, Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.GibbsMinimization3P).ForceTwoPhaseOnly = True
                    AvailableFlashAlgorithms.Add(obj2.Name, obj2)
                End If
                If TypeOf obj Is Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoopsSLE Then
                    Dim obj2 = obj.Clone
                    DirectCast(obj2, Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoopsSLE).SolidSolution = True
                    AvailableFlashAlgorithms.Add(obj2.Name, obj2)
                End If
            End If
        Next

    End Sub

    Private Sub AddSystemsOfUnits()

        With Me.AvailableSystemsOfUnits

            .Add(New SystemsOfUnits.SI)
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
                Dim dontadd = New String() {"SI", "CGS", "ENG", "C1", "C2", "C3", "C4", "C5"}
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
            aTypeList.AddRange(unitopassembly.GetTypes().Where(Function(x) If(x.GetInterface("DWSIM.Interfaces.ISimulationObject") IsNot Nothing, True, False)))

            For Each item In aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    obj.SetFlowsheet(Me)
                    Me.FlowsheetOptions.VisibleProperties(item.Name) = obj.GetDefaultProperties.ToList
                    obj = Nothing
                End If
            Next

        End If

    End Sub

    Public Function GetSpreadsheetData(range As String) As List(Of String()) Implements IFlowsheet.GetSpreadsheetData

        Return RetrieveSpreadsheetData.Invoke(range)

    End Function

    Public Property Scripts As New Dictionary(Of String, IScript) Implements IFlowsheet.Scripts

    Public Property Charts As Dictionary(Of String, IChart) = New Dictionary(Of String, IChart) Implements IFlowsheet.Charts

    Public Sub RunScript(ScriptID As String)
        Dim script = Scripts(ScriptID)
        If script.PythonInterpreter = Enums.Scripts.Interpreter.IronPython Then
            RunScript_IronPython(script.ScriptText)
        Else
            RunScript_PythonNET(script.ScriptText)
        End If
    End Sub

    Public Async Sub RunScriptAsync(ScriptID As String)
        Await Task.Factory.StartNew(Sub() RunScript(ScriptID))
    End Sub

    Private Sub RunScript_IronPython(scripttext As String)

        Dim scope As Microsoft.Scripting.Hosting.ScriptScope
        Dim engine As Microsoft.Scripting.Hosting.ScriptEngine

        Dim opts As New Dictionary(Of String, Object)()
        opts("Frames") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True
        engine = IronPython.Hosting.Python.CreateEngine(opts)
        engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
        engine.Runtime.LoadAssembly(GetType(Thermodynamics.BaseClasses.ConstantProperties).Assembly)
        engine.Runtime.LoadAssembly(GetType(Drawing.SkiaSharp.GraphicsSurface).Assembly)
        engine.Runtime.IO.SetOutput(New FlowsheetLogTextStream(Me), UTF8Encoding.UTF8)
        scope = engine.CreateScope()
        scope.SetVariable("Plugins", UtilityPlugins)
        scope.SetVariable("Flowsheet", Me)
        scope.SetVariable("Application", GetApplicationObject)
        scope.SetVariable("Spreadsheet", GetSpreadsheetObject.Invoke())
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

        If Not GlobalSettings.Settings.PythonInitialized Then

            Dim t As Task = Task.Factory.StartNew(Sub()
                                                      RunCodeOnUIThread(Sub()
                                                                            If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                PythonEngine.PythonHome = GlobalSettings.Settings.PythonPath
                                                                            End If
                                                                            PythonEngine.Initialize()
                                                                            GlobalSettings.Settings.PythonInitialized = True
                                                                        End Sub)
                                                  End Sub)
            t.Wait()

            Dim t2 As Task = Task.Factory.StartNew(Sub()
                                                       RunCodeOnUIThread(Sub()
                                                                             PythonEngine.BeginAllowThreads()
                                                                         End Sub)
                                                   End Sub)
            t2.Wait()

        End If

        Dim t3 As Task = Task.Factory.StartNew(Sub()
                                                   RunCodeOnUIThread(Sub()
                                                                         Using Py.GIL

                                                                             Try

                                                                                 Dim sys As Object = PythonEngine.ImportModule("sys")

                                                                                 If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                     Dim codeToRedirectOutput As String = "import sys" & vbCrLf + "from io import BytesIO as StringIO" & vbCrLf + "sys.stdout = mystdout = StringIO()" & vbCrLf + "sys.stdout.flush()" & vbCrLf + "sys.stderr = mystderr = StringIO()" & vbCrLf + "sys.stderr.flush()"
                                                                                     PythonEngine.RunSimpleString(codeToRedirectOutput)
                                                                                 End If

                                                                                 Dim locals As New PyDict()

                                                                                 locals.SetItem("Plugins", UtilityPlugins.ToPython)
                                                                                 locals.SetItem("Flowsheet", Me.ToPython)
                                                                                 Try
                                                                                     locals.SetItem("Spreadsheet", (GetSpreadsheetObject.Invoke()).ToPython)
                                                                                 Catch ex As Exception
                                                                                 End Try
                                                                                 Dim Solver As New FlowsheetSolver.FlowsheetSolver
                                                                                 locals.SetItem("Solver", Solver.ToPython)

                                                                                 If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                     locals.SetItem("Application", GetApplicationObject.ToPython)
                                                                                 End If

                                                                                 PythonEngine.Exec(scripttext, Nothing, locals.Handle)

                                                                                 If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                     ShowMessage(sys.stdout.getvalue().ToString, IFlowsheet.MessageType.Information)
                                                                                 End If

                                                                             Catch ex As Exception

                                                                                 ShowMessage("Error running script: " & ex.Message.ToString, IFlowsheet.MessageType.GeneralError)

                                                                             Finally

                                                                             End Try

                                                                         End Using
                                                                     End Sub)
                                               End Sub)
        t3.Wait()

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

End Class

