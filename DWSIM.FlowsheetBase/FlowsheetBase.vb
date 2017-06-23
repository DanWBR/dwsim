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
Imports DWSIM.SharedClasses.DWSIM
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.UnitOperations.Streams
Imports DWSIM.Thermodynamics.Streams
Imports ICSharpCode.SharpZipLib.Zip
Imports System.IO

Public MustInherit Class FlowsheetBase

    Implements IFlowsheet, IFlowsheetCalculationQueue

    Public WithEvents Options As New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables

    Private FlowsheetSurface As New GraphicsSurface

    Public SensAnalysisCollection As New List(Of Optimization.SensitivityAnalysisCase)

    Public OptimizationCollection As New List(Of Optimization.OptimizationCase)

    Private loaded As Boolean = False

    Private rm, prm As Resources.ResourceManager

    Public Sub AddCompoundsToMaterialStream(ms As IMaterialStream) Implements IFlowsheet.AddCompoundsToMaterialStream
        For Each phase As IPhase In ms.Phases.Values
            For Each comp In Me.Options.SelectedComponents.Values
                With phase
                    .Compounds.Add(comp.Name, New Compound(comp.Name, ""))
                    .Compounds(comp.Name).ConstantProperties = comp
                End With
            Next
        Next
        ms.EqualizeOverallComposition()
        ms.CalcOverallCompMassFractions()
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
        FlowsheetSurface.ConnectObject(gobjfrom, gobjto, fromidx, toidx)
    End Sub

    Public Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject) Implements IFlowsheet.DisconnectObjects
        FlowsheetSurface.DisconnectObject(gobjfrom, gobjto, False)
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

                    If gobj.ObjectType = ObjectType.GO_Table Then

                        Me.FlowsheetSurface.DeleteSelectedObject(gobj)

                    ElseIf gobj.ObjectType = ObjectType.GO_Text Then

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

                        'If gobj.ObjectType = ObjectType.OT_Spec Then
                        '    Dim specobj As PortableDTL.UnitOperati = Me.Collections.FlowsheetObjectCollection(namesel)
                        '    If Me.SimulationObjects.ContainsKey(specobj.TargetObjectData.ID) Then
                        '        Me.SimulationObjects(specobj.TargetObjectData.ID).IsSpecAttached = False
                        '        Me.SimulationObjects(specobj.TargetObjectData.ID).AttachedSpecId = ""
                        '    End If
                        '    If Me.SimulationObjects.ContainsKey(specobj.SourceObjectData.ID) Then
                        '        Me.SimulationObjects(specobj.SourceObjectData.ID).IsSpecAttached = False
                        '        Me.SimulationObjects(specobj.SourceObjectData.ID).AttachedSpecId = ""
                        '    End If
                        'Else
                        If gobj.ObjectType = ObjectType.OT_Adjust Then
                            Dim adjobj As IAdjust = SimulationObjects(namesel)
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
        Return SimulationObjects(FlowsheetSurface.SelectedObject.Name)
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
                If text.Split("/").Length = 2 Then
                    Dim prop As String = text.Split("/")(0)
                    Dim ttext2 As String = prm.GetString(prop)
                    If ttext2 Is Nothing Then Return text Else Return ttext2 + " / " + text.Split("/")(1)
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

    Public Sub RequestCalculation(Optional sender As ISimulationObject = Nothing) Implements IFlowsheet.RequestCalculation

        If Not sender Is Nothing Then
            FlowsheetSolver.FlowsheetSolver.CalculateObject(Me, sender.Name)
        Else
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, GlobalSettings.Settings.SolverMode)
        End If

    End Sub

    Public Property SelectedCompounds As Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.SelectedCompounds
        Get
            Return Options.SelectedComponents
        End Get
        Set(value As Dictionary(Of String, ICompoundConstantProperties))
            Options.SelectedComponents = value
        End Set
    End Property

    Public MustOverride Sub ShowDebugInfo(text As String, level As Integer) Implements IFlowsheet.ShowDebugInfo

    Public MustOverride Sub ShowMessage(text As String, mtype As IFlowsheet.MessageType) Implements IFlowsheet.ShowMessage

    Public Property SimulationObjects As New Dictionary(Of String, ISimulationObject) Implements IFlowsheet.SimulationObjects

    Public MustOverride Sub UpdateOpenEditForms() Implements IFlowsheet.UpdateOpenEditForms

    Public Property CalculationQueue As New Queue(Of ICalculationArgs) Implements IFlowsheetCalculationQueue.CalculationQueue

    Public Function AddObjectToSurface(ByVal type As ObjectType, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "", Optional ByVal id As String = "") As String

        Dim gObj As IGraphicObject = Nothing
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Select Case type

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
                'Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("ADJT001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

                'Case ObjectType.OT_Spec

                '    Dim myNode As New SpecGraphic(mpx, mpy, 40, 40, 0)
                '    myNode.LineWidth = 2
                '    myNode.Fill = True
                '    myNode.FillColor = fillclr
                '    myNode.LineColor = lineclr
                '    myNode.Tag = "SPEC-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myNode.Tag = tag
                '    gObj = myNode
                '    gObj.Name = "ES-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myNode)
                '    Dim myADJ As Spec = New Spec(myNode.Name, "Especificao")
                '    myADJ.GraphicObject = myNode
                '    SimulationObjects.Add(myNode.Name, myADJ)

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

                'Case ObjectType.OT_EnergyRecycle

                '    Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 40, 40, 0)
                '    myNode.LineWidth = 2
                '    myNode.Fill = True
                '    myNode.FillColor = fillclr
                '    myNode.LineColor = lineclr
                '    myNode.Tag = "EREC-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myNode.Tag = tag
                '    gObj = myNode
                '    gObj.Name = "EREC-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myNode)
                '    Dim myADJ As EnergyRecycle = New EnergyRecycle(myNode.Name, "EnergyRecycle")
                '    myADJ.GraphicObject = myNode
                '    SimulationObjects.Add(myNode.Name, myADJ)

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

                'Case ObjectType.Tank

                '    Dim myTank As New TankGraphic(mpx, mpy, 50, 50, 0)
                '    myTank.LineWidth = 2
                '    myTank.Fill = True
                '    myTank.FillColor = fillclr
                '    myTank.LineColor = lineclr
                '    myTank.Tag = "TANK-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myTank.Tag = tag
                '    gObj = myTank
                '    gObj.Name = "TQ-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myTank)
                '    'OBJETO DWSIM
                '    Dim myCOTK As Tank = New Tank(myTank.Name, "Tanque")
                '    myCOTK.GraphicObject = myTank
                '    SimulationObjects.Add(myTank.Name, myCOTK)

            Case ObjectType.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 70)
                myVessel.Tag = "SEP-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                gObj.Name = "SEP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myVessel)
                'OBJETO DWSIM
                Dim myCOVESSEL As Vessel = New Vessel(myVessel.Name, "")
                myCOVESSEL.GraphicObject = myVessel
                SimulationObjects.Add(myVessel.Name, myCOVESSEL)

            Case ObjectType.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 30, 30)
                myMStr.Tag = "MSTR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "MSTR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                GraphicObjects.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As MaterialStream = New MaterialStream(myMStr.Name, "CorrentedeMatria")
                myCOMS.GraphicObject = myMStr
                AddCompoundsToMaterialStream(myCOMS)
                SimulationObjects.Add(myCOMS.Name, myCOMS)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 30, 30)
                myMStr.Tag = "ESTR-" & SimulationObjects.Count.ToString("00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "ESTR-" & Guid.NewGuid.ToString
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

                'Case ObjectType.RCT_Gibbs

                '    Dim myRgibbs As New ReactorGibbsGraphic(mpx, mpy, 50, 50, 0)
                '    myRgibbs.LineWidth = 2
                '    myRgibbs.Fill = True
                '    myRgibbs.FillColor = fillclr
                '    myRgibbs.LineColor = lineclr
                '    myRgibbs.Tag = "RG-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myRgibbs.Tag = tag
                '    gObj = myRgibbs
                '    gObj.Name = "RG-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myRgibbs)
                '    'OBJETO DWSIM
                '    Dim myCORGIBBS As Reactor_Gibbs = New Reactor_Gibbs(myRgibbs.Name, "ReatorGibbs")
                '    myCORGIBBS.GraphicObject = myRgibbs
                '    SimulationObjects.Add(myRgibbs.Name, myCORGIBBS)

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

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

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

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

                'Case ObjectType.ReboiledAbsorber

                '    Dim mySC As New ReboiledAbsorberGraphic(mpx, mpy, 144, 180, 0)
                '    mySC.LineWidth = 2
                '    mySC.Fill = True
                '    mySC.FillColor = fillclr
                '    mySC.LineColor = lineclr
                '    mySC.Tag = "RBA-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then mySC.Tag = tag
                '    gObj = mySC
                '    gObj.Name = "RBA-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, mySC)
                '    'OBJETO DWSIM
                '    Dim myCOSC As ReboiledAbsorber = New ReboiledAbsorber(mySC.Name, "ReboiledAbsorber", Flowsheet)
                '    myCOSC.GraphicObject = mySC
                '    SimulationObjects.Add(mySC.Name, myCOSC)

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

                'Case ObjectType.RefluxedAbsorber

                '    Dim mySC As New RefluxedAbsorberGraphic(mpx, mpy, 144, 180, 0)
                '    mySC.LineWidth = 2
                '    mySC.Fill = True
                '    mySC.FillColor = fillclr
                '    mySC.LineColor = lineclr
                '    mySC.Tag = "RFA-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then mySC.Tag = tag
                '    gObj = mySC
                '    gObj.Name = "RFA-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, mySC)
                '    'OBJETO DWSIM
                '    Dim myCOSC As RefluxedAbsorber = New RefluxedAbsorber(mySC.Name, "RefluxedAbsorber", Flowsheet)
                '    myCOSC.GraphicObject = mySC
                '    SimulationObjects.Add(mySC.Name, myCOSC)

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

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

                'Case ObjectType.SolidSeparator

                '    Dim myCSep As New SolidSeparatorGraphic(mpx, mpy, 50, 50, 0)
                '    myCSep.LineWidth = 2
                '    myCSep.Fill = True
                '    myCSep.FillColor = fillclr
                '    myCSep.LineColor = lineclr
                '    myCSep.Tag = "SS-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myCSep.Tag = tag
                '    gObj = myCSep
                '    gObj.Name = "SS-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myCSep)
                '    'OBJETO DWSIM
                '    Dim myCOCSEP As SolidsSeparator = New SolidsSeparator(myCSep.Name, "SolidsSeparator")
                '    myCOCSEP.GraphicObject = myCSep
                '    SimulationObjects.Add(myCSep.Name, myCOCSEP)

                'Case ObjectType.Filter

                '    Dim myCSep As New FilterGraphic(mpx, mpy, 50, 50, 0)
                '    myCSep.LineWidth = 2
                '    myCSep.Fill = True
                '    myCSep.FillColor = fillclr
                '    myCSep.LineColor = lineclr
                '    myCSep.Tag = "FT-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myCSep.Tag = tag
                '    gObj = myCSep
                '    gObj.Name = "FT-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myCSep)
                '    'OBJETO DWSIM
                '    Dim myCOCSEP As Filter = New Filter(myCSep.Name, "Filter")
                '    myCOCSEP.GraphicObject = myCSep
                '    SimulationObjects.Add(myCSep.Name, myCOCSEP)

                'Case ObjectType.OrificePlate

                '    Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25, 0)
                '    myOPL.LineWidth = 2
                '    myOPL.Fill = True
                '    myOPL.FillColor = fillclr
                '    myOPL.LineColor = lineclr
                '    myOPL.Tag = "OP-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myOPL.Tag = tag
                '    gObj = myOPL
                '    gObj.Name = "OP-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myOPL)
                '    'OBJETO DWSIM
                '    Dim myCOOPL As OrificePlate = New OrificePlate(myOPL.Name, "OrificePlate")
                '    myCOOPL.GraphicObject = myOPL
                '    SimulationObjects.Add(myOPL.Name, myCOOPL)

                'Case ObjectType.CustomUO

                '    Dim myCUO As New CustomUOGraphic(mpx, mpy, 25, 25, 0)
                '    myCUO.LineWidth = 2
                '    myCUO.Fill = True
                '    myCUO.FillColor = fillclr
                '    myCUO.LineColor = lineclr
                '    myCUO.Tag = "UO-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myCUO.Tag = tag
                '    gObj = myCUO
                '    gObj.Name = "UO-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myCUO)
                '    'OBJETO DWSIM
                '    Dim myCOCUO As CustomUO = New CustomUO(myCUO.Name, "CustomUnitOp")
                '    myCOCUO.GraphicObject = myCUO
                '    SimulationObjects.Add(myCUO.Name, myCOCUO)

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSUO001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

                'Case ObjectType.ExcelUO

                '    Dim myEUO As New ExcelUOGraphic(mpx, mpy, 25, 25, 0)
                '    myEUO.LineWidth = 2
                '    myEUO.Fill = True
                '    myEUO.FillColor = fillclr
                '    myEUO.LineColor = lineclr
                '    myEUO.Tag = "EXL-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myEUO.Tag = tag
                '    gObj = myEUO
                '    gObj.Name = "EXL-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myEUO)
                '    'OBJETO DWSIM
                '    Dim myCOEUO As ExcelUO = New ExcelUO(myEUO.Name, "ExcelUnitOp")
                '    myCOEUO.GraphicObject = myEUO
                '    SimulationObjects.Add(myEUO.Name, myCOEUO)

                'Case ObjectType.FlowsheetUO

                '    Dim myEUO As New FlowsheetUOGraphic(mpx, mpy, 25, 25, 0)
                '    myEUO.LineWidth = 2
                '    myEUO.Fill = True
                '    myEUO.FillColor = fillclr
                '    myEUO.LineColor = lineclr
                '    myEUO.Tag = "FS-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myEUO.Tag = tag
                '    gObj = myEUO
                '    gObj.Name = "FS-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myEUO)
                '    'OBJETO DWSIM
                '    Dim myCOEUO As Flowsheet = New Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                '    myCOEUO.GraphicObject = myEUO
                '    SimulationObjects.Add(myEUO.Name, myCOEUO)

                'Case ObjectType.CapeOpenUO

                '    Dim myCUO As New CapeOpenUOGraphic(mpx, mpy, 40, 40, 0)
                '    myCUO.LineWidth = 2
                '    myCUO.Fill = True
                '    myCUO.FillColor = fillclr
                '    myCUO.LineColor = lineclr
                '    myCUO.Tag = "COUO-" & SimulationObjects.Count.ToString("00#")
                '    If tag <> "" Then myCUO.Tag = tag
                '    gObj = myCUO
                '    gObj.Name = "COUO-" & Guid.NewGuid.ToString
                '    If id <> "" Then gObj.Name = id
                '    GraphicObjects.Add(gObj.Name, myCUO)
                '    'OBJETO DWSIM
                '    Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj)
                '    myCOCUO.GraphicObject = myCUO
                '    SimulationObjects.Add(myCUO.Name, myCOCUO)

                '    Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CAPE001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

        End Select

        If Not gObj Is Nothing Then
            gObj.Owner = SimulationObjects(gObj.Name)
            SimulationObjects(gObj.Name).SetFlowsheet(Me)
            FlowsheetSurface.DrawingObjects.Add(gObj)
            'If TypeOf gObj.Owner Is Streams.MaterialStream Then
            '    gObj.CreateConnectors(1, 1)
            '    FlowsheetSolver.CalculateObject(Me, gObj.Name)
            'End If

        End If

        Return gObj.Name

    End Function

    Public Sub LoadFromXML(xdoc As XDocument) Implements IFlowsheet.LoadFromXML

        For Each xel1 As XElement In xdoc.Descendants
            SharedClasses.Utility.UpdateElement(xel1)
            SharedClasses.Utility.UpdateElementForV5(xel1)
        Next

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("Settings").Elements.ToList

        Try
            Options.LoadData(data)
        Catch ex As Exception
            excs.Add(New Exception("Error Loading Flowsheet Settings", ex))
        End Try

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
                xel.Element("Type").Value = xel.Element("Type").Value.Replace("DWSIM.DWSIM.SimulationObjects", "DWSIM.Thermodynamics")
                Dim obj As PropertyPackage = New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value)
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
                If xel.Element("Type").Value.Contains("MaterialStream") Then
                    obj = New RaoultPropertyPackage().ReturnInstance(xel.Element("Type").Value)
                Else
                    obj = UnitOperations.ReturnInstance(xel.Element("Type").Value)
                End If
                Dim gobj As GraphicObject = (From go As GraphicObject In
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

        If excs.Count > 0 Then
            ShowMessage("Some errors where found while parsing the XML file. The simulation might not work as expected. Please read the subsequent messages for more details.", IFlowsheet.MessageType.GeneralError)
            For Each ex As Exception In excs
                ShowMessage(ex.Message.ToString & ": " & ex.InnerException.ToString, IFlowsheet.MessageType.GeneralError)
            Next
        Else
            ShowMessage("Data loaded successfully.", IFlowsheet.MessageType.Information)
        End If

    End Sub

    Public Function SaveToXML() As XDocument Implements IFlowsheet.SaveToXML

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))
        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GeneralInfo"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GeneralInfo")

        xel.Add(New XElement("SavedOn", Date.Now))

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

        For Each so As ICustomXMLSerialization In SimulationObjects.Values
            DirectCast(so, ISimulationObject).SetFlowsheet(Me)
            xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Settings"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Settings")

        xel.Add(Options.SaveData().ToArray())

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
                If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                If obj Is Nothing Then obj = GraphicObject.ReturnInstance(xel.Element("Type").Value)
                If Not obj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
                    obj.Name = pkey & obj.Name
                    obj.X += shift
                    obj.Y += shift
                    If pkey <> "" Then
                        searchtext = obj.Tag.Split("(")(0).Trim()
                        objcount = (From go As GraphicObject In FlowsheetSurface.DrawingObjects Select go Where go.Tag.Equals(obj.Tag)).Count
                        If objcount > 0 Then obj.Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    End If
                    If Not TypeOf obj Is TableGraphic Then

                        With DirectCast(obj, DWSIM.Drawing.SkiaSharp.GraphicObjects.ShapeGraphic)
                            .Fill = False
                            .LineWidth = 1
                            .GradientMode = False
                        End With

                        If obj.Name = "" Then obj.Name = obj.Tag
                        obj.CreateConnectors(0, 0)
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
                    Dim obj As GraphicObject = (From go As GraphicObject In
                                                            FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If obj Is Nothing Then obj = (From go As GraphicObject In
                                                                                    FlowsheetSurface.DrawingObjects Where go.Name = xel.Element("Name").Value).SingleOrDefault
                    If Not obj Is Nothing Then
                        Dim i As Integer = 0
                        For Each xel2 As XElement In xel.Element("InputConnectors").Elements
                            If xel2.@IsAttached = True Then
                                obj.InputConnectors(i).ConnectorName = pkey & xel2.@AttachedFromObjID & "|" & xel2.@AttachedFromConnIndex
                                obj.InputConnectors(i).Type = [Enum].Parse(obj.InputConnectors(i).Type.GetType, xel2.@ConnType)
                                If reconnectinlets Then
                                    Dim objFrom As GraphicObject = (From go As GraphicObject In
                                                                                   FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedFromObjID).SingleOrDefault
                                    If Not objFrom Is Nothing Then
                                        If Not objFrom.OutputConnectors(xel2.@AttachedFromConnIndex).IsAttached Then
                                            FlowsheetSurface.ConnectObject(objFrom, obj, xel2.@AttachedFromConnIndex, xel2.@AttachedToConnIndex)
                                        End If
                                    End If
                                End If
                            End If
                            i += 1
                        Next
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
                    Dim obj As IGraphicObject = (From go As IGraphicObject In
                                                            FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
                    If Not obj Is Nothing Then
                        For Each xel2 As XElement In xel.Element("OutputConnectors").Elements
                            If xel2.@IsAttached = True Then
                                Dim objToID = pkey & xel2.@AttachedToObjID
                                If objToID <> "" Then
                                    Dim objTo As IGraphicObject = (From go As IGraphicObject In
                                                                                    FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                    If objTo Is Nothing Then objTo = (From go As IGraphicObject In
                                                                                    FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                    Dim fromidx As Integer = -1
                                    Dim cp As IConnectionPoint = (From cp2 As IConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = obj.Name).SingleOrDefault
                                    If cp Is Nothing Then cp = (From cp2 As IConnectionPoint In objTo.InputConnectors Select cp2 Where cp2.ConnectorName.Split("|")(0) = xel2.@AttachedToObjID).SingleOrDefault
                                    If Not cp Is Nothing Then
                                        fromidx = cp.ConnectorName.Split("|")(1)
                                    End If
                                    If Not obj Is Nothing And Not objTo Is Nothing Then FlowsheetSurface.ConnectObject(obj, objTo, fromidx, xel2.@AttachedToConnIndex)
                                End If
                            End If
                        Next
                        For Each xel2 As XElement In xel.Element("EnergyConnector").Elements
                            If xel2.@IsAttached = True Then
                                Dim objToID = pkey & xel2.@AttachedToObjID
                                If objToID <> "" Then
                                    Dim objTo As IGraphicObject = (From go As IGraphicObject In
                                                                                   FlowsheetSurface.DrawingObjects Where go.Name = objToID).SingleOrDefault
                                    If objTo Is Nothing Then obj = (From go As IGraphicObject In
                                                                                    FlowsheetSurface.DrawingObjects Where go.Name = xel2.@AttachedToObjID).SingleOrDefault
                                    If Not obj Is Nothing And Not objTo Is Nothing Then FlowsheetSurface.ConnectObject(obj, objTo, -1, xel2.@AttachedToConnIndex)
                                End If
                            End If
                        Next
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
                If Not t Is Nothing Then obj = Activator.CreateInstance(t)
                If obj Is Nothing Then obj = GraphicObject.ReturnInstance(xel.Element("Type").Value)
                If Not obj Is Nothing Then
                    obj.LoadData(xel.Elements.ToList)
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
                    Dim so2 As Adjust = so
                    If SimulationObjects.ContainsKey(so2.ManipulatedObjectData.ID) Then
                        so2.ManipulatedObject = SimulationObjects(so2.ManipulatedObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToMv = so2.ManipulatedObject.GraphicObject
                    End If
                    If SimulationObjects.ContainsKey(so2.ControlledObjectData.ID) Then
                        so2.ControlledObject = SimulationObjects(so2.ControlledObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToCv = so2.ControlledObject.GraphicObject
                    End If
                    If SimulationObjects.ContainsKey(so2.ReferencedObjectData.ID) Then
                        so2.ReferenceObject = SimulationObjects(so2.ReferencedObjectData.ID)
                        DirectCast(so2.GraphicObject, AdjustGraphic).ConnectedToRv = so2.ReferenceObject.GraphicObject
                    End If
                End If
                'If TryCast(so, Spec) IsNot Nothing Then
                '    Dim so2 As Spec = so
                '    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.TargetObjectData.ID) Then
                '        so2.TargetObject = form.Collections.FlowsheetObjectCollection(so2.TargetObjectData.ID)
                '        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToTv = so2.TargetObject.GraphicObject
                '    End If
                '    If form.Collections.FlowsheetObjectCollection.ContainsKey(so2.SourceObjectData.ID) Then
                '        so2.SourceObject = form.Collections.FlowsheetObjectCollection(so2.SourceObjectData.ID)
                '        DirectCast(so2.GraphicObject, SpecGraphic).ConnectedToSv = so2.SourceObject.GraphicObject
                '    End If
                'End If
                'If TryCast(so, CapeOpenUO) IsNot Nothing Then
                '    DirectCast(so, CapeOpenUO).UpdateConnectors2()
                '    DirectCast(so, CapeOpenUO).UpdatePortsFromConnectors()
                'End If
            Catch ex As Exception
                excs.Add(New Exception("Error Loading Unit Operation Connection Information", ex))
            End Try
        Next

    End Sub

    Public Property AvailableCompounds As New Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.AvailableCompounds

    Public Sub Initialize() Implements IFlowsheet.Initialize

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
                                  For Each cp As ConstantProperties In cpa
                                      If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                  Next
                                  Dim bddb As New Databases.Biodiesel
                                  bddb.Load()
                                  cpa = bddb.Transfer()
                                  For Each cp As ConstantProperties In cpa
                                      If Not AvailableCompounds.ContainsKey(cp.Name) Then AvailableCompounds.Add(cp.Name, cp)
                                  Next
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
        Return Me
    End Function

    Public Function GetUtility(uttype As Enums.FlowsheetUtility) As IAttachedUtility Implements IFlowsheet.GetUtility
        Return Nothing
    End Function

    Public Property MasterFlowsheet As IFlowsheet Implements IFlowsheet.MasterFlowsheet

    Public Property MasterUnitOp As ISimulationObject Implements IFlowsheet.MasterUnitOp

    Public Property Message As String Implements IFlowsheet.Message

    Public Property MobileCompatibilityMode As Boolean Implements IFlowsheet.MobileCompatibilityMode

    Public Sub RunCodeOnUIThread(act As Action) Implements IFlowsheet.RunCodeOnUIThread

    End Sub

    Public MustOverride Sub SetMessageListener(act As Action(Of String)) Implements IFlowsheet.SetMessageListener

    Public Property Solved As Boolean Implements IFlowsheet.Solved

    Public Sub UpdateSpreadsheet() Implements IFlowsheet.UpdateSpreadsheet

    End Sub

    Public MustOverride Sub UpdateInformation() Implements IFlowsheet.UpdateInformation

    Public MustOverride Sub UpdateInterface() Implements IFlowsheet.UpdateInterface

    Public ReadOnly Property UtilityPlugins As Dictionary(Of String, IUtilityPlugin) Implements IFlowsheet.UtilityPlugins
        Get
            Return Nothing
        End Get
    End Property

    Public Sub WriteSpreadsheetVariables() Implements IFlowsheet.WriteSpreadsheetVariables

    End Sub

    Public Sub ProcessScripts(eventType As Enums.Scripts.EventType, objectType As Enums.Scripts.ObjectType, obj As String) Implements IFlowsheet.ProcessScripts

    End Sub

    Public Sub LoadZippedXML(pathtofile As String)

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

        LoadFromXML(XDocument.Load(fullname))
        File.Delete(fullname)
        FilePath = pathtofile
        Options.FilePath = pathtofile

    End Sub

    Shared Function IsZipFilePasswordProtected(ByVal ZipFile As String) As Boolean
        Using fsIn As New FileStream(ZipFile, FileMode.Open, FileAccess.Read)
            Using zipInStream As New ZipInputStream(fsIn)
                Dim zEntry As ZipEntry = zipInStream.GetNextEntry()
                Return zEntry.IsCrypted
            End Using
        End Using
    End Function

End Class
