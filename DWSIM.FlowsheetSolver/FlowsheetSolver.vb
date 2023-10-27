'    DWSIM Flowsheet Solver & Auxiliary Functions
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

Imports System.IO
Imports System.Threading
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.Interfaces.Enums
Imports DWSIM.GlobalSettings
Imports DWSIM.ExtensionMethods
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports DWSIM.SharedClasses

'custom event handler declaration
Public Delegate Sub CustomEvent(ByVal sender As Object, ByVal e As System.EventArgs, ByVal extrainfo As Object)
Public Delegate Sub CustomEvent2(ByVal objinfo As CalculationArgs)

<System.Serializable()> Public Class FlowsheetSolver

    'events for plugins
    Public Shared Event UnitOpCalculationStarted As CustomEvent
    Public Shared Event UnitOpCalculationFinished As CustomEvent
    Public Shared Event FlowsheetCalculationStarted As CustomEvent
    Public Shared Event FlowsheetCalculationFinished As CustomEvent
    Public Shared Event MaterialStreamCalculationStarted As CustomEvent
    Public Shared Event MaterialStreamCalculationFinished As CustomEvent
    Public Shared Event CalculationError As CustomEvent
    Public Shared Event CalculatingObject As CustomEvent2

    ''' <summary>
    ''' Flowsheet calculation routine 1. Calculates the object using information sent by the queue and updates the flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to calculate (FormChild object).</param>
    ''' <param name="objArgs">A CalculationArgs object containing information about the object to be calculated and its current status.</param>
    ''' <param name="sender"></param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateObject(ByVal fobj As Object, ByVal objArgs As CalculationArgs, ByVal sender As Object, Optional ByVal OnlyMe As Boolean = False)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        RaiseEvent UnitOpCalculationStarted(fobj, New System.EventArgs(), objArgs)

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, objArgs.Name)

        Select Case objArgs.ObjectType
            Case ObjectType.MaterialStream
                Dim myObj = fbag.SimulationObjects(objArgs.Name)
                Dim gobj As IGraphicObject = myObj.GraphicObject
                If Not gobj Is Nothing Then
                    If gobj.OutputConnectors(0).IsAttached = True Then
                        Dim myUnitOp = fbag.SimulationObjects(myObj.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                        If objArgs.Sender = "Spec" Or objArgs.Sender = "FlowsheetSolver" Then
                            CalculateMaterialStream(fobj, myObj, , OnlyMe)
                        Else
                            If objArgs.Calculated = True Then
                                gobj = myUnitOp.GraphicObject
                                gobj.Calculated = False
                                myUnitOp.Calculated = False

                                If myUnitOp.IsSpecAttached = True Then
                                    If myUnitOp.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                                        fbag.SimulationObjects(myUnitOp.AttachedSpecId).Solve()
                                    End If
                                End If

                                For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                                    Dim spec = DirectCast(obj, ISpec)
                                    If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = myUnitOp.Name Then
                                        obj.Solve()
                                    End If
                                Next

                                If fbag.DynamicMode Then
                                    myUnitOp.RunDynamicModel()
                                Else
                                    myUnitOp.Solve()
                                End If

                                For Each utility In myUnitOp.AttachedUtilities
                                    If utility.AutoUpdate Then utility.Update()
                                Next

                                myUnitOp.Calculated = True
                                gobj.Status = Status.Calculated

                                If myUnitOp.IsSpecAttached = True Then
                                    If myUnitOp.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                                        fbag.SimulationObjects(myUnitOp.AttachedSpecId).Solve()
                                    End If
                                End If

                                For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                                    Dim spec = DirectCast(obj, ISpec)
                                    If spec.SpecCalculationMode = SpecCalcMode2.AfterObject And spec.ReferenceObjectID = myUnitOp.Name Then
                                        obj.Solve()
                                    End If
                                Next

                                fgui.ShowMessage(gobj.Tag & ": " & fgui.GetTranslatedString("Calculadocomsucesso"), IFlowsheet.MessageType.Information)
                            Else
                                myUnitOp.DeCalculate()
                                gobj = myUnitOp.GraphicObject
                                myUnitOp.Calculated = False
                                gobj.Calculated = False
                            End If
                        End If
                    End If
                    If myObj.IsSpecAttached And myObj.SpecVarType = SpecVarType.Source Then fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                End If
            Case ObjectType.EnergyStream
                Dim myObj = fbag.SimulationObjects(objArgs.Name)
                myObj.Calculated = True
                Dim gobj As IGraphicObject = myObj.GraphicObject
                If Not gobj Is Nothing Then
                    If gobj.OutputConnectors(0).IsAttached = True And Not OnlyMe Then
                        Dim myUnitOp = fbag.SimulationObjects(myObj.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                        If objArgs.Calculated = True Then
                            myUnitOp.GraphicObject.Calculated = False
                            myUnitOp.Calculated = True

                            If myUnitOp.IsSpecAttached = True Then
                                If myUnitOp.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                                    fbag.SimulationObjects(myUnitOp.AttachedSpecId).Solve()
                                End If
                            End If

                            If fbag.DynamicMode Then
                                myUnitOp.RunDynamicModel()
                            Else
                                myUnitOp.Solve()
                            End If

                            For Each utility In myUnitOp.AttachedUtilities
                                If utility.AutoUpdate Then utility.Update()
                            Next

                            myUnitOp.Calculated = False
                            fgui.ShowMessage(gobj.Tag & ": " & fgui.GetTranslatedString("Calculadocomsucesso"), IFlowsheet.MessageType.Information)
                            myUnitOp.GraphicObject.Calculated = True

                            If myUnitOp.IsSpecAttached = True Then
                                If myUnitOp.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                                    fbag.SimulationObjects(myUnitOp.AttachedSpecId).Solve()
                                End If
                            End If

                            gobj = myUnitOp.GraphicObject
                            gobj.Calculated = True
                        Else
                            myUnitOp.DeCalculate()
                            myUnitOp.GraphicObject.Calculated = False
                            myUnitOp.Calculated = False
                        End If
                    End If
                    If myObj.IsSpecAttached And myObj.SpecVarType = SpecVarType.Source Then fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                End If
            Case Else
                If objArgs.Sender = "Adjust" Or objArgs.Sender = "FlowsheetSolver" Then
                    Dim myObj As ISimulationObject = fbag.SimulationObjects(objArgs.Name)
                    myObj.GraphicObject.Calculated = False
                    myObj.Calculated = False

                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If

                    If fbag.DynamicMode Then
                        myObj.RunDynamicModel()
                    Else
                        myObj.Solve()
                    End If

                    For Each utility In myObj.AttachedUtilities
                        If utility.AutoUpdate Then utility.Update()
                    Next

                    myObj.Calculated = True
                    fgui.ShowMessage(objArgs.Tag & ": " & fgui.GetTranslatedString("Calculadocomsucesso"), IFlowsheet.MessageType.Information)
                    myObj.GraphicObject.Calculated = True

                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If

                Else
                    Dim myObj As ISimulationObject = fbag.SimulationObjects(objArgs.Name)
                    Dim gobj As IGraphicObject = myObj.GraphicObject

                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If

                    If Not OnlyMe Then
                        For Each cp As IConnectionPoint In gobj.OutputConnectors
                            If cp.IsAttached And cp.Type = ConType.ConOut Then
                                Dim obj = fbag.SimulationObjects(cp.AttachedConnector.AttachedTo.Name)
                                If obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                                    obj.GraphicObject.Calculated = False
                                    obj.Calculated = False
                                    If fbag.DynamicMode Then
                                        obj.RunDynamicModel()
                                    Else
                                        obj.Solve()
                                    End If
                                    obj.Calculated = True
                                    obj.GraphicObject.Calculated = True
                                End If
                            End If
                        Next
                    End If

                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If

                End If
        End Select

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, objArgs.Name)

        RaiseEvent UnitOpCalculationFinished(fobj, New System.EventArgs(), objArgs)

    End Sub

    ''' <summary>
    ''' Calculates the flowsheet objects asynchronously. This function is always called from a task or a different thread other than UI's.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to calculate (FormChild object).</param>
    ''' <param name="objArgs">A CalculationArgs object containing information about the object to be calculated and its current status.</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateObjectAsync(ByVal fobj As Object, ByVal objArgs As CalculationArgs, ct As Threading.CancellationToken)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

        If objArgs.Sender = "FlowsheetSolver" Then
            fgui.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, objArgs.Name)
            Select Case objArgs.ObjectType
                Case ObjectType.MaterialStream
                    Dim myObj = fbag.SimulationObjects(objArgs.Name)
                    RaiseEvent MaterialStreamCalculationStarted(fobj, New System.EventArgs(), myObj)
                    CalculateMaterialStreamAsync(fobj, myObj, ct)
                    RaiseEvent MaterialStreamCalculationFinished(fobj, New System.EventArgs(), myObj)
                Case ObjectType.EnergyStream
                    Dim myObj = fbag.SimulationObjects(objArgs.Name)
                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If
                    For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                        Dim spec = DirectCast(obj, ISpec)
                        If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = objArgs.Name Then
                            obj.Solve()
                        End If
                    Next
                    myObj.Solve()
                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If
                    For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                        Dim spec = DirectCast(obj, ISpec)
                        If spec.SpecCalculationMode = SpecCalcMode2.AfterObject And spec.ReferenceObjectID = objArgs.Name Then
                            obj.Solve()
                        End If
                    Next
                    myObj.Calculated = True
                Case Else
                    Dim myObj As ISimulationObject = fbag.SimulationObjects(objArgs.Name)
                    RaiseEvent UnitOpCalculationStarted(fobj, New System.EventArgs(), objArgs)
                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If
                    For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                        Dim spec = DirectCast(obj, ISpec)
                        If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = objArgs.Name Then
                            obj.Solve()
                        End If
                    Next
                    If fbag.DynamicMode Then
                        myObj.RunDynamicModel()
                    Else
                        myObj.Solve()
                    End If
                    For Each utility In myObj.AttachedUtilities
                        If utility.AutoUpdate Then utility.Update()
                    Next
                    myObj.Calculated = True
                    If myObj.IsSpecAttached = True Then
                        If myObj.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                            fbag.SimulationObjects(myObj.AttachedSpecId).Solve()
                        End If
                    End If
                    For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                        Dim spec = DirectCast(obj, ISpec)
                        If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = objArgs.Name Then
                            obj.Solve()
                        End If
                    Next
                    RaiseEvent UnitOpCalculationFinished(fobj, New System.EventArgs(), objArgs)
            End Select
            fgui.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, objArgs.Name)
        End If

    End Sub

    ''' <summary>
    ''' Material Stream calculation routine 1. This routine check all input values and calculates all remaining properties of the stream.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to what the stream belongs to.</param>
    ''' <param name="ms">Material Stream object to be calculated.</param>
    ''' <param name="DoNotCalcFlash">Tells the calculator whether to do flash calculations or not.</param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateMaterialStream(ByVal fobj As Object, ByVal ms As ISimulationObject, Optional ByVal DoNotCalcFlash As Boolean = False, Optional ByVal OnlyMe As Boolean = False)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        ms.Calculated = False

        RaiseEvent MaterialStreamCalculationStarted(fobj, New System.EventArgs(), ms)

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, ms.Name)

        ms.GraphicObject.Calculated = False

        If ms.IsSpecAttached = True Then
            If ms.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                fbag.SimulationObjects(ms.AttachedSpecId).Solve()
            End If
        End If

        For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
            Dim spec = DirectCast(obj, ISpec)
            If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = ms.Name Then
                obj.Solve()
            End If
        Next

        If fbag.DynamicMode Then
            ms.RunDynamicModel()
        Else
            ms.Solve()
        End If

        If ms.IsSpecAttached = True Then
            If ms.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                fbag.SimulationObjects(ms.AttachedSpecId).Solve()
            End If
        End If

        For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
            Dim spec = DirectCast(obj, ISpec)
            If spec.SpecCalculationMode = SpecCalcMode2.AfterObject And spec.ReferenceObjectID = ms.Name Then
                obj.Solve()
            End If
        Next

        fgui.ShowMessage(ms.GraphicObject.Tag & ": " & fgui.GetTranslatedString("Calculadocomsucesso"), IFlowsheet.MessageType.Information)

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, ms.Name)

        RaiseEvent MaterialStreamCalculationFinished(fobj, New System.EventArgs(), ms)

        ms.LastUpdated = Date.Now
        ms.Calculated = True

        If Not OnlyMe Then
            Dim objargs As New CalculationArgs
            With objargs
                .Calculated = True
                .Name = ms.Name
                .ObjectType = ObjectType.MaterialStream
            End With
            CalculateObject(fobj, objargs, Nothing)
        End If

    End Sub

    ''' <summary>
    ''' Calculates a material stream object asynchronously. This function is always called from a task or a different thread other than UI's.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to what the stream belongs to.</param>
    ''' <param name="ms">Material Stream object to be calculated.</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateMaterialStreamAsync(ByVal fobj As Object, ByVal ms As ISimulationObject, ct As Threading.CancellationToken)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

        ms.Calculated = False

        RaiseEvent MaterialStreamCalculationStarted(fobj, New System.EventArgs(), ms)

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, ms.Name)

        If ms.IsSpecAttached = True Then
            If ms.SpecVarType = SpecVarType.Target And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeTargetObject Then
                fbag.SimulationObjects(ms.AttachedSpecId).Solve()
            End If
        End If

        For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
            Dim spec = DirectCast(obj, ISpec)
            If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = ms.Name Then
                obj.Solve()
            End If
        Next

        If fbag.DynamicMode Then
            ms.RunDynamicModel()
        Else
            ms.Solve()
        End If

        fgui.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, ms.Name)

        RaiseEvent MaterialStreamCalculationFinished(fobj, New System.EventArgs(), ms)

        If ms.IsSpecAttached = True Then
            If ms.SpecVarType = SpecVarType.Source And fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterSourceObject Then
                fbag.SimulationObjects(ms.AttachedSpecId).Solve()
            End If
        End If

        For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
            Dim spec = DirectCast(obj, ISpec)
            If spec.SpecCalculationMode = SpecCalcMode2.BeforeObject And spec.ReferenceObjectID = ms.Name Then
                obj.Solve()
            End If
        Next

        ms.LastUpdated = Date.Now
        ms.Calculated = True

    End Sub

    ''' <summary>
    ''' Process the calculation queue of the Flowsheet passed as an argument. Checks all elements in the queue and calculates them.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object)</param>
    ''' <remarks></remarks>
    Public Shared Function ProcessCalculationQueue(ByVal fobj As Object, Optional ByVal ct As Threading.CancellationToken = Nothing,
                                              Optional ByVal Adjusting As Boolean = False) As List(Of Exception)

        Dim exlist As New List(Of Exception)

        exlist = ProcessQueueInternalAsync(fobj, ct)
        If Not Adjusting Then SolveSimultaneousAdjustsAsync(fobj, ct)

        Return exlist

    End Function

    Private Shared Sub CheckExceptionForAdditionalInfo(ex As Exception)
        If Not ex.Data.Contains("DetailedDescription") Then
            ex.Data.Add("DetailedDescription", "This error was raised during the calculation of a Unit Operation or Material Stream.")
        End If
        If Not ex.Data.Contains("UserAction") Then
            ex.Data.Add("UserAction", "Check input parameters. If this error keeps occurring, try another Property Package and/or Flash Algorithm.")
        End If
    End Sub

    ''' <summary>
    ''' This is the internal routine called by ProcessCalculationQueue when the UI thread is used to calculate the flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object)</param>
    ''' <param name="Isolated">Tells to the calculator that only the objects in the queue must be calculated without checking the outlet connections, that is, no more objects will be added to the queue</param>
    ''' <param name="FlowsheetSolverMode">Only objects added by the flowsheet solving routine to the queue will be calculated.</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Private Shared Function ProcessQueueInternal(ByVal fobj As Object, Optional ByVal Isolated As Boolean = False, Optional ByVal FlowsheetSolverMode As Boolean = False, Optional ByVal ct As Threading.CancellationToken = Nothing) As List(Of Exception)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        Dim d0 As Date = Date.Now

        Dim loopex As New List(Of Exception)

        While fqueue.CalculationQueue.Count >= 1

            If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

            Dim myinfo As CalculationArgs = fqueue.CalculationQueue.Peek()

            RaiseEvent CalculatingObject(myinfo)

            If fbag.SimulationObjects.ContainsKey(myinfo.Name) Then

                Dim myobj = fbag.SimulationObjects(myinfo.Name)
                Try
                    myobj.GraphicObject.Status = Status.Calculating
                    myobj.ErrorMessage = ""
                    If myobj.GraphicObject.Active Then
                        If FlowsheetSolverMode Then
                            If myinfo.Sender = "FlowsheetSolver" Then
                                If myinfo.ObjectType = ObjectType.MaterialStream Then
                                    CalculateMaterialStream(fobj, fbag.SimulationObjects(myinfo.Name), , Isolated)
                                Else
                                    CalculateObject(fobj, myinfo, Nothing, Isolated)
                                End If
                            End If
                        Else
                            If myinfo.ObjectType = ObjectType.MaterialStream Then
                                CalculateMaterialStream(fobj, fbag.SimulationObjects(myinfo.Name), , Isolated)
                            Else
                                CalculateObject(fobj, myinfo, Nothing, Isolated)
                            End If
                        End If
                        For Each au In myobj.AttachedUtilities
                            If au.AutoUpdate Then au.Update()
                        Next
                        myobj.GraphicObject.Calculated = True
                        myobj.LastUpdated = Date.Now
                        myobj.UpdateEditForm()
                        If fbag.DynamicMode Then myobj.UpdateDynamicsEditForm()
                    End If
                    myobj.GraphicObject.Status = Status.Calculated
                Catch ex As AggregateException
                    myobj.GraphicObject.Status = Status.ErrorCalculating
                    RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                    myobj.ErrorMessage = ""
                    For Each iex In ex.InnerExceptions
                        If TypeOf iex Is AggregateException Then
                            For Each iex2 In DirectCast(iex, AggregateException).InnerExceptions
                                If TypeOf iex2 Is AggregateException Then
                                    For Each iex3 In DirectCast(iex2, AggregateException).InnerExceptions
                                        If TypeOf iex3 Is AggregateException Then
                                            For Each iex4 In DirectCast(iex3, AggregateException).InnerExceptions
                                                myobj.ErrorMessage += iex4.Message.ToString & vbCrLf
                                                CheckExceptionForAdditionalInfo(iex4)
                                                iex4.Source = myinfo.Tag
                                                loopex.Add(New Exception(myinfo.Tag & ": " & iex4.Message, iex4))
                                            Next
                                        Else
                                            myobj.ErrorMessage += iex3.Message.ToString & vbCrLf
                                            CheckExceptionForAdditionalInfo(iex3)
                                            iex3.Source = myinfo.Tag
                                            loopex.Add(New Exception(myinfo.Tag & ": " & iex3.Message, iex3))
                                        End If
                                    Next
                                Else
                                    myobj.ErrorMessage += iex2.Message.ToString & vbCrLf
                                    CheckExceptionForAdditionalInfo(iex2)
                                    iex2.Source = myinfo.Tag
                                    loopex.Add(New Exception(myinfo.Tag & ": " & iex2.Message, iex2))
                                End If
                            Next
                        Else
                            myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                            CheckExceptionForAdditionalInfo(iex)
                            iex.Source = myinfo.Tag
                            loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message, iex))
                        End If
                    Next
                    If GlobalSettings.Settings.SolverBreakOnException Then Exit While
                Catch ex As Exception
                    myobj.GraphicObject.Status = Status.ErrorCalculating
                    RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                    myobj.ErrorMessage = ex.Message.ToString & vbCrLf
                    CheckExceptionForAdditionalInfo(ex)
                    ex.Source = myinfo.Tag
                    loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message))
                    If GlobalSettings.Settings.SolverBreakOnException Then Exit While
                Finally
                    fgui.UpdateInterface()
                End Try

            End If

            CheckCalculatorStatus()

            If fqueue.CalculationQueue.Count > 0 Then fqueue.CalculationQueue.Dequeue()

        End While

        fgui.ShowMessage(fgui.GetTranslatedString("Runtime") & " (s): " & (Date.Now - d0).TotalSeconds.ToString("G4"), IFlowsheet.MessageType.Information)

        Return loopex

    End Function

    ''' <summary>
    ''' This is the internal routine called by ProcessCalculationQueue when a background thread is used to calculate the flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object)</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Private Shared Function ProcessQueueInternalAsync(ByVal fobj As Object, ByVal ct As Threading.CancellationToken) As List(Of Exception)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        Dim d0 As Date = Date.Now

        Dim loopex As New List(Of Exception)

        While fqueue.CalculationQueue.Count >= 1

            If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

            Dim myinfo As CalculationArgs = fqueue.CalculationQueue.Peek()

            RaiseEvent CalculatingObject(myinfo)

            'fobj.UIThread(Sub() UpdateDisplayStatus(fobj, New String() {myinfo.Name}, True))

            Dim myobj = fbag.SimulationObjects(myinfo.Name)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.SetCurrent(Nothing)

            IObj?.Paragraphs.Add("This is the main routine for the calculation of a single object. Check the nested items for model details.")

            Try
                myobj.GraphicObject.Status = Status.Calculating
                myobj.ErrorMessage = ""
                If myobj.GraphicObject.Active Then
                    If myinfo.ObjectType = ObjectType.MaterialStream Then
                        CalculateMaterialStreamAsync(fobj, myobj, ct)
                    Else
                        CalculateObjectAsync(fobj, myinfo, ct)
                    End If
                    For Each au In myobj.AttachedUtilities
                        If au.AutoUpdate Then au.Update()
                    Next
                    myobj.GraphicObject.Calculated = True
                    myobj.LastUpdated = Date.Now
                    myobj.UpdateEditForm()
                    If fbag.DynamicMode Then myobj.UpdateDynamicsEditForm()
                End If
                myobj.GraphicObject.Status = Status.Calculated
            Catch ex As AggregateException
                myobj.GraphicObject.Status = Status.ErrorCalculating
                RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                fgui.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, myobj.Name)
                myobj.ErrorMessage = ""
                For Each iex In ex.InnerExceptions
                    If TypeOf iex Is AggregateException Then
                        For Each iex2 In DirectCast(iex, AggregateException).InnerExceptions
                            If TypeOf iex2 Is AggregateException Then
                                For Each iex3 In DirectCast(iex2, AggregateException).InnerExceptions
                                    If TypeOf iex3 Is AggregateException Then
                                        For Each iex4 In DirectCast(iex3, AggregateException).InnerExceptions
                                            myobj.ErrorMessage += iex4.Message.ToString & vbCrLf
                                            CheckExceptionForAdditionalInfo(iex4)
                                            iex4.Source = myinfo.Tag
                                            loopex.Add(New Exception(myinfo.Tag & ": " & iex4.Message, iex4))
                                        Next
                                    Else
                                        myobj.ErrorMessage += iex3.Message.ToString & vbCrLf
                                        CheckExceptionForAdditionalInfo(iex3)
                                        iex3.Source = myinfo.Tag
                                        loopex.Add(New Exception(myinfo.Tag & ": " & iex3.Message, iex3))
                                    End If
                                Next
                            Else
                                myobj.ErrorMessage += iex2.Message.ToString & vbCrLf
                                CheckExceptionForAdditionalInfo(iex2)
                                iex2.Source = myinfo.Tag
                                loopex.Add(New Exception(myinfo.Tag & ": " & iex2.Message, iex2))
                            End If
                        Next
                    Else
                        myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                        CheckExceptionForAdditionalInfo(iex)
                        iex.Source = myinfo.Tag
                        loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message, iex))
                    End If
                Next
                If GlobalSettings.Settings.SolverBreakOnException Then Exit While
            Catch ex As Exception
                myobj.GraphicObject.Status = Status.ErrorCalculating
                RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                fgui.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, myobj.Name)
                myobj.ErrorMessage = ex.Message.ToString
                CheckExceptionForAdditionalInfo(ex)
                ex.Source = myinfo.Tag
                loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message, ex))
                If GlobalSettings.Settings.SolverBreakOnException Then Exit While
            Finally
                fgui.UpdateInterface()
            End Try

            IObj?.Close()

            If fqueue.CalculationQueue.Count > 0 Then fqueue.CalculationQueue.Dequeue()

        End While


        Return loopex

    End Function

    ''' <summary>
    ''' This is the internal routine called by ProcessCalculationQueue when background parallel threads are used to calculate the flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object)</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Private Shared Function ProcessQueueInternalAsyncParallel(ByVal fobj As Object, ByVal orderedlist As Dictionary(Of Integer, List(Of CalculationArgs)), ct As Threading.CancellationToken) As List(Of Exception)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        Dim loopex As New Concurrent.ConcurrentBag(Of Exception)

        For Each obj In fbag.SimulationObjects.Values
            If TypeOf obj Is IMaterialStream Then
                DirectCast(obj, IMaterialStream).SetPropertyPackageObject(DirectCast(obj, IMaterialStream).GetPropertyPackageObjectCopy)
                DirectCast(obj, IMaterialStream).SetCurrentMaterialStream(obj)
            ElseIf TypeOf obj Is ISimulationObject Then
                obj.PropertyPackage = Nothing
                obj.PropertyPackage = DirectCast(obj, ISimulationObject).PropertyPackage.Clone()
            End If
        Next

        For Each li In orderedlist
            Dim objlist As New ArrayList
            For Each item In li.Value
                objlist.Add(item.Name)
            Next
            Parallel.ForEach(li.Value, Sub(myinfo, state)
                                           If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()
                                           Dim myobj = fbag.SimulationObjects(myinfo.Name)
                                           myobj.ErrorMessage = ""
                                           myobj.GraphicObject.Status = Status.Calculating
                                           Try
                                               If myobj.GraphicObject.Active Then
                                                   If myinfo.ObjectType = ObjectType.MaterialStream Then
                                                       CalculateMaterialStreamAsync(fobj, myobj, ct)
                                                   Else
                                                       CalculateObjectAsync(fobj, myinfo, ct)
                                                   End If
                                                   For Each au In myobj.AttachedUtilities
                                                       If au.AutoUpdate Then au.Update()
                                                   Next
                                                   myobj.GraphicObject.Calculated = True
                                                   myobj.LastUpdated = Date.Now
                                                   myobj.UpdateEditForm()
                                                   If fbag.DynamicMode Then myobj.UpdateDynamicsEditForm()
                                               End If
                                               myobj.GraphicObject.Status = Status.Calculated
                                           Catch ex As AggregateException
                                               myobj.GraphicObject.Status = Status.ErrorCalculating
                                               RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                                               fgui.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, myobj.Name)
                                               myobj.ErrorMessage = ""
                                               For Each iex In ex.InnerExceptions
                                                   If TypeOf iex Is AggregateException Then
                                                       For Each iex2 In DirectCast(iex, AggregateException).InnerExceptions
                                                           If TypeOf iex2 Is AggregateException Then
                                                               For Each iex3 In DirectCast(iex2, AggregateException).InnerExceptions
                                                                   If TypeOf iex3 Is AggregateException Then
                                                                       For Each iex4 In DirectCast(iex3, AggregateException).InnerExceptions
                                                                           myobj.ErrorMessage += iex4.Message.ToString & vbCrLf
                                                                           CheckExceptionForAdditionalInfo(iex4)
                                                                           iex4.Source = myinfo.Tag
                                                                           loopex.Add(New Exception(myinfo.Tag & ": " & iex4.Message, iex4))
                                                                       Next
                                                                   Else
                                                                       myobj.ErrorMessage += iex3.Message.ToString & vbCrLf
                                                                       CheckExceptionForAdditionalInfo(iex3)
                                                                       iex3.Source = myinfo.Tag
                                                                       loopex.Add(New Exception(myinfo.Tag & ": " & iex3.Message, iex3))
                                                                   End If
                                                               Next
                                                           Else
                                                               myobj.ErrorMessage += iex2.Message.ToString & vbCrLf
                                                               CheckExceptionForAdditionalInfo(iex2)
                                                               iex2.Source = myinfo.Tag
                                                               loopex.Add(New Exception(myinfo.Tag & ": " & iex2.Message, iex2))
                                                           End If
                                                       Next
                                                   Else
                                                       myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                                                       CheckExceptionForAdditionalInfo(iex)
                                                       iex.Source = myinfo.Tag
                                                       loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message, iex))
                                                   End If
                                               Next
                                               If GlobalSettings.Settings.SolverBreakOnException Then state.Break()
                                           Catch ex As Exception
                                               myobj.GraphicObject.Status = Status.ErrorCalculating
                                               RaiseEvent CalculationError(myinfo, New EventArgs(), ex)
                                               fgui.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, myobj.Name)
                                               myobj.ErrorMessage = ex.Message.ToString
                                               CheckExceptionForAdditionalInfo(ex)
                                               loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message, ex))
                                               ex.Source = myinfo.Tag
                                               If GlobalSettings.Settings.SolverBreakOnException Then state.Break()
                                           Finally
                                               fgui.UpdateInterface()
                                           End Try
                                       End Sub)
        Next

        For Each obj In fbag.SimulationObjects.Values
            If TypeOf obj Is ISimulationObject Then
                DirectCast(obj, ISimulationObject).PropertyPackage = Nothing
            ElseIf TypeOf obj Is IMaterialStream Then
                DirectCast(obj, ISimulationObject).PropertyPackage = Nothing
            End If
        Next

        Return loopex.ToList

    End Function

    ''' <summary>
    ''' Checks the calculator status to see if the user did any stop/abort request, and throws an exception to force aborting, if necessary.
    ''' </summary>
    ''' <remarks></remarks>
    Public Shared Sub CheckCalculatorStatus()
        If Not Settings.CAPEOPENMode Then
            If Settings.CalculatorStopRequested = True Then
                If Settings.TaskCancellationTokenSource IsNot Nothing Then
                    If Not Settings.TaskCancellationTokenSource.IsCancellationRequested Then
                        Settings.TaskCancellationTokenSource.Cancel()
                    End If
                    Settings.TaskCancellationTokenSource.Token.ThrowIfCancellationRequested()
                Else
                    Throw New Exception("Calculation Aborted")
                End If
            End If
            Settings.TaskCancellationTokenSource?.Token.ThrowIfCancellationRequested()
        End If
    End Sub

    ''' <summary>
    ''' This routine updates the display status of a list of graphic objects in the flowsheet according to their calculated status.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object).</param>
    ''' <param name="ObjIDlist">List of object IDs to be updated.</param>
    ''' <param name="calculating">Tell the routine that the objects in the list are being calculated at the moment.</param>
    ''' <remarks></remarks>
    Shared Sub UpdateDisplayStatus(fobj As Object, Optional ByVal ObjIDlist() As String = Nothing, Optional ByVal calculating As Boolean = False)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        If ObjIDlist Is Nothing Then
            For Each baseobj In fbag.SimulationObjects.Values
                If Not baseobj.GraphicObject Is Nothing Then
                    If Not baseobj.GraphicObject.Active Then
                        baseobj.GraphicObject.Status = Status.Inactive
                    Else
                        baseobj.GraphicObject.Calculated = baseobj.Calculated
                        'If baseobj.Calculated Then baseobj.UpdatePropertyNodes(fobj.Options.SelectedUnitSystem, fobj.Options.NumberFormat)
                    End If
                End If
            Next
        Else
            For Each ObjID In ObjIDlist
                If fbag.SimulationObjects.ContainsKey(ObjID) Then
                    Dim baseobj = fbag.SimulationObjects(ObjID)
                    If Not baseobj.GraphicObject Is Nothing Then
                        If calculating Then
                            baseobj.GraphicObject.Status = Status.Calculating
                        Else
                            If Not baseobj.GraphicObject.Active Then
                                baseobj.GraphicObject.Status = Status.Inactive
                            Else
                                baseobj.GraphicObject.Calculated = baseobj.Calculated
                                'If baseobj.Calculated Then baseobj.UpdatePropertyNodes(fobj.Options.SelectedUnitSystem, fobj.Options.NumberFormat)
                            End If
                        End If
                    End If
                End If
            Next
        End If
        'fobj.UIThread(Sub()
        '                  fobj.FormSurface.FlowsheetDesignSurface.Invalidate()
        '              End Sub)

    End Sub

    ''' <summary>
    ''' Retrieves the list of objects to be solved in the flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormChild object)</param>
    ''' <param name="frompgrid">Starts the search from the edited object if the propert was changed from the property grid.</param>
    ''' <returns>A list of objects to be calculated in the flowsheet.</returns>
    ''' <remarks></remarks>
    Public Shared Function GetSolvingList(fobj As Object, frompgrid As Boolean) As Object()

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        Dim obj As ISimulationObject

        Dim lists As New Dictionary(Of Integer, List(Of String))
        Dim filteredlist As New Dictionary(Of Integer, List(Of String))
        Dim objstack As New List(Of String)

        Dim onqueue As CalculationArgs = Nothing

        Dim listidx As Integer = 0
        Dim maxidx As Integer = 0

        If frompgrid Then

            If fqueue.CalculationQueue.Count > 0 Then

                onqueue = fqueue.CalculationQueue.Dequeue()
                fqueue.CalculationQueue.Clear()

                lists.Add(0, New List(Of String))

                lists(0).Add(onqueue.Name)

                'now start walking through the flowsheet until it reaches its end starting from this particular object.

                Do
                    listidx += 1
                    If lists(listidx - 1).Count > 0 Then
                        lists.Add(listidx, New List(Of String))
                        maxidx = listidx
                        For Each o As String In lists(listidx - 1)
                            obj = fbag.SimulationObjects(o)
                            If obj.GraphicObject.Active Then
                                For Each c As IConnectionPoint In obj.GraphicObject.OutputConnectors
                                    If c.IsAttached Then
                                        If obj.GraphicObject.ObjectType = ObjectType.OT_Recycle Or obj.GraphicObject.ObjectType = ObjectType.OT_EnergyRecycle Then Exit For
                                        lists(listidx).Add(c.AttachedConnector.AttachedTo.Name)
                                    End If
                                Next
                                If obj.GraphicObject.EnergyConnector.IsAttached AndAlso obj.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo IsNot obj Then
                                    lists(listidx).Add(obj.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Name)
                                End If
                            End If
                        Next
                    Else
                        Exit Do
                    End If
                    If lists.Count > 10000 Then
                        lists.Clear()
                        GlobalSettings.Settings.CalculatorBusy = False
                        Throw New Exception("Infinite loop detected while obtaining flowsheet object calculation order. Please insert recycle blocks where needed.")
                    End If
                Loop

                'process the lists , adding objects to the stack, discarding duplicate entries.

                listidx = 0

                Do
                    If lists.ContainsKey(listidx) Then
                        filteredlist.Add(listidx, New List(Of String)(lists(listidx).ToArray))
                        For Each o As String In lists(listidx)
                            objstack.Add(o)
                        Next
                    Else
                        Exit Do
                    End If
                    listidx += 1
                Loop Until listidx > maxidx

                objstack.Reverse()
                objstack = objstack.Distinct().ToList()
                objstack.Reverse()

            End If

        Else

            'add endpoint material streams and recycle ops to the list, they will be the last objects to be calculated.

            lists.Add(0, New List(Of String))

            For Each baseobj In fbag.SimulationObjects.Values
                If baseobj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                    If baseobj.GraphicObject.OutputConnectors(0).IsAttached = False Then
                        lists(0).Add(baseobj.Name)
                    End If
                ElseIf baseobj.GraphicObject.ObjectType = ObjectType.EnergyStream Then
                    If baseobj.GraphicObject.OutputConnectors(0).IsAttached = False Then
                        lists(0).Add(baseobj.Name)
                    End If
                ElseIf baseobj.GraphicObject.ObjectType = ObjectType.OT_Recycle Then
                    lists(0).Add(baseobj.Name)
                ElseIf baseobj.GraphicObject.ObjectType = ObjectType.OT_EnergyRecycle Then
                    lists(0).Add(baseobj.Name)
                ElseIf baseobj.IsSource Then
                    lists(0).Add(baseobj.Name)
                End If
            Next

            'now start processing the list at each level, until it reaches the beginning of the flowsheet.

            Dim totalobjs As Integer = 0

            Do
                listidx += 1
                If lists(listidx - 1).Count > 0 Then
                    lists.Add(listidx, New List(Of String))
                    maxidx = listidx
                    For Each o As String In lists(listidx - 1)
                        If fbag.SimulationObjects.ContainsKey(o) Then
                            obj = fbag.SimulationObjects(o)
                            If Not onqueue Is Nothing Then
                                If onqueue.Name = obj.Name Then Exit Do
                            End If
                            For Each c As IConnectionPoint In obj.GraphicObject.InputConnectors
                                If c.IsAttached Then
                                    If c.AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_Recycle And
                                        c.AttachedConnector.AttachedFrom.ObjectType <> ObjectType.OT_EnergyRecycle Then
                                        lists(listidx).Add(c.AttachedConnector.AttachedFrom.Name)
                                        totalobjs += 1
                                        If totalobjs > 10000 Then
                                            Throw New Exception("Infinite loop detected while obtaining flowsheet object calculation order. Please insert recycle blocks where needed.")
                                        End If
                                    End If
                                End If
                            Next
                        End If
                    Next
                Else
                    Exit Do
                End If
            Loop

            'process the lists backwards, adding objects to the stack, discarding duplicate entries.

            listidx = maxidx

            Do
                If lists.ContainsKey(listidx) Then
                    filteredlist.Add(maxidx - listidx, New List(Of String)(lists(listidx).ToArray))
                    For Each o As String In lists(listidx)
                        If Not objstack.Contains(o) Then
                            objstack.Add(o)
                        Else
                            filteredlist(maxidx - listidx).Remove(o)
                        End If
                    Next
                Else
                    Exit Do
                End If
                listidx -= 1
            Loop

        End If

        Return New Object() {objstack, lists, filteredlist}

    End Function

    ''' <summary>
    ''' Calculate all objects in the Flowsheet using an ordering method.
    ''' </summary>
    ''' <param name="fobj">Flowsheet to be calculated (FormFlowsheet object).</param>
    ''' <param name="Adjusting">True if the routine is called from the Simultaneous Adjust Solver.</param>
    ''' <param name="frompgrid">True if the routine is called from a PropertyGrid PropertyChanged event.</param>
    ''' <param name="mode">0 = Main Thread, 1 = Background Thread, 2 = Background Parallel Threads, 3 = Azure Service Bus, 4 = Network Computer</param>
    ''' <param name="ts">CancellationTokenSource instance from main flowsheet when calculating subflowsheets.</param>
    ''' <remarks></remarks>
    Public Shared Function SolveFlowsheet(ByVal fobj As Object, mode As Integer, Optional ByVal ts As CancellationTokenSource = Nothing,
                                          Optional frompgrid As Boolean = False, Optional Adjusting As Boolean = False,
                                          Optional ByVal FinishSuccess As Action = Nothing,
                                          Optional ByVal FinishWithErrors As Action = Nothing,
                                          Optional ByVal FinishAny As Action = Nothing,
                                          Optional ByVal ChangeCalcOrder As Boolean = False) As List(Of Exception)

        GlobalSettings.Settings.LockModelParameters = True

        If GlobalSettings.Settings.CalculatorActivated Then

            Dim fs As IFlowsheet = TryCast(fobj, IFlowsheet)

            If fs.MasterFlowsheet Is Nothing And GlobalSettings.Settings.CalculatorBusy And Not Adjusting Then
                FinishAny?.Invoke()
                GlobalSettings.Settings.CalculatorBusy = False
                Return New List(Of Exception)
            End If

            Inspector.Host.CurrentSolutionID = Date.Now.ToBinary

            If GlobalSettings.Settings.InspectorEnabled Then
                GlobalSettings.Settings.EnableParallelProcessing = False
                mode = 1
            End If

            'clears any previous calculation stop request.

            Settings.CalculatorStopRequested = False

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "SolveFlowsheet", "Solver Call", "Flowsheet Solver Call Event")

            IObj?.Paragraphs.Add("The Flowsheet Solver controls the calculation of the entire flowsheet.")

            IObj?.Paragraphs.Add("When the user requests a flowsheet calculation, it tries to determine the order of the objects to be calculated.")

            If fs.PropertyPackages.Count = 0 Then
                Dim el = New List(Of Exception)
                el.Add(New Exception(fs.GetTranslatedString("NoPropPackAdded")))
                fs.ShowMessage(fs.GetTranslatedString("NoPropPackAdded"), IFlowsheet.MessageType.GeneralError)
                Return el
            End If

            If fs.SelectedCompounds.Count = 0 Then
                Dim el = New List(Of Exception)
                el.Add(New Exception(fs.GetTranslatedString("NoCompoundsAdded")))
                fs.ShowMessage(fs.GetTranslatedString("NoCompoundsAdded"), IFlowsheet.MessageType.GeneralError)
                Return el
            End If

            If Not fs Is Nothing Then
                If fs.MasterFlowsheet Is Nothing And Not Adjusting And GlobalSettings.Settings.CalculatorBusy Then
                    FinishAny?.Invoke()
                    GlobalSettings.Settings.CalculatorBusy = False
                    Return New List(Of Exception)
                End If
            End If

            Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
            Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
            Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

            'checks if the calculator is activated.

            If fs.MasterFlowsheet Is Nothing Then GlobalSettings.Settings.CalculatorBusy = True

            'this is the cancellation token for background threads. it checks for calculator stop requests and forwards the request to the tasks.

            If fs IsNot Nothing AndAlso fs.MasterFlowsheet Is Nothing Then
                If ts Is Nothing Then ts = New CancellationTokenSource
                Settings.TaskCancellationTokenSource = ts
            End If

            Dim obj As ISimulationObject

            'mode:
            '0 = Synchronous (main thread)
            '1 = Asynchronous (background thread)
            '2 = Asynchronous Parallel (background thread)
            '3 = Azure Service Bus
            '4 = Network Computer

            Dim d1 As Date = Date.Now
            Dim age As AggregateException = Nothing
            Dim exlist As New List(Of Exception)

            'gets a list of objects to be solved in the flowsheet

            Dim objl As Object()
            Try
                objl = GetSolvingList(fobj, frompgrid)
            Catch ex As Exception
                FinishAny?.Invoke()
                GlobalSettings.Settings.CalculatorBusy = False
                Return New List(Of Exception)({ex})
            End Try

            'assign the list of objects, the filtered list (which contains no duplicate elements) and the object stack
            'which contains the ordered list of objects to be calculated.

            Dim objstack As List(Of String) = objl(0)

            If ChangeCalcOrder Then
                If mode = 0 Or mode = 1 Then
                    fgui.RunCodeOnUIThread(Sub()
                                               Dim customlist = fgui.FlowsheetOptions.CustomCalculationOrder
                                               Dim reflist = New List(Of String)(customlist)
                                               If customlist.Count > 0 Then
                                                   For Each item In reflist
                                                       If Not objstack.Contains(item) Then
                                                           customlist.Remove(item)
                                                       End If
                                                   Next
                                                   For Each item In objstack
                                                       If Not customlist.Contains(item) Then
                                                           customlist.Add(item)
                                                       End If
                                                   Next
                                                   objstack = fgui.ChangeCalculationOrder(customlist)
                                               Else
                                                   objstack = fgui.ChangeCalculationOrder(objstack)
                                               End If
                                               fgui.FlowsheetOptions.CustomCalculationOrder = New List(Of String)(objstack)
                                           End Sub)
                End If
            End If

            IObj?.Paragraphs.Add("The objects which will be calculated are (in this order): ")

            If IObj IsNot Nothing Then
                For Each item In objstack
                    IObj.Paragraphs.Add(fbag.SimulationObjects(item).GraphicObject.Tag & " (" & fbag.SimulationObjects(item).GetDisplayName & ")")
                Next
            End If

            If objstack.Count = 0 Then
                Settings.CalculatorBusy = False
                FinishAny?.Invoke()
                Return New List(Of Exception)
            End If

            fs.Solved = False
            fs.ErrorMessage = ""

            'adds a message to the log window to indicate that the flowsheet started solving

            fgui.ShowMessage(fgui.GetTranslatedString("FSstartedsolving"), IFlowsheet.MessageType.Information)

            If fgui.FlowsheetOptions.ForceStreamPhase <> ForcedPhase.None Then
                fgui.ShowMessage(fgui.GetTranslatedString(String.Format("Global Phase Override is defined to '{0}'",
                                                          fgui.FlowsheetOptions.ForceStreamPhase)), IFlowsheet.MessageType.Warning)
            End If

            'process scripts associated with the solverstarted event

            fgui.ProcessScripts(Scripts.EventType.SolverStarted, Scripts.ObjectType.Solver, "")

            'call spreadsheet update to get values when in write mode

            fs.UpdateSpreadsheet(Nothing)
            fs.WriteSpreadsheetVariables(Nothing)

            RaiseEvent FlowsheetCalculationStarted(fobj, New System.EventArgs(), Nothing)

            'find recycles

            IObj?.Paragraphs.Add("The solver will now check for Recycles connected to 'tear' Material Streams...")

            Dim recycles As New List(Of String)
            Dim totalv As Integer = 0
            Dim totalr As Integer = 0

            For Each r In objstack
                If fbag.SimulationObjects.ContainsKey(r) Then
                    Dim robj = fbag.SimulationObjects(r)
                    If robj.GraphicObject.ObjectType = ObjectType.OT_Recycle Then
                        recycles.Add(robj.Name)
                        Dim rec As IRecycle = fbag.SimulationObjects(robj.Name)
                        If rec.AccelerationMethod = AccelMethod.GlobalBroyden Then
                            If rec.Values.Count = 0 Then fbag.SimulationObjects(robj.Name).Solve()
                            totalv += rec.Values.Count
                        End If
                        totalr += 1
                    End If
                End If
            Next

            IObj?.Paragraphs.Add(String.Format("Number of Recycles found: {0}.", totalr))

            'size hessian matrix, variables and error vectors for recycle simultaneous solving.

            Dim rechess(totalv - 1, totalv - 1), recvars(totalv - 1), recdvars(totalv - 1), recerrs(totalv - 1), recvarsb(totalv - 1), recerrsb(totalv - 1) As Double

            'identity matrix as first hessian.

            For i As Integer = 0 To totalv - 1
                rechess(i, i) = 1
            Next

            'initialize GPU if option enabled

            If Settings.EnableGPUProcessing Then Settings.gpu.EnableMultithreading()

            Dim maintask As Task

            Select Case mode

                Case 0, 1, 2

                    If mode = 0 Then mode = 1

                    '0 = main thread, 1 = bg thread, 2 = bg parallel threads

                    'define variable to check for flowsheet convergence if there are recycle ops

                    Dim converged As Boolean = False

                    Dim loopidx As Integer = 0

                    'process/calculate the queue.

                    If fqueue.CalculationQueue Is Nothing Then fqueue.CalculationQueue = New Queue(Of ICalculationArgs)

                    'My.Application.MasterCalculatorStopRequested = False

                    Dim objargs As CalculationArgs = Nothing

                    maintask = TaskHelper.Run(Sub()

                                                  Dim icount As Integer = 0

                                                  While Not converged

                                                      fgui.ClearLog()

                                                      'calc specs

                                                      If fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.BeforeFlowsheet Then
                                                          For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                                                              Dim spec = DirectCast(obj, ISpec)
                                                              If spec.SpecCalculationMode = SpecCalcMode2.GlobalSetting Or spec.SpecCalculationMode = SpecCalcMode2.BeforeFlowsheet Then
                                                                  obj.Solve()
                                                              End If
                                                          Next
                                                      End If

                                                      'add the objects to the calculation queue.

                                                      For Each o As String In objstack
                                                          If fbag.SimulationObjects.ContainsKey(o) Then
                                                              obj = fbag.SimulationObjects(o)
                                                              If obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                                                                  Dim ms As IMaterialStream = fbag.SimulationObjects(obj.Name)
                                                                  ms.AtEquilibrium = False
                                                              End If
                                                              objargs = New CalculationArgs
                                                              With objargs
                                                                  .Sender = "FlowsheetSolver"
                                                                  .Calculated = True
                                                                  .Name = obj.Name
                                                                  .ObjectType = obj.GraphicObject.ObjectType
                                                                  .Tag = obj.GraphicObject.Tag
                                                                  fqueue.CalculationQueue.Enqueue(objargs)
                                                              End With
                                                          End If
                                                      Next

                                                      'set the flowsheet instance for all objects, this is required for the async threads

                                                      For Each o In fbag.SimulationObjects.Values
                                                          o.SetFlowsheet(fobj)
                                                      Next

                                                      'set all objects' status to 'not calculated' (red) in the list

                                                      For Each o In objstack
                                                          obj = fbag.SimulationObjects(o)
                                                          With obj
                                                              .Calculated = False
                                                              If Not obj.GraphicObject Is Nothing Then
                                                                  If obj.GraphicObject.Active Then
                                                                      obj.GraphicObject.Calculated = False
                                                                  Else
                                                                      fgui.ShowMessage(obj.GraphicObject.Tag & ": " & fgui.GetTranslatedString("ObjDeactivated"), IFlowsheet.MessageType.Warning)
                                                                      obj.GraphicObject.Status = Status.Inactive
                                                                  End If
                                                              End If
                                                          End With
                                                      Next

                                                      exlist = ProcessCalculationQueue(fobj, Settings.TaskCancellationTokenSource.Token, Adjusting)

                                                      'calc specs

                                                      If fbag.FlowsheetOptions.SpecCalculationMode = SpecCalcMode.AfterFlowsheet Then

                                                          For Each obj In fbag.SimulationObjects.Values.Where(Function(o) TypeOf o Is ISpec)
                                                              Dim spec = DirectCast(obj, ISpec)
                                                              If spec.SpecCalculationMode = SpecCalcMode2.GlobalSetting Or spec.SpecCalculationMode = SpecCalcMode2.AfterFlowsheet Then
                                                                  obj.Solve()
                                                              End If
                                                          Next

                                                          'calc again

                                                          'add the objects to the calculation queue.

                                                          For Each o As String In objstack
                                                              If fbag.SimulationObjects.ContainsKey(o) Then
                                                                  obj = fbag.SimulationObjects(o)
                                                                  If obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                                                                      Dim ms As IMaterialStream = fbag.SimulationObjects(obj.Name)
                                                                      ms.AtEquilibrium = False
                                                                  End If
                                                                  objargs = New CalculationArgs
                                                                  With objargs
                                                                      .Sender = "FlowsheetSolver"
                                                                      .Calculated = True
                                                                      .Name = obj.Name
                                                                      .ObjectType = obj.GraphicObject.ObjectType
                                                                      .Tag = obj.GraphicObject.Tag
                                                                      fqueue.CalculationQueue.Enqueue(objargs)
                                                                  End With
                                                              End If
                                                          Next

                                                          exlist = ProcessCalculationQueue(fobj, Settings.TaskCancellationTokenSource.Token, Adjusting)

                                                      End If

                                                      'throws exceptions if any

                                                      If Settings.SolverBreakOnException And exlist.Count > 0 Then
                                                          Throw New AggregateException(exlist)
                                                      End If

                                                      'checks for recycle convergence.

                                                      converged = True
                                                      For Each r As String In recycles
                                                          obj = fbag.SimulationObjects(r)
                                                          converged = DirectCast(obj, IRecycle).Converged
                                                          If Not converged Then Exit For
                                                      Next

                                                      'in dynamic mode, recycles are redundant

                                                      If fbag.DynamicMode Then converged = True

                                                      If Not converged Then

                                                          Dim avgerr As Double = 0.0#
                                                          Dim rcount As Integer = 0

                                                          For Each r As String In recycles
                                                              obj = fbag.SimulationObjects(r)
                                                              With DirectCast(obj, IRecycle)
                                                                  avgerr += 0.33 * .ConvergenceHistory.TemperaturaE / .ConvergenceHistory.Temperatura
                                                                  avgerr += 0.33 * .ConvergenceHistory.PressaoE / .ConvergenceHistory.Pressao
                                                                  avgerr += 0.33 * .ConvergenceHistory.VazaoMassicaE / .ConvergenceHistory.VazaoMassica
                                                              End With
                                                              rcount += 1
                                                          Next

                                                          avgerr *= 100
                                                          avgerr /= rcount

                                                          fgui.ClearLog()

                                                          fgui.ShowMessage("Recycle loop #" & (icount + 1) & ", average recycle error: " & Format(avgerr, "N") & "%", IFlowsheet.MessageType.Information)

                                                          fgui.UpdateInterface()

                                                          fgui.UpdateOpenEditForms()

                                                      End If

                                                      'process the scripts associated with the recycle loop event.

                                                      fgui.ProcessScripts(Scripts.EventType.SolverRecycleLoop, Scripts.ObjectType.Solver, "")

                                                      'if the all recycles have converged (if any), then exit the loop.

                                                      If converged Then

                                                          Exit While

                                                      Else

                                                          If totalv > 0 Then

                                                              'update variables of all recycles set to global broyden.

                                                              Dim i As Integer = 0
                                                              For Each r As String In recycles
                                                                  Dim rec = DirectCast(fbag.SimulationObjects(r), IRecycle)
                                                                  If rec.AccelerationMethod = AccelMethod.GlobalBroyden Then
                                                                      For Each kvp In rec.Values
                                                                          recvars(i) = kvp.Value
                                                                          recerrs(i) = rec.Errors(kvp.Key)
                                                                          i += 1
                                                                      Next
                                                                  End If
                                                              Next

                                                              MathEx.Broyden.broydn(totalv - 1, recvars, recerrs, recdvars, recvarsb, recerrsb, rechess, If(icount < 2, 0, 1))

                                                              i = 0
                                                              For Each r As String In recycles
                                                                  Dim rec = DirectCast(fbag.SimulationObjects(r), IRecycle)
                                                                  If rec.AccelerationMethod = AccelMethod.GlobalBroyden Then
                                                                      For Each kvp In rec.Errors
                                                                          rec.Values(kvp.Key) = recvars(i) + 0.7 * recdvars(i)
                                                                          i += 1
                                                                      Next
                                                                  End If
                                                                  rec.SetOutletStreamProperties()
                                                              Next

                                                          End If

                                                      End If

                                                      If frompgrid Then
                                                          Try
                                                              objl = GetSolvingList(fobj, False)
                                                              objstack = objl(0)
                                                          Catch ex As Exception
                                                              GlobalSettings.Settings.CalculatorBusy = False
                                                              Throw ex
                                                          End Try
                                                      End If

                                                      icount += 1

                                                  End While

                                              End Sub, Settings.TaskCancellationTokenSource.Token)

                    Try
                        While Not (Date.Now - d1).TotalMilliseconds >= Settings.SolverTimeoutSeconds * 1000
                            maintask.Wait(500)
                            fgui.UpdateInterface()
                            If Settings.TaskCancellationTokenSource.IsCancellationRequested Then
                                Throw New OperationCanceledException("Flowsheet solving stopped by the user.")
                            End If
                            If maintask.Status = TaskStatus.RanToCompletion Then Exit While
                        End While
                        fgui.UpdateInterface()
                        If maintask.Status = TaskStatus.Running Then
                            If GlobalSettings.Settings.IsRunningOnMono() Then
                                Throw New OperationCanceledException(fgui.GetTranslatedString("SolverTimeout"))
                            Else
                                Throw New TimeoutException(fgui.GetTranslatedString("SolverTimeout"))
                            End If
                        End If
                        If maintask.IsFaulted Then Throw maintask.Exception
                        If exlist.Count > 0 Then Throw New AggregateException(exlist)
                    Catch agex As AggregateException
                        age = agex
                    Catch ex As OperationCanceledException
                        age = New AggregateException(fgui.GetTranslatedString("CalculationAborted"), ex)
                    Catch ex As Exception
                        age = New AggregateException(ex.Message.ToString, ex)
                    Finally
                        If maintask.IsCompleted Then
                            maintask.Dispose()
                            maintask = Nothing
                        End If
                    End Try

                    'clears the calculation queue.

                    fqueue.CalculationQueue.Clear()

                    'clears the object lists.

                    objstack.Clear()
                    recycles.Clear()

                    'Case 3

                    '    'Azure Service Bus

                    '    Dim azureclient As New AzureSolverClient()

                    '    Try
                    '        azureclient.SolveFlowsheet(fobj)
                    '        For Each baseobj In fbag.SimulationObjects.Values
                    '            If baseobj.Calculated Then baseobj.LastUpdated = Date.Now
                    '        Next
                    '    Catch ex As Exception
                    '        age = New AggregateException(ex.Message.ToString, ex)
                    '    Finally
                    '        If Not azureclient.qcc.IsClosed Then azureclient.qcc.Close()
                    '        If Not azureclient.qcs.IsClosed Then azureclient.qcs.Close()
                    '    End Try

                    '    azureclient = Nothing

                    'Case 4

                    '    'TCP/IP Solver

                    '    Dim tcpclient As New TCPSolverClient()

                    '    Try
                    '        tcpclient.SolveFlowsheet(fobj)
                    '        For Each baseobj In fbag.SimulationObjects.Values
                    '            If baseobj.Calculated Then baseobj.LastUpdated = Date.Now
                    '        Next
                    '    Catch ex As Exception
                    '        age = New AggregateException(ex.Message.ToString, ex)
                    '    Finally
                    '        tcpclient.client.Close()
                    '    End Try

                    '    tcpclient = Nothing

            End Select

            'clears any calculation stop request.

            Settings.CalculatorStopRequested = False

            'Frees GPU memory if enabled.

            If Settings.EnableGPUProcessing Then
                Settings.gpu.DisableMultithreading()
                Settings.gpu.FreeAll()
            End If

            'updates the display status of all objects in the calculation list.

            UpdateDisplayStatus(fobj, objstack.ToArray)

            'disposes the cancellation token source.

            If fs.MasterFlowsheet Is Nothing Then ts?.Dispose()

            GlobalSettings.Settings.TaskCancellationTokenSource = Nothing

            'checks if exceptions were thrown during the calculation and displays them in the log window.

            If age Is Nothing Then

                fgui.ShowMessage(fgui.GetTranslatedString("FSfinishedsolvingok") + " [" & (Date.Now - d1).TotalSeconds.ToString("G4") + "s]", IFlowsheet.MessageType.Information)

                IObj?.Paragraphs.Add(String.Format("Solver finished calculation of all objects in {0} seconds.", (Date.Now - d1).TotalSeconds))

                fs.ErrorMessage = ""
                fs.Solved = True

            Else

                Dim baseexception As Exception = Nothing

                fgui.ShowMessage(fgui.GetTranslatedString("FSfinishedsolvingerror"), IFlowsheet.MessageType.GeneralError)

                IObj?.Paragraphs.Add(fgui.GetTranslatedString("FSfinishedsolvingerror"))

                For Each ex In age.Flatten().InnerExceptions
                    Dim euid As String = Guid.NewGuid().ToString()
                    SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, ex)
                    If TypeOf ex Is AggregateException Then
                        baseexception = ex.InnerException
                        For Each iex In DirectCast(ex, AggregateException).Flatten().InnerExceptions
                            While iex.InnerException IsNot Nothing
                                baseexception = iex.InnerException
                            End While
                        Next
                    Else
                        baseexception = ex
                        While baseexception.InnerException IsNot Nothing
                            baseexception = baseexception.InnerException
                        End While
                    End If
                    Dim message = baseexception.Message
                    If baseexception.Source <> "" Then
                        message = String.Format("Error in '{0}': {1}", baseexception.Source, baseexception.Message)
                    End If
                    Try
                        Dim st As New StackTrace(baseexception, True)
                        Dim frame As StackFrame = st.GetFrame(0)
                        Dim line = frame.GetFileLineNumber().ToString()
                        Dim dirName = New DirectoryInfo(frame.GetFileName).Name
                        message += " (" + dirName + ", " + line + ")"
                    Catch exs As Exception
                    End Try
                    fgui.ShowMessage(message, IFlowsheet.MessageType.GeneralError, euid)
                    'Console.WriteLine(baseexception.ToString)
                    IObj?.Paragraphs.Add(baseexception.Message)
                Next

                'fgui.ShowMessage(fgui.GetTranslatedString("If Anonymous Analytics Sharing is enabled, the developers will be notified shortly about the errors. Thank you for your contribution!"), IFlowsheet.MessageType.GeneralError)

                fs.Solved = False
                If baseexception IsNot Nothing Then fs.ErrorMessage = baseexception.ToString

            End If

            'updates the flowsheet display information if the fobj is visible.

            fs.UpdateSpreadsheet(Nothing)
            fs.UpdateSpreadsheet(Nothing)

            fgui.UpdateInformation()

            fgui.UpdateInterface()

            fgui.RefreshInterface()

            fgui.ProcessScripts(Scripts.EventType.SolverFinished, Scripts.ObjectType.Solver, "")

            GlobalSettings.Settings.CalculatorBusy = False

            IObj?.Close()

            GlobalSettings.Settings.LockModelParameters = False

            FinishAny?.Invoke()

            If age Is Nothing Then
                FinishSuccess?.Invoke()
                RaiseEvent FlowsheetCalculationFinished(fobj, New System.EventArgs(), (Date.Now - d1).TotalSeconds)
                Return New List(Of Exception)
            Else
                FinishWithErrors?.Invoke()
                RaiseEvent FlowsheetCalculationFinished(fobj, New System.EventArgs(), age.InnerExceptions.ToList())
                Return age.Flatten().InnerExceptions.ToList()
            End If

        Else

            GlobalSettings.Settings.LockModelParameters = False

            FinishAny?.Invoke()

            Return New List(Of Exception)

        End If

    End Function

    ''' <summary>
    ''' Calculates a single object in the Flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
    ''' <remarks></remarks>
    Public Shared Function CalculateObject(ByVal fobj As Object, ByVal ObjID As String) As List(Of Exception)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        If fbag.SimulationObjects.ContainsKey(ObjID) Then

            Dim baseobj = fbag.SimulationObjects(ObjID)

            Dim objargs As New CalculationArgs
            With objargs
                .Calculated = True
                .Name = baseobj.Name
                .ObjectType = baseobj.GraphicObject.ObjectType
                .Tag = baseobj.GraphicObject.Tag
            End With

            fqueue.CalculationQueue.Enqueue(objargs)

            'Return SolveFlowsheet(fobj, Settings.SolverMode, Nothing, False, False, Nothing, Nothing, Nothing)
            Return SolveFlowsheet(fobj, Settings.SolverMode, Nothing, True)

        Else

            Return New List(Of Exception)

        End If

    End Function

    ''' <summary>
    ''' Calculates a single object in the Flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateObjectSync(ByVal fobj As Object, ByVal ObjID As String)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        If fbag.SimulationObjects.ContainsKey(ObjID) Then

            Dim baseobj = fbag.SimulationObjects(ObjID)

            If baseobj.GraphicObject.ObjectType = ObjectType.MaterialStream Then

                If baseobj.GraphicObject.InputConnectors(0).IsAttached = False Then
                    'add this stream to the calculator queue list
                    Dim objargs As New CalculationArgs
                    With objargs
                        .Calculated = True
                        .Name = baseobj.Name
                        .ObjectType = ObjectType.MaterialStream
                        .Tag = baseobj.GraphicObject.Tag
                    End With
                    If baseobj.IsSpecAttached = True And baseobj.SpecVarType = SpecVarType.Source Then
                        fbag.SimulationObjects(baseobj.AttachedSpecId).Solve()
                    End If
                    fqueue.CalculationQueue.Enqueue(objargs)
                    ProcessQueueInternal(fobj)
                Else
                    If baseobj.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_Recycle Then
                        'add this stream to the calculator queue list
                        Dim objargs As New CalculationArgs
                        With objargs
                            .Calculated = True
                            .Name = baseobj.Name
                            .ObjectType = ObjectType.MaterialStream
                            .Tag = baseobj.GraphicObject.Tag
                        End With
                        If baseobj.IsSpecAttached = True And baseobj.SpecVarType = SpecVarType.Source Then
                            fbag.SimulationObjects(baseobj.AttachedSpecId).Solve()
                        End If
                        fqueue.CalculationQueue.Enqueue(objargs)
                        ProcessQueueInternal(fobj)
                    End If
                End If
            Else
                Dim unit As ISimulationObject = baseobj
                Dim objargs As New CalculationArgs
                With objargs
                    .Sender = "PropertyGrid"
                    .Calculated = True
                    .Name = unit.Name
                    .ObjectType = unit.GraphicObject.ObjectType
                    .Tag = unit.GraphicObject.Tag
                End With
                fqueue.CalculationQueue.Enqueue(objargs)
                ProcessQueueInternal(fobj)
            End If

        End If

    End Sub

    ''' <summary>
    ''' Calculates a single object in the Flowsheet.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks></remarks>
    Public Shared Sub CalculateObjectAsync(ByVal fobj As Object, ByVal ObjID As String, ByVal ct As CancellationToken)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)

        If fbag.SimulationObjects.ContainsKey(ObjID) Then

            Dim baseobj = fbag.SimulationObjects(ObjID)

            Dim objargs As New CalculationArgs
            With objargs
                .Sender = "PropertyGrid"
                .Calculated = True
                .Name = baseobj.Name
                .ObjectType = ObjectType.MaterialStream
                .Tag = baseobj.GraphicObject.Tag
            End With
            fqueue.CalculationQueue.Enqueue(objargs)

            Dim objl = GetSolvingList(fobj, True)

            Dim objstack As List(Of String) = objl(0)

            For Each o As String In objstack
                Dim obj = fbag.SimulationObjects(o)
                objargs = New CalculationArgs
                With objargs
                    .Sender = "FlowsheetSolver"
                    .Calculated = True
                    .Name = obj.Name
                    .ObjectType = obj.GraphicObject.ObjectType
                    .Tag = obj.GraphicObject.Tag
                    fqueue.CalculationQueue.Enqueue(objargs)
                End With
            Next

            For Each o In fbag.SimulationObjects.Values
                o.SetFlowsheet(fobj)
            Next

            ProcessQueueInternalAsync(fobj, ct)

        End If

    End Sub

    ''' <summary>
    ''' Simultaneous adjust solver routine.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <remarks>Solves all marked Adjust objects in the flowsheet simultaneously using Netwon's method.</remarks>
    Private Shared Sub SolveSimultaneousAdjusts(ByVal fobj As Object)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)
        Dim fs As IFlowsheet = TryCast(fobj, IFlowsheet)

        If fs.FlowsheetOptions.SimultaneousAdjustSolverEnabled Then

            Try

                Dim n As Integer = 0

                For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
                    If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then n += 1
                Next

                If n > 0 Then

                    n -= 1

                    Dim i As Integer = 0
                    Dim dfdx(n, n), dx(n), fx(n), x(n), tols(n) As Double
                    Dim il_err_ant As Double = 10000000000.0
                    Dim il_err As Double = 10000000000.0
                    Dim ic As Integer
                    Dim converged As Boolean = False

                    i = 0
                    For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
                        If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                            x(i) = GetMnpVarValue(fobj, adj)
                            tols(i) = adj.Tolerance
                            i += 1
                        End If
                    Next

                    ic = 0
                    Do

                        fx = FunctionValueSync(fobj, x)

                        converged = False

                        il_err_ant = il_err
                        il_err = fx.AbsSqrSumY
                        For i = 0 To x.Length - 1
                            If Math.Abs(fx(i)) < tols(i) Then
                                converged = True
                            Else
                                converged = False
                                Exit For
                            End If
                        Next

                        fgui.ShowMessage(fgui.GetTranslatedString("SimultaneousAdjust") & ": Iteration #" & ic + 1 & ", NSSE: " & il_err, IFlowsheet.MessageType.Information)

                        If converged Then Exit Do

                        dfdx = FunctionGradientSync(fobj, x)

                        Dim dfac As Double = 100000.0
                        Dim success = MathEx.SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)
                        If success Then

                            dfac = (ic + 1) * 0.2
                            If dfac > 1.0 Then dfac = 1.0

                            For i = 0 To x.Length - 1
                                If Math.Abs(-dx(i) * dfac) > x(i) Then
                                    dfac /= 10
                                    Exit For
                                End If
                            Next

                            For i = 0 To x.Length - 1
                                dx(i) = -dx(i)
                                x(i) += dfac * dx(i)
                            Next

                        End If

                        ic += 1

                        fs.CheckStatus()
                        fgui.UpdateInterface()

                        If ic >= 25 Then Throw New Exception(fgui.GetTranslatedString("SADJMaxIterationsReached"))
                        If Double.IsNaN(il_err) Then Throw New Exception(fgui.GetTranslatedString("SADJGeneralError"))
                        If Math.Abs(MathEx.Common.AbsSum(dx)) < 0.000001 Then Exit Do

                    Loop

                End If

            Catch ex As Exception
                fgui.ShowMessage(fgui.GetTranslatedString("SADJGeneralError") & ": " & ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
            End Try

        End If

    End Sub

    ''' <summary>
    ''' Async simultaneous adjust solver routine.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <remarks>Solves all marked Adjust objects in the flowsheet simultaneously using Netwon's method.</remarks>
    Private Shared Sub SolveSimultaneousAdjustsAsync(ByVal fobj As Object, ct As CancellationToken)

        Dim fgui As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)
        Dim fqueue As IFlowsheetCalculationQueue = TryCast(fobj, IFlowsheetCalculationQueue)
        Dim fs As IFlowsheet = TryCast(fobj, IFlowsheet)

        If fs.FlowsheetOptions.SimultaneousAdjustSolverEnabled Then

            'this is the cancellation token for background threads. it checks for calculator stop requests and passes the request to the tasks.

            Dim n As Integer = 0

            For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
                If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then n += 1
            Next

            If n > 0 Then

                n -= 1

                Dim i As Integer = 0
                Dim dfdx(n, n), dx(n), fx(n), x(n), tols(n) As Double
                Dim il_err_ant As Double = 10000000000.0
                Dim il_err As Double = 10000000000.0
                Dim ic As Integer
                Dim converged As Boolean = False

                i = 0
                For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
                    If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                        x(i) = GetMnpVarValue(fobj, adj)
                        tols(i) = adj.Tolerance
                        i += 1
                    End If
                Next

                ic = 0
                Do

                    fx = FunctionValueAsync(fobj, x, ct)

                    converged = False

                    il_err_ant = il_err
                    il_err = fx.AbsSqrSumY
                    For i = 0 To x.Length - 1
                        If Math.Abs(fx(i)) < tols(i) Then
                            converged = True
                        Else
                            converged = False
                            Exit For
                        End If
                    Next

                    fgui.ShowMessage(fgui.GetTranslatedString("SimultaneousAdjust") & ": Iteration #" & ic + 1 & ", NSSE: " & il_err, IFlowsheet.MessageType.Information)

                    If converged Then Exit Do

                    dfdx = FunctionGradientAsync(fobj, x, ct)

                    Dim dfac As Double = 100000.0
                    Dim success = MathEx.SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)
                    If success Then

                        dfac = (ic + 1) * 0.2
                        If dfac > 1.0 Then dfac = 1.0

                        For i = 0 To x.Length - 1
                            If Math.Abs(-dx(i) * dfac) > x(i) Then
                                dfac /= 10
                                Exit For
                            End If
                        Next

                        For i = 0 To x.Length - 1
                            dx(i) = -dx(i)
                            x(i) += dfac * dx(i)
                        Next

                    End If

                    fs.CheckStatus()
                    fgui.UpdateInterface()

                    ic += 1

                    If ic >= 25 Then Throw New Exception(fgui.GetTranslatedString("SADJMaxIterationsReached"))
                    If Double.IsNaN(il_err) Then Throw New Exception(fgui.GetTranslatedString("SADJGeneralError"))
                    If Math.Abs(MathEx.Common.AbsSum(dx)) < 0.000001 Then Exit Do

                Loop

            End If

        End If

    End Sub

    ''' <summary>
    ''' Function called by the simultaneous adjust solver. Retrieves the error function value for each adjust object.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="x"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function FunctionValueSync(ByVal fobj As Object, ByVal x() As Double) As Double()

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        Dim i As Integer = 0
        For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
            If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                SetMnpVarValue(x(i), fobj, adj)
                i += 1
            End If
        Next

        Dim exceptions = SolveFlowsheet(fobj, Settings.SolverMode, Nothing, False, True)

        If exceptions.Count > 0 Then Throw New AggregateException(exceptions)

        Dim fx(x.Length - 1) As Double
        i = 0
        For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
            If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                Dim adjvalue As Double
                Dim punit = fbag.SimulationObjects(adj.ControlledObjectData.ID).GetPropertyUnit(adj.ControlledObjectData.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem)
                If adj.Referenced Then
                    If fbag.FlowsheetOptions.SelectedUnitSystem.GetUnitType(punit) = UnitOfMeasure.temperature Then
                        adjvalue = cv.ConvertFromSI(punit & ".", adj.AdjustValue)
                    Else
                        adjvalue = cv.ConvertFromSI(punit, adj.AdjustValue)
                    End If
                    fx(i) = adjvalue + GetRefVarValue(fobj, adj) - GetCtlVarValue(fobj, adj)
                Else
                    adjvalue = cv.ConvertFromSI(fbag.SimulationObjects(adj.ControlledObjectData.ID).GetPropertyUnit(adj.ControlledObjectData.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem), adj.AdjustValue)
                    fx(i) = adjvalue - GetCtlVarValue(fobj, adj)
                End If
                i += 1
            End If
        Next

        Return fx

    End Function

    ''' <summary>
    ''' Gradient function called by the simultaneous adjust solver. Retrieves the gradient of the error function value for each adjust object.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="x"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function FunctionGradientSync(ByVal fobj As Object, ByVal x() As Double) As Double(,)

        Dim epsilon As Double = 0.01

        Dim f2(), f3() As Double
        Dim g(x.Length - 1, x.Length - 1), x1(x.Length - 1), x2(x.Length - 1), x3(x.Length - 1), x4(x.Length - 1) As Double
        Dim i, j, k As Integer

        For i = 0 To x.Length - 1
            For j = 0 To x.Length - 1
                If i <> j Then
                    x2(j) = x(j)
                    x3(j) = x(j)
                Else
                    If x(j) <> 0.0# Then
                        x2(j) = x(j) * (1 + epsilon)
                        x3(j) = x(j) * (1 - epsilon)
                    Else
                        x2(j) = x(j) + epsilon
                        x3(j) = x(j)
                    End If
                End If
            Next
            f2 = FunctionValueSync(fobj, x2)
            f3 = FunctionValueSync(fobj, x3)
            For k = 0 To x.Length - 1
                g(k, i) = (f2(k) - f3(k)) / (x2(i) - x3(i))
            Next
        Next

        Return g

    End Function

    ''' <summary>
    ''' Function called asynchronously by the simultaneous adjust solver. Retrieves the error function value for each adjust object.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="x"></param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function FunctionValueAsync(ByVal fobj As Object, ByVal x() As Double, ct As CancellationToken) As Double()

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        Dim i As Integer = 0
        For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
            If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                SetMnpVarValue(x(i), fobj, adj)
                i += 1
            End If
        Next

        Dim exceptions = SolveFlowsheet(fobj, Settings.SolverMode, Nothing, False, True)

        If exceptions.Count > 0 Then Throw New AggregateException(exceptions)

        Dim fx(x.Length - 1) As Double
        i = 0
        For Each adj As IAdjust In fbag.SimulationObjects.Values.Where(Function(a) TypeOf a Is IAdjust)
            If adj.SimultaneousAdjust And DirectCast(adj, ISimulationObject).GraphicObject.Active Then
                Dim adjvalue As Double
                Dim punit = fbag.SimulationObjects(adj.ControlledObjectData.ID).GetPropertyUnit(adj.ControlledObjectData.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem)
                If adj.Referenced Then
                    If fbag.FlowsheetOptions.SelectedUnitSystem.GetUnitType(punit) = UnitOfMeasure.temperature Then
                        adjvalue = cv.ConvertFromSI(punit & ".", adj.AdjustValue)
                    Else
                        adjvalue = cv.ConvertFromSI(punit, adj.AdjustValue)
                    End If
                    fx(i) = adjvalue + GetRefVarValue(fobj, adj) - GetCtlVarValue(fobj, adj)
                Else
                    adjvalue = cv.ConvertFromSI(fbag.SimulationObjects(adj.ControlledObjectData.ID).GetPropertyUnit(adj.ControlledObjectData.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem), adj.AdjustValue)
                    fx(i) = adjvalue - GetCtlVarValue(fobj, adj)
                End If
            End If
        Next

        Return fx

    End Function

    ''' <summary>
    ''' Gradient function called asynchronously by the simultaneous adjust solver. Retrieves the gradient of the error function value for each adjust object.
    ''' </summary>
    ''' <param name="fobj">Flowsheet where the object belongs to.</param>
    ''' <param name="x"></param>
    ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function FunctionGradientAsync(ByVal fobj As Object, ByVal x() As Double, ct As CancellationToken) As Double(,)

        Dim epsilon As Double = 0.01

        Dim f2(), f3() As Double
        Dim g(x.Length - 1, x.Length - 1), x1(x.Length - 1), x2(x.Length - 1), x3(x.Length - 1), x4(x.Length - 1) As Double
        Dim i, j, k As Integer

        For i = 0 To x.Length - 1
            For j = 0 To x.Length - 1
                If i <> j Then
                    x2(j) = x(j)
                    x3(j) = x(j)
                Else
                    If x(j) <> 0.0# Then
                        x2(j) = x(j) * (1 + epsilon)
                        x3(j) = x(j) * (1 - epsilon)
                    Else
                        x2(j) = x(j) + epsilon
                        x3(j) = x(j)
                    End If
                End If
            Next
            f2 = FunctionValueAsync(fobj, x2, ct)
            f3 = FunctionValueAsync(fobj, x3, ct)
            For k = 0 To x.Length - 1
                g(k, i) = (f2(k) - f3(k)) / (x2(i) - x3(i))
            Next
        Next

        Return g

    End Function

    ''' <summary>
    ''' Gets the controlled variable value for the selected adjust op.
    ''' </summary>
    ''' <param name="fobj"></param>
    ''' <param name="adj"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function GetCtlVarValue(ByVal fobj As Object, ByVal adj As IAdjust) As Double

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        With adj.ControlledObjectData
            Return fbag.SimulationObjects(.ID).GetPropertyValue(.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem)
        End With

    End Function

    ''' <summary>
    ''' Gets the manipulated variable value for the selected adjust op.
    ''' </summary>
    ''' <param name="fobj"></param>
    ''' <param name="adj"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function GetMnpVarValue(ByVal fobj As Object, ByVal adj As IAdjust) As Double

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        With adj.ManipulatedObjectData
            Return fbag.SimulationObjects(.ID).GetPropertyValue(.PropertyName)
        End With

    End Function

    ''' <summary>
    ''' Sets the manipulated variable value for the selected adjust op.
    ''' </summary>
    ''' <param name="fobj"></param>
    ''' <param name="adj"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function SetMnpVarValue(ByVal val As Nullable(Of Double), ByVal fobj As Object, ByVal adj As IAdjust)

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        With adj.ManipulatedObjectData
            fbag.SimulationObjects(.ID).SetPropertyValue(.PropertyName, val)
        End With

        Return 1

    End Function

    ''' <summary>
    ''' Gets the referenced variable value for the selected adjust op.
    ''' </summary>
    ''' <param name="fobj"></param>
    ''' <param name="adj"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    Private Shared Function GetRefVarValue(ByVal fobj As Object, ByVal adj As IAdjust) As Double

        Dim fbag As IFlowsheet = TryCast(fobj, IFlowsheet)

        With adj.ReferencedObjectData
            Return fbag.SimulationObjects(.ID).GetPropertyValue(.PropertyName, fbag.FlowsheetOptions.SelectedUnitSystem)
        End With

    End Function

End Class
