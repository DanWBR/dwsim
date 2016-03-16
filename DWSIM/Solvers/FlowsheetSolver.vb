'    DWSIM Flowsheet Solver & Auxiliary Functions
'    Copyright 2008-2015 Daniel Wagner O. de Medeiros
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

Imports Microsoft.Msdn.Samples.GraphicObjects
Imports System.Collections.Generic
Imports System.ComponentModel
Imports PropertyGridEx
Imports WeifenLuo.WinFormsUI
Imports System.Drawing
Imports System.IO
Imports DWSIM.DWSIM.SimulationObjects
Imports System.Threading
Imports System.Threading.Tasks
Imports DWSIM.DWSIM.Outros
Imports Microsoft.Scripting.Hosting
Imports System.Linq

Namespace DWSIM.Flowsheet

    <System.Serializable()> Public Class FlowsheetSolver

        'events for plugins
        Public Shared Event UnitOpCalculationStarted As CustomEvent
        Public Shared Event UnitOpCalculationFinished As CustomEvent
        Public Shared Event FlowsheetCalculationStarted As CustomEvent
        Public Shared Event FlowsheetCalculationFinished As CustomEvent
        Public Shared Event MaterialStreamCalculationStarted As CustomEvent
        Public Shared Event MaterialStreamCalculationFinished As CustomEvent

        ''' <summary>
        ''' Flowsheet calculation routine 1. Calculates the object using information sent by the queue and updates the flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to calculate (FormChild object).</param>
        ''' <param name="objArgs">A StatusChangeEventArgs object containing information about the object to be calculated and its current status.</param>
        ''' <param name="sender"></param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateFlowsheet(ByVal form As FormFlowsheet, ByVal objArgs As DWSIM.Outros.StatusChangeEventArgs, ByVal sender As Object, Optional ByVal OnlyMe As Boolean = False)

            Dim preLab As String = form.FormSurface.LabelCalculator.Text

            If form.Options.CalculatorActivated Then

                RaiseEvent UnitOpCalculationStarted(form, New System.EventArgs(), objArgs)

                form.ProcessScripts(Script.EventType.ObjectCalculationStarted, Script.ObjectType.FlowsheetObject, objArgs.Nome)

                Select Case objArgs.Tipo
                    Case TipoObjeto.MaterialStream
                        Dim myObj As DWSIM.SimulationObjects.Streams.MaterialStream = form.Collections.CLCS_MaterialStreamCollection(objArgs.Nome)
                        Dim gobj As GraphicObject = myObj.GraphicObject
                        If Not gobj Is Nothing Then
                            If gobj.OutputConnectors(0).IsAttached = True Then
                                Dim myUnitOp As SimulationObjects_UnitOpBaseClass
                                myUnitOp = form.Collections.ObjectCollection(myObj.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                                If objArgs.Emissor = "Spec" Or objArgs.Emissor = "FlowsheetSolver" Then
                                    CalculateMaterialStream(form, myObj, , OnlyMe)
                                    myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                                Else
                                    If objArgs.Calculado = True Then
                                        gobj = myUnitOp.GraphicObject
                                        gobj.Calculated = True
                                        preLab = form.FormSurface.LabelCalculator.Text
                                        form.UpdateStatusLabel(String.Format(DWSIM.App.GetLocalString("CalculatingWith"), gobj.Tag, "'" & myObj.PropertyPackage.Tag & "' (" & myObj.PropertyPackage.ComponentName & ")"))
                                        myUnitOp.Solve()
                                        gobj.Status = Status.Calculated
                                        If myUnitOp.IsSpecAttached = True And myUnitOp.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myUnitOp.AttachedSpecId).Calculate()
                                        form.WriteToLog(gobj.Tag & ": " & DWSIM.App.GetLocalString("Calculadocomsucesso"), Color.DarkGreen, DWSIM.FormClasses.TipoAviso.Informacao)
                                        form.UpdateStatusLabel(preLab)
                                    Else
                                        myUnitOp.Unsolve()
                                        gobj = myUnitOp.GraphicObject
                                        gobj.Calculated = False
                                    End If
                                End If
                            End If
                            If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                            form.FormSurface.Refresh()
                        End If
                    Case TipoObjeto.EnergyStream
                        Dim myObj As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(objArgs.Nome)
                        myObj.Calculated = True
                        Dim gobj As GraphicObject = myObj.GraphicObject
                        If Not gobj Is Nothing Then
                            If gobj.OutputConnectors(0).IsAttached = True And Not OnlyMe Then
                                Dim myUnitOp As SimulationObjects_UnitOpBaseClass
                                myUnitOp = form.Collections.ObjectCollection(myObj.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
                                If objArgs.Calculado = True Then
                                    preLab = form.FormSurface.LabelCalculator.Text
                                    myUnitOp.GraphicObject.Calculated = False
                                    form.UpdateStatusLabel(String.Format(DWSIM.App.GetLocalString("CalculatingWith"), gobj.Tag, "'" & myUnitOp.PropertyPackage.Tag & "' (" & myUnitOp.PropertyPackage.ComponentName & ")"))
                                    myUnitOp.Solve()
                                    myUnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                                    form.WriteToLog(gobj.Tag & ": " & DWSIM.App.GetLocalString("Calculadocomsucesso"), Color.DarkGreen, DWSIM.FormClasses.TipoAviso.Informacao)
                                    myUnitOp.GraphicObject.Calculated = True
                                    If myUnitOp.IsSpecAttached = True And myUnitOp.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myUnitOp.AttachedSpecId).Calculate()
                                    form.UpdateStatusLabel(preLab)
                                    gobj = myUnitOp.GraphicObject
                                    gobj.Calculated = True
                                Else
                                    myUnitOp.Unsolve()
                                    myUnitOp.GraphicObject.Calculated = False
                                End If
                                myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                                form.FormSurface.Refresh()
                            End If
                            If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                        End If
                    Case Else
                        If objArgs.Emissor = "PropertyGrid" Or objArgs.Emissor = "Adjust" Or objArgs.Emissor = "FlowsheetSolver" Then
                            Dim myObj As SimulationObjects_UnitOpBaseClass = form.Collections.ObjectCollection(objArgs.Nome)
                            myObj.GraphicObject.Calculated = False
                            form.UpdateStatusLabel(String.Format(DWSIM.App.GetLocalString("CalculatingWith"), myObj.GraphicObject.Tag, "'" & myObj.PropertyPackage.Tag & "' (" & myObj.PropertyPackage.ComponentName & ")"))
                            myObj.Solve()
                            form.WriteToLog(objArgs.Tag & ": " & DWSIM.App.GetLocalString("Calculadocomsucesso"), Color.DarkGreen, DWSIM.FormClasses.TipoAviso.Informacao)
                            myObj.GraphicObject.Calculated = True
                            If myObj.IsSpecAttached = True And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                            form.FormProps.PGEx1.Refresh()
                            myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        Else
                            Dim myObj As SimulationObjects_UnitOpBaseClass = form.Collections.ObjectCollection(objArgs.Nome)
                            Dim gobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByName(objArgs.Nome, form.FormSurface.FlowsheetDesignSurface)
                            If Not OnlyMe Then
                                For Each cp As ConnectionPoint In gobj.OutputConnectors
                                    If cp.IsAttached And cp.Type = ConType.ConOut Then
                                        Dim obj As SimulationObjects_BaseClass = form.Collections.ObjectCollection(cp.AttachedConnector.AttachedTo.Name)
                                        If TypeOf obj Is Streams.MaterialStream Then
                                            Dim ms As Streams.MaterialStream = CType(obj, Streams.MaterialStream)
                                            ms.GraphicObject.Calculated = False
                                            form.UpdateStatusLabel(String.Format(DWSIM.App.GetLocalString("CalculatingWith"), ms.GraphicObject.Tag, "'" & myObj.PropertyPackage.Tag & "' (" & ms.PropertyPackage.ComponentName & ")"))
                                            CalculateMaterialStream(form, ms)
                                            ms.GraphicObject.Calculated = True
                                        End If
                                    End If
                                Next
                            End If
                            If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                            myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        End If
                End Select

                form.ProcessScripts(Script.EventType.ObjectCalculationFinished, Script.ObjectType.FlowsheetObject, objArgs.Nome)

                RaiseEvent UnitOpCalculationFinished(form, New System.EventArgs(), objArgs)

            End If

            Application.DoEvents()

            form.FormSurface.LabelCalculator.Text = preLab

        End Sub

        ''' <summary>
        ''' Calculates the flowsheet objects asynchronously. This function is always called from a task or a different thread other than UI's.
        ''' </summary>
        ''' <param name="form">Flowsheet to calculate (FormChild object).</param>
        ''' <param name="objArgs">A StatusChangeEventArgs object containing information about the object to be calculated and its current status.</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateFlowsheetAsync(ByVal form As FormFlowsheet, ByVal objArgs As DWSIM.Outros.StatusChangeEventArgs, ct As Threading.CancellationToken)

            If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

            If objArgs.Emissor = "FlowsheetSolver" Then
                form.ProcessScripts(Script.EventType.ObjectCalculationStarted, Script.ObjectType.FlowsheetObject, objArgs.Nome)
                Select Case objArgs.Tipo
                    Case TipoObjeto.MaterialStream
                        Dim myObj As DWSIM.SimulationObjects.Streams.MaterialStream = form.Collections.CLCS_MaterialStreamCollection(objArgs.Nome)
                        RaiseEvent MaterialStreamCalculationStarted(form, New System.EventArgs(), myObj)
                        CalculateMaterialStreamAsync(form, myObj, ct)
                        If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                        myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        RaiseEvent MaterialStreamCalculationFinished(form, New System.EventArgs(), myObj)
                    Case TipoObjeto.EnergyStream
                        Dim myObj As DWSIM.SimulationObjects.Streams.EnergyStream = form.Collections.CLCS_EnergyStreamCollection(objArgs.Nome)
                        If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                        myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        myObj.Calculated = True
                    Case Else
                        Dim myObj As SimulationObjects_UnitOpBaseClass = form.Collections.ObjectCollection(objArgs.Nome)
                        RaiseEvent UnitOpCalculationStarted(form, New System.EventArgs(), objArgs)
                        myObj.Solve()
                        If myObj.IsSpecAttached And myObj.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(myObj.AttachedSpecId).Calculate()
                        myObj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        RaiseEvent UnitOpCalculationFinished(form, New System.EventArgs(), objArgs)
                End Select
                form.ProcessScripts(Script.EventType.ObjectCalculationFinished, Script.ObjectType.FlowsheetObject, objArgs.Nome)
            End If

        End Sub

        ''' <summary>
        ''' Material Stream calculation routine 1. This routine check all input values and calculates all remaining properties of the stream.
        ''' </summary>
        ''' <param name="form">Flowsheet to what the stream belongs to.</param>
        ''' <param name="ms">Material Stream object to be calculated.</param>
        ''' <param name="DoNotCalcFlash">Tells the calculator whether to do flash calculations or not.</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateMaterialStream(ByVal form As FormFlowsheet, ByVal ms As DWSIM.SimulationObjects.Streams.MaterialStream, Optional ByVal DoNotCalcFlash As Boolean = False, Optional ByVal OnlyMe As Boolean = False)

            If form.Options.CalculatorActivated Then

                Dim preLab As String = form.FormSurface.LabelCalculator.Text

                ms.Calculated = False

                RaiseEvent MaterialStreamCalculationStarted(form, New System.EventArgs(), ms)

                form.ProcessScripts(Script.EventType.ObjectCalculationStarted, Script.ObjectType.FlowsheetObject, ms.Nome)

                ms.GraphicObject.Calculated = False

                form.UpdateStatusLabel(String.Format(DWSIM.App.GetLocalString("CalculatingWith"), ms.GraphicObject.Tag, "'" & ms.PropertyPackage.Tag & "' (" & ms.PropertyPackage.ComponentName & ")"))

                ms.Calculate(True, True)

                form.UpdateStatusLabel(preLab)

                form.WriteToLog(ms.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("Calculadocomsucesso"), Color.DarkGreen, DWSIM.FormClasses.TipoAviso.Informacao)

                form.ProcessScripts(Script.EventType.ObjectCalculationFinished, Script.ObjectType.FlowsheetObject, ms.Nome)

                RaiseEvent MaterialStreamCalculationFinished(form, New System.EventArgs(), ms)

                ms.LastUpdated = Date.Now
                ms.Calculated = True

                If Not OnlyMe Then
                    Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                    With objargs
                        .Calculado = True
                        .Nome = ms.Nome
                        .Tipo = TipoObjeto.MaterialStream
                    End With
                    CalculateFlowsheet(form, objargs, Nothing)
                End If

            End If

        End Sub

        ''' <summary>
        ''' Calculates a material stream object asynchronously. This function is always called from a task or a different thread other than UI's.
        ''' </summary>
        ''' <param name="form">Flowsheet to what the stream belongs to.</param>
        ''' <param name="ms">Material Stream object to be calculated.</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateMaterialStreamAsync(ByVal form As FormFlowsheet, ByVal ms As DWSIM.SimulationObjects.Streams.MaterialStream, ct As Threading.CancellationToken)

            If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

            ms.Calculated = False

            If My.Settings.EnableGPUProcessing Then DWSIM.App.InitComputeDevice()

            RaiseEvent MaterialStreamCalculationStarted(form, New System.EventArgs(), ms)

            form.ProcessScripts(Script.EventType.ObjectCalculationStarted, Script.ObjectType.FlowsheetObject, ms.Nome)

            ms.Calculate(True, True)

            form.ProcessScripts(Script.EventType.ObjectCalculationFinished, Script.ObjectType.FlowsheetObject, ms.Nome)

            RaiseEvent MaterialStreamCalculationFinished(form, New System.EventArgs(), ms)

            If ms.IsSpecAttached = True And ms.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then form.Collections.CLCS_SpecCollection(ms.AttachedSpecId).Calculate()

            ms.LastUpdated = Date.Now
            ms.Calculated = True

        End Sub

        ''' <summary>
        ''' Calls the cleaning routine for the object sent as an argument.
        ''' </summary>
        ''' <param name="form">Flowsheet to what the stream belongs to.</param>
        ''' <param name="obj">Object to be decalculated.</param>
        ''' <remarks></remarks>
        Public Shared Sub DeCalculateObject(ByVal form As FormFlowsheet, ByVal obj As GraphicObject)

            If obj.TipoObjeto = TipoObjeto.MaterialStream Then

                Dim con As ConnectionPoint
                For Each con In obj.InputConnectors
                    If con.IsAttached Then
                        Dim UnitOp As Object = form.Collections.ObjectCollection(con.AttachedConnector.AttachedFrom.Name)
                        UnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        UnitOp.DeCalculate()
                    End If
                Next
                For Each con In obj.OutputConnectors
                    If con.IsAttached Then
                        Dim UnitOp As Object = form.Collections.ObjectCollection(con.AttachedConnector.AttachedTo.Name)
                        UnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        UnitOp.DeCalculate()
                    End If
                Next

            ElseIf obj.TipoObjeto = TipoObjeto.EnergyStream Then

                Dim con As ConnectionPoint
                For Each con In obj.InputConnectors
                    If con.IsAttached Then
                        Dim UnitOp As Object = form.Collections.ObjectCollection(con.AttachedConnector.AttachedFrom.Name)
                        UnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        UnitOp.DeCalculate()
                    End If
                Next
                For Each con In obj.OutputConnectors
                    If con.IsAttached Then
                        Dim UnitOp As Object = form.Collections.ObjectCollection(con.AttachedConnector.AttachedTo.Name)
                        UnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                        UnitOp.DeCalculate()
                    End If
                Next

            Else

                Dim UnitOp As Object = form.Collections.ObjectCollection(obj.Name)
                UnitOp.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                UnitOp.DeCalculate()

            End If

        End Sub

        ''' <summary>
        ''' Calls the cleaning routine for the object sent as an argument.
        ''' </summary>
        ''' <param name="form">Flowsheet to what the stream belongs to.</param>
        ''' <param name="obj">Object to be decalculated.</param>
        ''' <param name="side"></param>
        ''' <remarks></remarks>
        Public Shared Sub DeCalculateDisconnectedObject(ByVal form As FormFlowsheet, ByVal obj As GraphicObject, ByVal side As String)

            If obj.TipoObjeto = TipoObjeto.MaterialStream Then

                Dim con As ConnectionPoint

                If side = "In" Then

                    For Each con In obj.InputConnectors
                        If con.IsAttached Then
                            Try
                                Dim UnitOp = form.Collections.ObjectCollection(con.AttachedConnector.AttachedFrom.Name)
                                form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                            Catch ex As Exception

                            End Try
                        End If
                    Next

                Else

                    For Each con In obj.OutputConnectors
                        If con.IsAttached Then
                            Try
                                Dim UnitOp = form.Collections.ObjectCollection(con.AttachedConnector.AttachedTo.Name)
                                form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                            Catch ex As Exception

                            End Try
                        End If
                    Next

                End If

            ElseIf obj.TipoObjeto = TipoObjeto.EnergyStream Then

                Dim con As ConnectionPoint
                If side = "In" Then

                    For Each con In obj.InputConnectors
                        If con.IsAttached Then
                            Dim UnitOp = form.Collections.ObjectCollection(con.AttachedConnector.AttachedFrom.Name)
                            form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                        End If
                    Next

                Else

                    For Each con In obj.OutputConnectors
                        If con.IsAttached Then
                            Dim UnitOp = form.Collections.ObjectCollection(con.AttachedConnector.AttachedTo.Name)
                            form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                        End If
                    Next

                End If

            Else

                If side = "In" Then

                    Try
                        Dim UnitOp = form.Collections.ObjectCollection(obj.Name)
                        form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                    Catch ex As Exception

                    End Try

                Else

                    Try
                        Dim UnitOp = form.Collections.ObjectCollection(obj.Name)
                        form.CalculationQueue.Enqueue(New StatusChangeEventArgs() With {.Calculado = False, .Emissor = "FlowsheetSolver", .Nome = UnitOp.Nome, .Tag = UnitOp.GraphicObject.Tag, .Tipo = UnitOp.GraphicObject.TipoObjeto})
                    Catch ex As Exception

                    End Try

                End If

            End If

        End Sub

        ''' <summary>
        ''' Process the calculation queue of the Flowsheet passed as an argument. Checks all elements in the queue and calculates them.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <remarks></remarks>
        Public Shared Function ProcessCalculationQueue(ByVal form As FormFlowsheet, Optional ByVal Isolated As Boolean = False,
                                                  Optional ByVal FlowsheetSolverMode As Boolean = False,
                                                  Optional ByVal mode As Integer = 0,
                                                  Optional orderedlist As Object = Nothing,
                                                  Optional ByVal ct As Threading.CancellationToken = Nothing,
                                                  Optional ByVal Adjusting As Boolean = False) As List(Of Exception)

            Dim exlist As New List(Of Exception)

            If mode = 0 Then
                'UI thread
                exlist = ProcessQueueInternal(form, Isolated, FlowsheetSolverMode, ct)
                If Not Adjusting Then SolveSimultaneousAdjusts(form)
            ElseIf mode = 1 Then
                'bg thread
                exlist = ProcessQueueInternalAsync(form, ct)
                If Not Adjusting Then SolveSimultaneousAdjustsAsync(form, ct)
            ElseIf mode = 2 Then
                'bg parallel thread
                'Dim prevset As Boolean = My.Settings.EnableParallelProcessing
                'My.Settings.EnableParallelProcessing = False
                exlist = ProcessQueueInternalAsyncParallel(form, orderedlist, ct)
                If Not Adjusting Then SolveSimultaneousAdjustsAsync(form, ct)
                'My.Settings.EnableParallelProcessing = prevset
            End If

            Return exlist

        End Function

        ''' <summary>
        ''' This is the internal routine called by ProcessCalculationQueue when the UI thread is used to calculate the flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <param name="Isolated">Tells to the calculator that only the objects in the queue must be calculated without checking the outlet connections, that is, no more objects will be added to the queue</param>
        ''' <param name="FlowsheetSolverMode">Only objects added by the flowsheet solving routine to the queue will be calculated.</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Private Shared Function ProcessQueueInternal(ByVal form As FormFlowsheet, Optional ByVal Isolated As Boolean = False, Optional ByVal FlowsheetSolverMode As Boolean = False, Optional ByVal ct As Threading.CancellationToken = Nothing) As List(Of Exception)

            form.FormSurface.LabelTime.Text = ""
            form.FormSurface.calcstart = Date.Now
            form.FormSurface.PictureBox3.Image = My.Resources.weather_lightning
            'form.FormSurface.PicSimultAdjust.Visible = True
            'form.FormSurface.LabelSimultAdjInfo.Visible = True
            'form.FormSurface.LabelSimultAdjustStatus.Visible = True

            Dim loopex As New List(Of Exception)

            My.Application.CalculatorStopRequested = False

            While form.CalculationQueue.Count >= 1

                If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

                If form.FormSurface.Timer2.Enabled = False Then form.FormSurface.Timer2.Start()

                Dim myinfo As DWSIM.Outros.StatusChangeEventArgs = form.CalculationQueue.Peek()

                Try
                    form.FormQueue.TextBox1.Clear()
                    For Each c As DWSIM.Outros.StatusChangeEventArgs In form.CalculationQueue
                        form.FormQueue.TextBox1.AppendText(form.Collections.ObjectCollection(c.Nome).GraphicObject.Tag & vbTab & vbTab & vbTab & "[" & DWSIM.App.GetLocalString(form.Collections.ObjectCollection(c.Nome).Descricao) & "]" & vbCrLf)
                    Next
                Catch ex As Exception

                End Try

                If form.Collections.ObjectCollection.ContainsKey(myinfo.Nome) Then

                    Dim myobj = form.Collections.ObjectCollection(myinfo.Nome)
                    Try
                        myobj.ErrorMessage = ""
                        If myobj.GraphicObject.Active Then
                            If FlowsheetSolverMode Then
                                If myinfo.Emissor = "FlowsheetSolver" Then
                                    If myinfo.Tipo = TipoObjeto.MaterialStream Then
                                        CalculateMaterialStream(form, form.Collections.CLCS_MaterialStreamCollection(myinfo.Nome), , Isolated)
                                    Else
                                        If My.Settings.EnableGPUProcessing Then DWSIM.App.InitComputeDevice()
                                        CalculateFlowsheet(form, myinfo, Nothing, Isolated)
                                    End If
                                End If
                            Else
                                If myinfo.Tipo = TipoObjeto.MaterialStream Then
                                    CalculateMaterialStream(form, form.Collections.CLCS_MaterialStreamCollection(myinfo.Nome), , Isolated)
                                Else
                                    If My.Settings.EnableGPUProcessing Then DWSIM.App.InitComputeDevice()
                                    CalculateFlowsheet(form, myinfo, Nothing, Isolated)
                                End If
                            End If
                            myobj.GraphicObject.Calculated = True
                        End If
                    Catch ex As AggregateException
                        myobj.ErrorMessage = ""
                        For Each iex In ex.InnerExceptions
                            myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                            loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message))
                        Next
                        If My.Settings.SolverBreakOnException Then Exit While
                    Catch ex As Exception
                        myobj.ErrorMessage = ex.Message.ToString & vbCrLf
                        loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message))
                        If My.Settings.SolverBreakOnException Then Exit While
                    End Try

                    form.FormWatch.UpdateList()

                    For Each g As GraphicObject In form.FormSurface.FlowsheetDesignSurface.drawingObjects
                        If g.TipoObjeto = TipoObjeto.GO_MasterTable Then
                            CType(g, DWSIM.GraphicObjects.MasterTableGraphic).Update(form)
                        End If
                    Next

                End If

                If form.CalculationQueue.Count = 1 Then form.FormSpreadsheet.InternalCounter = 0
                If form.CalculationQueue.Count > 0 Then form.CalculationQueue.Dequeue()

                CheckCalculatorStatus()

                Application.DoEvents()

            End While

            form.FormQueue.TextBox1.Clear()

            If Not form.FormSpreadsheet Is Nothing Then
                If form.FormSpreadsheet.chkUpdate.Checked Then form.FormSpreadsheet.EvaluateAll()
            End If

            If form.FormSurface.LabelTime.Text <> "" Then
                form.WriteToLog(DWSIM.App.GetLocalString("Runtime") & ": " & form.FormSurface.LabelTime.Text, Color.MediumBlue, DWSIM.FormClasses.TipoAviso.Informacao)
            End If

            If form.FormSurface.Timer2.Enabled = True Then form.FormSurface.Timer2.Stop()
            form.FormSurface.PictureBox3.Image = My.Resources.tick
            form.FormSurface.PicSimultAdjust.Visible = False
            form.FormSurface.LabelSimultAdjInfo.Visible = False
            form.FormSurface.LabelSimultAdjustStatus.Visible = False

            form.FormSurface.LabelTime.Text = ""

            If Not form.FormSurface.FlowsheetDesignSurface.SelectedObject Is Nothing Then Call form.FormSurface.UpdateSelectedObject()

            form.FormSurface.LabelCalculator.Text = DWSIM.App.GetLocalString("CalculadorOcioso")

            Application.DoEvents()

            Return loopex

        End Function

        ''' <summary>
        ''' This is the internal routine called by ProcessCalculationQueue when a background thread is used to calculate the flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Private Shared Function ProcessQueueInternalAsync(ByVal form As FormFlowsheet, ByVal ct As Threading.CancellationToken) As List(Of Exception)

            If My.Settings.EnableGPUProcessing Then DWSIM.App.InitComputeDevice()

            Dim loopex As New List(Of Exception)

            While form.CalculationQueue.Count >= 1

                If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()

                Dim myinfo As DWSIM.Outros.StatusChangeEventArgs = form.CalculationQueue.Peek()

                'form.UIThread(Sub() UpdateDisplayStatus(form, New String() {myinfo.Nome}, True))
                Dim myobj = form.Collections.ObjectCollection(myinfo.Nome)
                Try
                    myobj.ErrorMessage = ""
                    If myobj.GraphicObject.Active Then
                        If myinfo.Tipo = TipoObjeto.MaterialStream Then
                            CalculateMaterialStreamAsync(form, myobj, ct)
                        Else
                            CalculateFlowsheetAsync(form, myinfo, ct)
                        End If
                        myobj.GraphicObject.Calculated = True
                    End If
                Catch ex As AggregateException
                    form.ProcessScripts(Script.EventType.ObjectCalculationError, Script.ObjectType.FlowsheetObject, myobj.Nome)
                    myobj.ErrorMessage = ""
                    For Each iex In ex.InnerExceptions
                        myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                        loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message))
                    Next
                    If My.Settings.SolverBreakOnException Then Exit While
                Catch ex As Exception
                    form.ProcessScripts(Script.EventType.ObjectCalculationError, Script.ObjectType.FlowsheetObject, myobj.Nome)
                    myobj.ErrorMessage = ex.Message.ToString
                    loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message))
                    If My.Settings.SolverBreakOnException Then Exit While
                Finally
                    form.UIThread(Sub() UpdateDisplayStatus(form, New String() {myinfo.Nome}))
                End Try

                If form.CalculationQueue.Count = 1 Then form.FormSpreadsheet.InternalCounter = 0
                If form.CalculationQueue.Count > 0 Then form.CalculationQueue.Dequeue()

            End While

            Return loopex

        End Function

        ''' <summary>
        ''' This is the internal routine called by ProcessCalculationQueue when background parallel threads are used to calculate the flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Private Shared Function ProcessQueueInternalAsyncParallel(ByVal form As FormFlowsheet, ByVal orderedlist As Dictionary(Of Integer, List(Of StatusChangeEventArgs)), ct As Threading.CancellationToken) As List(Of Exception)

            If My.Settings.EnableGPUProcessing Then DWSIM.App.InitComputeDevice()

            Dim loopex As New Concurrent.ConcurrentBag(Of Exception)

            For Each obj In form.Collections.ObjectCollection.Values
                If TypeOf obj Is SimulationObjects_UnitOpBaseClass Then
                    DirectCast(obj, SimulationObjects_UnitOpBaseClass).PropertyPackage = Nothing
                    DirectCast(obj, SimulationObjects_UnitOpBaseClass).PropertyPackage = DirectCast(obj, SimulationObjects_UnitOpBaseClass).PropertyPackage.Clone
                    DirectCast(obj, SimulationObjects_UnitOpBaseClass).PropertyPackage.ForceNewFlashAlgorithmInstance = True
                ElseIf TypeOf obj Is Streams.MaterialStream Then
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage = Nothing
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage = DirectCast(obj, Streams.MaterialStream).PropertyPackage.Clone
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage.ForceNewFlashAlgorithmInstance = True
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage.CurrentMaterialStream = DirectCast(obj, Streams.MaterialStream)
                End If
            Next

            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = My.Settings.MaxDegreeOfParallelism,
                                                        .TaskScheduler = My.Application.AppTaskScheduler}

            For Each li In orderedlist
                Dim objlist As New ArrayList
                For Each item In li.Value
                    objlist.Add(item.Nome)
                Next
                Parallel.ForEach(li.Value, poptions, Sub(myinfo, state)
                                                         If ct.IsCancellationRequested = True Then ct.ThrowIfCancellationRequested()
                                                         Dim myobj = form.Collections.ObjectCollection(myinfo.Nome)
                                                         myobj.ErrorMessage = ""
                                                         Try
                                                             If myobj.GraphicObject.Active Then
                                                                 If myinfo.Tipo = TipoObjeto.MaterialStream Then
                                                                     CalculateMaterialStreamAsync(form, myobj, ct)
                                                                 Else
                                                                     CalculateFlowsheetAsync(form, myinfo, ct)
                                                                 End If
                                                                 myobj.GraphicObject.Calculated = True
                                                             End If
                                                         Catch ex As AggregateException
                                                             form.ProcessScripts(Script.EventType.ObjectCalculationError, Script.ObjectType.FlowsheetObject, myobj.Nome)
                                                             myobj.ErrorMessage = ""
                                                             For Each iex In ex.InnerExceptions
                                                                 myobj.ErrorMessage += iex.Message.ToString & vbCrLf
                                                                 loopex.Add(New Exception(myinfo.Tag & ": " & iex.Message))
                                                             Next
                                                             If My.Settings.SolverBreakOnException Then state.Break()
                                                         Catch ex As Exception
                                                             form.ProcessScripts(Script.EventType.ObjectCalculationError, Script.ObjectType.FlowsheetObject, myobj.Nome)
                                                             myobj.ErrorMessage = ex.Message.ToString
                                                             loopex.Add(New Exception(myinfo.Tag & ": " & ex.Message))
                                                             If My.Settings.SolverBreakOnException Then state.Break()
                                                         Finally
                                                             form.UIThread(Sub() UpdateDisplayStatus(form, New String() {myinfo.Nome}))
                                                         End Try
                                                     End Sub)
            Next

            For Each obj In form.Collections.ObjectCollection.Values
                If TypeOf obj Is SimulationObjects_UnitOpBaseClass Then
                    DirectCast(obj, SimulationObjects_UnitOpBaseClass).PropertyPackage = Nothing
                ElseIf TypeOf obj Is Streams.MaterialStream Then
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage = Nothing
                End If
            Next

            Return loopex.ToList

        End Function

        ''' <summary>
        ''' Checks the calculator status to see if the user did any stop/abort request, and throws an exception to force aborting, if necessary.
        ''' </summary>
        ''' <remarks></remarks>
        Public Shared Sub CheckCalculatorStatus()
            If DWSIM.App.IsMainThread Then
                If Not My.Application.IsRunningParallelTasks Then
                    If Not My.Application.CAPEOPENMode Then
                        If My.Application.CalculatorStopRequested = True Then
                            My.Application.MasterCalculatorStopRequested = True
                            My.Application.CalculatorStopRequested = False
                            If My.Application.TaskCancellationTokenSource IsNot Nothing Then
                                If Not My.Application.TaskCancellationTokenSource.IsCancellationRequested Then
                                    My.Application.TaskCancellationTokenSource.Cancel()
                                End If
                                My.Application.TaskCancellationTokenSource.Token.ThrowIfCancellationRequested()
                            Else
                                Throw New Exception(DWSIM.App.GetLocalString("CalculationAborted"))
                            End If
                        End If
                    End If
                    Application.DoEvents()
                End If
            End If
        End Sub

        ''' <summary>
        ''' This routine updates the display status of a list of graphic objects in the flowsheet according to their calculated status.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object).</param>
        ''' <param name="ObjIDlist">List of object IDs to be updated.</param>
        ''' <param name="calculating">Tell the routine that the objects in the list are being calculated at the moment.</param>
        ''' <remarks></remarks>
        Shared Sub UpdateDisplayStatus(form As FormFlowsheet, Optional ByVal ObjIDlist() As String = Nothing, Optional ByVal calculating As Boolean = False)
            If form.Visible Then
                If ObjIDlist Is Nothing Then
                    For Each baseobj As SimulationObjects_BaseClass In form.Collections.ObjectCollection.Values
                        If Not baseobj.GraphicObject Is Nothing Then
                            If Not baseobj.GraphicObject.Active Then
                                baseobj.GraphicObject.Status = Status.Inactive
                            Else
                                baseobj.GraphicObject.Calculated = baseobj.Calculated
                                If baseobj.Calculated Then baseobj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                            End If
                        End If
                    Next
                Else
                    For Each ObjID In ObjIDlist
                        If form.Collections.ObjectCollection.ContainsKey(ObjID) Then
                            Dim baseobj As SimulationObjects_BaseClass = form.Collections.ObjectCollection(ObjID)
                            If Not baseobj.GraphicObject Is Nothing Then
                                If calculating Then
                                    baseobj.GraphicObject.Status = Status.Calculating
                                Else
                                    If Not baseobj.GraphicObject.Active Then
                                        baseobj.GraphicObject.Status = Status.Inactive
                                    Else
                                        baseobj.GraphicObject.Calculated = baseobj.Calculated
                                        If baseobj.Calculated Then baseobj.UpdatePropertyNodes(form.Options.SelectedUnitSystem, form.Options.NumberFormat)
                                    End If
                                End If
                            End If
                        End If
                    Next
                End If
                form.UIThread(Sub()
                                  form.FormSurface.FlowsheetDesignSurface.Invalidate()
                              End Sub)
            End If
        End Sub

        ''' <summary>
        ''' Retrieves the list of objects to be solved in the flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <param name="frompgrid">Starts the search from the edited object if the propert was changed from the property grid.</param>
        ''' <returns>A list of objects to be calculated in the flowsheet.</returns>
        ''' <remarks></remarks>
        Private Shared Function GetSolvingList(form As FormFlowsheet, frompgrid As Boolean)

            Dim obj As SimulationObjects_BaseClass

            Dim lists As New Dictionary(Of Integer, List(Of String))
            Dim filteredlist As New Dictionary(Of Integer, List(Of String))
            Dim objstack As New List(Of String)

            Dim onqueue As DWSIM.Outros.StatusChangeEventArgs = Nothing

            Dim listidx As Integer = 0
            Dim maxidx As Integer = 0

            If frompgrid Then

                If form.CalculationQueue.Count > 0 Then

                    onqueue = form.CalculationQueue.Dequeue()
                    form.CalculationQueue.Clear()

                    lists.Add(0, New List(Of String))

                    lists(0).Add(onqueue.Nome)

                    'now start walking through the flowsheet until it reaches its end starting from this particular object.

                    Do
                        listidx += 1
                        If lists(listidx - 1).Count > 0 Then
                            lists.Add(listidx, New List(Of String))
                            maxidx = listidx
                            For Each o As String In lists(listidx - 1)
                                obj = form.Collections.ObjectCollection(o)
                                If obj.GraphicObject.Active Then
                                    For Each c As ConnectionPoint In obj.GraphicObject.OutputConnectors
                                        If c.IsAttached Then
                                            If obj.GraphicObject.TipoObjeto = TipoObjeto.OT_Reciclo Or obj.GraphicObject.TipoObjeto = TipoObjeto.OT_EnergyRecycle Then Exit For
                                            lists(listidx).Add(c.AttachedConnector.AttachedTo.Name)
                                        End If
                                    Next
                                End If
                            Next
                        Else
                            Exit Do
                        End If
                        If lists.Count > 10000 Then
                            lists.Clear()
                            Throw New Exception("Infinite loop detected while obtaining flowsheet object calculation order. Please insert recycle blocks where needed.")
                        End If
                    Loop

                    'process the lists , adding objects to the stack, discarding duplicate entries.

                    listidx = 0

                    Do
                        If lists.ContainsKey(listidx) Then
                            filteredlist.Add(listidx, New List(Of String)(lists(listidx).ToArray))
                            For Each o As String In lists(listidx)
                                If Not objstack.Contains(o) Then
                                    objstack.Add(o)
                                Else
                                    filteredlist(listidx).Remove(o)
                                End If
                            Next
                        Else
                            Exit Do
                        End If
                        listidx += 1
                    Loop Until listidx > maxidx

                End If

            Else

                'add endpoint material streams and recycle ops to the list, they will be the last objects to be calculated.

                lists.Add(0, New List(Of String))

                For Each baseobj As SimulationObjects_BaseClass In form.Collections.ObjectCollection.Values
                    If baseobj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream Then
                        Dim ms As Streams.MaterialStream = baseobj
                        If ms.GraphicObject.OutputConnectors(0).IsAttached = False Then
                            lists(0).Add(baseobj.Nome)
                        End If
                    ElseIf baseobj.GraphicObject.TipoObjeto = TipoObjeto.EnergyStream Then
                        lists(0).Add(baseobj.Nome)
                    ElseIf baseobj.GraphicObject.TipoObjeto = TipoObjeto.OT_Reciclo Then
                        lists(0).Add(baseobj.Nome)
                    ElseIf baseobj.GraphicObject.TipoObjeto = TipoObjeto.OT_EnergyRecycle Then
                        lists(0).Add(baseobj.Nome)
                    End If
                Next

                'now start processing the list at each level, until it reaches the beginning of the flowsheet.

                Do
                    listidx += 1
                    If lists(listidx - 1).Count > 0 Then
                        lists.Add(listidx, New List(Of String))
                        maxidx = listidx
                        For Each o As String In lists(listidx - 1)
                            obj = form.Collections.ObjectCollection(o)
                            If Not onqueue Is Nothing Then
                                If onqueue.Nome = obj.Nome Then Exit Do
                            End If
                            For Each c As ConnectionPoint In obj.GraphicObject.InputConnectors
                                If c.IsAttached Then
                                    If c.AttachedConnector.AttachedFrom.TipoObjeto <> TipoObjeto.OT_Reciclo And
                                        c.AttachedConnector.AttachedFrom.TipoObjeto <> TipoObjeto.OT_EnergyRecycle Then
                                        lists(listidx).Add(c.AttachedConnector.AttachedFrom.Name)
                                    End If
                                End If
                            Next
                        Next
                    Else
                        Exit Do
                    End If
                    If lists.Count > 10000 Then
                        lists.Clear()
                        Throw New Exception("Infinite loop detected while obtaining flowsheet object calculation order. Please insert recycle blocks where needed.")
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

            If form.Collections.CLCS_SpecCollection.Count > 0 Then
                Dim newstack As New List(Of String)
                For Each o In objstack
                    newstack.Add(o)
                    obj = form.Collections.ObjectCollection(o)
                    'if the object has a spec attached to it, set the destination object to be calculated after it.
                    If obj.IsSpecAttached And obj.SpecVarType = SpecialOps.Helpers.Spec.TipoVar.Fonte Then
                        newstack.Add(form.Collections.CLCS_SpecCollection(obj.AttachedSpecId).TargetObjectData.m_ID)
                    End If
                Next
                Dim newfilteredlist As New Dictionary(Of Integer, List(Of String))
                For Each kvp In filteredlist
                    Dim newlist As New List(Of String)
                    For Each o In kvp.Value
                        newlist.Add(o)
                        obj = form.Collections.ObjectCollection(o)
                        'if the object has a spec attached to it, set the destination object to be calculated after it.
                        If obj.IsSpecAttached And obj.SpecVarType = SpecialOps.Helpers.Spec.TipoVar.Fonte Then
                            newlist.Add(form.Collections.CLCS_SpecCollection(obj.AttachedSpecId).TargetObjectData.m_ID)
                        End If
                    Next
                    newfilteredlist.Add(kvp.Key, newlist)
                Next
                Return New Object() {newstack, lists, newfilteredlist}
            End If

            Return New Object() {objstack, lists, filteredlist}

        End Function

        ''' <summary>
        ''' Calculate all objects in the Flowsheet using a ordering method.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormFlowsheet object).</param>
        ''' <param name="Adjusting">True if the routine is called from the Simultaneous Adjust Solver.</param>
        ''' <param name="frompgrid">True if the routine is called from a PropertyGrid PropertyChanged event.</param>
        ''' <param name="mode">0 = Main Thread, 1 = Background Thread, 2 = Background Parallel Threads, 3 = Azure Service Bus, 4 = Network Computer</param>
        ''' <param name="ts">CancellationTokenSource instance from main flowsheet when calculating subflowsheets.</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateAll2(ByVal form As FormFlowsheet, mode As Integer, Optional ByVal ts As CancellationTokenSource = Nothing, Optional frompgrid As Boolean = False, Optional Adjusting As Boolean = False)

            'checks if the calculator is activated.

            If form.Options.CalculatorActivated Then

                If form.MasterFlowsheet Is Nothing Then My.Application.CalculatorBusy = True

                'this is the cancellation token for background threads. it checks for calculator stop requests and forwards the request to the tasks.

                If form.MasterFlowsheet Is Nothing Then
                    If ts Is Nothing Then ts = New CancellationTokenSource
                    My.Application.TaskCancellationTokenSource = ts
                End If
                Dim ct As CancellationToken = My.Application.TaskCancellationTokenSource.Token

                Dim obj As SimulationObjects_BaseClass

                'mode:
                '0 = Synchronous (main thread)
                '1 = Asynchronous (background thread)
                '2 = Asynchronous Parallel (background thread)
                '3 = Azure Service Bus
                '4 = Network Computer

                Dim d1 As Date = Date.Now
                Dim preLab As String = form.FormSurface.LabelCalculator.Text
                Dim age As AggregateException = Nothing
                Dim exlist As New List(Of Exception)

                'gets a list of objects to be solved in the flowsheet

                Dim objl = GetSolvingList(form, frompgrid)

                'declare a filteredlist dictionary. this will hold the sequence of grouped objects that can be calculated 
                'this way if the user selects the background parallel threads solver option

                Dim filteredlist2 As New Dictionary(Of Integer, List(Of StatusChangeEventArgs))

                'assign the list of objects, the filtered list (which contains no duplicate elements) and the object stack
                'which contains the ordered list of objects to be calculated.

                Dim lists As Dictionary(Of Integer, List(Of String)) = objl(1)
                Dim filteredlist As Dictionary(Of Integer, List(Of String)) = objl(2)
                Dim objstack As List(Of String) = objl(0)

                If objstack.Count = 0 Then Exit Sub

                'adds a message to the log window to indicate that the flowsheet started solving

                If form.MasterFlowsheet Is Nothing Then
                    form.WriteToLog(DWSIM.App.GetLocalString("FSstartedsolving"), Color.Blue, FormClasses.TipoAviso.Informacao)
                Else
                    form.WriteToLog(DWSIM.App.GetLocalString("Solving") & " '" & form.MasterUnitOp.GraphicObject.Tag & "'...", Color.Blue, FormClasses.TipoAviso.Informacao)
                End If

                'process scripts associated with the solverstarted event

                form.ProcessScripts(Script.EventType.SolverStarted, Script.ObjectType.Solver)

                RaiseEvent FlowsheetCalculationStarted(form, New System.EventArgs(), Nothing)

                'find recycles

                Dim recycles As New List(Of String)
                Dim totalv As Integer = 0

                For Each r In objstack
                    Dim robj = form.Collections.ObjectCollection(r)
                    If robj.GraphicObject.TipoObjeto = TipoObjeto.OT_Reciclo Then
                        recycles.Add(robj.Nome)
                        Dim rec = form.Collections.CLCS_RecycleCollection(robj.Nome)
                        If rec.AccelerationMethod = SpecialOps.Helpers.Recycle.AccelMethod.GlobalBroyden Then
                            If rec.Values.Count = 0 Then rec.Calculate()
                            totalv += form.Collections.CLCS_RecycleCollection(robj.Nome).Values.Count
                        End If
                    End If
                Next

                'size hessian matrix, variables and error vectors for recycle simultaneous solving.

                Dim rechess(totalv - 1, totalv - 1), recvars(totalv - 1), recdvars(totalv - 1), recerrs(totalv - 1), recvarsb(totalv - 1), recerrsb(totalv - 1) As Double

                'identity matrix as first hessian.

                For i As Integer = 0 To totalv - 1
                    rechess(i, i) = 1
                Next

                'set all objects' status to 'not calculated' (red) in the list

                For Each o In objstack
                    Dim fobj = form.Collections.ObjectCollection(o)
                    With fobj
                        .Calculated = False
                        If Not fobj.GraphicObject Is Nothing Then
                            If fobj.GraphicObject.Active Then
                                fobj.GraphicObject.Calculated = False
                            Else
                                fobj.GraphicObject.Status = Status.Inactive
                            End If
                        End If
                    End With
                Next

                Application.DoEvents()

                'initialize GPU if option enabled

                If My.Settings.EnableGPUProcessing And form.MasterFlowsheet Is Nothing Then
                    DWSIM.App.InitComputeDevice()
                    My.Application.gpu.EnableMultithreading()
                End If

                Select Case mode

                    Case 0, 1, 2

                        '0 = main thread, 1 = bg thread, 2 = bg parallel threads

                        'define variable to check for flowsheet convergence if there are recycle ops

                        Dim converged As Boolean = False

                        Dim loopidx As Integer = 0

                        'process/calculate the queue.

                        If form.CalculationQueue Is Nothing Then form.CalculationQueue = New Queue(Of DWSIM.Outros.StatusChangeEventArgs)

                        My.Application.MasterCalculatorStopRequested = False

                        Dim objargs As DWSIM.Outros.StatusChangeEventArgs = Nothing

                        Dim maintask As New Task(Sub()

                                                     Dim icount As Integer = 0

                                                     While Not converged

                                                         'add the objects to the calculation queue.

                                                         For Each o As String In objstack
                                                             obj = form.Collections.ObjectCollection(o)
                                                             objargs = New DWSIM.Outros.StatusChangeEventArgs
                                                             With objargs
                                                                 .Emissor = "FlowsheetSolver"
                                                                 .Calculado = True
                                                                 .Nome = obj.Nome
                                                                 .Tipo = obj.GraphicObject.TipoObjeto
                                                                 .Tag = obj.GraphicObject.Tag
                                                                 form.CalculationQueue.Enqueue(objargs)
                                                             End With
                                                         Next

                                                         'set the flowsheet instance for all objects, this is required for the async threads

                                                         For Each o In form.Collections.ObjectCollection.Values
                                                             o.SetFlowsheet(form)
                                                         Next

                                                         If mode = 0 Then

                                                             exlist = ProcessCalculationQueue(form, True, True, 0, Nothing, ct, Adjusting)

                                                         ElseIf mode = 1 Or mode = 2 Then

                                                             filteredlist2.Clear()

                                                             For Each li In filteredlist
                                                                 Dim objcalclist As New List(Of StatusChangeEventArgs)
                                                                 For Each o In li.Value
                                                                     obj = form.Collections.ObjectCollection(o)
                                                                     objcalclist.Add(New StatusChangeEventArgs() With {.Emissor = "FlowsheetSolver", .Nome = obj.Nome, .Tipo = obj.GraphicObject.TipoObjeto, .Tag = obj.GraphicObject.Tag})
                                                                 Next
                                                                 filteredlist2.Add(li.Key, objcalclist)
                                                             Next

                                                             exlist = ProcessCalculationQueue(form, True, True, mode, filteredlist2, ct, Adjusting)

                                                         End If

                                                         'throws exceptions if any

                                                         If My.Settings.SolverBreakOnException And exlist.Count > 0 Then Throw New AggregateException(exlist)

                                                         'checks for recycle convergence.

                                                         converged = True
                                                         For Each r As String In recycles
                                                             obj = form.Collections.CLCS_RecycleCollection(r)
                                                             converged = DirectCast(obj, SpecialOps.Recycle).Converged
                                                             If Not converged Then Exit For
                                                         Next

                                                         If Not converged Then

                                                             Dim avgerr As Double = 0.0#
                                                             Dim rcount As Integer = 0

                                                             For Each r As String In recycles
                                                                 obj = form.Collections.CLCS_RecycleCollection(r)
                                                                 With DirectCast(obj, SpecialOps.Recycle)
                                                                     avgerr += 0.33 * .ConvergenceHistory.TemperaturaE / .ConvergenceHistory.Temperatura
                                                                     avgerr += 0.33 * .ConvergenceHistory.PressaoE / .ConvergenceHistory.Pressao
                                                                     avgerr += 0.33 * .ConvergenceHistory.VazaoMassicaE / .ConvergenceHistory.VazaoMassica
                                                                 End With
                                                                 rcount += 1
                                                             Next

                                                             avgerr *= 100
                                                             avgerr /= rcount

                                                             form.WriteToLog("Recycle loop #" & (icount + 1) & ", average recycle error: " & Format(avgerr, "N") & "%", Color.Blue, FormClasses.TipoAviso.Informacao)

                                                         End If

                                                         'process the scripts associated with the recycle loop event.

                                                         form.ProcessScripts(Script.EventType.SolverRecycleLoop, Script.ObjectType.Solver)

                                                         'if the all recycles have converged (if any), then exit the loop.

                                                         If converged Then

                                                             Exit While

                                                         Else

                                                             If totalv > 0 Then

                                                                 'update variables of all recycles set to global broyden.

                                                                 Dim i As Integer = 0
                                                                 For Each r As String In recycles
                                                                     Dim rec = DirectCast(form.Collections.CLCS_RecycleCollection(r), SpecialOps.Recycle)
                                                                     If rec.AccelerationMethod = SpecialOps.Helpers.Recycle.AccelMethod.GlobalBroyden Then
                                                                         For Each kvp In rec.Values
                                                                             recvars(i) = kvp.Value
                                                                             recerrs(i) = rec.Errors(kvp.Key)
                                                                             i += 1
                                                                         Next
                                                                     End If
                                                                 Next

                                                                 DWSIM.MathEx.Broyden.broydn(totalv - 1, recvars, recerrs, recdvars, recvarsb, recerrsb, rechess, If(icount < 2, 0, 1))

                                                                 i = 0
                                                                 For Each r As String In recycles
                                                                     Dim rec = DirectCast(form.Collections.CLCS_RecycleCollection(r), SpecialOps.Recycle)
                                                                     If rec.AccelerationMethod = SpecialOps.Helpers.Recycle.AccelMethod.GlobalBroyden Then
                                                                         For Each kvp In rec.Errors
                                                                             rec.Values(kvp.Key) = recvars(i) + recdvars(i)
                                                                             i += 1
                                                                         Next
                                                                     End If
                                                                     rec.SetOutletStreamProperties()
                                                                 Next

                                                             End If

                                                         End If

                                                         If frompgrid Then
                                                             objl = GetSolvingList(form, False)
                                                             lists = objl(1)
                                                             filteredlist = objl(2)
                                                             objstack = objl(0)
                                                         End If

                                                         icount += 1



                                                     End While

                                                 End Sub)

                        'save temporary data of cape-open objects

                        For Each s In objstack
                            If TypeOf form.Collections.ObjectCollection(s) Is SimulationObjects.UnitOps.CapeOpenUO Then
                                'saves UO data to a temporary list so it can be loaded correctly by other threads
                                form.Collections.CLCS_CapeOpenUOCollection(s).SaveTempData()
                            End If
                        Next

                        'configure the task scheduler

                        If form.MasterFlowsheet Is Nothing Then

                            Dim nthreads As Integer = My.Settings.MaxThreadMultiplier * System.Environment.ProcessorCount

                            Select Case My.Settings.TaskScheduler
                                Case 0 'default
                                    If My.Settings.EnableGPUProcessing Then
                                        My.Application.AppTaskScheduler = TaskScheduler.FromCurrentSynchronizationContext
                                    Else
                                        My.Application.AppTaskScheduler = TaskScheduler.Default
                                    End If
                                Case 1 'sta
                                    My.Application.AppTaskScheduler = New DWSIM.Auxiliary.TaskSchedulers.StaTaskScheduler(nthreads)
                                Case 2 'limited concurrency
                                    My.Application.AppTaskScheduler = New DWSIM.Auxiliary.TaskSchedulers.LimitedConcurrencyLevelTaskScheduler(nthreads)
                            End Select

                        End If

                        Try
                            If mode = 0 Then
                                'this task will run synchronously with the UI thread.
                                maintask.RunSynchronously(My.Application.AppTaskScheduler)
                            Else
                                form.UpdateStatusLabel(DWSIM.App.GetLocalString("Calculando") & " " & DWSIM.App.GetLocalString("Fluxograma") & "...")
                                'this task will run asynchronously.
                                maintask.Start(My.Application.AppTaskScheduler)
                                If form.MasterFlowsheet Is Nothing Then
                                    While Not (Date.Now - d1).TotalMilliseconds >= My.Settings.SolverTimeoutSeconds * 1000
                                        maintask.Wait(500, ct)
                                        Application.DoEvents()
                                        If maintask.Status = TaskStatus.RanToCompletion Then Exit While
                                    End While
                                    If maintask.Status = TaskStatus.Running Then Throw New TimeoutException(DWSIM.App.GetLocalString("SolverTimeout"))
                                Else
                                    maintask.Wait()
                                End If
                            End If
                            If maintask.IsFaulted Then Throw maintask.Exception
                            If exlist.Count > 0 Then Throw New AggregateException(exlist)
                        Catch agex As AggregateException
                            age = agex
                        Catch ex As OperationCanceledException
                            age = New AggregateException(DWSIM.App.GetLocalString("CalculationAborted"), ex)
                        Catch ex As Exception
                            age = New AggregateException(ex.Message.ToString, ex)
                        Finally
                            If maintask.IsCompleted Then maintask.Dispose()
                            maintask = Nothing
                        End Try

                        'clears the calculation queue.

                        form.CalculationQueue.Clear()

                        'disposes the cancellation token source.

                        'If form.Visible Then ts.Dispose()

                        'My.Application.TaskCancellationTokenSource = Nothing

                        'clears the object lists.

                        objstack.Clear()
                        lists.Clear()
                        recycles.Clear()

                    Case 3

                        'Azure Service Bus

                        Dim azureclient As New Flowsheet.AzureSolverClient()

                        Try
                            azureclient.SolveFlowsheet(form)
                            For Each baseobj As SimulationObjects_BaseClass In form.Collections.ObjectCollection.Values
                                If baseobj.Calculated Then baseobj.LastUpdated = Date.Now
                            Next
                        Catch ex As Exception
                            age = New AggregateException(ex.Message.ToString, ex)
                        Finally
                            If Not azureclient.qcc.IsClosed Then azureclient.qcc.Close()
                            If Not azureclient.qcs.IsClosed Then azureclient.qcs.Close()
                        End Try

                        azureclient = Nothing

                    Case 4

                        'TCP/IP Solver

                        Dim tcpclient As New Flowsheet.TCPSolverClient()

                        Try
                            tcpclient.SolveFlowsheet(form)
                            For Each baseobj As SimulationObjects_BaseClass In form.Collections.ObjectCollection.Values
                                If baseobj.Calculated Then baseobj.LastUpdated = Date.Now
                            Next
                        Catch ex As Exception
                            age = New AggregateException(ex.Message.ToString, ex)
                        Finally
                            tcpclient.client.Close()
                        End Try

                        tcpclient = Nothing

                End Select

                'Frees GPU memory if enabled.

                If My.Settings.EnableGPUProcessing And form.MasterFlowsheet Is Nothing Then
                    My.Application.gpu.DisableMultithreading()
                    My.Application.gpu.FreeAll()
                End If

                'updates the display status of all objects in the calculation list.

                UpdateDisplayStatus(form, objstack.ToArray)

                'checks if exceptions were thrown during the calculation and displays them in the log window.

                If age Is Nothing Then

                    If form.MasterFlowsheet Is Nothing Then
                        form.WriteToLog(DWSIM.App.GetLocalString("FSfinishedsolvingok"), Color.Blue, FormClasses.TipoAviso.Informacao)
                        form.WriteToLog(DWSIM.App.GetLocalString("Runtime") & ": " & (Date.Now - d1).ToString("g"), Color.MediumBlue, DWSIM.FormClasses.TipoAviso.Informacao)
                    Else
                        form.WriteToLog(form.MasterUnitOp.GraphicObject.Tag & ": " & DWSIM.App.GetLocalString("PartialCalcTime") & " " & (Date.Now - d1).ToString("g") & ".", Color.Blue, FormClasses.TipoAviso.Informacao)
                    End If

                    If My.Settings.StorePreviousSolutions Then

                        'adds the current solution to the valid solution list.
                        'the XML data is converted to a compressed byte array before being added to the collection.

                        Dim stask As Task = Task.Factory.StartNew(Sub()
                                                                      Try
                                                                          Dim retbytes As MemoryStream = DWSIM.SimulationObjects.UnitOps.Flowsheet.ReturnProcessData(form)
                                                                          Using retbytes
                                                                              Dim uncompressedbytes As Byte() = retbytes.ToArray
                                                                              Using compressedstream As New MemoryStream()
                                                                                  Using gzs As New BufferedStream(New Compression.GZipStream(compressedstream, Compression.CompressionMode.Compress, True), 64 * 1024)
                                                                                      gzs.Write(uncompressedbytes, 0, uncompressedbytes.Length)
                                                                                      gzs.Close()
                                                                                      Dim id As String = Date.Now.ToBinary.ToString
                                                                                      If form.PreviousSolutions Is Nothing Then form.PreviousSolutions = New Dictionary(Of String, FormClasses.FlowsheetSolution)
                                                                                      form.PreviousSolutions.Add(id, New DWSIM.FormClasses.FlowsheetSolution() With {.ID = id, .SaveDate = Date.Now, .Solution = compressedstream.ToArray})
                                                                                  End Using
                                                                              End Using
                                                                          End Using
                                                                      Catch ex As Exception
                                                                      End Try
                                                                  End Sub).ContinueWith(Sub(t)
                                                                                            form.UpdateSolutionsList()
                                                                                        End Sub, TaskContinuationOptions.ExecuteSynchronously)

                    End If

                Else

                    If form.MasterFlowsheet Is Nothing Then

                        Dim baseexception As Exception = Nothing

                        form.WriteToLog(DWSIM.App.GetLocalString("FSfinishedsolvingerror"), Color.Red, FormClasses.TipoAviso.Erro)

                        For Each ex In age.Flatten().InnerExceptions
                            If TypeOf ex Is AggregateException Then
                                baseexception = ex.InnerException
                                For Each iex In DirectCast(ex, AggregateException).Flatten().InnerExceptions
                                    While iex.InnerException IsNot Nothing
                                        baseexception = iex.InnerException
                                    End While
                                    form.WriteToLog(baseexception.Message.ToString, Color.Red, FormClasses.TipoAviso.Erro)
                                Next
                            Else
                                baseexception = ex
                                While ex.InnerException IsNot Nothing
                                    baseexception = ex.InnerException
                                End While
                                form.WriteToLog(baseexception.Message.ToString, Color.Red, FormClasses.TipoAviso.Erro)
                            End If
                        Next

                        age = Nothing

                    Else

                        Throw age

                    End If

                End If

                'updates the flowsheet display information if the form is visible.

                If form.Visible And form.MasterFlowsheet Is Nothing Then

                    form.FormWatch.UpdateList()

                    form.FormQueue.TextBox1.Clear()

                    For Each g As GraphicObject In form.FormSurface.FlowsheetDesignSurface.drawingObjects
                        If g.TipoObjeto = TipoObjeto.GO_MasterTable Then
                            CType(g, DWSIM.GraphicObjects.MasterTableGraphic).Update(form)
                        End If
                    Next

                    If Not form.FormSpreadsheet Is Nothing Then
                        If form.FormSpreadsheet.chkUpdate.Checked Then
                            form.FormSpreadsheet.EvaluateAll()
                            form.FormSpreadsheet.EvaluateAll()
                        End If
                    End If

                    form.UpdateStatusLabel(preLab)

                    If form.FormSurface.Timer2.Enabled = True Then form.FormSurface.Timer2.Stop()
                    form.FormSurface.PictureBox3.Image = My.Resources.tick
                    form.FormSurface.LabelTime.Text = ""

                    'form.FormSurface.LabelSimultAdjInfo.Text = ""
                    'form.FormSurface.PicSimultAdjust.Visible = False
                    'form.FormSurface.LabelSimultAdjInfo.Visible = False
                    'form.FormSurface.LabelSimultAdjustStatus.Visible = False

                    If Not form.FormSurface.FlowsheetDesignSurface.SelectedObject Is Nothing Then Call form.FormSurface.UpdateSelectedObject()

                    Application.DoEvents()

                End If

                If form.MasterFlowsheet Is Nothing Then My.Application.CalculatorBusy = False

                form.ProcessScripts(Script.EventType.SolverFinished, Script.ObjectType.Solver)

                RaiseEvent FlowsheetCalculationFinished(form, New System.EventArgs(), Nothing)

            Else

                form.WriteToLog(DWSIM.App.GetLocalString("Calculadordesativado"), Color.DarkGray, FormClasses.TipoAviso.Informacao)

            End If

        End Sub

        ''' <summary>
        ''' Calculate all objects in the Flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet to be calculated (FormChild object)</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateAll(ByVal form As FormFlowsheet)

            CalculateAll2(form, My.Settings.SolverMode)

        End Sub

        ''' <summary>
        ''' Calculates a single object in the Flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateObject(ByVal form As FormFlowsheet, ByVal ObjID As String)

            If form.Collections.ObjectCollection.ContainsKey(ObjID) Then

                Dim baseobj As SimulationObjects_BaseClass = form.Collections.ObjectCollection(ObjID)

                Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                With objargs
                    .Calculado = True
                    .Nome = baseobj.Nome
                    .Tipo = baseobj.GraphicObject.TipoObjeto
                    .Tag = baseobj.GraphicObject.Tag
                End With

                form.CalculationQueue.Enqueue(objargs)

                CalculateAll2(form, My.Settings.SolverMode, , True)

            End If

        End Sub

        ''' <summary>
        ''' Calculates a single object in the Flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateObjectSync(ByVal form As FormFlowsheet, ByVal ObjID As String)

            If form.Collections.ObjectCollection.ContainsKey(ObjID) Then

                Dim baseobj As SimulationObjects_BaseClass = form.Collections.ObjectCollection(ObjID)

                If baseobj.GraphicObject.TipoObjeto = TipoObjeto.MaterialStream Then
                    Dim ms As Streams.MaterialStream = baseobj
                    If ms.GraphicObject.InputConnectors(0).IsAttached = False Then
                        'add this stream to the calculator queue list
                        Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                        With objargs
                            .Calculado = True
                            .Nome = ms.Nome
                            .Tipo = TipoObjeto.MaterialStream
                            .Tag = ms.GraphicObject.Tag
                        End With
                        If ms.IsSpecAttached = True And ms.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then
                            form.Collections.CLCS_SpecCollection(ms.AttachedSpecId).Calculate()
                        End If
                        form.CalculationQueue.Enqueue(objargs)
                        ProcessQueueInternal(form)
                    Else
                        If ms.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.TipoObjeto = TipoObjeto.OT_Reciclo Then
                            'add this stream to the calculator queue list
                            Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                            With objargs
                                .Calculado = True
                                .Nome = ms.Nome
                                .Tipo = TipoObjeto.MaterialStream
                                .Tag = ms.GraphicObject.Tag
                            End With
                            If ms.IsSpecAttached = True And ms.SpecVarType = DWSIM.SimulationObjects.SpecialOps.Helpers.Spec.TipoVar.Fonte Then
                                form.Collections.CLCS_SpecCollection(ms.AttachedSpecId).Calculate()
                            End If
                            form.CalculationQueue.Enqueue(objargs)
                            ProcessQueueInternal(form)
                        End If
                    End If
                Else
                    Dim unit As SimulationObjects_UnitOpBaseClass = baseobj
                    Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                    With objargs
                        .Emissor = "PropertyGrid"
                        .Calculado = True
                        .Nome = unit.Nome
                        .Tipo = unit.GraphicObject.TipoObjeto
                        .Tag = unit.GraphicObject.Tag
                    End With
                    form.CalculationQueue.Enqueue(objargs)
                    ProcessQueueInternal(form)
                End If

            End If

        End Sub

        ''' <summary>
        ''' Calculates a single object in the Flowsheet.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="ObjID">Unique Id of the object ("Name" or "GraphicObject.Name" properties). This is not the object's Flowsheet display name ("Tag" property or its GraphicObject object).</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks></remarks>
        Public Shared Sub CalculateObjectAsync(ByVal form As FormFlowsheet, ByVal ObjID As String, ByVal ct As CancellationToken)

            If form.Collections.ObjectCollection.ContainsKey(ObjID) Then

                Dim baseobj As SimulationObjects_BaseClass = form.Collections.ObjectCollection(ObjID)

                Dim objargs As New DWSIM.Outros.StatusChangeEventArgs
                With objargs
                    .Emissor = "PropertyGrid"
                    .Calculado = True
                    .Nome = baseobj.Nome
                    .Tipo = TipoObjeto.MaterialStream
                    .Tag = baseobj.GraphicObject.Tag
                End With
                form.CalculationQueue.Enqueue(objargs)

                Dim objl = GetSolvingList(form, True)

                Dim objstack As List(Of String) = objl(0)

                For Each o As String In objstack
                    Dim obj = form.Collections.ObjectCollection(o)
                    objargs = New DWSIM.Outros.StatusChangeEventArgs
                    With objargs
                        .Emissor = "FlowsheetSolver"
                        .Calculado = True
                        .Nome = obj.Nome
                        .Tipo = obj.GraphicObject.TipoObjeto
                        .Tag = obj.GraphicObject.Tag
                        form.CalculationQueue.Enqueue(objargs)
                    End With
                Next

                For Each o In form.Collections.ObjectCollection.Values
                    o.SetFlowsheet(form)
                Next

                ProcessQueueInternalAsync(form, ct)

            End If

        End Sub

        ''' <summary>
        ''' Simultaneous adjust solver routine.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <remarks>Solves all marked Adjust objects in the flowsheet simultaneously using Netwon's method.</remarks>
        Private Shared Sub SolveSimultaneousAdjusts(ByVal form As FormFlowsheet)

            If form.m_simultadjustsolverenabled Then

                form.FormSurface.LabelSimultAdjInfo.Text = ""
                form.FormSurface.PicSimultAdjust.Visible = True
                form.FormSurface.LabelSimultAdjInfo.Visible = True
                form.FormSurface.LabelSimultAdjustStatus.Visible = True


                Try

                    Dim n As Integer = 0

                    For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                        If adj.SimultaneousAdjust Then n += 1
                    Next

                    If n > 0 Then

                        n -= 1

                        Dim i As Integer = 0
                        Dim dfdx(n, n), dx(n), fx(n), x(n) As Double
                        Dim il_err_ant As Double = 10000000000.0
                        Dim il_err As Double = 10000000000.0
                        Dim ic As Integer

                        i = 0
                        For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                            If adj.SimultaneousAdjust Then
                                x(i) = GetMnpVarValue(form, adj)
                                i += 1
                            End If
                        Next

                        ic = 0
                        Do

                            fx = FunctionValueSync(form, x)

                            il_err_ant = il_err
                            il_err = 0
                            For i = 0 To x.Length - 1
                                il_err += (fx(i)) ^ 2
                            Next

                            form.FormSurface.LabelSimultAdjInfo.Text = "Iteration #" & ic + 1 & ", NSSE: " & il_err

                            Application.DoEvents()

                            If il_err < 0.0000000001 Then Exit Do

                            dfdx = FunctionGradientSync(form, x)

                            Dim success As Boolean
                            success = MathEx.SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)
                            If success Then
                                For i = 0 To x.Length - 1
                                    dx(i) = -dx(i)
                                    x(i) += dx(i)
                                Next
                            End If

                            ic += 1

                            If ic >= 100 Then Throw New Exception(DWSIM.App.GetLocalString("SADJMaxIterationsReached"))
                            If Double.IsNaN(il_err) Then Throw New Exception(DWSIM.App.GetLocalString("SADJGeneralError"))
                            If Math.Abs(MathEx.Common.AbsSum(dx)) < 0.000001 Then Exit Do

                        Loop

                    End If

                Catch ex As Exception
                    form.WriteToLog(DWSIM.App.GetLocalString("SADJGeneralError") & ": " & ex.Message.ToString, Color.Red, FormClasses.TipoAviso.Erro)
                Finally
                    form.FormSurface.LabelSimultAdjInfo.Text = ""
                    form.FormSurface.PicSimultAdjust.Visible = False
                    form.FormSurface.LabelSimultAdjInfo.Visible = False
                    form.FormSurface.LabelSimultAdjustStatus.Visible = False
                End Try

            End If

        End Sub

        ''' <summary>
        ''' Async simultaneous adjust solver routine.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <remarks>Solves all marked Adjust objects in the flowsheet simultaneously using Netwon's method.</remarks>
        Private Shared Sub SolveSimultaneousAdjustsAsync(ByVal form As FormFlowsheet, ct As CancellationToken)

            If form.m_simultadjustsolverenabled Then

                'this is the cancellation token for background threads. it checks for calculator stop requests and passes the request to the tasks.

                Dim n As Integer = 0

                For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                    If adj.SimultaneousAdjust Then n += 1
                Next

                If n > 0 Then

                    form.UIThread(Sub()
                                      form.FormSurface.LabelSimultAdjInfo.Text = ""
                                      form.FormSurface.PicSimultAdjust.Visible = True
                                      form.FormSurface.LabelSimultAdjInfo.Visible = True
                                      form.FormSurface.LabelSimultAdjustStatus.Visible = True

                                  End Sub)

                    n -= 1

                    Dim i As Integer = 0
                    Dim dfdx(n, n), dx(n), fx(n), x(n) As Double
                    Dim il_err_ant As Double = 10000000000.0
                    Dim il_err As Double = 10000000000.0
                    Dim ic As Integer

                    i = 0
                    For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                        If adj.SimultaneousAdjust Then
                            x(i) = GetMnpVarValue(form, adj)
                            i += 1
                        End If
                    Next

                    ic = 0
                    Do

                        fx = FunctionValueAsync(form, x, ct)

                        il_err_ant = il_err
                        il_err = 0
                        For i = 0 To x.Length - 1
                            il_err += (fx(i)) ^ 2
                        Next

                        form.UIThread(Sub() form.FormSurface.LabelSimultAdjInfo.Text = "Iteration #" & ic + 1 & ", NSSE: " & il_err)

                        If il_err < 0.0000000001 Then Exit Do

                        dfdx = FunctionGradientAsync(form, x, ct)

                        Dim success As Boolean
                        success = MathEx.SysLin.rsolve.rmatrixsolve(dfdx, fx, x.Length, dx)
                        If success Then
                            For i = 0 To x.Length - 1
                                dx(i) = -dx(i)
                                x(i) += dx(i)
                            Next
                        End If

                        ic += 1

                        If ic >= 100 Then Throw New Exception(DWSIM.App.GetLocalString("SADJMaxIterationsReached"))
                        If Double.IsNaN(il_err) Then Throw New Exception(DWSIM.App.GetLocalString("SADJGeneralError"))
                        If Math.Abs(MathEx.Common.AbsSum(dx)) < 0.000001 Then Exit Do

                    Loop

                    form.UIThread(Sub()
                                      form.FormSurface.LabelSimultAdjInfo.Text = ""
                                      form.FormSurface.PicSimultAdjust.Visible = False
                                      form.FormSurface.LabelSimultAdjInfo.Visible = False
                                      form.FormSurface.LabelSimultAdjustStatus.Visible = False
                                  End Sub)

                End If

            End If

        End Sub

        ''' <summary>
        ''' Function called by the simultaneous adjust solver. Retrieves the error function value for each adjust object.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="x"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function FunctionValueSync(ByVal form As FormFlowsheet, ByVal x() As Double) As Double()

            Dim i As Integer = 0
            For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                If adj.SimultaneousAdjust Then
                    SetMnpVarValue(x(i), form, adj)
                    i += 1
                End If
            Next

            CalculateAll2(form, My.Settings.SolverMode, Nothing, False, True)

            Dim fx(x.Length - 1) As Double
            i = 0
            For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                If adj.SimultaneousAdjust Then
                    If adj.Referenced Then
                        fx(i) = adj.AdjustValue + GetRefVarValue(form, adj) - GetCtlVarValue(form, adj)
                    Else
                        fx(i) = adj.AdjustValue - GetCtlVarValue(form, adj)
                    End If
                    i += 1
                End If
            Next

            Return fx

        End Function

        ''' <summary>
        ''' Gradient function called by the simultaneous adjust solver. Retrieves the gradient of the error function value for each adjust object.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="x"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function FunctionGradientSync(ByVal form As FormFlowsheet, ByVal x() As Double) As Double(,)

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
                f2 = FunctionValueSync(form, x2)
                f3 = FunctionValueSync(form, x3)
                For k = 0 To x.Length - 1
                    g(k, i) = (f2(k) - f3(k)) / (x2(i) - x3(i))
                Next
            Next

            Return g

        End Function

        ''' <summary>
        ''' Function called asynchronously by the simultaneous adjust solver. Retrieves the error function value for each adjust object.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="x"></param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function FunctionValueAsync(ByVal form As FormFlowsheet, ByVal x() As Double, ct As CancellationToken) As Double()

            Dim i As Integer = 0
            For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                If adj.SimultaneousAdjust Then
                    SetMnpVarValue(x(i), form, adj)
                    i += 1
                End If
            Next

            CalculateAll2(form, My.Settings.SolverMode, Nothing, False, True)

            Dim fx(x.Length - 1) As Double
            i = 0
            For Each adj As SimulationObjects.SpecialOps.Adjust In form.Collections.CLCS_AdjustCollection.Values
                If adj.SimultaneousAdjust Then
                    If adj.Referenced Then
                        fx(i) = adj.AdjustValue + GetRefVarValue(form, adj) - GetCtlVarValue(form, adj)
                    Else
                        fx(i) = adj.AdjustValue - GetCtlVarValue(form, adj)
                    End If
                    i += 1
                End If
            Next

            Return fx

        End Function

        ''' <summary>
        ''' Gradient function called asynchronously by the simultaneous adjust solver. Retrieves the gradient of the error function value for each adjust object.
        ''' </summary>
        ''' <param name="form">Flowsheet where the object belongs to.</param>
        ''' <param name="x"></param>
        ''' <param name="ct">The cancellation token, used to listen for calculation cancellation requests from the user.</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function FunctionGradientAsync(ByVal form As FormFlowsheet, ByVal x() As Double, ct As CancellationToken) As Double(,)

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
                f2 = FunctionValueAsync(form, x2, ct)
                f3 = FunctionValueAsync(form, x3, ct)
                For k = 0 To x.Length - 1
                    g(k, i) = (f2(k) - f3(k)) / (x2(i) - x3(i))
                Next
            Next

            Return g

        End Function

        ''' <summary>
        ''' Gets the controlled variable value for the selected adjust op.
        ''' </summary>
        ''' <param name="form"></param>
        ''' <param name="adj"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function GetCtlVarValue(ByVal form As FormFlowsheet, ByVal adj As SimulationObjects.SpecialOps.Adjust)

            With adj.ControlledObjectData
                Return form.Collections.ObjectCollection(.m_ID).GetPropertyValue(.m_Property)
            End With

        End Function

        ''' <summary>
        ''' Gets the manipulated variable value for the selected adjust op.
        ''' </summary>
        ''' <param name="form"></param>
        ''' <param name="adj"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function GetMnpVarValue(ByVal form As FormFlowsheet, ByVal adj As SimulationObjects.SpecialOps.Adjust)

            With adj.ManipulatedObjectData()
                Return form.Collections.ObjectCollection(.m_ID).GetPropertyValue(.m_Property)
            End With

        End Function

        ''' <summary>
        ''' Sets the manipulated variable value for the selected adjust op.
        ''' </summary>
        ''' <param name="form"></param>
        ''' <param name="adj"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function SetMnpVarValue(ByVal val As Nullable(Of Double), ByVal form As FormFlowsheet, ByVal adj As SimulationObjects.SpecialOps.Adjust)

            With adj.ManipulatedObjectData()
                form.Collections.ObjectCollection(.m_ID).SetPropertyValue(.m_Property, val)
            End With

            Return 1

        End Function

        ''' <summary>
        ''' Gets the referenced variable value for the selected adjust op.
        ''' </summary>
        ''' <param name="form"></param>
        ''' <param name="adj"></param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Private Shared Function GetRefVarValue(ByVal form As FormFlowsheet, ByVal adj As SimulationObjects.SpecialOps.Adjust)

            With adj.ManipulatedObjectData
                With adj.ControlledObjectData()
                    Return form.Collections.ObjectCollection(.m_ID).GetPropertyValue(.m_Name, form.Options.SelectedUnitSystem)
                End With
            End With

        End Function

    End Class

End Namespace
