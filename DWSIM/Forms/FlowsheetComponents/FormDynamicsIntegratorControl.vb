Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.DynamicsManager
Imports Eto.Threading
Imports Python.Runtime

Public Class FormDynamicsIntegratorControl

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Public Busy As Boolean = False

    Public Abort As Boolean = False

    Private Sub FormDynamicsIntegratorControl_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub

    Private Sub FormDynamicsIntegratorControl_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        If Visible Then

            cbScenario.Items.Clear()

            For Each item In Flowsheet.DynamicsManager.ScheduleList
                If item.Value.CurrentIntegrator <> "" Then
                    Dim integ = Flowsheet.DynamicsManager.IntegratorList(item.Value.CurrentIntegrator).Description
                    cbScenario.Items.Add(item.Value.Description & " (" & integ & ")")
                Else
                    cbScenario.Items.Add(item.Value.Description)
                End If
            Next

            If cbScenario.Items.Count > 0 Then
                cbScenario.SelectedIndex = 0
            End If

        End If

    End Sub

    Private Sub cbScenario_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbScenario.SelectedIndexChanged
        Flowsheet.DynamicsManager.CurrentSchedule = Flowsheet.DynamicsManager.ScheduleList.Values.ToList(cbScenario.SelectedIndex).ID
    End Sub

    Private Paused As Boolean = False
    Private Running As Boolean = False

    Private Sub btnRun_Click(sender As Object, e As EventArgs) Handles btnRun.Click

        If Running Then Paused = Not Paused

        If Paused Then
            btnRun.BackgroundImage = My.Resources.icons8_play
        Else
            btnRun.BackgroundImage = My.Resources.icons8_pause
        End If

        If Flowsheet.DynamicMode Then
            If Not Running Then
                RunIntegrator(False, False, False)
            Else
                If Not Paused Then
                    RunIntegrator(False, False, True)
                End If
            End If
        Else
            Flowsheet.ShowMessage(DWSIM.App.GetLocalString("DynamicsDisabled"), Interfaces.IFlowsheet.MessageType.Warning)
        End If

    End Sub

    Private Sub btnStop_Click(sender As Object, e As EventArgs) Handles btnStop.Click

        btnRun.BackgroundImage = My.Resources.icons8_play

        Paused = False
        Abort = True

    End Sub

    Sub StoreVariableValues(integrator As DynamicsManager.Integrator, tstep As Integer, tstamp As DateTime)

        Dim list As New List(Of Interfaces.IDynamicsMonitoredVariable)

        For Each v As DynamicsManager.MonitoredVariable In integrator.MonitoredVariables
            Dim vnew = DirectCast(v.Clone, DynamicsManager.MonitoredVariable)
            Dim sobj = Flowsheet.SimulationObjects(vnew.ObjectID)
            vnew.PropertyValue = SystemsOfUnits.Converter.ConvertFromSI(vnew.PropertyUnits, sobj.GetPropertyValue(vnew.PropertyID)).ToString(Globalization.CultureInfo.InvariantCulture)
            vnew.TimeStamp = tstamp
            list.Add(vnew)
        Next

        integrator.MonitoredVariableValues.Add(tstamp.Ticks, list)

    End Sub

    Sub ProcessEvents(eventsetID As String, currentposition As DateTime, interval As TimeSpan)

        Dim eventset = Flowsheet.DynamicsManager.EventSetList(eventsetID)

        Dim initialtime = currentposition - interval

        Dim finaltime = currentposition

        Dim events = eventset.Events.Values.Where(Function(x) x.TimeStamp >= initialtime And x.TimeStamp < finaltime).ToList

        For Each ev In events
            If ev.Enabled Then
                Select Case ev.EventType
                    Case Dynamics.DynamicsEventType.ChangeProperty
                        Dim obj = Flowsheet.SimulationObjects(ev.SimulationObjectID)
                        Dim value = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ev.SimulationObjectPropertyUnits, ev.SimulationObjectPropertyValue)
                        obj.SetPropertyValue(ev.SimulationObjectProperty, value)
                    Case Dynamics.DynamicsEventType.RunScript
                End Select
            End If
        Next

    End Sub

    Sub ProcessCEMatrix(cematrixID As String)

        Dim matrix = Flowsheet.DynamicsManager.CauseAndEffectMatrixList(cematrixID)

        For Each item In matrix.Items.Values
            If item.Enabled Then
                Dim indicator = DirectCast(Flowsheet.SimulationObjects(item.AssociatedIndicator), Interfaces.IIndicator)
                Select Case item.AssociatedIndicatorAlarm
                    Case Dynamics.DynamicsAlarmType.LL
                        If indicator.VeryLowAlarmActive Then DoAlarmEffect(item)
                    Case Dynamics.DynamicsAlarmType.L
                        If indicator.LowAlarmActive Then DoAlarmEffect(item)
                    Case Dynamics.DynamicsAlarmType.H
                        If indicator.HighAlarmActive Then DoAlarmEffect(item)
                    Case Dynamics.DynamicsAlarmType.HH
                        If indicator.VeryHighAlarmActive Then DoAlarmEffect(item)
                End Select
            End If
        Next

    End Sub

    Sub DoAlarmEffect(ceitem As Interfaces.IDynamicsCauseAndEffectItem)

        Dim obj = Flowsheet.SimulationObjects(ceitem.SimulationObjectID)
        Dim value = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ceitem.SimulationObjectPropertyUnits, ceitem.SimulationObjectPropertyValue)
        obj.SetPropertyValue(ceitem.SimulationObjectProperty, value)

    End Sub

    Private Sub btnRealtime_Click(sender As Object, e As EventArgs) Handles btnRealtime.Click

        If Flowsheet.DynamicMode Then
            RunIntegrator(True, False, False)
        Else
            Flowsheet.ShowMessage(DWSIM.App.GetLocalString("DynamicsDisabled"), Interfaces.IFlowsheet.MessageType.Warning)
        End If

    End Sub

    Public Sub RestoreState(stateID As String)

        Try

            Dim initialstate = Flowsheet.StoredSolutions(stateID)

            Flowsheet.LoadProcessData(initialstate)

            Flowsheet.UpdateInterface()

        Catch ex As Exception

            MessageBox.Show(String.Format("Error Restoring State {0}: {1}", stateID, ex.Message), "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Public Function RunIntegrator(realtime As Boolean, waittofinish As Boolean, restarting As Boolean) As Task

        btnRealtime.Enabled = False

        btnViewResults.Enabled = False

        Abort = False

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)
        Dim integrator = Flowsheet.DynamicsManager.IntegratorList(schedule.CurrentIntegrator)

        integrator.RealTime = realtime

        Dim Controllers = Flowsheet.SimulationObjects.Values.Where(Function(x) x.ObjectClass = SimulationObjectClass.Controllers).ToList

        If Not restarting Then
            If Not waittofinish Then
                If Not realtime Then
                    If Not schedule.UseCurrentStateAsInitial Then
                        RestoreState(schedule.InitialFlowsheetStateID)
                    End If
                End If
            End If
        End If

        If Not restarting Then ProgressBar1.Value = 0

        ProgressBar1.Minimum = 0

        If Not restarting Then integrator.MonitoredVariableValues.Clear()

        lblFinish.Text = integrator.Duration.ToString("c")

        If realtime Then

            ProgressBar1.Maximum = Integer.MaxValue
            ProgressBar1.Style = ProgressBarStyle.Marquee

        Else

            ProgressBar1.Maximum = integrator.Duration.TotalSeconds
            ProgressBar1.Style = ProgressBarStyle.Continuous

        End If

        Dim interval = integrator.IntegrationStep.TotalSeconds

        If realtime Then interval = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

        Dim final = ProgressBar1.Maximum

        If Not restarting Then
            For Each controller As PIDController In Controllers
                controller.Reset()
            Next
        End If

        If schedule.ResetContentsOfAllObjects And Not restarting Then
            For Each obj In Flowsheet.SimulationObjects.Values
                If obj.HasPropertiesForDynamicMode Then
                    If TypeOf obj Is BaseClass Then
                        Dim bobj = DirectCast(obj, BaseClass)
                        If bobj.GetDynamicProperty("Reset Content") IsNot Nothing Then
                            bobj.SetDynamicProperty("Reset Content", 1)
                        End If
                        If bobj.GetDynamicProperty("Reset Contents") IsNot Nothing Then
                            bobj.SetDynamicProperty("Reset Contents", 1)
                        End If
                        If bobj.GetDynamicProperty("Initialize using Inlet Stream") IsNot Nothing Then
                            bobj.SetDynamicProperty("Initialize using Inlet Stream", 1)
                        End If
                        If bobj.GetDynamicProperty("Initialize using Inlet Streams") IsNot Nothing Then
                            bobj.SetDynamicProperty("Initialize using Inlet Streams", 1)
                        End If
                    End If
                End If
            Next
        End If

        If Not restarting Then
            integrator.CurrentTime = New Date
            integrator.MonitoredVariableValues.Clear()
        End If

        Dim controllers_check As Double = 100000
        Dim streams_check As Double = 100000
        Dim pf_check As Double = 100000

        Flowsheet.SupressMessages = True

        Dim exceptions As New List(Of Exception)

        If Not restarting Then
            Flowsheet.ProcessScripts(Scripts.EventType.IntegratorStarted, Scripts.ObjectType.Integrator, "")
        End If

        Dim maintask = New Task(Sub()

                                    Running = True

                                    Dim i As Double = 0

                                    If restarting Then
                                        Flowsheet.RunCodeOnUIThread(Sub()
                                                                        i = ProgressBar1.Value
                                                                    End Sub)
                                        Application.DoEvents()
                                    End If

                                    While i <= final

                                        Dim sw As New Stopwatch
                                        sw.Start()

                                        Dim i0 As Integer = i

                                        Flowsheet.RunCodeOnUIThread(Sub()
                                                                        ProgressBar1.Value = i0
                                                                        lblCurrent.Text = New TimeSpan(0, 0, i0).ToString("c")
                                                                    End Sub)

                                        controllers_check += interval
                                        streams_check += interval
                                        pf_check += interval

                                        If controllers_check >= integrator.CalculationRateControl * interval Then
                                            controllers_check = 0.0
                                            integrator.ShouldCalculateControl = True
                                        Else
                                            integrator.ShouldCalculateControl = False
                                        End If

                                        If streams_check >= integrator.CalculationRateEquilibrium * interval Then
                                            streams_check = 0.0
                                            integrator.ShouldCalculateEquilibrium = True
                                        Else
                                            integrator.ShouldCalculateEquilibrium = False
                                        End If

                                        If pf_check >= integrator.CalculationRatePressureFlow * interval Then
                                            pf_check = 0.0
                                            integrator.ShouldCalculatePressureFlow = True
                                        Else
                                            integrator.ShouldCalculatePressureFlow = False
                                        End If

                                        exceptions = FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, GlobalSettings.Settings.SolverMode)

                                        While GlobalSettings.Settings.CalculatorBusy
                                            Task.Delay(200).Wait()
                                        End While

                                        If exceptions.Count > 0 Then Exit While

                                        StoreVariableValues(integrator, i, integrator.CurrentTime)

                                        Flowsheet.RunCodeOnUIThread(Sub()
                                                                        Flowsheet.FormDynamics.UpdateControllerList()
                                                                        Flowsheet.FormDynamics.UpdateIndicatorList()
                                                                        Flowsheet.FormSurface.FControl.Invalidate()
                                                                        Application.DoEvents()
                                                                    End Sub)

                                        integrator.CurrentTime = integrator.CurrentTime.AddSeconds(interval)

                                        If integrator.ShouldCalculateControl Then
                                            For Each controller As PIDController In Controllers
                                                If controller.Active Then
                                                    Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, controller.Name)
                                                    Try
                                                        controller.Solve()
                                                        Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, controller.Name)
                                                    Catch ex As Exception
                                                        Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, controller.Name)
                                                        Throw ex
                                                    End Try
                                                End If
                                            Next
                                        End If

                                        Dim waittime = integrator.RealTimeStepMs - sw.ElapsedMilliseconds

                                        If waittime > 0 And realtime Then
                                            Task.Delay(waittime).Wait()
                                        End If

                                        sw.Stop()

                                        If Abort Or Paused Then Exit While

                                        If Not realtime Then
                                            If schedule.UsesEventList Then
                                                ProcessEvents(schedule.CurrentEventList, integrator.CurrentTime, integrator.IntegrationStep)
                                            End If
                                            If schedule.UsesCauseAndEffectMatrix Then
                                                ProcessCEMatrix(schedule.CurrentCauseAndEffectMatrix)
                                            End If
                                        End If

                                        i += interval

                                    End While

                                    If exceptions.Count > 0 Then Throw exceptions(0)

                                End Sub)

        maintask.ContinueWith(Sub(t)
                                  If Not Paused Then Running = False
                                  Flowsheet.RunCodeOnUIThread(Sub()
                                                                  btnViewResults.Enabled = True
                                                                  btnRealtime.Enabled = True
                                                                  If Not Paused Then
                                                                      ProgressBar1.Value = 0
                                                                      ProgressBar1.Style = ProgressBarStyle.Continuous
                                                                  End If
                                                                  Flowsheet.SupressMessages = False
                                                                  Flowsheet.UpdateOpenEditForms()
                                                                  Dim baseexception As Exception
                                                                  If t.Exception IsNot Nothing Then
                                                                      btnRun.BackgroundImage = My.Resources.icons8_play
                                                                      Paused = False
                                                                      Running = False
                                                                      Flowsheet.ProcessScripts(Scripts.EventType.IntegratorError, Scripts.ObjectType.Integrator, "")
                                                                      For Each ex In t.Exception.Flatten().InnerExceptions
                                                                          Dim euid As String = Guid.NewGuid().ToString()
                                                                          SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, ex)
                                                                          If TypeOf ex Is AggregateException Then
                                                                              baseexception = ex.InnerException
                                                                              For Each iex In DirectCast(ex, AggregateException).Flatten().InnerExceptions
                                                                                  While iex.InnerException IsNot Nothing
                                                                                      baseexception = iex.InnerException
                                                                                  End While
                                                                                  Flowsheet.ShowMessage(baseexception.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError, euid)
                                                                              Next
                                                                          Else
                                                                              baseexception = ex
                                                                              If baseexception.InnerException IsNot Nothing Then
                                                                                  While baseexception.InnerException.InnerException IsNot Nothing
                                                                                      baseexception = baseexception.InnerException
                                                                                      If baseexception Is Nothing Then Exit While
                                                                                      If baseexception.InnerException Is Nothing Then Exit While
                                                                                  End While
                                                                              End If
                                                                              Flowsheet.ShowMessage(baseexception.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError, euid)
                                                                          End If
                                                                      Next
                                                                  Else
                                                                      Flowsheet.ProcessScripts(Scripts.EventType.IntegratorFinished, Scripts.ObjectType.Integrator, "")
                                                                  End If
                                                              End Sub)
                              End Sub)

        If waittofinish Then
            maintask.RunSynchronously(TaskScheduler.Default)
        Else
            maintask.Start(TaskScheduler.Default)
        End If

        Return maintask

    End Function

    Private Sub btnViewResults_Click(sender As Object, e As EventArgs) Handles btnViewResults.Click

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)

        Dim integrator = Flowsheet.DynamicsManager.IntegratorList(schedule.CurrentIntegrator)

        Dim sheet = Flowsheet.FormSpreadsheet.Spreadsheet.NewWorksheet()

        sheet.RowCount = integrator.MonitoredVariableValues.Count + 1

        sheet.Cells(0, 0).Data = "Time (ms)"

        Dim i, j As Integer

        i = 1
        For Each var In integrator.MonitoredVariables
            sheet.Cells(0, i).Data = var.Description & If(var.PropertyUnits <> "", " (" + var.PropertyUnits + ")", "")
            i += 1
        Next

        i = 1
        For Each item In integrator.MonitoredVariableValues
            sheet.Cells(i, 0).Data = New TimeSpan(item.Key).TotalMilliseconds
            j = 1
            For Each var In item.Value
                sheet.Cells(i, j).Data = var.PropertyValue.ToDoubleFromInvariant
                j += 1
            Next
            i += 1
        Next

#If LINUX Then
        'sheet.SetRangeDataFormat(New unvell.ReoGrid.RangePosition(
        '                         New unvell.ReoGrid.CellPosition(1, 1),
        '                         New unvell.ReoGrid.CellPosition(i - 1, j - 1)),
        '                         unvell.ReoGrid.DataFormat.CellDataFormatFlag.Number,
        '                         New unvell.ReoGrid.DataFormat.NumberDataFormatter.NumberFormatArgs With {
        '                            .DecimalPlaces = 4, .UseSeparator = False})
#Else
        sheet.SetRangeDataFormat(New unvell.ReoGrid.RangePosition(
                                 New unvell.ReoGrid.CellPosition(1, 1),
                                 New unvell.ReoGrid.CellPosition(i - 1, j - 1)),
                                 unvell.ReoGrid.DataFormat.CellDataFormatFlag.Number,
                                 New unvell.ReoGrid.DataFormat.NumberDataFormatter.NumberFormatArgs With {
                                    .DecimalPlaces = 4, .UseSeparator = False})
#End If

        Flowsheet.FormSpreadsheet.Activate()

        Flowsheet.FormSpreadsheet.Spreadsheet.CurrentWorksheet = sheet

    End Sub

    Private Sub FormDynamicsIntegratorControl_MouseEnter(sender As Object, e As EventArgs) Handles Me.MouseEnter

        If cbScenario.Items.Count <> Flowsheet.DynamicsManager.ScheduleList.Count Then

            cbScenario.Items.Clear()

            For Each item In Flowsheet.DynamicsManager.ScheduleList
                If item.Value.CurrentIntegrator <> "" Then
                    Dim integ = Flowsheet.DynamicsManager.IntegratorList(item.Value.CurrentIntegrator).Description
                    cbScenario.Items.Add(item.Value.Description & " (" & integ & ")")
                Else
                    cbScenario.Items.Add(item.Value.Description)
                End If
            Next

            cbScenario.SelectedIndex = 0

        End If

    End Sub

End Class