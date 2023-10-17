Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.DynamicsManager
Imports DWSIM.Interfaces
Imports OxyPlot
Imports OxyPlot.Axes
Imports OxyPlot.Series

Public Class FormDynamicsIntegratorControl

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Public Busy As Boolean = False

    Public Abort As Boolean = False

    Public LiveChart As New FormChart_OxyPlot

    Private ChartIsSetup As Boolean = False

    Private FlowsheetClone As IFlowsheet

    Private Historian As New Dictionary(Of Date, XDocument)

    Private Sub FormDynamicsIntegratorControl_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Flowsheet.DynamicsManager.ToggleDynamicMode = Function()
                                                          Flowsheet.DynamicMode = Not Flowsheet.DynamicMode
                                                          Return Flowsheet.DynamicMode
                                                      End Function

        Flowsheet.DynamicsManager.RunSchedule = Function(schname)
                                                    Flowsheet.DynamicsManager.CurrentSchedule = Flowsheet.DynamicsManager.GetSchedule(schname).ID
                                                    Return RunIntegrator(False, False, False, True)
                                                End Function

    End Sub

    Private Sub FormDynamicsIntegratorControl_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        If Visible Then

            cbScenario.Items.Clear()

            For Each item In Flowsheet.DynamicsManager.ScheduleList
                If item.Value.CurrentIntegrator <> "" Then
                    If Flowsheet.DynamicsManager.IntegratorList.ContainsKey(item.Value.CurrentIntegrator) Then
                        Dim integ = Flowsheet.DynamicsManager.IntegratorList(item.Value.CurrentIntegrator).Description
                        cbScenario.Items.Add(item.Value.Description & " (" & integ & ")")
                    Else
                        cbScenario.Items.Add(item.Value.Description)
                    End If
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
                ChartIsSetup = False
                Try
                    RunIntegrator(False, False, False, False)
                Catch ex As Exception
                    Running = False
                    btnRun.BackgroundImage = My.Resources.icons8_play
                    Throw ex
                End Try
            Else
                If Not Paused Then
                    Try
                        RunIntegrator(False, False, True, False)
                    Catch ex As Exception
                        Running = False
                        btnRun.BackgroundImage = My.Resources.icons8_play
                        Throw ex
                    End Try
                End If
            End If
        Else
            Flowsheet.ShowMessage(DWSIM.App.GetLocalString("DynamicsDisabled"), Interfaces.IFlowsheet.MessageType.Warning)
            Running = False
            btnRun.BackgroundImage = My.Resources.icons8_play
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
            If Not Flowsheet.SimulationObjects.ContainsKey(vnew.ObjectID) Then
                Throw New Exception(Flowsheet.GetTranslatedString1("At least one of the monitored variables is not configured correctly, please check."))
            End If
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

        Dim props = Flowsheet.DynamicsManager.GetPropertyValuesFromEvents(FlowsheetClone, currentposition, Historian, eventset)

        For Each p In props
            Dim obj = Flowsheet.SimulationObjects(p.Item1)
            Dim value = p.Item3
            obj.SetPropertyValue(p.Item2, value)
        Next

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
            RunIntegrator(True, False, False, False)
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

    Public Function RunIntegrator(realtime As Boolean, waittofinish As Boolean, restarting As Boolean, guiless As Boolean) As Task

        ChartIsSetup = False

        Abort = False

        If Not Flowsheet.DynamicsManager.ScheduleList.ContainsKey(Flowsheet.DynamicsManager.CurrentSchedule) Then
            Throw New Exception(Flowsheet.GetTranslatedString1("Please select a valid schedule."))
        End If

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)

        If Not Flowsheet.DynamicsManager.IntegratorList.ContainsKey(schedule.CurrentIntegrator) Then
            Throw New Exception(Flowsheet.GetTranslatedString1("Please select a valid integrator for the selected schedule."))
        End If

        Flowsheet.ShowMessage(DWSIM.App.GetLocalString("Dynamics Integrator Starting..."), Interfaces.IFlowsheet.MessageType.Information)

        Dim integrator = Flowsheet.DynamicsManager.IntegratorList(schedule.CurrentIntegrator)

        integrator.RealTime = realtime

        Dim Controllers = Flowsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is PIDController).ToList

        Dim Controllers2 = Flowsheet.SimulationObjects.Values.Where(Function(x) TypeOf x Is PythonController).ToList

        If Not restarting Then
            If Not waittofinish Then
                If Not realtime Then
                    If Not schedule.UseCurrentStateAsInitial Then
                        RestoreState(schedule.InitialFlowsheetStateID)
                    End If
                End If
            End If
        End If

        Dim final As Integer

        If Not guiless Then

            btnRealtime.Enabled = False

            btnViewResults.Enabled = False

            If Not restarting Then ProgressBar1.Value = 0

            ProgressBar1.Minimum = 0

            lblFinish.Text = integrator.Duration.ToString("c")

            If realtime Then

                ProgressBar1.Maximum = Integer.MaxValue
                ProgressBar1.Style = ProgressBarStyle.Marquee

            Else

                ProgressBar1.Maximum = integrator.Duration.TotalSeconds
                ProgressBar1.Style = ProgressBarStyle.Continuous

            End If

            final = ProgressBar1.Maximum

        End If

        If Not restarting Then integrator.MonitoredVariableValues.Clear()

        Dim interval = integrator.IntegrationStep.TotalSeconds

        If realtime Then interval = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0

        If Not restarting Then
            For Each controller As PIDController In Controllers
                controller.Reset()
            Next
            For Each controller As PythonController In Controllers2
                controller.ResetRequested = True
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

        FlowsheetClone = Flowsheet.Clone()

        Historian = New Dictionary(Of Date, XDocument)()

        Dim exceptions As New List(Of Exception)

        If Not restarting Then
            Flowsheet.ProcessScripts(Scripts.EventType.IntegratorStarted, Scripts.ObjectType.Integrator, "")
        End If

        Dim maintask = New Task(Sub()

                                    Running = True

                                    Dim i As Double = 0

                                    If restarting And Not guiless Then
                                        Flowsheet.RunCodeOnUIThread(Sub()
                                                                        Try
                                                                            i = ProgressBar1.Value
                                                                        Catch ex As Exception
                                                                        End Try
                                                                    End Sub)
                                        Application.DoEvents()
                                    End If

                                    While i <= final

                                        Dim sw As New Stopwatch
                                        sw.Start()

                                        Flowsheet.ProcessScripts(Scripts.EventType.IntegratorPreStep, Scripts.ObjectType.Integrator, "")

                                        Dim i0 As Integer = i

                                        If Not guiless Then
                                            Flowsheet.RunCodeOnUIThread(Sub()
                                                                            ProgressBar1.Value = i0
                                                                            lblCurrent.Text = New TimeSpan(0, 0, i0).ToString("c")
                                                                        End Sub)
                                        End If

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

                                        Historian.Add(integrator.CurrentTime, Flowsheet.GetSnapshot(SnapshotType.ObjectData))

                                        StoreVariableValues(integrator, i, integrator.CurrentTime)

                                        Flowsheet.ProcessScripts(Scripts.EventType.IntegratorStep, Scripts.ObjectType.Integrator, "")

                                        If Not guiless Then
                                            Flowsheet.RunCodeOnUIThread(Sub()
                                                                            Flowsheet.FormDynamics.UpdateControllerList()
                                                                            Flowsheet.FormDynamics.UpdateIndicatorList()
                                                                            Flowsheet.FormSurface.FControl.Invalidate()
                                                                            Application.DoEvents()
                                                                        End Sub)
                                        End If

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
                                            For Each controller As PythonController In Controllers2
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

                                        If Not guiless Then
                                            Flowsheet.RunCodeOnUIThread(Sub()
                                                                            If Not ChartIsSetup Then SetupChart(integrator)
                                                                            UpdateChart(integrator)
                                                                        End Sub)
                                        End If

                                    End While

                                    If exceptions.Count > 0 Then Throw exceptions(0)

                                End Sub)

        maintask.ContinueWith(Sub(t)
                                  If Not Paused Then Running = False
                                  If t.Exception IsNot Nothing Then
                                      Flowsheet.ProcessScripts(Scripts.EventType.IntegratorError, Scripts.ObjectType.Integrator, "")
                                  Else
                                      Flowsheet.ProcessScripts(Scripts.EventType.IntegratorFinished, Scripts.ObjectType.Integrator, "")
                                  End If
                                  If Not guiless Then
                                      Flowsheet.RunCodeOnUIThread(Sub()
                                                                      btnViewResults.Enabled = True
                                                                      btnRealtime.Enabled = True
                                                                      btnRun.BackgroundImage = My.Resources.icons8_play
                                                                      If Not Paused Then
                                                                          ProgressBar1.Value = 0
                                                                          ProgressBar1.Style = ProgressBarStyle.Continuous
                                                                      End If
                                                                      Flowsheet.SupressMessages = False
                                                                      Flowsheet.UpdateOpenEditForms()
                                                                      Dim baseexception As Exception
                                                                      If t.Exception IsNot Nothing Then
                                                                          Paused = False
                                                                          Running = False
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
                                                                      End If
                                                                  End Sub)
                                  End If
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

    Private Sub btnLiveChart_Click(sender As Object, e As EventArgs) Handles btnLiveChart.Click

        LiveChart.Text = "Live View - " + cbScenario.SelectedItem.ToString()
        LiveChart.TabText = LiveChart.Text
        LiveChart.Show(Flowsheet.GetDockPanel())

    End Sub

    Public Sub UpdateChart(integrator As Interfaces.IDynamicsIntegrator)

        Dim i As Integer
        Dim x, y As Double

        Dim model = LiveChart.PlotView1.Model

        Dim pointset = integrator.MonitoredVariableValues.Last

        x = New TimeSpan(pointset.Key).TotalMilliseconds

        i = 0
        For Each var In pointset.Value
            If Double.TryParse(var.PropertyValue, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture, New Double) Then
                y = var.PropertyValue.ToDoubleFromInvariant()
            Else
                y = Double.NaN
            End If
            Dim series = DirectCast(model.Series(i), LineSeries)
            series.Points.Add(New DataPoint(x, y))
            If series.Points.Count > 100 Then series.Points.RemoveAt(0)
            i += 1
        Next

        LiveChart.PlotView1.InvalidatePlot(True)

    End Sub

    Public Sub SetupChart(integrator As Interfaces.IDynamicsIntegrator)

        Dim model = New PlotModel() With {.Subtitle = "Live View", .Title = integrator.Description}

        model.TitleFontSize = 16
        model.SubtitleFontSize = 14
        model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinPlotArea

        model.Axes.Add(New LinearAxis() With {
            .MajorGridlineStyle = LineStyle.Dash,
            .MinorGridlineStyle = LineStyle.Dot,
            .Position = AxisPosition.Bottom,
            .FontSize = 14,
            .Title = "Time (ms)"
        })

        model.LegendFontSize = 12
        model.LegendPlacement = LegendPlacement.Outside
        model.LegendOrientation = LegendOrientation.Horizontal
        model.LegendPosition = LegendPosition.TopCenter
        model.LegendBorderThickness = 0.0

        Dim vx As New List(Of Double)(), vy As New List(Of Double)()
        Dim vya As New List(Of List(Of Double))()
        Dim vn As New List(Of String)()

        Dim i As Integer

        i = 0
        For Each item In integrator.MonitoredVariables
            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Left,
                .FontSize = 14,
                .Key = item.ID,
                .Title = integrator.MonitoredVariables(i).Description + If(item.PropertyUnits <> "", " (" + item.PropertyUnits + ")", ""),
                .PositionTier = i,
                .AxislineStyle = LineStyle.Solid
            })
            i += 1
        Next

        Dim lastset = integrator.MonitoredVariableValues

        i = 0
        For Each item In lastset.Values(0)
            model.AddLineSeries(New List(Of Double), New List(Of Double),
                                integrator.MonitoredVariables(i).Description &
                                If(integrator.MonitoredVariables(i).PropertyUnits <> "", " (" +
                                integrator.MonitoredVariables(i).PropertyUnits + ")", ""))
            DirectCast(model.Series(model.Series.Count - 1), LineSeries).Tag = integrator.MonitoredVariables(i).ID
            DirectCast(model.Series(model.Series.Count - 1), LineSeries).YAxisKey = integrator.MonitoredVariables(i).ID
            i += 1
        Next

        LiveChart.PlotView1.Model = model

        ChartIsSetup = True

    End Sub

    Private Sub FormDynamicsIntegratorControl_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class