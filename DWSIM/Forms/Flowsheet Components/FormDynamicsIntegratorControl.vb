Imports System.Linq

Public Class FormDynamicsIntegratorControl

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Public Busy As Boolean = False

    Public Abort As Boolean = False

    Private Sub FormDynamicsIntegratorControl_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub FormDynamicsIntegratorControl_VisibleChanged(sender As Object, e As EventArgs) Handles Me.VisibleChanged

        If Visible Then

            cbScenario.Items.Clear()

            For Each item In Flowsheet.DynamicsManager.IntegratorList
                cbScenario.Items.Add(item.Value.Description)
            Next

            If cbScenario.Items.Count > 0 Then
                cbScenario.SelectedIndex = 0
            End If

        End If

    End Sub

    Private Sub cbScenario_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbScenario.SelectedIndexChanged
        Flowsheet.DynamicsManager.CurrentSchedule = Flowsheet.DynamicsManager.ScheduleList.Values.ToList(cbScenario.SelectedIndex).ID
    End Sub

    Private Sub btnRun_Click(sender As Object, e As EventArgs) Handles btnRun.Click

        btnRun.Enabled = False

        Abort = False

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)

        Dim integrator = Flowsheet.DynamicsManager.IntegratorList(schedule.CurrentIntegrator)

        Dim Controllers = Flowsheet.SimulationObjects.Values.Where(Function(x) x.ObjectClass = SimulationObjectClass.Controllers).ToList

        If Not schedule.UseCurrentStateAsInitial Then

            Dim initialstate = Flowsheet.StoredSolutions(schedule.InitialFlowsheetStateID)

            Flowsheet.LoadProcessData(initialstate)

            Flowsheet.UpdateInterface()

        End If

        TrackBar1.Value = 0

        TrackBar1.Minimum = 0

        integrator.StoredSolutions.Clear()

        lblFinish.Text = integrator.Duration.ToString("c")

        TrackBar1.Maximum = integrator.Duration.TotalSeconds

        Dim interval = integrator.IntegrationStep.TotalSeconds

        TrackBar1.TickFrequency = interval

        Dim final = integrator.Duration.TotalSeconds

        For Each controller As PIDController In Controllers
            controller.Reset()
        Next

        Dim j As Integer = 0

        integrator.CurrentTime = New Date

        Dim controllers_check As Double = 100000
        Dim streams_check As Double = 100000
        Dim pf_check As Double = 100000

        TrackBar1.Enabled = False

        For i = 0 To final Step interval

            TrackBar1.Value = i

            lblCurrent.Text = New TimeSpan(0, 0, i).ToString("c")

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

            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, 0)

            Flowsheet.UpdateInterface()

            Flowsheet.FormDynamics.UpdateControllerList()

            Flowsheet.FormDynamics.UpdateIndicatorList()

            Flowsheet.UpdateInterface()

            Flowsheet.UpdateOpenEditForms()

            integrator.StoredSolutions.Add(j, Flowsheet.GetProcessData())

            integrator.CurrentTime = integrator.CurrentTime.AddSeconds(interval)

            If integrator.ShouldCalculateControl Then
                For Each controller As PIDController In Controllers
                    If controller.Active Then controller.Calculate()
                Next
            End If

            If Abort Then Exit For

            If schedule.UsesEventList Then

                ProcessEvents(schedule.CurrentEventList, integrator.CurrentTime, integrator.IntegrationStep)

            End If

            If schedule.UsesCauseAndEffectMatrix Then

                ProcessCEMatrix(schedule.CurrentCauseAndEffectMatrix)

            End If

            j += 1

        Next

        TrackBar1.Enabled = True

        btnRun.Enabled = True

    End Sub

    Private Sub btnStop_Click(sender As Object, e As EventArgs) Handles btnStop.Click

        Abort = True

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

End Class