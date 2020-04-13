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

        For i = 0 To final Step interval

            TrackBar1.Value = i

            lblCurrent.Text = New TimeSpan(0, 0, i).ToString("c")

            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, 0)

            Flowsheet.UpdateInterface()

            Flowsheet.UpdateOpenEditForms()

            integrator.StoredSolutions.Add(j, Flowsheet.GetProcessData())

            integrator.CurrentTime = integrator.CurrentTime.AddSeconds(interval)

            For Each controller As PIDController In Controllers
                controller.Calculate()
            Next

            If Abort Then Exit For

            j += 1

            If schedule.UsesEventList Then

                ProcessEvents(schedule.CurrentEventList, integrator.CurrentTime, integrator.IntegrationStep)

            End If

        Next

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
            Select Case ev.EventType
                Case Dynamics.DynamicsEventType.ChangeProperty
                    Dim obj = Flowsheet.SimulationObjects(ev.SimulationObjectID)
                    Dim value = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ev.SimulationObjectPropertyUnits, ev.SimulationObjectPropertyValue)
                    obj.SetPropertyValue(ev.SimulationObjectProperty, value)
                Case Dynamics.DynamicsEventType.RunScript
            End Select
        Next

    End Sub

End Class