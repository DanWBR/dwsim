Imports System.Linq

Public Class FormDynamicsIntegratorControl

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

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

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)

        Dim integrator = Flowsheet.DynamicsManager.IntegratorList(schedule.CurrentIntegrator)

        Dim initialstate = Flowsheet.StoredSolutions(schedule.InitialFlowsheetStateID)

        Flowsheet.LoadProcessData(initialstate)

        Flowsheet.UpdateInterface()

        TrackBar1.Value = 0

        TrackBar1.Minimum = 0

        integrator.StoredSolutions.Clear()

        lblFinish.Text = integrator.Duration.ToString("c")

        TrackBar1.Maximum = integrator.Duration.TotalSeconds

        Dim interval = integrator.IntegrationStep.TotalSeconds

        TrackBar1.TickFrequency = interval

        Dim final = integrator.Duration.TotalSeconds

        For i = 0 To final Step interval

            TrackBar1.Value = i

            lblCurrent.Text = New TimeSpan(0, 0, i).ToString("c")

            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, 0)

            integrator.StoredSolutions.Add(New TimeSpan(0, 0, i), Flowsheet.GetProcessData())

            integrator.CurrentTime = integrator.CurrentTime.AddSeconds(interval)

        Next

    End Sub

End Class