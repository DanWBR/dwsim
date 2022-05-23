Imports System.CodeDom
Imports System.Linq
Imports System.Threading.Tasks
Imports DotNumerics.Optimization
Imports DWSIM.ExtensionMethods

Public Class FormPIDTuning

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Abort As Boolean = False

    Private Sub FormPIDTuning_Load(sender As Object, e As EventArgs) Handles Me.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        lbControllers.Items.Clear()

        Dim Controllers = Flowsheet.SimulationObjects.Values.Where(Function(x) x.ObjectClass = SimulationObjectClass.Controllers).ToList

        For Each controller In Controllers
            lbControllers.Items.Add(controller.GraphicObject.Tag)
        Next

        For Each sch In Flowsheet.DynamicsManager.ScheduleList.Values
            cbScenario.Items.Add(sch.Description)
        Next

        If cbScenario.Items.Count > 0 Then cbScenario.SelectedIndex = 0

        cbSolver.SelectedIndex = 0

    End Sub

    Private Sub btnRun_Click(sender As Object, e As EventArgs) Handles btnRun.Click

        tbResults.Text = ""

        If Not Flowsheet.DynamicMode Then
            tbResults.AppendText("Error: Dynamic Mode is not activated. Activate Dynamic Mode and try again.")
            Exit Sub
        End If

        Flowsheet.FormIntegratorControls.cbScenario.SelectedItem = cbScenario.SelectedItem

        Dim schedule = Flowsheet.DynamicsManager.ScheduleList(Flowsheet.DynamicsManager.CurrentSchedule)

        Dim vars As New List(Of OptSimplexBoundVariable)
        Dim controllers As New List(Of PIDController)

        For Each item In lbControllers.CheckedItems
            Dim controller = DirectCast(Flowsheet.GetFlowsheetSimulationObject(item), PIDController)
            controllers.Add(controller)
            vars.Add(New OptSimplexBoundVariable(controller.Kp, 0.0, controller.Kp * 10))
            vars.Add(New OptSimplexBoundVariable(controller.Ki, 0.0, 100.0))
            vars.Add(New OptSimplexBoundVariable(controller.Kd, 0.0, 100.0))
        Next

        btnRun.Enabled = False
        btnCancel.Enabled = True


        Dim simplex As New Simplex

        simplex.MaxFunEvaluations = NumericUpDown1.Value

        Abort = False

        Dim counter As Integer = 1

        If schedule.InitialFlowsheetStateID = "" Or schedule.UseCurrentStateAsInitial Then
            tbResults.AppendText("The selected schedule must have a valid initial state to start from.")
            btnRun.Enabled = True
            btnCancel.Enabled = False
            Exit Sub
        End If

        Flowsheet.FormIntegratorControls.Show(Flowsheet.GetDockPanel)

        Dim result = simplex.ComputeMin(Function(x)
                                            If Abort Then Return 0.0
                                            Flowsheet.RunCodeOnUIThread(Sub()
                                                                            tbResults.AppendText(String.Format("Beginning Iteration #{0}..." + vbCrLf, counter))
                                                                        End Sub)
                                            Flowsheet.FormIntegratorControls.RestoreState(schedule.InitialFlowsheetStateID)
                                            Dim i = 0
                                            For Each controller In controllers
                                                controller.Kp = x(i)
                                                controller.Ki = x(i + 1)
                                                controller.Kd = x(i + 2)
                                                Flowsheet.RunCodeOnUIThread(Sub()
                                                                                tbResults.AppendText(String.Format("Controller: {0} - Kp = {1}, Ki = {2}, Kd = {3}" + vbCrLf, controller.GraphicObject.Tag, controller.Kp, controller.Ki, controller.Kd))
                                                                            End Sub)
                                                i += 3
                                            Next
                                            Flowsheet.FormIntegratorControls.RunIntegrator(False, True, False, False)
                                            Dim totalerror = controllers.Select(Function(c) c.CumulativeError).ToArray.AbsSumY
                                            Flowsheet.RunCodeOnUIThread(Sub()
                                                                            tbResults.AppendText(String.Format("Total Error: {0}" + vbCrLf, totalerror))
                                                                        End Sub)
                                            counter += 1
                                            Return totalerror
                                        End Function, vars.ToArray)

        If Abort Then
            tbResults.AppendText(String.Format("Tuning aborted by the user. Results:" + vbCrLf))
        Else
            tbResults.AppendText(String.Format("Tuning finished successfully. Results:" + vbCrLf))
        End If

        Dim j = 0
        For Each controller In controllers
            controller.Kp = result(j)
            controller.Ki = result(j + 1)
            controller.Kd = result(j + 2)
            tbResults.AppendText(String.Format("Controller: {0} - Kp = {1}, Ki = {2}, Kd = {3}" + vbCrLf, controller.GraphicObject.Tag, controller.Kp, controller.Ki, controller.Kd))
            j += 3
        Next

        btnRun.Enabled = True
        btnCancel.Enabled = False

        Flowsheet.UpdateOpenEditForms()
        Flowsheet.UpdateInterface()

    End Sub

    Private Sub btnCancel_Click(sender As Object, e As EventArgs) Handles btnCancel.Click
        Abort = True
    End Sub

End Class