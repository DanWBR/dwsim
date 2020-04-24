Imports System.Linq

Public Class FormPIDTuning

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Sub FormPIDTuning_Load(sender As Object, e As EventArgs) Handles Me.Load

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

End Class