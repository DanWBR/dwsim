using Eto.Drawing;
using Eto.Forms;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using System.Linq;
using System.Collections.Generic;
using DWSIM.Interfaces;
using DWSIM.UnitOperations.UnitOperations;
using DotNumerics.Optimization;
using DWSIM.UnitOperations.SpecialOps;

namespace DWSIM.UI.Desktop.Editors.Dynamics
{
    public class PIDTuningTool : TableLayout
    {

        private Shared.Flowsheet Flowsheet;

        private DynamicsIntegratorControl intc;

        private IDynamicsSchedule schedule;

        private int iterations = 30;

        private TextArea txtResults;

        private bool Abort = false;

        public PIDTuningTool(Shared.Flowsheet fs, DynamicsIntegratorControl intcontrol) : base()
        {

            Flowsheet = fs;
            intc = intcontrol;
            Padding = new Eto.Drawing.Padding(5);
            Spacing = new Size(5, 5);

            Padding = new Eto.Drawing.Padding(5);
            Spacing = new Size(10, 10);

            var leftcontainer = new DynamicLayout { Width = 300 };

            leftcontainer.CreateAndAddLabelRow("Schedule");

            var schlist = Flowsheet.DynamicsManager.ScheduleList.Values.ToList();

            var cbSchedule = leftcontainer.CreateAndAddDropDownRow("Schedule", schlist.Select((x) => x.Description).ToList(), 0,
                (dd, e) => schedule = schlist[dd.SelectedIndex]);

            leftcontainer.CreateAndAddLabelRow("Controllers");

            leftcontainer.CreateAndAddDescriptionRow("Select the PID Controllers to tune.");

            var listb = new CheckBoxList() { Height = 200 };

            foreach (var obj in Flowsheet.SimulationObjects.Values.Where((x) => x.ObjectClass == Interfaces.Enums.SimulationObjectClass.Controllers))
            {
                listb.Items.Add(obj.GraphicObject.Tag, obj.Name);
            }

            leftcontainer.CreateAndAddControlRow(listb);

            leftcontainer.CreateAndAddNumericEditorRow2("Maximum Iterations", iterations, 5, 100, 0,
                (ns, e) =>
                {
                    iterations = (int)ns.Text.ToDoubleFromCurrent();
                });

            var btnRun = leftcontainer.CreateAndAddButtonRow("Begin Tuning", null, null);

            var btnCancel = leftcontainer.CreateAndAddButtonRow("Cancel", null, (btn, e) => Abort = true);

            txtResults = new TextArea { ReadOnly = true, Wrap = true };

            Rows.Add(new TableRow(leftcontainer, new Scrollable { Content = txtResults }));

            btnRun.Click += (s, e) =>
            {

                Flowsheet.RunCodeOnUIThread(() =>
                {

                    txtResults.Text = "";

                    if (!Flowsheet.DynamicMode)
                    {
                        txtResults.Text += "Error: Dynamic Mode is not activated. Activate Dynamic Mode and try again.";
                        return;
                    }

                    intc.cbsc.SelectedIndex = cbSchedule.SelectedIndex;

                    var schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule];

                    List<OptSimplexBoundVariable> vars = new List<OptSimplexBoundVariable>();
                    List<PIDController> controllers = new List<PIDController>();

                    foreach (var item in listb.SelectedKeys)
                    {
                        var controller = (PIDController)Flowsheet.SimulationObjects[item];
                        controllers.Add(controller);
                        vars.Add(new OptSimplexBoundVariable(controller.Kp, 0.0, controller.Kp * 10));
                        vars.Add(new OptSimplexBoundVariable(controller.Ki, 0.0, 100.0));
                        vars.Add(new OptSimplexBoundVariable(controller.Kd, 0.0, 100.0));
                    }

                    btnRun.Enabled = false;
                    btnCancel.Enabled = true;

                    Simplex simplex = new Simplex();

                    simplex.MaxFunEvaluations = iterations;

                    Abort = false;

                    int counter = 1;

                    if (schedule.InitialFlowsheetStateID == "" | schedule.UseCurrentStateAsInitial)
                    {
                        txtResults.Text += "The selected schedule must have a valid initial state to start from.";
                        btnRun.Enabled = true;
                        btnCancel.Enabled = false;
                        return;
                    }

                    var result = simplex.ComputeMin(x =>
                    {
                        if (Abort)
                            return 0.0;
                        Flowsheet.RunCodeOnUIThread(() =>
                        {
                            txtResults.Text += (string.Format("Beginning Iteration #{0}...\n", counter));
                        });
                        DynamicsIntegratorControl.RestoreState(Flowsheet, schedule.InitialFlowsheetStateID);
                        var i = 0;
                        foreach (var controller in controllers)
                        {
                            controller.Kp = x[i];
                            controller.Ki = x[i + 1];
                            controller.Kd = x[i + 2];
                            Flowsheet.RunCodeOnUIThread(() =>
                            {
                                txtResults.Text += (string.Format("Controller: {0} - Kp = {1}, Ki = {2}, Kd = {3}\n", controller.GraphicObject.Tag, controller.Kp, controller.Ki, controller.Kd));
                            });
                            i += 3;
                        }
                        DynamicsIntegratorControl.RunIntegrator(false, true, Flowsheet, intc);
                        var totalerror = controllers.Select(c => c.CumulativeError).ToArray().AbsSumY();
                        Flowsheet.RunCodeOnUIThread(() =>
                        {
                            txtResults.Text += (string.Format("Total Error: {0}\n", totalerror));
                            txtResults.CaretIndex = txtResults.Text.Length - 1;
                        });
                        Application.Instance.RunIteration();
                        counter += 1;
                        return totalerror;
                    }, vars.ToArray());

                    if (Abort)
                        txtResults.Text += (string.Format("Tuning aborted by the user. Results:\n"));
                    else
                        txtResults.Text += (string.Format("Tuning finished successfully. Results:\n"));

                    var j = 0;
                    foreach (var controller in controllers)
                    {
                        controller.Kp = result[j];
                        controller.Ki = result[j + 1];
                        controller.Kd = result[j + 2];
                        txtResults.Text += (string.Format("Controller: {0} - Kp = {1}, Ki = {2}, Kd = {3}\n", controller.GraphicObject.Tag, controller.Kp, controller.Ki, controller.Kd));
                        j += 3;
                    }

                    btnRun.Enabled = true;
                    btnCancel.Enabled = false;

                    Flowsheet.UpdateInterface();

                });

            };

        }

    }
}
