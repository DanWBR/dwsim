using Eto.Drawing;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.ExtensionMethods;
using DWSIM.Interfaces.Enums;
using DWSIM.UnitOperations.SpecialOps;
using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using System.Diagnostics;
using DWSIM.SharedClasses;

namespace DWSIM.UI.Desktop.Editors.Dynamics
{
    public class DynamicsIntegratorControl : TableLayout
    {

        public DropDown cbsc;

        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        private Shared.Flowsheet Flowsheet;

        public Button btnPlay, btnRT, btnStop, btnViewResults;

        private ProgressBar pbProgress;

        private Label lbStatus;

        public bool Abort = false;

        public DynamicsIntegratorControl(DWSIM.UI.Desktop.Shared.Flowsheet fs)
            : base()
        {
            Flowsheet = fs;
        }

        public void Init()
        {

            var tl1 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr1 = new TableRow();

            tr1.Cells.Add(new Label { Text = "Schedule", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center });

            cbsc = new DropDown { Width = 300 };

            if (cbsc.Items.Count > 0) cbsc.SelectedIndex = 0;

            cbsc.SelectedIndexChanged += (s, e) =>
            {
                if (cbsc.SelectedIndex < 0) return;
                Flowsheet.DynamicsManager.CurrentSchedule = Flowsheet.DynamicsManager.ScheduleList.Values.ToList()[cbsc.SelectedIndex].ID;
            };

            tr1.Cells.Add(cbsc);

            tr1.Cells.Add(null);

            btnViewResults = new Button { Text = "View Results" };

            tr1.Cells.Add(btnViewResults);

            tl1.Rows.Add(tr1);

            Rows.Add(new TableRow(tl1));

            var tl2 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr2 = new TableRow();

            btnPlay = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png", this.GetType().Assembly)).WithSize(30, 30) };
            btnRT = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-realtime.png", this.GetType().Assembly)).WithSize(30, 30) };
            btnStop = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-stop.png", this.GetType().Assembly)).WithSize(30, 30) };

            pbProgress = new ProgressBar { MinValue = 0, MaxValue = 100 };

            lbStatus = new Label { Text = "00:00:00/00:30:00", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };

            tr2.Cells.Add(btnPlay);
            tr2.Cells.Add(btnRT);
            tr2.Cells.Add(btnStop);
            tr2.Cells.Add(lbStatus);
            tr2.Cells.Add(pbProgress);

            tl2.Rows.Add(tr2);

            Rows.Add(new TableRow(tl2));

            Rows.Add(null);

            this.MouseEnter += (s, e) =>
            {
                var prevsel = -1;
                if (cbsc.Items.Count > 0) prevsel = cbsc.SelectedIndex;
                cbsc.Items.Clear();
                foreach (var sch in Flowsheet.DynamicsManager.ScheduleList.Values)
                {
                    if (sch.CurrentIntegrator != "")
                    {
                        var integ = Flowsheet.DynamicsManager.IntegratorList[sch.CurrentIntegrator].Description;
                        cbsc.Items.Add(sch.Description + " (" + integ + ")");
                    }
                    else
                    {
                        cbsc.Items.Add(sch.Description);
                    }
                }
                if (cbsc.Items.Count > 0) cbsc.SelectedIndex = 0;
                try
                {
                    if (prevsel != -1) cbsc.SelectedIndex = prevsel;
                }
                catch { }
            };

            btnStop.Click += (s, e) =>
            {
                Abort = true;
            };

            btnPlay.Click += (s, e) =>
            {
                if (Flowsheet.DynamicMode) { RunIntegrator(false, false, Flowsheet, this); } else { Flowsheet.ShowMessage("Dynamic Mode is inactive.", Interfaces.IFlowsheet.MessageType.Warning); }
            };

            btnRT.Click += (s, e) =>
            {
                if (Flowsheet.DynamicMode) { RunIntegrator(true, false, Flowsheet, this); } else { Flowsheet.ShowMessage("Dynamic Mode is inactive.", Interfaces.IFlowsheet.MessageType.Warning); }
            };

            btnViewResults.Click += btnViewResults_Click;

        }

        public void Populate()
        {
            foreach (var sch in Flowsheet.DynamicsManager.ScheduleList.Values)
            {
                if (sch.CurrentIntegrator != "")
                {
                    var integ = Flowsheet.DynamicsManager.IntegratorList[sch.CurrentIntegrator].Description;
                    cbsc.Items.Add(sch.Description + " (" + integ + ")");
                }
                else
                {
                    cbsc.Items.Add(sch.Description);
                }
            }
        }

        public static void StoreVariableValues(Interfaces.IFlowsheet Flowsheet, DynamicsManager.Integrator integrator, int tstep, DateTime tstamp)
        {
            List<Interfaces.IDynamicsMonitoredVariable> list = new List<Interfaces.IDynamicsMonitoredVariable>();

            foreach (DynamicsManager.MonitoredVariable v in integrator.MonitoredVariables)
            {
                var vnew = (DynamicsManager.MonitoredVariable)v.Clone();
                var sobj = Flowsheet.SimulationObjects[vnew.ObjectID];
                vnew.PropertyValue = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(vnew.PropertyUnits, sobj.GetPropertyValue(vnew.PropertyID).ToString().ToDoubleFromInvariant()).ToString(System.Globalization.CultureInfo.InvariantCulture);
                vnew.TimeStamp = tstamp;
                list.Add(vnew);
            }

            integrator.MonitoredVariableValues.Add(tstep, list);
        }

        public static void ProcessEvents(Interfaces.IFlowsheet Flowsheet, string eventsetID, DateTime currentposition, TimeSpan interval)
        {
            var eventset = Flowsheet.DynamicsManager.EventSetList[eventsetID];

            var initialtime = currentposition - interval;

            var finaltime = currentposition;

            var events = eventset.Events.Values.Where(x => x.TimeStamp >= initialtime & x.TimeStamp < finaltime).ToList();

            foreach (var ev in events)
            {
                if (ev.Enabled)
                {
                    switch (ev.EventType)
                    {
                        case Interfaces.Enums.Dynamics.DynamicsEventType.ChangeProperty:
                            var obj = Flowsheet.SimulationObjects[ev.SimulationObjectID];
                            var value = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ev.SimulationObjectPropertyUnits, ev.SimulationObjectPropertyValue.ToDoubleFromInvariant());
                            obj.SetPropertyValue(ev.SimulationObjectProperty, value);
                            break;
                        case Interfaces.Enums.Dynamics.DynamicsEventType.RunScript:
                            break;
                    }
                }
            }
        }

        public static void ProcessCEMatrix(Interfaces.IFlowsheet Flowsheet, string cematrixID)
        {
            var matrix = Flowsheet.DynamicsManager.CauseAndEffectMatrixList[cematrixID];

            foreach (var item in matrix.Items.Values)
            {
                if (item.Enabled)
                {
                    var indicator = (Interfaces.IIndicator)Flowsheet.SimulationObjects[item.AssociatedIndicator];
                    switch (item.AssociatedIndicatorAlarm)
                    {
                        case Interfaces.Enums.Dynamics.DynamicsAlarmType.LL:
                            if (indicator.VeryLowAlarmActive)
                                DoAlarmEffect(Flowsheet, item);
                            break;
                        case Interfaces.Enums.Dynamics.DynamicsAlarmType.L:
                            if (indicator.LowAlarmActive)
                                DoAlarmEffect(Flowsheet, item);
                            break;
                        case Interfaces.Enums.Dynamics.DynamicsAlarmType.H:
                            if (indicator.HighAlarmActive)
                                DoAlarmEffect(Flowsheet, item);
                            break;
                        case Interfaces.Enums.Dynamics.DynamicsAlarmType.HH:
                            if (indicator.VeryHighAlarmActive)
                                DoAlarmEffect(Flowsheet, item);
                            break;
                    }
                }
            }
        }

        public static void DoAlarmEffect(Interfaces.IFlowsheet Flowsheet, Interfaces.IDynamicsCauseAndEffectItem ceitem)
        {
            var obj = Flowsheet.SimulationObjects[ceitem.SimulationObjectID];
            var value = SharedClasses.SystemsOfUnits.Converter.ConvertToSI(ceitem.SimulationObjectPropertyUnits, ceitem.SimulationObjectPropertyValue.ToDoubleFromInvariant());
            obj.SetPropertyValue(ceitem.SimulationObjectProperty, value);
        }

        private void btnRealtime_Click(object sender, EventArgs e)
        {
            if (Flowsheet.DynamicMode)
                RunIntegrator(true, false, Flowsheet, this);
        }

        public static void RestoreState(Interfaces.IFlowsheet Flowsheet, string stateID)
        {
            try
            {
                var initialstate = Flowsheet.StoredSolutions[stateID];
                Flowsheet.LoadProcessData(initialstate);
                Flowsheet.UpdateInterface();
            }
            catch (Exception ex)
            {
                MessageBox.Show(String.Format("Error Restoring State {0}: {1}", stateID, ex.Message), "Error", MessageBoxType.Error);
            }
        }

        public static Task RunIntegrator(bool realtime, bool waittofinish, Interfaces.IFlowsheet Flowsheet, DynamicsIntegratorControl control)
        {

            if (control != null)
            {
                control.btnPlay.Enabled = false;

                control.btnRT.Enabled = false;

                control.btnViewResults.Enabled = false;

                control.Abort = false;
            }

            var schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule];

            var integrator = Flowsheet.DynamicsManager.IntegratorList[schedule.CurrentIntegrator];

            integrator.RealTime = realtime;

            var Controllers = Flowsheet.SimulationObjects.Values.Where(x => x is PIDController).ToList();
            var Controllers2 = Flowsheet.SimulationObjects.Values.Where(x => x is PythonController).ToList();

            if (!waittofinish)
                if (!realtime)
                    if (!schedule.UseCurrentStateAsInitial)
                        RestoreState(Flowsheet, schedule.InitialFlowsheetStateID);


            if (control != null)
            {
                control.pbProgress.Value = 0;
                control.pbProgress.MinValue = 0;
                control.lbStatus.Text = "00:00:00/" + integrator.Duration.ToString("c");
            }

            integrator.MonitoredVariableValues.Clear();

            if (control != null)
            {
                if (realtime)
                    control.pbProgress.MaxValue = int.MaxValue;
                else
                    control.pbProgress.MaxValue = (int)integrator.Duration.TotalSeconds;
            }

            var interval = integrator.IntegrationStep.TotalSeconds;

            if (realtime)
                interval = Convert.ToDouble(integrator.RealTimeStepMs) / 1000.0;

            var final = control.pbProgress.MaxValue;

            foreach (PIDController controller in Controllers)
                controller.Reset();

            foreach (PythonController controller in Controllers2)
                controller.ResetRequested = true;

            if (schedule.ResetContentsOfAllObjects)
            {
                foreach (var obj in Flowsheet.SimulationObjects.Values)
                {
                    if (obj.HasPropertiesForDynamicMode)
                    {
                        if (obj is DWSIM.SharedClasses.UnitOperations.BaseClass)
                        {
                            var bobj = (DWSIM.SharedClasses.UnitOperations.BaseClass)obj;
                            if (bobj.GetDynamicProperty("Reset Content") != null)
                                bobj.SetDynamicProperty("Reset Content", 1);
                            if (bobj.GetDynamicProperty("Reset Contents") != null)
                                bobj.SetDynamicProperty("Reset Contents", 1);
                            if (bobj.GetDynamicProperty("Initialize using Inlet Stream") != null)
                                bobj.SetDynamicProperty("Initialize using Inlet Stream", 1);
                            if (bobj.GetDynamicProperty("Initialize using Inlet Streams") != null)
                                bobj.SetDynamicProperty("Initialize using Inlet Streams", 1);
                        }
                    }
                }
            }

            integrator.CurrentTime = new DateTime();

            integrator.MonitoredVariableValues.Clear();

            double controllers_check = 100000;
            double streams_check = 100000;
            double pf_check = 100000;

            Flowsheet.SupressMessages = true;

            var exceptions = new List<Exception>();

            var maintask = new Task(() =>
            {
                int j = 0;

                double i = 0;

                Flowsheet.ProcessScripts(Scripts.EventType.IntegratorStarted, Scripts.ObjectType.Integrator, "");

                while (i <= final)
                {
                    int i0 = (int)i;

                    var sw = new Stopwatch();

                    sw.Start();

                    if (control != null)
                    {
                        Flowsheet.RunCodeOnUIThread(() =>
                        {
                            control.pbProgress.Value = i0;
                            control.lbStatus.Text = new TimeSpan(0, 0, i0).ToString("c") + "/" + integrator.Duration.ToString("c"); ;
                        });
                    }

                    controllers_check += interval;
                    streams_check += interval;
                    pf_check += interval;

                    if (controllers_check >= integrator.CalculationRateControl * interval)
                    {
                        controllers_check = 0.0;
                        integrator.ShouldCalculateControl = true;
                    }
                    else
                        integrator.ShouldCalculateControl = false;

                    if (streams_check >= integrator.CalculationRateEquilibrium * interval)
                    {
                        streams_check = 0.0;
                        integrator.ShouldCalculateEquilibrium = true;
                    }
                    else
                        integrator.ShouldCalculateEquilibrium = false;

                    if (pf_check >= integrator.CalculationRatePressureFlow * interval)
                    {
                        pf_check = 0.0;
                        integrator.ShouldCalculatePressureFlow = true;
                    }
                    else
                        integrator.ShouldCalculatePressureFlow = false;

                    GlobalSettings.Settings.CalculatorActivated = true;
                    GlobalSettings.Settings.CalculatorBusy = false;

                    exceptions = FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Flowsheet, GlobalSettings.Settings.SolverMode);

                    while (GlobalSettings.Settings.CalculatorBusy)
                        Task.Delay(200).Wait();

                    if (exceptions.Count > 0) break;

                    StoreVariableValues(Flowsheet, (DynamicsManager.Integrator)integrator, j, integrator.CurrentTime);

                    Flowsheet.ProcessScripts(Scripts.EventType.IntegratorStep, Scripts.ObjectType.Integrator, "");

                    if (control != null)
                    {
                        Flowsheet.RunCodeOnUIThread(() =>
                        {
                            Flowsheet.UpdateInterface();
                        });
                    }

                    integrator.CurrentTime = integrator.CurrentTime.AddSeconds(interval);

                    if (integrator.ShouldCalculateControl)
                    {
                        foreach (PIDController controller in Controllers)
                        {
                            if (controller.Active)
                            {
                                Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                try
                                {
                                    controller.Solve();
                                    Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                }
                                catch (Exception ex)
                                {
                                    Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                    throw ex;
                                }
                            }
                        }
                        foreach (PythonController controller in Controllers2)
                        {
                            if (controller.Active)
                            {
                                Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationStarted, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                try
                                {
                                    controller.Solve();
                                    Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationFinished, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                }
                                catch (Exception ex)
                                {
                                    Flowsheet.ProcessScripts(Scripts.EventType.ObjectCalculationError, Scripts.ObjectType.FlowsheetObject, controller.Name);
                                    throw ex;
                                }
                            }
                        }
                    }

                    var waittime = integrator.RealTimeStepMs - sw.ElapsedMilliseconds;

                    if (waittime > 0 && realtime)
                        Task.Delay((int)waittime).Wait();

                    sw.Stop();

                    if (control != null)
                    {
                        if (control.Abort)
                            break;
                    }

                    if (!realtime)
                    {
                        if (schedule.UsesEventList)
                            ProcessEvents(Flowsheet, schedule.CurrentEventList, integrator.CurrentTime, integrator.IntegrationStep);

                        if (schedule.UsesCauseAndEffectMatrix)
                            ProcessCEMatrix(Flowsheet, schedule.CurrentCauseAndEffectMatrix);
                    }

                    j += 1;

                    i += interval;

                }

                if (exceptions.Count > 0) throw exceptions[0];

            });

            maintask.ContinueWith(t =>
            {
                if (t.Exception != null)
                    Flowsheet.ProcessScripts(Scripts.EventType.IntegratorError, Scripts.ObjectType.Integrator, "");
                else
                    Flowsheet.ProcessScripts(Scripts.EventType.IntegratorFinished, Scripts.ObjectType.Integrator, "");
                if (control != null)
                {
                    Flowsheet.RunCodeOnUIThread(() =>
                    {
                        control.btnPlay.Enabled = true;
                        control.btnViewResults.Enabled = true;
                        control.btnRT.Enabled = true;
                        control.pbProgress.Value = 0;
                        Flowsheet.SupressMessages = false;
                        Flowsheet.UpdateOpenEditForms();
                        if (t.Exception != null)
                        {
                            Exception baseexception;
                            foreach (var ex in t.Exception.Flatten().InnerExceptions)
                            {
                                string euid = Guid.NewGuid().ToString();
                                SharedClasses.ExceptionProcessing.ExceptionList.Exceptions.Add(euid, ex);
                                if (ex is AggregateException)
                                {
                                    baseexception = ex.InnerException;
                                    foreach (var iex in ((AggregateException)ex).Flatten().InnerExceptions)
                                    {
                                        while (iex.InnerException != null)
                                            baseexception = iex.InnerException;
                                        Flowsheet.ShowMessage(baseexception.Message.ToString(), Interfaces.IFlowsheet.MessageType.GeneralError, euid);
                                    }
                                }
                                else
                                {
                                    baseexception = ex;
                                    if (baseexception.InnerException != null)
                                    {
                                        while (baseexception.InnerException.InnerException != null)
                                        {
                                            baseexception = baseexception.InnerException;
                                            if ((baseexception == null))
                                                break;
                                            if ((baseexception.InnerException == null))
                                                break;
                                        }
                                        Flowsheet.ShowMessage(baseexception.Message.ToString(), Interfaces.IFlowsheet.MessageType.GeneralError, euid);
                                    }
                                }
                            }
                        }
                    });
                }
            });

            if (waittofinish)
                maintask.RunSynchronously(TaskScheduler.Default);
            else
                maintask.Start(TaskScheduler.Default);

            return maintask;
        }

        private void btnViewResults_Click(object sender, EventArgs e)
        {

            var schedule = Flowsheet.DynamicsManager.ScheduleList[Flowsheet.DynamicsManager.CurrentSchedule];

            var integrator = Flowsheet.DynamicsManager.IntegratorList[schedule.CurrentIntegrator];

            var sheetc = ((dynamic)(Flowsheet.FlowsheetForm)).Spreadsheet.Sheet;

            var spreadsheet = (ReoGridControl)sheetc;

            var sheet = spreadsheet.NewWorksheet();

            sheet.RowCount = integrator.MonitoredVariableValues.Count + 1;

            sheet.Cells[0, 0].Data = "Time (ms)";

            int i, j;

            i = 1;
            foreach (var var in integrator.MonitoredVariables)
            {
                sheet.Cells[0, i].Data = var.Description + " " + (var.PropertyUnits != "" ? " (" + var.PropertyUnits + ")" : "");
                i += 1;
            }

            i = 1;
            foreach (var item in integrator.MonitoredVariableValues)
            {
                if (integrator.RealTime)
                {
                    sheet.Cells[i, 0].Data = item.Key * integrator.RealTimeStepMs;
                }
                else
                {
                    sheet.Cells[i, 0].Data = item.Key * integrator.IntegrationStep.TotalMilliseconds;
                }
                j = 1;
                foreach (var var in item.Value)
                {
                    sheet.Cells[i, j].Data = var.PropertyValue.ToDoubleFromInvariant();
                    j += 1;
                }
                i += 1;
            }

            spreadsheet.CurrentWorksheet = sheet;

        }
    }
}
