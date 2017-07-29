using System;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using Eto.Forms;

namespace DWSIM.UI.Desktop.Shared
{
    public class Flowsheet : FlowsheetBase.FlowsheetBase
    {
        private Action<string, IFlowsheet.MessageType> listeningaction ;

        public bool optimizing = false;
        public bool supressmessages = false;
        private bool eventattached = false;

        public Eto.Forms.Form FlowsheetForm;
        public Eto.Forms.Control FlowsheetControl;
        private Forms.SolvingFlowsheet solvform;

        public void SetFlowsheetForm(Eto.Forms.Form form)
        {
            FlowsheetForm = form;
        }

        public override void DisplayForm(object form)
        {
            throw new NotImplementedException();
        }

        public override Interfaces.IFlowsheet GetNewInstance()
        {
            return new Flowsheet();
        }

        public override void UpdateInformation()
        {
            UpdateInterface();
        }

        public override void UpdateInterface()
        {
            if (!supressmessages)
            {
                Application.Instance.AsyncInvoke(() => { if (FlowsheetForm != null) FlowsheetForm.Invalidate(); });                
            }
        }

        public override void ShowDebugInfo(string text, int level)
        {
            Console.WriteLine(text);
        }

        public override void ShowMessage(string text, IFlowsheet.MessageType mtype)
        {
            if (listeningaction != null) listeningaction(text, mtype);
            Console.WriteLine(text);
        }

        public override void UpdateOpenEditForms()
        {
            throw new NotImplementedException();
        }

        public void SolveFlowsheet(ISimulationObject gobj = null)
        {

            var surface = ((DWSIM.Drawing.SkiaSharp.GraphicsSurface)this.GetSurface());

            if (PropertyPackages.Count == 0)
            {
                ShowMessage("Please select a Property Package before solving the flowsheet.", IFlowsheet.MessageType.GeneralError);
                return;
            }

            if (SelectedCompounds.Count == 0)
            {
                ShowMessage("Please select a Compound before solving the flowsheet.", IFlowsheet.MessageType.GeneralError);
                return;
            }

            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.SolverBreakOnException = true;

            solvform = new Forms.SolvingFlowsheet() ;
            solvform.lblMessage.Text = "Solving flowsheet model, please wait...\n(touch to abort calculation)";
            solvform.btnAbort.Click += (sender, e) => {
                Application.Instance.AsyncInvoke(() =>
                {
                    surface.BackgroundColor = SkiaSharp.SKColors.White;
                    FlowsheetForm.Enabled = true;
                    FlowsheetControl.Invalidate();
                    solvform.Close();
                });
                GlobalSettings.Settings.CalculatorStopRequested = true;
                if (GlobalSettings.Settings.TaskCancellationTokenSource != null)
                {
                    try
                    {
                        GlobalSettings.Settings.TaskCancellationTokenSource.Cancel();
                    }
                    catch (Exception) { }
                }
            };

            Task st = new Task(() =>
            {
                if (!eventattached)
                {
                    eventattached = true;
                    FlowsheetSolver.FlowsheetSolver.CalculatingObject += (objinfo) =>
                    {
                        Application.Instance.AsyncInvoke(() =>
                        {
                            if (solvform != null && !optimizing)
                            {
                                solvform.lblMessage.Text = "Solving flowsheet model, please wait...\nCurrent object: " + objinfo.Tag;
                            }
                        });
                    };
                }
                RequestCalculation(gobj);
            });

            st.ContinueWith((t) =>
            {
                Application.Instance.AsyncInvoke(() =>
                {
                    surface.BackgroundColor = SkiaSharp.SKColors.White;
                    FlowsheetForm.Enabled = true;
                    FlowsheetControl.Invalidate();
                    if (solvform != null) solvform.Close();
                    solvform = null;
                });
                GlobalSettings.Settings.CalculatorStopRequested = false;
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
            });
                 
            st.Start();
            surface.BackgroundColor = SkiaSharp.SKColors.LightGray;
            FlowsheetForm.Enabled = false;
            FlowsheetControl.Invalidate();
            FlowsheetForm.Invalidate();
            solvform.ShowModal(FlowsheetControl);

        }

        public override void SetMessageListener(Action<string, IFlowsheet.MessageType> act)
        {
            listeningaction = act;
        }
    }
}
