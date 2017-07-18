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

        public Eto.Forms.Form FlowsheetForm;

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
               Application.Instance.Invoke(() => FlowsheetForm.Invalidate());                
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

        public void SolveFlowsheet()
        {

            if (PropertyPackages.Count == 0)
            {
                //Toast toast = Toast.MakeText(this, "Please select a Property Package before solving the ", ToastLength.Long);
                //toast.Show();
                return;
            }

            if (SelectedCompounds.Count == 0)
            {
                //Toast toast = Toast.MakeText(this, "Please add a Compound before solving the ", ToastLength.Long);
                //toast.Show();
                return;
            }

            GlobalSettings.Settings.CalculatorActivated = true;
            GlobalSettings.Settings.SolverMode = 1;
            GlobalSettings.Settings.SolverBreakOnException = true;
            Task st = new Task(() =>
            {
                
                    //AndroidHUD.AndHUD.Shared.Show(this, "Solving flowsheet model, please wait...\n\n(touch to abort calculation)", -1, AndroidHUD.MaskType.Black,
                    //                              null, () =>
                    //                              {
                    //                                  AndroidHUD.AndHUD.Shared.Dismiss(this);
                    //                                  GlobalSettings.Settings.CalculatorStopRequested = true;
                    //                                  if (GlobalSettings.Settings.TaskCancellationTokenSource != null)
                    //                                  {
                    //                                      try
                    //                                      {
                    //                                          GlobalSettings.Settings.TaskCancellationTokenSource.Cancel();
                    //                                      }
                    //                                      catch (Exception) { }
                    //                                  }
                    //                              }, true, null);

                    //if (!eventattached)
                    //{
                    //    eventattached = true;
                    //    PortableFlowsheetSolver.FlowsheetSolver.CalculatingObject += (objinfo) =>
                    //    {
                    //        RunOnUiThread(() =>
                    //        {
                    //            if (AndroidHUD.AndHUD.Shared.CurrentDialog != null && !optimizing)
                    //            {
                    //                AndroidHUD.AndHUD.Shared.statusText.Text = "Solving flowsheet model, please wait...\n\nCurrent object: " + objinfo.Tag + "\n\n(touch to abort calculation)";
                    //            }
                    //        });
                    //    };
                    //}

                foreach (var sobj in SimulationObjects.Values)
                {
                    //sobj.PropertyPackage.StabilityTestSeverity = 0;
                    //sobj.PropertyPackage.StabilityTestKeyCompounds = SelectedCompounds.Select((x) => x.Key).ToArray();
                    //if (sobj.GraphicObject.ObjectType.ToString().Contains("Reactor"))
                    //{
                    //    ((PortableDTL.Reactors.Reactor)sobj).ReactionSetID = "DefaultSet";
                    //    ((PortableDTL.Reactors.Reactor)sobj).ReactionSetName = "DefaultSet";
                    //}
                    //else if (sobj is PortableDTL.DTL.SimulationObjects.Streams.MaterialStream)
                    //{
                    //    ((PortableDTL.DTL.SimulationObjects.Streams.MaterialStream)sobj).NormalizeOverallMoleComposition();
                    //    ((PortableDTL.DTL.SimulationObjects.Streams.MaterialStream)sobj).NormalizeOverallMassComposition();
                    //}
                }
                RequestCalculation();
            });

            st.ContinueWith((t) =>
            {
                //AndroidHUD.AndHUD.Shared.Dismiss(this);
                if (t.Exception == null)
                {
                    
                    //AndroidHUD.AndHUD.Shared.ShowSuccess(this, "Flowsheet model solved successfully.", AndroidHUD.MaskType.Black, TimeSpan.FromSeconds(2));
                    
                }
                else
                {
                     FlowsheetForm.Platform.Invoke(() =>
                    {
                        if (!supressmessages)
                        {
                            foreach (Exception ex in t.Exception.InnerExceptions)
                            {
                                Exception ex2 = ex.InnerException;
                                if (ex2 != null)
                                {
                                    while (ex2.InnerException != null)
                                    {
                                        ex2 = ex2.InnerException;
                                    }
                                }
                                else { ex2 = ex; }
                                if (ex2 is System.OperationCanceledException)
                                {
                                    //Toast toast = Toast.MakeText(this, "Flowsheet calculation aborted by the user.", ToastLength.Short);
                                    //toast.Show();
                                }
                                else
                                {
                                    string obj = (string)ex2.Data["Object"];
                                    if (obj == null)
                                    {
                                        //AndroidHUD.AndHUD.Shared.ShowError(this, "Error solving flowsheet model.", AndroidHUD.MaskType.Black, TimeSpan.FromSeconds(2));
                                    }
                                    else
                                    {
                                        //AndroidHUD.AndHUD.Shared.ShowError(this, "Error solving object " + obj + ".\n\n(touch for more details)", AndroidHUD.MaskType.Black, TimeSpan.FromSeconds(4), () =>
                                        //{
                                        //    AndroidHUD.AndHUD.Shared.Dismiss(this);
                                        //    CreateExceptionAlertDialog(ex2);
                                        //});

                                    }
                                }
                            }
                        }
                        GlobalSettings.Settings.CalculatorStopRequested = false;
                        GlobalSettings.Settings.CalculatorBusy = false;
                        GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();

                    });
                }
            });
                 
            st.Start();

        }

        public override void SetMessageListener(Action<string, IFlowsheet.MessageType> act)
        {
            listeningaction = act;
        }
    }
}
