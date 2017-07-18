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

        public void SolveFlowsheet(ISimulationObject gobj = null)
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
                RequestCalculation(gobj);
            });

            st.ContinueWith((t) =>
            {
                GlobalSettings.Settings.CalculatorStopRequested = false;
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
            });
                 
            st.Start();

        }

        public override void SetMessageListener(Action<string, IFlowsheet.MessageType> act)
        {
            listeningaction = act;
        }
    }
}
