using System;
using System.Collections.Generic;
using System.IO;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
using iTextSharp.text;

namespace DWSIM.UI.Desktop.Shared
{
    public class Flowsheet : FlowsheetBase.FlowsheetBase
    {
        private Action<string, IFlowsheet.MessageType> listeningaction;

        public Action FinishedSolving;

        public Action HighLevelSolve;

        public Action ActComps, ActBasis, ActGlobalOptions, ActSave, ActSaveAs, ActOptions, ActZoomIn, ActZoomOut, ActZoomFit, ActZoomDefault, ActSimultAdjustSolver;
        public Action ActDrawGrid, ActSnapToGrid, ActMultiSelect, ActAlignLefts, ActAlignCenters, ActAlignRights, ActAlignTops, ActAlignMiddles, ActAlignBottoms, ActHorizAlign, ActVertAlign;

        public Action UpdateEditorPanels, CloseEditorPanels;

        public Action UpdateSurface;

        public Action ActClearLog;

        public bool optimizing = false;

        public override bool SupressMessages { get; set; } = false;

        private bool eventattached = false;

        public Eto.Forms.Form FlowsheetForm;
        public Eto.Forms.Control FlowsheetControl;

        public Action<DocumentPage> AddChart;

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
            var fs = new Flowsheet();
            fs.Initialize();
            return fs;
        }

        public override void UpdateInformation()
        {
            UpdateInterface();
        }

        public override void UpdateInterface()
        {
            Application.Instance.Invoke(() =>
            {
                if (FlowsheetForm != null) FlowsheetForm.Invalidate();
            });
        }

        public override void ClearLog()
        {
            ActClearLog?.Invoke();
        }

        public override void ShowDebugInfo(string text, int level)
        {
            Console.WriteLine(text);
        }

        public override void ShowMessage(string text, IFlowsheet.MessageType mtype, string exceptionid = "")
        {
            if (!SupressMessages)
            {
                if (listeningaction != null) listeningaction(text, mtype);
            }
        }

        public void WriteMessage(string text)
        {
            if (!SupressMessages)
            {
                listeningaction?.Invoke(text, IFlowsheet.MessageType.Information);
                //Console.WriteLine(text);
            }
        }

        public override void UpdateOpenEditForms()
        {
            UpdateEditorPanels?.Invoke();
        }

        public override void CloseOpenEditForms()
        {
            CloseEditorPanels?.Invoke();
        }

        public override object GetApplicationObject()
        {
            return Application.Instance;
        }

        public void SolveFlowsheet(bool wait, ISimulationObject gobj = null, bool changecalcorder = false)
        {

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

            Task st = new Task(() =>
            {
                RequestCalculation(gobj, changecalcorder);
                Task.Delay(1000).Wait();
            });

            st.ContinueWith((t) =>
            {
                GlobalSettings.Settings.CalculatorStopRequested = false;
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();

                if (FinishedSolving != null) FinishedSolving.Invoke();

            });


            if (wait)
            {
                try
                {
                    st.Start(TaskScheduler.Default);
                    st.Wait();
                }
                catch (AggregateException aex)
                {
                    foreach (Exception ex2 in aex.InnerExceptions)
                    {
                        if (!SupressMessages)
                        {
                            ShowMessage(ex2.ToString(), IFlowsheet.MessageType.GeneralError);
                        }
                    }
                    GlobalSettings.Settings.CalculatorBusy = false;
                    GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
                }
                catch (Exception ex)
                {
                    if (!SupressMessages)
                    {
                        ShowMessage(ex.ToString(), IFlowsheet.MessageType.GeneralError);
                    }
                    GlobalSettings.Settings.CalculatorBusy = false;
                    GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
                }
            }
            else
            {
                st.Start(TaskScheduler.Default);
            }

        }

        public void SolveFlowsheet2()
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

            Task st = new Task(() =>
            {
                RequestCalculation();
            });

            st.ContinueWith((t) =>
            {
                GlobalSettings.Settings.CalculatorStopRequested = false;
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();

                if (FinishedSolving != null) FinishedSolving.Invoke();
            });

            try
            {
                st.Start(TaskScheduler.Default);
                st.Wait();
            }
            catch (AggregateException aex)
            {
                foreach (Exception ex2 in aex.InnerExceptions)
                {
                    if (!SupressMessages)
                    {
                        ShowMessage(ex2.ToString(), IFlowsheet.MessageType.GeneralError);
                    }
                }
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
            }
            catch (Exception ex)
            {
                if (!SupressMessages)
                {
                    ShowMessage(ex.ToString(), IFlowsheet.MessageType.GeneralError);
                }
                GlobalSettings.Settings.CalculatorBusy = false;
                GlobalSettings.Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
            }

        }

        public override void SetMessageListener(Action<string, IFlowsheet.MessageType> act)
        {
            listeningaction = act;
        }

        public void GenerateReport(List<ISimulationObject> objects, string format, Stream ms)
        {

            string ptext = "";

            switch (format)
            {

                case "PDF":

                    iTextSharp.text.Document document = new iTextSharp.text.Document(PageSize.A4, 25, 25, 30, 30);
                    var writer = iTextSharp.text.pdf.PdfWriter.GetInstance(document, ms);

                    var bf = iTextSharp.text.pdf.BaseFont.CreateFont(iTextSharp.text.pdf.BaseFont.COURIER, iTextSharp.text.pdf.BaseFont.CP1252, true);

                    var regfont = new Font(bf, 12, Font.NORMAL);
                    var boldfont = new Font(bf, 12, Font.BOLD);

                    document.Open();
                    document.Add(new Paragraph("DWSIM Simulation Results Report", boldfont));
                    document.Add(new Paragraph("Simulation Name: " + Options.SimulationName, boldfont));
                    document.Add(new Paragraph("Date created: " + System.DateTime.Now.ToString() + "\n\n", boldfont));

                    foreach (var obj in objects)
                    {
                        ptext = obj.GetDisplayName() + ": " + obj.GraphicObject.Tag + "\n\n";
                        document.Add(new Paragraph(ptext, boldfont));
                        ptext = obj.GetReport(Options.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, Options.NumberFormat);
                        ptext += "\n";
                        document.Add(new Paragraph(ptext, regfont));
                    }

                    document.Close();

                    writer.Close();

                    break;

                case "TXT":

                    string report = "";

                    report += "DWSIM Simulation Results Report\nSimulation Name: " + Options.SimulationName + "\nDate created: " + System.DateTime.Now.ToString() + "\n\n";

                    foreach (var obj in objects)
                    {
                        ptext = "";
                        ptext += obj.GetDisplayName() + ": " + obj.GraphicObject.Tag + "\n\n";
                        ptext += obj.GetReport(Options.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, Options.NumberFormat);
                        ptext += "\n";
                        report += ptext;
                    }


                    using (StreamWriter wr = new StreamWriter(ms))
                    {
                        wr.Write(report);
                    }
                    break;

                default:

                    throw new NotImplementedException("Sorry, this feature is not yet available.");
            }

        }

        public override void RunCodeOnUIThread(Action act)
        {
            Application.Instance.Invoke(act);
        }

        public override IFlowsheet Clone()
        {

            var fs = new Flowsheet();
            fs.Initialize();
            var xdoc = SaveToXML();
            fs.LoadFromXML(xdoc);
            return fs;

        }

    }
}
