using DWSIM.GlobalSettings;
using DWSIM.Interfaces;
using ICSharpCode.SharpZipLib.Zip;
using iTextSharp.text;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.InteropServices;
using System.Threading.Tasks;
using System.Xml.Linq;
using static DWSIM.Interfaces.Enums.Scripts;

namespace DWSIM.Automation
{
    [Guid("474a8e52-0b3e-48cb-a3bb-aae60f843578"), ClassInterface(ClassInterfaceType.None)]
    [ComVisible(true)]
    public class Flowsheet2 : FlowsheetBase.FlowsheetBase
    {
        private Action<string, IFlowsheet.MessageType> listeningaction;

        private Action updateUIaction;

        public override bool SupressMessages { get; set; } = false;

        public Flowsheet2(Action<string, IFlowsheet.MessageType> messageListener, Action updateUIhandler)
        {

            SaveSpreadsheetData = new Action<XDocument>((xdoc) =>
            {
            });

            RetrieveSpreadsheetData = new Func<string, List<string[]>>((range) =>
            {
                return null;
                //return Spreadsheet.GetDataFromRange(range);
            });

            RetrieveSpreadsheetFormat = new Func<string, List<string[]>>((range) =>
            {
                return null;
                //return Spreadsheet.GetDataFromRange(range);
            });

            DynamicsManager.RunSchedule = (schname) =>
            {
                DynamicsManager.CurrentSchedule = DynamicsManager.GetSchedule(schname).ID;
                return null;
            };

            listeningaction = messageListener;

            updateUIaction = updateUIhandler;

        }

        public void Init()
        {

            Initialize();

        }

        public override IFlowsheet GetNewInstance()
        {
            var fs = new Flowsheet2(null, null);
            return fs;
        }

        public override void UpdateInformation()
        {
            UpdateInterface();
        }

        public override void UpdateInterface()
        {
            updateUIaction?.Invoke();
        }

        public override void ShowDebugInfo(string text, int level)
        {
            Console.WriteLine(text);
        }

        public override void ShowMessage(string text, IFlowsheet.MessageType mtype, string exceptionid = "")
        {
            if (listeningaction != null) listeningaction(text, mtype);
        }

        public void WriteMessage(string text)
        {
            listeningaction?.Invoke(text, IFlowsheet.MessageType.Information);
        }

        public override void UpdateOpenEditForms()
        {

        }

        public override object GetApplicationObject()
        {
            return null;
        }

        public void SolveFlowsheet()
        {

            if (PropertyPackages.Count == 0)
            {
                throw new Exception("Please select a Property Package before solving the flowsheet.");
            }

            if (SelectedCompounds.Count == 0)
            {
                throw new Exception("Please select a Compound before solving the flowsheet.");
            }

            Settings.CalculatorActivated = true;
            Settings.SolverMode = 1;
            Settings.SolverBreakOnException = true;

            RequestCalculation();

        }

        public List<Exception> SolveFlowsheet2( )
        { 
            if (PropertyPackages.Count == 0)
            {
                ShowMessage("Please select a Property Package before solving the flowsheet.", IFlowsheet.MessageType.GeneralError);
                return new List<Exception>();
            }

            if (SelectedCompounds.Count == 0)
            {
                ShowMessage("Please select a Compound before solving the flowsheet.", IFlowsheet.MessageType.GeneralError);
                return new List<Exception>();
            }

            Settings.CalculatorActivated = true;

            Task<List<Exception>> st = new Task<List<Exception>>(() =>
            {
                return FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(this, GlobalSettings.Settings.SolverMode);
            });
            
            st.ContinueWith((t) =>
            {
                Settings.CalculatorStopRequested = false;
                Settings.CalculatorBusy = false;
                Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
            });

            try
            {
                st.Start(TaskScheduler.Default);
                st.Wait();
                return st.Result;
            }
            catch (AggregateException aex)
            {
                foreach (Exception ex2 in aex.InnerExceptions)
                {
                    ShowMessage(ex2.ToString(), IFlowsheet.MessageType.GeneralError);
                }
                Settings.CalculatorBusy = false;
                Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
                return new List<Exception>(aex.InnerExceptions);
            }
            catch (Exception ex)
            {
                ShowMessage(ex.ToString(), IFlowsheet.MessageType.GeneralError);
                Settings.CalculatorBusy = false;
                Settings.TaskCancellationTokenSource = new System.Threading.CancellationTokenSource();
                return new List<Exception> { ex };  
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
            act.Invoke();
        }

        public override void DisplayForm(object form)
        {
            throw new NotImplementedException();
        }
        public void SaveSimulation(string path, bool backup = false)
        {

            if (System.IO.Path.GetExtension(path).ToLower() == ".dwxmz")
            {

                path = Path.ChangeExtension(path, ".dwxmz");

                string xmlfile = Path.ChangeExtension(GetTempFileName(), ".xml");

                SaveToXML().Save(xmlfile);

                var i_Files = new List<string>();
                if (File.Exists(xmlfile))
                    i_Files.Add(xmlfile);

                var strmZipOutputStream = new ZipOutputStream(File.Create(path));

                strmZipOutputStream.SetLevel(9);

                if (Options.UsePassword)
                    strmZipOutputStream.Password = Options.Password;

                string strFile = "";

                foreach (string strFile_loopVariable in i_Files)
                {
                    strFile = strFile_loopVariable;
                    FileStream strmFile = File.OpenRead(strFile);
                    byte[] abyBuffer = new byte[strmFile.Length];

                    strmFile.Read(abyBuffer, 0, abyBuffer.Length);
                    ZipEntry objZipEntry = new ZipEntry(Path.GetFileName(strFile));

                    objZipEntry.DateTime = DateTime.Now;
                    objZipEntry.Size = strmFile.Length;
                    strmFile.Close();
                    strmZipOutputStream.PutNextEntry(objZipEntry);
                    strmZipOutputStream.Write(abyBuffer, 0, abyBuffer.Length);

                }

                strmZipOutputStream.Finish();
                strmZipOutputStream.Close();

                try
                {
                    File.Delete(xmlfile);
                }
                catch { }
                //try
                //{
                //    File.Delete(dbfile);
                //}
                //catch { }
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".dwxml")
            {
                SaveToXML().Save(path);
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".xml")
            {
                SaveToMXML().Save(path);
            }

            ProcessScripts(EventType.SimulationSaved, ObjectType.Simulation, "");

        }

        private string GetTempFileName()
        {
            return Path.Combine(Path.GetTempPath(), $"{Guid.NewGuid()}.tmp");
        }

        public override void CloseOpenEditForms()
        {

        }

        public override IFlowsheet Clone()
        {

            var fs = new Flowsheet2(null, null);
            fs.Initialize();
            var xdoc = SaveToXML();
            fs.LoadFromXML(xdoc);
            return fs;

        }

    }
}
