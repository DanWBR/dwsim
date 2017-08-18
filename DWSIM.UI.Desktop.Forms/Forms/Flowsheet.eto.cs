using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Xml;
using System.Xml.Linq;
using DWSIM.UI.Shared;
using System.IO;
using ICSharpCode.SharpZipLib.Zip;
using System.Linq;
using System.Threading.Tasks;
using DWSIM.UI.Desktop.Editors;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables;
using System.Timers;
using System.Diagnostics;

namespace DWSIM.UI.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;
        public DWSIM.UI.Desktop.Editors.Spreadsheet Spreadsheet;
        private DWSIM.UI.Controls.FlowsheetSurfaceControl FlowsheetControl;

        private TableLayout SpreadsheetControl;

        private TabPage TabPageSpreadsheet;

        private DWSIM.UI.Desktop.Editors.ResultsViewer ResultsControl;

        private DWSIM.UI.Desktop.Editors.MaterialStreamListViewer MaterialStreamListControl;

        private DWSIM.UI.Desktop.Editors.ScriptManager ScriptListControl;

        string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

        private string backupfilename = "";

        ContextMenu selctxmenu, deselctxmenu;

        public Dictionary<string, Interfaces.ISimulationObject> ObjectList = new Dictionary<string, Interfaces.ISimulationObject>();

        void InitializeComponent()
        {

            // setup backup timer

            backupfilename = DateTime.Now.ToString().Replace('-', '_').Replace(':', '_').Replace(' ', '_').Replace('/', '_') + ".dwxmz";

            var BackupTimer = new Timer(GlobalSettings.Settings.BackupInterval * 60 * 1000);
            BackupTimer.Elapsed += (sender, e) =>
            {
                Task.Factory.StartNew(() => SaveBackupCopy());
            };
            BackupTimer.Enabled = true;
            BackupTimer.Start();

            WindowState = Eto.Forms.WindowState.Maximized;

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };
            FlowsheetObject.Initialize();

            Title = "New Flowsheet";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            FlowsheetControl = new DWSIM.UI.Controls.FlowsheetSurfaceControl();

            FlowsheetControl.FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface();

            FlowsheetControl.FlowsheetSurface.BackgroundColor = SkiaSharp.SKColors.White;

            FlowsheetObject.FlowsheetControl = FlowsheetControl;

            FlowsheetControl.FlowsheetObject = FlowsheetObject;

            FlowsheetControl.KeyDown += (sender, e) =>
            {
                if (e.Key == Keys.Delete) DeleteObject();
            };

            ClientSize = new Size(1024, 768);

            var btnSave = new ButtonMenuItem { Text = "Save Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save.png")), Shortcut = Keys.S | Application.Instance.AlternateModifier };
            var btnSaveAs = new ButtonMenuItem { Text = "Save As...", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save_as.png")), Shortcut = Keys.S | Application.Instance.AlternateModifier | Keys.Shift };
            var btnClose = new ButtonMenuItem { Text = "Close Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Delete_96px.png")), Shortcut = Keys.Q | Application.Instance.AlternateModifier };
            var btnComps = new ButtonMenuItem { Text = "Compounds", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-thin_test_tube.png")), Shortcut = Keys.C | Application.Instance.AlternateModifier };
            var btnBasis = new ButtonMenuItem { Text = "Basis", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-math.png")), Shortcut = Keys.B | Application.Instance.AlternateModifier };
            var btnOptions = new ButtonMenuItem { Text = "Settings", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")), Shortcut = Keys.M | Application.Instance.AlternateModifier };
            var btnSolve = new ButtonMenuItem { Text = "Solve Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")), Shortcut = Keys.F5 };

            var btnUtilities_TrueCriticalPoint = new ButtonMenuItem { Text = "True Critical Point", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };
            var btnUtilities_BinaryEnvelope = new ButtonMenuItem { Text = "Binary Envelope", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };
            var btnUtilities_PhaseEnvelope = new ButtonMenuItem { Text = "Phase Envelope", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };

            btnUtilities_TrueCriticalPoint.Click += (sender, e) => {
                var tcp = new Desktop.Editors.Utilities.TrueCriticalPointView(FlowsheetObject);
                var form = Common.GetDefaultEditorForm("True Critical Point", 500, 250, tcp);
                form.Show();
            };

            btnUtilities_BinaryEnvelope.Click += (sender, e) =>
            {
                var bpe = new Desktop.Editors.Utilities.BinaryEnvelopeView(FlowsheetObject);
                var form = Common.GetDefaultEditorForm("Binary Phase Envelope", 500, 750, bpe);
                form.Show();
            };

            btnUtilities_PhaseEnvelope.Click += (sender, e) =>
            {
                var pe = new Desktop.Editors.Utilities.PhaseEnvelopeView(FlowsheetObject);
                var form = Common.GetDefaultEditorForm("Phase Envelope", 500, 750, pe);
                form.Show();
            };

            var btnObjects = new ButtonMenuItem { Text = "Add New Simulation Object", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")), Shortcut = Keys.A | Application.Instance.AlternateModifier };
            var btnInsertText = new ButtonMenuItem { Text = "Add New Text Block", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "TextWidth_96px.png")) };
            var btnInsertTable = new ButtonMenuItem { Text = "Add New Property Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Grid_96px.png")) };
            var btnInsertMasterTable = new ButtonMenuItem { Text = "Add New Master Property Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "GridView_96px.png")) };
            var btnInsertSpreadsheetTable = new ButtonMenuItem { Text = "Add New Linked Spreadsheet Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "PivotTable_96px.png")) };

            var btnSensAnalysis = new ButtonMenuItem { Text = "Sensitivity Analysis", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-maintenance.png")) };
            var btnOptimization = new ButtonMenuItem { Text = "Flowsheet Optimizer", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-maintenance.png")) };

            btnSensAnalysis.Click += (sender, e) =>
            {
                var saeditor = new Desktop.Editors.SensAnalysisView(FlowsheetObject);
                var form = Common.GetDefaultEditorForm("Sensitivity Analysis", 500, 700, saeditor);
                form.Show();
            };

            btnOptimization.Click += (sender, e) =>
            {
                var foeditor = new Desktop.Editors.OptimizerView(FlowsheetObject);
                var form = Common.GetDefaultEditorForm("Flowsheet Optimizer", 500, 700, foeditor);
                form.Show();
            };

            btnInsertText.Click += (sender, e) =>
            {
                FlowsheetControl.AddObject("Text", 50, 50);
            };

            btnInsertTable.Click += (sender, e) =>
            {
                FlowsheetControl.AddObject("Property Table", 50, 50);
            };

            btnInsertMasterTable.Click += (sender, e) =>
            {
                FlowsheetControl.AddObject("Master Property Table", 50, 50);
            };

            btnInsertSpreadsheetTable.Click += (sender, e) =>
            {
                FlowsheetControl.AddObject("Spreadsheet Table", 50, 50);
            };

            FlowsheetControl.MouseDoubleClick += (sender, e) =>
            {
                if (Application.Instance.Platform.IsMac) FlowsheetControl.FlowsheetSurface.InputRelease();
                var obj = FlowsheetControl.FlowsheetSurface.SelectedObject;
                if (e.Modifiers == Keys.Shift)
                {
                    if (obj == null) return;
                    if (obj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream ||
                        obj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream)
                    {
                        return;
                    }
                    EditConnections();
                }
                else if (e.Modifiers == Keys.Alt)
                {
                    if (obj == null) return;
                    ViewSelectedObjectResults();
                }
                else if (e.Modifiers == Keys.Control)
                {
                    var surface = FlowsheetControl.FlowsheetSurface;
                    surface.ZoomAll((int)FlowsheetControl.Width, (int)FlowsheetControl.Height);
                    FlowsheetControl.Invalidate();
                }
                else
                {
                    if (obj == null) return;
                    EditSelectedObjectProperties();
                }

            };

            var chkSimSolver = new CheckMenuItem { Text = "Simultaneous Adjust Solver Active" };
            chkSimSolver.Checked = FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
            chkSimSolver.CheckedChanged += (sender, e) =>
            {
                FlowsheetObject.Options.SimultaneousAdjustSolverEnabled = chkSimSolver.Checked;
            };

            Menu = new MenuBar();
            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Mac:
                    var btnfile = (ButtonMenuItem)Menu.Items.Where((x) => x.Text == "&File").FirstOrDefault();
                    btnfile.Items.AddRange(new[] { btnSave, btnSaveAs });
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                    Menu.Items.Add(new ButtonMenuItem { Text = "File", Items = { btnSave, btnSaveAs, btnClose } });
                    break;
                case GlobalSettings.Settings.Platform.Windows:
                    Menu.ApplicationItems.AddRange(new[] { btnSave, btnSaveAs, btnClose });
                    break;
            }
            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Mac:
                    Menu.Items.Insert(3, new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions } });
                    Menu.Items.Insert(4, new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable } });
                    Menu.Items.Insert(5, new ButtonMenuItem { Text = "Solver", Items = { btnSolve, chkSimSolver } });
                    Menu.Items.Insert(6, new ButtonMenuItem { Text = "Tools", Items = { btnSensAnalysis, btnOptimization } });
                    Menu.Items.Insert(7, new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities_TrueCriticalPoint, btnUtilities_PhaseEnvelope, btnUtilities_BinaryEnvelope } });
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                case GlobalSettings.Settings.Platform.Windows:
                    Menu.Items.Add(new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Solver", Items = { btnSolve, chkSimSolver } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Tools", Items = { btnSensAnalysis, btnOptimization } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities_TrueCriticalPoint, btnUtilities_PhaseEnvelope, btnUtilities_BinaryEnvelope } });
                    break;
            }

            var hitem1 = new ButtonMenuItem { Text = "Online Help", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem1.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br/docs/crossplatform/help/");
            };

            var hitem2 = new ButtonMenuItem { Text = "Discussion Forums".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem2.Click += (sender, e) =>
            {
                Process.Start("http://sourceforge.net/p/dwsim/discussion/");
            };

            var hitem3 = new ButtonMenuItem { Text = "Report a Bug".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem3.Click += (sender, e) =>
            {
                Process.Start("https://sourceforge.net/p/dwsim/tickets/");
            };

            var hitem4 = new ButtonMenuItem { Text = "Go to DWSIM's Website".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem4.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br");
            };

            Menu.HelpItems.Add(hitem1);
            Menu.HelpItems.Add(hitem4);
            Menu.HelpItems.Add(hitem2);
            Menu.HelpItems.Add(hitem3);

            btnClose.Click += (sender, e) => Close();

            btnObjects.Click += (sender, e) =>
            {
                var insform = new DWSIM.UI.Desktop.Editors.InsertObject { Flowsheet = FlowsheetObject, ObjList = ObjectList, FlowsheetHeight = FlowsheetControl.Height };
                insform.ShowModal(this);
            };

            btnComps.Click += (sender, e) =>
            {
                var cont = new TableLayout();
                new DWSIM.UI.Desktop.Editors.Compounds(FlowsheetObject, cont);
                cont.Tag = "Simulation Compounds";

                var cont2 = new Desktop.Editors.CompoundTools(FlowsheetObject);
                cont2.Tag = "Compound Tools";

                var form = UI.Shared.Common.GetDefaultTabbedForm("Compounds", 920, 500, new Control[] { cont, cont2 });
                form.Show();

            };

            btnBasis.Click += (sender, e) =>
            {
                var cont1 = UI.Shared.Common.GetDefaultContainer();
                cont1.Tag = "Thermodynamics";
                new DWSIM.UI.Desktop.Editors.Models(FlowsheetObject, cont1);
                var cont2 = UI.Shared.Common.GetDefaultContainer();
                cont2.Tag = "Reactions";
                new DWSIM.UI.Desktop.Editors.ReactionsManager(FlowsheetObject, cont2);
                var form = UI.Shared.Common.GetDefaultTabbedForm("Simulation Basis", 800, 600, new[] { cont1, cont2 });
                form.Show();
                form.Width += 10;
            };

            btnOptions.Click += (sender, e) =>
            {
                var cont = UI.Shared.Common.GetDefaultContainer();
                new DWSIM.UI.Desktop.Editors.SimulationSettings(FlowsheetObject, cont);
                cont.Tag = "Settings";
                var cont2 = new UI.Desktop.Editors.FloatingTablesView(FlowsheetObject);
                cont2.Tag = "Floating Tables";
                var form = UI.Shared.Common.GetDefaultTabbedForm("Settings", 500, 500, new[] { cont, cont2 });
                form.Show();
                form.Width += 1;
            };

            btnSolve.Click += (sender, e) =>
            {
                SolveFlowsheet();
            };

            btnSave.Click += (sender, e) =>
            {
                try
                {
                    if (FlowsheetObject.Options.FilePath != "")
                    {
                        SaveSimulation(FlowsheetObject.Options.FilePath);
                    }
                    else
                    {
                        btnSaveAs.PerformClick();
                    }
                }
                catch (Exception ex)
                {
                    MessageBox.Show("Error saving file", ex.ToString(), MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                }
            };

            btnSaveAs.Click += (sender, e) =>
            {

                var dialog = new SaveFileDialog();
                dialog.Title = "Save File".Localize();
                dialog.Filters.Add(new FileFilter("XML Simulation File (Compressed)".Localize(), new[] { ".dwxmz" }));
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(this) == DialogResult.Ok)
                {
                    SaveSimulation(dialog.FileName);
                }

            };

            Spreadsheet = new DWSIM.UI.Desktop.Editors.Spreadsheet(FlowsheetObject) { ObjList = ObjectList };

            FlowsheetObject.LoadSpreadsheetData = new Action<XDocument>((xdoc) =>
            {
                string data1 = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value;
                string data2 = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value;
                if (!string.IsNullOrEmpty(data1)) Spreadsheet.CopyDT1FromString(data1);
                if (!string.IsNullOrEmpty(data2)) Spreadsheet.CopyDT2FromString(data2);
                Spreadsheet.CopyFromDT();
                Spreadsheet.EvaluateAll();
            });

            FlowsheetObject.SaveSpreadsheetData = new Action<XDocument>((xdoc) =>
            {
                try { Spreadsheet.CopyToDT(); }
                catch (Exception) { }
                xdoc.Element("DWSIM_Simulation_Data").Add(new XElement("Spreadsheet"));
                xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Add(new XElement("Data1"));
                xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Add(new XElement("Data2"));
                Spreadsheet.CopyToDT();
                xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value = Spreadsheet.CopyDT1ToString();
                xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value = Spreadsheet.CopyDT2ToString();
            });

            FlowsheetObject.RetrieveSpreadsheetData = new Func<string, List<string[]>>((range) =>
            {
                return Spreadsheet.GetDataFromRange(range);
            });

            SpreadsheetControl = Spreadsheet.GetSpreadsheet(FlowsheetObject);

            ResultsControl = new DWSIM.UI.Desktop.Editors.ResultsViewer(FlowsheetObject);

            MaterialStreamListControl = new DWSIM.UI.Desktop.Editors.MaterialStreamListViewer(FlowsheetObject);

            ScriptListControl = new DWSIM.UI.Desktop.Editors.ScriptManager(FlowsheetObject);

            var tabholder = new TabControl();
            TabPageSpreadsheet = new TabPage { Content = SpreadsheetControl, Text = "Spreadsheet" };
            tabholder.Pages.Add(new TabPage { Content = FlowsheetControl, Text = "Flowsheet" });
            tabholder.Pages.Add(new TabPage { Content = MaterialStreamListControl, Text = "Material Streams" });
            tabholder.Pages.Add(TabPageSpreadsheet);
            tabholder.Pages.Add(new TabPage { Content = ScriptListControl, Text = "Scripts" });
            tabholder.Pages.Add(new TabPage { Content = ResultsControl, Text = "Results" });

            var split = new Eto.Forms.Splitter();
            split.Panel1 = tabholder;
            split.Panel2 = SetupLogWindow();
            split.Orientation = Orientation.Vertical;
            split.FixedPanel = SplitterFixedPanel.Panel2;
            split.Panel2.Height = 100;

            Content = split;

            selctxmenu = new ContextMenu();
            deselctxmenu = new ContextMenu();

            FlowsheetControl.MouseUp += (sender, e) =>
            {
                if (e.Buttons == MouseButtons.Alternate)
                {
                    if (Application.Instance.Platform.IsMac) FlowsheetControl.FlowsheetSurface.InputRelease();
                    if (FlowsheetControl.FlowsheetSurface.SelectedObject != null)
                    {
                        var obj = FlowsheetControl.FlowsheetSurface.SelectedObject;
                        switch (obj.ObjectType)
                        {
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Text:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Image:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Table:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_MasterTable:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable:
                                selctxmenu.Items.Clear();
                                var itemtype = new ButtonMenuItem { Text = "Table/Text/Image", Enabled = false };
                                selctxmenu.Items.Add(itemtype);
                                var delitem = new ButtonMenuItem { Text = "Delete", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Delete_96px.png")) };
                                delitem.Click += (sender2, e2) =>
                                {
                                    if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                    {
                                        FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj, false, false);
                                    }
                                };
                                selctxmenu.Items.Add(delitem);
                                break;
                            default:
                                SetupSelectedContextMenu();
                                break;
                        }

                        selctxmenu.Show(FlowsheetControl);
                    }
                    else
                    {
                        SetupDeselectedContextMenu();
                        deselctxmenu.Show(FlowsheetControl);
                    }
                }
            };

            Closing += Flowsheet_Closing;

            Closed += (sender, e) =>
            {
                SaveUserUnits();
                FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationClosed, Interfaces.Enums.Scripts.ObjectType.Simulation, "");
            };

            Shown += Flowsheet_Shown;

            Task.Factory.StartNew(() => LoadObjects());

        }

        private void SolveFlowsheet()
        {
            FlowsheetObject.UpdateSpreadsheet(() =>
            {
                Spreadsheet.EvaluateAll();
                Spreadsheet.WriteAll();
            });
            Application.Instance.AsyncInvoke(() => SpreadsheetControl.SuspendLayout());
            FlowsheetObject.SolveFlowsheet(false);
            Application.Instance.AsyncInvoke(() => SpreadsheetControl.ResumeLayout());
            FlowsheetObject.UpdateSpreadsheet(() =>
            {
                Spreadsheet.EvaluateAll();
            });
            ResultsControl.UpdateList();
            MaterialStreamListControl.UpdateList();
        }

        void Flowsheet_Shown(object sender, EventArgs e)
        {
            FlowsheetControl.FlowsheetSurface.ZoomAll(FlowsheetControl.Width, FlowsheetControl.Height);
            FlowsheetControl.FlowsheetSurface.ZoomAll(FlowsheetControl.Width, FlowsheetControl.Height);
            FlowsheetControl.Invalidate();
            ScriptListControl.UpdateList();
        }

        void Flowsheet_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (MessageBox.Show(this, "ConfirmFlowsheetExit".Localize(), "FlowsheetExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.No)
            {
                e.Cancel = true;
            }
        }

        void SaveSimulation(string path, bool backup = false)
        {

            path = Path.ChangeExtension(path, ".dwxmz");

            string xmlfile = Path.ChangeExtension(Path.GetTempFileName(), "xml");

            using (var fstream = new FileStream(xmlfile, FileMode.OpenOrCreate, FileAccess.ReadWrite))
            {
                FlowsheetObject.SaveToXML().Save(fstream);
            }
            
            var i_Files = new List<string>();
            if (File.Exists(xmlfile))
                i_Files.Add(xmlfile);

            ZipOutputStream strmZipOutputStream = default(ZipOutputStream);

            strmZipOutputStream = new ZipOutputStream(File.Create(path));

            strmZipOutputStream.SetLevel(9);

            if (FlowsheetObject.Options.UsePassword)
                strmZipOutputStream.Password = FlowsheetObject.Options.Password;

            string strFile = null;

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

            File.Delete(xmlfile);

            if (!backup)
            {
                FlowsheetObject.Options.FilePath = path;
                FlowsheetObject.ShowMessage("Simulation file successfully saved to '" + path + "'.", Interfaces.IFlowsheet.MessageType.Information);
                FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationSaved, Interfaces.Enums.Scripts.ObjectType.Simulation, "");
            }
            else
            {
                FlowsheetObject.ShowMessage("Backup file successfully saved to '" + path + "'.", Interfaces.IFlowsheet.MessageType.Information);
            }

        }

        void LoadObjects()
        {

            var dir = Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location);

            var calculatorassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.Thermodynamics.dll"));
            var unitopassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.UnitOperations.dll"));
            List<Type> availableTypes = new List<Type>();

            availableTypes.AddRange(calculatorassembly.GetTypes().Where(x => x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null ? true : false));
            availableTypes.AddRange(unitopassembly.GetTypes().Where(x => x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null ? true : false));

            List<ListItem> litems = new List<ListItem>();

            foreach (var item in availableTypes.OrderBy(x => x.Name))
            {
                if (!item.IsAbstract)
                {
                    var obj = (Interfaces.ISimulationObject)Activator.CreateInstance(item);
                    ObjectList.Add(obj.GetDisplayName(), obj);
                }
            }

        }

        Eto.Forms.Container SetupLogWindow()
        {

            var label = new Label { Text = "  " + "Log Panel", Font = SystemFonts.Bold(), VerticalAlignment = VerticalAlignment.Center };

            var outtxt = new ListBox(); //{ Font = Fonts.Monospace(SystemFonts.Default().Size - 1.0f)};

            var container = new TableLayout { Rows = { label, outtxt }, Spacing = new Size(5, 5) };

            var ctxmenu0 = new ContextMenu();

            var menuitem0 = new ButtonMenuItem { Text = "Clear List" };

            menuitem0.Click += (sender, e) =>
            {
                outtxt.Items.Clear();
            };

            ctxmenu0.Items.Add(menuitem0);

            var menuitem00 = new ButtonMenuItem { Text = "Copy Item Text to Clipboard" };

            menuitem00.Click += (sender, e) =>
            {
                new Clipboard().Text = outtxt.Items[outtxt.SelectedIndex].Text;
            };

            ctxmenu0.Items.Add(menuitem00);

            outtxt.MouseUp += (sender, e) =>
            {
                if (e.Buttons == MouseButtons.Alternate)
                {
                    ctxmenu0.Show(outtxt);
                }
            };

            FlowsheetObject.SetMessageListener((string text, Interfaces.IFlowsheet.MessageType mtype) =>
            {
                Application.Instance.AsyncInvoke(() =>
                {

                    var item = new ListItem { Text = "[" + DateTime.Now.ToString() + "] " + text };
                    switch (mtype)
                    {
                        case Interfaces.IFlowsheet.MessageType.Information:
                            item.Text = "[INFO] " + item.Text;
                            break;
                        case Interfaces.IFlowsheet.MessageType.GeneralError:
                            item.Text = "[ERROR] " + item.Text;
                            break;
                        case Interfaces.IFlowsheet.MessageType.Warning:
                            item.Text = "[WARNING] " + item.Text;
                            break;
                        case Interfaces.IFlowsheet.MessageType.Tip:
                            item.Text = "[TIP] " + item.Text;
                            break;
                        case Interfaces.IFlowsheet.MessageType.Other:
                            item.Text = "[OTHER] " + item.Text;
                            break;
                        default:
                            break;
                    }

                    outtxt.Items.Add(item);
                    outtxt.SelectedIndex = outtxt.Items.Count - 1;

                });
            });

            return container;

        }

        void SetupSelectedContextMenu()
        {

            selctxmenu.Items.Clear();

            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);

            var item0 = new ButtonMenuItem { Text = obj.GraphicObject.Tag, Enabled = false };

            var item1 = new CheckMenuItem { Text = "Toggle Active/Inactive", Checked = obj.GraphicObject.Active };

            item1.CheckedChanged += (sender, e) =>
            {
                obj.GraphicObject.Active = item1.Checked;
            };

            var item2 = new ButtonMenuItem { Text = "Edit Connections", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Electrical_96px.png")) };
            item2.Click += (sender, e) =>
            {
                EditConnections();
            };

            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream ||
                obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream)
            {
                item2.Enabled = false;
            }

            var item3 = new ButtonMenuItem { Text = "Calculate", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")) };
            item3.Click += (sender, e) => FlowsheetObject.SolveFlowsheet(false, obj);

            var item4 = new ButtonMenuItem { Text = "Debug", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Console_96px.png")) };
            item4.Click += (sender, e) =>
            {
                var txt = new TextArea { ReadOnly = true, Wrap = true, Font = Fonts.Monospace(SystemFonts.Default().Size) };
                txt.Text = "Please wait, debugging object...";
                var form1 = DWSIM.UI.Shared.Common.CreateDialog(txt, "Debugging" + " " + obj.GraphicObject.Tag + "...", 400, 300);
                Task.Factory.StartNew(() => { return obj.GetDebugReport(); }).ContinueWith(t => { Application.Instance.Invoke(() => { txt.Text = t.Result; }); }, TaskContinuationOptions.ExecuteSynchronously);
                form1.ShowModal(this);
            };

            var menuitem1 = new ButtonMenuItem { Text = "Edit Properties", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "EditProperty_96px.png")) };
            menuitem1.Click += (sender, e) =>
            {
                EditSelectedObjectProperties();
            };

            var menuitem2 = new ButtonMenuItem { Text = "View Results", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "ReportCard_96px.png")) };
            menuitem2.Click += (sender, e) =>
            {
                ViewSelectedObjectResults();
            };

            var item5 = new ButtonMenuItem { Text = "Clone", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Copy_96px.png")) };
            item5.Click += (sender, e) =>
            {
                var isobj = FlowsheetObject.AddObject(obj.GraphicObject.ObjectType, obj.GraphicObject.X + 50, obj.GraphicObject.Y + 50, obj.GraphicObject.Tag + "_CLONE");
                ((Interfaces.ICustomXMLSerialization)isobj).LoadData(((Interfaces.ICustomXMLSerialization)obj).SaveData());
            };

            var item6 = new ButtonMenuItem { Text = "Delete", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Delete_96px.png")) };

            item6.Click += (sender, e) =>
            {
                DeleteObject();
            };

            selctxmenu.Items.AddRange(new MenuItem[] { item0, item1, new SeparatorMenuItem(), item2, menuitem1, new SeparatorMenuItem(), item3, item4, new SeparatorMenuItem(), menuitem2, new SeparatorMenuItem(), item5, item6 });

            return;

        }

        void SetupDeselectedContextMenu()
        {

            deselctxmenu.Items.Clear();

            var item0 = new ButtonMenuItem { Text = "Add New Object", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")) };

            int currposx = (int)Mouse.Position.X - Location.X;
            int currposy = (int)Mouse.Position.Y - Location.Y;

            foreach (var item in ObjectList.Values)
            {
                var menuitem = new ButtonMenuItem { Text = item.GetDisplayName() };
                menuitem.Click += (sender2, e2) =>
                {
                    var z = FlowsheetControl.FlowsheetSurface.Zoom;
                    FlowsheetObject.AddObject(item.GetDisplayName(), (int)(currposx / z), (int)(currposy / z), "");
                    FlowsheetControl.Invalidate();
                };
                item0.Items.Add(menuitem);
            }

            deselctxmenu.Items.AddRange(new MenuItem[] { item0 });

            return;

        }

        void EditConnections()
        {
            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
            if (obj == null) return;
            var cont = UI.Shared.Common.GetDefaultContainer();
            UI.Shared.Common.CreateAndAddLabelRow(cont, "Object Connections".Localize());
            UI.Shared.Common.CreateAndAddDescriptionRow(cont, "ConnectorsEditorDescription".Localize());
            new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont);
            var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - Edit Connections", 500, 500, cont);
            form.ShowInTaskbar = false;
            form.Show();
            form.Width += 1;
        }

        private void EditSelectedObjectProperties()
        {
            var selobj = FlowsheetControl.FlowsheetSurface.SelectedObject;
            if (selobj != null)
            {
                if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Table)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.PropertyTableEditor { Table = (TableGraphic)selobj };
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.SpreadsheetTableEditor { Table = (SpreadsheetTableGraphic)selobj };
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_MasterTable)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.MasterPropertyTableEditor { Table = (MasterTableGraphic)selobj };
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Text)
                {
                    var txtobj = (TextGraphic)selobj;
                    var dyn1 = new DynamicLayout();
                    var fontsizes = new List<string>() { "6", "7", "8", "9", "10", "11", "12", "13", "14", "16", "18", "20", "22", "24" };
                    dyn1.CreateAndAddDropDownRow("Font size", fontsizes, fontsizes.IndexOf(txtobj.Size.ToString("N0")), (sender, e) => txtobj.Size = double.Parse(fontsizes[sender.SelectedIndex]));
                    var container = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };
                    container.Rows.Add(new TableRow(dyn1));
                    var txt = new TextArea { Text = txtobj.Text };
                    txt.TextChanged += (sender2, e2) =>
                    {
                        txtobj.Text = txt.Text;
                    };
                    container.Rows.Add(new TableRow(txt));
                    var editor = UI.Shared.Common.GetDefaultEditorForm("Edit Text Object", 500, 500, container, false);
                    editor.Show();
                }
                else
                {
                    var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
                    if (obj == null) return;
                    if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.CapeOpenUO)
                    {
                        ((UnitOperations.UnitOperations.CapeOpenUO)obj).Edit();
                    }
                    else
                    {
                        var cont = UI.Shared.Common.GetDefaultContainer();
                        if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
                        {
                            new DWSIM.UI.Desktop.Editors.MaterialStreamEditor(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DistillationColumn)
                        {
                            new DWSIM.UI.Desktop.Editors.DistillationColumnEditor(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AbsorptionColumn)
                        {
                            new DWSIM.UI.Desktop.Editors.AbsorptionColumnEditor(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Adjust)
                        {
                            DWSIM.UI.Desktop.Editors.LogicalBlocks.AdjustEditor.Populate(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Spec)
                        {
                            DWSIM.UI.Desktop.Editors.LogicalBlocks.SpecEditor.Populate(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Recycle)
                        {
                            DWSIM.UI.Desktop.Editors.LogicalBlocks.RecycleEditor.Populate(obj, cont);
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_EnergyRecycle)
                        {
                            DWSIM.UI.Desktop.Editors.LogicalBlocks.EnergyRecycleEditor.Populate(obj, cont);
                        }
                        else
                        {
                            new DWSIM.UI.Desktop.Editors.GeneralEditors(obj, cont);
                        }
                        if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Pipe)
                        {
                            cont.Tag = "General";
                            var cont2 = UI.Shared.Common.GetDefaultContainer();
                            cont2.Tag = "Hydraulic Profile";
                            new PipeHydraulicProfile(obj, cont2);
                            var cont3 = UI.Shared.Common.GetDefaultContainer();
                            cont3.Tag = "Thermal Profile";
                            new PipeThermalProfile(obj, cont3);
                            var form = UI.Shared.Common.GetDefaultTabbedForm(obj.GraphicObject.Tag + ": Edit Properties", 500, 500, new[] { cont, cont2, cont3 });
                            form.Show();
                            form.Width += 1;
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.CustomUO)
                        {
                            cont.Tag = "General";
                            var cont2 = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };
                            cont2.Tag = "Python Script";
                            var scripteditor = new DWSIM.UI.Controls.CodeEditorControl() { Text = ((CustomUO)obj).ScriptText };
                            var dyn1 = new DynamicLayout();
                            dyn1.CreateAndAddLabelAndButtonRow("Click to commit script changes", "Update", null, (sender, e) =>
                            {
                                ((CustomUO)obj).ScriptText = scripteditor.Text;
                            });
                            cont2.Rows.Add(new TableRow(dyn1));
                            cont2.Rows.Add(new TableRow(scripteditor));
                            var form = UI.Shared.Common.GetDefaultTabbedForm(obj.GraphicObject.Tag + ": Edit Properties", 800, 600, new Control[] { cont, cont2 });
                            form.Show();
                            form.Width += 1;
                        }
                        else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.HeatExchanger)
                        {
                            cont.Tag = "General";
                            var dyn1 = new UI.Desktop.Editors.ShellAndTubePropertiesView(obj);
                            dyn1.Tag = "Shell and Tube Properties";
                            var form = UI.Shared.Common.GetDefaultTabbedForm(obj.GraphicObject.Tag + ": Edit Properties", 500, 500, new Control[] { cont, dyn1 });
                            form.Show();
                            form.Width += 1;
                        }
                        else
                        {
                            var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + ": Edit Properties", 500, 500, cont);
                            form.Show();
                            form.Width += 1;
                        }
                    }
                }
            }
        }

        private void ViewSelectedObjectResults()
        {

            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
            if (obj == null) return;

            var report = obj.GetReport(FlowsheetObject.Options.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, FlowsheetObject.Options.NumberFormat);
            var container = new TableLayout();
            new DWSIM.UI.Desktop.Editors.Results(obj, container);
            var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - View Results", 500, 500, container, true);
            form.ShowInTaskbar = false;
            form.Show();
            form.Width += 1;

        }

        private void DeleteObject()
        {
            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
            if (obj == null) return;
            if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
            {
                FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj.GraphicObject, false, false);
            }
        }

        private void SaveBackupCopy()
        {
            try
            {
                string backupdir = "";
                if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
                {
                    backupdir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), "Documents", "DWSIM Application Data", "Backup") + Path.DirectorySeparatorChar;
                }
                else
                {
                    backupdir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.MyDocuments), "DWSIM Application Data", "Backup") + Path.DirectorySeparatorChar;
                }
                if (!Directory.Exists(backupdir)) Directory.CreateDirectory(backupdir);
                if (GlobalSettings.Settings.EnableBackupCopies)
                {
                    if (FlowsheetObject.Options.FilePath != "")
                    {
                        backupfilename = Path.GetFileName(FlowsheetObject.Options.FilePath);
                    }
                    var savefile = Path.Combine(backupdir, backupfilename);
                    SaveSimulation(savefile, true);
                }
            }
            catch (Exception ex)
            {
                Console.WriteLine("Error saving backup file: " + ex.ToString());
                FlowsheetObject.ShowMessage("Error saving backup file: " + ex.Message.ToString(), Interfaces.IFlowsheet.MessageType.GeneralError);
            }
        }

        private void SaveUserUnits()
        {
        
            var userunits = new List<DWSIM.SharedClasses.SystemsOfUnits.Units>();
            var toadd = new List<DWSIM.SharedClasses.SystemsOfUnits.Units>();

            try {
                userunits = Newtonsoft.Json.JsonConvert.DeserializeObject<List<DWSIM.SharedClasses.SystemsOfUnits.Units>>(GlobalSettings.Settings.UserUnits);
            }
            catch { }

            foreach (var unit in FlowsheetObject.AvailableSystemsOfUnits)
            {
                foreach (var unit2 in userunits)
                {
                    if (unit.Name == unit2.Name)
                    {
                        unit2.LoadData(((DWSIM.SharedClasses.SystemsOfUnits.Units)unit).SaveData());
                    }
                }
            }

            var names = userunits.Select((x) => x.Name).ToList();
            var defaults = new string[] {"SI", "CGS", "ENG", "C1", "C2", "C3", "C4", "C5" };

            foreach (var unit in FlowsheetObject.AvailableSystemsOfUnits)
            {
                if (!defaults.Contains(unit.Name) && !names.Contains(unit.Name))
                {
                    userunits.Add((DWSIM.SharedClasses.SystemsOfUnits.Units)unit);
                }
            }

            GlobalSettings.Settings.UserUnits = Newtonsoft.Json.JsonConvert.SerializeObject(userunits, Newtonsoft.Json.Formatting.Indented).Replace("\"", "\'");
        
        }

    }
}
