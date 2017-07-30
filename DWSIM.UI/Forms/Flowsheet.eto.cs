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

namespace DWSIM.UI.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;
        public DWSIM.UI.Desktop.Editors.Spreadsheet Spreadsheet;
        private DWSIM.UI.Controls.FlowsheetSurfaceControl FlowsheetControl;
        private TableLayout SpreadsheetControl;
        private TabPage TabPageSpreadsheet;

        string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

        ContextMenu selctxmenu, deselctxmenu;

        public Dictionary<string, Interfaces.ISimulationObject> ObjectList = new Dictionary<string, Interfaces.ISimulationObject>();

        void InitializeComponent()
        {

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

            ClientSize = new Size(1024, 768);

            var closeCommand = new Command { MenuText = "Close".Localize(), Shortcut = Application.Instance.CommonModifier | Keys.Q };
            closeCommand.Executed += (sender, e) => Close();

            var saveCommand = new Command { MenuText = "SaveFlowsheet".Localize() };

            var btnSave = new ButtonMenuItem { Text = "Save Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save.png")), Shortcut = Keys.S | Keys.Control };
            var btnSaveAs = new ButtonMenuItem { Text = "Save As...", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save_as.png")), Shortcut = Keys.S | Keys.Control | Keys.Shift };
            var btnClose = new ButtonMenuItem { Text = "Close Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Delete_96px.png")), Shortcut = Keys.Q | Keys.Control };
            var btnComps = new ButtonMenuItem { Text = "Compounds", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-thin_test_tube.png")), Shortcut = Keys.C | Keys.Control };
            var btnBasis = new ButtonMenuItem { Text = "Basis", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-math.png")), Shortcut = Keys.B | Keys.Control };
            var btnTools = new ButtonMenuItem { Text = "Tools", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-maintenance.png")) };
            var btnUtilities = new ButtonMenuItem { Text = "Utilities", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };
            var btnOptions = new ButtonMenuItem { Text = "Settings", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")), Shortcut = Keys.M | Keys.Control };
            var btnSolve = new ButtonMenuItem { Text = "Solve Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")), Shortcut = Keys.F5 };

            var btnObjects = new ButtonMenuItem { Text = "Add New Simulation Object", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")), Shortcut = Keys.A | Keys.Control };
            var btnInsertText = new ButtonMenuItem { Text = "Add New Text Block", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "TextWidth_96px.png")) };
            var btnInsertTable = new ButtonMenuItem { Text = "Add New Property Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Grid_96px.png")) };
            var btnInsertMasterTable = new ButtonMenuItem { Text = "Add New Master Property Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "GridView_96px.png")) };
            var btnInsertSpreadsheetTable = new ButtonMenuItem { Text = "Add New Linked Spreadsheet Table", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "PivotTable_96px.png")) };

            btnInsertText.Click += (sender, e) => {
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

            FlowsheetControl.MouseDoubleClick += (sender, e) => {
                var selobj = FlowsheetControl.FlowsheetSurface.SelectedObject;
                if (selobj != null) {
                    if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Table)
                    {
                        var editor = new DWSIM.UI.Desktop.Editors.Tables.PropertyTableEditor { Table = (TableGraphic)selobj };
                        editor.ShowModalAsync(FlowsheetControl);
                    } else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable)
                    {
                        var editor = new DWSIM.UI.Desktop.Editors.Tables.SpreadsheetTableEditor { Table = (SpreadsheetTableGraphic)selobj };
                        editor.ShowModalAsync(FlowsheetControl);
                    } else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_MasterTable)
                    {
                        var editor = new DWSIM.UI.Desktop.Editors.Tables.MasterPropertyTableEditor { Table = (MasterTableGraphic)selobj };
                        editor.ShowModalAsync(FlowsheetControl);
                    } else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Text)
                    {
                        var txtobj = (TextGraphic)selobj;
                        var container = new TableLayout();
                        var txt = new TextArea { Text = txtobj.Text };
                        txt.TextChanged += (sender2, e2) => {
                            txtobj.Text = txt.Text;
                        };
                        container.Rows.Add(new TableRow(txt));
                        var editor = UI.Shared.Common.GetDefaultEditorForm("Edit Text Object", 400, 400, container , false);
                        editor.Show();
                    }


                }
            };

            var chkSimSolver = new CheckMenuItem { Text = "Simultaneous Adjust Solver Active" };
            chkSimSolver.Checked = FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
            chkSimSolver.CheckedChanged += (sender, e) => {
                FlowsheetObject.Options.SimultaneousAdjustSolverEnabled = chkSimSolver.Checked;
            };

            Menu = new MenuBar();
            Menu.Items.Add(new ButtonMenuItem { Text = "File", Items = { btnSave, btnSaveAs, btnClose } });
            Menu.Items.Add(new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions } });
            Menu.Items.Add(new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable } });
            Menu.Items.Add(new ButtonMenuItem { Text = "Solver", Items = { btnSolve, chkSimSolver } });
            Menu.Items.Add(new ButtonMenuItem { Text = "Tools", Items = { btnTools } });
            Menu.Items.Add(new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities } });
            
            btnClose.Click += (sender, e) => Close();

            btnObjects.Click += (sender, e) => {
                var insform = new DWSIM.UI.Desktop.Editors.InsertObject { Flowsheet = FlowsheetObject, ObjList = ObjectList, FlowsheetHeight = FlowsheetControl.Height };
                insform.ShowModal(this);
            };

            btnComps.Click += (sender, e) => {
                var cont = new TableLayout();
                new DWSIM.UI.Desktop.Editors.Compounds(FlowsheetObject, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm("Simulation Compounds", 829, 500, cont, false);
                form.Show();
                form.Width += 1;
            };

            btnBasis.Click += (sender, e) =>
            {
                var cont1 = UI.Shared.Common.GetDefaultContainer();
                cont1.Tag = "Thermodynamics";
                new DWSIM.UI.Desktop.Editors.Models(FlowsheetObject, cont1);
                var cont2 = UI.Shared.Common.GetDefaultContainer();
                cont2.Tag = "Reactions";
                new DWSIM.UI.Desktop.Editors.ReactionsManager(FlowsheetObject, cont2);
                var form = UI.Shared.Common.GetDefaultTabbedForm("Simulation Basis", 500, 500, new []{cont1, cont2});
                form.Show();
                form.Width += 10;
            };

            btnOptions.Click += (sender, e) =>
            {
                var cont = UI.Shared.Common.GetDefaultContainer();
                new DWSIM.UI.Desktop.Editors.SimulationSettings(FlowsheetObject, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm("Other Settings", 500, 500, cont);
                form.Show();
                form.Width += 1;
            };

            btnSolve.Click += (sender, e) => {
                SolveFlowsheet();
            };

            this.KeyDown += (sender, e) =>
            {
                if (e.Key == Keys.F5)
                {
                    SolveFlowsheet();
                };
            };

            btnSave.Click += (sender, e) =>
            {

                if (FlowsheetObject.Options.FilePath != "")
                {
                    SaveSimulation(FlowsheetObject.Options.FilePath);
                }
                else
                {
                    btnSaveAs.PerformClick();
                }

            };

            btnSaveAs.Click += (sender, e) =>
            {

                var dialog = new SaveFileDialog();
                dialog.Title = "Save File".Localize();
                dialog.Filters.Add(new FileDialogFilter("XML Simulation File (Compressed)".Localize(), new[] { ".dwxmz" }));
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

            SpreadsheetControl =  Spreadsheet.GetSpreadsheet(FlowsheetObject);
            
            var tabholder = new TabControl();
            TabPageSpreadsheet = new TabPage { Content = SpreadsheetControl, Text = "Spreadsheet" };
            tabholder.Pages.Add(new TabPage { Content = FlowsheetControl, Text = "Flowsheet" });
            tabholder.Pages.Add(new TabPage { Content = new Panel(), Text = "Material Streams" });
            tabholder.Pages.Add(TabPageSpreadsheet);
            tabholder.Pages.Add(new TabPage { Content = new Panel(), Text = "Scripts" });
            tabholder.Pages.Add(new TabPage { Content = new Panel(), Text = "Results" });

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
                    if (FlowsheetControl.FlowsheetSurface.SelectedObject != null)
                    {
                        SetupSelectedContextMenu();
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
            Application.Instance.AsyncInvoke(() => TabPageSpreadsheet.Enabled = false);
            FlowsheetObject.SolveFlowsheet();
            Application.Instance.AsyncInvoke(() => TabPageSpreadsheet.Enabled = true);
            FlowsheetObject.UpdateSpreadsheet(() =>
            {
                Spreadsheet.EvaluateAll();
            });
        }

        void Flowsheet_Shown(object sender, EventArgs e)
        {
            FlowsheetControl.FlowsheetSurface.ZoomAll(FlowsheetControl.Width, FlowsheetControl.Height);
            FlowsheetControl.FlowsheetSurface.ZoomAll(FlowsheetControl.Width, FlowsheetControl.Height);
            FlowsheetControl.Invalidate();
        }

        void Flowsheet_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (MessageBox.Show(this, "ConfirmFlowsheetExit".Localize(), "FlowsheetExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.No)
            {
                e.Cancel = true;
            }
        }

        void SaveSimulation(string path)
        {

            FlowsheetObject.SaveToXML().Save(path);

            string xmlfile = Path.ChangeExtension(Path.GetTempFileName(), "xml");

            FlowsheetObject.SaveToXML().Save(xmlfile);

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

            FlowsheetObject.Options.FilePath = path;

            FlowsheetObject.ShowMessage("File saved successfully.", Interfaces.IFlowsheet.MessageType.Information);

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
                var cont = UI.Shared.Common.GetDefaultContainer();
                UI.Shared.Common.CreateAndAddLabelRow(cont, "Object Connections".Localize());
                UI.Shared.Common.CreateAndAddDescriptionRow(cont, "ConnectorsEditorDescription".Localize());
                new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - Edit Connections", 500, 500, cont);
                form.ShowInTaskbar = false;
                form.Show();
                form.Width += 1;
            };

            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream ||
                obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream)
            {
                item2.Enabled = false;
            }

            var item3 = new ButtonMenuItem { Text = "Calculate", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")) };
            item3.Click += (sender, e) => FlowsheetObject.SolveFlowsheet(obj);

            var item4 = new ButtonMenuItem { Text = "Debug", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Console_96px.png")) };
            item4.Click += (sender, e) =>
            {
                var txt = new TextArea { ReadOnly = true, Wrap = true };
                txt.Text = "Please wait, debugging object...";
                var form1 = DWSIM.UI.Shared.Common.CreateDialog(txt, "Debugging" + " " + obj.GraphicObject.Tag + "...", 400, 300);
                Task.Factory.StartNew(() => { return obj.GetDebugReport(); }).ContinueWith(t => { Application.Instance.Invoke(() => { txt.Text = t.Result; }); }, TaskContinuationOptions.ExecuteSynchronously);
                form1.ShowModal(this);
            };

            var menuitem1 = new ButtonMenuItem { Text = "Edit Properties", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "EditProperty_96px.png")) };
            menuitem1.Click += (sender, e) =>
            {
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
                        var scripteditor = new TextArea() {Text = ((CustomUO)obj).ScriptText, Wrap = false, AcceptsReturn = true, AcceptsTab = true, SpellCheck = false, Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize)};
                        scripteditor.Tag = "Python Script";
                        var form = UI.Shared.Common.GetDefaultTabbedForm(obj.GraphicObject.Tag + ": Edit Properties", 500, 500, new Control[] { cont, scripteditor });
                        form.Show();
                        form.Width += 1;
                    }
                    else {
                        var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + ": Edit Properties", 500, 500, cont);
                        form.Show();
                        form.Width += 1;                    
                    }
                }
            };

            var menuitem2 = new ButtonMenuItem { Text = "View Results", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "ReportCard_96px.png")) };
            menuitem2.Click += (sender, e) =>
            {
                var report = obj.GetReport(FlowsheetObject.Options.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, FlowsheetObject.Options.NumberFormat);
                var container = new TableLayout();
                new DWSIM.UI.Desktop.Editors.Results(obj, container);
                var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - View Results", 500, 500, container, true);
                form.ShowInTaskbar = false;
                form.Show();
                form.Width += 1;
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
                if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                {
                    FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj.GraphicObject, false, false);
                }
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

    }
}
