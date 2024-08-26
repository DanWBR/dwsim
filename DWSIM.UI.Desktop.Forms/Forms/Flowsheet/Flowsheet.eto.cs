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
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables;
using System.Timers;
using System.Diagnostics;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts;
using System.Reflection;
using s = DWSIM.GlobalSettings.Settings;
using DWSIM.UI.Desktop.Editors.Charts;
using DWSIM.UI.Desktop.Editors.Dynamics;
using SkiaSharp;
using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;
        public Spreadsheet Spreadsheet;
        private Controls.FlowsheetSurfaceControlBase FlowsheetControl;

        private DocumentControl EditorHolder;

        public Color BGColor = new Color(0.051f, 0.447f, 0.651f);

        private PixelLayout SpreadsheetControl;

        private Eto.Forms.Splitter SplitterFlowsheet;

        private DocumentPage DocumentPageSpreadsheet;
        private DocumentControl DocumentContainer;
        private bool TabSwitch = true;

        private DWSIM.UI.Desktop.Editors.ResultsViewer ResultsControl;

        private DynamicsManagerControl DynManagerControl;

        private DynamicsIntegratorControl DynIntegratorControl;

        private FileExplorerControl FileExplControl;

        public ChartManager ChartsControl;

        private DWSIM.UI.Desktop.Editors.MaterialStreamListViewer MaterialStreamListControl;

        private DWSIM.UI.Desktop.Editors.ScriptManagerBase ScriptListControl;

        string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

        string bitmapprefix = "";

        private string backupfilename = "";

        public bool newsim = false;

        ContextMenu selctxmenu, deselctxmenu;

        public Dictionary<string, Interfaces.ISimulationObject> ObjectList = new Dictionary<string, Interfaces.ISimulationObject>();

        public Action ActComps, ActBasis, ActGlobalOptions, ActSave, ActSaveAs, ActOptions, ActZoomIn, ActZoomOut, ActZoomFit, ActZoomDefault, ActSimultAdjustSolver, ActInspector;
        public Action ActDrawGrid, ActSnapToGrid, ActMultiSelect, ActAlignLefts, ActAlignCenters, ActAlignRights, ActAlignTops, ActAlignMiddles, ActAlignBottoms, ActHorizAlign, ActVertAlign;

        private double sf = s.UIScalingFactor;

        private CheckBox btnmSnapToGrid, btnmDrawGrid, btnmMultiSelect;

        private CheckToolItem chkmDynamics;

        private CheckMenuItem chkDynamics;

        private DropDown ddstates;

        private Timer BackupTimer;

        protected override void Dispose(bool disposing)
        {
            if (disposing)
            {
                if (!DynManagerControl.IsDisposed)
                {
                    DynManagerControl?.Dispose();
                    DynManagerControl = null;
                }
                if (!DynIntegratorControl.IsDisposed)
                {
                    DynIntegratorControl?.Dispose();
                    DynIntegratorControl = null;
                }
                if (!MaterialStreamListControl.IsDisposed)
                {
                    MaterialStreamListControl?.Dispose();
                    MaterialStreamListControl = null;
                }
                if (!SpreadsheetControl.IsDisposed)
                {
                    SpreadsheetControl?.Dispose();
                    SpreadsheetControl = null;
                }
                if (!ChartsControl.IsDisposed)
                {
                    ChartsControl?.Dispose();
                    ChartsControl = null;
                }
                if (!FileExplControl.IsDisposed)
                {
                    FileExplControl?.Dispose();
                    FileExplControl = null;
                    FlowsheetObject.FileDatabaseProvider.ReleaseDatabase();
                }
                if (!FlowsheetControl.IsDisposed)
                {
                    FlowsheetControl?.Dispose();
                    FlowsheetControl = null;
                }
                BackupTimer?.Stop();
                BackupTimer?.Dispose();
                FlowsheetObject.Reset();
                FlowsheetObject.AvailableCompounds.Clear();
                FlowsheetObject.AvailablePropertyPackages.Clear();
                FlowsheetObject.AvailableSystemsOfUnits.Clear();
                FlowsheetObject = null;
            }
            base.Dispose(disposing);
        }

        void InitializeComponent()
        {

            bitmapprefix = Path.Combine(Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location), "bitmaps") + Path.DirectorySeparatorChar;

            if (s.DarkMode) BGColor = SystemColors.ControlBackground;

            if (Application.Instance.Platform.IsWpf)
            {
                GlobalSettings.Settings.DpiScale = Screen.RealDPI / 96.0;
            }

            // setup backup timer

            backupfilename = DateTime.Now.ToString().Replace('-', '_').Replace(':', '_').Replace(' ', '_').Replace('/', '_') + ".dwxmz";

            BackupTimer = new Timer(GlobalSettings.Settings.BackupInterval * 60 * 1000);
            BackupTimer.Elapsed += (sender, e) =>
            {
                Task.Factory.StartNew(() => SaveBackupCopy());
            };
            BackupTimer.Enabled = true;
            BackupTimer.Start();

            WindowState = Eto.Forms.WindowState.Maximized;

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };
            FlowsheetObject.Initialize();

            FlowsheetObject.DynamicsManager.RunSchedule = (schname) =>
            {
                FlowsheetObject.DynamicsManager.CurrentSchedule = FlowsheetObject.DynamicsManager.GetSchedule(schname).ID;
                return DynamicsIntegratorControl.RunIntegrator(false, false, FlowsheetObject, null);
            };

            Title = "New Flowsheet";

            if (s.FlowsheetRenderer == s.SkiaCanvasRenderer.CPU)
            {
                FlowsheetControl = new DWSIM.UI.Controls.FlowsheetSurfaceControl() { FlowsheetObject = FlowsheetObject, FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface() };
            }
            else
            {
                FlowsheetControl = new DWSIM.UI.Controls.FlowsheetSurfaceControl_OpenGL() { FlowsheetObject = FlowsheetObject, FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface() };
            }

            FlowsheetObject.FlowsheetControl = FlowsheetControl;

            FlowsheetControl.FlowsheetSurface.Flowsheet = FlowsheetObject;

            FlowsheetControl.FlowsheetSurface.InvalidateCallback = (() =>
            {
                Application.Instance.Invoke(() =>
                {
                    FlowsheetControl.Invalidate();
                });
            });

            ClientSize = new Size((int)(sf * 1024), (int)(sf * 768));

            // main panels

            Spreadsheet = new DWSIM.UI.Desktop.Editors.Spreadsheet(FlowsheetObject) { ObjList = ObjectList };

            FlowsheetObject.LoadSpreadsheetData = new Action<XDocument>((xdoc) =>
            {
                if (xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet") != null)
                {
                    var rgfdataelement = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData");
                    if (rgfdataelement != null)
                    {
                        Application.Instance.Invoke(() =>
                        {
                            string rgfdata = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value;
                            rgfdata = rgfdata.Replace("Calibri", "Arial").Replace("10.25", "10");
                            Dictionary<string, string> sdict = new Dictionary<string, string>();
                            sdict = Newtonsoft.Json.JsonConvert.DeserializeObject<Dictionary<string, string>>(rgfdata);
                            Spreadsheet.Sheet.RemoveWorksheet(0);
                            Spreadsheet.Loaded = false;
                            foreach (var item in sdict)
                            {
                                var tmpfile = DWSIM.SharedClasses.Utility.GetTempFileName();
                                var sheet = Spreadsheet.Sheet.NewWorksheet(item.Key);
                                var xmldoc = Newtonsoft.Json.JsonConvert.DeserializeXmlNode(item.Value);
                                xmldoc.Save(tmpfile);
                                sheet.LoadRGF(tmpfile);
                                File.Delete(tmpfile);
                            }
                            Spreadsheet.Loaded = true;
                            Spreadsheet.Sheet.CurrentWorksheet = Spreadsheet.Sheet.Worksheets[0];
                        });
                    }
                    else
                    {
                        string data1 = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data1").Value;
                        string data2 = xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("Data2").Value;
                        if (!string.IsNullOrEmpty(data1)) Spreadsheet.CopyDT1FromString(data1);
                        if (!string.IsNullOrEmpty(data2)) Spreadsheet.CopyDT2FromString(data2);
                        Application.Instance.Invoke(() =>
                        {
                            Spreadsheet.CopyFromDT();
                            Spreadsheet.EvaluateAll();
                        });
                    }
                }
            });

            FlowsheetObject.SaveSpreadsheetData = new Action<XDocument>((xdoc) =>
            {
                xdoc.Element("DWSIM_Simulation_Data").Add(new XElement("Spreadsheet"));
                xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Add(new XElement("RGFData"));
                var tmpfile = DWSIM.SharedClasses.Utility.GetTempFileName();
                Application.Instance.Invoke(() =>
                {
                    Dictionary<string, string> sdict = new Dictionary<string, string>();
                    foreach (var sheet in Spreadsheet.Sheet.Worksheets)
                    {
                        var tmpfile2 = DWSIM.SharedClasses.Utility.GetTempFileName();
                        sheet.SaveRGF(tmpfile2);
                        var xmldoc = new XmlDocument();
                        xmldoc.Load(tmpfile2);
                        sdict.Add(sheet.Name, Newtonsoft.Json.JsonConvert.SerializeXmlNode(xmldoc));
                        File.Delete(tmpfile2);
                    }
                    xdoc.Element("DWSIM_Simulation_Data").Element("Spreadsheet").Element("RGFData").Value = Newtonsoft.Json.JsonConvert.SerializeObject(sdict);
                });
            });

            FlowsheetObject.RetrieveSpreadsheetData = new Func<string, List<string[]>>((range) =>
            {
                return Spreadsheet.GetDataFromRange(range);
            });

            FlowsheetObject.RetrieveSpreadsheetFormat = new Func<string, List<string[]>>((range) =>
            {
                return Spreadsheet.GetFormatFromRange(range);
            });

            SpreadsheetControl = Spreadsheet.GetSpreadsheet(FlowsheetObject);

            ChartsControl = new ChartManager(FlowsheetObject);

            FlowsheetObject.AddChart = (dpage) =>
            {
                Application.Instance.Invoke(() =>
                {
                    ChartsControl.TabControl.Pages.Add(dpage);
                    DocumentContainer.SelectedIndex = 3;
                    ChartsControl.TabControl.SelectedPage = dpage;
                });
            };

            ResultsControl = new ResultsViewer(FlowsheetObject);

            MaterialStreamListControl = new MaterialStreamListViewer(FlowsheetObject);

            DynManagerControl = new DynamicsManagerControl(FlowsheetObject);

            DynIntegratorControl = new DynamicsIntegratorControl(FlowsheetObject);

            FileExplControl = new FileExplorerControl(FlowsheetObject);

            // if automation then stop loading UI controls

            if (GlobalSettings.Settings.AutomationMode) return;

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico", this.GetType().Assembly);

            LoadObjects();

            // toolbar

            var btnmSave = new ButtonToolItem { ToolTip = "Save Flowsheet", Image = new Bitmap(bitmapprefix + "icons8-save.png") };

            var btnmSolve = new ButtonToolItem { ToolTip = "Solve Flowsheet", Image = new Bitmap(bitmapprefix + "icons8-play.png") };
            var btnmStop = new ButtonToolItem { ToolTip = "Stop Solving", Image = new Bitmap(bitmapprefix + "icons8-stop.png") };
            var btnmSimultSolve = new CheckToolItem { ToolTip = "Enable/Disable Simultaneous Adjust Solver", Image = new Bitmap(bitmapprefix + "Checked_96px.png") };

            var btnmComps = new ButtonToolItem { ToolTip = "Compounds", Image = new Bitmap(bitmapprefix + "icons8-thin_test_tube.png") };
            var btnmBasis = new ButtonToolItem { ToolTip = "Basis", Image = new Bitmap(bitmapprefix + "icons8-math.png") };
            var btnmOptions = new ButtonToolItem { ToolTip = "Settings", Image = new Bitmap(bitmapprefix + "icons8-sorting_options.png") };

            var btnmUndo = new ButtonToolItem { ToolTip = "Undo Action", Image = new Bitmap(bitmapprefix + "back_arrow_96px.png") };
            var btnmRedo = new ButtonToolItem { ToolTip = "Redo Action", Image = new Bitmap(bitmapprefix + "forward_button_96px.png") };

            chkmDynamics = new CheckToolItem { Checked = FlowsheetObject.DynamicMode, ToolTip = "Enable/Disable Dynamic Mode", Image = new Bitmap(bitmapprefix + "icons8-exercise.png") };
            var btnmDynManager = new ButtonToolItem { ToolTip = "Dynamics Manager", Image = new Bitmap(bitmapprefix + "icons8-module.png") };
            var btnmDynIntegrator = new ButtonToolItem { ToolTip = "Dynamics Integrator Controls", Image = new Bitmap(bitmapprefix + "icons8-ecg.png") };

            var chkmInspector = new CheckToolItem { Checked = s.InspectorEnabled, ToolTip = "Enable/Disable Inspector", Image = new Bitmap(bitmapprefix + "icons8-spy_male.png") };
            var btnmInspector = new ButtonToolItem { ToolTip = "View Inspector Window", Image = new Bitmap(bitmapprefix + "icons8-spy_filled.png") };

            if (Application.Instance.Platform.IsMac)
            {
                btnmUndo.Text = "Undo";
                btnmRedo.Text = "Redo";
                btnmSave.Text = "Save";
                btnmSolve.Text = "Solve Flowsheet";
                btnmStop.Text = "Stop Solving";
                btnmSimultSolve.Text = "E/D Simult. Adj. Solver";
                btnmComps.Text = "Compounds";
                btnmBasis.Text = "Basis";
                btnmOptions.Text = "Settings";
                chkmInspector.Text = "Enable/Disable Inspector";
                btnmInspector.Text = "View Inspector";
                chkmDynamics.Text = "Dynamic Mode";
                btnmDynIntegrator.Text = "Integrator";
                btnmDynManager.Text = "Manager";
            }

            ToolBar = new ToolBar
            {
                Items = { btnmSave, new SeparatorToolItem { Type = SeparatorToolItemType.Space },
                btnmComps, btnmBasis, btnmOptions,
                new SeparatorToolItem{ Type = SeparatorToolItemType.Space },
                btnmUndo, btnmRedo,
                new SeparatorToolItem{ Type = SeparatorToolItemType.Space },
                btnmSolve, btnmStop, btnmSimultSolve,
                new SeparatorToolItem{ Type = SeparatorToolItemType.Space },
                chkmDynamics, btnmDynManager, btnmDynIntegrator,
                new SeparatorToolItem{ Type = SeparatorToolItemType.Space},
                chkmInspector, btnmInspector
                }
            };

            // menu items

            var btnSave = new ButtonMenuItem { Text = "Save Flowsheet", Image = new Bitmap(bitmapprefix + "icons8-save.png"), Shortcut = Keys.S | Application.Instance.CommonModifier };
            var btnSaveAs = new ButtonMenuItem { Text = "Save As...", Image = new Bitmap(bitmapprefix + "icons8-save_as.png"), Shortcut = Keys.S | Application.Instance.CommonModifier | Keys.Shift };
            var btnClose = new ButtonMenuItem { Text = "Close Flowsheet", Image = new Bitmap(bitmapprefix + "Delete_96px.png"), Shortcut = Keys.Q | Application.Instance.CommonModifier };
            var btnComps = new ButtonMenuItem { Text = "Compounds", Image = new Bitmap(bitmapprefix + "icons8-thin_test_tube.png"), Shortcut = Keys.C | Application.Instance.AlternateModifier };
            var btnBasis = new ButtonMenuItem { Text = "Basis", Image = new Bitmap(bitmapprefix + "icons8-math.png"), Shortcut = Keys.B | Application.Instance.AlternateModifier };
            var btnOptions = new ButtonMenuItem { Text = "Flowsheet Settings", Image = new Bitmap(bitmapprefix + "icons8-sorting_options.png"), Shortcut = Keys.M | Application.Instance.AlternateModifier };
            var btnGlobalOptions = new ButtonMenuItem { Text = "Global Settings", Image = new Bitmap(bitmapprefix + "icons8-sorting_options.png"), Shortcut = Keys.G | Application.Instance.AlternateModifier };
            var btnSolve = new ButtonMenuItem { Text = "Solve Flowsheet", Image = new Bitmap(bitmapprefix + "icons8-play.png"), Shortcut = Keys.F5 };
            var btnSolveC = new ButtonMenuItem { Text = "Solve Flowsheet (Custom Calculation Order)", Image = new Bitmap(bitmapprefix + "icons8-play.png"), Shortcut = Keys.F5 | Application.Instance.CommonModifier | Application.Instance.AlternateModifier };

            // actions

            FlowsheetObject.UpdateEditorPanels = () => UpdateEditorPanels();

            FlowsheetObject.CloseEditorPanels = () => CloseOpenedEditorPanels();

            FlowsheetObject.UpdateSurface = (() =>
            {
                Application.Instance.Invoke(() =>
                {
                    FlowsheetControl.Invalidate();
                });
            });

            ActComps = () =>
            {
                var cont = new TableLayout();
                var editor = new DWSIM.UI.Desktop.Editors.Compounds(FlowsheetObject, cont);
                cont.Tag = "Simulation Compounds";

                var cont2 = new Desktop.Editors.CompoundTools(FlowsheetObject);
                cont2.Tag = "Compound Tools";

                var form = UI.Shared.Common.GetDefaultTabbedForm("Compounds", (int)(sf * 920), (int)(sf * 500), new Control[] { cont, cont2 });

                editor.listcontainer.CellEdited += (sender, e) => UpdateEditorPanels();

                form.Show();
                form.Center();
            };

            ActBasis = () =>
            {
                var cont1 = UI.Shared.Common.GetDefaultContainer();
                cont1.Tag = "Thermodynamics";
                new DWSIM.UI.Desktop.Editors.Models(FlowsheetObject, cont1);
                var cont2 = UI.Shared.Common.GetDefaultContainer();
                cont2.Tag = "Reactions";
                new DWSIM.UI.Desktop.Editors.ReactionsManager(FlowsheetObject, cont2);
                var form = UI.Shared.Common.GetDefaultTabbedForm("Simulation Basis", (int)(sf * 800), (int)(sf * 600), new[] { cont1, cont2 });
                form.Show();
                form.Center();
                form.Width += 10;
            };

            ActOptions = () =>
            {
                FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.SimulationSettings);
                var cont = UI.Shared.Common.GetDefaultContainer();
                new DWSIM.UI.Desktop.Editors.SimulationSettings(FlowsheetObject, cont);
                cont.Tag = "Settings";
                var cont2 = new UI.Desktop.Editors.FloatingTablesView(FlowsheetObject);
                cont2.Tag = "Visible Properties";
                var form = UI.Shared.Common.GetDefaultTabbedForm("Flowsheet Settings", (int)(sf * 800), (int)(sf * 600), new[] { cont, cont2 });
                form.Show();
                form.Center();
                form.Width += 1;
            };

            ActGlobalOptions = () =>
            {
                new Forms.GeneralSettings().GetForm().Show();
            };

            ActSave = () =>
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
                    MessageBox.Show(ex.ToString(), "Error saving file", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                }
            };

            ActSaveAs = () =>
            {
                var dialog = new SaveFileDialog();
                dialog.Title = "Save File".Localize();
                dialog.Filters.Add(new FileFilter("XML Simulation File (Compressed)".Localize(), new[] { ".dwxmz" }));
                dialog.Filters.Add(new FileFilter("Mobile XML Simulation File (Android/iOS)".Localize(), new[] { ".xml" }));
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(this) == DialogResult.Ok)
                {
                    if (!Path.HasExtension(dialog.FileName))
                    {
                        if (dialog.CurrentFilterIndex == 0)
                        {
                            dialog.FileName = Path.ChangeExtension(dialog.FileName, ".dwxmz");
                        }
                        else
                        {
                            dialog.FileName = Path.ChangeExtension(dialog.FileName, ".xml");
                        }
                    }
                    SaveSimulation(dialog.FileName);
                }
            };

            ActZoomIn = () =>
            {
                FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
                FlowsheetControl.FlowsheetSurface.Zoom += 0.1f;
                FlowsheetControl.Invalidate();
            };

            ActZoomOut = () =>
            {
                FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
                FlowsheetControl.FlowsheetSurface.Zoom -= 0.1f;
                FlowsheetControl.Invalidate();
            };

            ActZoomFit = () =>
            {
                FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
                FlowsheetControl.FlowsheetSurface.ZoomAll((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                FlowsheetControl.FlowsheetSurface.ZoomAll((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                FlowsheetControl.FlowsheetSurface.Center((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                FlowsheetControl.Invalidate();
            };

            ActZoomDefault = () =>
            {
                FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
                FlowsheetControl.FlowsheetSurface.Zoom = 1.0f;
                FlowsheetControl.Invalidate();
            };

            ActInspector = () =>
            {
                var iwindow = DWSIM.Inspector.Window_Eto.GetInspectorWindow();
                var iform = DWSIM.UI.Shared.Common.GetDefaultEditorForm("DWSIM - Solution Inspector", (int)(sf * 1024), (int)(sf * 768), iwindow, false);
                iform.WindowState = WindowState.Maximized;
                iform.Show();
            };

            ActDrawGrid = () =>
            {
                FlowsheetObject.Options.FlowsheetDisplayGrid = !FlowsheetObject.Options.FlowsheetDisplayGrid;
                FlowsheetControl.FlowsheetSurface.ShowGrid = btnmDrawGrid.Checked.GetValueOrDefault();
                FlowsheetControl.Invalidate();
            };

            ActSnapToGrid = () =>
            {
                FlowsheetObject.Options.FlowsheetSnapToGrid = !FlowsheetObject.Options.FlowsheetSnapToGrid;
                FlowsheetControl.FlowsheetSurface.SnapToGrid = btnmSnapToGrid.Checked.GetValueOrDefault();
                FlowsheetControl.Invalidate();
            };

            ActMultiSelect = () =>
            {
                FlowsheetObject.Options.FlowsheetMultiSelectMode = !FlowsheetObject.Options.FlowsheetMultiSelectMode;
                FlowsheetControl.FlowsheetSurface.MultiSelectMode = btnmMultiSelect.Checked.GetValueOrDefault();
                FlowsheetControl.Invalidate();
            };

            FlowsheetObject.ActBasis = ActBasis;
            FlowsheetObject.ActComps = ActComps;
            FlowsheetObject.ActGlobalOptions = ActGlobalOptions;
            FlowsheetObject.ActOptions = ActOptions;
            FlowsheetObject.ActSave = ActSave;
            FlowsheetObject.ActSaveAs = ActSaveAs;
            FlowsheetObject.ActZoomFit = ActZoomFit;
            FlowsheetObject.ActZoomIn = ActZoomIn;
            FlowsheetObject.ActZoomOut = ActZoomOut;
            FlowsheetObject.ActZoomDefault = ActZoomDefault;
            FlowsheetObject.ActDrawGrid = ActDrawGrid;
            FlowsheetObject.ActSnapToGrid = ActSnapToGrid;
            FlowsheetObject.ActMultiSelect = ActMultiSelect;
            FlowsheetObject.ActAlignLefts = ActAlignLefts;
            FlowsheetObject.ActAlignCenters = ActAlignCenters;
            FlowsheetObject.ActAlignRights = ActAlignRights;
            FlowsheetObject.ActAlignTops = ActAlignTops;
            FlowsheetObject.ActAlignMiddles = ActAlignMiddles;
            FlowsheetObject.ActAlignBottoms = ActAlignBottoms;
            FlowsheetObject.ActVertAlign = ActVertAlign;
            FlowsheetObject.ActHorizAlign = ActHorizAlign;

            // button click events

            btnmUndo.Click += (sender, e) => {
                if (!FlowsheetObject.FlowsheetOptions.EnabledUndoRedo)
                {
                    FlowsheetObject.ShowMessage("Undo/Redo feature is disabled (Edit > Flowsheet Settings > General > Enable Undo/Redo)", Interfaces.IFlowsheet.MessageType.Tip);
                }
                else {
                    FlowsheetObject.ProcessUndo();
                }
            };

            btnmRedo.Click += (sender, e) => {
                if (!FlowsheetObject.FlowsheetOptions.EnabledUndoRedo)
                {
                    FlowsheetObject.ShowMessage("Undo/Redo feature is disabled (Edit > Flowsheet Settings > General > Enable Undo/Redo)", Interfaces.IFlowsheet.MessageType.Tip);
                }
                else
                {
                    FlowsheetObject.ProcessRedo();
                }
            };

            chkmInspector.CheckedChanged += (sender, e) => s.InspectorEnabled = chkmInspector.Checked;

            btnmInspector.Click += (sender, e) => ActInspector.Invoke();

            btnClose.Click += (sender, e) => Close();

            btnComps.Click += (sender, e) => ActComps.Invoke();
            btnmComps.Click += (sender, e) => ActComps.Invoke();

            btnBasis.Click += (sender, e) => ActBasis.Invoke();
            btnmBasis.Click += (sender, e) => ActBasis.Invoke();

            btnOptions.Click += (sender, e) => ActOptions.Invoke();
            btnmOptions.Click += (sender, e) => ActOptions.Invoke();

            btnGlobalOptions.Click += (sender, e) => ActGlobalOptions.Invoke();

            btnSolve.Click += (sender, e) => SolveFlowsheet(false);
            btnmSolve.Click += (sender, e) => SolveFlowsheet(false);
            btnSolveC.Click += (sender, e) => SolveFlowsheet(true);

            btnmStop.Click += (sender, e) =>
            {
                s.CalculatorStopRequested = true;
                if (s.TaskCancellationTokenSource != null)
                {
                    s.TaskCancellationTokenSource.Cancel();
                }
            };

            btnSave.Click += (sender, e) => ActSave.Invoke();
            btnmSave.Click += (sender, e) => ActSave.Invoke();

            btnSaveAs.Click += (sender, e) => ActSaveAs.Invoke();

            var btnUtilities_TrueCriticalPoint = new ButtonMenuItem { Text = "True Critical Point", Image = new Bitmap(bitmapprefix + "icons8-swiss_army_knife.png") };
            var btnUtilities_BinaryEnvelope = new ButtonMenuItem { Text = "Binary Envelope", Image = new Bitmap(bitmapprefix + "icons8-swiss_army_knife.png") };
            var btnUtilities_PhaseEnvelope = new ButtonMenuItem { Text = "Phase Envelope", Image = new Bitmap(bitmapprefix + "icons8-swiss_army_knife.png") };

            btnUtilities_TrueCriticalPoint.Click += (sender, e) =>
            {
                var tcp = new Desktop.Editors.Utilities.TrueCriticalPointView(FlowsheetObject);
                var form = DWSIM.UI.Shared.Common.GetDefaultEditorForm("True Critical Point", (int)(sf * 500), (int)(sf * 500), tcp);
                form.Show();
                form.Center();
            };

            btnUtilities_BinaryEnvelope.Click += (sender, e) =>
            {
                var bpe = new Desktop.Editors.Utilities.BinaryEnvelopeView(FlowsheetObject);
                var form = DWSIM.UI.Shared.Common.GetDefaultEditorForm("Binary Phase Envelope", (int)(sf * 1024), (int)(sf * 768), bpe, false);
                form.Show();
                form.Center();
            };

            btnUtilities_PhaseEnvelope.Click += (sender, e) =>
            {
                var pe = new Desktop.Editors.Utilities.PhaseEnvelopeView(FlowsheetObject);
                var form = DWSIM.UI.Shared.Common.GetDefaultEditorForm("Phase Envelope", (int)(sf * 1024), (int)(sf * 768), pe, false);
                form.Show();
                form.Center();
            };

            var btnObjects = new ButtonMenuItem { Text = "Add New Simulation Object", Image = new Bitmap(bitmapprefix + "icons8-workflow.png"), Shortcut = Keys.A | Application.Instance.AlternateModifier };
            var btnInsertText = new ButtonMenuItem { Text = "Add New Text Block", Image = new Bitmap(bitmapprefix + "TextWidth_96px.png") };
            var btnInsertTable = new ButtonMenuItem { Text = "Add New Property Table", Image = new Bitmap(bitmapprefix + "Grid_96px.png") };
            var btnInsertMasterTable = new ButtonMenuItem { Text = "Add New Master Property Table", Image = new Bitmap(bitmapprefix + "GridView_96px.png") };
            var btnInsertSpreadsheetTable = new ButtonMenuItem { Text = "Add New Linked Spreadsheet Table", Image = new Bitmap(bitmapprefix + "PivotTable_96px.png") };
            var btnInsertChartObject = new ButtonMenuItem { Text = "Add New Chart Object", Image = new Bitmap(bitmapprefix + "AreaChart_100px.png") };

            var btnSensAnalysis = new ButtonMenuItem { Text = "Sensitivity Analysis", Image = new Bitmap(bitmapprefix + "icons8-maintenance.png") };
            var btnOptimization = new ButtonMenuItem { Text = "Flowsheet Optimizer", Image = new Bitmap(bitmapprefix + "icons8-maintenance.png") };

            var btnInspector = new ButtonMenuItem { Text = "Solution Inspector", Image = new Bitmap(bitmapprefix + "icons8-spy_filled.png") };

            btnInspector.Click += (sender, e) => ActInspector.Invoke();

            btnObjects.Click += (sender, e) =>
            {
                var insform = new DWSIM.UI.Desktop.Editors.InsertObject { Flowsheet = FlowsheetObject, ObjList = ObjectList, FlowsheetHeight = FlowsheetControl.Height };
                insform.ShowModal(this);
                UpdateEditorConnectionsPanel();
            };

            btnSensAnalysis.Click += (sender, e) =>
            {
                var saeditor = new Desktop.Editors.SensAnalysisView(FlowsheetObject);
                var form = DWSIM.UI.Shared.Common.GetDefaultEditorForm("Sensitivity Analysis", (int)(sf * 860), (int)(sf * 600), saeditor);
                form.Show();
                form.Center();
            };

            btnOptimization.Click += (sender, e) =>
            {
                var foeditor = new Desktop.Editors.OptimizerView(FlowsheetObject);
                var form = DWSIM.UI.Shared.Common.GetDefaultEditorForm("Flowsheet Optimizer", (int)(sf * 800), (int)(sf * 650), foeditor);
                form.Show();
                form.Center();
            };

            Drawing.SkiaSharp.GraphicsSurface.BackgroundColor = SkiaSharp.SKColor.Parse(SystemColors.ControlBackground.ToHex());
            Drawing.SkiaSharp.GraphicsSurface.ForegroundColor = SkiaSharp.SKColor.Parse(SystemColors.ControlText.ToHex());

            FlowsheetControl.KeyDown += (sender, e) =>
            {
                if (e.Key == Keys.Delete) DeleteObject();
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
            btnInsertChartObject.Click += (sender, e) =>
            {
                FlowsheetControl.AddObject("Chart Object", 50, 50);
            };
            FlowsheetControl.MouseDoubleClick += (sender, e) =>
            {

                if (Application.Instance.Platform.IsMac) FlowsheetControl.FlowsheetSurface.InputRelease();
                var obj = FlowsheetControl.FlowsheetSurface.SelectedObject;
                if (obj == null)
                {
                    FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);

                    FlowsheetControl.FlowsheetSurface.ZoomAll((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                    FlowsheetControl.FlowsheetSurface.ZoomAll((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                    FlowsheetControl.FlowsheetSurface.Center((int)(FlowsheetControl.Width * GlobalSettings.Settings.DpiScale), (int)(FlowsheetControl.Height * GlobalSettings.Settings.DpiScale));
                    FlowsheetControl.Invalidate();
                }
                else
                {
                    obj.DoubleClickAction?.Invoke(obj);
                }
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
                    if (obj == null) return;
                    DebugObject();
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
                btnmSimultSolve.Checked = chkSimSolver.Checked;
            };

            btnmSimultSolve.Checked = FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
            btnmSimultSolve.CheckedChanged += (sender, e) =>
            {
                FlowsheetObject.Options.SimultaneousAdjustSolverEnabled = btnmSimultSolve.Checked;
                chkSimSolver.Checked = btnmSimultSolve.Checked;
            };

            ActSimultAdjustSolver = () =>
            {
                FlowsheetObject.Options.SimultaneousAdjustSolverEnabled = !FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
                chkSimSolver.Checked = FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
                btnmSimultSolve.Checked = FlowsheetObject.Options.SimultaneousAdjustSolverEnabled;
            };

            FlowsheetObject.ActSimultAdjustSolver = ActSimultAdjustSolver;

            chkDynamics = new CheckMenuItem { Text = "Enable/Disable Dynamic Mode" };
            chkDynamics.Checked = FlowsheetObject.DynamicMode;
            var btnDynManager = new ButtonMenuItem { Text = "Dynamics Manager", Image = new Bitmap(bitmapprefix + "icons8-module.png") };
            var btnDynIntegrator = new ButtonMenuItem { Text = "Integrator Controls", Image = new Bitmap(bitmapprefix + "icons8-ecg.png") };
            var btnDynPIDTuning = new ButtonMenuItem { Text = "PID Controller Tuning", Image = new Bitmap(bitmapprefix + "icons8-adjust.png") };

            btnDynPIDTuning.Click += (s, e) =>
            {
                var editor = new PIDTuningTool(FlowsheetObject, DynIntegratorControl);
                var form = UI.Shared.Common.GetDefaultEditorForm("PID Tuning Tool", 800, 600, editor, false);
                form.Show();
                form.Center();
            };

            // menu items

            Menu = new MenuBar();

            if (Application.Instance.Platform.IsMac)
            {
                var btnfile = (ButtonMenuItem)Menu.Items.Where((x) => x.Text == "&File").FirstOrDefault();
                btnfile.Items.AddRange(new[] { btnSave, btnSaveAs });
            }
            else if (Application.Instance.Platform.IsGtk)
            {
                Menu.Items.Add(new ButtonMenuItem { Text = "File", Items = { btnSave, btnSaveAs, btnClose } });
            }
            else if (Application.Instance.Platform.IsWinForms || Application.Instance.Platform.IsWpf)
            {
                Menu.ApplicationItems.AddRange(new[] { btnSave, btnSaveAs, btnClose });
            }

            var btnShowHideObjectPalette = new ButtonMenuItem { Text = "Show/Hide Object Palette" };

            var btnShowHideObjectEditorPanel = new ButtonMenuItem { Text = "Show/Hide Object Editor Panel" };

            var btnCloseAllEditors = new ButtonMenuItem { Text = "Close All Opened Object Editors" };

            var btnToggleWeatherPanel = new ButtonMenuItem { Text = "Toggle Weather Panel Visibility" };

            //process plugin list

            var pluginbuttons = new List<ButtonMenuItem>();

            var mform = (MainForm)Application.Instance.MainForm;

            if (mform != null)
            {
                foreach (Interfaces.IUtilityPlugin5 iplugin in mform.plugins)
                {
                    ButtonMenuItem tsmi = new ButtonMenuItem();
                    tsmi.Text = iplugin.Name;
                    tsmi.Tag = iplugin.UniqueID;
                    tsmi.Image = new Bitmap(bitmapprefix + "Electrical_96px.png");
                    tsmi.Click += (sender, e) =>
                    {
                        iplugin.SetFlowsheet(this.FlowsheetObject);
                        Application.Instance.Invoke(() =>
                        {
                            if (iplugin.UtilityForm is Form)
                            {
                                Form f = (Form)iplugin.UtilityForm;
                                f.Show();
                                f.Center();
                            }
                            else
                            {
                                System.Windows.Forms.Form f = (System.Windows.Forms.Form)iplugin.UtilityForm;
                                f.Show();
                            }
                        });
                    };
                    pluginbuttons.Add(tsmi);
                }
            }

            var pluginsmenu = new ButtonMenuItem { Text = "Plugins" };
            pluginsmenu.Items.AddRange(pluginbuttons);

            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Mac:
                    if (Application.Instance.Platform.IsMac)
                    {
                        Menu.Items.Insert(3, new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions, btnGlobalOptions } });
                        Menu.Items.Insert(4, new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable, btnInsertChartObject } });
                        Menu.Items.Insert(5, new ButtonMenuItem { Text = "Solver", Items = { btnSolve, btnSolveC, chkSimSolver } });
                        Menu.Items.Insert(6, new ButtonMenuItem { Text = "Dynamics", Items = { chkDynamics, btnDynManager, btnDynIntegrator, btnDynPIDTuning } });
                        Menu.Items.Insert(7, new ButtonMenuItem { Text = "Tools", Items = { btnSensAnalysis, btnOptimization, btnInspector } });
                        Menu.Items.Insert(8, new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities_TrueCriticalPoint, btnUtilities_PhaseEnvelope, btnUtilities_BinaryEnvelope } });
                        Menu.Items.Insert(9, pluginsmenu);
                        Menu.Items.Insert(10, new ButtonMenuItem { Text = "View", Items = { btnShowHideObjectPalette, btnShowHideObjectEditorPanel, btnCloseAllEditors, btnToggleWeatherPanel } });
                    }
                    else
                    {
                        Menu.Items.Add(new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions, btnGlobalOptions } });
                        Menu.Items.Add(new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable, btnInsertChartObject } });
                        Menu.Items.Add(new ButtonMenuItem { Text = "Solver", Items = { btnSolve, btnSolveC, chkSimSolver } });
                        Menu.Items.Add(new ButtonMenuItem { Text = "Dynamics", Items = { chkDynamics, btnDynManager, btnDynIntegrator, btnDynPIDTuning } });
                        Menu.Items.Add(new ButtonMenuItem { Text = "Tools", Items = { btnSensAnalysis, btnOptimization, btnInspector } });
                        Menu.Items.Add(new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities_TrueCriticalPoint, btnUtilities_PhaseEnvelope, btnUtilities_BinaryEnvelope } });
                        Menu.Items.Add(pluginsmenu);
                        Menu.Items.Add(new ButtonMenuItem { Text = "View", Items = { btnShowHideObjectPalette, btnShowHideObjectEditorPanel, btnCloseAllEditors, btnToggleWeatherPanel } });
                    }
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                case GlobalSettings.Settings.Platform.Windows:
                    Menu.Items.Add(new ButtonMenuItem { Text = "Setup", Items = { btnComps, btnBasis, btnOptions, btnGlobalOptions } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Objects", Items = { btnObjects, btnInsertText, btnInsertTable, btnInsertMasterTable, btnInsertSpreadsheetTable, btnInsertChartObject } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Solver", Items = { btnSolve, btnSolveC, chkSimSolver } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Dynamics", Items = { chkDynamics, btnDynManager, btnDynIntegrator, btnDynPIDTuning } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Tools", Items = { btnSensAnalysis, btnOptimization, btnInspector } });
                    Menu.Items.Add(new ButtonMenuItem { Text = "Utilities", Items = { btnUtilities_TrueCriticalPoint, btnUtilities_PhaseEnvelope, btnUtilities_BinaryEnvelope } });
                    Menu.Items.Add(pluginsmenu);
                    Menu.Items.Add(new ButtonMenuItem { Text = "View", Items = { btnShowHideObjectPalette, btnShowHideObjectEditorPanel, btnCloseAllEditors, btnToggleWeatherPanel } });
                    break;
            }

            var hitem1 = new ButtonMenuItem { Text = "Online Help", Image = new Bitmap(bitmapprefix + "help_browser.png") };
            hitem1.Click += (sender, e) =>
            {
                "https://dwsim.org/docs/crossplatform/help/".OpenURL();
            };

            var hitem2 = new ButtonMenuItem { Text = "Support".Localize(), Image = new Bitmap(bitmapprefix + "help_browser.png") };
            hitem2.Click += (sender, e) =>
            {
                "https://dwsim.org/wiki/index.php?title=Support".OpenURL();
            };

            var hitem3 = new ButtonMenuItem { Text = "Report a Bug".Localize(), Image = new Bitmap(bitmapprefix + "help_browser.png") };
            hitem3.Click += (sender, e) =>
            {
                "https://github.com/DanWBR/dwsim/issues".OpenURL();
            };

            var hitem4 = new ButtonMenuItem { Text = "Go to DWSIM's Website".Localize(), Image = new Bitmap(bitmapprefix + "help_browser.png") };
            hitem4.Click += (sender, e) =>
            {
                "https://dwsim.org".OpenURL();
            };

            Menu.HelpItems.Add(hitem1);
            Menu.HelpItems.Add(hitem4);
            Menu.HelpItems.Add(hitem2);
            Menu.HelpItems.Add(hitem3);

            ScriptListControl = new DWSIM.UI.Desktop.Editors.ScriptManager_Mac(FlowsheetObject);

            var Split1 = new Eto.Forms.Splitter { Orientation = Orientation.Horizontal, FixedPanel = SplitterFixedPanel.Panel1 };
            var Split2 = new Eto.Forms.Splitter { Orientation = Orientation.Vertical, FixedPanel = SplitterFixedPanel.Panel2 };
            var Split3 = new Eto.Forms.Splitter { Orientation = Orientation.Vertical, FixedPanel = SplitterFixedPanel.Panel2 };

            EditorHolder = new DocumentControl() { AllowReordering = true, DisplayArrows = false };

            var PanelEditors = new DocumentControl() { TabBarBackgroundColor = SystemColors.Highlight };
            PanelEditors.Pages.Add(new DocumentPage(EditorHolder) { Text = "Object Editors", Closable = false });

            Split1.Panel1 = PanelEditors;

            Split1.Panel1.Width = (int)(sf * 360);
            Split1.Panel1.Visible = true;

            btnShowHideObjectEditorPanel.Click += (sender, e) =>
            {
                Split1.Panel1.Visible = !Split1.Panel1.Visible;
            };

            btnCloseAllEditors.Click += (sender, e) =>
            {
                EditorHolder.Pages.Clear();
            };

            // obj containers

            var panelstreams = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelpressurechangers = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelseparators = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelmixers = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelexchangers = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelcolumns = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelreactors = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelsolids = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var paneluser = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panellogical = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelcontrollers = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelindicators = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelinputs = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelother = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };
            var panelrenew = new StackLayout() { Padding = new Padding(4), Orientation = Orientation.Horizontal, BackgroundColor = !s.DarkMode ? Colors.White : SystemColors.ControlBackground };


            var objcontainer = new DocumentControl { AllowReordering = true };
            objcontainer.Style = "drop";
            objcontainer.Pages.Add(new DocumentPage(panelstreams) { Closable = false, Text = "Streams" });
            objcontainer.Pages.Add(new DocumentPage(panelpressurechangers) { Closable = false, Text = "Pressure Changers" });
            objcontainer.Pages.Add(new DocumentPage(panelseparators) { Closable = false, Text = "Separators" });
            objcontainer.Pages.Add(new DocumentPage(panelmixers) { Closable = false, Text = "Mixers/Splitters" });
            objcontainer.Pages.Add(new DocumentPage(panelexchangers) { Closable = false, Text = "Exchangers" });
            objcontainer.Pages.Add(new DocumentPage(panelcolumns) { Closable = false, Text = "Columns" });
            objcontainer.Pages.Add(new DocumentPage(panelreactors) { Closable = false, Text = "Reactors" });
            objcontainer.Pages.Add(new DocumentPage(panelrenew) { Closable = false, Text = "Renewable Energies" });
            objcontainer.Pages.Add(new DocumentPage(panelsolids) { Closable = false, Text = "Solids" });
            objcontainer.Pages.Add(new DocumentPage(paneluser) { Closable = false, Text = "User Models" });
            objcontainer.Pages.Add(new DocumentPage(panellogical) { Closable = false, Text = "Logical Blocks" });
            objcontainer.Pages.Add(new DocumentPage(panelcontrollers) { Closable = false, Text = "Controllers" });
            objcontainer.Pages.Add(new DocumentPage(panelindicators) { Closable = false, Text = "Indicators" });
            objcontainer.Pages.Add(new DocumentPage(panelother) { Closable = false, Text = "Other" });

            var PanelObjects = new DocumentControl() { DisplayArrows = false, TabBarBackgroundColor = SystemColors.Highlight };
            PanelObjects.Pages.Add(new DocumentPage(objcontainer) { Text = "Object Palette", Closable = false });

            Split2.Panel2 = PanelObjects;
            Split2.Panel2.Height = 120 * (int)sf;

            foreach (var obj in ObjectList.Values.OrderBy(x => x.GetDisplayName()))
            {
                if ((Boolean)(obj.GetType().GetProperty("Visible").GetValue(obj)))
                {
                    var pitem = new FlowsheetObjectPanelItem();
                    if (!(Application.Instance.Platform.IsGtk && s.RunningPlatform() == s.Platform.Mac))
                    {
                        var bmp = (System.Drawing.Bitmap)obj.GetIconBitmap();
                        pitem.imgIcon.Image = new Bitmap(DWSIM.UI.Shared.Common.ImageToByte(bmp));
                    }
                    pitem.txtName.Text = obj.GetDisplayName();
                    pitem.MouseDown += (sender, e) =>
                    {
                        var dobj = new DataObject();
                        dobj.Image = pitem.imgIcon.Image;
                        dobj.SetString(obj.GetDisplayName(), "ObjectName");
                        pitem.DoDragDrop(dobj, DragEffects.All);
                        e.Handled = true;
                    };
                    switch (obj.ObjectClass)
                    {
                        case Interfaces.Enums.SimulationObjectClass.CAPEOPEN:
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Columns:
                            panelcolumns.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Exchangers:
                            panelexchangers.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Switches:
                        case Interfaces.Enums.SimulationObjectClass.Inputs:
                        case Interfaces.Enums.SimulationObjectClass.Logical:
                            panellogical.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.MixersSplitters:
                            panelmixers.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Other:
                            panelother.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.PressureChangers:
                            panelpressurechangers.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Reactors:
                            panelreactors.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Separators:
                            panelseparators.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Solids:
                            panelsolids.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Streams:
                            panelstreams.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.UserModels:
                            paneluser.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Controllers:
                            panelcontrollers.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.Indicators:
                            panelindicators.Items.Add(pitem);
                            break;
                        case Interfaces.Enums.SimulationObjectClass.CleanPowerSources:
                        case Interfaces.Enums.SimulationObjectClass.Electrolyzers:
                            panelrenew.Items.Add(pitem);
                            break;
                    }
                }
            }

            if (Application.Instance.Platform.IsWpf) FlowsheetControl.AllowDrop = true;
            FlowsheetControl.DragDrop += (sender, e) =>
            {
                if (e.Data.GetString("ObjectName") != null)
                {
                    var objname = e.Data.GetString("ObjectName");
                    var euo = FlowsheetObject.ExternalUnitOperations.Values.Where((x) => x.Name == objname).FirstOrDefault();
                    if (euo != null)
                    {
                        var item = ((Interfaces.ISimulationObject)euo);
                        var isobj = (Interfaces.ISimulationObject)item.CloneXML();
                        FlowsheetObject.AddObjectToSurface(Interfaces.Enums.GraphicObjects.ObjectType.External,
                            (int)(e.Location.X * s.DpiScale / FlowsheetControl.FlowsheetSurface.Zoom),
                            (int)(e.Location.Y * s.DpiScale / FlowsheetControl.FlowsheetSurface.Zoom),
                            "", "",
                            (Interfaces.IExternalUnitOperation)isobj);
                    }
                    else
                    {
                        FlowsheetObject.AddObject(e.Data.GetString("ObjectName"),
                            (int)(e.Location.X * GlobalSettings.Settings.DpiScale / FlowsheetControl.FlowsheetSurface.Zoom),
                            (int)(e.Location.Y * GlobalSettings.Settings.DpiScale / FlowsheetControl.FlowsheetSurface.Zoom), "", "", true);
                    }
                    UpdateEditorConnectionsPanel();
                    FlowsheetObject.UpdateInterface();
                }
            };

            var flowsheetcontrolcontainer = new TableLayout { Padding = new Padding(5), Spacing = new Size(5, 5) };

            var btnmZoomIn = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Zoom In", Image = new Bitmap(bitmapprefix + "icons8-zoom_in_filled.png").WithSize(16, 16) };
            var btnmZoomOut = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Zoom Out", Image = new Bitmap(bitmapprefix + "icons8-zoom_out_filled.png").WithSize(16, 16) };
            var btnmZoomFit = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Zoom to Fit", Image = new Bitmap(bitmapprefix + "icons8-zoom_to_extents.png").WithSize(16, 16) };

            btnmDrawGrid = new Eto.Forms.CheckBox { Text = "Draw Grid" };
            btnmSnapToGrid = new Eto.Forms.CheckBox { Text = "Snap to Grid" };

            btnmMultiSelect = new Eto.Forms.CheckBox { Text = "MultiSelect" };

            btnmDrawGrid.CheckedChanged += (sender, e) => ActDrawGrid.Invoke();
            btnmSnapToGrid.CheckedChanged += (sender, e) => ActSnapToGrid.Invoke();
            btnmMultiSelect.CheckedChanged += (sender, e) => ActMultiSelect.Invoke();

            var btnmZoomDefault = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Default Zoom", Image = new Bitmap(bitmapprefix + "icons8-zoom_to_actual_size_filled.png").WithSize(16, 16) };
            var btnmAlignLefts = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Lefts", Image = new Bitmap(bitmapprefix + "shape_align_left.png").WithSize(16, 16) };
            var btnmAlignCenters = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Centers", Image = new Bitmap(bitmapprefix + "shape_align_center.png").WithSize(16, 16) };
            var btnmAlignRights = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Rights", Image = new Bitmap(bitmapprefix + "shape_align_right.png").WithSize(16, 16) };
            var btnmAlignTops = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Tops", Image = new Bitmap(bitmapprefix + "shape_align_top.png").WithSize(16, 16) };
            var btnmAlignMiddles = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Middles", Image = new Bitmap(bitmapprefix + "shape_align_middle.png").WithSize(16, 16) };
            var btnmAlignBottoms = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Align Bottoms", Image = new Bitmap(bitmapprefix + "shape_align_bottom.png").WithSize(16, 16) };
            var btnmEqHoriz = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Equalize Horizontally", Image = new Bitmap(bitmapprefix + "shape_align_middle1.png").WithSize(16, 16) };
            var btnmEqVert = new Eto.Forms.Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, Tag = "Equalize Vertically", Image = new Bitmap(bitmapprefix + "shape_align_center1.png").WithSize(16, 16) };

            ActAlignLefts = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignLefts)); };
            ActAlignCenters = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignCenters)); };
            ActAlignRights = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignRights)); };
            ActAlignTops = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignTops)); };
            ActAlignMiddles = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignMiddles)); };
            ActAlignBottoms = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignBottoms)); };
            ActVertAlign = () => { Application.Instance.Invoke(() => AlignObjects(btnmEqVert)); };
            ActHorizAlign = () => { Application.Instance.Invoke(() => AlignObjects(btnmEqHoriz)); };

            ActAlignLefts = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignLefts)); };
            ActAlignCenters = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignCenters)); };
            ActAlignRights = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignRights)); };
            ActAlignTops = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignTops)); };
            ActAlignMiddles = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignMiddles)); };
            ActAlignBottoms = () => { Application.Instance.Invoke(() => AlignObjects(btnmAlignBottoms)); };
            ActVertAlign = () => { Application.Instance.Invoke(() => AlignObjects(btnmEqVert)); };
            ActHorizAlign = () => { Application.Instance.Invoke(() => AlignObjects(btnmEqHoriz)); };

            btnmZoomOut.Click += (sender, e) => ActZoomOut.Invoke();
            btnmZoomIn.Click += (sender, e) => ActZoomIn.Invoke();
            btnmZoomFit.Click += (sender, e) => ActZoomFit.Invoke();
            btnmZoomDefault.Click += (sender, e) => ActZoomDefault.Invoke();

            if (Application.Instance.Platform.IsGtk)
            {
                btnmDrawGrid.CheckedChanged += (sender, e) => ActDrawGrid.Invoke();
                btnmSnapToGrid.CheckedChanged += (sender, e) => ActSnapToGrid.Invoke();
                btnmMultiSelect.CheckedChanged += (sender, e) => ActMultiSelect.Invoke();
            }

            btnmAlignBottoms.Click += (sender, e) => ActAlignBottoms.Invoke();
            btnmAlignCenters.Click += (sender, e) => ActAlignCenters.Invoke();
            btnmAlignTops.Click += (sender, e) => ActAlignTops.Invoke();
            btnmAlignLefts.Click += (sender, e) => ActAlignLefts.Invoke();
            btnmAlignMiddles.Click += (sender, e) => ActAlignMiddles.Invoke();
            btnmAlignRights.Click += (sender, e) => ActAlignRights.Invoke();
            btnmEqHoriz.Click += (sender, e) => ActHorizAlign.Invoke();
            btnmEqVert.Click += (sender, e) => ActVertAlign.Invoke();

            var chkControlPanelMode = new Eto.Forms.CheckBox { Text = "Control Panel Mode", ToolTip = "Enable/Disable Control Panel Mode" };

            ddstates = new DropDown { Width = 100 };
            var btnSaveState = new Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Save State", Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-scroll_up.png").WithSize(16, 16)) };
            var btnLoadState = new Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Load State", Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-scroll_down.png").WithSize(16, 16)) };
            var btnDeleteState = new Button { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Delete Selected State", Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-cancel.png").WithSize(16, 16)) };

            btnDeleteState.Click += (s, e) =>
            {
                if (ddstates.SelectedValue != null)
                {
                    if (FlowsheetObject.StoredSolutions.ContainsKey(ddstates.SelectedValue.ToString()))
                    {
                        try
                        {
                            if (MessageBox.Show("Confirm?", MessageBoxButtons.YesNo, MessageBoxType.Question) == DialogResult.Yes)
                            {
                                FlowsheetObject.StoredSolutions.Remove(ddstates.SelectedValue.ToString());
                                ddstates.Items.RemoveAt(ddstates.SelectedIndex);
                                DynManagerControl.UpdateSelectedSchedule();
                                MessageBox.Show("State Deleted successfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxType.Information);
                            }
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show("Error: " + ex.Message, "DWSIM", MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                    }
                }
            };

            btnSaveState.Click += (s, e) =>
            {
                string sname = "";
                Dialog formName = null;
                var tb = new TextBox { Text = "NewState" };
                formName = UI.Shared.Common.CreateDialogWithButtons(tb, "Enter a Name", () => sname = tb.Text);
                formName.Location = new Point(Mouse.Position);
                formName.ShowModal(this);
                if (sname != "" && !FlowsheetObject.StoredSolutions.ContainsKey(sname))
                {
                    FlowsheetObject.StoredSolutions.Add(sname, FlowsheetObject.GetProcessData());
                    ddstates.Items.Add(sname);
                    DynManagerControl.UpdateSelectedSchedule();
                    MessageBox.Show("State Saved successfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxType.Information);
                }
                else
                {
                    MessageBox.Show("Invalid name.", "DWSIM", MessageBoxButtons.OK, MessageBoxType.Error);
                }
            };

            btnLoadState.Click += (s, e) =>
            {
                if (ddstates.SelectedValue != null)
                {
                    if (FlowsheetObject.StoredSolutions.ContainsKey(ddstates.SelectedValue.ToString()))
                    {
                        try
                        {
                            FlowsheetObject.LoadProcessData(FlowsheetObject.StoredSolutions[ddstates.SelectedValue.ToString()]);
                            FlowsheetObject.UpdateInterface();
                            FlowsheetObject.UpdateEditorPanels?.Invoke();
                            MessageBox.Show("State Restored successfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxType.Information);
                        }
                        catch (Exception ex)
                        {
                            MessageBox.Show("Error: " + ex.Message, "DWSIM", MessageBoxButtons.OK, MessageBoxType.Error);
                        }
                    }
                }
            };

            var lblSetFontSize = new Label { Text = "Object Labels Font Size" };
            var tbFontSize = new TextBox { Width = 40, Text = FlowsheetObject.Options.LabelFontSize.ToString("G2") };
            var btnSetFont = new Button { Width = 40, Text = "Set" };

            if (Application.Instance.Platform.IsGtk) btnSetFont.Size = new Size(40, 30);

            btnSetFont.Click += (s, e) =>
            {
                FlowsheetObject.Options.LabelFontSize = Double.Parse(tbFontSize.Text);
                foreach (var obj in FlowsheetControl.FlowsheetSurface.DrawingObjects)
                {
                    if (obj is ShapeGraphic)
                    {
                        ((ShapeGraphic)obj).FontSize = FlowsheetObject.Options.LabelFontSize;
                    }
                }
                FlowsheetControl.Invalidate();
            };

            var lblColorTheme = new Label { Text = "Flowsheet Color Theme" };
            var cbColorTheme = new DropDown { Width = 140, Items = { "Default", "Black-and-White PFD", "Color Icons" }, SelectedIndex = FlowsheetObject.Options.FlowsheetColorTheme };
            cbColorTheme.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.FlowsheetColorTheme = cbColorTheme.SelectedIndex;
                FlowsheetControl.Invalidate();
            };

            var lblRegularFont = new Label { Text = "Regular Font" };
            var lblBoldFont = new Label { Text = "Bold Font" };
            var lblItalicFont = new Label { Text = "Italic Font" };
            var lblBoldItalicFont = new Label { Text = "Bold/Italic Font" };

            var cbRegularFont = new DropDown { Width = 170 };
            cbRegularFont.Items.AddRange(DWSIM.Drawing.SkiaSharp.GraphicsSurface.RegularFonts.Select(f => new ListItem { Text = f, Key = f }));
            cbRegularFont.SelectedKey = FlowsheetObject.Options.RegularFontName;

            var cbBoldFont = new DropDown { Width = 170 };
            cbBoldFont.Items.AddRange(DWSIM.Drawing.SkiaSharp.GraphicsSurface.BoldFonts.Select(f => new ListItem { Text = f, Key = f }));
            cbBoldFont.SelectedKey = FlowsheetObject.Options.BoldFontName;

            var cbItalicFont = new DropDown { Width = 170 };
            cbItalicFont.Items.AddRange(DWSIM.Drawing.SkiaSharp.GraphicsSurface.ItalicFonts.Select(f => new ListItem { Text = f, Key = f }));
            cbItalicFont.SelectedKey = FlowsheetObject.Options.ItalicFontName;

            var cbBoldItalicFont = new DropDown { Width = 170 };
            cbBoldItalicFont.Items.AddRange(DWSIM.Drawing.SkiaSharp.GraphicsSurface.BoldItalicFonts.Select(f => new ListItem { Text = f, Key = f }));
            cbBoldItalicFont.SelectedKey = FlowsheetObject.Options.BoldItalicFontName;

            cbRegularFont.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.RegularFontName = cbRegularFont.SelectedKey;
                FlowsheetControl.FlowsheetSurface.SetRegularFont(FlowsheetObject.Options.RegularFontName);
                FlowsheetControl.Invalidate();
            };

            cbBoldFont.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.BoldFontName = cbBoldFont.SelectedKey;
                FlowsheetControl.FlowsheetSurface.SetBoldFont(FlowsheetObject.Options.BoldFontName);
                FlowsheetControl.Invalidate();
            };

            cbItalicFont.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.ItalicFontName = cbItalicFont.SelectedKey;
                FlowsheetControl.FlowsheetSurface.SetItalicFont(FlowsheetObject.Options.ItalicFontName);
                FlowsheetControl.Invalidate();
            };

            cbBoldItalicFont.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.BoldItalicFontName = cbBoldItalicFont.SelectedKey;
                FlowsheetControl.FlowsheetSurface.SetBoldItalicFont(FlowsheetObject.Options.BoldItalicFontName);
                FlowsheetControl.Invalidate();
            };

            if (Application.Instance.Platform.IsGtk)
            {

                ddstates.Height = 26;

                btnSaveState.Size = new Size(30, 30);
                btnLoadState.Size = new Size(30, 30);
                btnDeleteState.Size = new Size(30, 30);
                btnmZoomOut.Size = new Size(30, 30);
                btnmZoomIn.Size = new Size(30, 30);
                btnmZoomFit.Size = new Size(30, 30);
                btnmZoomDefault.Size = new Size(30, 30);

                btnmAlignBottoms.Size = new Size(30, 30);
                btnmAlignCenters.Size = new Size(30, 30);
                btnmAlignTops.Size = new Size(30, 30);
                btnmAlignLefts.Size = new Size(30, 30);
                btnmAlignMiddles.Size = new Size(30, 30);
                btnmAlignRights.Size = new Size(30, 30);
                btnmEqHoriz.Size = new Size(30, 30);
                btnmEqVert.Size = new Size(30, 30);

            }

            var lbl1 = new Label { Text = "Search" };

            var tbSearch = new TextBox { Width = 100 };

            tbSearch.TextChanged += (s, e) =>
            {

                if (tbSearch.Text == "") return;

                var obj = FlowsheetObject.GetFlowsheetSimulationObject(tbSearch.Text);
                if (obj != null)
                {
                    try
                    {
                        Point center = new Point((FlowsheetControl.Width / 2), (FlowsheetControl.Height / 2));
                        FlowsheetControl.FlowsheetSurface.OffsetAll((center.X / FlowsheetControl.FlowsheetSurface.Zoom)
                                        - obj.GraphicObject.X, (center.Y / FlowsheetControl.FlowsheetSurface.Zoom)
                                        - obj.GraphicObject.Y);
                        FlowsheetControl.Invalidate();
                        FlowsheetControl.Invalidate();
                    }
                    catch
                    {
                    }

                }

            };

            var lblAutoConnect = new Label { Text = "Auto-Connect Added Objects" };
            var cbAutoConnect = new DropDown { Width = 100, Items = { "No", "Yes", "Smart" }, SelectedIndex = FlowsheetObject.Options.AddObjectsWithStreams };
            cbAutoConnect.SelectedIndexChanged += (s, e) =>
            {
                FlowsheetObject.Options.AddObjectsWithStreams = cbAutoConnect.SelectedIndex;
                FlowsheetControl.Invalidate();
            };

            var menu1 = new StackLayout
            {
                Items = { lbl1, tbSearch, new Label {Text =" " }, lblAutoConnect, cbAutoConnect, new Label {Text =" " }, chkControlPanelMode,  new Label {Text =" " },
                new Label{Text = "States"}, ddstates, btnSaveState, btnLoadState, btnDeleteState,new Label {Text =" " },
                btnmZoomOut, btnmZoomIn, btnmZoomFit, btnmZoomDefault, new Label {Text =" " },
                btnmDrawGrid, btnmSnapToGrid, btnmMultiSelect, new Label {Text =" " },
                btnmAlignBottoms, btnmAlignCenters, btnmAlignTops, btnmAlignLefts, btnmAlignMiddles, btnmAlignRights, new Label {Text =" " },
                btnmEqHoriz, btnmEqVert,  new Label {Text =" " }, lblSetFontSize, tbFontSize, btnSetFont, lblColorTheme, cbColorTheme,
                lblRegularFont, cbRegularFont, lblBoldFont, cbBoldFont, lblItalicFont, cbItalicFont, lblBoldItalicFont, cbBoldItalicFont
                },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Center,
                Padding = new Padding(5)
            };
            flowsheetcontrolcontainer.Rows.Add(new TableRow(new Scrollable { Border = BorderType.None, Content = menu1 }));

            Button btnUp, btnLeft, btnRight, btnDown;

            btnUp = new Button { Text = "^", Height = 24, Visible = false };
            btnLeft = new Button { Text = "<", Width = 24, Visible = false };
            btnRight = new Button { Text = ">", Width = 24, Visible = false };
            btnDown = new Button { Text = "v", Height = 24, Visible = false };

            flowsheetcontrolcontainer.Rows.Add(new TableRow(btnUp));

            var tabfl1 = new TableLayout();
            var tabfl1r = new TableRow();
            tabfl1r.Cells.Add(new TableCell(btnLeft));
            tabfl1r.Cells.Add(new TableCell(FlowsheetControl, true));
            tabfl1r.Cells.Add(new TableCell(btnRight));
            tabfl1.Rows.Add(tabfl1r);

            flowsheetcontrolcontainer.Rows.Add(new TableRow(tabfl1) { ScaleHeight = true });

            flowsheetcontrolcontainer.Rows.Add(new TableRow(btnDown));

            // weather bar

            var imgsun = new ImageView { ToolTip = "Solar Irradiation", Image = new Bitmap(bitmapprefix + "icons8-sun_with_face.png") };
            var imgtemp = new ImageView { ToolTip = "Ambient Temperature", Image = new Bitmap(bitmapprefix + "icons8-temperature.png") };
            var imgpgauge = new ImageView { ToolTip = "Atmospheric Pressure", Image = new Bitmap(bitmapprefix + "icons8-pressure_gauge.png") };
            var imghum = new ImageView { ToolTip = "Relative Humidity", Image = new Bitmap(bitmapprefix + "icons8-humidity.png") };
            var imgwind = new ImageView { ToolTip = "Wind Speed", Image = new Bitmap(bitmapprefix + "icons8-wind.png") };

            var wobj = FlowsheetObject.FlowsheetOptions.CurrentWeather;
            var nf = FlowsheetObject.FlowsheetOptions.NumberFormat;

            var tbirr = new TextBox { ToolTip = "Solar Irradiation", Width = 75, TextAlignment = TextAlignment.Right, Text = wobj.SolarIrradiation_kWh_m2.ToString(nf) };
            var tbtemp = new TextBox { ToolTip = "Ambient Temperature", Width = 75, TextAlignment = TextAlignment.Right, Text = wobj.Temperature_C.ToString(nf) };
            var tbhum = new TextBox { ToolTip = "Relative Humidity", Width = 75, TextAlignment = TextAlignment.Right, Text = wobj.RelativeHumidity_pct.ToString(nf) };
            var tbwind = new TextBox { ToolTip = "Wind Speed", Width = 75, TextAlignment = TextAlignment.Right, Text = wobj.WindSpeed_km_h.ToString(nf) };
            var tbpgauge = new TextBox { ToolTip = "Atmospheric Pressure", Width = 75, TextAlignment = TextAlignment.Right, Text = (wobj.AtmosphericPressure_Pa / 100.0).ToString(nf) };

            tbirr.TextChanged += (tb, e) => { if (tbirr.Text.IsValidDouble()) wobj.SolarIrradiation_kWh_m2 = tbirr.Text.ToDoubleFromInvariant(); };
            tbtemp.TextChanged += (tb, e) => { if (tbtemp.Text.IsValidDouble()) wobj.Temperature_C = tbtemp.Text.ToDoubleFromInvariant(); };
            tbhum.TextChanged += (tb, e) => { if (tbhum.Text.IsValidDouble()) wobj.RelativeHumidity_pct = tbhum.Text.ToDoubleFromInvariant(); };
            tbwind.TextChanged += (tb, e) => { if (tbwind.Text.IsValidDouble()) wobj.WindSpeed_km_h = tbwind.Text.ToDoubleFromInvariant(); };
            tbpgauge.TextChanged += (tb, e) => { if (tbpgauge.Text.IsValidDouble()) wobj.AtmosphericPressure_Pa = tbpgauge.Text.ToDoubleFromInvariant() * 100.0; };

            var lblirr = new Label { Text = "kW/m2" };
            var lbltemp = new Label { Text = "C" };
            var lblwind = new Label { Text = "km/h" };
            var lblhum = new Label { Text = "%" };
            var lblpgauge = new Label { Text = "hPa" };

            var lblweather = new Label { Text = "Current Weather Conditions", Enabled = false };

            var weatherpanel = new StackLayout
            {
                Orientation = Orientation.Horizontal,
                Items = { lblweather, imgtemp, tbtemp, lbltemp, imgpgauge, tbpgauge, lblpgauge, imgwind, tbwind, lblwind,
                    imghum, tbhum, lblhum, imgsun, tbirr, lblirr },
                VerticalContentAlignment = VerticalAlignment.Center,
                Spacing = 4,
                Visible = false
            };

            btnToggleWeatherPanel.Click += (s, e) =>
            {
                weatherpanel.Visible = !weatherpanel.Visible;
            };

            flowsheetcontrolcontainer.Rows.Add(new TableRow(new Scrollable { Border = BorderType.None, Content = weatherpanel }));

            Split2.Panel1 = flowsheetcontrolcontainer;

            SplitterFlowsheet = Split2;

            btnLeft.Click += (s, e) =>
            {
                Size sz = SplitterFlowsheet.Panel1.Size;
                var z = this.FlowsheetControl.FlowsheetSurface.Zoom;
                var offs = (double)sz.Width / (double)z * sf;
                this.FlowsheetControl.FlowsheetSurface.OffsetAll((int)offs, 0);
                this.FlowsheetControl.Invalidate();
            };

            btnRight.Click += (s, e) =>
            {
                Size sz = SplitterFlowsheet.Panel1.Size;
                var z = this.FlowsheetControl.FlowsheetSurface.Zoom;
                var offs = -(double)sz.Width / (double)z * sf;
                this.FlowsheetControl.FlowsheetSurface.OffsetAll((int)offs, 0);
                this.FlowsheetControl.Invalidate();
            };

            btnUp.Click += (s, e) =>
            {
                Size sz = SplitterFlowsheet.Panel1.Size;
                var z = this.FlowsheetControl.FlowsheetSurface.Zoom;
                var offs = (double)sz.Height / (double)z * sf;
                this.FlowsheetControl.FlowsheetSurface.OffsetAll(0, (int)offs);
                this.FlowsheetControl.Invalidate();
            };

            btnDown.Click += (s, e) =>
            {
                Size sz = SplitterFlowsheet.Panel1.Size;
                var z = this.FlowsheetControl.FlowsheetSurface.Zoom;
                var offs = (double)sz.Height / (double)z * sf;
                this.FlowsheetControl.FlowsheetSurface.OffsetAll(0, -(int)offs);
                this.FlowsheetControl.Invalidate();
            };

            chkControlPanelMode.CheckedChanged += (s, e) =>
            {
                if (chkControlPanelMode.Checked.GetValueOrDefault())
                {
                    FlowsheetControl.FlowsheetSurface.ControlPanelMode = true;
                    GlobalSettings.Settings.DarkMode = true;
                    Drawing.SkiaSharp.GraphicsSurface.BackgroundColor = SKColors.DimGray;
                    Drawing.SkiaSharp.GraphicsSurface.ForegroundColor = SKColors.WhiteSmoke;
                    btnLeft.Visible = true;
                    btnUp.Visible = true;
                    btnDown.Visible = true;
                    btnRight.Visible = true;
                }
                else
                {
                    FlowsheetControl.FlowsheetSurface.ControlPanelMode = false;
                    GlobalSettings.Settings.DarkMode = false;
                    Drawing.SkiaSharp.GraphicsSurface.BackgroundColor = SKColors.White;
                    Drawing.SkiaSharp.GraphicsSurface.ForegroundColor = SKColors.Black;
                    btnLeft.Visible = false;
                    btnUp.Visible = false;
                    btnDown.Visible = false;
                    btnRight.Visible = false;
                }
                FlowsheetControl.Invalidate();
                Split2.Invalidate();
            };

            Split3.Panel2 = SetupLogWindow();
            Split3.Panel2.Height = (int)(sf * 120);

            var documentcontainer = (DocumentControl)Split3.Panel2;

            documentcontainer.Pages.Add(new DocumentPage(DynIntegratorControl) { Text = "Integrator Controls", Closable = false });

            btnDynIntegrator.Click += (s, e) =>
            {
                documentcontainer.SelectedIndex = 1;
            };

            btnmDynIntegrator.Click += (s, e) =>
            {
                documentcontainer.SelectedIndex = 1;
            };

            Split1.Panel2 = Split3;

            btnShowHideObjectPalette.Click += (sender, e) =>
            {
                Split2.Panel2.Visible = !Split2.Panel2.Visible;
            };

            DocumentPageSpreadsheet = new DocumentPage { Content = SpreadsheetControl, Text = "Spreadsheet", Closable = false };

            DocumentContainer = new DocumentControl() { AllowReordering = false, DisplayArrows = false };
            DocumentContainer.Pages.Add(new DocumentPage { Content = Split2, Text = "Flowsheet", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Content = MaterialStreamListControl, Text = "Material Streams", Closable = false });
            DocumentContainer.Pages.Add(DocumentPageSpreadsheet);
            DocumentContainer.Pages.Add(new DocumentPage { Content = ChartsControl, Text = "Charts", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Content = FileExplControl, Text = "Files", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Content = ScriptListControl, Text = "Script Manager", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Content = DynManagerControl, Text = "Dynamics Manager", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Content = ResultsControl, Text = "Results", Closable = false });

            DocumentContainer.SelectedIndexChanged += (s, e) =>
            {
                if (DocumentContainer.SelectedIndex == 4)
                {
                    FileExplControl.ListFiles();
                    FileExplControl.UpdateSize();
                }
            };

            Split3.Panel1 = DocumentContainer;

            btnDynManager.Click += (s, e) =>
            {
                DocumentContainer.SelectedIndex = 6;
            };

            btnmDynManager.Click += (s, e) =>
            {
                DocumentContainer.SelectedIndex = 6;
            };

            chkmDynamics.CheckedChanged += (s, e) =>
            {
                FlowsheetObject.DynamicMode = chkmDynamics.Checked;
                chkDynamics.Checked = FlowsheetObject.DynamicMode;
                DynManagerControl.chkDynamics.Checked = FlowsheetObject.DynamicMode;
                UpdateEditorPanels();
                FlowsheetObject.UpdateInterface();
            };

            chkDynamics.CheckedChanged += (s, e) =>
            {
                FlowsheetObject.DynamicMode = chkDynamics.Checked;
                chkmDynamics.Checked = FlowsheetObject.DynamicMode;
                DynManagerControl.chkDynamics.Checked = FlowsheetObject.DynamicMode;
                UpdateEditorPanels();
                FlowsheetObject.UpdateInterface();
            };

            // status bar

            var imgheart = new ImageView { Image = new Bitmap(bitmapprefix + "heart.png") };
            var lbldonate = new Label { Text = "Support continuous development and maintenance of DWSIM for as low as 3 USD/month or with a one-time donation." };

            var btnSingleDonation = new Button { Text = "One-Time Donation", ImagePosition = ButtonImagePosition.Left, Height = 24, Image = new Bitmap(Bitmap.FromResource(imgprefix + "coffee.png").WithSize(16, 16)) };
            var btnMonthlyDonation = new Button { Text = "Monthly Donation", ImagePosition = ButtonImagePosition.Left, Height = 24, Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-patreon.png").WithSize(16, 16)) };

            if (s.RunningPlatform() == s.Platform.Linux)
            {
                lbldonate.Text = "Support DWSIM development and maintenance.";
                btnSingleDonation.Width = (int)(140 * sf);
                btnMonthlyDonation.Width = (int)(140 * sf);
            }

            btnSingleDonation.Click += (s, e) =>
            {
                "https://www.buymeacoffee.com/dwsim".OpenURL();
            };

            btnMonthlyDonation.Click += (s, e) =>
            {
                "https://www.patreon.com/dwsim".OpenURL();
            };

            var statuspanel = new StackLayout
            {
                Orientation = Orientation.Horizontal,
                Items = { imgheart, lbldonate, btnSingleDonation, btnMonthlyDonation },
                VerticalContentAlignment = VerticalAlignment.Center,
                Spacing = 4,
                Visible = true
            };

            var Split0 = new Eto.Forms.Splitter { Orientation = Orientation.Vertical, FixedPanel = SplitterFixedPanel.Panel2 };
            Split0.Panel1 = Split1;

            Split0.Panel2 = statuspanel;
            Split0.Panel2.Height = (int)(sf * 32);

            // main container

            Content = Split0;

            // context menus

            selctxmenu = new ContextMenu();
            deselctxmenu = new ContextMenu();

            // flowsheet mouse up

            FlowsheetControl.FlowsheetSurface.InputReleased += (sender, e) =>
            {
                if (GlobalSettings.Settings.EditOnSelect)
                {
                    var sobj = FlowsheetObject.GetSelectedFlowsheetSimulationObject("");
                    if (sobj != null)
                    {
                        try
                        {
                            EditObject_New(sobj);
                        }
                        catch (Exception ex)
                        {
                            Logging.Logger.LogError("Object Editor Error", ex);
                            Console.WriteLine(ex.ToString());
                        }
                    }
                }
            };

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
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Table:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_MasterTable:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable:
                                selctxmenu.Items.Clear();
                                var itemtype = new ButtonMenuItem { Text = "Data Table", Enabled = false };
                                selctxmenu.Items.Add(itemtype);

                                var menuitem0 = new ButtonMenuItem { Text = "Edit", Image = new Bitmap(bitmapprefix + "EditProperty_96px.png") };
                                menuitem0.Click += (sender2, e2) =>
                                {
                                    EditSelectedObjectProperties();
                                };

                                selctxmenu.Items.Add(menuitem0);

                                var item7 = new ButtonMenuItem { Text = "Copy Data to Clipboard", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };

                                item7.Click += (sender2, e2) =>
                                {
                                    new Clipboard().Text = obj.GetType().GetProperty("ClipboardData").GetValue(obj).ToString();
                                };

                                selctxmenu.Items.Add(item7);

                                var delitem = new ButtonMenuItem { Text = "Delete", Image = new Bitmap(bitmapprefix + "Delete_96px.png") };
                                delitem.Click += (sender2, e2) =>
                                {
                                    if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                    {
                                        FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj, false, false);
                                    }
                                };
                                selctxmenu.Items.Add(delitem);
                                break;
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Text:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_HTMLText:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Button:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Image:
                            case Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart:
                                selctxmenu.Items.Clear();
                                var itemtype2 = new ButtonMenuItem { Text = "Misc Object", Enabled = false };
                                selctxmenu.Items.Add(itemtype2);

                                var menuitem02 = new ButtonMenuItem { Text = "Edit", Image = new Bitmap(bitmapprefix + "EditProperty_96px.png") };
                                menuitem02.Click += (sender2, e2) =>
                                {
                                    EditSelectedObjectProperties();
                                };

                                selctxmenu.Items.Add(menuitem02);

                                var item7a = new ButtonMenuItem { Text = "Copy Data to Clipboard", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };

                                item7a.Click += (sender2, e2) =>
                                {
                                    new Clipboard().Text = obj.GetType().GetProperty("ClipboardData").GetValue(obj).ToString();
                                };

                                selctxmenu.Items.Add(item7a);

                                var delitem2 = new ButtonMenuItem { Text = "Delete", Image = new Bitmap(bitmapprefix + "Delete_96px.png") };
                                delitem2.Click += (sender2, e2) =>
                                {
                                    if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                                    {
                                        FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj, false, false);
                                    }
                                };
                                selctxmenu.Items.Add(delitem2);
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

            // additional events

            Closing += Flowsheet_Closing;

            Closed += (sender, e) =>
            {
                SaveUserUnits();
                FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationClosed, Interfaces.Enums.Scripts.ObjectType.Simulation, "");
            };

            Shown += Flowsheet_Shown;

            FlowsheetObject.HighLevelSolve = () => SolveFlowsheet(false);

            DynIntegratorControl.Init();

            DynManagerControl.Init();

            FileExplControl.Init();

        }

        public void SolveFlowsheet(bool changecalcorder)
        {
            if (!FlowsheetObject.DynamicMode)
            {
                FlowsheetObject.UpdateSpreadsheet(() =>
                {
                    Spreadsheet.EvaluateAll();
                    Spreadsheet.WriteAll();
                });
                FlowsheetObject.FinishedSolving = (() =>
                {
                    Application.Instance.AsyncInvoke(() =>
                    {
                        ResultsControl.UpdateList();
                        //MaterialStreamListControl.UpdateList();
                        UpdateEditorPanels();
                    });
                });
                FlowsheetObject.SolveFlowsheet(false, null, changecalcorder);
                FlowsheetObject.UpdateSpreadsheet(() =>
                {
                    Spreadsheet.EvaluateAll();
                });
            }
            else
            {
                FlowsheetObject.ShowMessage("Dynamic Mode is enabled.", Interfaces.IFlowsheet.MessageType.Warning);
            }
        }

        void Flowsheet_Shown(object sender, EventArgs e)
        {

            btnmDrawGrid.Checked = FlowsheetObject.Options.FlowsheetDisplayGrid;
            btnmSnapToGrid.Checked = FlowsheetObject.Options.FlowsheetSnapToGrid;
            btnmMultiSelect.Checked = FlowsheetObject.Options.FlowsheetMultiSelectMode;

            var surface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface();
            surface.ZoomAll((int)(FlowsheetControl.Width * s.DpiScale), (int)(FlowsheetControl.Height * s.DpiScale));
            surface.ZoomAll((int)(FlowsheetControl.Width * s.DpiScale), (int)(FlowsheetControl.Height * s.DpiScale));
            surface.Zoom *= 0.7f;
            surface.Center((int)(FlowsheetControl.Width * s.DpiScale), (int)(FlowsheetControl.Height * s.DpiScale));

            ddstates.Items.Clear();
            ddstates.Items.Add("");
            foreach (var item in FlowsheetObject.StoredSolutions)
            {
                ddstates.Items.Add(item.Key);
            }

            ScriptListControl.UpdateList();

            DynIntegratorControl.Populate();

            DynManagerControl.Populate();

            DynIntegratorControl.btnViewResults.Click += (s2, e2) =>
            {
                DocumentContainer.SelectedIndex = 2;
            };

            DynManagerControl.chkDynamics.CheckedChanged += (s2, e2) =>
            {
                FlowsheetObject.DynamicMode = DynManagerControl.chkDynamics.Checked.GetValueOrDefault();
                chkmDynamics.Checked = FlowsheetObject.DynamicMode;
                chkDynamics.Checked = FlowsheetObject.DynamicMode;
                UpdateEditorPanels();
                FlowsheetObject.UpdateInterface();
            };

            if (Application.Instance.Platform.IsWpf)
            {
                DocumentContainer.SelectedIndexChanged += (sender2, e2) =>
                {
                    if (TabSwitch)
                    {
                        Task.Delay(100).ContinueWith((t) =>
                        {
                            TabSwitch = false;
                            Application.Instance.Invoke(() =>
                            {
                                DocumentContainer.SelectedIndex = 0;
                                this.Enabled = true;
                            });
                        });
                    }
                };

                //this.Enabled = false;
                DocumentContainer.SelectedPage = DocumentPageSpreadsheet;

            }

            if (newsim)
            {
                var sswiz = new SimulationSetupWizard(this.FlowsheetObject);
                sswiz.DisplayPage0(this);
            }

        }

        void Flowsheet_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (MessageBox.Show(this, "ConfirmFlowsheetExit".Localize(), "FlowsheetExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.No)
            {
                e.Cancel = true;
            }
        }

        public void SaveSimulation(string path, bool backup = false)
        {


            if (System.IO.Path.GetExtension(path).ToLower() == ".dwxmz")
            {
                if (!s.AutomationMode) Application.Instance.Invoke(() => ScriptListControl.UpdateScripts());

                path = Path.ChangeExtension(path, ".dwxmz");

                string xmlfile = Path.ChangeExtension(DWSIM.SharedClasses.Utility.GetTempFileName(), "xml");

                FlowsheetObject.SaveToXML().Save(xmlfile);

                var dbfile = Path.ChangeExtension(xmlfile, "db");

                FlowsheetObject.FileDatabaseProvider.ExportDatabase(dbfile);

                var i_Files = new List<string>();
                if (File.Exists(xmlfile))
                    i_Files.Add(xmlfile);
                if (File.Exists(dbfile))
                    i_Files.Add(dbfile);

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

                try
                {
                    File.Delete(xmlfile);
                }
                catch { }
                try
                {
                    File.Delete(dbfile);
                }
                catch { }
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".dwxml")
            {
                FlowsheetObject.SaveToXML().Save(path);
            }
            else if (System.IO.Path.GetExtension(path).ToLower() == ".xml")
            {
                FlowsheetObject.SaveToMXML().Save(path);
            }

            FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationSaved, Interfaces.Enums.Scripts.ObjectType.Simulation, "");

            if (!s.AutomationMode)
            {
                if (!backup)
                {
                    FlowsheetObject.Options.FilePath = path;
                    Title = FlowsheetObject.Options.SimulationName + " [" + FlowsheetObject.Options.FilePath + "]";
                    FlowsheetObject.ShowMessage("Simulation file successfully saved to '" + path + "'.", Interfaces.IFlowsheet.MessageType.Information);
                }
                else
                {
                    //FlowsheetObject.ShowMessage("Backup file successfully saved to '" + path + "'.", Interfaces.IFlowsheet.MessageType.Information);
                }
            }

        }

        void LoadObjects()
        {

            var dir = Path.GetDirectoryName(this.GetType().Assembly.Location);

            var calculatorassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.Thermodynamics.dll"));
            Assembly unitopassembly = null;
            try
            {
                unitopassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.UnitOperations.dll"));
            }
            catch (Exception ex)
            {
                var reflection = ex as ReflectionTypeLoadException;
                if (reflection != null)
                {
                    foreach (var exception in reflection.LoaderExceptions)
                    {
                        Console.WriteLine(exception.ToString());
                    }
                }
            }

            var fsolverassembly = System.Reflection.Assembly.LoadFile(Path.Combine(dir, "DWSIM.FlowsheetSolver.dll"));
            List<Type> availableTypes = new List<Type>();

            availableTypes.AddRange(calculatorassembly.GetTypes().Where(x => x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null ? true : false));
            availableTypes.AddRange(unitopassembly.GetTypes().Where(x => x.GetInterface("DWSIM.Interfaces.ISimulationObject") != null && !x.IsAbstract ? true : false));

            List<ListItem> litems = new List<ListItem>();

            foreach (var item in availableTypes.OrderBy(x => x.Name))
            {
                if (!item.IsAbstract)
                {
                    var obj = (Interfaces.ISimulationObject)Activator.CreateInstance(item);
                    if (!ObjectList.ContainsKey(obj.GetDisplayName())) ObjectList.Add(obj.GetDisplayName(), obj);
                }
            }

            foreach (var item in FlowsheetObject.ExternalUnitOperations.Values.OrderBy(x => x.Name))
            {
                if (!ObjectList.ContainsKey(item.Name)) ObjectList.Add(item.Name, (Interfaces.ISimulationObject)item);
            }

            string netprops = "";

            PropertyInfo[] props = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetProperties();

            foreach (var p in props)
            {
                netprops = (netprops + (p.Name + " "));
            }

            MethodInfo[] methods = calculatorassembly.GetType("DWSIM.Thermodynamics.Streams.MaterialStream").GetMethods();
            foreach (var m in methods)
            {
                netprops = (netprops + (m.Name + " "));
            }

            props = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetProperties();
            foreach (var p in props)
            {
                netprops = (netprops + (p.Name + " "));
            }

            methods = unitopassembly.GetType("DWSIM.UnitOperations.Streams.EnergyStream").GetMethods();
            foreach (var m in methods)
            {
                netprops = (netprops + (m.Name + " "));
            }

            props = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetProperties();
            foreach (var p in props)
            {
                if ((p.PropertyType.Namespace != "System.Windows.Forms"))
                {
                    netprops = (netprops + (p.Name + " "));
                }
            }

            methods = calculatorassembly.GetType("DWSIM.Thermodynamics.PropertyPackages.PropertyPackage").GetMethods();
            foreach (var m in methods)
            {
                netprops = (netprops + (m.Name + " "));
            }

            string objects = "";

            objects = "ims1 ims2 ims3 ims4 ims5 ims6 ies1 oms1 oms2 oms3 oms4 oms5 oms6 oes1 Flowsheet Spreadsheet Plugins Solver Me DWSIM";

            this.FlowsheetObject.ScriptKeywordsU = netprops + objects;

            // editor is being used at flowsheet level.
            props = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetProperties();
            foreach (var p in props)
            {
                if ((p.PropertyType.Namespace != "System.Windows.Forms"))
                {
                    netprops = (netprops + (p.Name + " "));
                }
            }

            methods = fsolverassembly.GetType("DWSIM.FlowsheetSolver.FlowsheetSolver").GetMethods();
            foreach (var m in methods)
            {
                netprops = (netprops + (m.Name + " "));
            }

            objects = "MaterialStream EnergyStream PropertyPackage UnitOp Flowsheet Plugins Solver DWSIM";

            this.FlowsheetObject.ScriptKeywordsF = netprops + objects;
        }

        Eto.Forms.DocumentControl SetupLogWindow()
        {

            var outtxt = new RichTextArea();
            outtxt.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            outtxt.ReadOnly = true;
            outtxt.SelectionBold = true;

            FlowsheetObject.ActClearLog = () =>
            {
                Application.Instance.AsyncInvoke(() =>
                {
                    outtxt.Text = "";
                });
            };

            var container = new DocumentControl() { DisplayArrows = false };

            container.Pages.Add(new DocumentPage(outtxt) { Text = "Log Panel", Closable = false });

            var ctxmenu0 = new ContextMenu();

            var menuitem0 = new ButtonMenuItem { Text = "Clear List" };

            menuitem0.Click += (sender, e) =>
            {
                outtxt.Text = "";
            };

            ctxmenu0.Items.Add(menuitem0);

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
                    var item = "[" + DateTime.Now.ToString() + "] " + text;
                    try
                    {
                        if (FlowsheetObject.DynamicMode)
                        {
                            if (outtxt.Text.Length > 500 * 50) outtxt.Text = "";
                        }
                        outtxt.Append(item, true);
                        if (s.RunningPlatform() == s.Platform.Windows)
                        {
                            outtxt.Selection = new Range<int>(outtxt.Text.Length - item.Length - 1, outtxt.Text.Length - 1);
                        }
                        else
                        {
                            outtxt.Selection = new Range<int>(outtxt.Text.Length - item.Length, outtxt.Text.Length - 1);
                        }
                        switch (mtype)
                        {
                            case Interfaces.IFlowsheet.MessageType.Information:
                                outtxt.SelectionForeground = SystemColors.ControlText;
                                break;
                            case Interfaces.IFlowsheet.MessageType.GeneralError:
                                if (s.DarkMode)
                                {
                                    outtxt.SelectionForeground = Colors.Salmon;
                                }
                                else
                                {
                                    outtxt.SelectionForeground = Colors.Red;
                                }
                                break;
                            case Interfaces.IFlowsheet.MessageType.Warning:
                                if (s.DarkMode)
                                {
                                    outtxt.SelectionForeground = Colors.Orange;
                                }
                                else
                                {
                                    outtxt.SelectionForeground = Colors.DarkOrange;
                                }
                                break;
                            case Interfaces.IFlowsheet.MessageType.Tip:
                                outtxt.SelectionForeground = SystemColors.ControlText;
                                break;
                            case Interfaces.IFlowsheet.MessageType.Other:
                                outtxt.SelectionForeground = SystemColors.ControlText;
                                break;
                            default:
                                break;
                        }
                        outtxt.SelectionBold = true;
                        outtxt.Append("\n", true);
                        outtxt.Selection = new Range<int>(outtxt.Text.Length);
                    }
                    catch { }

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
                obj.GraphicObject.Status = item1.Checked ? Interfaces.Enums.GraphicObjects.Status.Idle : Interfaces.Enums.GraphicObjects.Status.Inactive;
            };

            var item1a = new CheckMenuItem { Text = "Toggle Show/Hide Label", Checked = obj.GraphicObject.Active };

            item1a.CheckedChanged += (sender, e) =>
            {
                obj.GraphicObject.DrawLabel = !obj.GraphicObject.DrawLabel;
            };

            var item3 = new ButtonMenuItem { Text = "Calculate", Image = new Bitmap(bitmapprefix + "icons8-play.png") };
            item3.Click += (sender, e) => FlowsheetObject.RequestCalculation(obj, false);

            var item4 = new ButtonMenuItem { Text = "Debug", Image = new Bitmap(bitmapprefix + "Console_96px.png") };
            item4.Click += (sender, e) =>
            {
                DebugObject();
            };

            var selobj = FlowsheetControl.FlowsheetSurface.SelectedObject;

            var menuitem0 = new ButtonMenuItem { Text = "Edit/View", Image = new Bitmap(bitmapprefix + "EditProperty_96px.png") };
            menuitem0.Click += (sender, e) =>
            {
                var simobj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
                if (simobj == null) return;
                EditObject_New(simobj);
            };

            var item5 = new ButtonMenuItem { Text = "Clone", Image = new Bitmap(bitmapprefix + "Copy_96px.png") };
            item5.Click += (sender, e) =>
            {
                Interfaces.ISimulationObject isobj;
                if (obj is Interfaces.IExternalUnitOperation)
                {
                    isobj = (Interfaces.ISimulationObject)obj.CloneXML();
                    FlowsheetObject.AddObjectToSurface(obj.GraphicObject.ObjectType,
                        (int)obj.GraphicObject.X + 50,
                        (int)obj.GraphicObject.Y + 50,
                        obj.GraphicObject.Tag + "_CLONE", "",
                        (Interfaces.IExternalUnitOperation)isobj);
                }
                else
                {
                    isobj = FlowsheetObject.AddObject(obj.GraphicObject.ObjectType, (int)obj.GraphicObject.X + 50, (int)obj.GraphicObject.Y + 50, obj.GraphicObject.Tag + "_CLONE");
                }
                var id = isobj.Name;
                ((Interfaces.ICustomXMLSerialization)isobj).LoadData(((Interfaces.ICustomXMLSerialization)obj).SaveData());
                isobj.Name = id;
                if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
                {
                    foreach (var phase in ((DWSIM.Thermodynamics.Streams.MaterialStream)isobj).Phases.Values)
                    {
                        foreach (var comp in FlowsheetObject.SelectedCompounds.Values)
                        {
                            phase.Compounds[comp.Name].ConstantProperties = comp;
                        }
                    }
                }
            };

            var item6 = new ButtonMenuItem { Text = "Delete", Image = new Bitmap(bitmapprefix + "Delete_96px.png") };

            item6.Click += (sender, e) =>
            {
                DeleteObject();
            };

            var item7 = new ButtonMenuItem { Text = "Copy Data to Clipboard", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };

            item7.Click += (sender, e) =>
            {
                //copy all simulation properties from the selected object to clipboard
                try
                {
                    var sobj = FlowsheetControl.FlowsheetSurface.SelectedObject;
                    ((SharedClasses.UnitOperations.BaseClass)FlowsheetObject.SimulationObjects[sobj.Name]).CopyDataToClipboard((DWSIM.SharedClasses.SystemsOfUnits.Units)FlowsheetObject.FlowsheetOptions.SelectedUnitSystem, FlowsheetObject.FlowsheetOptions.NumberFormat);
                }
                catch (Exception ex)
                {
                    FlowsheetObject.ShowMessage("Error copying data to clipboard: " + ex.ToString(), Interfaces.IFlowsheet.MessageType.GeneralError);
                }
            };

            selctxmenu.Items.AddRange(new MenuItem[] { item0, item1, item1a, new SeparatorMenuItem(), menuitem0, item7, new SeparatorMenuItem(), item3, item4, new SeparatorMenuItem(), item5, item6 });

            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
            {
                bool cancopy;
                if (!obj.GraphicObject.InputConnectors[0].IsAttached)
                {
                    cancopy = true;
                }
                else
                {
                    if (obj.GraphicObject.InputConnectors[0].AttachedConnector.AttachedFrom.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Recycle)
                    {
                        cancopy = true;
                    }
                    else
                    {
                        cancopy = false;
                    }
                }
                if (cancopy)
                {
                    var aitem1 = new ButtonMenuItem { Text = "Copy Data From...", Image = new Bitmap(bitmapprefix + "Copy_96px.png") };
                    foreach (var mstr in FlowsheetObject.SimulationObjects.Values.Where((x) => x is Thermodynamics.Streams.MaterialStream))
                    {
                        if (mstr.GraphicObject.Tag != obj.GraphicObject.Tag)
                        {
                            var newtsmi = new ButtonMenuItem { Text = mstr.GraphicObject.Tag };
                            newtsmi.Click += (sender, e) =>
                            {
                                var obj1 = FlowsheetObject.SimulationObjects[obj.Name];
                                var obj2 = FlowsheetObject.GetSelectedFlowsheetSimulationObject(newtsmi.Text);
                                ((Thermodynamics.Streams.MaterialStream)obj1).Assign((Thermodynamics.Streams.MaterialStream)obj2);
                                SolveFlowsheet(false);
                            };
                            if (mstr.GraphicObject.Calculated) aitem1.Items.Add(newtsmi);
                        }
                    }
                    selctxmenu.Items.Insert(5, aitem1);
                }
                var aitem2 = new ButtonMenuItem { Text = "Split Stream", Image = new Bitmap(bitmapprefix + "icons8-line_spliting_filled.png") };
                aitem2.Click += (sender, e) =>
                {
                    try
                    {

                        var stream = FlowsheetControl.FlowsheetSurface.SelectedObject;
                        var isobj = FlowsheetObject.AddObject(obj.GraphicObject.ObjectType, (int)obj.GraphicObject.X + 20, (int)obj.GraphicObject.Y, obj.GraphicObject.Tag + "_CLONE");
                        var id = isobj.Name;
                        ((Interfaces.ICustomXMLSerialization)isobj).LoadData(((Interfaces.ICustomXMLSerialization)obj).SaveData());
                        isobj.Name = id;
                        foreach (var phase in ((DWSIM.Thermodynamics.Streams.MaterialStream)isobj).Phases.Values)
                        {
                            foreach (var comp in FlowsheetObject.SelectedCompounds.Values)
                            {
                                phase.Compounds[comp.Name].ConstantProperties = comp;
                            }
                        }
                        isobj.GraphicObject.Status = stream.Status;
                        Interfaces.IGraphicObject objfrom;
                        int fromidx;
                        if (stream.InputConnectors[0].IsAttached)
                        {
                            objfrom = stream.InputConnectors[0].AttachedConnector.AttachedFrom;
                            fromidx = stream.InputConnectors[0].AttachedConnector.AttachedFromConnectorIndex;
                            FlowsheetObject.DisconnectObjects(objfrom, stream);
                            FlowsheetObject.ConnectObjects(objfrom, isobj.GraphicObject, fromidx, 0);
                        }
                    }
                    catch (Exception ex)
                    {
                        FlowsheetObject.ShowMessage("Error splitting Material Stream: " + ex.ToString(), Interfaces.IFlowsheet.MessageType.GeneralError);
                    }
                };
                selctxmenu.Items.Insert(5, aitem2);
            }

            return;

        }

        private void DebugObject()
        {
            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
            if (obj == null) return;
            var txt = new TextArea { ReadOnly = true, Wrap = true, Font = s.RunningPlatform() == s.Platform.Mac ? new Font("Menlo", s.ResultsReportFontSize) : Fonts.Monospace(s.ResultsReportFontSize) };
            txt.Text = "Please wait, debugging object...";
            var form1 = DWSIM.UI.Shared.Common.CreateDialog(txt, "Debugging" + " " + obj.GraphicObject.Tag + "...", 500, 600);
            Task.Factory.StartNew(() => { return obj.GetDebugReport(); }).ContinueWith(t => { Application.Instance.Invoke(() => { txt.Text = t.Result; }); }, TaskContinuationOptions.ExecuteSynchronously);
            form1.ShowModal(this);
        }

        void SetupDeselectedContextMenu()
        {

            deselctxmenu.Items.Clear();

            var item0 = new ButtonMenuItem { Text = "Add New Object", Image = new Bitmap(bitmapprefix + "icons8-workflow.png") };

            int currposx = (int)Mouse.Position.X - Location.X;
            int currposy = (int)Mouse.Position.Y - Location.Y;

            foreach (var item in ObjectList.Values)
            {
                var menuitem = new ButtonMenuItem { Text = item.GetDisplayName() };
                menuitem.Click += (sender2, e2) =>
                {
                    var z = FlowsheetControl.FlowsheetSurface.Zoom;
                    if (item is Interfaces.IExternalUnitOperation)
                    {
                        var isobj = (Interfaces.ISimulationObject)item.CloneXML();
                        FlowsheetObject.AddObjectToSurface(Interfaces.Enums.GraphicObjects.ObjectType.External,
                            (int)(currposx / z), (int)(currposy / z),
                            "", "",
                            (Interfaces.IExternalUnitOperation)isobj);
                    }
                    else
                    {
                        FlowsheetObject.AddObject(item.GetDisplayName(), (int)(currposx / z), (int)(currposy / z), "", "", true);
                    }
                    FlowsheetControl.Invalidate();
                    UpdateEditorConnectionsPanel();
                };
                item0.Items.Add(menuitem);
            }

            var item1 = new ButtonMenuItem { Text = "Zoom All", Image = new Bitmap(bitmapprefix + "icons8-zoom_to_extents.png") };
            var item2 = new ButtonMenuItem { Text = "Default Zoom", Image = new Bitmap(bitmapprefix + "icons8-zoom_to_actual_size_filled.png") };

            item1.Click += (sender, e) =>
            {
                ActZoomFit.Invoke();
            };

            item2.Click += (sender, e) =>
            {
                ActZoomDefault.Invoke();
            };

            var item4 = new ButtonMenuItem { Text = "Copy as Image (100%)", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };
            var item5 = new ButtonMenuItem { Text = "Copy as Image (200%)", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };
            var item6 = new ButtonMenuItem { Text = "Copy as Image (300%)", Image = new Bitmap(bitmapprefix + "icons8-copy_2_filled.png") };

            item4.Click += (sender, e) => CopyAsImage(1);
            item5.Click += (sender, e) => CopyAsImage(2);
            item6.Click += (sender, e) => CopyAsImage(3);

            var item7a = new ButtonMenuItem { Text = "Perform Natural Layout", Image = new Bitmap(bitmapprefix + "icons8-parallel_workflow.png") };
            var item7b = new ButtonMenuItem { Text = "Perform Auto-Layout", Image = new Bitmap(bitmapprefix + "icons8-parallel_workflow.png") };
            var item8 = new ButtonMenuItem { Text = "Restore Layout", Image = new Bitmap(bitmapprefix + "icons8-parallel_workflow.png") };

            item7a.Click += (sender, e) =>
            {
                FlowsheetControl.FlowsheetSurface.ApplyNaturalLayout((List<string>)DWSIM.FlowsheetSolver.FlowsheetSolver.GetSolvingList(this.FlowsheetObject, false)[0], 75);
                ActZoomFit.Invoke();
            };

            item7b.Click += (sender, e) =>
            {
                FlowsheetControl.FlowsheetSurface.AutoArrange();
                ActZoomFit.Invoke();
            };

            item8.Click += (sender, e) =>
            {
                FlowsheetControl.FlowsheetSurface.RestoreLayout();
                ActZoomFit.Invoke();
            };

            var item9 = new ButtonMenuItem { Text = "Export to PDF File", Image = new Bitmap(bitmapprefix + "icons8-pdf.png") };
            var item10 = new ButtonMenuItem { Text = "Export to SVG File", Image = new Bitmap(bitmapprefix + "icons8-vector.png") };

            item9.Click += (sender, e) => ExportToPDF();
            item10.Click += (sender, e) => ExportToSVG();

            deselctxmenu.Items.AddRange(new MenuItem[] { item0, new SeparatorMenuItem(), item1, item2, new SeparatorMenuItem(), item4, item5, item6, new SeparatorMenuItem(), item9, item10, new SeparatorMenuItem(), item7a, item7b, item8 });

            return;

        }

        void ExportToPDF()
        {
            var ds = (float)GlobalSettings.Settings.DpiScale;
            var dialog = new SaveFileDialog();
            dialog.Title = "Export Flowsheet to PDF File";
            dialog.Filters.Add(new FileFilter("PDF Files", new[] { ".pdf" }));
            dialog.CurrentFilterIndex = 0;
            if (dialog.ShowDialog(this) == DialogResult.Ok)
            {
                // create the document
                using (var stream = SKFileWStream.OpenStream(dialog.FileName))
                {
                    using (var document = SKDocument.CreatePdf(stream))
                    {
                        var canvas = document.BeginPage(FlowsheetControl.Width * ds, FlowsheetControl.Height * ds);
                        FlowsheetControl.FlowsheetSurface.UpdateCanvas(canvas);
                        // end the page and document
                        document.EndPage();
                        document.Close();
                    }
                }
                FlowsheetObject.ShowMessage(String.Format("Flowsheet exported successfully to {0}.", dialog.FileName), Interfaces.IFlowsheet.MessageType.Information);
            }
        }

        void ExportToSVG()
        {
            var ds = (float)GlobalSettings.Settings.DpiScale;
            var dialog = new SaveFileDialog();
            dialog.Title = "Export Flowsheet to SVG File";
            dialog.Filters.Add(new FileFilter("SVG Files", new[] { ".svg" }));
            dialog.CurrentFilterIndex = 0;
            if (dialog.ShowDialog(this) == DialogResult.Ok)
            {
                // create the document
                using (var stream = SKFileWStream.OpenStream(dialog.FileName))
                {
                    var writer = new SKXmlStreamWriter(stream);
                    using (var canvas = SKSvgCanvas.Create(SKRect.Create(FlowsheetControl.Width * ds, FlowsheetControl.Height * ds), writer))
                    {
                        FlowsheetControl.FlowsheetSurface.UpdateCanvas(canvas);
                    }
                }
                FlowsheetObject.ShowMessage(String.Format("Flowsheet exported successfully to {0}.", dialog.FileName), Interfaces.IFlowsheet.MessageType.Information);
            }
        }

        void CopyAsImage(int Zoom)
        {
            using (SkiaSharp.SKBitmap bmp = new SkiaSharp.SKBitmap(FlowsheetControl.Width * Zoom, FlowsheetControl.Height * Zoom))
            {
                using (SkiaSharp.SKCanvas canvas = new SkiaSharp.SKCanvas(bmp))
                {
                    canvas.Scale(Zoom);
                    FlowsheetControl.FlowsheetSurface.UpdateCanvas(canvas);
                    var d = SkiaSharp.SKImage.FromBitmap(bmp).Encode(SkiaSharp.SKEncodedImageFormat.Png, 100);
                    using (System.IO.MemoryStream str = new MemoryStream())
                    {
                        d.SaveTo(str);
                        Clipboard.Instance.Image = new Bitmap(str);
                        FlowsheetObject.ShowMessage("The flowsheet was copied as an image to the clipboard.", Interfaces.IFlowsheet.MessageType.Information);
                    }
                }
            }
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
            form.ShowInTaskbar = true;
            form.Topmost = true;
            form.Show();
            form.Width += 1;
        }

        void EditAppearance()
        {
            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
            if (obj == null) return;
            if (obj.GraphicObject is ShapeGraphic)
            {
                var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - Edit Appearance", 500, 500, new ObjectAppearanceEditorView(FlowsheetObject, (ShapeGraphic)obj.GraphicObject));
                form.ShowInTaskbar = true;
                form.Topmost = true;
                form.Show();
                form.Width += 1;
            }
        }

        private void EditSelectedObjectProperties()
        {
            Interfaces.IGraphicObject selobj;

            selobj = FlowsheetControl.FlowsheetSurface.SelectedObject;

            if (selobj != null)
            {
                if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Table)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.PropertyTableEditor { Table = (DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables.TableGraphic)selobj };
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_SpreadsheetTable)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.SpreadsheetTableEditor { Table = (SpreadsheetTableGraphic)selobj };
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_MasterTable)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Tables.MasterPropertyTableEditor { Table = (MasterTableGraphic)selobj };
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart)
                {
                    var editor = new DWSIM.UI.Desktop.Editors.Charts.ChartObjectEditor((OxyPlotGraphic)selobj);
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
                    editor.Show();
                }
                else if (selobj.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.GO_HTMLText)
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
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
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
                    editor.ShowInTaskbar = true;
                    editor.Topmost = true;
                    editor.Show();
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
            form.ShowInTaskbar = true;
            form.Show();
            form.Width += 1;

        }

        private void DeleteObject()
        {
            var n = FlowsheetControl.FlowsheetSurface.SelectedObjects.Count;
            if (n > 1)
            {
                if (MessageBox.Show("Delete " + n.ToString() + " objects?", "Mass Delete", MessageBoxButtons.YesNo) == DialogResult.Yes)
                {
                    var indexes = new List<string>();
                    foreach (GraphicObject gobj in FlowsheetControl.FlowsheetSurface.SelectedObjects.Values)
                    {
                        indexes.Add(gobj.Tag);
                    }
                    foreach (string s in indexes)
                    {
                        Interfaces.IGraphicObject gobj;
                        gobj = FlowsheetObject.GetFlowsheetSimulationObject(s)?.GraphicObject;
                        if (gobj != null)
                        {
                            FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), gobj, false, false);
                        }
                    }
                }
            }
            else if (n == 1)
            {
                var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
                if (obj == null) return;
                if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                {
                    var editor = EditorHolder.Pages.Where(x => (string)x.Content.Tag == obj.Name).FirstOrDefault();
                    if (editor != null)
                    {
                        EditorHolder.Pages.Remove(editor);
                    }
                    FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj.GraphicObject, false, false);
                }
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

            try
            {
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
            var defaults = new string[] { "SI", "CGS", "ENG", "C1", "C2", "C3", "C4", "C5" };

            foreach (var unit in FlowsheetObject.AvailableSystemsOfUnits)
            {
                if (!defaults.Contains(unit.Name) && !names.Contains(unit.Name))
                {
                    userunits.Add((DWSIM.SharedClasses.SystemsOfUnits.Units)unit);
                }
            }

            GlobalSettings.Settings.UserUnits = Newtonsoft.Json.JsonConvert.SerializeObject(userunits, Newtonsoft.Json.Formatting.Indented).Replace("\"", "\'");

        }

        private void EditObject_New(Interfaces.ISimulationObject obj)
        {

            var existingeditor = EditorHolder.Pages.Where(x => x.Content.Tag.ToString() == obj.Name).FirstOrDefault();

            if (existingeditor != null)
            {
                EditorHolder.SelectedPage = (DocumentPage)existingeditor;
            }
            else
            {
                var editor = new ObjectEditorContainer(obj);
                var editorc = new DocumentPage(editor) { Closable = true, Text = obj.GraphicObject.Tag };
                EditorHolder.Pages.Add(editorc);
                EditorHolder.SelectedPage = editorc;
                if (EditorHolder.Pages.Count > 6)
                {
                    try
                    {
                        EditorHolder.Pages.Remove(EditorHolder.Pages.First());
                    }
                    catch { }
                }
            }

            SplitterFlowsheet.Invalidate();

        }

        public void UpdateEditorPanels()
        {
            foreach (DocumentPage item in EditorHolder.Pages)
            {
                ((ObjectEditorContainer)item.Content).Update();
                ((ObjectEditorContainer)item.Content).UpdateConnections();
            }
        }

        public void CloseOpenedEditorPanels()
        {
            EditorHolder.Pages.Clear();
        }

        public void UpdateEditorConnectionsPanel()
        {
            foreach (DocumentPage item in EditorHolder.Pages)
            {
                ((ObjectEditorContainer)item.Content).UpdateConnections();
            }
        }

        public void AlignObjects(Button tsb)
        {
            try
            {
                Drawing.SkiaSharp.GraphicsSurface.AlignDirection direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Centers;
                string text = tsb.Tag.ToString();
                if (text.Contains("Lefts"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Lefts;
                }
                else if (text.Contains("Centers"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Centers;
                }
                else if (text.Contains("Rights"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Rights;
                }
                else if (text.Contains("Tops"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Tops;
                }
                else if (text.Contains("Middles"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Middles;
                }
                else if (text.Contains("Bottoms"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Bottoms;
                }
                else if (text.Contains("Vertical"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.EqualizeVertical;
                }
                else if (text.Contains("Horizontal"))
                {
                    direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.EqualizeHorizontal;
                }
                FlowsheetControl.FlowsheetSurface.AlignSelectedObjects(direction);
                FlowsheetControl.Invalidate();
            }
            catch (Exception ex)
            {
                Logging.Logger.LogError("Alignment Command Error", ex);
            }
        }

    }
}
