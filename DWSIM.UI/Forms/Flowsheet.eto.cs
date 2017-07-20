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

namespace DWSIM.UI.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;
        private DWSIM.UI.Controls.FlowsheetSurfaceControl FlowsheetControl;

        public List<Interfaces.ISimulationObject> ObjectList = new List<Interfaces.ISimulationObject>();

        void InitializeComponent()
        {

            WindowState = Eto.Forms.WindowState.Maximized;

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };
            FlowsheetObject.Initialize();

            Title = "New Flowsheet";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            FlowsheetControl = new DWSIM.UI.Controls.FlowsheetSurfaceControl();

            FlowsheetControl.FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface();

            FlowsheetControl.FlowsheetSurface.BackgroundColor = SkiaSharp.SKColors.White;

            ClientSize = new Size(1024, 768);

            var closeCommand = new Command { MenuText = "Close".Localize(), Shortcut = Application.Instance.CommonModifier | Keys.Q };
            closeCommand.Executed += (sender, e) => Close();

            var saveCommand = new Command { MenuText = "SaveFlowsheet".Localize() };

            var btnSave = new ButtonToolItem { Text = "Save", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save.png")) };
            var btnSaveAs = new ButtonToolItem { Text = "Save As", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-save_as.png")) };
            var btnComps = new ButtonToolItem { Text = "Compounds", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-thin_test_tube.png")) };
            var btnBasis = new ButtonToolItem { Text = "Basis", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-math.png")) };
            var btnObjects = new ButtonToolItem { Text = "Insert Object", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")) };
            var btnTools = new ButtonToolItem { Text = "Tools", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-maintenance.png")) };
            var btnUtilities = new ButtonToolItem { Text = "Utilities", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };
            var btnScripts = new ButtonToolItem { Text = "Script Manager", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-property_script.png")) };
            var btnReports = new ButtonToolItem { Text = "Reports", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-report_card.png")) };
            var btnOptions = new ButtonToolItem { Text = "Simulation Options", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")) };

            btnComps.Click += (sender, e) => {
                var cont = UI.Shared.Common.GetDefaultContainer();
                new DWSIM.UI.Desktop.Editors.Compounds(FlowsheetObject, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm("Simulation Compounds", 500, 500, cont);
                form.ShowInTaskbar = false;
                form.Show();
            };

            btnBasis.Click += (sender, e) =>
            {
                var cont = UI.Shared.Common.GetDefaultContainer();
                new DWSIM.UI.Desktop.Editors.Models(FlowsheetObject, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm("Simulation Basis", 500, 500, cont);
                form.ShowInTaskbar = false;
                form.Show();
            };

            var btnSolve = new ButtonToolItem { Text = "Solve Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")) };

            btnSolve.Click += (sender, e) => { FlowsheetObject.SolveFlowsheet(); };

            btnSave.Click += (sender, e) =>
            {

                if (FlowsheetObject.Options.FilePath != "")
                {
                    SaveSimulation(FlowsheetObject.Options.FilePath);
                }
                else
                {
                    btnSaveAs.OnClick(new EventArgs());
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

            // create menu
            ToolBar = new ToolBar();
            if (Application.Instance.Platform.IsMac)
            {
                ToolBar.Items.AddRange(new ToolItem[] { btnSave, btnSaveAs, btnSolve,
                                           btnComps, btnBasis, btnObjects, btnTools, btnUtilities,
                                           btnScripts, btnReports, btnOptions});
            }
            else
            {
                ToolBar.Items.AddRange(new ToolItem[] { btnSave, btnSaveAs, new SeparatorToolItem(), btnSolve, new SeparatorToolItem(),
                                           btnComps, btnBasis, btnObjects, new SeparatorToolItem(), btnTools, btnUtilities,
                                           btnScripts, btnReports, btnOptions});
            }

            var split = new Splitter();
            split.Panel1 = FlowsheetControl;
            split.Panel2 = SetupLogWindow();
            split.Orientation = Orientation.Vertical;
            split.FixedPanel = SplitterFixedPanel.Panel2;
            split.Panel2.Height = 100;

            Content = split;

            FlowsheetControl.MouseUp += (sender, e) =>
            {
                if (e.Buttons == MouseButtons.Alternate)
                {
                    if (FlowsheetControl.FlowsheetSurface.SelectedObject != null)
                    {
                        SetupSelectedContextMenu().Show(FlowsheetControl);
                    }
                    else
                    {
                        SetupDeselectedContextMenu().Show(FlowsheetControl);
                    }
                }
            };

            Closing += Flowsheet_Closing;

            Shown += Flowsheet_Shown;

            Task.Factory.StartNew(() => LoadObjects());

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
                    ObjectList.Add(obj);
                }
            }

        }

        Eto.Forms.Container SetupLogWindow()
        {

            var label = new Label { Text = "  " + "Log Window", Font = SystemFonts.Bold(), VerticalAlignment = VerticalAlignment.Center };

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
                Application.Instance.Invoke(() =>
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

        Eto.Forms.ContextMenu SetupSelectedContextMenu()
        {

            var ctxmenu = new ContextMenu();

            var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);

            var item0 = new ButtonMenuItem { Text = obj.GraphicObject.Tag, Enabled = false };

            var item1 = new CheckMenuItem { Text = "Toggle Active/Inactive", Checked = obj.GraphicObject.Active };

            item1.CheckedChanged += (sender, e) =>
            {
                obj.GraphicObject.Active = item1.Checked;
            };

            var item2 = new ButtonMenuItem { Text = "Edit Connections" };
            item2.Click += (sender, e) =>
            {
                var cont = UI.Shared.Common.GetDefaultContainer();
                UI.Shared.Common.CreateAndAddLabelRow(cont, "Object Connections".Localize());
                UI.Shared.Common.CreateAndAddDescriptionRow(cont, "ConnectorsEditorDescription".Localize());
                new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont);
                var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - Edit Connections", 500, 500, cont);
                form.ShowInTaskbar = false;
                form.Show();
            };

            var item3 = new ButtonMenuItem { Text = "Calculate" };
            item3.Click += (sender, e) => FlowsheetObject.SolveFlowsheet(obj);

            var item4 = new ButtonMenuItem { Text = "Debug" };
            item4.Click += (sender, e) =>
            {
                var txt = new TextArea { ReadOnly = true, Wrap = true };
                txt.Text = "Please wait, debugging object...";
                var form1 = DWSIM.UI.Shared.Common.CreateDialog(txt, "Debugging" + " " + obj.GraphicObject.Tag + "...", 400, 300);
                Task.Factory.StartNew(() => { return obj.GetDebugReport(); }).ContinueWith(t => { Application.Instance.Invoke(() => { txt.Text = t.Result; }); }, TaskContinuationOptions.ExecuteSynchronously);
                form1.ShowModal(this);
            };

            var menuitem1 = new ButtonMenuItem { Text = "Edit Properties" };
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
                    else
                    {
                        new DWSIM.UI.Desktop.Editors.GeneralEditors(obj, cont);
                    }
                    var form = UI.Shared.Common.GetDefaultEditorForm(obj.GraphicObject.Tag + " - Edit Properties", 500, 500, cont);
                    form.ShowInTaskbar = false;
                    form.Show();
                }
            };

            var menuitem2 = new ButtonMenuItem { Text = "View Results" };
            menuitem2.Click += (sender, e) =>
            {
                var report = obj.GetReport(FlowsheetObject.Options.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, FlowsheetObject.Options.NumberFormat);
                var form = new Form
                {
                    Title = "Results",
                    ClientSize = new Size(500, 600),
                    Content = new Scrollable { Content = new TextArea { Text = report, ReadOnly = true, Font = Fonts.Monospace(SystemFonts.Default().Size) } },
                    ShowInTaskbar = false
                };
                form.Show();
            };

            var item5 = new ButtonMenuItem { Text = "Clone" };
            item5.Click += (sender, e) =>
            {
                var isobj = FlowsheetObject.AddObject(obj.GraphicObject.ObjectType, obj.GraphicObject.X + 50, obj.GraphicObject.Y + 50, obj.GraphicObject.Tag + "_CLONE");
                ((Interfaces.ICustomXMLSerialization)isobj).LoadData(((Interfaces.ICustomXMLSerialization)obj).SaveData());
            };

            var item6 = new ButtonMenuItem { Text = "Delete" };

            item6.Click += (sender, e) =>
            {
                if (MessageBox.Show(this, "Confirm object removal?", "Delete Object", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                {
                    FlowsheetObject.DeleteSelectedObject(this, new EventArgs(), obj.GraphicObject, false, false);
                }
            };

            ctxmenu.Items.AddRange(new MenuItem[] { item0, item1, new SeparatorMenuItem(), item2, menuitem1, new SeparatorMenuItem(), item3, item4, new SeparatorMenuItem(), menuitem2, new SeparatorMenuItem(), item5, item6 });

            return ctxmenu;

        }

        Eto.Forms.ContextMenu SetupDeselectedContextMenu()
        {

            var ctxmenu = new ContextMenu();

            var item0 = new ButtonMenuItem { Text = "Add New Object" };

            int currposx = (int)Mouse.Position.X;
            int currposy = (int)Mouse.Position.Y;

            foreach (var item in ObjectList)
            {
                var menuitem = new ButtonMenuItem { Text = item.GetDisplayName() };
                menuitem.Click += (sender, e) =>
                {
                    var mp = this.PointFromScreen(new PointF(currposx, currposy));
                    var z = FlowsheetControl.FlowsheetSurface.Zoom;
                    FlowsheetObject.AddObject(item.GetDisplayName(), (int)(mp.X / z), (int)(mp.Y / z), "");
                };
                item0.Items.Add(menuitem);
            }

            ctxmenu.Items.AddRange(new MenuItem[] { item0 });

            return ctxmenu;

        }

    }
}
