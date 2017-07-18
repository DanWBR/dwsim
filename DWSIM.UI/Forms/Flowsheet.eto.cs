using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Xml;
using System.Xml.Linq;
using DWSIM.UI.Shared;

namespace DWSIM.UI.Forms
{
    partial class Flowsheet : Form
    {

        public Desktop.Shared.Flowsheet FlowsheetObject;
        private DWSIM.UI.Controls.FlowsheetSurfaceControl FlowsheetControl;

        void InitializeComponent()
        {

            WindowState = Eto.Forms.WindowState.Maximized;

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };

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
            var btnObjects = new ButtonToolItem { Text = "Objects", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")) };
            var btnTools = new ButtonToolItem { Text = "Tools", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-maintenance.png")) };
            var btnUtilities = new ButtonToolItem { Text = "Utilities", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-swiss_army_knife.png")) };
            var btnScripts = new ButtonToolItem { Text = "Scripts", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-property_script.png")) };
            var btnReports = new ButtonToolItem { Text = "Reports", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-report_card.png")) };
            var btnOptions = new ButtonToolItem { Text = "Options", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")) };

            var btnSolve = new ButtonToolItem { Text = "Solve Flowsheet", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png")) };
            btnSolve.Click += (sender, e) => {FlowsheetObject.SolveFlowsheet();};

            // create menu
            ToolBar = new ToolBar();
            if (Application.Instance.Platform.IsMac)
            {
                ToolBar.Items.AddRange(new ToolItem[] { btnSave, btnSaveAs, btnSolve,
                                           btnComps, btnBasis, btnObjects, btnTools, btnUtilities,
                                           btnScripts, btnReports, btnOptions});
            }
            else {
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

            var ctxmenu = new ContextMenu();

            var menuitem1 = new ButtonMenuItem { Text = "Edit Properties" };
            menuitem1.Click += (sender, e) =>
            {
                var cont = UI.Shared.Common.GetDefaultContainer();
                var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
                if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
                {
                    new DWSIM.UI.Desktop.Editors.MaterialStreamEditor(obj, cont);
                }
                else
                {
                    new DWSIM.UI.Desktop.Editors.GeneralEditors(obj, cont);
                }
                var form = UI.Shared.Common.GetDefaultEditorForm("Edit Properties", 500, 500, cont);
                form.ShowInTaskbar = false;
                form.Show();
            };

            var menuitem2 = new ButtonMenuItem { Text = "View Results" };
            menuitem2.Click += (sender, e) =>
            {
                var obj = FlowsheetObject.GetSelectedFlowsheetSimulationObject(null);
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

            ctxmenu.Items.AddRange(new[] { menuitem1, menuitem2 });

            FlowsheetControl.MouseUp += (sender, e) =>
            {
                if (e.Buttons == MouseButtons.Alternate)
                {
                    ctxmenu.Show(FlowsheetControl);
                }
            };

            Closing += Flowsheet_Closing;

            Shown += Flowsheet_Shown;

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

        Eto.Forms.Container SetupLogWindow()
        {

            var label = new Label {Text = "  " + "Log Window", Font = SystemFonts.Bold(), VerticalAlignment = VerticalAlignment.Center};

            var outtxt = new ListBox(); //{ Font = Fonts.Monospace(SystemFonts.Default().Size - 1.0f)};

            var container = new TableLayout { Rows = { label, outtxt }, Spacing = new Size(5, 5)};

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
                Application.Instance.Invoke(() =>{
                    
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

    }
}
