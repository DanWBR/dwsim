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

        void InitializeComponent()
        {

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            FlowsheetObject = new Desktop.Shared.Flowsheet() { FlowsheetForm = this };

            Title = "New Flowsheet";
            
            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var fsc = new DWSIM.UI.Controls.FlowsheetSurfaceControl();

            fsc.FlowsheetSurface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)FlowsheetObject.GetSurface();

            fsc.FlowsheetSurface.BackgroundColor = SkiaSharp.SKColors.White;

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
                 
            var outtxt = new TextArea { Text = "", ReadOnly = true, Font = Fonts.Monospace(SystemFonts.Default().Size)};

            var ctxmenu0 = new ContextMenu();

            var menuitem0 = new ButtonMenuItem { Text = "Clear" };

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

            FlowsheetObject.SetMessageListener((string text) => {
               Application.Instance.Invoke(() => outtxt.Append("[" + DateTime.Now.ToString() + "] " + text + "\n", true));
            });

            var split = new Splitter();
            split.Panel1 = fsc;
            split.Panel2 = outtxt;
            split.Orientation = Orientation.Vertical;
            split.FixedPanel = SplitterFixedPanel.Panel2;
            split.Panel2.Height = 150;

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

            fsc.MouseUp += (sender, e) => {
                if (e.Buttons == MouseButtons.Alternate)
                {
                    ctxmenu.Show(fsc);
                }
            };

            Closing += Flowsheet_Closing;

        }

        void Flowsheet_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (MessageBox.Show(this, "ConfirmFlowsheetExit".Localize(), "FlowsheetExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.No)
            {
                e.Cancel = true; 
            }
        }
              
    }
}
