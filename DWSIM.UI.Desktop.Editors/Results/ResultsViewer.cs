using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Eto.Forms;
using Eto.Drawing;

using System.IO;

using DWSIM.Interfaces;
using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using System.Collections.ObjectModel;

using System.Globalization;

namespace DWSIM.UI.Desktop.Editors
{
    public class ResultsViewer : TableLayout
    {

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;
        private ListBox lbObjects;
        private TextArea txtResults;
        private Label lblLastCalc;

        public ResultsViewer(DWSIM.UI.Desktop.Shared.Flowsheet fs)
        {
            Flowsheet = fs;
            Init();
        }

        void Init()
        {

            var topcontainer = new TableLayout();
            var centercontainer = new TableLayout();

            var lbl = new Label { Text = "Select Object / View Results Report", Font = SystemFonts.Bold(), VerticalAlignment = VerticalAlignment.Center };
            lbl.Font = new Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize());

            var btnExport = new Button { Text = "Export Selected to PDF" };
            btnExport.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnExport.Click += (sender, e) =>
            {
                ExportReports(true);
            };

            var btnExportAll = new Button { Text = "Export All to PDF" };
            btnExportAll.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnExportAll.Click += (sender, e) =>
            {
                ExportReports(false);
            };

            var btnExportAllODS = new Button { Text = "Export All to ODS" };
            btnExportAllODS.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnExportAllODS.Click += (sender, e) =>
            {
                ExportODS();
            };

            var btnExportAllODT = new Button { Text = "Export All to ODT" };
            btnExportAllODT.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            btnExportAllODT.Click += (sender, e) =>
            {
                ExportODT();
            };

            topcontainer.Rows.Add(new TableRow(lbl, null, btnExport, btnExportAll, btnExportAllODS, btnExportAllODT));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            lblLastCalc = new Label { Text = "Last successful flowsheet calculation:" };
            lblLastCalc.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());

            topcontainer.Rows.Add(new TableRow(lblLastCalc));

            lbObjects = new ListBox { Width = 300 };
            lbObjects.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());

            txtResults = new TextArea { ReadOnly = true };
            txtResults.Font = GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac ? new Font("Menlo", Common.GetEditorFontSize()) : Fonts.Monospace(Common.GetEditorFontSize());

            centercontainer.Rows.Add(new TableRow(lbObjects, txtResults));
            centercontainer.Padding = new Padding(5, 5, 5, 5);
            centercontainer.Spacing = new Size(10, 10);

            Rows.Add(new TableRow(topcontainer));
            Rows.Add(new TableRow(centercontainer));

        }

        public void ExportReports(bool onlyselected)
        {
            var dialog = new SaveFileDialog();
            dialog.Title = "Export Results to PDF";
            dialog.Filters.Add(new FileFilter("PDF File", new[] { ".pdf" }));
            dialog.CurrentFilterIndex = 0;
            if (dialog.ShowDialog(this) == DialogResult.Ok)
            {
                using (FileStream fs = new FileStream(dialog.FileName, FileMode.OpenOrCreate, FileAccess.ReadWrite))
                {
                    var list = new List<ISimulationObject>();
                    if (onlyselected)
                    {
                        list.Add(Flowsheet.SimulationObjects[lbObjects.SelectedKey]);
                    }
                    else
                    {
                        foreach (var item in lbObjects.Items)
                        {
                            list.Add(Flowsheet.SimulationObjects[item.Key]);
                        }
                    }
                    Flowsheet.GenerateReport(list, "PDF", fs);
                }
            }
        }

        public void ExportODS()
        {
            var dialog = new SaveFileDialog();
            dialog.Title = "Export Results to Spreadsheet (ODS)";
            dialog.Filters.Add(new FileFilter("ODS File", new[] { ".ods" }));
            dialog.CurrentFilterIndex = 0;
            if (dialog.ShowDialog(this) == DialogResult.Ok)
            {
                new DWSIM.FlowsheetBase.ReportCreator(Flowsheet).CreateAndSaveODSFile(dialog.FileName);
            }
        }
        public void ExportODT()
        {
            var dialog = new SaveFileDialog();
            dialog.Title = "Export Results to Document (ODT)";
            dialog.Filters.Add(new FileFilter("ODT File", new[] { ".odt" }));
            dialog.CurrentFilterIndex = 0;
            if (dialog.ShowDialog(this) == DialogResult.Ok)
            {
                new DWSIM.FlowsheetBase.ReportCreator(Flowsheet).CreateAndSaveODTFile(dialog.FileName);
            }
        }

        public void UpdateList()
        {
            lblLastCalc.Text = "Last successful flowsheet calculation: " + DateTime.Now.ToString();
            lbObjects.Items.Clear();
            foreach (var obj in Flowsheet.SimulationObjects.Values)
            {
                var li = new ListItem { Key = obj.Name, Text = obj.GraphicObject.Tag };
                lbObjects.Items.Add(li);
            }

            lbObjects.SelectedIndexChanged += (sender, e) =>
            {
                if (lbObjects.SelectedKey != null) txtResults.Text = Flowsheet.SimulationObjects[lbObjects.SelectedKey].GetReport(Flowsheet.FlowsheetOptions.SelectedUnitSystem, CultureInfo.InvariantCulture, Flowsheet.FlowsheetOptions.NumberFormat);
            };

        }


    }
}
