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

            var lbl = new Label { Text = "Select Object / View Results Report", VerticalAlignment = VerticalAlignment.Center };
            
            var btnExport = new Button { Text = "Export to PDF" };
            btnExport.Click += (sender, e) => {
                ExportReports(true);
            };

            var btnExportAll = new Button { Text = "Export All to PDF" };
            btnExportAll.Click += (sender, e) =>
            {
                ExportReports(false);
            };

            topcontainer.Rows.Add(new TableRow(lbl, null, btnExport, btnExportAll));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            lblLastCalc = new Label { Text = "Last successful flowsheet calculation:" };

            topcontainer.Rows.Add(new TableRow(lblLastCalc));

            lbObjects = new ListBox { Width = 300 };
            txtResults = new TextArea { ReadOnly = true, Font = Fonts.Monospace(SystemFonts.Default().Size) };

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
            dialog.Filters.Add(new FileDialogFilter("PDF File", new[] { ".pdf" }));
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
                txtResults.Text = Flowsheet.SimulationObjects[lbObjects.SelectedKey].GetReport(Flowsheet.FlowsheetOptions.SelectedUnitSystem, CultureInfo.InvariantCulture, Flowsheet.FlowsheetOptions.NumberFormat);
            };

        }


    }
}
