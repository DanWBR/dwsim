using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using DWSIM.SharedClasses.Charts;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Editors.Charts
{
    public class ChartManager : DynamicLayout
    {

        private DWSIM.UI.Desktop.Shared.Flowsheet Flowsheet;

        public DocumentControl TabControl;

        private bool IsLoaded = false;

        public ChartManager(DWSIM.UI.Desktop.Shared.Flowsheet fs)
        {

            Flowsheet = fs;

            Init();

        }

        public void Init()
        {

            TabControl = new DocumentControl();

            this.Shown += (sender, e) =>
            {
                var selectedtab = TabControl.SelectedPage;
                if (selectedtab != null)
                {
                    var chart = (ChartControl)selectedtab.Content;
                    Application.Instance.Invoke(() =>
                    {
                        chart.Splitter.Position = chart.Splitter.Width - 350;
                        chart.UpdatePlotModelData();
                        chart.UpdatePropertiesLayout();
                    });
                }
            };

            this.Load += (sender, e) =>
            {
                Application.Instance.Invoke(() =>
                {
                    if ((Flowsheet.Charts.Count > 0) && !IsLoaded)
                    {
                        foreach (var item in Flowsheet.Charts)
                        {
                            DocumentPage tabpage = new DocumentPage();
                            var ccontrol = new ChartControl { Chart = (Chart)item.Value, Flowsheet = Flowsheet, Spreadsheet = (ReoGridControl)Flowsheet.GetSpreadsheetObjectFunc?.Invoke() };
                            tabpage.Content = ccontrol;
                            TabControl.Pages.Add(tabpage);
                            tabpage.Text = item.Value.DisplayName;
                            tabpage.Closed += Tabpage_Closed;
                            tabpage.Shown += (s1, e1) => {
                                ccontrol.UpdatePlotModelData();
                                ccontrol.UpdatePropertiesLayout();
                            };
                        }
                    }
                    IsLoaded = true;
                });
            };

            var l1 = new Label { Text = "Create and view Charts using data from Flowsheet Objects or Spreadsheet Cell Ranges." };

            var b1 = new Button { Text = "Add New 2D XY Chart" };

            b1.Click += (s, e) =>
            {
                Application.Instance.Invoke((Action)(() =>
                {
                    DocumentPage tabpage = new DocumentPage();
                    var chart = new Chart();
                    tabpage.Text = chart.DisplayName;
                    var ccontrol = new ChartControl { Chart = chart, Flowsheet = Flowsheet, Spreadsheet = (ReoGridControl)Flowsheet.GetSpreadsheetObjectFunc?.Invoke() };
                    tabpage.Content = ccontrol;
                    ccontrol.UpdatePlotModelData();
                    ccontrol.UpdatePropertiesLayout();
                    TabControl.Pages.Add(tabpage);
                    tabpage.Closed += Tabpage_Closed;
                    tabpage.Shown += (s1, e1) => {
                        ccontrol.UpdatePlotModelData();
                        ccontrol.UpdatePropertiesLayout();
                    };
                }));
            };

            var tl = new TableLayout { Spacing = new Eto.Drawing.Size(5, 5), Padding = new Eto.Drawing.Padding(5) };

            var tr = new TableRow { Cells = { l1, b1 } };
            tr.Cells[0].ScaleWidth = true;

            tl.Rows.Add(tr);

            this.BeginVertical();
            this.Add(tl, true);
            this.Add(TabControl, true, true);
            this.BeginVertical();

        }

        private void Tabpage_Closed(object sender, EventArgs e)
        {
            var chart = (ChartControl)TabControl.SelectedPage.Content;
            Flowsheet.Charts.Remove(chart.Chart.ID);
        }

    }

}