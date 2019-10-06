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

        public ChartManager(DWSIM.UI.Desktop.Shared.Flowsheet fs)
        {

            Flowsheet = fs;

            Init();

        }

        public void Init()
        {

            TabControl = new DocumentControl();

            var tab1 = new DocumentPage { Content = new ChartControl(), Text = "MyChart" };

            TabControl.Pages.Add(tab1);

            var l1 = new Label { Text = "Create and view Charts using data from Flowsheet Objects or Spreadsheet Cell Ranges." };

            var b1 = new Button { Text = "Add New 2D XY Chart" };
            var b2 = new Button { Text = "Remove Selected" };

            var tl = new TableLayout {Spacing = new Eto.Drawing.Size(5, 5), Padding = new Eto.Drawing.Padding(5) };

            var tr = new TableRow { Cells = { l1, b1, b2 } };
            tr.Cells[0].ScaleWidth = true;

            tl.Rows.Add(tr);

            this.BeginVertical();
            this.Add(tl, true);
            this.Add(TabControl, true, true);
            this.BeginVertical();

        }

    }

}