using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;

using Eto.Forms;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables;
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors.Tables
{
    public class SpreadsheetTableEditor : Dialog
    {

        public SpreadsheetTableGraphic Table;

        public int mode = 0;

        private bool initialized = false;

        public Button btnOK;

        public SpreadsheetTableEditor()
        {
            Init();
        }

        void Init()
        {

            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            Title = "Configure Linked Spreadsheet Table";

            var container = new TableLayout();

            var topcontainer = new TableLayout();
           
            btnOK = new Button { Text = "Close", Enabled = true };

            btnOK.Click += (sender, e) =>
            {
                Close();
            };

            var header = new TextBox();
            header.TextChanged += (sender, e) =>
            {
                Table.SpreadsheetCellRange = header.Text;
            };

            topcontainer.Rows.Add(new TableRow(new Label { Text = "Cell Range" }, header));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(topcontainer));

            container.Padding = new Padding(5, 5, 5, 5);

            Content = container;

            Load += (sender, e) =>
            {
                header.Text = Table.SpreadsheetCellRange;
            };

            initialized = true;


        }

    }
}
