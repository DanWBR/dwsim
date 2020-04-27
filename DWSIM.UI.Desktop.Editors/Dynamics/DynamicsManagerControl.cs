using Eto.Drawing;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Editors.Dynamics
{

    public class DynamicsManagerControl : TableLayout
    {
        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        public CheckBox chkDynamics;

        private Shared.Flowsheet Flowsheet;

        public DynamicsManagerControl(Shared.Flowsheet fs) : base()
        {
            Flowsheet = fs;
        }

        public void Init()
        {

            var tl1 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr1 = new TableRow();

            chkDynamics = new CheckBox { Text = "Dynamic Mode Enabled" };

            tr1.Cells.Add(chkDynamics);

            tr1.Cells.Add(null);

            var btnShowIntegrator = new Button { Text = "Show Integrator Controls" };

            tr1.Cells.Add(btnShowIntegrator);

            tl1.Rows.Add(tr1);

            Rows.Add(new TableRow(tl1));

            var DocumentContainer = new DocumentControl() { AllowReordering = false, DisplayArrows = false };

            DocumentContainer.Pages.Add(new DocumentPage { Text = "Model Status", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Event Sets", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Cause-and-Effect Matrices", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Integrators", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Schedules", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Controllers", Closable = false });
            DocumentContainer.Pages.Add(new DocumentPage { Text = "Indicators", Closable = false });

            var tl2 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr2 = new TableRow();

            tr2.Cells.Add(DocumentContainer);

            tl2.Rows.Add(tr2);

            Rows.Add(new TableRow(tl2));

            // model status

            var l1 = new PixelLayout();

            var pb1 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };
            var pb2 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };
            var pb3 = new ImageView { Height = 40, Width = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-ok.png"), 40, 40, ImageInterpolation.Default) };

            var label1 = new Label { Height = 40, Text = "All Endpoint Material Streams are connected to Control Valves", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };
            var label2 = new Label { Height = 40, Text = "All Control Valves are configured with Kv Calculation Modes", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };
            var label3 = new Label { Height = 40, Text = "All Unit Operations in Flowsheet support Dynamic Mode", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };

            l1.Add(pb1, 20, 20);
            l1.Add(pb2, 20, 70);
            l1.Add(pb3, 20, 120);

            l1.Add(label1, 70, 20);
            l1.Add(label2, 70, 70);
            l1.Add(label3, 70, 120);

            DocumentContainer.Pages[0].Content = l1;

            // event sets

            var lce = new TableLayout();
            var rce = new TableLayout();

            var btnAddEventSet = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Add New", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-plus_math.png")).WithSize(16, 16) };
            var btnRemoveEventSet = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Remove Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-delete.png")).WithSize(16, 16) };

            var lbEventSets = new ListBox();

            lce.Rows.Add(new Label { Text = "Event Sets", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            var menu1 = new StackLayout
            {
                Items = { btnAddEventSet, btnRemoveEventSet },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Bottom,
                Padding = 5,
                Height = 34
            };
            lce.Rows.Add(new TableRow(menu1));

            lce.Rows.Add(new TableRow(lbEventSets));
            lce.Padding = new Padding(5, 5, 5, 5);
            lce.Spacing = new Size(0, 0);
            lce.Width = 250;

            rce.Rows.Add(new Label { Text = "Selected Event Set", Font = new Font(SystemFont.Bold, UI.Shared.Common.GetEditorFontSize()), Height = 30, VerticalAlignment = VerticalAlignment.Center });

            var btnAddEvent = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Add New", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-plus_math.png")).WithSize(16, 16) };
            var btnRemoveEvent = new Button() { ImagePosition = ButtonImagePosition.Overlay, Height = 24, Width = 24, ToolTip = "Remove Selected", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-delete.png")).WithSize(16, 16) };

            var menu2 = new StackLayout
            {
                Items = { btnAddEvent, btnRemoveEvent },
                Orientation = Orientation.Horizontal,
                Spacing = 4,
                HorizontalContentAlignment = HorizontalAlignment.Stretch,
                VerticalContentAlignment = VerticalAlignment.Bottom,
                Padding = 5,
                Height = 34
            };
            rce.Rows.Add(new TableRow(menu2));

            rce.Rows.Add(new TableRow(new Panel()));
            rce.Padding = new Padding(5, 5, 5, 5);

            var splites = new Splitter() { };
            splites.Panel1 = lce;
            splites.Panel1.Width = 250;
            splites.Panel2 = rce;
            splites.SplitterWidth = 2;

            DocumentContainer.Pages[1].Content = splites;

        }

    }
}
