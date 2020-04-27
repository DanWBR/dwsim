using Eto.Drawing;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Editors.Dynamics
{
    public class DynamicsIntegratorControl : TableLayout
    {

        public DropDown cbsc;

        string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

        private Shared.Flowsheet Flowsheet;

        public DynamicsIntegratorControl(DWSIM.UI.Desktop.Shared.Flowsheet fs)
            : base()
        {
            Flowsheet = fs;
        }

        public void Init()
        {

            var tl1 = new TableLayout {Padding = new Padding(5), Spacing = new Size(10, 10)};

            var tr1 = new TableRow();

            tr1.Cells.Add(new Label { Text = "Schedule", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center });

            cbsc = new DropDown { Width = 300 };

            foreach (var sch in Flowsheet.DynamicsManager.ScheduleList.Values)
            {
                cbsc.Items.Add(sch.Description);
            }

            if (cbsc.Items.Count > 0) cbsc.SelectedIndex = 0;

            tr1.Cells.Add(cbsc);
            
            tr1.Cells.Add(null);

            var btnViewResults = new Button {Text = "View Results" };

            tr1.Cells.Add(btnViewResults);

            tl1.Rows.Add(tr1);

            Rows.Add(new TableRow(tl1));

            var tl2 = new TableLayout { Padding = new Padding(5), Spacing = new Size(10, 10) };

            var tr2 = new TableRow();

            var btnPlay = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-play.png"), 40, 40, ImageInterpolation.Default) };
            var btnRT = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-realtime.png"), 40, 40, ImageInterpolation.Default) };
            var btnStop = new Button { ImagePosition = ButtonImagePosition.Overlay, Text = "", Width = 40, Height = 40, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-stop.png"), 40, 40, ImageInterpolation.Default) };

            var pbProgress = new ProgressBar { MinValue = 0, MaxValue = 100 };

            var lbStatus = new Label { Text = "00:00:00/00:30:00", Font = new Font(SystemFont.Default, UI.Shared.Common.GetEditorFontSize()), VerticalAlignment = VerticalAlignment.Center };

            tr2.Cells.Add(btnPlay);
            tr2.Cells.Add(btnRT);
            tr2.Cells.Add(btnStop);
            tr2.Cells.Add(lbStatus);
            tr2.Cells.Add(pbProgress);

            tl2.Rows.Add(tr2);

            Rows.Add(new TableRow(tl2));

            Rows.Add(null);

        }

    }
}
