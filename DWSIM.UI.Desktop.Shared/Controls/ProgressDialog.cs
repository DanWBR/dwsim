using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;

namespace DWSIM.UI.Desktop.Shared.Controls
{
    public static class ProgressDialog
    {

        public static Dialog Show(Control owner, string message1, string message2, bool modal)
        {

            var dialog = new Dialog();

            var progressSpinner = new Spinner { Width = 80, Height = 80, Enabled = true };

            var loadingtext = new Label { Text = message1 };
            var loadingtext2 = new Label { Text = message2 };

            loadingtext.VerticalAlignment = VerticalAlignment.Center;
            loadingtext.TextAlignment = TextAlignment.Center;

            loadingtext2.VerticalAlignment = VerticalAlignment.Center;
            loadingtext2.TextAlignment = TextAlignment.Center;

            var row1 = new TableLayout { Rows = { new TableRow(null, progressSpinner, null), null } };
            var container = new TableLayout { Rows = { row1, loadingtext, loadingtext2, null }, Spacing = new Size(5, 5), Padding = new Padding(25, 10, 25, 10) };

            dialog.Content = container;

            dialog.WindowStyle = Eto.Forms.WindowStyle.None;

            dialog.Topmost = true;

            dialog.ShowInTaskbar = false;

            dialog.Maximizable = false;
            dialog.Minimizable = false;

            if (owner == null)
            {
                int w = 400;
                int h = 160;

                var center = Screen.PrimaryScreen.WorkingArea.Center;
                center.X -= w / 2;
                center.Y -= h / 2;

                dialog.Location = new Point(center);

                dialog.ClientSize = new Size(w, h);

            }

            if (modal) dialog.ShowModal(owner); else dialog.ShowModalAsync(owner);
            
            return dialog;

        }
    }

}
