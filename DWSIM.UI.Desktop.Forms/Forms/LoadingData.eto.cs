using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.UI.Forms.Forms
{
    partial class LoadingData : Form
    {

        public Label loadingtext;

        private double sf = GlobalSettings.Settings.UIScalingFactor;

        void InitializeComponent()
        {

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Title = "Loading...";

            var progressSpinner = new Spinner { Width = (int)(sf*80), Height = (int)(sf * 80), Enabled = true };

            loadingtext = new Label { Text = "Please wait, loading data..." };

            loadingtext.VerticalAlignment = VerticalAlignment.Center;
            loadingtext.TextAlignment = TextAlignment.Center;

            var row1 = new TableLayout { Rows = { new TableRow(null, progressSpinner, null), null } };
            var container = new TableLayout { Rows = { row1, loadingtext, null }, Spacing = new Size(5, 5), Padding = new Padding(25, 10, 25, 10) };

            Content = container;

            WindowStyle = Eto.Forms.WindowStyle.Default;

            Topmost = true;

            ShowInTaskbar = false;

            Maximizable = false;
            Minimizable = false;

            int w = (int)(sf * 400);
            int h = (int)(sf * 160);

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= w / 2;
            center.Y -= h / 2;

            Location = new Point(center);

            ClientSize = new Size(w, h);

        }
    }
}
