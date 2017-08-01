using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.UI.Forms.Forms
{
    partial class LoadingData : Form
    {
        void InitializeComponent()
        {

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Title = "Loading...";

            var progress = new ProgressBar { Indeterminate = true};

            var label = new Label { Text = "Please wait, loading data..." };
            
            label.VerticalAlignment = VerticalAlignment.Center;
            label.TextAlignment = TextAlignment.Center;

            var container = new TableLayout { Rows = { progress, label }, Spacing = new Size(5, 5), Padding = new Padding(5, 5, 5, 5) };

            Content = container;

            WindowStyle = Eto.Forms.WindowStyle.Default;

            Topmost = true;

            ShowInTaskbar = false;

            Maximizable = false;
            Minimizable = false;

            int w = 300;
            int h = 60;

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= w / 2;
            center.Y -= h / 2;

            Location = new Point(center);

            ClientSize = new Size(w, h);

        }
    }
}
