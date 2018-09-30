using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.UI.Desktop.Shared.Forms
{
    partial class SolvingFlowsheet : Dialog
    {

        public Spinner progressSpinner;
        public Label lblMessage;
        public Button btnAbort;

        private double sf = GlobalSettings.Settings.UIScalingFactor;
                
        void InitializeComponent()
        {
            
            progressSpinner = new Spinner { Width = (int)(sf*80), Height = (int)(sf * 80), Enabled = true };

            lblMessage = new Label { Text = "Solving flowsheet, please wait...\n", Height = (int)(sf * 50)};

            lblMessage.VerticalAlignment = VerticalAlignment.Center;
            lblMessage.TextAlignment = TextAlignment.Center;

            btnAbort = new Button { Text = "Abort", Height = (int)(sf * 30)};

            var row1 = new TableLayout { Rows = { new TableRow(null, progressSpinner, null), null } };
            var container = new TableLayout { Rows = { row1, lblMessage, btnAbort, null }, Spacing = new Size(5, 5), Padding = new Padding((int)(sf * 25), (int)(sf * 10), (int)(sf * 25), (int)(sf * 10)) };

            Content = container;

            WindowStyle = Eto.Forms.WindowStyle.None;

            Topmost = true;

            ShowInTaskbar = false;

            Maximizable = false;
            Minimizable = false;

        }
    }
}
