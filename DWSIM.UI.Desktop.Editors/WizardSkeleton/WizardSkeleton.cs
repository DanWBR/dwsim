using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Eto.Drawing;
using Eto.Forms;
using c = DWSIM.UI.Shared.Common;

using cv = DWSIM.SharedClasses.SystemsOfUnits.Converter;
using DWSIM.Thermodynamics.Streams;
using DWSIM.UI.Desktop.Shared;

using DWSIM.ExtensionMethods;
using System.IO;

using s = DWSIM.GlobalSettings.Settings;
using System.Windows.Threading;

namespace DWSIM.UI.Desktop.Editors
{
    public class WizardPage : Form
    {

        public bool hasBackButton { get; set; }
        public bool hasNextButton { get; set; }
        public bool hasCancelButton { get; set; }
        public bool hasFinishButton { get; set; }

        public Button btnBack, btnNext, btnCancel, btnFinish;

        public Spinner footerSpinner;

        public Action backAction, nextAction, cancelAction, finishAction;

        public DynamicLayout ContentContainer;

        public string HeaderTitle = "";
        public string HeaderDescription = "";

        public string FooterText = "";

        public Label footerLabel;

        public WizardPage() : base()
        {

        }

        public void Init(int width, int height)
        {


            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            var container = new TableLayout();

            var topcontainer = new TableLayout();

            topcontainer.BackgroundColor = s.DarkMode ? SystemColors.ControlBackground : Colors.White;

            if (HeaderTitle != "") topcontainer.Rows.Add(new TableRow(new Label { Text = HeaderTitle, Font = SystemFonts.Bold() }));
            if (HeaderDescription != "") topcontainer.Rows.Add(new TableRow(new Label { Text = HeaderDescription }));

            var middlecontainer = new DynamicLayout();

            var footercontainer = new TableLayout();

            footercontainer.BackgroundColor = s.DarkMode ? SystemColors.ControlBackground : Colors.White;

            topcontainer.Padding = new Padding(15);
            topcontainer.Spacing = new Size(10, 10);

            middlecontainer.Padding = new Padding(5);
            middlecontainer.Spacing = new Size(10, 10);

            var buttons = new List<Button>();

            if (hasCancelButton)
            {
                btnCancel = new Button { Text = "Cancel" };
                if (cancelAction != null) btnCancel.Click += (sender, e) =>
                {
                    cancelAction.Invoke();
                };
                buttons.Add(btnCancel);
            }

            if (hasBackButton)
            {
                btnBack = new Button { Text = "Previous" };
                if (backAction != null) btnBack.Click += (sender, e) => backAction.Invoke();
                buttons.Add(btnBack);
            }

            if (hasNextButton)
            {
                btnNext = new Button { Text = "Next" };
                if (nextAction != null) btnNext.Click += (sender, e) => nextAction.Invoke();
                buttons.Add(btnNext);
            }

            if (hasFinishButton)
            {
                btnFinish = new Button { Text = "Finish" };
                if (finishAction != null) btnFinish.Click += (sender, e) =>
                {
                    finishAction.Invoke();
                };
                buttons.Add(btnFinish);
            }

            var tr = new TableRow();

            footerSpinner = new Spinner { Visible = false, Size = new Size((int)(s.UIScalingFactor * 20), (int)(s.UIScalingFactor * 20)) };
            tr.Cells.Add(footerSpinner);

            footerLabel = new Label { Text = FooterText, VerticalAlignment = VerticalAlignment.Center };
            tr.Cells.Add(footerLabel);

            tr.Cells.Add(null);

            foreach (var btn in buttons)
            {
                if (Application.Instance.Platform.IsWinForms) btn.Height = (int)(s.UIScalingFactor * 30);
                tr.Cells.Add(btn);
            }

            footercontainer.Rows.Add(tr);
            footercontainer.Padding = new Padding(15);
            footercontainer.Spacing = new Size(10, 10);
            if (Application.Instance.Platform.IsWinForms) footercontainer.Height = (int)(s.UIScalingFactor * 60);

            container.Rows.Add(new TableRow(topcontainer));
            container.Rows.Add(new TableRow(middlecontainer));
            if (Application.Instance.Platform.IsWinForms) container.Rows.Add(null);
            container.Rows.Add(new TableRow(footercontainer));

            if (Application.Instance.Platform.IsGtk) container.Rows.Add(null);

            container.Padding = new Padding(0);

            Content = container;

            ContentContainer = middlecontainer;

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= (int)(s.UIScalingFactor * width) / 2;
            center.Y -= ((int)(s.UIScalingFactor * height) + (int)(s.UIScalingFactor * 150)) / 2;

            Location = new Point(center);

            if (!s.OldUI) Topmost = true;

            Visible = true;
        }

    }
}
