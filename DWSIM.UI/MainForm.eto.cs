using System;
using Eto.Forms;
using Eto.Drawing;
using DWSIM.UI.Forms;
using System.Threading;
using System.Diagnostics;
using DWSIM.UI.Forms.Shared;
using System.Linq;

namespace DWSIM.UI
{
    partial class MainForm : Form
    {

        void InitializeComponent()
        {
            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Title = "DWSIMLauncher".Localize();
            ClientSize = new Size(490, 260);
            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var bgcolor = new Color(0.051f, 0.447f, 0.651f);

            Eto.Style.Add<Button>("main", button => { button.BackgroundColor = bgcolor;
                                                      button.Font = new Font(FontFamilies.Sans, 12f, FontStyle.None);
                                                      button.TextColor = Colors.White;
                                                      button.ImagePosition = ButtonImagePosition.Left;
                                                      button.Width = 230;
                                                    });

            var btn1 = new Button(){ Style = "main", Text = "OpenSavedFile".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "OpenFolder_100px.png"), 40, 40, ImageInterpolation.Default)};
            var btn2 = new Button(){ Style = "main", Text = "NewSimulation".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Workflow_100px.png"), 40, 40, ImageInterpolation.Default)};
            var btn3 = new Button(){ Style = "main", Text = "NewCompound".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Peptide_100px.png"), 40, 40, ImageInterpolation.Default)};
            var btn4 =  new Button(){ Style = "main", Text = "NewDataRegression".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "AreaChart_100px.png"), 40, 40, ImageInterpolation.Default)};
            var btn5 = new Button() { Style = "main", Text = "Settings".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "VerticalSettingsMixer_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn6 = new Button(){ Style = "main", Text = "Help".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Help_100px.png"), 40, 40, ImageInterpolation.Default)};
            var btn7 = new Button() { Style = "main", Text = "About".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Info_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn8 = new Button() { Style = "main", Text = "Donate".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Donate_100px.png"), 40, 40, ImageInterpolation.Default) };

            btn5.Click += (sender, e) => {
                new Forms.Forms.GeneralSettings().GetForm().Show();
            };

            btn1.Click += (sender, e) => {
                var dialog = new OpenFileDialog();
                dialog.Title = "Open File".Localize();
                dialog.Filters.Add(new FileDialogFilter("XML Simulation File v5".Localize(), new[] { ".dwxz5" }));
                dialog.Filters.Add(new FileDialogFilter("XML Simulation File v4".Localize(), new[] { ".dwxml", ".dwxmz" }));
                dialog.Filters.Add(new FileDialogFilter("Compound Creator Project".Localize(), new[] { ".dwcsd" }));
                dialog.Filters.Add(new FileDialogFilter("Data Regression Project".Localize(), new[] { ".dwrsd" }));
                dialog.MultiSelect = false;
                dialog.ShowDialog(this);
            };

            btn7.Click += (sender, e) => new About().Show();
            btn8.Click += (sender, e) => Process.Start("http://sourceforge.net/p/dwsim/donate/");

            Content = new TableLayout
            {
                Padding = 10,
                Spacing = new Size(5, 5),
                Rows =
                {
                    new TableRow(btn1, btn2, null),
                    new TableRow(btn3, btn4, null),
                    new TableRow(btn5, btn6, null),
                    new TableRow(btn7, btn8, null),
                    null
                },
                BackgroundColor = bgcolor,
            };

            var quitCommand = new Command { MenuText = "Quit".Localize(), Shortcut = Application.Instance.CommonModifier | Keys.Q };
            quitCommand.Executed += (sender, e) => Application.Instance.Quit();

            var aboutCommand = new Command { MenuText = "About".Localize() };
            aboutCommand.Executed += (sender, e) => new About().Show();

            // create menu
            Menu = new MenuBar
            {
               ApplicationItems =
                    {
					    // application (OS X) or file menu (others)
					    new ButtonMenuItem { Text = "Preferences".Localize() },
                    },
                QuitItem = quitCommand,
                AboutItem = aboutCommand

            };

            Shown += MainForm_Shown;
                        
        }

        void MainForm_Shown(object sender, EventArgs e)
        {
            Application.Instance.Invoke(() =>
            {
                var splash = new SplashScreen();
                splash.Show();
            });
        }
             
    }
}