using System;
using Eto.Forms;
using Eto.Drawing;
using DWSIM.UI.Forms;
using System.Threading;

namespace DWSIM.UI
{
    partial class MainForm : Form
    {

        void InitializeComponent()
        {
            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";
            
            Title = "DWSIM Launcher";
            ClientSize = new Size(490, 240);
            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var bgcolor = new Color(0.051f, 0.447f, 0.651f);

            Eto.Style.Add<Button>("main", button => { button.BackgroundColor = bgcolor;
                                                      button.Font = new Font(FontFamilies.Sans, 12f, FontStyle.None);
                                                      button.TextColor = Colors.White;
                                                      button.ImagePosition = ButtonImagePosition.Left;
                                                      button.Width = 230;
                                                      
                                                    });

            Content = new TableLayout
            {
                Padding = 10,
                Spacing = new Size(5, 5),
                Rows =
                {
                    new TableRow(new Button(){ Style = "main", Text = "Open Saved File", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "OpenFolder_100px.png"), 40, 40, ImageInterpolation.Default)},
                                new Button(){ Style = "main", Text = "New Simulation", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Workflow_100px.png"), 40, 40, ImageInterpolation.Default)}, null),
                    new TableRow(new Button(){ Style = "main", Text = "New Compound", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Peptide_100px.png"), 40, 40, ImageInterpolation.Default)},
                                new Button(){ Style = "main", Text = "New Data Regression", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "AreaChart_100px.png"), 40, 40, ImageInterpolation.Default)}, null),
                    new TableRow(new Button(){ Style = "main", Text = "Settings", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "VerticalSettingsMixer_100px.png"), 40, 40, ImageInterpolation.Default)},
                                new Button(){ Style = "main", Text = "Help", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Help_100px.png"), 40, 40, ImageInterpolation.Default)}, null),
                    new TableRow(new Button(){ Style = "main", Text = "About", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Info_100px.png"), 40, 40, ImageInterpolation.Default)},
                                new Button(){ Style = "main", Text = "Donate", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Donate_100px.png"), 40, 40, ImageInterpolation.Default)}, null),
                    null
                },
                BackgroundColor = bgcolor,
            };

            var quitCommand = new Command { MenuText = "Quit", Shortcut = Application.Instance.CommonModifier | Keys.Q };
            quitCommand.Executed += (sender, e) => Application.Instance.Quit();

            var aboutCommand = new Command { MenuText = "About..." };
            aboutCommand.Executed += (sender, e) => new About().Show();

            // create menu
            Menu = new MenuBar
            {
               ApplicationItems =
                    {
					    // application (OS X) or file menu (others)
					    new ButtonMenuItem { Text = "&Preferences..." },
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