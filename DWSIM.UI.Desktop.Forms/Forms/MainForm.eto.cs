using System;
using Eto.Forms;
using Eto.Drawing;
using DWSIM.UI.Forms;
using System.Threading;
using System.Diagnostics;
using System.Linq;
using DWSIM.UI.Forms.Forms;
using System.Xml.Linq;
using System.Threading.Tasks;
using System.IO;
using System.Collections.Generic;
using DWSIM.Thermodynamics.BaseClasses;

namespace DWSIM.UI
{
    partial class MainForm : Form
    {

        private int OpenForms = 0;

        public List<ConstantProperties> UserCompounds = new List<ConstantProperties>();

        ListBox MostRecentList;

        void InitializeComponent()
        {

            //exception handling

            Application.Instance.UnhandledException += (sender, e) =>
            {
                new DWSIM.UI.Desktop.Editors.UnhandledExceptionView((Exception)e.ExceptionObject).ShowModalAsync();
            };

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Title = "DWSIMLauncher".Localize();

            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Windows:
                    ClientSize = new Size(660, 390);
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                    ClientSize = new Size(660, 365);
                    break;
                case GlobalSettings.Settings.Platform.Mac:
                    ClientSize = new Size(660, 350);
                    break;
            }

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var bgcolor = new Color(0.051f, 0.447f, 0.651f);

            Eto.Style.Add<Button>("main", button =>
            {
                button.BackgroundColor = bgcolor;
                button.Font = new Font(FontFamilies.Sans, 12f, FontStyle.None);
                button.TextColor = Colors.White;
                button.ImagePosition = ButtonImagePosition.Left;
                button.Width = 230;
            });

            var btn1 = new Button() { Style = "main", Text = "OpenSavedFile".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "OpenFolder_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn2 = new Button() { Style = "main", Text = "NewSimulation".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Workflow_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn3 = new Button() { Style = "main", Text = "NewCompound".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Peptide_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn4 = new Button() { Style = "main", Text = "NewDataRegression".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "AreaChart_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn5 = new Button() { Style = "main", Text = "OpenSamples".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "OpenBook_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn6 = new Button() { Style = "main", Text = "Help".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Help_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn7 = new Button() { Style = "main", Text = "About".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Info_100px.png"), 40, 40, ImageInterpolation.Default) };
            var btn8 = new Button() { Style = "main", Text = "Donate".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Donate_100px.png"), 40, 40, ImageInterpolation.Default) };

            btn5.Click += (sender, e) =>
            {
                var dialog = new OpenFileDialog();
                dialog.Title = "OpenSamples".Localize();
                dialog.Filters.Add(new FileFilter("XML Simulation File".Localize(), new[] { ".dwxml", ".dwxmz" }));
                dialog.MultiSelect = false;
                dialog.Directory = new Uri(Path.Combine(Directory.GetCurrentDirectory(), "samples"));
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(this) == DialogResult.Ok)
                {
                    LoadSimulation(dialog.FileName);
                }
            };

            btn6.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br/docs/crossplatform/help/");
            };

            btn1.Click += (sender, e) =>
            {
                var dialog = new OpenFileDialog();
                dialog.Title = "Open File".Localize();
                dialog.Filters.Add(new FileFilter("XML Simulation File".Localize(), new[] { ".dwxml", ".dwxmz" }));
                dialog.MultiSelect = false;
                dialog.CurrentFilterIndex = 0;
                if (dialog.ShowDialog(this) == DialogResult.Ok)
                {
                    LoadSimulation(dialog.FileName);
                }

            };

            btn2.Click += (sender, e) =>
            {
                var form = new Forms.Flowsheet();
                AddUserCompounds(form.FlowsheetObject);
                OpenForms += 1;
                form.Closed += (sender2, e2) =>
                {
                    OpenForms -= 1;
                };
                form.Show();
            };

            btn7.Click += (sender, e) => new About().Show();
            btn8.Click += (sender, e) => Process.Start("http://sourceforge.net/p/dwsim/donate/");

            var stack = new StackLayout { Orientation = Orientation.Vertical, Spacing = 5 };
            stack.Items.Add(btn1);
            stack.Items.Add(btn2);
            stack.Items.Add(btn5);
            stack.Items.Add(btn6);
            stack.Items.Add(btn7);
            stack.Items.Add(btn8);

            var tableright = new TableLayout();
            tableright.Padding = new Padding(5, 5, 5, 5);
            tableright.Spacing = new Size(10, 10);

            MostRecentList = new ListBox { BackgroundColor = bgcolor, TextColor = Colors.White };

            foreach (var item in GlobalSettings.Settings.MostRecentFiles)
            {
                if (File.Exists(item)) MostRecentList.Items.Add(new ListItem { Text = item, Key = item });
            }

            MostRecentList.SelectedIndexChanged += (sender, e) =>
            {
                if (MostRecentList.SelectedIndex >= 0)
                {
                    LoadSimulation(MostRecentList.SelectedKey);
                    MostRecentList.SelectedIndex = -1;
                };
            };


            tableright.Rows.Add(new TableRow(new Label { Text = "Recent Files", Font = SystemFonts.Bold(), TextColor = Colors.White }));
            tableright.Rows.Add(new TableRow(MostRecentList));

            Content = new TableLayout
            {
                Padding = 10,
                Spacing = new Size(5, 5),
                Rows = { new TableRow(stack, tableright) },
                BackgroundColor = bgcolor,
            };

            var quitCommand = new Command { MenuText = "Quit".Localize(), Shortcut = Application.Instance.CommonModifier | Keys.Q };
            quitCommand.Executed += (sender, e) => Application.Instance.Quit();

            var aboutCommand = new Command { MenuText = "About".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "information.png")) };
            aboutCommand.Executed += (sender, e) => new About().Show();

            var aitem1 = new ButtonMenuItem { Text = "Preferences".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")) };
            aitem1.Click += (sender, e) =>
            {
                new Forms.Forms.GeneralSettings().GetForm().Show();
            };

            var hitem1 = new ButtonMenuItem { Text = "Help".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem1.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br/docs/crossplatform/help/");
            };

            // create menu
            Menu = new MenuBar
            {
                ApplicationItems = { aitem1 },
                QuitItem = quitCommand,
                HelpItems = { hitem1 },
                AboutItem = aboutCommand
            };

            Shown += MainForm_Shown;

            Closing += MainForm_Closing;

        }

        void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            if (OpenForms > 0)
            {
                if (MessageBox.Show(this, "ConfirmAppExit".Localize(), "AppExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.No)
                {
                    e.Cancel = true;
                }
            }
            DWSIM.GlobalSettings.Settings.SaveSettings("dwsim_newui.ini");
        }

        void MainForm_Shown(object sender, EventArgs e)
        {
            Application.Instance.Invoke(() =>
            {
                var splash = new SplashScreen { MainFrm = this };
                splash.Show();
            });
        }

        void LoadSimulation(string path)
        {

            var form = new Forms.Flowsheet();

            OpenForms += 1;
            form.Closed += (sender2, e2) =>
            {
                OpenForms -= 1;
            };

            AddUserCompounds(form.FlowsheetObject);

            var loadingdialog = new LoadingData();
            loadingdialog.loadingtext.Text = "Please wait, loading data...\n(" + path + ")";
            loadingdialog.Show();

            Task.Factory.StartNew(() =>
            {
                if (System.IO.Path.GetExtension(path).ToLower() == ".dwxmz")
                {
                    var xdoc = form.FlowsheetObject.LoadZippedXML(path);
                }
                else if (System.IO.Path.GetExtension(path).ToLower() == ".dwxml")
                {
                    form.FlowsheetObject.LoadFromXML(XDocument.Load(path));
                }
                form.FlowsheetObject.FilePath = path;
            }).ContinueWith((t) =>
            {
                Application.Instance.Invoke(() =>
                {
                    loadingdialog.Close();
                    var surface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)form.FlowsheetObject.GetSurface();
                    surface.ZoomAll(ClientSize.Width, ClientSize.Height);
                    surface.ZoomAll(ClientSize.Width, ClientSize.Height);
                    form.FlowsheetObject.UpdateInterface();
                    form.Title = form.FlowsheetObject.Options.SimulationName + " [" + form.FlowsheetObject.Options.FilePath + "]";
                    form.Show();
                    if (!GlobalSettings.Settings.MostRecentFiles.Contains(path))
                    {
                        MostRecentList.Items.Add(new ListItem { Text = path, Key = path });
                        GlobalSettings.Settings.MostRecentFiles.Add(path);
                    }
                    form.FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationOpened, Interfaces.Enums.Scripts.ObjectType.Simulation, "");
                });
            });
        }

        void AddUserCompounds(FlowsheetBase.FlowsheetBase flowsheet)
        {

            foreach (var compound in UserCompounds)
            {
                if (!flowsheet.AvailableCompounds.ContainsKey(compound.Name)) flowsheet.AvailableCompounds.Add(compound.Name, compound);
            }

        }

    }
}