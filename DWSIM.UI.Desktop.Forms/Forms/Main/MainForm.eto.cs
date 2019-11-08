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
using DWSIM.Interfaces;
using System.Reflection;
using System.Text;
using s = DWSIM.GlobalSettings.Settings;
using c = DWSIM.UI.Shared.Common;

namespace DWSIM.UI
{
    partial class MainForm : Form
    {

        private int OpenForms = 0;

        public List<ConstantProperties> UserCompounds = new List<ConstantProperties>();

        public List<IUtilityPlugin5> plugins = new List<IUtilityPlugin5>();

        ListBox SampleList, FoldersList, FOSSEEList;

        TreeGridView MostRecentList;

        private TableLayout TableContainer;

        string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

        void InitializeComponent()
        {

            //exception handling

            var sf = s.UIScalingFactor;

            Application.Instance.UnhandledException += (sender, e) =>
            {
                new DWSIM.UI.Desktop.Editors.UnhandledExceptionView((Exception)e.ExceptionObject).ShowModalAsync();
            };

            Title = "DWSIMLauncher".Localize();

            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Windows:
                    ClientSize = new Size((int)(690 * sf), (int)(420 * sf));
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                    ClientSize = new Size((int)(690 * sf), (int)(370 * sf));
                    break;
                case GlobalSettings.Settings.Platform.Mac:
                    ClientSize = new Size((int)(690 * sf), (int)(350 * sf));
                    break;
            }

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var bgcolor = new Color(0.051f, 0.447f, 0.651f);

            if (s.DarkMode) bgcolor = SystemColors.ControlBackground;

            Eto.Style.Add<Button>("main", button =>
            {
                button.BackgroundColor = bgcolor;
                button.Font = new Font(FontFamilies.Sans, 12f, FontStyle.None);
                button.TextColor = Colors.White;
                button.ImagePosition = ButtonImagePosition.Left;
                button.Width = (int)(sf * 250);
                button.Height = (int)(sf * 50);
            });

            Eto.Style.Add<Button>("donate", button =>
            {
                button.BackgroundColor = !s.DarkMode ? Colors.LightYellow : SystemColors.ControlBackground;
                button.Font = new Font(FontFamilies.Sans, 12f, FontStyle.None);
                button.TextColor = !s.DarkMode ? bgcolor : Colors.White;
                button.ImagePosition = ButtonImagePosition.Left;
                button.Width = (int)(sf * 250);
                button.Height = (int)(sf * 50);
            });

            var btn1 = new Button() { Style = "main", Text = "OpenSavedFile".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "OpenFolder_100px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn2 = new Button() { Style = "main", Text = "NewSimulation".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn3 = new Button() { Style = "main", Text = "NewCompound".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Peptide_100px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn4 = new Button() { Style = "main", Text = "NewDataRegression".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "AreaChart_100px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn6 = new Button() { Style = "main", Text = "User Guide".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Help_100px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn7 = new Button() { Style = "main", Text = "About".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "Info_100px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn8 = new Button() { Style = "donate", Text = "Become a Patron", Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-patreon.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };
            var btn9 = new Button() { Style = "main", Text = "Preferences".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "ReportCard_96px.png"), (int)(sf * 40), (int)(sf * 40), ImageInterpolation.Default) };

            btn9.Click += (sender, e) =>
            {
                new Forms.Forms.GeneralSettings().GetForm().Show();
            };

            btn6.Click += (sender, e) =>
            {
                var basepath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                Process.Start(basepath + Path.DirectorySeparatorChar + "docs" + Path.DirectorySeparatorChar + "user_guide.pdf");
            };

            btn1.Click += (sender, e) =>
            {
                var dialog = new OpenFileDialog();
                dialog.Title = "Open File".Localize();
                dialog.Filters.Add(new FileFilter("XML Simulation File".Localize(), new[] { ".dwxml", ".dwxmz" }));
                dialog.Filters.Add(new FileFilter("Mobile XML Simulation File (Android/iOS)", new[] { ".xml" }));
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
                form.newsim = true;
                form.Show();
            };

            btn7.Click += (sender, e) => new AboutBox().Show();
            btn8.Click += (sender, e) => Process.Start("https://patreon.com/dwsim");

            var stack = new StackLayout { Orientation = Orientation.Vertical, Spacing = 5 };
            stack.Items.Add(btn1);
            stack.Items.Add(btn2);
            stack.Items.Add(btn9);
            stack.Items.Add(btn6);
            stack.Items.Add(btn7);
            stack.Items.Add(btn8);

            var tableright = new TableLayout();
            tableright.Padding = new Padding(5, 5, 5, 5);
            tableright.Spacing = new Size(10, 10);

            MostRecentList = new TreeGridView { Height = (int)(sf * 330) };
            SampleList = new ListBox { BackgroundColor = bgcolor, Height = (int)(sf * 330) };
            FoldersList = new ListBox { BackgroundColor = bgcolor, Height = (int)(sf * 330) };
            FOSSEEList = new ListBox { BackgroundColor = bgcolor, Height = (int)(sf * 330) };

            if (Application.Instance.Platform.IsGtk &&
                GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                SampleList.TextColor = bgcolor;
                FoldersList.TextColor = bgcolor;
                FOSSEEList.TextColor = bgcolor;
            }
            else if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                SampleList.BackgroundColor = SystemColors.ControlBackground;
                FoldersList.BackgroundColor = SystemColors.ControlBackground;
                FOSSEEList.BackgroundColor = SystemColors.ControlBackground;
                SampleList.TextColor = SystemColors.ControlText;
                FoldersList.TextColor = SystemColors.ControlText;
                FOSSEEList.TextColor = SystemColors.ControlText;
            }
            else
            {
                SampleList.TextColor = Colors.White;
                FoldersList.TextColor = Colors.White;
                FOSSEEList.TextColor = Colors.White;
            }

            MostRecentList.AllowMultipleSelection = false;
            MostRecentList.ShowHeader = true;
            MostRecentList.Columns.Clear();
            MostRecentList.Columns.Add(new GridColumn { DataCell = new ImageViewCell(0) { ImageInterpolation = ImageInterpolation.High } });
            MostRecentList.Columns.Add(new GridColumn { DataCell = new TextBoxCell(1), HeaderText = "Name", Sortable = true });
            MostRecentList.Columns.Add(new GridColumn { DataCell = new TextBoxCell(2), HeaderText = "Date", Sortable = true });
            MostRecentList.Columns.Add(new GridColumn { DataCell = new TextBoxCell(3), HeaderText = "DWSIM Version", Sortable = true });
            MostRecentList.Columns.Add(new GridColumn { DataCell = new TextBoxCell(4), HeaderText = "Operating System", Sortable = true });

            var invertedlist = new List<string>(GlobalSettings.Settings.MostRecentFiles);
            invertedlist.Reverse();

            var tgc = new TreeGridItemCollection();

            foreach (var item in invertedlist)
            {
                if (File.Exists(item))
                {
                    var li = new TreeGridItem();
                    var data = new Dictionary<string, string>();
                    if (Path.GetExtension(item).ToLower() == ".dwxmz")
                    {
                        data = SharedClasses.Utility.GetSimulationFileDetails(FlowsheetBase.FlowsheetBase.LoadZippedXMLDoc(item));
                    }
                    else
                    {
                        data = SharedClasses.Utility.GetSimulationFileDetails(XDocument.Load(item));
                    }
                    li.Tag = data;
                    data.Add("Path", item);
                    DateTime dt;
                    if (data.ContainsKey("SavedOn"))
                    {
                        dt = DateTime.Parse(data["SavedOn"]);
                    }
                    else
                    {
                        dt = File.GetLastWriteTime(item);
                    }
                    string dwsimver, osver;
                    if (data.ContainsKey("DWSIMVersion"))
                    {
                        dwsimver = data["DWSIMVersion"];
                    }
                    else
                    {
                        dwsimver = "N/A";
                    }
                    if (data.ContainsKey("OSInfo"))
                    {
                        osver = data["OSInfo"];
                    }
                    else
                    {
                        osver = "N/A";
                    }
                    li.Values = new object[] {new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")).WithSize(16, 16),
                            System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(Path.GetFileNameWithoutExtension(item)),
                            dt, dwsimver, osver};
                    tgc.Add(li);
                }
            }

            tgc = new TreeGridItemCollection(tgc.OrderByDescending(x => ((DateTime)((TreeGridItem)x).Values[2]).Ticks));

            MostRecentList.DataStore = tgc;

            IEnumerable<string> samplist = new List<string>();
            try
            {
                samplist = Directory.EnumerateFiles(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "samples"), "*.dwxm*");
            }
            catch
            { }

            foreach (var item in samplist)
            {
                if (File.Exists(item)) SampleList.Items.Add(new ListItem { Text = Path.GetFileNameWithoutExtension(item), Key = item });
            }

            foreach (String f in invertedlist)
            {
                if (Path.GetExtension(f).ToLower() != ".dwbcs")
                {
                    if (FoldersList.Items.Where((x) => x.Text == Path.GetDirectoryName(f)).Count() == 0)
                    {
                        if (Directory.Exists(Path.GetDirectoryName(f)))
                        {
                            FoldersList.Items.Add(new ListItem { Text = Path.GetDirectoryName(f), Key = Path.GetDirectoryName(f) });
                        }
                    }
                }
            }

            FOSSEEList.Items.Add(new ListItem { Text = "Downloading flowsheet list, please wait...", Key = "" });

            Dictionary<string, SharedClasses.FOSSEEFlowsheet> fslist = new Dictionary<string, SharedClasses.FOSSEEFlowsheet>();

            Task.Factory.StartNew(() =>
            {
                return SharedClasses.FOSSEEFlowsheets.GetFOSSEEFlowsheets();
            }).ContinueWith((t) =>
            {
                Application.Instance.Invoke(() =>
                {
                    FOSSEEList.Items.Clear();
                    if (t.Exception != null)
                    {
                        FOSSEEList.Items.Add(new ListItem { Text = "Error loading flowsheet list. Check your internet connection.", Key = "" });
                    }
                    else
                    {
                        foreach (var item in t.Result)
                        {
                            fslist.Add(item.DownloadLink, item);
                            FOSSEEList.Items.Add(new ListItem { Text = item.DisplayName, Key = item.DownloadLink });
                        }
                    }
                });
            });

            FoldersList.SelectedIndexChanged += (sender, e) =>
            {
                if (FoldersList.SelectedIndex >= 0)
                {
                    var dialog = new OpenFileDialog();
                    dialog.Title = "Open File".Localize();
                    dialog.Directory = new Uri(FoldersList.SelectedKey);
                    dialog.Filters.Add(new FileFilter("XML Simulation File".Localize(), new[] { ".dwxml", ".dwxmz" }));
                    dialog.MultiSelect = false;
                    dialog.CurrentFilterIndex = 0;
                    if (dialog.ShowDialog(this) == DialogResult.Ok)
                    {
                        LoadSimulation(dialog.FileName);
                    }
                }
            };

            MostRecentList.SelectedItemChanged += (sender, e) =>
            {
                if (MostRecentList.SelectedItem != null)
                {
                    var si = (TreeGridItem)MostRecentList.SelectedItem;
                    var data = (Dictionary<string, string>)si.Tag;
                    LoadSimulation(data["Path"]);
                    //MostRecentList.SelectedIndex = -1;
                };
            };

            SampleList.SelectedIndexChanged += (sender, e) =>
            {
                if (SampleList.SelectedIndex >= 0)
                {
                    LoadSimulation(SampleList.SelectedKey);
                    //MostRecentList.SelectedIndex = -1;
                };
            };

            FOSSEEList.SelectedIndexChanged += (sender, e) =>
            {
                if (FOSSEEList.SelectedIndex >= 0 && FOSSEEList.SelectedKey != "")
                {
                    var item = fslist[FOSSEEList.SelectedKey];
                    var sb = new StringBuilder();
                    sb.AppendLine("Title: " + item.Title);
                    sb.AppendLine("Author: " + item.ProposerName);
                    sb.AppendLine("Institution: " + item.Institution);
                    sb.AppendLine();
                    sb.AppendLine("Click 'Yes' to download and open this flowsheet.");

                    if (MessageBox.Show(sb.ToString(), "Open FOSSEE Flowsheet", MessageBoxButtons.YesNo, MessageBoxType.Information, MessageBoxDefaultButton.Yes) == DialogResult.Yes)
                    {
                        var loadingdialog = new LoadingData();
                        loadingdialog.loadingtext.Text = "Please wait, downloading file...\n(" + FOSSEEList.SelectedKey + ")";
                        loadingdialog.Show();
                        var address = FOSSEEList.SelectedKey;
                        Task.Factory.StartNew(() =>
                        {
                            return SharedClasses.FOSSEEFlowsheets.DownloadFlowsheet(address, (p) =>
                            {
                                Application.Instance.Invoke(() => loadingdialog.loadingtext.Text = "Please wait, downloading file... (" + p + "%)\n(" + address + ")");
                            });
                        }).ContinueWith((t) =>
                        {
                            Application.Instance.Invoke(() => loadingdialog.Close());
                            if (t.Exception != null)
                            {
                                MessageBox.Show(t.Exception.Message, "Error downloading file", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                            }
                            else
                            {
                                Application.Instance.Invoke(() => LoadSimulation(SharedClasses.FOSSEEFlowsheets.LoadFlowsheet(t.Result)));
                            }
                        });
                        FOSSEEList.SelectedIndex = -1;
                    }
                };
            };

            var fosseecontainer = c.GetDefaultContainer();
            var l1 = c.CreateAndAddLabelRow3(fosseecontainer, "About the Project");
            var l2 = c.CreateAndAddDescriptionRow(fosseecontainer, "FOSSEE, IIT Bombay, invites chemical engineering students, faculty and practitioners to the flowsheeting project using DWSIM. We want you to convert existing flowsheets into DWSIM and get honoraria and certificates.");
            var bu1 = c.CreateAndAddButtonRow(fosseecontainer, "Submit a Flowsheet", null, (b1, e1) => Process.Start("https://dwsim.fossee.in/flowsheeting-project"));
            var bu2 = c.CreateAndAddButtonRow(fosseecontainer, "About FOSSEE", null, (b2, e2) => Process.Start("https://fossee.in/"));
            var l3 = c.CreateAndAddLabelRow3(fosseecontainer, "Completed Flowsheets");
            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                fosseecontainer.BackgroundColor = SystemColors.ControlBackground;
            }
            else
            {
                fosseecontainer.BackgroundColor = bgcolor;
                l1.TextColor = Colors.White;
                l2.TextColor = Colors.White;
                l3.TextColor = Colors.White;
                bu1.TextColor = Colors.White;
                bu2.TextColor = Colors.White;
                bu1.BackgroundColor = bgcolor;
                bu2.BackgroundColor = bgcolor;
            }
            fosseecontainer.Add(FOSSEEList);
            fosseecontainer.EndVertical();

            var tabview = new TabControl();
            var tab1 = new TabPage(MostRecentList) { Text = "Recent Files" }; ;
            var tab2 = new TabPage(SampleList) { Text = "Samples" }; ;
            var tab2a = new TabPage(fosseecontainer) { Text = "FOSSEE Flowsheets" }; ;
            var tab3 = new TabPage(FoldersList) { Text = "Recent Folders" }; ;
            tabview.Pages.Add(tab1);
            tabview.Pages.Add(tab2);
            tabview.Pages.Add(tab2a);
            tabview.Pages.Add(tab3);

            tableright.Rows.Add(new TableRow(tabview));

            var tl = new DynamicLayout();
            tl.Add(new TableRow(stack, tableright));

            TableContainer = new TableLayout
            {
                Padding = 10,
                Spacing = new Size(5, 5),
                Rows = { new TableRow(tl) }
            };

            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                TableContainer.BackgroundColor = SystemColors.ControlBackground;
            }
            else
            {
                TableContainer.BackgroundColor = bgcolor;
            }


            Content = TableContainer;

            var quitCommand = new Command { MenuText = "Quit".Localize(), Shortcut = Application.Instance.CommonModifier | Keys.Q };
            quitCommand.Executed += (sender, e) =>
            {
                try
                {
                    DWSIM.GlobalSettings.Settings.SaveSettings("dwsim_newui.ini");
                }
                catch (Exception ex)
                {
                    MessageBox.Show(this, ex.Message, "Error Saving Settings to File", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                }
                if (MessageBox.Show(this, "ConfirmAppExit".Localize(), "AppExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Information, MessageBoxDefaultButton.No) == DialogResult.Yes)
                {
                    Application.Instance.Quit();
                }
            };

            var aboutCommand = new Command { MenuText = "About".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "information.png")) };
            aboutCommand.Executed += (sender, e) => new AboutBox().Show();

            var aitem1 = new ButtonMenuItem { Text = "Preferences".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-sorting_options.png")) };
            aitem1.Click += (sender, e) =>
            {
                new Forms.Forms.GeneralSettings().GetForm().Show();
            };

            var hitem1 = new ButtonMenuItem { Text = "User Guide".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem1.Click += (sender, e) =>
            {
                var basepath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                Process.Start(basepath + Path.DirectorySeparatorChar + "docs" + Path.DirectorySeparatorChar + "user_guide.pdf");
            };

            var hitem2 = new ButtonMenuItem { Text = "Support".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem2.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Support");
            };

            var hitem3 = new ButtonMenuItem { Text = "Report a Bug".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem3.Click += (sender, e) =>
            {
                Process.Start("https://sourceforge.net/p/dwsim/tickets/");
            };

            var hitem4 = new ButtonMenuItem { Text = "Go to DWSIM's Website".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem4.Click += (sender, e) =>
            {
                Process.Start("http://dwsim.inforside.com.br");
            };

            // create menu
            Menu = new MenuBar
            {
                ApplicationItems = { aitem1 },
                QuitItem = quitCommand,
                HelpItems = { hitem1, hitem4, hitem2, hitem3 },
                AboutItem = aboutCommand
            };

            Shown += MainForm_Shown;

            Closing += MainForm_Closing;

            //Plugins

            LoadPlugins();

        }

        private void LoadPlugins()
        {
            //load plugins from 'Plugins' folder
            {
                List<Interfaces.IUtilityPlugin5> pluginlist = GetPlugins(LoadPluginAssemblies());
                foreach (Interfaces.IUtilityPlugin5 ip in pluginlist)
                {
                    plugins.Add(ip);
                }
            }
        }

        void MainForm_Closing(object sender, System.ComponentModel.CancelEventArgs e)
        {
            DWSIM.GlobalSettings.Settings.SaveSettings("dwsim_newui.ini");
            {
                if (MessageBox.Show(this, "ConfirmAppExit".Localize(), "AppExit".Localize(), MessageBoxButtons.YesNo, MessageBoxType.Information, MessageBoxDefaultButton.No) == DialogResult.No)
                {
                    e.Cancel = true;
                }
            }
        }

        void MainForm_Shown(object sender, EventArgs e)
        {
            Application.Instance.Invoke(() =>
            {
                switch (GlobalSettings.Settings.RunningPlatform())
                {
                    case GlobalSettings.Settings.Platform.Windows:
                        ClientSize = new Size((int)(s.UIScalingFactor * 700), (int)(s.UIScalingFactor * 400));
                        break;
                }
                var splash = new SplashScreen { MainFrm = this };
                splash.Show();
            });

        }

        void LoadSimulation(string path)
        {

            Forms.Flowsheet form = null;

            Application.Instance.Invoke(() =>
            {
                form = new Forms.Flowsheet();
            });

            OpenForms += 1;
            form.Closed += (sender2, e2) =>
            {
                OpenForms -= 1;
            };

            AddUserCompounds(form.FlowsheetObject);

            var loadingdialog = new LoadingData();
            loadingdialog.loadingtext.Text = "Please wait, loading data...\n(" + Path.GetFileNameWithoutExtension(path) + ")";
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
                else if (System.IO.Path.GetExtension(path).ToLower() == ".xml")
                {
                    form.FlowsheetObject.LoadFromMXML(XDocument.Load(path));
                }
                form.FlowsheetObject.FilePath = path;
                form.FlowsheetObject.FlowsheetOptions.FilePath = path;
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
                        GlobalSettings.Settings.MostRecentFiles.Add(path);
                        var ds = (TreeGridItemCollection)MostRecentList.DataStore;
                        var li = new TreeGridItem();
                        var data = new Dictionary<string, string>();
                        if (Path.GetExtension(path).ToLower() == ".dwxmz")
                        {
                            data = SharedClasses.Utility.GetSimulationFileDetails(FlowsheetBase.FlowsheetBase.LoadZippedXMLDoc(path));
                        }
                        else
                        {
                            data = SharedClasses.Utility.GetSimulationFileDetails(XDocument.Load(path));
                        }
                        li.Tag = data;
                        data.Add("Path", path);
                        DateTime dt;
                        if (data.ContainsKey("SavedOn"))
                        {
                            dt = DateTime.Parse(data["SavedOn"]);
                        }
                        else
                        {
                            dt = File.GetLastWriteTime(path);
                        }
                        string dwsimver, osver;
                        if (data.ContainsKey("DWSIMVersion"))
                        {
                            dwsimver = data["DWSIMVersion"];
                        }
                        else
                        {
                            dwsimver = "N/A";
                        }
                        if (data.ContainsKey("OSInfo"))
                        {
                            osver = data["OSInfo"];
                        }
                        else
                        {
                            osver = "N/A";
                        }
                        li.Values = new object[] {new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "icons8-workflow.png")).WithSize(16, 16),
                            System.Globalization.CultureInfo.CurrentCulture.TextInfo.ToTitleCase(Path.GetFileNameWithoutExtension(path)),
                            dt, dwsimver, osver};
                        ds.Add(li);
                        MostRecentList.ReloadData();
                    }
                    form.FlowsheetObject.ProcessScripts(Interfaces.Enums.Scripts.EventType.SimulationOpened, Interfaces.Enums.Scripts.ObjectType.Simulation, "");
                });
            });
        }

        void LoadSimulation(XDocument xdoc)
        {

            Forms.Flowsheet form = null;

            Application.Instance.Invoke(() =>
            {
                form = new Forms.Flowsheet();
            });

            OpenForms += 1;
            form.Closed += (sender2, e2) =>
            {
                OpenForms -= 1;
            };

            AddUserCompounds(form.FlowsheetObject);

            var loadingdialog = new LoadingData();
            loadingdialog.loadingtext.Text = "Please wait, loading data...";
            loadingdialog.Show();

            Task.Factory.StartNew(() =>
            {
                form.FlowsheetObject.LoadFromXML(xdoc);
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

        //plugins

        public List<Interfaces.IUtilityPlugin5> GetPlugins(List<Assembly> alist)
        {

            List<Type> availableTypes = new List<Type>();

            foreach (Assembly currentAssembly in alist)
            {
                try
                {
                    availableTypes.AddRange(currentAssembly.GetTypes());
                }
                catch
                {
                }
            }

            List<Type> pluginlist = availableTypes.FindAll((t) =>
            {
                List<Type> interfaceTypes = new List<Type>(t.GetInterfaces());
                return (interfaceTypes.Contains(typeof(Interfaces.IUtilityPlugin5)));
            });

            return pluginlist.ConvertAll<Interfaces.IUtilityPlugin5>((Type t) => Activator.CreateInstance(t) as Interfaces.IUtilityPlugin5);

        }

        private List<Assembly> LoadPluginAssemblies()
        {

            List<Assembly> pluginassemblylist = new List<Assembly>();


            if (Directory.Exists(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "plugins")))
            {
                DirectoryInfo dinfo = new DirectoryInfo(Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "plugins"));

                FileInfo[] files = dinfo.GetFiles("*.*", SearchOption.TopDirectoryOnly);

                if ((files != null))
                {
                    foreach (FileInfo fi in files)
                    {
                        if (fi.Extension.ToLower() == ".exe" | fi.Extension.ToLower() == ".dll")
                        {
                            try
                            {
                                pluginassemblylist.Add(Assembly.LoadFile(fi.FullName));

                            }
                            catch
                            {
                            }

                        }
                    }
                }

            }

            return pluginassemblylist;

        }

    }
}