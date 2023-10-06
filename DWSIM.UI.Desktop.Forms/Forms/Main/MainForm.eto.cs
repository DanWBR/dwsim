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
using DWSIM.ExtensionMethods;
using DWSIM.ExtensionMethods.Eto;

namespace DWSIM.UI
{
    partial class MainForm : Form
    {

        private int OpenForms = 0;

        public List<ConstantProperties> UserCompounds = new List<ConstantProperties>();

        public List<IUtilityPlugin5> plugins = new List<IUtilityPlugin5>();

        ListBox SampleList, FoldersList, FOSSEEList;

        TreeGridView MostRecentList;

        string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";
        string bmpprefix = "DWSIM.UI.Forms.Resources.Bitmaps.";

        private int width = 1024;

        private int height = 640;

        void InitializeComponent()
        {

            //exception handling

            var sf = s.UIScalingFactor;

            width = (int)(width * sf);

            height = (int)(height * sf);

            Application.Instance.UnhandledException += (sender, e) =>
            {
                new DWSIM.UI.Desktop.Editors.UnhandledExceptionView((Exception)e.ExceptionObject).ShowModalAsync();
            };

            Title = "DWSIMLauncher".Localize();

            ClientSize = new Size((int)(width * sf), (int)(height * sf));

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            if (Application.Instance.Platform.IsGtk)
            {
                BackgroundColor = Colors.White;
            }

            var abslayout = new PixelLayout();

            if (!GlobalSettings.Settings.DarkMode)
            {
                var background = new ImageView { Size = ClientSize, Image = new Bitmap(Bitmap.FromResource("DWSIM.UI.Forms.Resources.Bitmaps.background_welcome.png")) };
                abslayout.Add(background, 0, 0);
            }

            int dx = (int)(15 * sf), dy = (int)(15 * sf), dx2 = (int)(10 * sf), dy2 = (int)(10 * sf);

            float fsize1, fsize2;

            if (GlobalSettings.Settings.RunningPlatform() == s.Platform.Mac)
            {
                fsize1 = 12.0f;
                fsize2 = 10.0f;
            }
            else
            {
                fsize1 = 11.0f;
                fsize2 = 9.0f;
            }

            var boldfont = new Font(SystemFont.Bold, fsize1, FontDecoration.None);
            var regularfont = new Font(SystemFont.Default, fsize2, FontDecoration.None);
            var boldfont2 = new Font(SystemFont.Bold, fsize2, FontDecoration.None);
            var bfh = (int)boldfont.LineHeight;
            var rfh = (int)regularfont.LineHeight;

            var psize = new Size((int)(100 * sf), (int)(100 * sf));
            var psize2 = new Size((int)(80 * sf), (int)(80 * sf));
            var psize3 = new Size((int)(134 * sf), (int)(100 * sf));
            var lsize = new Size((int)(350 * sf), (int)(50 * sf));

            abslayout.Add(new Label { Text = "Welcome to DWSIM!", Width = (int)(200 * sf), Font = boldfont }, dx, dy);
            abslayout.Add(new Label { Text = "Quick Access", Width = (int)(200 * sf), Font = boldfont }, dx * 2 + (int)(500 * sf), dy);

            PixelLayout pfile, pccreator, pdocs, pabout, ppatreon;

            pfile = new PixelLayout { Width = (int)(500 * sf), Height = (int)(100 * sf) };
            if (!GlobalSettings.Settings.DarkMode) pfile.BackgroundColor = Colors.White;
            pfile.Add(new Label { Text = "Process Modeling", Width = (int)(200 * sf), Font = boldfont }, dx2, dy2);
            pfile.Add(new Label { Size = lsize, Font = regularfont, Wrap = WrapMode.Word, Text = "Create or load chemical steady-state or dynamic process models." }, dx2, dy2 * 2 + bfh);
            var img1 = new ImageView { Size = psize, Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-chemical_plant.png")) };
            pfile.Add(img1, (int)(400 * sf), 0);
            var link1 = new LinkButton { Text = "Create New", Width = (int)(140 * sf), Font = boldfont2 };
            pfile.Add(link1, dx2, (int)(100 * sf - rfh - dy));
            var link2 = new LinkButton { Text = "Open File", Width = (int)(200 * sf), Font = boldfont2 };
            pfile.Add(link2, dx2 + (int)(150 * sf), (int)(100 * sf - rfh - dy));

            link2.Click += (sender, e) =>
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

            link1.Click += (sender, e) =>
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

            abslayout.Add(pfile, dx, dy * 2 + bfh);

            pccreator = new PixelLayout { Width = (int)(500 * sf), Height = (int)(100 * sf) };
            if (!GlobalSettings.Settings.DarkMode) pccreator.BackgroundColor = Colors.White;
            pccreator.Add(new Label { Text = "Compound Creator", Width = (int)(200 * sf), Font = boldfont }, dx2, dy2);
            pccreator.Add(new Label { Size = lsize, Font = regularfont, Wrap = WrapMode.Word, Text = "Use this tool to create new compounds and load them in your models." }, dx2, dy2 * 2 + bfh);
            var img2 = new ImageView { Size = psize2, Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-test_tube.png")) };
            pccreator.Add(img2, (int)(410 * sf), (int)(10 * sf));
            var link3 = new LinkButton { Text = "Create New", Width = (int)(140 * sf), Font = boldfont2 };
            pccreator.Add(link3, dx2, (int)(100 * sf - rfh - dy));

            link3.Click += (sender, e) =>
            {
                var form = new Desktop.Editors.CompoundCreatorWizard(null);
                form.SetupAndDisplayPage(1);
            };

            abslayout.Add(pccreator, dx, dy * 3 + bfh + (int)(100 * sf));

            ppatreon = new PixelLayout { Width = (int)(500 * sf), Height = (int)(100 * sf) };
            if (!GlobalSettings.Settings.DarkMode) ppatreon.BackgroundColor = Colors.White;
            ppatreon.Add(new Label { Text = "Become a Patron", Width = (int)(200 * sf), Font = boldfont }, dx2, dy2);
            ppatreon.Add(new Label { Size = lsize, Font = regularfont, Wrap = WrapMode.Word, Text = "Become a Patron and get access to exclusive Unit Operations, Property Packages, Plugins/Add-Ins, Mugs and T-Shirts!" }, dx2, dy2 * 2 + bfh);
            var img4 = new ImageView { Size = psize3, Image = new Bitmap(Bitmap.FromResource(bmpprefix + "tshirt_mockup.jpg")) };
            ppatreon.Add(img4, (int)(367 * sf), 0);
            var link4 = new LinkButton { Text = "Support the Project", Width = (int)(140 * sf), Font = boldfont2 };
            ppatreon.Add(link4, dx2, (int)(100 * sf - rfh - dy));
            var bwidth = 70;
            if (Application.Instance.Platform.IsGtk) bwidth = 140;
            var link4a = new LinkButton { Text = "Get Benefits", Width = bwidth, Font = boldfont2, BackgroundColor = Colors.DodgerBlue, TextColor = Colors.White };
            ppatreon.Add(link4a, dx2 + (int)(150 * sf), (int)(100 * sf - rfh - dy));

            link4.Click += (sender, e) => "https://patreon.com/dwsim".OpenURL();
            link4a.Click += (sender, e) => "https://www.patreon.com/join/dwsim?".OpenURL();

            abslayout.Add(ppatreon, dx, dy * 4 + bfh + 2 * (int)(100 * sf));

            pdocs = new PixelLayout { Width = (int)(500 * sf), Height = (int)(100 * sf) };
            if (!GlobalSettings.Settings.DarkMode) pdocs.BackgroundColor = Colors.White;
            pdocs.Add(new Label { Text = "Documentation", Width = (int)(200 * sf), Font = boldfont }, dx2, dy2);
            pdocs.Add(new Label { Size = lsize, Font = regularfont, Wrap = WrapMode.Word, Text = "View DWSIM's User Guide in PDF format." }, dx2, dy2 * 2 + bfh);
            var img5 = new ImageView { Size = psize, Image = new Bitmap(Bitmap.FromResource(imgprefix + "icons8-books.png")) };
            pdocs.Add(img5, (int)(400 * sf), 0);
            var link5 = new LinkButton { Text = "User Guide", Width = (int)(140 * sf), Font = boldfont2 };
            pdocs.Add(link5, dx2, (int)(100 * sf - rfh - dy));
            var link6 = new LinkButton { Text = "Learning Resources", Width = (int)(140 * sf), Font = boldfont2 };
            pdocs.Add(link6, dx2 + (int)(150 * sf), (int)(100 * sf - rfh - dy));

            link6.Click += (sender, e) => "https://dwsim.org/wiki/index.php?title=Tutorials".OpenURL();

            link5.Click += (sender, e) =>
            {
                var basepath = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location);
                try
                {
                    Process.Start(basepath + Path.DirectorySeparatorChar + "docs" + Path.DirectorySeparatorChar + "user_guide.pdf");
                }
                catch (Exception ex)
                {
                    MessageBox.Show(ex.Message, "Error opening User Guide", MessageBoxButtons.OK, MessageBoxType.Error, MessageBoxDefaultButton.OK);
                }
            };

            abslayout.Add(pdocs, dx, dy * 5 + bfh + 3 * (int)(100 * sf));

            pabout = new PixelLayout { Width = (int)(500 * sf), Height = (int)(100 * sf) };
            if (!GlobalSettings.Settings.DarkMode) pabout.BackgroundColor = Colors.White;
            pabout.Add(new Label { Text = "About DWSIM", Width = (int)(200 * sf), Font = boldfont }, dx2, dy2);
            pabout.Add(new Label { Size = lsize, Font = regularfont, Wrap = WrapMode.Word, Text = "Adjust Global Settings and view DWSIM licensing and version information." }, dx2, dy2 * 2 + bfh);
            var img6 = new ImageView { Size = psize, Image = new Bitmap(Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) };
            pabout.Add(img6, (int)(400 * sf), 0);
            var link7 = new LinkButton { Text = "Global Settings", Width = (int)(140 * sf), Font = boldfont2 };
            pabout.Add(link7, dx2, (int)(100 * sf - rfh - dy));
            var link8 = new LinkButton { Text = "About DWSIM", Width = (int)(140 * sf), Font = boldfont2 };
            pabout.Add(link8, dx2 + (int)(150 * sf), (int)(100 * sf - rfh - dy));

            link7.Click += (sender, e) =>
            {
                new Forms.Forms.GeneralSettings().GetForm().Show();
            };

            link8.Click += (sender, e) => new AboutBox().Show();

            abslayout.Add(pabout, dx, dy * 6 + bfh + 4 * (int)(100 * sf));

            MostRecentList = new TreeGridView();
            SampleList = new ListBox();
            FoldersList = new ListBox();
            FOSSEEList = new ListBox();

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
                    try
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
                    catch { }
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

            foreach (var item in samplist.OrderBy((x) => Path.GetFileNameWithoutExtension(x)))
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
                        DWSIM.Logging.Logger.LogError("FOSSEE Flowsheets loading error", t.Exception);
                        Console.WriteLine(t.Exception.ToString());
                        foreach (var iex in t.Exception.InnerExceptions)
                        {
                            DWSIM.Logging.Logger.LogError("FOSSEE Flowsheets loading inner exception", iex);
                            if (iex.InnerException != null) DWSIM.Logging.Logger.LogError("FOSSEE Flowsheets loading inner exception", iex.InnerException);
                        }
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
                };
            };

            SampleList.SelectedIndexChanged += (sender, e) =>
            {
                if (SampleList.SelectedIndex >= 0)
                {
                    LoadSimulation(SampleList.SelectedKey);
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

            if (Application.Instance.Platform.IsGtk)
            {
                tabview.Size = new Size((int)(480 * sf), (int)(636 - dy * 4 - bfh));
            }
            else
            {
                tabview.Size = new Size((int)(480 * sf), (int)(ClientSize.Height - dy * 4 - bfh));
            }

            abslayout.Add(tabview, dx * 2 + (int)(500 * sf), dy * 2 + bfh);

            Content = abslayout;

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
                "https://dwsim.org/wiki/index.php?title=Support".OpenURL();
            };

            var hitem3 = new ButtonMenuItem { Text = "Report a Bug".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem3.Click += (sender, e) =>
            {
                "https://github.com/DanWBR/dwsim/issues".OpenURL();
            };

            var hitem4 = new ButtonMenuItem { Text = "Go to DWSIM Website".Localize(), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "help_browser.png")) };
            hitem4.Click += (sender, e) =>
            {
                "http://dwsim.inforside.com.br".OpenURL();
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
        //switch (GlobalSettings.Settings.RunningPlatform())
        //{
        //    case GlobalSettings.Settings.Platform.Windows:
        //        ClientSize = new Size((int)(s.UIScalingFactor * 700), (int)(s.UIScalingFactor * 400));
        //        break;
        //}
        var splash = new SplashScreen { MainFrm = this };
                splash.Show();
            });
            this.Center();

        }

        void LoadSimulation(string path)
        {

            Task.Factory.StartNew(() => Task.Delay(1000).Wait()).ContinueWith((td) =>
            {

                Forms.Flowsheet form = null;

                LoadingData loadingdialog = null;

                Application.Instance.Invoke(() =>
                {
                    form = new Forms.Flowsheet();

                    AddUserCompounds(form.FlowsheetObject);

                    form.Closed += (sender2, e2) =>
                    {
                        OpenForms -= 1;
                    };

                    OpenForms += 1;

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

                    loadingdialog = new LoadingData();
                    loadingdialog.loadingtext.Text = "Please wait, loading data...\n(" + Path.GetFileNameWithoutExtension(path) + ")";
                    loadingdialog.Show();

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