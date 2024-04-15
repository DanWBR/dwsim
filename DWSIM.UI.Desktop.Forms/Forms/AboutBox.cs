using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Forms;
using Eto.Drawing;
using System.Reflection;
using System.IO;

using DWSIM.UI.Shared;
using System.Collections.ObjectModel;

namespace DWSIM.UI.Forms.Forms
{
    class AboutBox: Form
    {

        private double sf = GlobalSettings.Settings.UIScalingFactor;

        private ObservableCollection<ComponentInfo> components = new ObservableCollection<ComponentInfo>();
        private ObservableCollection<ComponentInfo> componentsn = new ObservableCollection<ComponentInfo>();

        public AboutBox(): base()
        {
            Init();
        }

        public void Init()
        {

            int w = (int)(sf * 640);
            int h = (int)(sf * 480);

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= w / 2;
            center.Y -= h / 2;

            Location = new Point(center);

            ClientSize = new Size(w, h);
            
            Maximizable = false;

            Minimizable = false;

            Resizable = false;

            ShowInTaskbar = false;

            AddComponentInfo();
            AddComponentInfoN();

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Title = "AboutDWSIM".Localize();

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var layout = new PixelLayout();

            string vtext = "Version".Localize() + " " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + 
                "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() +
                "." + Assembly.GetExecutingAssembly().GetName().Version.Build.ToString();

#if DEBUG
            vtext += "-" + File.GetLastWriteTimeUtc(Assembly.GetExecutingAssembly().Location).ToString();
#endif

            string crtext = Shared.AssemblyCopyright;

            layout.Add(new ImageView { Size = new Size((int)(sf * 100), (int)(sf * 100)), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) }, 0, 0);
            layout.Add(new Label { Text = "DWSIM Simulator (Cross-Platform User Interface)", TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, (int)(sf * 110), 0);
            layout.Add(new Label { Text = vtext, TextAlignment = TextAlignment.Left}, (int)(sf * 110), (int)(sf * 20));
            layout.Add(new Label { Text = crtext, TextAlignment = TextAlignment.Left}, (int)(sf * 110), (int)(sf * 40));

            string osinfo = "", clrinfo = "", meminfo = "";

            if (Application.Instance.Platform.IsMac)
            {
                var osversion = Environment.OSVersion.Version.ToString();
                switch (osversion)
                {
                    case "11.0.0.0": osinfo = "Mac OS X Lion (v10.7.0)"; break;
                    case "11.4.2.0": osinfo = "Mac OS X Lion (v10.7.5)"; break;
                    case "12.0.0.0": osinfo = "OS X Mountain Lion (v10.8.0)"; break;
                    case "13.0.0.0": osinfo = "OS X Mavericks (v10.9.0)"; break;
                    case "13.4.0.0": osinfo = "OS X Mavericks (v10.9.5)"; break;
                    case "14.0.0.0": osinfo = "OS X Yosemite (v10.10.0)"; break;
                    case "14.5.0.0": osinfo = "OS X Yosemite (v10.10.5)"; break;
                    case "15.0.0.0": osinfo = "OS X El Captain (v10.11.0)"; break;
                    case "15.6.0.0": osinfo = "OS X El Captain (v10.11.6)"; break;
                    case "16.0.0.0": osinfo = "macOS Sierra (v10.12.0)"; break;
                    case "16.1.0.0": osinfo = "macOS Sierra (v10.12.1)"; break;
                    case "16.3.0.0": osinfo = "macOS Sierra (v10.12.2)"; break;
                    case "16.4.8.0": osinfo = "macOS Sierra (v10.12.3)"; break;
                    case "16.5.3.0": osinfo = "macOS Sierra (v10.12.4)"; break;
                    case "16.5.6.0": osinfo = "macOS Sierra (v10.12.5)"; break;
                    case "16.7.1.2": osinfo = "macOS Sierra (v10.12.6)"; break;
                    case "17.0.0.0": osinfo = "macOS High Sierra (v10.13.0)"; break;
                    case "17.4.0.0": osinfo = "macOS High Sierra (v10.13.3)"; break;
                    case "18.0.3.0": osinfo = "macOS Mojave (v10.14.0)"; break;
                    case "18.3.0.0": osinfo = "macOS Mojave (v10.14.1-3)"; break;
                    case "18.5.0.0": osinfo = "macOS Mojave (v10.14.4)"; break;
                    case "18.6.0.0": osinfo = "macOS Mojave (v10.14.5)"; break;
                    case "18.7.1.1": osinfo = "macOS Mojave (v10.14.6)"; break;
                    case "19.0.0.0": osinfo = "macOS Catalina (v10.15)"; break;
                    case "19.2.0.0": osinfo = "macOS Catalina (v10.15.2)"; break;
                    case "19.3.0.0": osinfo = "macOS Catalina (v10.15.3)"; break;
                    case "19.4.0.0": osinfo = "macOS Catalina (v10.15.4)"; break;
                    case "19.5.0.0": osinfo = "macOS Catalina (v10.15.5)"; break;
                    case "19.6.0.0": osinfo = "macOS Catalina (v10.15.7)"; break;
                    case "20.0.0.0": osinfo = "macOS Big Sur (v11.0.0)"; break;
                    case "20.1.0.0": osinfo = "macOS Big Sur (v11.0.1)"; break;
                    case "20.3.0.0": osinfo = "macOS Big Sur (v11.1.0)"; break;
                    default: osinfo = "macOS (v" + osversion + ")"; break;
                }
            }
            else {
                osinfo = Environment.OSVersion.ToString();
            }

            clrinfo = SharedClasses.Utility.GetRuntimeVersion();
            
            meminfo = (GC.GetTotalMemory(false) / 1024 / 1024).ToString("#") + " MB managed, " + (Environment.WorkingSet / 1024 / 1024).ToString("#") + " MB total";

            var container1 = new DynamicLayout() { Padding = new Padding(10) };

            container1.CreateAndAddDescriptionRow("DWSIM is released under the terms of the GNU General Public License (GPL) version 3.");
            container1.CreateAndAddTwoLabelsRow2("OS Info:", osinfo);
            container1.CreateAndAddTwoLabelsRow2("CLR Info:", clrinfo);
            container1.CreateAndAddTwoLabelsRow2("Memory Usage:", meminfo);
            
            var listcontainer = new GridView { DataStore = components, RowHeight = (int)(sf * 20) };
            var listcontainern = new GridView { DataStore = componentsn, RowHeight = (int)(sf * 20) };

            var col1 = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Name) },
                HeaderText = "Name"
            };
            col1.AutoSize = true;
            listcontainer.Columns.Add(col1);
            var col1a = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Version) },
                HeaderText = "Version"
            };
            col1a.AutoSize = true;
            listcontainer.Columns.Add(col1a);
            var col1b = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Year) },
                HeaderText = "Year"
            };
            col1b.AutoSize = true;
            listcontainer.Columns.Add(col1b);
            var col1c = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Copyright) },
                HeaderText = "Copyright"
            };
            col1c.AutoSize = true;
            listcontainer.Columns.Add(col1c);
            var col1d = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Website) },
                HeaderText = "Website"
            };
            col1d.AutoSize = true;
            listcontainer.Columns.Add(col1d);
            var col1e = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.License) },
                HeaderText = "License"
            };
            col1e.AutoSize = true;
            listcontainer.Columns.Add(col1e);
            var col1f = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.LicenseText) },
                HeaderText = "License Text"
            };
            col1f.AutoSize = true;
            listcontainer.Columns.Add(col1f);


            var col1n = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Name) },
                HeaderText = "Name"
            };
            col1n.AutoSize = true;
            listcontainern.Columns.Add(col1n);
            var col1an = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.Version) },
                HeaderText = "Version"
            };
            col1an.AutoSize = true;
            listcontainern.Columns.Add(col1an);
            var col1fn = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<ComponentInfo, string>(r => r.LicenseText) },
                HeaderText = "License Text"
            };
            col1fn.AutoSize = true;
            listcontainern.Columns.Add(col1fn);
            
            string gpltext;
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.UI.Forms.Resources.TextFiles.gpl-3.0.txt"))
            using (StreamReader reader = new StreamReader(stream))
            {
                gpltext = reader.ReadToEnd();
            }

            var txt1 = new TextArea { Text = gpltext, ReadOnly = true, Font = Fonts.Monospace(10.0f) };

            var t1 = new TableLayout(new TableRow(txt1));

            var tab1 = new TabPage { Content = container1, Text = "General Information" };
            var tab2 = new TabPage { Content = new Scrollable { Content = t1 }, Text = "GPLv3 License" };
            var tab3 = new TabPage { Content = new Scrollable { Content = listcontainer }, Text = "External Components" };
            var tab4 = new TabPage { Content = new Scrollable { Content = listcontainern }, Text = "NuGet Packages" };

            var tabc = new TabControl();
            tabc.Pages.Add(tab1);
            tabc.Pages.Add(tab2);
            tabc.Pages.Add(tab3);
            tabc.Pages.Add(tab4);

            var tablecontainer = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };

            tablecontainer.Rows.Add(new TableRow(layout));
            tablecontainer.Rows.Add(new TableRow(tabc));

            Content = tablecontainer;

        }

        private void AddComponentInfo()
        {
            components.Add(new ComponentInfo("CoolProp", "6.0.0", "2016", "Ian H. Bell", "http://wwww.coolprop.org", "MIT-style License", "https://github.com/ibell/coolprop/blob/master/LICENSE"));
            components.Add(new ComponentInfo("GERG 2008", "2.0", "2017", "E. W. Lemmon, V. Heinemann, J. Lu, I. Bell", "https://github.com/usnistgov/AGA8", "NIST/17 U.S.C. 105", "https://github.com/usnistgov/AGA8/blob/master/LICENSE"));
            components.Add(new ComponentInfo("ChemSep Database", "7.30", "2018", "Harry Kooijman, Ross Taylor", "http://www.chemsep.org", "Perl Artistic License v2", "http://www.perlfoundation.org/artistic_license_2_0"));
            components.Add(new ComponentInfo("Flee", "0.9.14", "2009", "Eugene Ciloci", "https://flee.codeplex.com", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"));
            components.Add(new ComponentInfo("DotNumerics", "1.0", "2009", "Jose Antonio De Santiago Castillo", "http://www.dotnumerics.com", "GPLv3", "http://www.gnu.org/licenses/gpl.html"));
            components.Add(new ComponentInfo("FileHelpers", "3.2.7", "2018", "Marcos Meli", "https://sourceforge.net/projects/filehelpers", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"));
            components.Add(new ComponentInfo("SharpZipLib", "0.85.4.369", "2010", "IC#Code", "http://www.icsharpcode.net/OpenSource/SharpZipLib", "GPLv2", "http://www.gnu.org/licenses/gpl.html"));
            components.Add(new ComponentInfo("Nini", "1.1", "2010", "Brent R. Matzelle", "https://sourceforge.net/projects/nini", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("ScintillaNET", "3.5.1.0", "2015", "Jacob Slusser", "https://github.com/jacobslusser/scintillaNET", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("Yeppp!", "1.0.0.1", "2014", "Marat Dukhan", "http://www.yeppp.info", "Yeppp! License", "http://www.yeppp.info/resources/yeppp-license.txt"));
            components.Add(new ComponentInfo("SwarmOps", "3.1", "2011", "Magnus Erik Hvass Pedersen", "http://www.hvass-labs.org/projects/swarmops/cs/", "MIT-style License", "http://www.hvass-labs.org/projects/swarmops/cs/files/license.txt"));
            components.Add(new ComponentInfo("RandomOps", "2.1", "2010", "Magnus Erik Hvass Pedersen", "http://www.hvass-labs.org/projects/randomops/cs/", "MIT-style License", "http://www.hvass-labs.org/projects/randomops/cs/files/license.txt" ));
            components.Add(new ComponentInfo("OxyPlot", "2.0", "2017", "OxyPlot team", "http://www.oxyplot.org", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("SkiaSharp", "1.68.0", "2020", "Xamarin team", "https://github.com/mono/SkiaSharp/", "MIT License", "https://github.com/mono/SkiaSharp/blob/master/LICENSE.md"));
            components.Add(new ComponentInfo("Eto.Forms", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"));
            components.Add(new ComponentInfo("Eto.Platform.Gtk2", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"));
            components.Add(new ComponentInfo("Eto.Platform.Windows", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"));
            components.Add(new ComponentInfo("Eto.Platform.Wpf", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"));
            components.Add(new ComponentInfo("Eto.Platform.XamMac2", "custom version", "2019", "", "https://github.com/DanWBR/Eto", "BSD-style License", "https://github.com/DanWBR/Eto/blob/develop/LICENSE.txt"));
            components.Add(new ComponentInfo("ReoGrid", "custom version", "2020", "", "https://github.com/DanWBR/ReoGrid", "MIT-style License", "https://github.com/DanWBR/ReoGrid/blob/master/LICENSE"));
        }

        private void AddComponentInfoN()
        {
           
            componentsn.Add(new ComponentInfo("cef.redist.x64", "75.1.14", "https://raw.github.com/cefsharp/cef-binary/master/LICENSE.txt"));
            componentsn.Add(new ComponentInfo("cef.redist.x86", "75.1.14", "https://raw.github.com/cefsharp/cef-binary/master/LICENSE.txt"));
            componentsn.Add(new ComponentInfo("CefSharp.Common", "75.1.14", "https://raw.github.com/cefsharp/CefSharp/master/LICENSE"));
            componentsn.Add(new ComponentInfo("CefSharp.WinForms", "75.1.14", "https://raw.github.com/cefsharp/CefSharp/master/LICENSE"));
            componentsn.Add(new ComponentInfo("DynamicLanguageRuntime", "1.2.3", "https://github.com/IronLanguages/dlr/blob/master/LICENSE"));
            componentsn.Add(new ComponentInfo("IronPython", "2.7.10", "https://github.com/IronLanguages/ironpython2/blob/master/LICENSE"));
            componentsn.Add(new ComponentInfo("IronPython.StdLib", "2.7.10", "http://docs.python.org/license.html"));
            componentsn.Add(new ComponentInfo("jacobslusser.ScintillaNET", "3.6.3", ""));
            componentsn.Add(new ComponentInfo("MathNet.Numerics", "4.7.0", "https://numerics.mathdotnet.com/License.html"));
            componentsn.Add(new ComponentInfo("Newtonsoft.Json", "12.0.3", "https://raw.github.com/JamesNK/Newtonsoft.Json/master/LICENSE.md"));
            componentsn.Add(new ComponentInfo("OpenTK", "3.0.", "http://github.com/opentk/opentk/blob/master/License.txt"));
            componentsn.Add(new ComponentInfo("OpenTK.GLControl", "3.0.1", "http://github.com/opentk/opentk/blob/master/License.txt"));
            componentsn.Add(new ComponentInfo("OxyPlot.Core", "2.0.0-unstable0956", "https://raw.githubusercontent.com/oxyplot/oxyplot/master/LICENSE"));
            componentsn.Add(new ComponentInfo("OxyPlot.Wpf", "2.0.0-unstable0956", "https://raw.githubusercontent.com/oxyplot/oxyplot/master/LICENSE"));
            componentsn.Add(new ComponentInfo("SharpDX", "4.0.1", "http://sharpdx.org/License.txt"));
            componentsn.Add(new ComponentInfo("SharpDX.Direct2D1", "4.0.1", "http://sharpdx.org/License.txt"));
            componentsn.Add(new ComponentInfo("SharpDX.DXGI", "4.0.1", "http://sharpdx.org/License.txt"));
            componentsn.Add(new ComponentInfo("SharpDX.Mathematics", "4.0.1", "http://sharpdx.org/License.txt"));
            componentsn.Add(new ComponentInfo("SharpZipLib", "1.1.0", "https://github.com/icsharpcode/SharpZipLib/blob/master/LICENSE.txt"));
            componentsn.Add(new ComponentInfo("SkiaSharp", "1.68.2.1", "https://github.com/mono/SkiaSharp/blob/master/LICENSE.md"));
            componentsn.Add(new ComponentInfo("SkiaSharp.Extended", "1.68.2.1", "https://github.com/mono/SkiaSharp.Extended/blob/master/LICENSE"));
            componentsn.Add(new ComponentInfo("System.ComponentModel", "4.3.0", "http://go.microsoft.com/fwlink/?LinkId=329770"));
            componentsn.Add(new ComponentInfo("System.Runtime.Serialization.Primitives", "4.3.0", "http://go.microsoft.com/fwlink/?LinkId=329770"));
            componentsn.Add(new ComponentInfo("Eto.OxyPlot", "1.2.0-beta", ""));
            componentsn.Add(new ComponentInfo("Eto.OxyPlot.Wpf", "1.2.0-beta", ""));
        }

        public class ComponentInfo
        {
            public ComponentInfo(string name, string version, string year, string copyright, string website, string license, string licensetext)
            {
                Name = name;
                Version = version;
                Year = year;
                Copyright = copyright;
                Website = website;
                License = license;
                LicenseText = licensetext;
            }

            public ComponentInfo(string name, string version, string licensetext)
            {
                Name = name;
                Version = version;
                LicenseText = licensetext;
            }

            public string Name { get; set; }

            public string Version { get; set; }

            public string Year { get; set; }

            public string Copyright { get; set; }
            public string Website { get; set; }

            public string License { get; set; }

            public string LicenseText { get; set; }

        }

    }
}
