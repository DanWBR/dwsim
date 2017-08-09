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

        private ObservableCollection<ComponentInfo> components = new ObservableCollection<ComponentInfo>();

        public AboutBox(): base()
        {
            Init();
        }

        public void Init()
        {

            int w = 640;
            int h = 480;

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

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Title = "AboutDWSIM".Localize();

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            var layout = new PixelLayout();

            string vtext = "Version".Localize() + " " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString();

            var updfile = AppDomain.CurrentDomain.BaseDirectory + Path.DirectorySeparatorChar + "version.info";

            if (File.Exists(updfile))
            {
                int vinfo = 0;
                int.TryParse(File.ReadAllText(updfile), out vinfo);
                if (vinfo > 0) vtext += " Update " + vinfo;
            }
            
            string crtext = Shared.AssemblyCopyright;

            layout.Add(new ImageView { Size = new Size(100, 100), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) }, 0, 0);
            layout.Add(new Label { Text = "DWSIM Simulator (Cross-Platform User Interface)", TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 110, 0);
            layout.Add(new Label { Text = vtext, TextAlignment = TextAlignment.Left}, 110, 20);
            layout.Add(new Label { Text = crtext, TextAlignment = TextAlignment.Left}, 110, 40);

            string osinfo = "", clrinfo = "", meminfo = "";

            osinfo = Environment.OSVersion.ToString();

            if (GlobalSettings.Settings.IsRunningOnMono())
            {
                Type type = Type.GetType("Mono.Runtime");
                if (type != null)
                {
                    MethodInfo dispalayName = type.GetMethod("GetDisplayName", BindingFlags.NonPublic | BindingFlags.Static);
                    if (dispalayName != null) clrinfo = "Mono Framework, Version " + dispalayName.Invoke(null, null).ToString();
                }
            }
            else {
                clrinfo = "Microsoft .NET Framework, Runtime Version " + System.Runtime.InteropServices.RuntimeEnvironment.GetSystemVersion().ToString();            
            }
            
            meminfo = (GC.GetTotalMemory(false) / 1024 / 1024).ToString("#") + " MB managed, " + (Environment.WorkingSet / 1024 / 1024).ToString("#") + " MB total";

            var container1 = new DynamicLayout() { Padding = new Padding(10) };

            container1.CreateAndAddTwoLabelsRow2("Main Developer:", "Daniel Medeiros (dwsim@inforside.com.br)");
            container1.CreateAndAddTwoLabelsRow2("Contributors:", "Gregor Reichert, Gustavo León and others");
            container1.CreateAndAddTwoLabelsRow2("Splash Screen Design:", "Wendel Marcus (www.behance.net/wendelmarcus)");
            container1.CreateAndAddTwoLabelsRow2("OS Info:", osinfo);
            container1.CreateAndAddTwoLabelsRow2("CLR Info:", clrinfo);
            container1.CreateAndAddTwoLabelsRow2("Memory Usage:", meminfo);
            container1.CreateAndAddLabelRow("DWSIM is released under the terms of the GNU General Public License (GPL) version 3.");
            
            var listcontainer = new GridView { DataStore = components, RowHeight = 20 };

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

            string gpltext;
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.UI.Forms.Resources.TextFiles.gpl-3.0.txt"))
            using (StreamReader reader = new StreamReader(stream))
            {
                gpltext = reader.ReadToEnd();
            }

            var txt1 = new TextArea { Text = gpltext, ReadOnly = true };

            var t1 = new TableLayout(new TableRow(txt1));

            var tab1 = new TabPage { Content = container1, Text = "General Information" };
            var tab2 = new TabPage { Content = new Scrollable { Content = t1 }, Text = "DWSIM License" };
            var tab3 = new TabPage { Content = new Scrollable { Content = listcontainer }, Text = "External Components" };

            var tabc = new TabControl();
            tabc.Pages.Add(tab1);
            tabc.Pages.Add(tab2);
            tabc.Pages.Add(tab3);

            var tablecontainer = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };

            tablecontainer.Rows.Add(new TableRow(layout));
            tablecontainer.Rows.Add(new TableRow(tabc));

            Content = tablecontainer;

        }

        private void AddComponentInfo()
        {
            components.Add(new ComponentInfo("CoolProp", "6.0.0", "2016", "Ian H. Bell", "http://wwww.coolprop.org", "MIT-style License", "https://github.com/ibell/coolprop/blob/master/LICENSE"));
            components.Add(new ComponentInfo("ChemSep Database", "7.12", "2016", "Harry Kooijman, Ross Taylor", "http://www.chemsep.org", "Perl Artistic License v2", "http://www.perlfoundation.org/artistic_license_2_0"));
            components.Add(new ComponentInfo("Flee", "0.9.14", "2009", "Eugene Ciloci", "https://flee.codeplex.com", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"));
            components.Add(new ComponentInfo("DotNumerics", "1.0", "2009", "Jose Antonio De Santiago Castillo", "http://www.dotnumerics.com", "GPLv3", "http://www.gnu.org/licenses/gpl.html"));
            components.Add(new ComponentInfo("FileHelpers", "1.6", "2007", "Marcos Meli", "https://sourceforge.net/projects/filehelpers", "LGPLv2", "http://www.gnu.org/licenses/lgpl.html"));
            components.Add(new ComponentInfo("SharpZipLib", "0.85.4.369", "2010", "IC#Code", "http://www.icsharpcode.net/OpenSource/SharpZipLib", "GPLv2", "http://www.gnu.org/licenses/gpl.html"));
            components.Add(new ComponentInfo("Nini", "1.1", "2010", "Brent R. Matzelle", "https://sourceforge.net/projects/nini", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("ScintillaNET", "3.5.1.0", "2015", "Jacob Slusser", "https://github.com/jacobslusser/scintillaNET", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("Yeppp!", "1.0.0.1", "2014", "Marat Dukhan", "http://www.yeppp.info", "Yeppp! License", "http://www.yeppp.info/resources/yeppp-license.txt"));
            components.Add(new ComponentInfo("SwarmOps", "3.1", "2011", "Magnus Erik Hvass Pedersen", "http://www.hvass-labs.org/projects/swarmops/cs/", "MIT-style License", "http://www.hvass-labs.org/projects/swarmops/cs/files/license.txt"));
            components.Add(new ComponentInfo("RandomOps", "2.1", "2010", "Magnus Erik Hvass Pedersen", "http://www.hvass-labs.org/projects/randomops/cs/", "MIT-style License", "http://www.hvass-labs.org/projects/randomops/cs/files/license.txt" ));
            components.Add(new ComponentInfo("Eto.Forms", "2.4", "2017", "Curtis Wensley", "https://github.com/picoe/Eto", "BSD-3 License", "http://opensource.org/licenses/BSD-3-Clause"));
            components.Add(new ComponentInfo("OxyPlot", "2.0", "2017", "OxyPlot team", "http://www.oxyplot.org", "MIT License", "http://www.opensource.org/licenses/mit-license.html"));
            components.Add(new ComponentInfo("SkiaSharp", "1.55.x", "2017", "Xamarin team", "https://github.com/mono/SkiaSharp/", "MIT License", "https://github.com/mono/SkiaSharp/blob/master/LICENSE.md"));
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
