using Eto.Forms;
using Eto.Drawing;
using System.Reflection;
using System.IO;

namespace DWSIM.UI.Forms
{
    partial class About : Form
    {
        void InitializeComponent()
        {

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            Title = "AboutDWSIM".Localize();
            
            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;

            Minimizable = false;

            Resizable = false;

            ShowInTaskbar = false;
            
            var layout = new PixelLayout();

            string vtext = "Version".Localize() + " " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString();
            string crtext = Shared.AssemblyCopyright;

            if (Application.Instance.Platform.IsWpf)
            {
                layout.Add(new ImageView { Size = new Size(133, 133), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) }, 10, 10);
            }
            else
            {
                layout.Add(new ImageView { Size = new Size(100, 100), Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) }, 10, 10);
            }
            layout.Add(new Label { Text = "DWSIM Simulator (Cross-Platform UI)", TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 120, 10);
            layout.Add(new Label { Text = vtext, TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 120, 30);
            layout.Add(new Label { Text = crtext, TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 120, 50);

            layout.Add(new Label { Text = "DWSIMLicense".Localize() }, 10, 120);

            string gpltext;
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("DWSIM.UI.Forms.Resources.TextFiles.gpl-3.0.txt"))
            using (StreamReader reader = new StreamReader(stream))
            {
                gpltext = reader.ReadToEnd();
            }

            var textbox = new TextArea { Text = gpltext, Width = 480, Height = 420, ReadOnly = true};

            layout.Add(textbox, 10, 140);

            var okbutton = new Button { Text = "OK", Width = 80 };

            okbutton.Click += (sender, e) => Close();

            layout.Add(okbutton, 500 - 80 -10, 600 - 35);

            ClientSize = new Eto.Drawing.Size(500, 600);

            layout.Width = Width;
            layout.Height = Height;
            
            Content = layout;
            
        }
    }
}
