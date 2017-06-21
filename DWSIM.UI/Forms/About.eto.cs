using Eto.Forms;
using Eto.Drawing;
using System.Reflection;

namespace DWSIM.UI.Forms
{
    partial class About : Form
    {
        void InitializeComponent()
        {

            Title = "About";

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            var layout = new PixelLayout();

            string vtext = "Version " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString();
            string crtext = Shared.AssemblyCopyright;

            layout.Add(new ImageView {Width = 100, Height = 100, Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imgprefix + "DWSIM_ico.png")) }, 10, 10);
            layout.Add(new Label { Text = "DWSIM Simulator", TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 110, 10);
            layout.Add(new Label { Text = vtext, TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 110, 30);
            layout.Add(new Label { Text = crtext, TextAlignment = TextAlignment.Left, Font = SystemFonts.Bold(null, FontDecoration.None) }, 110, 50);

            var okbutton = new Button { Text = "OK" };

            okbutton.Click += (sender, e) => Close();

            layout.Add(okbutton, 300, 230);

            Width = 400;
            Height = 300;

            layout.Width = Width;
            layout.Height = Height;

            Content = layout;
            
        }
    }
}
