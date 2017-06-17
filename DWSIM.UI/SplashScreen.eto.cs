using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;

namespace DWSIM.UI.Forms
{
    partial class SplashScreen : Form
    {
        void InitializeComponent()
        {
            Title = "My Form";

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";
           
            ClientSize = new Size(1088, 509);

            var lbl1 = new Label {Style = "splashlabels2", Text = "Application Version: " + Assembly.GetExecutingAssembly().GetName().Version.ToString() };

            var lbl1a = new Label { Style = "splashlabels1", Text = "Version " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() };

            var lbl2 = new Label { Style = "splashlabels2", Text = "Framework Version: " + Environment.Version.ToString() };

            var lbl3 = new Label { Style = "splashlabels1", Text = "This software is released under the terms of the GNU General Public License (GPL) version 3. See specific licenses for external components on the 'About' box." };

            lbl1.TextColor = Colors.White;
            lbl2.TextColor = Colors.White;
            lbl1a.TextColor = new Color(0.051f, 0.447f, 0.651f);
            lbl3.TextColor = new Color(0.051f, 0.447f, 0.651f);
            
            lbl3.Width = 576;
            lbl3.Height = 40;

            var lbl4 = new Label { Style = "splashlabels2", Text = "Loading components..." };

            lbl4.TextColor = Colors.White;

            var lbl5 = new Label { Style = "splashlabels1", Text = AssemblyCopyright };

            lbl5.TextColor = new Color(0.051f, 0.447f, 0.651f);

            var layout = new PixelLayout();

            var img = new ImageView { Image = Bitmap.FromResource(imgprefix + "DWSIM_splash.png") };
            
            layout.Add(img, 0, 0);
            layout.Add(lbl4, 101, 185);
            layout.Add(lbl1, 101, 381);
            layout.Add(lbl2, 101, 403);
            layout.Add(lbl1a, 419, 185);
            layout.Add(lbl5, 419, 213);
            layout.Add(lbl3, 419, 381);
            
            Content = layout;

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= 1088 / 2;
            center.Y -= 509 / 2;

            Location = new Point(center);

            Topmost = true;

            WindowStyle = Eto.Forms.WindowStyle.None;

            img.BackgroundColor = Colors.Transparent;
            layout.BackgroundColor = Colors.Transparent;
            BackgroundColor = Colors.Transparent;

            Style = "transparent-form";

            Shown += SplashScreen_Shown;
            
        }

        void SplashScreen_Shown(object sender, EventArgs e)
        {
            //Thread.Sleep(3000);
            //this.Close(); 
        }

        public string AssemblyCopyright
        {
            get
            {
                object[] attributes = Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
                if (attributes.Length == 0)
                {
                    return "";
                }
                return ((AssemblyCopyrightAttribute)attributes[0]).Copyright;
            }
        }

    }
}
