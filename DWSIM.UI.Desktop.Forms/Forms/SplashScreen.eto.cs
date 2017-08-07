using System;
using System.Collections.Generic;
using Eto.Forms;
using Eto.Drawing;
using System.Reflection;
using System.Threading;
using System.Threading.Tasks;
using System.IO;
using DWSIM.Thermodynamics.BaseClasses;

namespace DWSIM.UI.Forms
{
    partial class SplashScreen : Form
    {

        public MainForm MainFrm;

        private Label lblMessage;

        void InitializeComponent()
        {

            WindowStyle = Eto.Forms.WindowStyle.None;

            Style = "transparent-form";

            Title = "DWSIM";

            string imgprefix = "DWSIM.UI.Forms.Resources.Icons.";

            int dx, dy, w, h;

            if (Application.Instance.Platform.IsGtk)
            {
                w = 916;
                h = 426;
                dx = 83;
                dy = 32;
            }
            else
            {
                w = 1088;
                h = 509;
                dx = 0;
                dy = 0;
            }

            ClientSize = new Size(w, h);

            var lbl1 = new Label { Style = "splashlabels2", Text = "ApplicationVersion".Localize() + Assembly.GetExecutingAssembly().GetName().Version.ToString() };

            var lbl1a = new Label { Style = "splashlabels1", Text = "Version".Localize() + " " + Assembly.GetExecutingAssembly().GetName().Version.Major.ToString() + "." + Assembly.GetExecutingAssembly().GetName().Version.Minor.ToString() };

            var lbl2 = new Label { Style = "splashlabels2", Text = "FrameworkVersion".Localize() + Environment.Version.ToString() };

            var lbl3 = new Label { Style = "fixedwidth", Text = "GPLNotice".Localize() };

            lbl1.TextColor = Colors.White;
            lbl2.TextColor = Colors.White;
            lbl1a.TextColor = new Color(0.051f, 0.447f, 0.651f);
            lbl3.TextColor = new Color(0.051f, 0.447f, 0.651f);

            lbl3.Width = 576;
            lbl3.Height = 40;

            lblMessage = new Label { Style = "splashlabels2", Text = "LoadingComponents".Localize() };

            lblMessage.TextColor = Colors.White;

            var lbl5 = new Label { Style = "splashlabels1", Text = Shared.AssemblyCopyright };

            lbl5.TextColor = new Color(0.051f, 0.447f, 0.651f);

            var layout = new PixelLayout();

            var img = new ImageView { Image = Bitmap.FromResource(imgprefix + "DWSIM_splash.png") };

            layout.Add(img, 0 - dx, 0 - dy);
            layout.Add(lblMessage, 101 - dx, 185 - dy);
            layout.Add(lbl1, 101 - dx, 381 - dy);
            layout.Add(lbl2, 101 - dx, 403 - dy);
            layout.Add(lbl1a, 419 - dx, 185 - dy);
            layout.Add(lbl5, 419 - dx, 213 - dy);
            layout.Add(lbl3, 419 - dx, 381 - dy);

            Content = layout;

            var center = Screen.PrimaryScreen.WorkingArea.Center;
            center.X -= w / 2;
            center.Y -= h / 2;

            Location = new Point(center);

            Topmost = true;

            img.BackgroundColor = Colors.Transparent;
            layout.BackgroundColor = Colors.Transparent;
            if (Application.Instance.Platform.IsWinForms)
            {
                BackgroundColor = Colors.White;
            }
            else
            {
                BackgroundColor = Colors.Transparent;
            }

            Shown += SplashScreen_Shown;

            ShowInTaskbar = false;

        }

        void SplashScreen_Shown(object sender, EventArgs e)
        {
            Task.Factory.StartNew(() =>
            {
                Thread.Sleep(1000);
                PerformExtraTasks();
            }).ContinueWith((t) => Application.Instance.Invoke(() => this.Close()));
        }

        void PerformExtraTasks()
        {


            //Read user compounds

            Application.Instance.AsyncInvoke(() => lblMessage.Text = "Reading User Compounds...");

            foreach (var path in GlobalSettings.Settings.UserDatabases)
            {
                try
                {
                    if (Path.GetExtension(path).ToLower() == ".xml")
                    {
                        var comps = DWSIM.Thermodynamics.Databases.UserDB.ReadComps(path);
                        MainFrm.UserCompounds.AddRange(comps);
                    }
                    else if (Path.GetExtension(path).ToLower() == ".json")
                    {
                        var comp = Newtonsoft.Json.JsonConvert.DeserializeObject<ConstantProperties>(File.ReadAllText(path));
                        MainFrm.UserCompounds.Add(comp);
                    }
                }
                catch { }
            }

        }

    }
}
