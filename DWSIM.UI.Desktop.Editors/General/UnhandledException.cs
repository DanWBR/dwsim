using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.Thermodynamics.PropertyPackages;
using DWSIM.Thermodynamics.PropertyPackages.Auxiliary;
using Eto.Drawing;
using Eto.Forms;
using DWSIM.UI.Shared;
using System.Diagnostics;

namespace DWSIM.UI.Desktop.Editors
{
    public class UnhandledExceptionView : Dialog
    {

        private Exception exc;
        private string githublink = "";

        public UnhandledExceptionView(Exception ex)
            : base()
        {
            exc = ex;
            Init();
        }

        void Init()
        {

            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            Title = "Error";

            var mystring = SharedClasses.EncryptString.StringCipher.Decrypt("YEZeCozmw0l3XjOYI0EpOXHh1LK9as6Bi5Gwqr7pYZyXtcNYQyzayHXts6NjAJlpfixoim98NAwVHli/+h1fYk6g4W82ewXDxkLwzg5SFCCSS2W0K3TvGMgC0wQWuKfrut0QdnByVKZ4x+/svdQwwXsUkZdELOUtnWiOdeV6WIQ=", "dwsim000000");
            var mystring2 = SharedClasses.EncryptString.StringCipher.Decrypt("T+h/AQaXoM7xMDrov6dkD/82uHShQ6gX7MD+yyPG1ALdchPnpYsxHZWU8YcwP3jTPCZWRL9mmAWnQnWtp4ETyYh17Cgjt1EDYbEJJvh/PacWXami/6btnnbE0D5HBpnYrKamsf6qjjx9JbhQOZIvXJv6dIlJ7lMm5vWkhmLpNuc=", "dwsim000000");

            try
            {
                string baseaddress = "https://github.com/DanWBR/dwsim5/blob/windows/";
                StackTrace st = new StackTrace(exc, true);
                StackFrame frame = st.GetFrame(0);
                string path = frame.GetFileName().Replace(mystring, baseaddress);
                path = path.Replace(mystring2, baseaddress);
                int line = frame.GetFileLineNumber();
                if (path.Contains(baseaddress))
                {
                    githublink = path + "#L" + line;
                }
            }
            catch (Exception)
            {
            }

            var container = new TableLayout() { Padding = new Padding(10), Spacing = new Size(5, 5) };

            container.Rows.Add(new TableRow(new Label { Text = "DWSIM generated an unhandled error/exception." }));

            var txt1 = new TextArea { Text = exc.Message.ToString(), ReadOnly = true };
            var txt2 = new TextArea { Text = exc.ToString(), ReadOnly = true };

            var t1 = new TableLayout(new TableRow(txt1));
            var t2 = new TableLayout(new TableRow(txt2));

            var tab1 = new TabPage { Content = t1, Text = "Error Message" };
            var tab2 = new TabPage { Content = t2, Text = "Details" };

            var tabc = new TabControl { Height = 300 };
            tabc.Pages.Add(tab1);
            tabc.Pages.Add(tab2);

            container.Rows.Add(new TableRow(tabc));

            container.Rows.Add(new Label { Text = "Actions", Font = SystemFonts.Bold() });

            var dyn1 = new DynamicLayout { Padding = new Padding(0) };
            dyn1.CreateAndAddLabelAndButtonRow("Close this window.", "Continue", null, (sender, e) =>
            {
                this.Close();
            });
            container.Rows.Add(new TableRow(dyn1));

            var dyn2 = new DynamicLayout { Padding = new Padding(0) };
            dyn2.CreateAndAddLabelAndButtonRow("Kill current DWSIM process. Unsaved changes will be lost.", "Kill", null, (sender, e) =>
            {
                Process.GetCurrentProcess().Kill();
            });
            container.Rows.Add(new TableRow(dyn2));

            var dyn3 = new DynamicLayout { Padding = new Padding(0) };
            dyn3.CreateAndAddLabelAndButtonRow("Restart DWSIM", "Restart", null, (sender, e) =>
            {
                Application.Instance.Restart();
            });
            container.Rows.Add(new TableRow(dyn3));

            container.Rows.Add(new Label { Text = "Additional Support", Font = SystemFonts.Bold() });

            var dyn4 = new DynamicLayout { Padding = new Padding(0) };
            dyn4.CreateAndAddLabelAndButtonRow("Search DWSIM Forums for information about the current error.", "Search Forums", null, (sender, e) =>
            {
                var baseaddress = "https://sourceforge.net/p/dwsim/search/?q=";
                var searchtext = exc.Message.ToString().Replace(" ", "+");
                Process.Start(baseaddress + searchtext);
            });
            container.Rows.Add(new TableRow(dyn4));

            var dyn5 = new DynamicLayout { Padding = new Padding(0) };
            dyn5.CreateAndAddLabelAndButtonRow("Search DWSIM's Website for information about the current error.", "Search Website", null, (sender, e) =>
            {
                var baseaddress = "http://dwsim.inforside.com.br/wiki/index.php?title=Special:Search&fulltext=Search&profile=all&redirs=1&search=";
                var searchtext = exc.Message.ToString().Replace(" ", "+");
                Process.Start(baseaddress + searchtext);
            });
            container.Rows.Add(new TableRow(dyn5));

            if (githublink != "")
            {
                var dyn6 = new DynamicLayout { Padding = new Padding(0) };
                dyn6.CreateAndAddLabelAndButtonRow("View the code line where this error was raised/generated on DWSIM's GitHub repository.", "View code @ GitHub", null, (sender, e) =>
                {
                    Process.Start(githublink);
                });
                container.Rows.Add(new TableRow(dyn6));
            }

            Content = container;

        }



    }
}
