using System;
using Eto;
using Eto.Forms;

namespace DWSIM.UI.Desktop
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {

            var plat = Platform.Detect;

            if (plat.IsWpf)
            {
                DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();
            }else if (plat.IsGtk)
            {
                DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();
            }

            new Application().Run(new MainForm());
        }
    }
}
