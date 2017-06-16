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
                DWSIM.UI.Desktop.WPF.StyleSetter.SetMainButtonStyle();
            }

            new Application().Run(new MainForm());
        }
    }
}
