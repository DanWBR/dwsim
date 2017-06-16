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
            new Application(Platform.Detect).Run(new MainForm());
        }
    }
}
