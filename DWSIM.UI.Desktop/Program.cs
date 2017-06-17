using System;
using System.IO;
using Eto;
using Eto.Forms;

namespace DWSIM.UI.Desktop
{
    public class Program
    {
        [STAThread]
        public static void Main(string[] args)
        {

            if (RunningPlatform() == OSPlatform.Windows)
            {
                DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();
                new Application(Platforms.Wpf).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Linux)
            {
                DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();
                new Application(Platforms.Gtk2).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Mac)
            {
                new Application(Platforms.Mac64).Run(new MainForm());
            }
                        
        }
        
        public enum OSPlatform
        {
            Windows,
            Linux,
            Mac
        }

        public static OSPlatform RunningPlatform()
        {
            switch (Environment.OSVersion.Platform)
            {
                case PlatformID.Unix:
                    // Well, there are chances MacOSX is reported as Unix instead of MacOSX.
                    // Instead of platform check, we'll do a feature checks (Mac specific root folders)
                    if (Directory.Exists("/Applications")
                        & Directory.Exists("/System")
                        & Directory.Exists("/Users")
                        & Directory.Exists("/Volumes"))
                        return OSPlatform.Mac;
                    else
                        return OSPlatform.Linux;

                case PlatformID.MacOSX:
                    return OSPlatform.Mac;

                default:
                    return OSPlatform.Windows;
            }
        }

    }

}
