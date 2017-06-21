using System;
using System.IO;
using DWSIM.UI.Controls;
using Eto.Forms;
using SkiaSharp;
using System.Runtime.InteropServices;

namespace DWSIM.UI.Desktop
{
    public class Program
    {

        [STAThread]
        public static void Main(string[] args)
        {

            GlobalSettings.Settings.CultureInfo = "pt";
            
            if (RunningPlatform() == OSPlatform.Windows)
            {

                DWSIM.UI.Desktop.WPF.StyleSetter.SetTheme("aero", "normalcolor");

                DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();

                var platform = new Eto.Wpf.Platform();

                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WPF.FlowsheetSurfaceControlHandler());

                new Application(platform).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Linux)
            {

                DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();

                var platform = new Eto.GtkSharp.Platform();

                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new GTK.FlowsheetSurfaceControlHandler());
      
                new Application(platform).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Mac)
            {

                DWSIM.UI.Desktop.Mac.StyleSetter.SetStyles();

                var platform = new Eto.Mac.Platform();

                new Application(platform).Run(new MainForm());

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
