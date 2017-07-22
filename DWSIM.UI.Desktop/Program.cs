using System;
using System.IO;
using DWSIM.UI.Controls;
using Eto.Forms;
using SkiaSharp;
using System.Runtime.InteropServices;
using DWSIM.GlobalSettings;

namespace DWSIM.UI.Desktop
{
    public class Program
    {

        [STAThread]
        public static void Main(string[] args)
        {

            // set global settings

            Settings.CultureInfo = "en";
            Settings.EnableGPUProcessing = false;

            Settings.LoadSettings("dwsim_newui.ini");

            if (Settings.RunningPlatform() == Settings.Platform.Windows)
            {
                DWSIM.UI.Desktop.WPF.StyleSetter.SetTheme("aero", "normalcolor");

                DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();

                var platform = new Eto.Wpf.Platform();

                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WPF.FlowsheetSurfaceControlHandler());
                platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Wpf.PlotHandler());

                new Application(platform).Run(new MainForm());
            }
            else if (Settings.RunningPlatform() == Settings.Platform.Linux)
            {
                DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();

                var platform = new Eto.GtkSharp.Platform();

                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new GTK.FlowsheetSurfaceControlHandler());
                platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Gtk.PlotHandler());

                new Application(platform).Run(new MainForm());
            }
            else if (Settings.RunningPlatform() == Settings.Platform.Mac)
            {

                try
                {
                    DWSIM.UI.Desktop.Mac.StyleSetter.SetStyles();

                    var platform = new Eto.Mac.Platform();

                    platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new Mac.FlowsheetSurfaceControlHandler());
                    platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Mac.PlotHandler());

                    new Application(platform).Run(new MainForm());
                }
                catch (Exception ex)
                {
                    File.WriteAllText(System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "log.txt"), ex.ToString());
                   
                    if (ex.InnerException != null) File.WriteAllText(System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Desktop), "log.txt"), ex.InnerException.ToString());
                }

            }

            Settings.SaveSettings("dwsim_newui.ini");

        }

    }

}
