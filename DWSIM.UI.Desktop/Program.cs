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
            Settings.OldUI = false;
            Settings.LoadSettings("dwsim_newui.ini");

            Eto.Platform platform = null;
            
            if (Settings.RunningPlatform() == Settings.Platform.Windows)
            {
                switch (GlobalSettings.Settings.WindowsRenderer)
                {
                    case Settings.WindowsPlatformRenderer.WinForms:
                        DWSIM.UI.Desktop.WinForms.StyleSetter.SetStyles();
                        platform = new Eto.Direct2D.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WinForms.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.WinForms.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new WinForms.CodeEditorControlHandler());
                        break;
                    case Settings.WindowsPlatformRenderer.WinForms_Direct2D:
                        DWSIM.UI.Desktop.WinForms.StyleSetter.SetStyles();
                        platform = new Eto.WinForms.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WinForms.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.WinForms.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new WinForms.CodeEditorControlHandler());
                        break;
                    case Settings.WindowsPlatformRenderer.WPF:
                        DWSIM.UI.Desktop.WPF.StyleSetter.SetTheme("aero", "normalcolor");
                        DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();
                        platform = new Eto.Wpf.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WPF.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Wpf.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new WPF.CodeEditorControlHandler());
                        break;
                }
                new Application(platform).Run(new MainForm());
                Settings.SaveSettings("dwsim_newui.ini");
            }
            else if (Settings.RunningPlatform() == Settings.Platform.Linux)
            {
                switch (GlobalSettings.Settings.LinuxRenderer)
                {
                    case Settings.LinuxPlatformRenderer.Gtk2:
                        DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();
                        platform = new Eto.GtkSharp.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new GTK.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Gtk.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new GTK.CodeEditorControlHandler());
                        break;
                    case Settings.LinuxPlatformRenderer.Gtk3:
                        //DWSIM.UI.Desktop.WPF.StyleSetter.SetTheme("aero", "normalcolor");
                        //DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();
                        //platform = new Eto.Wpf.Platform();
                        //platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WPF.FlowsheetSurfaceControlHandler());
                        //platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Wpf.PlotHandler());
                        //platform.Add<CodeEditorControl.ICodeEditor>(() => new WPF.CodeEditorControlHandler());
                        break;
                    case Settings.LinuxPlatformRenderer.WinForms:
                        DWSIM.UI.Desktop.WinForms.StyleSetter.SetStyles();
                        platform = new Eto.WinForms.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WinForms.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.WinForms.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new WinForms.CodeEditorControlHandler());
                        break;
                }
                new Application(platform).Run(new MainForm());
                Settings.SaveSettings("dwsim_newui.ini");
            }
            else if (Settings.RunningPlatform() == Settings.Platform.Mac)
            {
                switch (GlobalSettings.Settings.MacOSRenderer)
                {
                    case Settings.MacOSPlatformRenderer.MonoMac:
                        DWSIM.UI.Desktop.Mac.StyleSetter.SetStyles();
                        platform = new Eto.Mac.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new Mac.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Mac.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new Mac.CodeEditorControlHandler());
                        break;
                    case Settings.MacOSPlatformRenderer.Gtk2:
                        DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();
                        platform = new Eto.GtkSharp.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new GTK.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.Gtk.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new GTK.CodeEditorControlHandler());
                        break;
                    case Settings.MacOSPlatformRenderer.WinForms:
                        DWSIM.UI.Desktop.WinForms.StyleSetter.SetStyles();
                        platform = new Eto.WinForms.Platform();
                        platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new WinForms.FlowsheetSurfaceControlHandler());
                        platform.Add<Eto.OxyPlot.Plot.IHandler>(() => new Eto.OxyPlot.WinForms.PlotHandler());
                        platform.Add<CodeEditorControl.ICodeEditor>(() => new WinForms.CodeEditorControlHandler());
                        break;
                }
                try
                {
                    new Application(platform).Run(new MainForm());
                    Application.Instance.Terminating += (sender, e) => {
                        Settings.SaveSettings("dwsim_newui.ini");
                    };
                }
                catch (Exception ex)
                {
                    string configfiledir = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.Personal), "Documents", "DWSIM Application Data");
                    if (!Directory.Exists(configfiledir)) Directory.CreateDirectory(configfiledir);
                    File.WriteAllText(System.IO.Path.Combine(configfiledir, "lasterror.txt"), ex.ToString());
                }
            }
        }

    }

}
