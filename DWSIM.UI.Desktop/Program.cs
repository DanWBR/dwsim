using System;
using System.IO;
using DWSIM.UI.Forms.Controls;
using Eto;
using Eto.Forms;
using SkiaSharp;
using System.Collections.Generic;
using System.Runtime.InteropServices;

namespace DWSIM.UI.Desktop
{
    public class Program
    {

        [DllImport("kernel32.dll", CharSet = CharSet.Auto, SetLastError = true)]
        static extern bool SetDllDirectory(string lpPathName);

        [STAThread]
        public static void Main(string[] args)
        {

            //SetDllDirectory("C:\\Users\\ptc0\\Documents\\GTK2-Runtime");

            Flowsheet flowsheet = new Flowsheet();

            var surface = (DWSIM.Drawing.SkiaSharp.GraphicsSurface)flowsheet.GetSurface();

            var obj0 = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.CompressorExpanderGraphic(100, 250, 100, 100) { Tag = "OBJ1" };
            var obj1 = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.RigorousColumnGraphic(100, 100, 140, 180) { Tag = "OBJ2" };
            var obj2 = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.ComponentSeparatorGraphic(300, 250, 50, 50) { Tag = "OBJ3" };
            var obj3 = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.AdjustGraphic(200, 50, 50, 50) { Tag = "OBJ4" };
            var obj4 = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.PipeSegmentGraphic(150, 150, 50, 50) { Tag = "OBJ5" };
            surface.DrawingObjects.Add(obj0);
            surface.DrawingObjects.Add(obj1);
            surface.DrawingObjects.Add(obj2);
            surface.DrawingObjects.Add(obj3);
            surface.DrawingObjects.Add(obj4);
            obj0.CreateConnectors(0, 0);
            obj1.CreateConnectors(0, 0);
            obj2.CreateConnectors(0, 0);
            obj3.CreateConnectors(0, 0);
            obj4.CreateConnectors(0, 0);
            //flowsheet.ConnectObjects(obj0, obj1, 0, 0);
            //flowsheet.ConnectObjects(obj1, obj2, 0, 0);
            //flowsheet.ConnectObjects(obj4, obj1, 0, 0);
            surface.BackgroundColor = SKColors.White;

            if (RunningPlatform() == OSPlatform.Windows)
            {

                DWSIM.UI.Desktop.WPF.StyleSetter.SetStyles();

                var platform = new Eto.Wpf.Platform();

                //to register your custom control handler, call this before using your class:
                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new DWSIM.UI.Desktop.WPF.FlowsheetSurfaceControlHandler());

                new Application(platform).Run(
                    new Form
                    {
                        Title = "Hello!",
                        Content = new FlowsheetSurfaceControl() { FlowsheetSurface = surface, Width = 1000, Height = 500 }
                    }
                );

                //new Application(platform).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Linux)
            {

                DWSIM.UI.Desktop.GTK.StyleSetter.SetStyles();

                var platform = new Eto.GtkSharp.Platform();

                //to register your custom control handler, call this before using your class:
                platform.Add<FlowsheetSurfaceControl.IFlowsheetSurface>(() => new DWSIM.UI.Desktop.GTK.FlowsheetSurfaceControlHandler());
      
                new Application(platform).Run(
                    new Form
                    {
                        Title = "Hello!",
                        Content = new FlowsheetSurfaceControl() { FlowsheetSurface = surface, Width = 1000, Height = 500 }
                    }
                );

                //new Application(platform).Run(new MainForm());
            }
            else if (RunningPlatform() == OSPlatform.Mac)
            {
                new Application(Platforms.XamMac2).Run(new MainForm());
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
