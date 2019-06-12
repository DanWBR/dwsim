using Eto.Forms;
using Eto.OxyPlot;
using AppKit;
using OxyPlot;
using System;
using Foundation;
using ObjCRuntime;
using System.IO;

namespace DWSIM.UI.Desktop.Mac
{
    public class PlotHandler : Eto.Mac.Forms.MacView<DWSIM.UI.Desktop.Mac.PlotView, Eto.OxyPlot.Plot, Control.ICallback>, Plot.IHandler
    {
        public PlotModel Model
        {
            get { return Control.Model; }
            set { Control.Model = value; }
        }

        public PlotHandler()
        {
            Control = new DWSIM.UI.Desktop.Mac.PlotView();
            {
            };

            ContextMenu cmenu = new ContextMenu();

            var b1 = new ButtonMenuItem() { Text = "Copy" };
            cmenu.Items.Add(b1);

            b1.Click += (sender, e) =>
            {

                Console.WriteLine(sender.ToString());

                // Get the standard pasteboard
                var pasteboard = NSPasteboard.GeneralPasteboard;

                // Empty the current contents
                pasteboard.ClearContents();

                NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width, Control.Bounds.Height));

                image.LockFocus();

                var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                Control.Layer.RenderInContext(ctx);

                image.UnlockFocus();

                // Add the current image to the pasteboard
                pasteboard.WriteObjects(new NSImage[] { image });

            };
            
            var b4 = new ButtonMenuItem() { Text = "Save to File" };
            cmenu.Items.Add(b4);

            b4.Click += (sender, e) =>
            {

                Console.WriteLine(sender.ToString());

                var sfd = new SaveFileDialog();

                sfd.Title = "Save Chart to PNG";
                sfd.Filters.Add(new FileFilter("PNG File", new string[] { ".png" }));
                sfd.CurrentFilterIndex = 0;

                if (sfd.ShowDialog(this.Widget) == DialogResult.Ok)
                {

                    NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width, Control.Bounds.Height));

                    image.LockFocus();

                    var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                    Control.Layer.RenderInContext(ctx);

                    image.UnlockFocus();

                    var imageRep = new NSBitmapImageRep(image.AsTiff());
                    var pngData = imageRep.RepresentationUsingTypeProperties(NSBitmapImageFileType.Png);
                    pngData.Save(sfd.FileName, false);

                }


            };

            var b7 = new ButtonMenuItem() { Text = "Reset to Default View" };
            cmenu.Items.Add(b7);

            b7.Click += (sender, e) =>
            {

                Console.WriteLine(sender.ToString());
                Control.Model.ResetAllAxes();
                Control.Model.InvalidatePlot(false);

            };

            Control.RightMouseAction = () => {
                cmenu.Show(this.Widget);
            };

        }

        public override NSView ContainerControl
        {
            get
            {
                return Control;
            }
        }

        public override bool Enabled { get; set; }

    }
}