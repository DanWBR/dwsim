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

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

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

            })
            { Text = "Copy" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                // Get the standard pasteboard
                var pasteboard = NSPasteboard.GeneralPasteboard;

                // Empty the current contents
                pasteboard.ClearContents();

                NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width * 2, Control.Bounds.Height * 2));

                image.LockFocus();

                var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                Control.Layer.RenderInContext(ctx);

                image.UnlockFocus();

                // Add the current image to the pasteboard
                pasteboard.WriteObjects(new NSImage[] { image });

            })
            { Text = "Copy @ 2x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                // Get the standard pasteboard
                var pasteboard = NSPasteboard.GeneralPasteboard;

                // Empty the current contents
                pasteboard.ClearContents();

                NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width * 3, Control.Bounds.Height * 3));

                image.LockFocus();

                var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                Control.Layer.RenderInContext(ctx);

                image.UnlockFocus();

                // Add the current image to the pasteboard
                pasteboard.WriteObjects(new NSImage[] { image });

            })
            { Text = "Copy @ 3x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

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


            })
            { Text = "Save to File" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var sfd = new SaveFileDialog();

                sfd.Title = "Save Chart to PNG";
                sfd.Filters.Add(new FileFilter("PNG File", new string[] { ".png" }));
                sfd.CurrentFilterIndex = 0;

                if (sfd.ShowDialog(this.Widget) == DialogResult.Ok)
                {

                    NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width * 2, Control.Bounds.Height * 2));

                    image.LockFocus();

                    var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                    Control.Layer.RenderInContext(ctx);

                    image.UnlockFocus();

                    var imageRep = new NSBitmapImageRep(image.AsTiff());
                    var pngData = imageRep.RepresentationUsingTypeProperties(NSBitmapImageFileType.Png);
                    pngData.Save(sfd.FileName, false);

                }

            })
            { Text = "Save to File @ 2x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var sfd = new SaveFileDialog();

                sfd.Title = "Save Chart to PNG";
                sfd.Filters.Add(new FileFilter("PNG File", new string[] { ".png" }));
                sfd.CurrentFilterIndex = 0;

                if (sfd.ShowDialog(this.Widget) == DialogResult.Ok)
                {

                    NSImage image = new NSImage(new CoreGraphics.CGSize(Control.Bounds.Width * 3, Control.Bounds.Height * 3));

                    image.LockFocus();

                    var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

                    Control.Layer.RenderInContext(ctx);

                    image.UnlockFocus();

                    var imageRep = new NSBitmapImageRep(image.AsTiff());
                    var pngData = imageRep.RepresentationUsingTypeProperties(NSBitmapImageFileType.Png);
                    pngData.Save(sfd.FileName, false);

                }

            })
            { Text = "Save to File @ 3x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                Control.Model.ResetAllAxes();
                Control.Model.InvalidatePlot(false);

            })
            { Text = "Reset to Default View" });

            this.ContainerControl.Menu = (NSMenu)cmenu.ControlObject;

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