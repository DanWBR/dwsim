using Eto.Forms;
using OxyPlot;
using OxyPlot.GtkSharp;
using System.IO;
using System.Linq;

namespace Eto.OxyPlot.Gtk
{
    public class PlotHandler : GtkSharp.Forms.GtkControl<global::OxyPlot.GtkSharp.PlotView, Eto.OxyPlot.Plot, Control.ICallback>, Plot.IHandler
    {
        public PlotModel Model
        {
            get { return Control.Model; }
            set { Control.Model = value; }
        }

        public PlotHandler()
        {
            Control = new global::OxyPlot.GtkSharp.PlotView();

            ContextMenu cmenu = new ContextMenu();

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var sfd = new SaveFileDialog();

                sfd.Title = "Save Chart to PNG";
                sfd.Filters.Add(new FileFilter("PNG File", new string[] { ".png" }));
                sfd.CurrentFilterIndex = 0;

                if (sfd.ShowDialog(this.Widget) == DialogResult.Ok)
                {

                    var pngExporter = new PngExporter { Width = (int)Control.Allocation.Width, Height = (int)Control.Allocation.Height, Background = OxyColors.White };
                    using (var sf = new FileStream(sfd.FileName, FileMode.OpenOrCreate))
                    {
                        pngExporter.Export(Model, sf);
                    }

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

                    var pngExporter = new PngExporter { Width = (int)Control.Allocation.Width * 2, Height = (int)Control.Allocation.Height * 2, Background = OxyColors.White };
                    using (var sf = new FileStream(sfd.FileName, FileMode.OpenOrCreate))
                    {
                        pngExporter.Export(Model, sf);
                    }

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

                    var pngExporter = new PngExporter { Width = (int)Control.Allocation.Width * 3, Height = (int)Control.Allocation.Height * 3, Background = OxyColors.White };

                    using (var sf = new FileStream(sfd.FileName, FileMode.OpenOrCreate))
                    {
                        pngExporter.Export(Model, sf);
                    }

                }


            })
            { Text = "Save to File @ 3x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                Control.Model.ResetAllAxes();
                Control.Model.InvalidatePlot(false);

            })
            { Text = "Reset to Default View" });

            this.EventControl.ButtonPressEvent += (sender, e) =>
            {
                cmenu.Show();
            };

        }
    }
}