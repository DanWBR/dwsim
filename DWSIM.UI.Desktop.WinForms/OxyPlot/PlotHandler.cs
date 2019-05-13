using Eto.Forms;
using OxyPlot;
using OxyPlot.WindowsForms;
using ContextMenu = Eto.Forms.ContextMenu;
using DialogResult = Eto.Forms.DialogResult;
using SaveFileDialog = Eto.Forms.SaveFileDialog;

namespace Eto.OxyPlot.WinForms
{
    public class PlotHandler : Eto.WinForms.Forms.WindowsControl<global::OxyPlot.WindowsForms.PlotView, Eto.OxyPlot.Plot, Eto.Forms.Control.ICallback>, Plot.IHandler
    {
        public PlotModel Model
        {
            get { return Control.Model; }
            set { Control.Model = value; }
        }

        public PlotHandler()
        {

            var pview = new global::OxyPlot.WindowsForms.PlotView();

            Control = pview;

            ContextMenu cmenu = new ContextMenu();

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = pview.Width, Height = pview.Height, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Forms.Clipboard.SetImage(bitmap);

            })
            { Text = "Copy" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = pview.Width * 2, Height = pview.Height * 2, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Forms.Clipboard.SetImage(bitmap);

            })
            { Text = "Copy @ 2x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = pview.Width * 3, Height = pview.Height * 3, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Forms.Clipboard.SetImage(bitmap);

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

                    var pngExporter = new PngExporter { Width = pview.Width, Height = pview.Height, Background = OxyColors.White };
                    pngExporter.ExportToFile(Model, sfd.FileName);

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

                    var pngExporter = new PngExporter { Width = pview.Width * 2, Height = pview.Height * 2, Background = OxyColors.White };
                    pngExporter.ExportToFile(Model, sfd.FileName);

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

                    var pngExporter = new PngExporter { Width = pview.Width * 3, Height = pview.Height * 3, Background = OxyColors.White };
                    pngExporter.ExportToFile(Model, sfd.FileName);

                }


            })
            { Text = "Save to File @ 3x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                pview.Model.ResetAllAxes();
                pview.Model.InvalidatePlot(false);

            })
            { Text = "Reset to Default View" });

            this.ContextMenu = cmenu;

        }
    }
}