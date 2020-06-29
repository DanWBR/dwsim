using Eto.Forms;
using Eto.Wpf.Forms;
using OxyPlot;
using OxyPlot.Wpf;
using ContextMenu = Eto.Forms.ContextMenu;

namespace Eto.OxyPlot.WPF2
{
    public class PlotHandler : Eto.OxyPlot.Wpf.PlotHandler
    {

        public PlotHandler()
        {

            var pview = new global::OxyPlot.Wpf.PlotView();

            Control = pview;

            ContextMenu cmenu = new ContextMenu();

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = (int)pview.ActualWidth, Height = (int)pview.ActualHeight, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Clipboard.SetImage(bitmap);

            })
            { Text = "Copy" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = (int)pview.ActualWidth * 2, Height = (int)pview.ActualHeight * 2, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Clipboard.SetImage(bitmap);

            })
            { Text = "Copy @ 2x" });

            cmenu.Items.Add(new ButtonMenuItem((sender, e) =>
            {

                var pngExporter = new PngExporter { Width = (int)pview.ActualWidth * 3, Height = (int)pview.ActualHeight * 3, Background = OxyColors.White };
                var bitmap = pngExporter.ExportToBitmap(pview.Model);
                System.Windows.Clipboard.SetImage(bitmap);

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

                    var pngExporter = new PngExporter { Width = (int)pview.ActualWidth, Height = (int)pview.ActualHeight, Background = OxyColors.White };
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

                    var pngExporter = new PngExporter { Width = (int)pview.ActualWidth * 2, Height = (int)pview.ActualHeight * 2, Background = OxyColors.White };
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

                    var pngExporter = new PngExporter { Width = (int)pview.ActualWidth * 3, Height = (int)pview.ActualHeight * 3, Background = OxyColors.White };
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

            this.ContainerControl.ContextMenu = (System.Windows.Controls.ContextMenu)cmenu.ControlObject;

        }
    }
}