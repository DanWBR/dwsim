using Eto.Forms;
using OxyPlot;
using OxyPlot.WindowsForms;
using System.Windows.Forms;

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
            Control = new global::OxyPlot.WindowsForms.PlotView()
            {
            };
        }
    }
}