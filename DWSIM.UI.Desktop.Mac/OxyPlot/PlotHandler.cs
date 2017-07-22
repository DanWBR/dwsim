using Eto.Forms;
using MonoMac.AppKit;
using OxyPlot;

namespace Eto.OxyPlot.Mac
{
    public class PlotHandler : Eto.Mac.Forms.MacView<global::OxyPlot.MonoMac.PlotView, Eto.OxyPlot.Plot, Control.ICallback>, Plot.IHandler
    {
        public PlotModel Model
        {
            get { return Control.Model; }
            set { Control.Model = value; }
        }

        public PlotHandler()
        {
            Control = new global::OxyPlot.MonoMac.PlotView();
            {
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