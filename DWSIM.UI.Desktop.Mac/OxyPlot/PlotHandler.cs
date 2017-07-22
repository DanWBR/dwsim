using Eto.Forms;
using Eto.OxyPlot;
using MonoMac.AppKit;
using OxyPlot;

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