namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface))]
    public class FlowsheetSurfaceControl : Eto.Forms.Control
    {
        IFlowsheetSurface Handler { get { return (IFlowsheetSurface)base.Handler; } }

        public DWSIM.Drawing.SkiaSharp.GraphicsSurface FlowsheetSurface
        {
            get { return Handler.FlowsheetSurface; }
            set { Handler.FlowsheetSurface = value; }
        }

        // interface to the platform implementations
        public interface IFlowsheetSurface : Eto.Forms.Control.IHandler
        {
            DWSIM.Drawing.SkiaSharp.GraphicsSurface FlowsheetSurface { get; set; }
        }
    }
}


