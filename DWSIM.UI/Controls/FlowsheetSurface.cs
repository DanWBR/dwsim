using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Forms.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface))]
    public class FlowsheetSurfaceControl : Eto.Forms.Control
    {
        IFlowsheetSurface Handler { get { return (IFlowsheetSurface)base.Handler; } }

        //public SkiaSharp.SKSurface FlowsheetSurface
        //{
        //    get { return Handler.FlowsheetSurface; }
        //    set { Handler.FlowsheetSurface = value; }
        //}

        // interface to the platform implementations
        public interface IFlowsheetSurface : Eto.Forms.Control.IHandler
        {
            //SkiaSharp.SKSurface FlowsheetSurface { get; set; }
        }
    }
}
