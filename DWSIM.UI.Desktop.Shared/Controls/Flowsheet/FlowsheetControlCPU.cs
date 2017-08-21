using System;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface))]
    public class FlowsheetSurfaceControl : FlowsheetSurfaceControlBase
    {
        public new IFlowsheetSurface Handler { get { return (IFlowsheetSurface)base.Handler; } }

    }
}


