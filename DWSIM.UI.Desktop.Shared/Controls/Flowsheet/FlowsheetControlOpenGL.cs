using System;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface_OpenGL))]
    public class FlowsheetSurfaceControl_OpenGL : FlowsheetSurfaceControlBase
    {
        public new IFlowsheetSurface_OpenGL Handler { get { return (IFlowsheetSurface_OpenGL)base.Handler; } }

    }
}


