using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.UI.Forms.Controls;
using SkiaSharp;

namespace DWSIM.UI.Desktop.GTK
{
    public class FlowsheetSurfaceControlHandler : Eto.Mac.Forms.MacView< , FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {
        public FlowsheetSurfaceControlHandler()
        {
            this.Control = new FlowsheetSurface_Mac();
        }

        public override Eto.Drawing.Color BackgroundColor
        {
            get
            {
                return Eto.Drawing.Colors.White;
            }
            set
            {
                return;
            }
        }

        public GraphicsSurface FlowsheetSurface
        {
            get
            {
                return ((FlowsheetSurface_Mac)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_Mac)this.Control).fsurface = value;
            }
        }


    }

    public class FlowsheetSurface_Mac : SkiaSharp.Views.Desktop.SKControl
    {

        public GraphicsSurface fsurface;

        private float _lastTouchX;
        private float _lastTouchY;

    }

}
