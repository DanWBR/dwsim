using DWSIM.Drawing.SkiaSharp;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
using SkiaSharp;
using System;
using DWSIM.UI.Controls;

namespace DWSIM.UI.Desktop.Mac
{
    public class FlowsheetSurfaceControlHandler : Eto.Mac.Forms.MacView<NSView, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
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

        public override NSView ContainerControl
        {
            get
            {
                return Control;
            }
        }

        public override bool Enabled { get; set; }

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
        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_Mac)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_Mac)this.Control).fbase = value;
            }
        }
    }

    public class FlowsheetSurface_Mac : NSView, Eto.Mac.Forms.IMacControl
    {

        private NSTrackingArea trackarea;

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private float _lastTouchX;
        private float _lastTouchY;

        private SKDrawable drawable;

        public FlowsheetSurface_Mac()
        {
            drawable = new SKDrawable();
            BecomeFirstResponder();
        }

        public override CGRect Bounds
        {
            get
            {
                return base.Bounds;
            }
            set
            {
                base.Bounds = value;
                UpdateTrackingAreas();
            }
        }

        public override CGRect Frame
        {
            get
            {
                return base.Frame;
            }

            set
            {
                base.Frame = value;
                UpdateTrackingAreas();
            }
        }

        public override void AwakeFromNib()
        {
            base.AwakeFromNib();
        }

        public override void UpdateTrackingAreas()
        {
            if (trackarea != null){RemoveTrackingArea(trackarea);}
            trackarea = new NSTrackingArea(Frame, NSTrackingAreaOptions.ActiveWhenFirstResponder | NSTrackingAreaOptions.MouseMoved | NSTrackingAreaOptions.InVisibleRect, this, null);
            AddTrackingArea(trackarea);
        }

        public override void DrawRect(CGRect dirtyRect)
        {
            
            base.DrawRect(dirtyRect);
            
            var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

            // create the skia context
            SKImageInfo info;
            
            var surface = drawable.CreateSurface(Bounds, 1.0f, out info);
            
            fsurface.UpdateSurface(surface);

            Console.WriteLine("Redraw");

            // draw the surface to the context
            drawable.DrawSurface(ctx, Bounds, info, surface);
            
        }

        public override void MouseDown(NSEvent theEvent)
        {
            base.MouseDown(theEvent);
            if (theEvent.ClickCount == 2) {
                fsurface.ZoomAll((int)this.Bounds.Width, (int)this.Bounds.Height);
            }
            else {
                _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
                _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
                fsurface.InputPress((int)_lastTouchX, (int)_lastTouchY);
            }
            this.NeedsDisplay = true;
        }

        public override void MouseMoved(NSEvent theEvent)
        {
            base.MouseMoved(theEvent);
            _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
            _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.NeedsDisplay = true;
        }

        public override void MouseDragged(NSEvent theEvent)
        {
            base.MouseDragged(theEvent);
            _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
            _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.NeedsDisplay = true;
        }

        public override void MouseUp(NSEvent theEvent)
        {
            base.MouseUp(theEvent);
            fsurface.InputRelease();
            this.NeedsDisplay = true;
        }

        public override void ScrollWheel(NSEvent theEvent)
        {
            var scroll = theEvent.ScrollingDeltaX;
            fsurface.Zoom += scroll / 100.0f;
            this.NeedsDisplay = true;
        }

        public WeakReference WeakHandler { get; set; }

    }

}
