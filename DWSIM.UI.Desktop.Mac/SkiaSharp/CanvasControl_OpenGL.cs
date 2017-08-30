using DWSIM.Drawing.SkiaSharp;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
using SkiaSharp;
using System;
using DWSIM.UI.Controls;
using SkiaSharp.Views.GlesInterop;

namespace DWSIM.UI.Desktop.Mac
{
    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.Mac.Forms.MacView<NSOpenGLView, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {

        private FlowsheetSurface_Mac_OpenGL nativecontrol;

        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            nativecontrol = new FlowsheetSurface_Mac_OpenGL();
            this.Control = nativecontrol;
        }

        public override void OnLoadComplete(EventArgs e)
        {
            base.OnLoadComplete(e);
            nativecontrol.fbase = this.Widget.FlowsheetObject;
            nativecontrol.fsurface = this.Widget.FlowsheetSurface;
        }

        protected override void Initialize()
        {
            base.Initialize();

            Widget.MouseDown += (sender, e) =>
            {
                //nativecontrol._lastTouchX = nativecontrol.ConvertPointFromView(theEvent.LocationInWindow, null).X;
                //nativecontrol._lastTouchY = nativecontrol.Bounds.Height - nativecontrol.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
                nativecontrol._lastTouchX = e.Location.X;
                nativecontrol._lastTouchY = e.Location.Y;
                nativecontrol.fsurface.InputPress((int)(nativecontrol._lastTouchX), (int)(nativecontrol._lastTouchY));
                nativecontrol.NeedsDisplay = true;
                nativecontrol.BecomeFirstResponder();
                nativecontrol.UpdateTrackingAreas();
            };

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
                return ((FlowsheetSurface_Mac_OpenGL)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_Mac_OpenGL)this.Control).fsurface = value;
            }
        }
        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_Mac_OpenGL)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_Mac_OpenGL)this.Control).fbase = value;
            }
        }
    }

    public class FlowsheetSurface_Mac_OpenGL  : SkiaSharp.Views.Mac.SKGLView, Eto.Mac.Forms.IMacControl
    {

        private NSTrackingArea trackarea;

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        public float _lastTouchX;
        public float _lastTouchY;

        public FlowsheetSurface_Mac_OpenGL(): base()
        {
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
                BecomeFirstResponder();
                UpdateTrackingAreas();
            }
        }

        public override void AwakeFromNib()
        {
            base.AwakeFromNib();
        }

        public override void UpdateTrackingAreas()
        {
            if (trackarea != null) { RemoveTrackingArea(trackarea); }
            trackarea = new NSTrackingArea(Frame, NSTrackingAreaOptions.ActiveWhenFirstResponder | NSTrackingAreaOptions.MouseMoved | NSTrackingAreaOptions.InVisibleRect, this, null);
            AddTrackingArea(trackarea);
        }

        public override void DrawRect(CGRect dirtyRect)
        {
            
            base.DrawRect(dirtyRect);

            if (fsurface == null) return;

            var size = ConvertSizeToBacking(Bounds.Size);
            renderTarget.Width = (int)size.Width;
            renderTarget.Height = (int)size.Height;

            Gles.glClear(Gles.GL_STENCIL_BUFFER_BIT);

            using (var surface = SKSurface.Create(context, renderTarget))
            {

                fsurface.UpdateSurface(surface);

                // draw on the surface
                DrawInSurface(surface, renderTarget);

                surface.Canvas.Flush();
            }

            // flush the SkiaSharp contents to GL
            context.Flush();

            OpenGLContext.FlushBuffer();
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
