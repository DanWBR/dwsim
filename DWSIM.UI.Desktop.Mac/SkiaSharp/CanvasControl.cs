using DWSIM.Drawing.SkiaSharp;
using AppKit;
using CoreGraphics;
using SkiaSharp;
using System;
using DWSIM.UI.Controls;
using System.Linq;
using DWSIM.UI.Desktop.Mac.TouchBar;
using Foundation;

namespace DWSIM.UI.Desktop.Mac
{
    public class FlowsheetSurfaceControlHandler : Eto.Mac.Forms.MacView<NSView, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {

        private FlowsheetSurface_Mac nativecontrol;

        public FlowsheetSurfaceControlHandler()
        {
            nativecontrol = new FlowsheetSurface_Mac();
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
                if (GlobalSettings.Settings.EnableCustomTouchBar)
                {
                    if (NSProcessInfo.ProcessInfo.IsOperatingSystemAtLeastVersion(new NSOperatingSystemVersion(10, 12, 2)))
                    {
                        try
                        {
                            BindTouchBar();
                            nativecontrol.SetTouchBar(null);
                        }
                        catch { }
                    }
                }
                Widget.FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
                var scale = (float)GlobalSettings.Settings.DpiScale;
                nativecontrol._lastTouchX = e.Location.X * scale;
                nativecontrol._lastTouchY = e.Location.Y * scale;
                nativecontrol.fsurface.InputPress((int)(nativecontrol._lastTouchX), (int)(nativecontrol._lastTouchY));
                nativecontrol.NeedsDisplay = true;
                nativecontrol.BecomeFirstResponder();
                nativecontrol.UpdateTrackingAreas();
            };
        
        }

        public void BindTouchBar()
        {
            nativecontrol.Window.Unbind(new NSString("touchBar"));
            nativecontrol.Window.Bind(new NSString("touchBar"), nativecontrol, new NSString("touchBar"), null);
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

        public nfloat _lastTouchX;
        public nfloat _lastTouchY;

        private SKDrawable drawable;

        public FlowsheetSurface_Mac()
        {
            drawable = new SKDrawable();
            //GlobalSettings.Settings.DpiScale = 1.0f;
            GlobalSettings.Settings.DpiScale = (float)AppKit.NSScreen.MainScreen.BackingScaleFactor;
            BecomeFirstResponder();
            RegisterForDraggedTypes(new string[] {"ObjectName" });
        }

        public override NSDragOperation DraggingEntered(NSDraggingInfo sender)
        {
            if (sender.DraggingPasteboard.Types.Contains("ObjectName"))
                return NSDragOperation.Generic;
            else return NSDragOperation.None;
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
                BecomeFirstResponder();
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
            if (trackarea != null) { RemoveTrackingArea(trackarea); }
            NSTrackingAreaOptions options = NSTrackingAreaOptions.ActiveAlways |  NSTrackingAreaOptions.InVisibleRect |
                         NSTrackingAreaOptions.MouseEnteredAndExited | NSTrackingAreaOptions.MouseMoved;
            trackarea = new NSTrackingArea(Frame, options, this, null);
            AddTrackingArea(trackarea);
        }

        public override void DrawRect(CGRect dirtyRect)
        {

            base.DrawRect(dirtyRect);

            if (fsurface == null) return;

            var ctx = NSGraphicsContext.CurrentContext.GraphicsPort;

            // create the skia context
            SKImageInfo info;

            var surface = drawable.CreateSurface(Bounds, (float)GlobalSettings.Settings.DpiScale, out info);

            fsurface.UpdateSurface(surface);

            // draw the surface to the context
            drawable.DrawSurface(ctx, Bounds, info, surface);

        }

        public override void MouseMoved(NSEvent theEvent)
        {
            try
            {
                base.MouseMoved(theEvent);
                var scale = (float)GlobalSettings.Settings.DpiScale;
                _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
                _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
                _lastTouchX *= scale;
                _lastTouchY *= scale;
                fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
                this.NeedsDisplay = true;
            }
            catch (Exception ex){
                //Console.WriteLine(ex.ToString());
            }
        }

        public override void MouseDragged(NSEvent theEvent)
        {
            base.MouseDragged(theEvent);
            var scale = (float)GlobalSettings.Settings.DpiScale;
            _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
            _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
            _lastTouchX *= scale;
            _lastTouchY *= scale;
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
            fbase.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            var oldzoom = fsurface.Zoom;
            var scroll = theEvent.ScrollingDeltaX;
            fsurface.Zoom += (float)(scroll / 100.0f);
            if (fsurface.Zoom < 0.05) fsurface.Zoom = 0.05f;
            var scale = (float)GlobalSettings.Settings.DpiScale;
            _lastTouchX = this.ConvertPointFromView(theEvent.LocationInWindow, null).X;
            _lastTouchY = Bounds.Height - this.ConvertPointFromView(theEvent.LocationInWindow, null).Y;
            _lastTouchX *= scale;
            _lastTouchY *= scale;
            fbase.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            fsurface.CenterTo(oldzoom, (int)_lastTouchX, (int)_lastTouchY,(int)Bounds.Width, (int)Bounds.Height);
            this.NeedsDisplay = true;
        }

        public WeakReference WeakHandler { get; set; }

        [Export("makeTouchBar")]
        public NSTouchBar MakeTouchBar()
        {

            var bar = new NSTouchBar()
            {
                Delegate = new FlowsheetTouchBarDelegate() { Flowsheet = fbase },
            };

            bar.DefaultItemIdentifiers = new string[] { "0", "1", "2", "3", "4", "5" };
            return bar;
        }

    }

}
