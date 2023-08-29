using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.UI.Controls;
using SkiaSharp;
using Eto.Forms;
using Eto.GtkSharp.Forms;
using Eto.Drawing;
using Eto.GtkSharp;
using Pango;

namespace DWSIM.UI.Desktop.GTK
{
    public class FlowsheetSurfaceControlHandler : Eto.GtkSharp.Forms.GtkControl<Gtk.EventBox, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {
        private FlowsheetSurface_GTK nativecontrol;

        public FlowsheetSurfaceControlHandler()
        {
            nativecontrol = new FlowsheetSurface_GTK();
            this.Control = nativecontrol;
        }

        public override void OnLoadComplete(EventArgs e)
        {
            base.OnLoadComplete(e);
            nativecontrol.fbase = this.Widget.FlowsheetObject;
            nativecontrol.fsurface = this.Widget.FlowsheetSurface;
            //nativecontrol.DragDataGet += (sender, e2) => {
            //    Console.WriteLine(e2.SelectionData.Type.Name);
            //};
            //nativecontrol.DragEnd += (sender, e2) => {
            //    foreach (var item in e2.Args)
            //    {
            //        Console.WriteLine(item.ToString());
            //    }
            //};
            //nativecontrol.DragFailed += (sender, e2) => {
            //    foreach (var item in e2.Args)
            //    {
            //        Console.WriteLine(item.ToString());
            //    }
            //};
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
                return ((FlowsheetSurface_GTK)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_GTK)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_GTK)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_GTK)this.Control).fbase = value;
            }
        }
        
    }

    public class FlowsheetSurface_GTK : Gtk.EventBox
    {

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private float _lastTouchX;
        private float _lastTouchY;

        public FlowsheetSurface_GTK()
        {
            this.AddEvents((int)Gdk.EventMask.PointerMotionMask);
            this.ButtonPressEvent += FlowsheetSurface_GTK_ButtonPressEvent;
            this.ButtonReleaseEvent += FlowsheetSurface_GTK_ButtonReleaseEvent;
            this.MotionNotifyEvent += FlowsheetSurface_GTK_MotionNotifyEvent;
            this.ScrollEvent += FlowsheetSurface_GTK_ScrollEvent;

            var targets = new List<Gtk.TargetEntry>();
            targets.Add(new Gtk.TargetEntry("ObjectName", 0, 1));
            Gtk.Drag.DestSet(this, Gtk.DestDefaults.All, targets.ToArray(), Gdk.DragAction.Copy | Gdk.DragAction.Link | Gdk.DragAction.Move);

        }

        void FlowsheetSurface_GTK_ScrollEvent(object o, Gtk.ScrollEventArgs args)
        {
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);

            var oldzoom = fsurface.Zoom;

            if (args.Event.Direction == Gdk.ScrollDirection.Down)
            {
                fsurface.Zoom += -5 / 100f;
            }
            else
            {
                fsurface.Zoom += 5 / 100f;
            }
            if (fsurface.Zoom < 0.05) fsurface.Zoom = 0.05f;

            int x = (int)args.Event.X;
            int y = (int)args.Event.Y;

            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);

            fsurface.CenterTo(oldzoom, x, y, this.WidthRequest, this.HeightRequest);

            this.QueueDraw();
        }

        void FlowsheetSurface_GTK_MotionNotifyEvent(object o, Gtk.MotionNotifyEventArgs args)
        {
            float x = (int)args.Event.X;
            float y = (int)args.Event.Y;
            _lastTouchX = x;
            _lastTouchY = y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.QueueDraw();
        }

        void FlowsheetSurface_GTK_ButtonReleaseEvent(object o, Gtk.ButtonReleaseEventArgs args)
        {
            fsurface.InputRelease();
            this.QueueDraw();
        }

        void FlowsheetSurface_GTK_ButtonPressEvent(object o, Gtk.ButtonPressEventArgs args)
        {
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            if (args.Event.Type == Gdk.EventType.TwoButtonPress)
            {
                //if (args.Event.State == Gdk.ModifierType.ShiftMask)
                //{
                //    fsurface.Zoom = 1.0f;
                //}
                //else {
                //    fsurface.ZoomAll((int)this.Allocation.Width, (int)this.Allocation.Height);
                //}
            }
            else
            {
                _lastTouchX = (int)args.Event.X;
                _lastTouchY = (int)args.Event.Y;
                fsurface.InputPress((int)_lastTouchX, (int)_lastTouchY);
            }
            this.QueueDraw();

        }

        protected override bool OnExposeEvent(Gdk.EventExpose evnt)
        {

            var rect = Allocation;

            if (rect.Width > 0 && rect.Height > 0)
            {
                var area = evnt.Area;
                SKColorType ctype = SKColorType.Bgra8888;
                if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac) ctype = SKColorType.Rgba8888;
                using (Cairo.Context cr = Gdk.CairoHelper.Create(base.GdkWindow))
                {
                    if (cr == null) { Console.WriteLine("Cairo Context is null"); }
                    using (var bitmap = new SKBitmap(rect.Width, rect.Height, ctype, SKAlphaType.Premul))
                    {
                        if (bitmap == null) { Console.WriteLine("Bitmap is null"); }
                        IntPtr len;
                        using (var skSurface = SKSurface.Create(bitmap.Info.Width, bitmap.Info.Height, ctype, SKAlphaType.Premul, bitmap.GetPixels(out len), bitmap.Info.RowBytes))
                        {
                            if (skSurface == null) { Console.WriteLine("skSurface is null"); }
                            if (fsurface != null) fsurface.UpdateSurface(skSurface);
                            skSurface.Canvas.Flush();
                            using (Cairo.Surface surface = new Cairo.ImageSurface(bitmap.GetPixels(out len), Cairo.Format.Argb32, bitmap.Width, bitmap.Height, bitmap.Width * 4))
                            {
                                surface.MarkDirty();
                                cr.SetSourceSurface(surface, 0, 0);
                                cr.Paint();
                            }
                        }
                    }
                }

            }
            
            return true;
        }

    }

}
