using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.UI.Controls;
using SkiaSharp;
using Eto.Forms;

namespace DWSIM.UI.Desktop.GTK
{
    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.GtkSharp.Forms.GtkControl<OpenTK.GLWidget, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {

        private FlowsheetSurface_GTK_OpenGL nativecontrol;

        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            nativecontrol = new FlowsheetSurface_GTK_OpenGL();
            this.Control = nativecontrol;
        }

        public override void OnLoadComplete(EventArgs e)
        {
            base.OnLoadComplete(e);
            nativecontrol.fbase = this.Widget.FlowsheetObject;
            nativecontrol.fsurface = this.Widget.FlowsheetSurface;
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
                return ((FlowsheetSurface_GTK_OpenGL)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_GTK_OpenGL)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_GTK_OpenGL)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_GTK_OpenGL)this.Control).fbase = value;
            }
        }

    }

    public class FlowsheetSurface_GTK_OpenGL : OpenTK.GLWidget
    {

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private GRContext grContext;
        private GRBackendRenderTargetDesc renderTarget;

        private float _lastTouchX;
        private float _lastTouchY;

        public FlowsheetSurface_GTK_OpenGL()
            : base()
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

        protected override void OnRenderFrame()
        {

            var rect = Allocation;

            // create the contexts if not done already
            if (grContext == null)
            {
                var glInterface = GRGlInterface.CreateNativeGlInterface();

                if (glInterface == null)
                {
                    Console.WriteLine("Error creating OpenGL ES interface. Check if you have OpenGL ES correctly installed and configured or change the PFD Renderer to 'Software (CPU)' on the Global Settings panel.", "Error Creating OpenGL ES interface");
                    System.Diagnostics.Process.GetCurrentProcess().Kill();
                }
                else
                {
                    grContext = GRContext.Create(GRBackend.OpenGL, glInterface);
                }

                try
                {
                    renderTarget = CreateRenderTarget();
                }
                catch (Exception ex)
                {
                    Console.WriteLine("Error creating OpenGL ES render target. Check if you have OpenGL ES correctly installed and configured or change the PFD Renderer to 'Software (CPU)' on the Global Settings panel.\nError message:\n" + ex.ToString());
                    System.Diagnostics.Process.GetCurrentProcess().Kill();
                }

            }

            if (grContext != null)
            {
                // update to the latest dimensions
                renderTarget.Width = rect.Width;
                renderTarget.Height = rect.Height;

                // create the surface
                using (var surface = SKSurface.Create(grContext, renderTarget))
                {

                    surface.Canvas.Clear(SKColors.White);

                    // start drawing
                    if (fsurface != null) fsurface.UpdateSurface(surface);

                    // start drawing
                    OnPaintSurface(new SKPaintGLSurfaceEventArgs(surface, renderTarget));

                    surface.Canvas.Flush();
                }
            }

        }

        public override void Dispose()
        {

            base.Dispose();

            // clean up
            if (grContext != null)
            {
                grContext.Dispose();
                grContext = null;
            }
        }

        public event EventHandler<SKPaintGLSurfaceEventArgs> PaintSurface;

        protected virtual void OnPaintSurface(SKPaintGLSurfaceEventArgs e)
        {
            if (PaintSurface != null) PaintSurface.Invoke(this, e);
        }

        public static GRBackendRenderTargetDesc CreateRenderTarget()
        {

            int framebuffer, stencil, samples;
            Gles.glGetIntegerv(Gles.GL_FRAMEBUFFER_BINDING, out framebuffer);
            Gles.glGetIntegerv(Gles.GL_STENCIL_BITS, out stencil);
            Gles.glGetIntegerv(Gles.GL_SAMPLES, out samples);

            int bufferWidth = 0;
            int bufferHeight = 0;

            return new GRBackendRenderTargetDesc
            {
                Width = bufferWidth,
                Height = bufferHeight,
                Config = GRPixelConfig.Rgba8888,
                Origin = GRSurfaceOrigin.BottomLeft,
                SampleCount = samples,
                StencilBits = stencil,
                RenderTargetHandle = (IntPtr)framebuffer,
            };
        }

    }

    public class SKPaintGLSurfaceEventArgs : EventArgs
    {
        public SKPaintGLSurfaceEventArgs(SKSurface surface, GRBackendRenderTargetDesc renderTarget)
        {
            Surface = surface;
            RenderTarget = renderTarget;
        }

        public SKSurface Surface { get; private set; }

        public GRBackendRenderTargetDesc RenderTarget { get; private set; }
    }

}
