using System;
using System.Linq;
using SkiaSharp;
using SkiaSharp.Views.Desktop;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.UI.Controls;
using System.Windows.Forms;
using System.Drawing;
using System.Drawing.Imaging;
using OpenTK;
using Eto.WinForms;

namespace DWSIM.UI.Desktop.WinForms
{
    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.WinForms.Forms.WindowsControl<GLControl, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {

        private FlowsheetSurface_WinForms_OpenGL nativecontrol;

        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            nativecontrol = new FlowsheetSurface_WinForms_OpenGL();
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

    }

    public class FlowsheetSurface_WinForms_OpenGL : SkiaSharp.Views.Desktop.SKGLControl
    {

        public bool WPFHost = false;

        public Action<Eto.Forms.MouseEventArgs> WPFMouseDown;
        public Action<Eto.Forms.MouseEventArgs> WPFMouseUp;
        public Action<Eto.Forms.MouseEventArgs> WPFMouseDoubleClick;
        public Action<Eto.Forms.DragEventArgs> WPFDragEnter;
        public Action<Eto.Forms.DragEventArgs> WPFDragOver;
        public Action<Eto.Forms.DragEventArgs> WPFDragDrop;

        public Func<DragEventArgs, Eto.Forms.DragEventArgs> GetEtoDragEventArgs;

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private GRContext grContext;
        private GRBackendRenderTargetDesc renderTarget;

        private float _lastTouchX;
        private float _lastTouchY;

        public FlowsheetSurface_WinForms_OpenGL()
        {
            ResizeRedraw = true;
            AllowDrop = true;
        }

        protected override void OnPaint(System.Windows.Forms.PaintEventArgs e)
        {

            this.MakeCurrent();

            //base.OnPaint(e);

            if (Width == 0) return;
            if (Height == 0) return;

            // create the contexts if not done already
            if (grContext == null)
            {
                var glInterface = GRGlInterface.CreateNativeGlInterface();
                grContext = GRContext.Create(GRBackend.OpenGL, glInterface);

                // get initial details
                renderTarget = CreateRenderTarget();
            
            }

            // update to the latest dimensions
            renderTarget.Width = Width;
            renderTarget.Height = Height;

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

            // update the control
            SwapBuffers();
        }

        protected override void Dispose(bool disposing)
        {
            base.Dispose(disposing);

            // clean up
            if (grContext != null)
            {
                grContext.Dispose();
                grContext = null;
            }
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

        protected override void OnMouseDown(System.Windows.Forms.MouseEventArgs e)
        {
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            _lastTouchX = e.X;
            _lastTouchY = e.Y;
            fsurface.InputPress((int)_lastTouchX, (int)_lastTouchY);
            this.Invalidate();
            if (WPFHost) WPFMouseDown.Invoke(e.ToEto(this));
        }

        protected override void OnMouseUp(System.Windows.Forms.MouseEventArgs e)
        {
            fsurface.InputRelease();
            this.Invalidate();
            if (WPFHost) WPFMouseUp.Invoke(e.ToEto(this));
        }

        protected override void OnMouseMove(System.Windows.Forms.MouseEventArgs e)
        {
            _lastTouchX = e.X;
            _lastTouchY = e.Y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.Invalidate();
        }

        protected override void OnMouseDoubleClick(System.Windows.Forms.MouseEventArgs e)
        {
            //fsurface.ZoomAll((int)this.Width, (int)this.Height);
            //this.Invalidate();
            if (WPFHost) WPFMouseDoubleClick.Invoke(e.ToEto(this));
        }

        protected override void OnMouseWheel(System.Windows.Forms.MouseEventArgs e)
        {
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            var oldzoom = fsurface.Zoom;
            fsurface.Zoom += e.Delta / 4 / 100.0f;
            if (fsurface.Zoom < 0.05) fsurface.Zoom = 0.05f;
            fsurface.CenterTo(oldzoom, e.X, e.Y, Width, Height);
            this.Invalidate();
        }

        protected override void OnDragEnter(DragEventArgs drgevent)
        {
            base.OnDragEnter(drgevent);
            drgevent.Effect = DragDropEffects.All;
            if (WPFHost) WPFDragEnter.Invoke(GetDragEventArgs(drgevent));
        }

        protected override void OnDragOver(DragEventArgs drgevent)
        {
            base.OnDragOver(drgevent);
            if (WPFHost) WPFDragOver.Invoke(GetDragEventArgs(drgevent));
        }

        protected override void OnDragDrop(DragEventArgs drgevent)
        {
            base.OnDragDrop(drgevent);
            if (WPFHost) WPFDragDrop.Invoke(GetDragEventArgs(drgevent));
        }

        Eto.Forms.DragEventArgs GetDragEventArgs(DragEventArgs data)
        {
            var dragData = (data.Data as DataObject).ToEto();
            var sourceWidget = data.Data.GetData("eto.source.control");
            var source = sourceWidget == null ? null : (Eto.Forms.Control)sourceWidget;
            var modifiers = data.GetEtoModifiers();
            var buttons = data.GetEtoButtons();
            var location = PointFromScreen(new Eto.Drawing.PointF(data.X, data.Y));
            return new Eto.Forms.DragEventArgs(source, dragData, data.AllowedEffect.ToEto(), location, modifiers, buttons);
        }

        public virtual Eto.Drawing.PointF PointFromScreen(Eto.Drawing.PointF point)
        {
            return !this.IsDisposed ? this.PointToClient(point.ToSDPoint()).ToEto() : Eto.Drawing.PointF.Empty; // safety check added because this is hit in certain situations.
        }

        public virtual Eto.Drawing.PointF PointToScreen(Eto.Drawing.PointF point)
        {
            return !this.IsDisposed ? this.PointToScreen(point.ToSDPoint()).ToEto() : Eto.Drawing.PointF.Empty; // safety check added because this is hit in certain situations.
        }


    }

}
