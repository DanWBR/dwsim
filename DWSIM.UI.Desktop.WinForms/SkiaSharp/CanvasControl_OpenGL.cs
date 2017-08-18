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

namespace DWSIM.UI.Desktop.WinForms
{

    public class FlowsheetSurfaceControlHandler_OpenGL : Eto.WinForms.Forms.WindowsControl<GLControl, FlowsheetSurfaceControl_OpenGL, FlowsheetSurfaceControl_OpenGL.ICallback>, FlowsheetSurfaceControl_OpenGL.IFlowsheetSurface_OpenGL
    {
        public FlowsheetSurfaceControlHandler_OpenGL()
        {
            this.Control = new FlowsheetSurface_WinForms_OpenGL();
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
                return ((FlowsheetSurface_WinForms_OpenGL)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_WinForms_OpenGL)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_WinForms_OpenGL)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_WinForms_OpenGL)this.Control).fbase = value;
            }
        }

    }

    public class FlowsheetSurface_WinForms_OpenGL : SkiaSharp.Views.Desktop.SKGLControl
    {

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private GRContext grContext;
        private GRBackendRenderTargetDesc renderTarget;

        private float _lastTouchX;
        private float _lastTouchY;

        public FlowsheetSurface_WinForms_OpenGL()
        {
            ResizeRedraw = true;
        }

        protected override void OnPaint(PaintEventArgs e)
        {
          
            //base.OnPaint(e);

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
                Origin = GRSurfaceOrigin.TopLeft,
                SampleCount = samples,
                StencilBits = stencil,
                RenderTargetHandle = (IntPtr)framebuffer,
            };
        }

        protected override void OnMouseDown(MouseEventArgs e)
        {
            _lastTouchX = e.X;
            _lastTouchY = e.Y;
            fsurface.InputPress((int)_lastTouchX, (int)_lastTouchY);
            this.Invalidate();
        }

        protected override void OnMouseUp(MouseEventArgs e)
        {
            fsurface.InputRelease();
            this.Invalidate();
        }

        protected override void OnMouseMove(MouseEventArgs e)
        {
            _lastTouchX = e.X;
            _lastTouchY = e.Y;
            fsurface.InputMove((int)_lastTouchX, (int)_lastTouchY);
            this.Invalidate();
        }

        protected override void OnMouseDoubleClick(MouseEventArgs e)
        {
            //fsurface.ZoomAll((int)this.Width, (int)this.Height);
            //this.Invalidate();
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            fsurface.Zoom += e.Delta / 4 / 100.0f;
            this.Invalidate();
        }

    }

}
