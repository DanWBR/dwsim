using System;
using System.Linq;
using SkiaSharp;
using SkiaSharp.Views.Desktop;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.UI.Controls;
using System.Windows.Forms;
using System.Drawing;
using System.Drawing.Imaging;

namespace DWSIM.UI.Desktop.WinForms
{

    public class FlowsheetSurfaceControlHandler : Eto.WinForms.Forms.WindowsControl<Control, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {
        public FlowsheetSurfaceControlHandler()
        {
            this.Control = new FlowsheetSurface_WinForms();
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
                return ((FlowsheetSurface_WinForms)this.Control).fsurface;
            }
            set
            {
                ((FlowsheetSurface_WinForms)this.Control).fsurface = value;
            }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get
            {
                return ((FlowsheetSurface_WinForms)this.Control).fbase;
            }
            set
            {
                ((FlowsheetSurface_WinForms)this.Control).fbase = value;
            }
        }

    }

    public class FlowsheetSurface_WinForms : SkiaSharp.Views.Desktop.SKControl
    {

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private Bitmap bitmap;

        private float _lastTouchX;
        private float _lastTouchY;
        
        public FlowsheetSurface_WinForms()
        {
        }

        protected override void OnPaint(PaintEventArgs e)
        {

            base.OnPaint(e);

            // get the bitmap
            CreateBitmap();
            var data = bitmap.LockBits(new Rectangle(0, 0, Width, Height), ImageLockMode.WriteOnly, bitmap.PixelFormat);

            // create the surface
            var info = new SKImageInfo(Width, Height, SKImageInfo.PlatformColorType, SKAlphaType.Premul);
            using (var surface = SKSurface.Create(info, data.Scan0, data.Stride))
            {
                // start drawing

                if (fsurface != null) fsurface.UpdateSurface(surface);

                OnPaintSurface(new SKPaintSurfaceEventArgs(surface, info));

                surface.Canvas.Flush();
            }

            // write the bitmap to the graphics
            bitmap.UnlockBits(data);
            e.Graphics.DrawImage(bitmap, 0, 0);
        }

        private void CreateBitmap()
        {
            if (bitmap == null || bitmap.Width != Width || bitmap.Height != Height)
            {
                FreeBitmap();

                bitmap = new Bitmap(Width, Height, PixelFormat.Format32bppPArgb);
            }
        }

        private void FreeBitmap()
        {
            if (bitmap != null)
            {
                bitmap.Dispose();
                bitmap = null;
            }
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
            fsurface.ZoomAll((int)this.Width, (int)this.Height);
            this.Invalidate();
        }

        protected override void OnMouseWheel(MouseEventArgs e)
        {
            fsurface.Zoom += e.Delta / 4 / 100.0f;
            this.Invalidate();
        }

    }

}
