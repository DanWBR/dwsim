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
        
        private FlowsheetSurface_WinForms nativecontrol;

        public FlowsheetSurfaceControlHandler()
        {
            nativecontrol = new FlowsheetSurface_WinForms();
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

        double DpiScale = 1.0;

        public GraphicsSurface fsurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet fbase;

        private Bitmap bitmap;

        private float _lastTouchX;
        private float _lastTouchY;
        
        public FlowsheetSurface_WinForms()
        {
            this.AllowDrop = true;
        }

        protected override void OnDragEnter(DragEventArgs drgevent)
        {
            drgevent.Effect = DragDropEffects.All;
            base.OnDragEnter(drgevent);
        }

        protected override void OnPaint(PaintEventArgs e)
        {

            base.OnPaint(e);

            if (Width == 0) return;
            if (Height == 0) return;

            DpiScale = e.Graphics.DpiX / 96.0;

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
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
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
            fbase?.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout);
            var oldzoom = fsurface.Zoom;
            fsurface.Zoom += e.Delta / 4 / 100.0f;
            if (fsurface.Zoom < 0.05) fsurface.Zoom = 0.05f;
            fsurface.CenterTo(oldzoom, e.X, e.Y, Width, Height);
            this.Invalidate();
        }

    }

}
