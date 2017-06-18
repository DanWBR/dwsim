using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System;
using SkiaSharp;
using SkiaSharp.Views.Desktop;
using System.Windows.Media;
using System.Windows;
using DWSIM.Drawing.SkiaSharp;
using DWSIM.Interfaces;
using System.Windows.Media.Imaging;
using DWSIM.UI.Forms.Controls;

namespace DWSIM.UI.Desktop.WPF
{

    public class FlowsheetSurfaceControlHandler : Eto.Wpf.Forms.WpfControl<FrameworkElement, FlowsheetSurfaceControl, FlowsheetSurfaceControl.ICallback>, FlowsheetSurfaceControl.IFlowsheetSurface
    {
        public FlowsheetSurfaceControlHandler(GraphicsSurface gsurf)
        {
            this.Control = new FlowsheetSurface_WPF(gsurf);
        }
    }

    public class FlowsheetSurface_WPF : SkiaSharp.Views.WPF.SKElement
    {

        public GraphicsSurface fsurface;
        private WriteableBitmap bitmap;

        public FlowsheetSurface_WPF(GraphicsSurface gsurf)
        {
            fsurface = gsurf;
        }

        protected override void OnRender(DrawingContext drawingContext)
        {
          
            base.OnRender(drawingContext);

            int width, height;
            double dpiX = 1.0;
            double dpiY = 1.0;
            if (IgnorePixelScaling)
            {
                width = (int)ActualWidth;
                height = (int)ActualHeight;
            }
            else
            {
                var m = PresentationSource.FromVisual(this).CompositionTarget.TransformToDevice;
                dpiX = m.M11;
                dpiY = m.M22;
                width = (int)(ActualWidth * dpiX);
                height = (int)(ActualHeight * dpiY);
            }

            var info = new SKImageInfo(width, height, SKImageInfo.PlatformColorType, SKAlphaType.Premul);

            // reset the bitmap if the size has changed
            if (bitmap == null || info.Width != bitmap.PixelWidth || info.Height != bitmap.PixelHeight)
            {
                bitmap = new WriteableBitmap(width, height, dpiX, dpiY, PixelFormats.Pbgra32, null);
            }

            // draw on the bitmap
            bitmap.Lock();
            using (var surface = SKSurface.Create(info, bitmap.BackBuffer, bitmap.BackBufferStride))
            {
                fsurface.UpdateSurface(surface);
                OnPaintSurface(new SKPaintSurfaceEventArgs(surface, info));
                surface.Canvas.Flush();
            }

            // draw the bitmap to the screen
            bitmap.AddDirtyRect(new Int32Rect(0, 0, width, height));
            bitmap.Unlock();
            drawingContext.DrawImage(bitmap, new Rect(0, 0, ActualWidth, ActualHeight));
        }


    }
}
