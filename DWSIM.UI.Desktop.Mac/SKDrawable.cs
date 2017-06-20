using System;
using System.Runtime.InteropServices;
using MonoMac.CoreGraphics;
using SkiaSharp;

namespace DWSIM.UI.Desktop.Mac
{
	internal class SKDrawable : IDisposable
{
    private const int BitsPerByte = 8; // 1 byte = 8 bits
    private const CGBitmapFlags BitmapFlags = CGBitmapFlags.ByteOrder32Big | CGBitmapFlags.PremultipliedLast;

    private IntPtr bitmapData;
    private int lastLength;

    public SKImageInfo Info { get; private set; }

    public SKSurface CreateSurface(CGRect contentsBounds, float scale, out SKImageInfo info)
    {
        // apply a scale
        contentsBounds.Width *= scale;
        contentsBounds.Height *= scale;

        // get context details
        info = new SKImageInfo((int)contentsBounds.Width, (int)contentsBounds.Height, SKColorType.Rgba8888, SKAlphaType.Premul);
        Info = info;

        // allocate a memory block for the drawing process
        var newLength = info.BytesSize;
        if (lastLength != newLength)
        {
            lastLength = newLength;
            if (bitmapData != IntPtr.Zero)
                bitmapData = Marshal.ReAllocCoTaskMem(bitmapData, newLength);
            else
                bitmapData = Marshal.AllocCoTaskMem(newLength);
        }

        return SKSurface.Create(info, bitmapData, info.RowBytes);
    }

    public void DrawSurface(CGContext ctx, CGRect viewBounds, SKImageInfo info, SKSurface surface)
    {
        surface.Canvas.Flush();
        surface.Dispose();

        // draw the image onto the context
        using (var dataProvider = new CGDataProvider(bitmapData, lastLength))
        using (var colorSpace = CGColorSpace.CreateDeviceRGB())
        using (var image = new CGImage(info.Width, info.Height, BitsPerByte, info.BytesPerPixel * BitsPerByte, info.RowBytes, colorSpace, BitmapFlags, dataProvider, null, false, CGColorRenderingIntent.Default))
        {
                // draw the image
				ctx.DrawImage(viewBounds, image);
        }
    }

    public void Dispose()
    {
        // make sure we free the image data
        if (bitmapData != IntPtr.Zero)
        {
            Marshal.FreeCoTaskMem(bitmapData);
            bitmapData = IntPtr.Zero;
        }
    }
}
}