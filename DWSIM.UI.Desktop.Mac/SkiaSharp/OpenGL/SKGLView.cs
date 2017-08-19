using System;
using System.ComponentModel;
using MonoMac.AppKit;
using MonoMac.CoreGraphics;
using MonoMac.Foundation;
using SkiaSharp.Views.GlesInterop;
using SkiaSharp.Views.Mac;

namespace SkiaSharp.Views.Mac
{
    public class SKGLView : NSOpenGLView
    {
        protected GRContext context;
        protected GRBackendRenderTargetDesc renderTarget;

        // created in code
        public SKGLView()
        {
            Initialize();
        }

        // created in code
        public SKGLView(CGRect frame)
            : base(frame)
        {
            Initialize();
        }

        // created via designer
        public SKGLView(IntPtr p)
            : base(p)
        {
        }

        // created via designer
        public override void AwakeFromNib()
        {
            Initialize();
        }

        private void Initialize()
        {
            WantsBestResolutionOpenGLSurface = true;

            var attrs = new NSOpenGLPixelFormatAttribute[]
			{
				//NSOpenGLPixelFormatAttribute.OpenGLProfile, (NSOpenGLPixelFormatAttribute)NSOpenGLProfile.VersionLegacy,
				NSOpenGLPixelFormatAttribute.Accelerated,
				NSOpenGLPixelFormatAttribute.DoubleBuffer,
				NSOpenGLPixelFormatAttribute.Multisample,

				NSOpenGLPixelFormatAttribute.ColorSize, (NSOpenGLPixelFormatAttribute)32,
				NSOpenGLPixelFormatAttribute.AlphaSize, (NSOpenGLPixelFormatAttribute)8,
				NSOpenGLPixelFormatAttribute.DepthSize, (NSOpenGLPixelFormatAttribute)24,
				NSOpenGLPixelFormatAttribute.StencilSize, (NSOpenGLPixelFormatAttribute)8,
				NSOpenGLPixelFormatAttribute.SampleBuffers, (NSOpenGLPixelFormatAttribute)1,
				NSOpenGLPixelFormatAttribute.Samples, (NSOpenGLPixelFormatAttribute)4,
				(NSOpenGLPixelFormatAttribute)0,
			};
            PixelFormat = new NSOpenGLPixelFormat(attrs);
        }

        public SKSize CanvasSize
        {
            get
            {
                return new SKSize(renderTarget.Width, renderTarget.Height);
            }
        }

        public override void PrepareOpenGL()
        {
            base.PrepareOpenGL();

            // create the context
            var glInterface = GRGlInterface.CreateNativeGlInterface();
            context = GRContext.Create(GRBackend.OpenGL, glInterface);

            renderTarget = SKGLDrawable.CreateRenderTarget();
        }

        public event EventHandler<SKPaintGLSurfaceEventArgs> PaintSurface;

        public virtual void DrawInSurface(SKSurface surface, GRBackendRenderTargetDesc renderTarget)
		{
			if (PaintSurface != null) PaintSurface.Invoke(this, new SKPaintGLSurfaceEventArgs(surface, renderTarget));
		}
    }
}