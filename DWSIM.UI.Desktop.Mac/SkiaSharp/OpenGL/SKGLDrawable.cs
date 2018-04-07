using System;
using SkiaSharp.Views.GlesInterop;

namespace SkiaSharp.Views.Mac
{
	internal static class SKGLDrawable
	{
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
}