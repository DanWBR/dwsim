// --------------------------------------------------------------------------------------------------------------------
// <copyright file="CanvasRenderContext.cs" company="OxyPlot">
//   Copyright (c) 2014 OxyPlot contributors
// </copyright>
// <summary>
//   Provides a render context for Android.Graphics.Canvas.
// </summary>
// --------------------------------------------------------------------------------------------------------------------

// 2017-09-02 Modified/Updated for DWSIM by Daniel Wagner

using System;
using System.Collections.Generic;
using System.Linq;
using OxyPlot;
using SkiaSharp;

namespace DWSIM.Drawing.SkiaSharp.Renderers
{

    /// <summary>
    /// Provides a render context for Android.Graphics.Canvas.
    /// </summary>
    public class SKCanvasRenderContext : RenderContextBase
    {
        /// <summary>
        /// The images in use
        /// </summary>
        private readonly HashSet<OxyImage> imagesInUse = new HashSet<OxyImage>();

        /// <summary>
        /// The image cache
        /// </summary>
        private readonly Dictionary<OxyImage, SKBitmap> imageCache = new Dictionary<OxyImage, SKBitmap>();

        /// <summary>
        /// The current paint.
        /// </summary>
        private readonly SKPaint paint;

        /// <summary>
        /// A reusable path object.
        /// </summary>
        private readonly SKPath path;

        /// <summary>
        /// A reusable bounds rectangle.
        /// </summary>
        private readonly SKRect bounds;

        /// <summary>
        /// A reusable list of points.
        /// </summary>
        private readonly List<float> pts;

        /// <summary>
        /// The canvas to draw on.
        /// </summary>
        private SKCanvas canvas;

        /// <summary>
        /// Initializes a new instance of the <see cref="SKCanvasRenderContext" /> class.
        /// </summary>
        /// <param name="scale">The scale.</param>
        public SKCanvasRenderContext(double scale)
        {
            this.paint = new SKPaint();
            this.path = new SKPath();
            this.bounds = new SKRect();
            this.pts = new List<float>();
            this.Scale = scale;
        }

        /// <summary>
        /// Gets the factor that this.Scales from OxyPlot´s device independent pixels (96 dpi) to 
        /// Android´s density-independent pixels (160 dpi).
        /// </summary>
        /// <remarks>See <a href="http://developer.android.com/guide/practices/screens_support.html">Supporting multiple screens.</a>.</remarks>
        public double Scale { get; private set; }

        /// <summary>
        /// Sets the target.
        /// </summary>
        /// <param name="c">The canvas.</param>
        public void SetTarget(SKCanvas c)
        {
            this.canvas = c;
        }

        /// <summary>
        /// Resets the paint state
        /// </summary>
        private void Reset()
        {
            this.paint.Style = SKPaintStyle.Fill;
            this.paint.StrokeCap = SKStrokeCap.Butt;
            this.paint.StrokeJoin = SKStrokeJoin.Miter;
            this.paint.TextAlign = SKTextAlign.Left;
            this.paint.Typeface = null;
            this.paint.StrokeWidth = 1.0f;
            this.paint.StrokeMiter = 4.0f;
            this.paint.TextSize = 20.0f;
            this.paint.TextScaleX = 1.0f;
            this.paint.TextSkewX = 0.0f;
            this.paint.BlendMode = SKBlendMode.SrcOver;
            this.paint.ColorFilter = null;
            this.paint.Shader = null;
            this.paint.PathEffect = null;
            this.paint.MaskFilter = null;
            this.paint.HintingLevel = SKPaintHinting.Normal;
            switch (GlobalSettings.Settings.RunningPlatform())
            {
                case GlobalSettings.Settings.Platform.Windows:
                    this.paint.Typeface = SKTypeface.FromFamilyName("Segoe UI", SKTypefaceStyle.Normal);
                    break;
                case GlobalSettings.Settings.Platform.Linux:
                    this.paint.Typeface = SKTypeface.FromFamilyName("Ubuntu", SKTypefaceStyle.Normal);
                    break;
                case GlobalSettings.Settings.Platform.Mac:
                    this.paint.Typeface = SKTypeface.FromFamilyName("Helvetica Neue", SKTypefaceStyle.Normal);
                    break;
            }
            this.paint.Color = GraphicsSurface.ForegroundColor;
        }

        /// <summary>
        /// Draws an ellipse.
        /// </summary>
        /// <param name="rect">The rectangle.</param>
        /// <param name="fill">The fill color.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The thickness.</param>
        public override void DrawEllipse(OxyRect rect, OxyColor fill, OxyColor stroke, double thickness)
        {
            this.Reset();
            {
                if (fill.IsVisible())
                {
                    this.SetFill(fill);
                    this.canvas.DrawOval(this.Convert(rect), this.paint);
                }

                if (stroke.IsVisible())
                {
                    this.SetStroke(stroke, thickness);
                    this.canvas.DrawOval(this.Convert(rect), this.paint);
                }
            }
        }

        /// <summary>
        /// Draws the collection of ellipses, where all have the same stroke and fill.
        /// This performs better than calling DrawEllipse multiple times.
        /// </summary>
        /// <param name="rectangles">The rectangles.</param>
        /// <param name="fill">The fill color.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        public override void DrawEllipses(IList<OxyRect> rectangles, OxyColor fill, OxyColor stroke, double thickness)
        {
            this.Reset();
            {
                foreach (var rect in rectangles)
                {
                    if (fill.IsVisible())
                    {
                        this.SetFill(fill);
                        this.canvas.DrawOval(this.Convert(rect), this.paint);
                    }

                    if (stroke.IsVisible())
                    {
                        this.SetStroke(stroke, thickness);
                        this.canvas.DrawOval(this.Convert(rect), this.paint);
                    }
                }
            }
        }

        /// <summary>
        /// Draws a polyline.
        /// </summary>
        /// <param name="points">The points.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        /// <param name="dashArray">The dash array.</param>
        /// <param name="lineJoin">The line join type.</param>
        /// <param name="aliased">if set to <c>true</c> the shape will be aliased.</param>
        public override void DrawLine(IList<ScreenPoint> points, OxyColor stroke, double thickness, double[] dashArray, LineJoin lineJoin, bool aliased)
        {
            this.Reset();
            {
                this.path.Reset();
                {
                    this.SetPath(points, aliased);
                    this.SetStroke(stroke, thickness, dashArray, lineJoin, aliased);
                    this.canvas.DrawPath(this.path, this.paint);
                }
            }
        }

        /// <summary>
        /// Draws multiple line segments defined by points (0,1) (2,3) (4,5) etc.
        /// This should have better performance than calling DrawLine for each segment.
        /// </summary>
        /// <param name="points">The points.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        /// <param name="dashArray">The dash array.</param>
        /// <param name="lineJoin">The line join type.</param>
        /// <param name="aliased">If set to <c>true</c> the shape will be aliased.</param>
        public override void DrawLineSegments(IList<ScreenPoint> points, OxyColor stroke, double thickness, double[] dashArray, LineJoin lineJoin, bool aliased)
        {
            this.Reset();
            {
                this.SetStroke(stroke, thickness, dashArray, lineJoin, aliased);

                if (aliased)
                {
                    for (int i = 0; i < points.Count; i += 2)
                    {
                        var pt0 = points[i];
                        var pt1 = points[i + 1];

                        this.canvas.DrawLine(this.ConvertAliased(pt0.X), this.ConvertAliased(pt0.Y), this.ConvertAliased(pt1.X), this.ConvertAliased(pt1.Y), this.paint);
                    }
                }
                else
                {
                    for (int i = 0; i < points.Count; i += 2)
                    {
                        var pt0 = points[i];
                        var pt1 = points[i + 1];

                        this.canvas.DrawLine(this.Convert(pt0.X), this.Convert(pt0.Y), this.Convert(pt1.X), this.Convert(pt1.Y), this.paint);
                    }
                }
            }
        }

        /// <summary>
        /// Draws a polygon. The polygon can have stroke and/or fill.
        /// </summary>
        /// <param name="points">The points.</param>
        /// <param name="fill">The fill color.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        /// <param name="dashArray">The dash array.</param>
        /// <param name="lineJoin">The line join type.</param>
        /// <param name="aliased">If set to <c>true</c> the shape will be aliased.</param>
        public override void DrawPolygon(IList<ScreenPoint> points, OxyColor fill, OxyColor stroke, double thickness, double[] dashArray, LineJoin lineJoin, bool aliased)
        {
            this.Reset();
            {
                this.path.Reset();
                {
                    this.SetPath(points, aliased);
                    this.path.Close();

                    if (fill.IsVisible())
                    {
                        this.SetFill(fill);
                        this.canvas.DrawPath(this.path, this.paint);
                    }

                    if (stroke.IsVisible())
                    {
                        this.SetStroke(stroke, thickness, dashArray, lineJoin, aliased);
                        this.canvas.DrawPath(this.path, this.paint);
                    }
                }
            }
        }

        /// <summary>
        /// Draws a rectangle.
        /// </summary>
        /// <param name="rect">The rectangle.</param>
        /// <param name="fill">The fill color.</param>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        public override void DrawRectangle(OxyRect rect, OxyColor fill, OxyColor stroke, double thickness)
        {
            this.Reset();
            {
                if (fill.IsVisible())
                {
                    this.SetFill(fill);
                    this.canvas.DrawRect(new SKRect(this.ConvertAliased(rect.Left), this.ConvertAliased(rect.Top), this.ConvertAliased(rect.Right), this.ConvertAliased(rect.Bottom)), this.paint);
                }

                if (stroke.IsVisible())
                {
                    this.SetStroke(stroke, thickness, aliased: true);
                    this.canvas.DrawRect(new SKRect(this.ConvertAliased(rect.Left), this.ConvertAliased(rect.Top), this.ConvertAliased(rect.Right), this.ConvertAliased(rect.Bottom)), this.paint);
                }
            }
        }

        /// <summary>
        /// Draws the text.
        /// </summary>
        /// <param name="p">The position of the text.</param>
        /// <param name="text">The text.</param>
        /// <param name="fill">The fill color.</param>
        /// <param name="fontFamily">The font family.</param>
        /// <param name="fontSize">Size of the font.</param>
        /// <param name="fontWeight">The font weight.</param>
        /// <param name="rotate">The rotation angle.</param>
        /// <param name="halign">The horizontal alignment.</param>
        /// <param name="valign">The vertical alignment.</param>
        /// <param name="maxSize">The maximum size of the text.</param>
        public override void DrawText(ScreenPoint p, string text, OxyColor fill, string fontFamily, double fontSize, double fontWeight, double rotate, HorizontalAlignment halign, VerticalAlignment valign, OxySize? maxSize)
        {
            this.Reset();
            {
                this.paint.TextSize = this.Convert(fontSize);
                this.SetFill(fill);

                float width;
                float height;
                float lineHeight, delta;
                this.GetFontMetrics(this.paint, out lineHeight, out delta);
                if (maxSize.HasValue || halign != HorizontalAlignment.Left || valign != VerticalAlignment.Bottom)
                {
                    width = this.paint.MeasureText(text);
                    height = lineHeight;
                }
                else
                {
                    width = height = 0f;
                }

                if (maxSize.HasValue)
                {
                    var maxWidth = this.Convert(maxSize.Value.Width);
                    var maxHeight = this.Convert(maxSize.Value.Height);

                    if (width > maxWidth)
                    {
                        width = maxWidth;
                    }

                    if (height > maxHeight)
                    {
                        height = maxHeight;
                    }
                }

                var dx = halign == HorizontalAlignment.Left ? 0d : (halign == HorizontalAlignment.Center ? -width * 0.5 : -width);
                var dy = valign == VerticalAlignment.Bottom ? 0d : (valign == VerticalAlignment.Middle ? height * 0.5 : height);
                var x0 = -this.bounds.Left;
                var y0 = delta;

                this.canvas.Save();
                this.canvas.Translate(this.Convert(p.X), this.Convert(p.Y));
                this.canvas.RotateDegrees((float)rotate);
                this.canvas.Translate((float)dx + x0, (float)dy + y0);

                if (maxSize.HasValue)
                {
                    var x1 = -x0;
                    var y1 = -height - y0;
                    this.canvas.ClipRect(new SKRect(x1, y1, x1 + width, y1 + height));
                    this.canvas.Translate(0, lineHeight - height);
                }

                this.canvas.DrawText(text, 0, 0, this.paint);
                this.canvas.Restore();
            }
        }

        /// <summary>
        /// Measures the text.
        /// </summary>
        /// <param name="text">The text.</param>
        /// <param name="fontFamily">The font family.</param>
        /// <param name="fontSize">Size of the font.</param>
        /// <param name="fontWeight">The font weight.</param>
        /// <returns>The text size.</returns>
        public override OxySize MeasureText(string text, string fontFamily, double fontSize, double fontWeight)
        {
            if (string.IsNullOrEmpty(text))
            {
                return OxySize.Empty;
            }

            this.Reset();
            {
                this.paint.IsAntialias = true;
                this.paint.TextSize = this.Convert(fontSize);
                float lineHeight, delta;
                this.GetFontMetrics(this.paint, out lineHeight, out delta);
                var textWidth = this.paint.MeasureText(text);
                return new OxySize(textWidth / this.Scale, lineHeight / this.Scale);
            }
        }

        /// <summary>
        /// Sets the clip rectangle.
        /// </summary>
        /// <param name="rect">The clip rectangle.</param>
        /// <returns>True if the clip rectangle was set.</returns>
        public override bool SetClip(OxyRect rect)
        {
            this.canvas.Save();
            this.canvas.ClipRect(this.Convert(rect));
            return true;
        }

        /// <summary>
        /// Resets the clip rectangle.
        /// </summary>
        public override void ResetClip()
        {
            this.canvas.Restore();
        }

        /// <summary>
        /// Draws the specified portion of the specified <see cref="OxyImage" /> at the specified location and with the specified size.
        /// </summary>
        /// <param name="source">The source.</param>
        /// <param name="srcX">The x-coordinate of the upper-left corner of the portion of the source image to draw.</param>
        /// <param name="srcY">The y-coordinate of the upper-left corner of the portion of the source image to draw.</param>
        /// <param name="srcWidth">Width of the portion of the source image to draw.</param>
        /// <param name="srcHeight">Height of the portion of the source image to draw.</param>
        /// <param name="destX">The x-coordinate of the upper-left corner of drawn image.</param>
        /// <param name="destY">The y-coordinate of the upper-left corner of drawn image.</param>
        /// <param name="destWidth">The width of the drawn image.</param>
        /// <param name="destHeight">The height of the drawn image.</param>
        /// <param name="opacity">The opacity.</param>
        /// <param name="interpolate">interpolate if set to <c>true</c>.</param>
        public override void DrawImage(
            OxyImage source,
            double srcX,
            double srcY,
            double srcWidth,
            double srcHeight,
            double destX,
            double destY,
            double destWidth,
            double destHeight,
            double opacity,
            bool interpolate)
        {
            var image = this.GetImage(source);
            if (image == null)
            {
                return;
            }

            var src = new SKRect((int)srcX, (int)srcY, (int)(srcX + srcWidth), (int)(srcY + srcHeight));
            var dest = new SKRect(this.Convert(destX), this.Convert(destY), this.Convert(destX + destWidth), this.Convert(destY + destHeight));

            this.Reset();

            // TODO: support opacity
            this.canvas.DrawBitmap(image, src, dest, this.paint);
        }

        /// <summary>
        /// Cleans up resources not in use.
        /// </summary>
        /// <remarks>This method is called at the end of each rendering.</remarks>
        public override void CleanUp()
        {
            var imagesToRelease = this.imageCache.Keys.Where(i => !this.imagesInUse.Contains(i)).ToList();
            foreach (var i in imagesToRelease)
            {
                var image = this.GetImage(i);
                image.Dispose();
                this.imageCache.Remove(i);
            }

            this.imagesInUse.Clear();
        }

        /// <summary>
        /// Gets font metrics for the font in the specified paint.
        /// </summary>
        /// <param name="paint">The paint.</param>
        /// <param name="defaultLineHeight">Default line height.</param>
        /// <param name="delta">The vertical delta.</param>
        private void GetFontMetrics(SKPaint paint, out float defaultLineHeight, out float delta)
        {
            var metrics = paint.FontMetrics;
            var ascent = -metrics.Ascent;
            var descent = metrics.Descent;
            var leading = metrics.Leading;

            //// http://stackoverflow.com/questions/5511830/how-does-line-spacing-work-in-core-text-and-why-is-it-different-from-nslayoutm

            leading = leading < 0 ? 0 : (float)Math.Floor(leading + 0.5f);
            var lineHeight = (float)Math.Floor(ascent + 0.5f) + (float)Math.Floor(descent + 0.5) + leading;
            var ascenderDelta = leading >= 0 ? 0 : (float)Math.Floor((0.2 * lineHeight) + 0.5);
            defaultLineHeight = lineHeight + ascenderDelta;
            delta = ascenderDelta - descent;
        }

        /// <summary>
        /// Converts the specified coordinate to a scaled coordinate.
        /// </summary>
        /// <param name="x">The coordinate to convert.</param>
        /// <returns>The converted coordinate.</returns>
        private float Convert(double x)
        {
            return (float)(x * this.Scale);
        }

        /// <summary>
        /// Converts the specified rectangle to a scaled rectangle.
        /// </summary>
        /// <param name="rect">The rectangle to convert.</param>
        /// <returns>The converted rectangle.</returns>
        private SKRect Convert(OxyRect rect)
        {
            return new SKRect(this.ConvertAliased(rect.Left), this.ConvertAliased(rect.Top), this.ConvertAliased(rect.Right), this.ConvertAliased(rect.Bottom));
        }

        /// <summary>
        /// Converts the specified coordinate to a pixel-aligned scaled coordinate.
        /// </summary>
        /// <param name="x">The coordinate to convert.</param>
        /// <returns>The converted coordinate.</returns>
        private float ConvertAliased(double x)
        {
            return (int)(x * this.Scale) + 0.5f;
        }

        /// <summary>
        /// Sets the path to the specified points.
        /// </summary>
        /// <param name="points">The points defining the path.</param>
        /// <param name="aliased">If set to <c>true</c> aliased.</param>
        private void SetPath(IList<ScreenPoint> points, bool aliased)
        {
            if (aliased)
            {
                this.path.MoveTo(this.ConvertAliased(points[0].X), this.ConvertAliased(points[0].Y));
                for (int i = 1; i < points.Count; i++)
                {
                    this.path.LineTo(this.ConvertAliased(points[i].X), this.ConvertAliased(points[i].Y));
                }
            }
            else
            {
                this.path.MoveTo(this.Convert(points[0].X), this.Convert(points[0].Y));
                for (int i = 1; i < points.Count; i++)
                {
                    this.path.LineTo(this.Convert(points[i].X), this.Convert(points[i].Y));
                }
            }
        }

        /// <summary>
        /// Sets the fill style.
        /// </summary>
        /// <param name="fill">The fill color.</param>
        private void SetFill(OxyColor fill)
        {
            this.paint.Style = SKPaintStyle.Fill;
            this.paint.Color = fill.ToSKColor();
            if (this.paint.Color == SKColors.Black && GlobalSettings.Settings.DarkMode) this.paint.Color = SKColors.White;
            this.paint.IsAntialias = true;
        }

        /// <summary>
        /// Sets the stroke style.
        /// </summary>
        /// <param name="stroke">The stroke color.</param>
        /// <param name="thickness">The stroke thickness.</param>
        /// <param name="dashArray">The dash array.</param>
        /// <param name="lineJoin">The line join.</param>
        /// <param name="aliased">Use aliased strokes if set to <c>true</c>.</param>
        private void SetStroke(OxyColor stroke, double thickness, double[] dashArray = null, LineJoin lineJoin = LineJoin.Miter, bool aliased = false)
        {
            this.paint.Style = SKPaintStyle.Stroke;
            this.paint.Color = stroke.ToSKColor();
            if (this.paint.Color == SKColors.Black && GlobalSettings.Settings.DarkMode) this.paint.Color = SKColors.White;
            this.paint.StrokeWidth = this.Convert(thickness);
            this.paint.StrokeJoin = lineJoin.Convert();
            if (dashArray != null)
            {
                var dashArrayF = dashArray.Select(this.Convert).ToArray();
                this.paint.PathEffect = SKPathEffect.CreateDash(dashArrayF, 0f);
            }

            this.paint.IsAntialias = !aliased;
        }

        /// <summary>
        /// Gets the image from cache or creates a new <see cref="Bitmap" />.
        /// </summary>
        /// <param name="source">The source image.</param>
        /// <returns>A <see cref="Bitmap" />.</returns>
        private SKBitmap GetImage(OxyImage source)
        {
            if (source == null)
            {
                return null;
            }

            if (!this.imagesInUse.Contains(source))
            {
                this.imagesInUse.Add(source);
            }

            SKBitmap bitmap;
            if (!this.imageCache.TryGetValue(source, out bitmap))
            {
                var bytes = source.GetData();
                bitmap = SKBitmap.Decode(bytes);
                this.imageCache.Add(source, bitmap);
            }

            return bitmap;
        }
    }

    /// <summary>
    /// Provides extension methods that converts between Android types and OxyPlot types.
    /// </summary>
    public static class ExtensionMethods
    {
        /// <summary>
        /// Converts an <see cref="OxyColor" /> to a <see cref="Color" />.
        /// </summary>
        /// <param name="color">The color to convert.</param>
        /// <returns>The converted color.</returns>
        public static SKColor ToColor(this OxyColor color)
        {
            return color.IsInvisible() ? SKColors.Transparent : new SKColor(color.R, color.G, color.B, color.A);
        }

        /// <summary>
        /// Converts an <see cref="OxyColor" /> to a <see cref="Color" />.
        /// </summary>
        /// <param name="color">The color to convert.</param>
        /// <returns>The converted color.</returns>
        public static SKColor ToSKColor(this OxyColor color)
        {
            return color.IsInvisible() ? new SKColor(0, 0, 0, 0) : new SKColor(color.R, color.G, color.B, color.A);
        }

        /// <summary>
        /// Converts an <see cref="LineJoin" /> to a <see cref="Paint.Join" />.
        /// </summary>
        /// <param name="join">The join value to convert.</param>
        /// <returns>The converted join value.</returns>
        public static SKStrokeJoin Convert(this LineJoin join)
        {
            switch (join)
            {
                case LineJoin.Bevel:
                    return SKStrokeJoin.Bevel;
                case LineJoin.Miter:
                    return SKStrokeJoin.Miter;
                case LineJoin.Round:
                    return SKStrokeJoin.Round;
                default:
                    throw new InvalidOperationException("Invalid join type.");
            }
        }

    }
}