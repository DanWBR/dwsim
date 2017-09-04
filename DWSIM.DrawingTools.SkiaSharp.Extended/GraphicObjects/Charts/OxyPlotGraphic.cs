using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using OxyPlot;
using SkiaSharp;

namespace DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts
{
    public class OxyPlotGraphic : ShapeGraphic
    {

        private Renderers.SKCanvasRenderContext renderer = new Renderers.SKCanvasRenderContext(1.0);

        #region "Constructors"

        public void Init()
        {
            this.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart;
            this.Description = "Chart Object";
        }

        public OxyPlotGraphic()
            : base()
        {
            Init();
        }

        public OxyPlotGraphic(SKPoint graphicPosition)
            : base()
        {
            Init();
            this.SetPosition(graphicPosition);
        }

        public OxyPlotGraphic(int posX, int posY)
        {
            Init();
            this.SetPosition(new SKPoint(posX, posY));
        }

        public OxyPlotGraphic(SKPoint graphicPosition, SKSize graphicSize)
            : base()
        {
            Init();
            this.SetSize(graphicSize);
            this.SetPosition(graphicPosition);
        }

        public OxyPlotGraphic(int posX, int posY, int width, int height)
            : base()
        {
            Init();
            this.SetSize(new SKSize(width, height));
            this.SetPosition(new SKPoint(posX, posY));
        }

        #endregion

        public double Scale { get; set; }

        public string ModelName { get; set; }

        public string OwnerID { get; set; }

        public Interfaces.IFlowsheet Flowsheet { get; set; }

        public override void Draw(object g)
        {

            if (OwnerID == null) OwnerID = "";

            var canvas = (SKCanvas)g;

            base.Draw(g);

            if (Flowsheet != null)
            {
                if (Flowsheet.SimulationObjects.ContainsKey(OwnerID))
                {

                    var obj = Flowsheet.SimulationObjects[OwnerID];

                    IPlotModel model = null;

                    try
                    {
                        model = (IPlotModel)(obj.GetChartModel(ModelName));
                    }
                    catch
                    {
                        PaintInstructions(canvas);
                        return;
                    }

                    if (model != null)
                    {
                        try
                        {
                            using (var surface = SKSurface.Create(new SKImageInfo(Width, Height)))
                            {
                                renderer.SetTarget(surface.Canvas);
                                if (Scale == 0.0) Scale = 1.0;
                                model.Update(true);
                                model.Render(renderer, Width / Scale, Height / Scale);
                                var paint = GetPaint(SKColors.Black);
                                paint.FilterQuality = SKFilterQuality.High;
                                var zoom = ((GraphicsSurface)Flowsheet.GetSurface()).Zoom;
                                surface.Canvas.Scale(zoom, zoom);
                                canvas.DrawSurface(surface, X, Y, paint);
                            }
                        }
                        catch
                        {
                            PaintInstructions(canvas);
                        }
                    }
                    else
                    {
                        PaintInstructions(canvas);
                    }

                }
                else
                {
                    PaintInstructions(canvas);
                }

            }
            else
            {
                PaintInstructions(canvas);
            }
        }

        private void PaintInstructions(SKCanvas canvas)
        {

            var tpaint = GetStrokePaint(SKColors.Black, 1.0f);

            var size = this.MeasureString("Double-click to edit", tpaint);

            Width = 20 + (int)size.Width;
            Height = 80 + (int)size.Height;

            canvas.DrawText("Double-click to edit", X + 10, Y + 40, tpaint);

            canvas.DrawRect(new SKRect(X, Y, X + Width, Y + Height), tpaint);

        }

    }
}
