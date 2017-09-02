using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using OxyPlot;
using SkiaSharp;

namespace DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
{
    class OxyPlotGraphic : ShapeGraphic
    {

        private Renderers.SKCanvasRenderContext renderer = new Renderers.SKCanvasRenderContext(1.0);

        #region "Constructors"

        public void Init()
        {
            this.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart;
            this.Description = "Chart Object";
        }

        public OxyPlotGraphic(): base()
        {
            Init();
        }

        public OxyPlotGraphic(SKPoint graphicPosition): base()
        {
            Init();
            this.SetPosition(graphicPosition);
        }

        public OxyPlotGraphic(int posX, int posY)
        {
            Init();
            this.SetPosition(new SKPoint(posX, posY));
        }

        public OxyPlotGraphic(SKPoint graphicPosition, SKSize graphicSize): base()
        {
            Init();
            this.SetSize(graphicSize);
            this.SetPosition(graphicPosition);
        }

        public OxyPlotGraphic(int posX, int posY, int width, int height): base()
        {
            Init();
            this.SetSize(new SKSize(width, height));
            this.SetPosition(new SKPoint(posX, posY));
        }

        #endregion

        public double Scale { get; set; }

        public string ModelName { get; set; }

        public override void Draw(object g)
        {
            
            base.Draw(g);

            var model = (IPlotModel)(Owner.GetChartModel(ModelName));

            if (model != null)
            {
                renderer.SetTarget((SKCanvas)g);
                if (Scale == 0.0) Scale = 1.0;
                model.Render(renderer, Width / Scale, Height / Scale);
            }

        }


    }
}
