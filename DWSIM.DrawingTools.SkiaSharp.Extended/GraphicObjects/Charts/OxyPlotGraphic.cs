using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using OxyPlot;
using OxyPlot.Series;
using SkiaSharp;

namespace DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts
{
    public class OxyPlotGraphic : ShapeGraphic
    {

        private Renderers.SKCanvasRenderContext renderer = new Renderers.SKCanvasRenderContext(1.0);

        public string ClipboardData { get; set; } = "";

        #region "Constructors"

        public void Init()
        {
            this.ObjectType = DWSIM.Interfaces.Enums.GraphicObjects.ObjectType.GO_Chart;
            this.Description = "Chart Object";
            Width = 100;
            Height = 100;
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

        public override bool LoadData(List<System.Xml.Linq.XElement> data)
        {
            base.LoadData(data);
            return true;
        }

        public override List<System.Xml.Linq.XElement> SaveData()
        {
            return base.SaveData();
        }

        private string _ModelName = "";

        public string ModelName
        {
            get
            {
                return _ModelName;
            }
            set
            {
                _ModelName = value;
            }
        }

        private string _OwnerID = "";

        public string OwnerID
        {
            get
            {
                return _OwnerID;
            }
            set
            {
                _OwnerID = value;
            }
        }

        public Interfaces.IFlowsheet Flowsheet { get; set; }

        public override void Draw(object g)
        {

            var canvas = (SKCanvas)g;

            base.Draw(g);

            if (Flowsheet != null)
            {

                IPlotModel model = null;

                if (OwnerID != null)
                {
                    if (Flowsheet.SimulationObjects.ContainsKey(OwnerID))
                    {
                        var obj = Flowsheet.SimulationObjects[OwnerID];

                        try
                        {
                            model = (IPlotModel)(obj.GetChartModel(ModelName));
                        }
                        catch
                        {
                            PaintInstructions(canvas, "Chart model not found.");
                            return;
                        }
                    }
                    else if (OwnerID == "Dynamic Mode Integrators")
                    {
                        var obj = Flowsheet.DynamicsManager.IntegratorList.Where(x => x.Value.Description == ModelName).FirstOrDefault();

                        try
                        {
                            model = (IPlotModel)(Flowsheet.DynamicsManager.GetChartModel(Flowsheet, obj.Key));
                        }
                        catch
                        {
                            PaintInstructions(canvas, "Chart model not found.");
                            return;
                        }
                    }
                    else if (OwnerID == "Chart Objects")
                    {
                        var obj = Flowsheet.Charts.Values.Where(x => x.DisplayName == ModelName).FirstOrDefault();

                        try
                        {
                            model = (IPlotModel)obj.PlotModel;
                        }
                        catch
                        {
                            PaintInstructions(canvas, "Chart model not found.");
                            return;
                        }
                    }
                    else
                    {
                        PaintInstructions(canvas, "Referenced object not found.");
                        return;
                    }

                    if (model != null)
                    {
                        ClipboardData = GetClipboardData((PlotModel)model);
                        try
                        {
                            using (var bmp = new SKBitmap(Width * 2, Height * 2))
                            {
                                using (var bmpcanvas = new SKCanvas(bmp))
                                {
                                    bmpcanvas.Clear(GetBackColor());
                                    bmpcanvas.Scale(2.0f);
                                    renderer.SetTarget(bmpcanvas);
                                    model.Update(true);
                                    model.Render(renderer, Width, Height);
                                    var paint = GetPaint(GetForeColor());
                                    paint.FilterQuality = SKFilterQuality.High;
                                    paint.IsAutohinted = true;
                                    paint.IsAntialias = true;
                                    canvas.DrawBitmap(bmp, new SKRect(X, Y, X + Width, Y + Height), paint);
                                    canvas.DrawRect(new SKRect(X, Y, X + Width, Y + Height), GetStrokePaint(GetForeColor(), 1.0f));
                                }
                            }
                        }
                        catch (Exception ex)
                        {
                            PaintInstructions(canvas, "Error drawing chart: " + ex.Message.ToString());
                        }
                    }
                    else
                    {
                        PaintInstructions(canvas, "Chart model not found.");
                        return;
                    }

                }
            }
            else
            {
                PaintInstructions(canvas, "Flowsheet not defined.");
                return;
            }
        }

        private void PaintInstructions(SKCanvas canvas, string text)
        {

            var tpaint = GetPaint(GlobalSettings.Settings.DarkMode ? LineColorDark : LineColor);

            var size = this.MeasureString(text, tpaint);

            var width = (int)size.Width;
            var height = (int)size.Height;

            canvas.DrawText(text, X + (Width - width) / 2, Y + (Height - height) / 2, tpaint);

            canvas.DrawRect(new SKRect(X, Y, X + Width, Y + Height), GetStrokePaint(GlobalSettings.Settings.DarkMode ? LineColorDark : LineColor, 1.0f));

        }

        private string GetClipboardData(PlotModel model)
        {
            var sb = new System.Text.StringBuilder();

            sb.AppendLine(model.Title);
            sb.AppendLine(model.Subtitle);

            sb.AppendLine();

            foreach (LineSeries ls in model.Series.Where((x) => x is LineSeries))
            {
                sb.AppendLine(ls.Title);
                sb.AppendLine(model.Axes[0].Title + "\t" + model.Axes[1].Title);
                foreach (var p in ls.Points)
                {
                    sb.AppendLine(p.X + "\t" + p.Y);
                }
                sb.AppendLine();
            }

            return sb.ToString();

        }

    }
}
