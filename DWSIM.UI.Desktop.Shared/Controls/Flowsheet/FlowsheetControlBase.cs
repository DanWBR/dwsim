using System;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface))]
    public class FlowsheetSurfaceControlBase : Eto.Forms.Control
    {

        public DWSIM.Drawing.SkiaSharp.GraphicsSurface FlowsheetSurface;
        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject;
                
        // interface to the platform implementations

        public interface IFlowsheetSurface : Eto.Forms.Control.IHandler
        {
        }

        public interface IFlowsheetSurface_OpenGL : Eto.Forms.Control.IHandler
        {
        }

        public void AddObject(string objtname, int x, int y)
        {

            FlowsheetObject.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectAddedOrRemoved);

            string prep;
            var count = FlowsheetObject.SimulationObjects.Count;
            ObjectType objtype = ObjectType.Nenhum;

            if (objtype == ObjectType.Nenhum)
            {
                if (objtname == "Text")
                {
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.TextGraphic(x, y, "TEXT");
                    gobj.Name = Guid.NewGuid().ToString();
                    FlowsheetObject.AddGraphicObject(gobj);
                }
                else if (objtname == "Property Table")
                {
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables.TableGraphic(x, y);
                    gobj.Name = Guid.NewGuid().ToString();
                    gobj.Flowsheet = FlowsheetObject;
                    FlowsheetObject.AddGraphicObject(gobj);
                }
                else if (objtname == "Spreadsheet Table")
                {
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables.SpreadsheetTableGraphic(x, y);
                    gobj.Name = Guid.NewGuid().ToString();
                    gobj.Flowsheet = FlowsheetObject;
                    FlowsheetObject.AddGraphicObject(gobj);
                }
                else if (objtname == "Master Property Table")
                {
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables.MasterTableGraphic(x, y);
                    gobj.Name = Guid.NewGuid().ToString();
                    gobj.Flowsheet = FlowsheetObject;
                    FlowsheetObject.AddGraphicObject(gobj);
                }
                else if (objtname == "Chart Object")
                {
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.Charts.OxyPlotGraphic(x, y);
                    gobj.Name = Guid.NewGuid().ToString();
                    gobj.Flowsheet = FlowsheetObject;
                    FlowsheetObject.AddGraphicObject(gobj);
                }
            }
            Invalidate();

        }
    }
}


