using System;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;
namespace DWSIM.UI.Controls
{
    // control to use in your eto.forms code
    [Eto.Handler(typeof(IFlowsheetSurface))]
    public class FlowsheetSurfaceControl : Eto.Forms.Control
    {
        IFlowsheetSurface Handler { get { return (IFlowsheetSurface)base.Handler; } }

        public DWSIM.Drawing.SkiaSharp.GraphicsSurface FlowsheetSurface
        {
            get { return Handler.FlowsheetSurface; }
            set { Handler.FlowsheetSurface = value; }
        }

        public DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject
        {
            get { return Handler.FlowsheetObject; }
            set { Handler.FlowsheetObject = value; }
        }

        public void AddObject(string objtname, int x, int y)
        {

            string prep;
            var count = FlowsheetObject.SimulationObjects.Count;
            ObjectType objtype = ObjectType.Nenhum;
            switch (objtname)
            {
                case "Material Stream":
                    objtype = ObjectType.MaterialStream;
                    prep = "MSTR-";
                    break;
                case "Energy Stream":
                    objtype = ObjectType.EnergyStream;
                    prep = "ESTR-";
                    break;
                case "Vessel":
                    objtype = ObjectType.Vessel;
                    prep = "SEP-";
                    break;
                case "Mixer":
                    objtype = ObjectType.NodeIn;
                    prep = "MIX-";
                    break;
                case "Splitter":
                    objtype = ObjectType.NodeOut;
                    prep = "SPL-";
                    break;
                case "Pump":
                    objtype = ObjectType.Pump;
                    prep = "PUMP-";
                    break;
                case "Valve":
                    objtype = ObjectType.Valve;
                    prep = "VALV-";
                    break;
                case "Heat Exchanger":
                    objtype = ObjectType.HeatExchanger;
                    prep = "HXC-";
                    break;
                case "Heater/Cooler":
                    objtype = ObjectType.HeaterCooler;
                    prep = "HCR-";
                    break;
                case "Compressor/Expander":
                    objtype = ObjectType.CompressorExpander;
                    prep = "CXR-";
                    break;
                case "Recycle":
                    objtype = ObjectType.OT_Recycle;
                    prep = "REC-";
                    break;
                case "Shortcut Column":
                    objtype = ObjectType.ShortcutColumn;
                    prep = "SCN-";
                    break;
                case "Compound Separator":
                    objtype = ObjectType.ComponentSeparator;
                    prep = "CS-";
                    break;
                case "Conversion Reactor":
                    objtype = ObjectType.RCT_Conversion;
                    prep = "CR-";
                    break;
                case "Equilibrium Reactor":
                    objtype = ObjectType.RCT_Equilibrium;
                    prep = "ER-";
                    break;
                case "Gibbs Reactor":
                    objtype = ObjectType.RCT_Gibbs;
                    prep = "GR-";
                    break;
                case "CSTR":
                    objtype = ObjectType.RCT_CSTR;
                    prep = "CSTR-";
                    break;
                case "PFR":
                    objtype = ObjectType.RCT_PFR;
                    prep = "PFR-";
                    break;
                case "Distillation Column":
                    objtype = ObjectType.DistillationColumn;
                    prep = "DC-";
                    break;
                case "Absorption Column":
                    objtype = ObjectType.AbsorptionColumn;
                    prep = "AC-";
                    break;
                case "Pipe Segment":
                    objtype = ObjectType.Pipe;
                    prep = "PIPE-";
                    break;
                case "Adjust":
                    objtype = ObjectType.OT_Adjust;
                    prep = "ADJ-";
                    break;
                case "Solids Separator":
                    objtype = ObjectType.SolidSeparator;
                    prep = "SS-";
                    break;
                default:
                    objtype = ObjectType.Nenhum;
                    prep = "";
                    break;
            }
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
                    var gobj = new DWSIM.Drawing.SkiaSharp.GraphicObjects.TableGraphic(x, y);
                    gobj.Name = Guid.NewGuid().ToString();
                    gobj.Flowsheet = FlowsheetObject;
                    FlowsheetObject.AddGraphicObject(gobj);
                }
            }
            else
            {
                FlowsheetObject.AddObject(objtype, x, y, prep + (count + 1).ToString("000"));
            }
            Invalidate();

        }

        protected override void OnKeyDown(Eto.Forms.KeyEventArgs e)
        {
            base.OnKeyDown(e);
            if (e.Key == Keys.F5)
            {
                FlowsheetObject.SolveFlowsheet(false);
            }
        }

        protected override void OnKeyUp(KeyEventArgs e)
        {
            base.OnKeyUp(e);
        }

        // interface to the platform implementations
        public interface IFlowsheetSurface : Eto.Forms.Control.IHandler
        {
            DWSIM.Drawing.SkiaSharp.GraphicsSurface FlowsheetSurface { get; set; }

            DWSIM.UI.Desktop.Shared.Flowsheet FlowsheetObject { get; set; }

        }


    }
}


