using System;
using Eto.Forms;
using Eto.Drawing;

using DWSIM.Interfaces;
using DWSIM.UI.Desktop.Editors;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DWSIM.UI.Shared;

namespace DWSIM.UI.Forms
{
    public class ObjectEditorContainer : TabControl
    {

        public ISimulationObject obj;

        public ObjectEditorContainer(ISimulationObject sobj) : base()
        {
            obj = sobj;
            Init();    
        }

        public void Init()
        {

            this.Width = 300;

            Pages.Clear();

            // connections

            if (obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
            {

                var tab1 = new TabPage();
                tab1.Text = "Connections";

                var cont0 = UI.Shared.Common.GetDefaultContainer();

                UI.Shared.Common.CreateAndAddDescriptionRow(cont0, "ConnectorsEditorDescription".Localize());
                new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont0);

                cont0.Width = this.Width;

                var scr1 = new Scrollable() { Content = cont0 };
                tab1.Content = scr1;

                Pages.Add(tab1);

            }


            // properties

            var tab2 = new TabPage();
            tab2.Text = "Properties";

            Pages.Add(tab2);

            var cont = UI.Shared.Common.GetDefaultContainer();

            cont.Width = this.Width;

            tab2.Content = new Scrollable() { Content = cont };

            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
            {
                new DWSIM.UI.Desktop.Editors.MaterialStreamEditor(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DistillationColumn)
            {
                new DWSIM.UI.Desktop.Editors.DistillationColumnEditor(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AbsorptionColumn)
            {
                new DWSIM.UI.Desktop.Editors.AbsorptionColumnEditor(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Adjust)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.AdjustEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Spec)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.SpecEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_Recycle)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.RecycleEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.OT_EnergyRecycle)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.EnergyRecycleEditor.Populate(obj, cont);
            }
            else
            {
                new DWSIM.UI.Desktop.Editors.GeneralEditors(obj, cont);
            }
            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Pipe)
            {
                tab2.Text = "General";
                var cont2 = UI.Shared.Common.GetDefaultContainer();
                cont2.Tag = "Hydraulic Profile";
                new PipeHydraulicProfile(obj, cont2);
                Pages.Add(new TabPage(new Scrollable() { Content = cont2, Width = this.Width }) { Text = "Hydraulic Profile" });
                var cont3 = UI.Shared.Common.GetDefaultContainer();
                cont3.Tag = "Thermal Profile";
                new PipeThermalProfile(obj, cont3);
                Pages.Add(new TabPage(new Scrollable() { Content = cont3, Width = this.Width }) { Text = "Thermal Profile" });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.CustomUO)
            {
                tab2.Text = "General";
                var cont2 = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };
                cont2.Tag = "Python Script";
                var scripteditor = new DWSIM.UI.Controls.CodeEditorControl() { Text = ((CustomUO)obj).ScriptText };
                var dyn1 = new DynamicLayout();
                dyn1.CreateAndAddLabelAndButtonRow("Click to commit script changes", "Update", null, (sender, e) =>
                {
                    ((CustomUO)obj).ScriptText = scripteditor.Text;
                });
                cont2.Rows.Add(new TableRow(dyn1));
                cont2.Rows.Add(new TableRow(scripteditor));
                Pages.Add(new TabPage(new Scrollable() { Content = cont2, Width = this.Width }) { Text = "Python Script" });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.HeatExchanger)
            {
                tab2.Text = "General";
                var dyn1 = new UI.Desktop.Editors.ShellAndTubePropertiesView(obj);
                Pages.Add(new TabPage(new Scrollable() { Content = dyn1, Width = this.Width }) { Text = "Shell and Tube Properties" });
            }
            
            if (obj.Calculated)
            {
                var tabr = new TabPage();
                tabr.Text = "Results";

                var report = obj.GetReport(obj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, obj.GetFlowsheet().FlowsheetOptions.NumberFormat);
                var container = new TableLayout();
                new DWSIM.UI.Desktop.Editors.Results(obj, container);

                tabr.Content = new Scrollable() { Content = container, Width = this.Width };
                Pages.Add(tabr);

            }

            if (obj.GraphicObject is ShapeGraphic)
            {
                var tabx = new TabPage();
                tabx.Text = "Appearance";
                var editor = new ObjectAppearanceEditorView(obj.GetFlowsheet(), (ShapeGraphic)obj.GraphicObject);
                tabx.Content = new Scrollable() { Content = editor, Width = this.Width };

                Pages.Add(tabx);
            }

        }
    }
}
