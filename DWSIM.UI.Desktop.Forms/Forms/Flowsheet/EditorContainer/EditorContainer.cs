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

        public int SelectedPanel = -1;

        private bool loaded = false;

        private TabPage PageResults, PageEditor;

        public ObjectEditorContainer(ISimulationObject sobj) : base()
        {
            obj = sobj;
            Tag = obj.Name;
            this.Width = 300;
            Init();
            SelectedIndexChanged += (sender, e) => { if (loaded) SelectedPanel = SelectedIndex; };
        }

        public void Init()
        {

            loaded = false;

            Pages.Clear();

            // connections

            if (obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Adjust &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Spec)
            {

                var tab1 = new TabPage();
                tab1.Text = "Connections";

                var cont0 = UI.Shared.Common.GetDefaultContainer();

                UI.Shared.Common.CreateAndAddDescriptionRow(cont0, "ConnectorsEditorDescription".Localize());
                new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont0);

                cont0.Width = this.Width - 30;

                var scr1 = new Scrollable() { Content = cont0 };
                tab1.Content = scr1;

                Pages.Add(tab1);

            }

            // properties

            var tab2 = new TabPage();
            tab2.Text = "Properties";

            Pages.Add(tab2);

            var cont = UI.Shared.Common.GetDefaultContainer();

            cont.Width = this.Width - 30;

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

            tab2.Content = new Scrollable() { Content = cont, Width = this.Width - 30 };

            if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Pipe)
            {
                tab2.Text = "General";
                var cont2 = UI.Shared.Common.GetDefaultContainer();
                cont2.Tag = "Hydraulic Profile";
                cont2.Width = this.Width - 30;
                new PipeHydraulicProfile(obj, cont2);
                Pages.Add(new TabPage(new Scrollable() { Content = cont2, Width = this.Width - 30 }) { Text = "Hydraulic Profile" });
                var cont3 = UI.Shared.Common.GetDefaultContainer();
                cont3.Tag = "Thermal Profile";
                cont3.Width = this.Width - 30;
                new PipeThermalProfile(obj, cont3);
                Pages.Add(new TabPage(new Scrollable() { Content = cont3, Width = this.Width - 30 }) { Text = "Thermal Profile" });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.CustomUO)
            {
                tab2.Text = "General";
                var cont2 = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };
                cont2.Tag = "Python Script";
                cont2.Width = this.Width - 30;
                var scripteditor = new DWSIM.UI.Controls.CodeEditorControl() { Text = ((CustomUO)obj).ScriptText };
                var dyn1 = new DynamicLayout();
                dyn1.CreateAndAddLabelAndButtonRow("Click to commit script changes", "Update", null, (sender, e) =>
                {
                    ((CustomUO)obj).ScriptText = scripteditor.Text;
                });
                dyn1.Width = this.Width - 30;
                cont2.Rows.Add(new TableRow(dyn1));
                cont2.Rows.Add(new TableRow(scripteditor));
                Pages.Add(new TabPage(new Scrollable() { Content = cont2, Width = this.Width - 30 }) { Text = "Python Script" });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.HeatExchanger)
            {
                tab2.Text = "General";
                var dyn1 = new UI.Desktop.Editors.ShellAndTubePropertiesView(obj);
                dyn1.Width = this.Width - 30;
                Pages.Add(new TabPage(new Scrollable() { Content = dyn1, Width = this.Width - 30 }) { Text = "Shell and Tube Properties" });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AbsorptionColumn ||
                    obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DistillationColumn)
            {
                tab2.Text = "General";
                var dyn2 = UI.Desktop.Editors.RigorousColumnShared.GetInitialEstimatesEditor((Column)obj);
                dyn2.Width = this.Width - 30;
                Pages.Add(new TabPage(new Scrollable() { Content = dyn2, Width = this.Width - 30 }) { Text = "Initial Estimates" });
            }

            PageEditor = tab2;

            var tabr = new TabPage();
            tabr.Text = "Results";

            var container = new TableLayout();
            new DWSIM.UI.Desktop.Editors.Results(obj, container);

            tabr.Content = new Scrollable() { Content = container, Width = this.Width - 30 };

            PageResults = tabr;

            Pages.Add(tabr);

            if (obj.GraphicObject is ShapeGraphic)
            {
                var tabx = new TabPage();
                tabx.Text = "Appearance";
                var editor = new ObjectAppearanceEditorView(obj.GetFlowsheet(), (ShapeGraphic)obj.GraphicObject);
                editor.Width = this.Width - 30;
                tabx.Content = new Scrollable() { Content = editor, Width = this.Width - 30 };

                Pages.Add(tabx);
            }

            if (SelectedPanel >= 0) SelectedIndex = SelectedPanel;

            loaded = true;

        }

        public void Update()
        {

            if (PageEditor != null)
            {

                try
                {
                    DocumentPage dpage = (DocumentPage)PageEditor.Parent.Parent;
                    dpage.Text = obj.GraphicObject.Tag;
                }
                catch { }

                PageEditor.Content = null;

                var cont = UI.Shared.Common.GetDefaultContainer();

                cont.Width = this.Width - 30;

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

                PageEditor.Content = new Scrollable() { Content = cont, Width = this.Width - 30 };

            }

            var report = obj.GetReport(obj.GetFlowsheet().FlowsheetOptions.SelectedUnitSystem, System.Globalization.CultureInfo.CurrentCulture, obj.GetFlowsheet().FlowsheetOptions.NumberFormat);
            var container = new TableLayout();
            new DWSIM.UI.Desktop.Editors.Results(obj, container);
            PageResults.Content = new Scrollable() { Content = container, Width = this.Width - 30 };

        }
    }
}
