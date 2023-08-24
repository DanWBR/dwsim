using System;
using Eto.Forms;
using Eto.Drawing;

using DWSIM.Interfaces;
using DWSIM.UI.Desktop.Editors;
using DWSIM.UnitOperations.UnitOperations;
using DWSIM.Drawing.SkiaSharp.GraphicObjects;
using DWSIM.UI.Shared;
using Mono.Cecil.Cil;
using System.Linq;

namespace DWSIM.UI.Forms
{
    public class ObjectEditorContainer : DocumentControl
    {

        public ISimulationObject obj;

        public int SelectedPanel = -1;

        private bool loaded = false;

        private DocumentPage PageResults, PageEditor, PageDynamics, PageConnections, PageCustomProperties;

        public ObjectEditorContainer(ISimulationObject sobj) : base()
        {
            DisplayArrows = true;
            obj = sobj;
            Tag = obj.Name;
            this.Width = 300;
            Init();
            SelectedIndexChanged += (sender, e) => { if (loaded) SelectedPanel = SelectedIndex; };
        }

        public void Init()
        {

            obj.GetFlowsheet().RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, obj);

            loaded = false;

            Pages.Clear();

            // connections

            if (obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Adjust &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Controller_PID &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.LevelGauge &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.DigitalGauge &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Input &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Switch &&
                obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Spec)
            {

                var tab1 = new DocumentPage { Closable = false };
                tab1.Text = "Connections";

                var cont0 = UI.Shared.Common.GetDefaultContainer();

                UI.Shared.Common.CreateAndAddLabelRow(cont0, "Object Connections Editor".Localize());

                UI.Shared.Common.CreateAndAddDescriptionRow(cont0, "ConnectorsEditorDescription".Localize());
                new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont0);

                cont0.Width = this.Width - 30;

                var scr1 = new Scrollable() { Content = cont0 };
                tab1.Content = scr1;

                Pages.Add(tab1);

                PageConnections = tab1;

            }

            // properties

            var tab2 = new DocumentPage { Closable = false };
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
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Controller_PID)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.PIDControllerEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Controller_Python)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.PythonControllerEditor.Populate(obj, cont);
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
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.LevelGauge)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.LevelGaugeEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.AnalogGaugeEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DigitalGauge)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.DigitalGaugeEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Switch)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.SwitchEditor.Populate(obj, cont);
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Input)
            {
                DWSIM.UI.Desktop.Editors.LogicalBlocks.InputEditor.Populate(obj, cont);
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
                Pages.Add(new DocumentPage(new Scrollable() { Content = cont2, Width = this.Width - 30 }) { Text = "Hydraulic Profile", Closable = false });
                var cont3 = UI.Shared.Common.GetDefaultContainer();
                cont3.Tag = "Thermal Profile";
                cont3.Width = this.Width - 30;
                new PipeThermalProfile(obj, ((Pipe)obj).ThermalProfile, cont3);
                Pages.Add(new DocumentPage(new Scrollable() { Content = cont3, Width = this.Width - 30 }) { Text = "Thermal Profile", Closable = false });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.CustomUO)
            {
                tab2.Text = "General";
                var cont2 = new TableLayout { Padding = new Padding(10), Spacing = new Size(5, 5) };
                cont2.Tag = "Python Script";
                cont2.Width = this.Width - 30;
                if (Application.Instance.Platform.IsWpf)
                {
                    var scripteditor = new Eto.Forms.Controls.Scintilla.Shared.ScintillaControl() { ScriptText = ((CustomUO)obj).ScriptText };
                    var dyn1 = new DynamicLayout();
                    dyn1.CreateAndAddLabelAndButtonRow("Click to commit script changes", "Update", null, (sender, e) =>
                    {
                        ((CustomUO)obj).ScriptText = scripteditor.ScriptText;
                    });
                    dyn1.Width = this.Width - 30;
                    cont2.Rows.Add(new TableRow(dyn1));
                    cont2.Rows.Add(new TableRow(scripteditor));
                }
                else if (Application.Instance.Platform.IsMac || Application.Instance.Platform.IsGtk)
                {
                    var scripteditor = new Eto.Forms.Controls.Scintilla.Shared.ScintillaControl() { ScriptText = ((CustomUO)obj).ScriptText };
                    scripteditor.SetKeywords(1, ((FlowsheetBase.FlowsheetBase)obj.GetFlowsheet()).ScriptKeywordsU);
                    var dyn1 = new DynamicLayout();
                    dyn1.CreateAndAddLabelAndButtonRow("Click to commit script changes", "Update", null, (sender, e) =>
                    {
                        ((CustomUO)obj).ScriptText = scripteditor.ScriptText;
                    });
                    dyn1.Width = this.Width - 30;
                    cont2.Rows.Add(new TableRow(dyn1));
                    cont2.Rows.Add(new TableRow(scripteditor));
                }
                Pages.Add(new DocumentPage(new Scrollable() { Content = cont2, Width = this.Width - 30 }) { Text = "Python Script", Closable = false });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.HeatExchanger)
            {
                tab2.Text = "General";
                var dyn1 = new ShellAndTubePropertiesView(obj);
                dyn1.Width = this.Width - 30;
                Pages.Add(new DocumentPage(new Scrollable() { Content = dyn1, Width = this.Width - 30 }) { Text = "Shell and Tube Properties", Closable = false });
            }
            else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AbsorptionColumn ||
                    obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DistillationColumn)
            {
                tab2.Text = "General";
                var dyn2 = RigorousColumnShared.GetInitialEstimatesEditor((Column)obj);
                dyn2.Width = this.Width - 30;
                Pages.Add(new DocumentPage(new Scrollable() { Content = dyn2, Width = this.Width - 30 }) { Text = "Initial Estimates", Closable = false });
            }

            PageEditor = tab2;

            // custom properties

            if (obj.ExtraProperties.Count() > 0 && (obj.ExtraProperties.Count() != obj.ExtraPropertiesDescriptions.Count()))
            {
                var tabcustom = new DocumentPage { Closable = false };
                tabcustom.Text = "Custom";

                var contd = UI.Shared.Common.GetDefaultContainer();

                contd.Width = this.Width - 30;

                new CustomPropertiesEditor(obj, contd);

                tabcustom.Content = new Scrollable() { Content = contd, Width = this.Width - 30 };

                PageDynamics = tabcustom;

                Pages.Add(tabcustom);
            }

            // dynamics

            if ((obj.SupportsDynamicMode && obj.HasPropertiesForDynamicMode) || obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
            {
                if (obj.ExtraPropertiesDescriptions.Count() > 0 || obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
                {

                    var tabd = new DocumentPage { Closable = false };
                    tabd.Text = "Dynamics";

                    var contd = UI.Shared.Common.GetDefaultContainer();

                    contd.Width = this.Width - 30;

                    new DynamicPropertiesEditor(obj, contd);

                    tabd.Content = new Scrollable() { Content = contd, Width = this.Width - 30 };

                    PageDynamics = tabd;

                    Pages.Add(tabd);
                }
            }

            // results

            var tabr = new DocumentPage { Closable = false };
            tabr.Text = "Results";

            var container = new TableLayout();
            new DWSIM.UI.Desktop.Editors.Results(obj, container);

            tabr.Content = new Scrollable() { Content = container, Width = this.Width - 30 };

            PageResults = tabr;

            Pages.Add(tabr);

            if (obj.GraphicObject is ShapeGraphic)
            {
                var tabx = new DocumentPage { Closable = false };
                tabx.Text = "Appearance";
                var editor = new ObjectAppearanceEditorView(obj.GetFlowsheet(), (ShapeGraphic)obj.GraphicObject);
                editor.Width = this.Width - 30;
                tabx.Content = new Scrollable() { Content = editor, Width = this.Width - 30 };

                Pages.Add(tabx);
            }

            if (SelectedPanel >= 0) SelectedIndex = SelectedPanel;

            loaded = true;

        }

        public void UpdateConnections()
        {

            if (PageConnections != null)
            {

                PageConnections.Content = null;

                // connections

                if (obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.EnergyStream &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Adjust &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Controller_PID &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.LevelGauge &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.DigitalGauge &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Input &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.Switch &&
                    obj.GraphicObject.ObjectType != Interfaces.Enums.GraphicObjects.ObjectType.OT_Spec)
                {

                    var tab1 = new DocumentPage { Closable = false };
                    tab1.Text = "Connections";

                    var cont0 = UI.Shared.Common.GetDefaultContainer();

                    UI.Shared.Common.CreateAndAddDescriptionRow(cont0, "ConnectorsEditorDescription".Localize());
                    new DWSIM.UI.Desktop.Editors.ConnectionsEditor(obj, cont0);

                    cont0.Width = this.Width - 30;

                    PageConnections.Content = new Scrollable() { Content = cont0, Width = this.Width - 30 };

                }

            }

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
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Controller_PID)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.PIDControllerEditor.Populate(obj, cont);
                }
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Controller_Python)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.PythonControllerEditor.Populate(obj, cont);
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
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.LevelGauge)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.LevelGaugeEditor.Populate(obj, cont);
                }
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.AnalogGauge)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.AnalogGaugeEditor.Populate(obj, cont);
                }
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.DigitalGauge)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.DigitalGaugeEditor.Populate(obj, cont);
                }
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Switch)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.SwitchEditor.Populate(obj, cont);
                }
                else if (obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.Input)
                {
                    DWSIM.UI.Desktop.Editors.LogicalBlocks.InputEditor.Populate(obj, cont);
                }
                else
                {
                    new DWSIM.UI.Desktop.Editors.GeneralEditors(obj, cont);
                }

                PageEditor.Content = new Scrollable() { Content = cont, Width = this.Width - 30 };

            }

            // custom

            if (PageCustomProperties != null)
            {

                PageCustomProperties.Content = null;

                if (obj.ExtraProperties.Count() > 0 && (obj.ExtraProperties.Count() != obj.ExtraPropertiesDescriptions.Count()))
                {

                    var contd = UI.Shared.Common.GetDefaultContainer();

                    contd.Width = this.Width - 30;

                    new CustomPropertiesEditor(obj, contd);

                    PageCustomProperties.Content = new Scrollable() { Content = contd, Width = this.Width - 30 };

                }

            }

            // dynamics

            if (PageDynamics != null)
            {

                PageDynamics.Content = null;

                if (obj.ExtraPropertiesDescriptions.Count() > 0 || obj.GraphicObject.ObjectType == Interfaces.Enums.GraphicObjects.ObjectType.MaterialStream)
                {

                    var contd = UI.Shared.Common.GetDefaultContainer();

                    contd.Width = this.Width - 30;

                    new DynamicPropertiesEditor(obj, contd);

                    PageDynamics.Content = new Scrollable() { Content = contd, Width = this.Width - 30 };

                }

            }

            PageResults.Content = null;

            var container = new TableLayout();
            new DWSIM.UI.Desktop.Editors.Results(obj, container);
            PageResults.Content = new Scrollable() { Content = container, Width = this.Width - 30 };

        }
    }
}
