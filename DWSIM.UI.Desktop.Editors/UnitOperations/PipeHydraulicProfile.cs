using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using Eto.Forms;

using s = DWSIM.UI.Shared.Common;

using DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe;
using DWSIM.UnitOperations.UnitOperations;

namespace DWSIM.UI.Desktop.Editors
{
    public class PipeHydraulicProfile
    {

        public ISimulationObject SimObject;

        public DynamicLayout container;
        public PipeHydraulicProfile(ISimulationObject selectedobject, DynamicLayout layout)
        {
            SimObject = selectedobject;
            container = layout;

            Initialize();

        }

        void Initialize()
        {

            var profile = (PipeProfile)((Pipe)SimObject).Profile;
            profile.Status = PipeEditorStatus.OK;

            var flowsheet = SimObject.GetFlowsheet();

            var su = flowsheet.FlowsheetOptions.SelectedUnitSystem;
            var nf = flowsheet.FlowsheetOptions.NumberFormat;

            var sectioncontainer = new StackLayout { Orientation = Orientation.Horizontal, Padding = new Eto.Drawing.Padding(10), Spacing = 10 };

            s.CreateAndAddButtonRow(container, "Add Segment", null, (arg1, arg2) =>
            {
                var ps = new PipeSection() { Indice = profile.Sections.Count + 1, Incrementos = 10, Comprimento = 1, Quantidade = 1 };
                profile.Sections.Add(ps.Indice, ps);
                var pscontainer = new DynamicLayout();
                var slcontainer = new StackLayoutItem(pscontainer);
                pscontainer.BackgroundColor = Eto.Drawing.Colors.White;
                var editor = new PipeSectionEditor(flowsheet, profile.Sections[ps.Indice], pscontainer);
                var btnRemove = s.CreateAndAddButtonRow(pscontainer, "Remove Segment", null, null);
                btnRemove.Click += (sender, e) =>
                {
                    if (MessageBox.Show("Remove segment?", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                    {
                        profile.Sections.Remove(ps.Indice);
                        sectioncontainer.Items.Remove(slcontainer);
                    };
                };
                sectioncontainer.Items.Add(slcontainer);
            });

            s.CreateAndAddButtonRow(container, "Remove All Segments", null, (arg1, arg2) =>
            {
                if (MessageBox.Show("Remove all segments?", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                {
                    profile.Sections.Clear();
                    sectioncontainer.RemoveAll();
                    sectioncontainer.Items.Clear();
                };
            });

            container.Add(new Scrollable {Border = BorderType.None, Content = sectioncontainer });

            foreach (var section in profile.Sections.Values)
            {
                var pscontainer = new DynamicLayout();
                var slcontainer = new StackLayoutItem(pscontainer);
                pscontainer.BackgroundColor = Eto.Drawing.Colors.White;
                var editor = new PipeSectionEditor(flowsheet, section, pscontainer);
                var btnRemove = s.CreateAndAddButtonRow(pscontainer, "Remove Segment", null, null);
                btnRemove.Click += (sender, e) =>
                {
                    if (MessageBox.Show("Remove segment?", MessageBoxButtons.YesNo, MessageBoxType.Question, MessageBoxDefaultButton.No) == DialogResult.Yes)
                    {
                        profile.Sections.Remove(section.Indice);
                        sectioncontainer.Items.Remove(slcontainer);
                    };
                };
                sectioncontainer.Items.Add(slcontainer);
            }

        }
    }
}
