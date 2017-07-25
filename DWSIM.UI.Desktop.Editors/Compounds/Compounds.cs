using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class Compounds
    {

        public IFlowsheet flowsheet;
        public DynamicLayout container;

        private ObservableCollection<CompoundItem> obslist = new ObservableCollection<CompoundItem>();

        public Compounds(IFlowsheet fs, DynamicLayout layout)
		{
            flowsheet = fs;
            container = layout;
			Initialize();
		}

        void Initialize()
        {

            var complist = flowsheet.AvailableCompounds.Values.ToList().OrderBy(x => x.Name).ToList();

            var newlist = new List<ICompoundConstantProperties>();
            var listitems = new List<CheckBox>();

            s.CreateAndAddLabelRow(container, "Simulation Compounds");

            s.CreateAndAddDescriptionRow(container, "Check compounds to add them to the simulation, uncheck to remove. You may have to double-click on the checkbox in order to change its state (checked/unchecked).");

            s.CreateAndAddDescriptionRow(container, "Number of available compounds: " + complist.Count().ToString());

            s.CreateAndAddStringEditorRow2(container, "Search", "Search by Name, Formula, CAS ID or Database", "", (sender, e) => { 
                newlist = complist.Where((x) => x.Name.ToLower().Contains(sender.Text.ToLower()) ||
                                    x.Formula.ToLower().Contains(sender.Text.ToLower()) ||
                                    x.CAS_Number.ToLower().Contains(sender.Text.ToLower()) ||
                                    x.CurrentDB.ToLower().Contains(sender.Text.ToLower())).OrderBy((x) => x.Name).ToList();
                Application.Instance.AsyncInvoke(() => UpdateList(newlist));
            });

            UpdateList(complist);

        }

        void UpdateList(List<ICompoundConstantProperties> list)
        {
            obslist.Clear();
            foreach (var cp in list)
            {
                if (flowsheet.SelectedCompounds.ContainsKey(cp.Name))
                {
                    obslist.Add(new CompoundItem { Text = cp.Name, Formula = cp.Formula, CAS = cp.CAS_Number, Database = cp.OriginalDB, Check = true });
                }
            }
            foreach (var cp in list)
            {
                if (!flowsheet.SelectedCompounds.ContainsKey(cp.Name))
                {
                    obslist.Add(new CompoundItem { Text = cp.Name, Formula = cp.Formula, CAS = cp.CAS_Number, Database = cp.OriginalDB, Check = false });
                }
            }

            var listcontainer = new GridView { DataStore = obslist };

            var col2 = new GridColumn
            {
                DataCell = new CheckBoxCell { Binding = Binding.Property<CompoundItem, bool?>(r => r.Check) },
                HeaderText = "Added",
                Editable = true,
            };
            col2.AutoSize = true;

            listcontainer.CellEdited += (sender, e) =>
            {
                UpdateCompound(((CompoundItem)e.Item).Text);
            };

            listcontainer.Columns.Add(col2);

            var col1 = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<CompoundItem, string>(r => r.Text) },
                HeaderText = "Compound"
            };
            col1.AutoSize = true;
            listcontainer.Columns.Add(col1);
            var col1a = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<CompoundItem, string>(r => r.Formula) },
                HeaderText = "Formula"
            };
            col1a.AutoSize = true;
            listcontainer.Columns.Add(col1a);
            var col1b = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<CompoundItem, string>(r => r.CAS) },
                HeaderText = "CAS Number"
            };
            col1b.AutoSize = true;
            listcontainer.Columns.Add(col1b);
            var col1c = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<CompoundItem, string>(r => r.Database) },
                HeaderText = "Database"
            };
            col1c.AutoSize = true;
            listcontainer.Columns.Add(col1c);

            var scroll = new Scrollable { Content = listcontainer, Height = 315 };
            
            s.CreateAndAddControlRow(container, scroll);
        }

        void UpdateCompound(String name)
        {

            if (flowsheet.SelectedCompounds.ContainsKey(name))
            {
                flowsheet.SelectedCompounds.Remove(name);
                foreach (IMaterialStream obj in flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream))
                {
                    foreach (var phase in obj.Phases.Values)
                    {
                        phase.Compounds.Remove(name);
                    }
                }
            }
            else
            {
                flowsheet.SelectedCompounds.Add(name, flowsheet.AvailableCompounds[name]);
                foreach (IMaterialStream obj in flowsheet.SimulationObjects.Values.Where((x) => x.GraphicObject.ObjectType == ObjectType.MaterialStream))
                {
                    foreach (var phase in obj.Phases.Values)
                    {
                        phase.Compounds.Add(name, new Compound(name, ""));
                        phase.Compounds[name].ConstantProperties = flowsheet.SelectedCompounds[name];
                    }
                }
            }

        }

    }

    class CompoundItem
    {
    
        public string Text { get; set; }

        public string Formula { get; set; }

        public string CAS { get; set; }

        public string Database { get; set; }

        public bool Check { get; set; }

    }

}
