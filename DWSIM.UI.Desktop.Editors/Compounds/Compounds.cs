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

            var complist = flowsheet.AvailableCompounds.Values.ToList().OrderBy(x => x.Name);

            var newlist = new List<string>();
            var listitems = new List<CheckBox>();

            s.CreateAndAddLabelRow(container, "Simulation Compounds");

            s.CreateAndAddDescriptionRow(container, "Check compounds to add them to the simulation, uncheck to remove. You may have to double-click on the checkbox in order to change its state (checked/unchecked).");

            s.CreateAndAddDescriptionRow(container, "Number of available compounds: " + complist.Count().ToString());

            s.CreateAndAddStringEditorRow2(container, "Search", "Search by Name, CAS ID or Formula", "", (sender, e) => { 
                newlist = complist.Where((x) => x.Name.ToLower().Contains(sender.Text.ToLower()) ||
                                    x.Formula.ToLower().Contains(sender.Text.ToLower()) ||
                                    x.CurrentDB.ToLower().Contains(sender.Text.ToLower())).OrderBy((x) => x.Name.Length).Select((x) => x.Name).ToList();
                Application.Instance.AsyncInvoke(() => UpdateList(newlist));
            });

            UpdateList(complist.Select((x) => x.Name).ToList());

        }

        void UpdateList(List<string> list)
        {
            obslist.Clear();
            foreach (string cp in list)
            {
                if (flowsheet.SelectedCompounds.ContainsKey(cp))
                {
                    obslist.Add(new CompoundItem { Text = cp, Check = true });
                }
            }
            foreach (string cp in list)
            {
                if (!flowsheet.SelectedCompounds.ContainsKey(cp))
                {
                    obslist.Add(new CompoundItem { Text = cp, Check = false });
                }
            }

            var listcontainer = new GridView { DataStore = obslist };
            var col1 = new GridColumn
            {
                DataCell = new TextBoxCell { Binding = Binding.Property<CompoundItem, string>(r => r.Text) },
                HeaderText = "Compound"
            };
            col1.AutoSize = true;
            listcontainer.Columns.Add(col1);
            var col2 = new GridColumn
            {
                DataCell = new CheckBoxCell { Binding = Binding.Property<CompoundItem, bool?>(r => r.Check) },
                HeaderText = "Added",
                Editable = true,
            };
            col2.AutoSize = true;

            listcontainer.CellEdited += (sender, e) => {
                UpdateCompound(((CompoundItem)e.Item).Text);
            };

            listcontainer.Columns.Add(col2);
            
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

        public bool Check { get; set; }

    }

}
