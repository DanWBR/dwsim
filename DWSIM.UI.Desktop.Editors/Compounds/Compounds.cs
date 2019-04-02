using System;
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;
using DWSIM.Thermodynamics.BaseClasses;
using DWSIM.UI.Desktop.Shared;
using Eto.Drawing;
using Eto.Forms;
using s = DWSIM.UI.Shared.Common;

namespace DWSIM.UI.Desktop.Editors
{
    public class Compounds
    {

        public Flowsheet flowsheet;
        public TableLayout container;

        private ObservableCollection<CompoundItem> obslist = new ObservableCollection<CompoundItem>();

        public GridView listcontainer;

        public Compounds(IFlowsheet fs, TableLayout layout)
        {
            flowsheet = (Flowsheet)fs;
            container = layout;
            Initialize();
        }

        void Initialize()
        {

            var complist = flowsheet.AvailableCompounds.Values.ToList().OrderBy(x => x.Name).ToList();

            var newlist = new List<ICompoundConstantProperties>();
            var listitems = new List<CheckBox>();

            container.Padding = 10;

            container.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(new Label { Text = "Simulation Compounds", Font = new Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())}));

            container.Rows.Add(new TableRow(new Label { Text = "Check compounds to add them to the simulation, uncheck to remove.", Font = SystemFonts.Label(DWSIM.UI.Shared.Common.GetEditorFontSize() - 2.0f) }));
            if (Application.Instance.Platform.IsWpf)
            {
                container.Rows.Add(new TableRow(new Label { Text = "To commit the changes, select another table cell or press ENTER after checking/unchecking the compound. You may have to double-click on the checkbox in order to change its state (checked/unchecked).", Font = SystemFonts.Label(DWSIM.UI.Shared.Common.GetEditorFontSize() - 2.0f) }));
            }
            container.Rows.Add(new TableRow(new Label { Text = "Number of compounds available: " + complist.Count().ToString(), Font = SystemFonts.Label(DWSIM.UI.Shared.Common.GetEditorFontSize() - 2.0f) }));

            var searchcontainer = new DynamicLayout();

            var txtsearch = new Label { Text = "Search  ", VerticalAlignment = VerticalAlignment.Center };
            txtsearch.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            var edittext = new TextBox { Text = "", PlaceholderText = "Search by Name, Formula, CAS ID or Database (press ENTER to search)" };
            edittext.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());

            var tr = new TableRow(txtsearch, edittext);

            searchcontainer.AddRow(tr);

            edittext.KeyDown += (sender, e) =>
            {
                if (e.Key == Keys.Enter)
                {
                    newlist = complist.Where((x) => x.Name.ToLower().Contains(edittext.Text.ToLower()) ||
                                            x.Formula.ToLower().Contains(edittext.Text.ToLower()) ||
                                            x.CAS_Number.ToLower().Contains(edittext.Text.ToLower()) ||
                                            x.CurrentDB.ToLower().Contains(edittext.Text.ToLower()))
                                            .ToList().OrderBy((x) => x.Name.Length).ToList();
                    Application.Instance.AsyncInvoke(() => UpdateList(newlist));
                }
            };

            container.Rows.Add(new TableRow(searchcontainer));

            var txt = new Label { Text = "Click to view properties of the selected compound", VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            var btn = new Button { Width = 200, Text = "View Properties" };
            btn.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());

            btn.Click += (sender, e) =>
            {
                if (listcontainer.SelectedItem == null) return;
                var compound = ((CompoundItem)listcontainer.SelectedItem);
                var form = s.GetDefaultEditorForm("Compound Properties: " + compound.Text, 800, 600, new CompoundViewer(flowsheet, flowsheet.AvailableCompounds[compound.Text]), false);
                form.Show();
            };

            var cont = new DynamicLayout();
            cont.AddRow(new TableRow(txt, null, btn));

            container.Rows.Add(cont);

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

            listcontainer = new GridView { DataStore = obslist, RowHeight = 20 };
            listcontainer.Style = "fastgrid";

            if (Application.Instance.Platform.IsWinForms) listcontainer.Height = 370;

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

            container.Rows.Add(new TableRow(new Scrollable { Content = listcontainer, Border = BorderType.None }));

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
