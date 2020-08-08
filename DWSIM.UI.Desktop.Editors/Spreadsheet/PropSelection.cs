using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;

namespace DWSIM.UI.Desktop.Editors
{
    class PropertySelector : Dialog
    {

        public Interfaces.IFlowsheet Flowsheet;

        public int mode = 0;

        private bool initialized = false;

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        public Button btnOK, btnCancel;
        public ListBox list1, list2, list3, list4; 

        public PropertySelector()
        {
            Init();
        }


        void Init()
        {

            string imgprefix = "DWSIM.UI.Desktop.Editors.Resources.Icons.";

            Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");

            Maximizable = false;
            Minimizable = false;
            WindowStyle = Eto.Forms.WindowStyle.Default;

            Title = "Select Object Property to Import from / Export to";

            var container = new TableLayout();

            var topcontainer = new TableLayout();

            var bottomcontainer = new TableLayout();

            var lblName = new Label() { Width = 300 };
            var lblVersion = new Label();
            var lblLib = new Label();
            var lblLibVersion = new Label();
            var lblDesc = new TextArea { ReadOnly = true };
            var lblAbout = new TextArea { ReadOnly = true };

            list1 = new ListBox { Height = 300, Width = 200 };
            list2 = new ListBox { Height = 300, Width = 150 };
            list3 = new ListBox { Height = 300, Width = 250 };
            list4 = new ListBox { Height = 300, Width = 100 };

            btnOK = new Button { Text = "OK", Enabled = false };

            btnCancel = new Button { Text = "Cancel" };

            btnCancel.Click += (sender, e) =>
            {
                Close();
            };

            topcontainer.Rows.Add(new TableRow(new Label { Text = "Type / Object / Property / Units" }));
            topcontainer.Rows.Add(new TableRow(list1, list2, list3, list4));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            bottomcontainer.Rows.Add(new TableRow(null, btnCancel, btnOK));
            bottomcontainer.Padding = new Padding(5, 5, 5, 5);
            bottomcontainer.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(topcontainer));
            container.Rows.Add(new TableRow(bottomcontainer));

            container.Padding = new Padding(5, 5, 5, 5);

            Content = container;

            Load += (sender, e) =>
            {

                foreach (var item in ObjList)
                {
                    var li = new ListItem() { Text = item.Key };
                    list1.Items.Add(li);
                }

                list3.SelectedIndexChanged += (sender2, e2) => {
                    
                    btnOK.Enabled = list3.SelectedKey != null;

                    list4.Items.Clear();

                    var obj = Flowsheet.SimulationObjects[list2.SelectedKey];
                    var prop = list3.SelectedKey;
                    var unit = obj.GetPropertyUnit(prop);
                    var su = Flowsheet.FlowsheetOptions.SelectedUnitSystem;
                    var units = su.GetUnitSet(su.GetUnitType(unit));

                    foreach (var item in units)
                    {
                        list4.Items.Add(item);
                    }

                };

                list2.SelectedIndexChanged += (sender2, e2) => {

                    list3.Items.Clear();

                    if (list2.SelectedKey == null) return;

                    string[] properties = null;
                    if (mode == 0)
                    {
                        properties = Flowsheet.SimulationObjects[list2.SelectedKey].GetProperties(Interfaces.Enums.PropertyType.ALL);
                    }
                    else
                    {
                        properties = Flowsheet.SimulationObjects[list2.SelectedKey].GetProperties(Interfaces.Enums.PropertyType.WR);
                    }

                    foreach (string prop in properties)
                    {
                        list3.Items.Add(Flowsheet.GetTranslatedString(prop), prop);
                    }

                };

                list1.SelectedIndexChanged += (sender2, e2) =>
                {

                    if (!initialized) return;

                    var obj = ObjList[list1.SelectedValue.ToString()];

                    var objs = Flowsheet.SimulationObjects.Values.Where((x) => x.GetType().Equals(obj.GetType())).ToList();

                    list2.Items.Clear();
                    list2.Items.AddRange(objs.Select((x) => new ListItem {Text = x.GraphicObject.Tag, Key = x.Name}));

                };

                list1.SelectedIndex = 0;
            };

            initialized = true;


        }

    
    }
}
