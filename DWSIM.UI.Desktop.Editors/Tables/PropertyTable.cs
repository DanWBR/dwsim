using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using DWSIM.Interfaces;
using DWSIM.Interfaces.Enums.GraphicObjects;

using Eto.Forms;
using Eto.Drawing;

using DWSIM.ExtensionMethods;
using DWSIM.UI.Shared;
using s = DWSIM.UI.Shared.Common;
using DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables;

namespace DWSIM.UI.Desktop.Editors.Tables
{
    class PropertyTableEditor : Dialog
    {

        public TableGraphic Table;

        public int mode = 0;

        private bool initialized = false;

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        public Button btnOK, btnCancel;
        public ListBox lvObjects, lvProps;

        public PropertyTableEditor()
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

            Title = "Configure Property Table";

            var container = new TableLayout();

            var topcontainer = new TableLayout();
            var centercontainer = new TableLayout();
            var bottomcontainer = new TableLayout();

            lvObjects = new ListBox { Height = 300, Width = 250 };
            lvProps = new ListBox { Height = 300, Width = 250 };

            btnOK = new Button { Text = "Close", Enabled = true };

            btnOK.Click += (sender, e) =>
            {
                Close();
            };

            topcontainer.Rows.Add(s.CreateAndAddStringEditorRow2())

            centercontainer.Rows.Add(new TableRow(new Label { Text = "Object Type / Instance / Property" }));
            centercontainer.Rows.Add(new TableRow(lvObjects, lvProps));
            centercontainer.Padding = new Padding(5, 5, 5, 5);
            centercontainer.Spacing = new Size(10, 10);

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
                       

                list1.SelectedIndexChanged += (sender2, e2) =>
                {

                    if (!initialized) return;

                    var obj = ObjList[list1.SelectedValue.ToString()];

                    var objs = Flowsheet.SimulationObjects.Values.Where((x) => x.GetType().Equals(obj.GetType())).ToList();

                    list2.Items.Clear();
                    list2.Items.AddRange(objs.Select((x) => new ListItem { Text = x.GraphicObject.Tag, Key = x.Name }));

                };

                list1.SelectedIndex = 0;
            };

            initialized = true;


        }

    }
}
