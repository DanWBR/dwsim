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
using DWSIM.Interfaces.Enums;

namespace DWSIM.UI.Desktop.Editors.Tables
{
    public class MasterPropertyTableEditor : Dialog
    {

        public MasterTableGraphic Table;

        public int mode = 0;

        private bool initialized = false;

        public Dictionary<String, Interfaces.ISimulationObject> ObjList;

        public Button btnOK, btnCancel;
        public ListBox lvObjects, lvProps, lvSelect;

        public MasterPropertyTableEditor()
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

            Title = "Configure Master Property Table";

            var container = new TableLayout();

            var topcontainer = new TableLayout();
            var centercontainer = new TableLayout();
            var bottomcontainer = new TableLayout();

            lvObjects = new ListBox { Height = 300, Width = 250 };
            lvProps = new ListBox { Height = 300, Width = 250 };
            lvSelect = new ListBox { Height = 300, Width = 250 };

            btnOK = new Button { Text = "Close", Enabled = true };

            btnOK.Click += (sender, e) =>
            {
                Close();
            };

            var header = new TextBox();
            header.TextChanged += (sender, e) =>
            {
                Table.HeaderText = header.Text;
            };

            topcontainer.Rows.Add(new TableRow(new Label { Text = "Table Header" }, header));
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            centercontainer.Rows.Add(new TableRow(new Label { Text = "Object / Property / Show" }));
            centercontainer.Rows.Add(new TableRow(lvObjects, lvProps, lvSelect));
            centercontainer.Padding = new Padding(5, 5, 5, 5);
            centercontainer.Spacing = new Size(10, 10);

            bottomcontainer.Rows.Add(new TableRow(null, btnCancel, btnOK));
            bottomcontainer.Padding = new Padding(5, 5, 5, 5);
            bottomcontainer.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(topcontainer));
            container.Rows.Add(new TableRow(centercontainer));
            container.Rows.Add(new TableRow(bottomcontainer));

            container.Padding = new Padding(5, 5, 5, 5);

            Content = container;

            lvObjects.SelectedIndexChanged += (sender, e) =>
            {
                if (lvObjects.SelectedValue != null)
                {
                    lvProps.Items.Clear();
                    foreach (var item in Table.Flowsheet.SimulationObjects[lvObjects.SelectedKey].GetProperties(PropertyType.ALL))
                    {
                        lvProps.Items.Add(Table.Flowsheet.GetTranslatedString(item), item);
                    }
                }
            };

            //bool adding = false;

            lvProps.SelectedIndexChanged += (sender, e) =>
            {
                //if (lvProps.SelectedValue != null)
                //{
                //    if (!Table.VisibleProperties.ContainsKey(lvObjects.SelectedKey))
                //    {
                //        Table.VisibleProperties.Add(lvObjects.SelectedKey, new List<string>());
                //    }
                //    adding = true;
                //    lvSelect.Items.Clear();
                //    lvSelect.Items.Add("Show");
                //    lvSelect.Items.Add("Hide");
                //    adding = false;
                //    if (Table.VisibleProperties.ContainsKey(lvObjects.SelectedKey))
                //    {
                //        if (Table.VisibleProperties[lvObjects.SelectedKey].Contains(lvProps.SelectedKey))
                //        {
                //            lvSelect.SelectedIndex = 0;
                //        }
                //        else
                //        {
                //            lvSelect.SelectedIndex = 1;
                //        }
                //    }
                //}
            };

            lvSelect.SelectedIndexChanged += (sender, e) =>
            {
                //if (!adding)
                //{
                //    if (lvSelect.SelectedIndex == 0)
                //    {
                //        if (!Table.VisibleProperties[lvObjects.SelectedKey].Contains(lvProps.SelectedKey))
                //        {
                //            Table.VisibleProperties[lvObjects.SelectedKey].Add(lvProps.SelectedKey);
                //        }
                //    }
                //    else if (lvSelect.SelectedIndex == 1)
                //    {
                //        if (Table.VisibleProperties[lvObjects.SelectedKey].Contains(lvProps.SelectedKey))
                //        {
                //            Table.VisibleProperties[lvObjects.SelectedKey].Remove(lvProps.SelectedKey);
                //        }
                //    }
                //}
            };


            Load += (sender, e) =>
            {
                header.Text = Table.HeaderText;
                
                //lvObjects.Items.Clear();
                //foreach (var obj in Table.Flowsheet.SimulationObjects.Values)
                //{
                //    lvObjects.Items.Add(obj.GraphicObject.Tag, obj.Name);
                //}

            };

            initialized = true;


        }

    }
}
