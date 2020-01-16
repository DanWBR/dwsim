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
    public class MasterPropertyTableEditor : Form
    {

        public MasterTableGraphic Table;

        public Button btnOK, btnOrderDown, btnOrderUp;
        public ListBox lvObjects, lvProps, lvSelectObj, lvSelectProp;
        public DropDown cbObjectType, cbOrderBy;
        public NumericStepper nsNumberOfLines;

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
            var topcontainer2 = new TableLayout();
            var centercontainer = new TableLayout();
            var bottomcontainer = new TableLayout();

            var tableleft = new TableLayout { Width = 200 };

            btnOrderUp = new Button { Text = "˄", Width = 25 };
            btnOrderDown = new Button { Text = "˅", Width = 25 };

            tableleft.Rows.Add(new TableRow(new Label { Text = "Show Objects/Properties:", VerticalAlignment = VerticalAlignment.Center }));
            tableleft.Rows.Add(null);
            tableleft.Rows.Add(new TableRow(new Label { Text = "Order objects", VerticalAlignment = VerticalAlignment.Center }, null, btnOrderUp, btnOrderDown));

            lvObjects = new ListBox { Height = 300, Width = 150 };
            lvProps = new ListBox { Height = 300, Width = 150 };
            lvSelectObj = new ListBox { Height = 300, Width = 75 };
            lvSelectProp = new ListBox { Height = 300, Width = 75 };

            btnOK = new Button { Text = "Close", Enabled = true };

            btnOK.Click += (sender, e) => Close();

            var header = new TextBox();
            header.TextChanged += (sender, e) => Table.HeaderText = header.Text;

            cbObjectType = new DropDown { Width = 300 };
            cbOrderBy = new DropDown { Width = 200 };

            topcontainer2.Rows.Add(new TableRow(new Label { Text = "Show Objects of Type", VerticalAlignment = VerticalAlignment.Center }, cbObjectType, null, new Label { Text = "Order Objects By", VerticalAlignment = VerticalAlignment.Center }, cbOrderBy));
            topcontainer2.Padding = new Padding(5, 5, 5, 5);
            topcontainer2.Spacing = new Size(10, 10);

            nsNumberOfLines = new NumericStepper { MinValue = 1, MaxValue = 10, Value = 1, DecimalPlaces = 0, Increment = 1.0 };

            nsNumberOfLines.ValueChanged += (s, e) =>
            {
                if (Table != null) Table.NumberOfLines = (int)nsNumberOfLines.Value;
            };

            topcontainer.Rows.Add(new TableRow(new Label { Text = "Table Header", VerticalAlignment = VerticalAlignment.Center }, header, new Label { Text = "Number of Grouping Rows", VerticalAlignment = VerticalAlignment.Center }, nsNumberOfLines));
            topcontainer.Rows[0].Cells[1].ScaleWidth = true;
            topcontainer.Padding = new Padding(5, 5, 5, 5);
            topcontainer.Spacing = new Size(10, 10);

            centercontainer.Rows.Add(new TableRow(tableleft, lvObjects, lvSelectObj, lvProps, lvSelectProp));
            centercontainer.Rows.Last().Cells[3].ScaleWidth = true;
            centercontainer.Padding = new Padding(5, 5, 5, 5);
            centercontainer.Spacing = new Size(10, 10);

            bottomcontainer.Rows.Add(new TableRow(null, btnOK));
            bottomcontainer.Padding = new Padding(5, 5, 5, 5);
            bottomcontainer.Spacing = new Size(10, 10);

            container.Rows.Add(new TableRow(topcontainer));
            container.Rows.Add(new TableRow(topcontainer2));
            container.Rows.Add(new TableRow(centercontainer));
            container.Rows.Add(new TableRow(bottomcontainer));
            container.Rows.Add(null);

            container.Padding = new Padding(5, 5, 5, 5);

            Content = container;

            cbObjectType.SelectedIndexChanged += (sender, e) =>
            {
                if (Loaded)
                {
                    Table.ObjectFamily = (Interfaces.Enums.GraphicObjects.ObjectType)Enum.Parse(Table.ObjectType.GetType(), cbObjectType.SelectedValue.ToString());
                    Table.ObjectList.Clear();
                    Table.SortedList.Clear();
                    Table.PropertyList.Clear();
                    Populate();
                }
            };

            cbOrderBy.SelectedIndexChanged += (sender, e) =>
            {
                if (cbOrderBy.SelectedIndex < 0) return;
                Table.SortBy = cbOrderBy.SelectedValue.ToString();
                if (Table.SortBy == "Custom")
                {
                    btnOrderDown.Enabled = true;
                    btnOrderUp.Enabled = true;
                }
                else
                {
                    btnOrderDown.Enabled = false;
                    btnOrderUp.Enabled = false;
                }
                Populate();
            };

            bool adding = false;

            lvObjects.SelectedIndexChanged += (sender, e) =>
            {
                if (lvObjects.SelectedIndex < 0) return;
                if (lvObjects.SelectedValue != null)
                {
                    adding = true;
                    lvSelectObj.Items.Clear();
                    lvSelectObj.Items.Add("Show");
                    lvSelectObj.Items.Add("Hide");
                    adding = false;
                    if (Table.ObjectList.ContainsKey(lvObjects.SelectedValue.ToString()))
                    {
                        if (Table.ObjectList[lvObjects.SelectedValue.ToString()])
                        {
                            lvSelectObj.SelectedIndex = 0;
                        }
                        else
                        {
                            lvSelectObj.SelectedIndex = 1;
                        }
                    }
                }
            };


            lvProps.SelectedIndexChanged += (sender, e) =>
            {
                if (lvProps.SelectedIndex < 0) return;
                if (lvProps.SelectedValue != null)
                {
                    adding = true;
                    lvSelectProp.Items.Clear();
                    lvSelectProp.Items.Add("Show");
                    lvSelectProp.Items.Add("Hide");
                    adding = false;
                    if (Table.PropertyList.ContainsKey(lvProps.SelectedKey))
                    {
                        if (Table.PropertyList[lvProps.SelectedKey])
                        {
                            lvSelectProp.SelectedIndex = 0;
                        }
                        else
                        {
                            lvSelectProp.SelectedIndex = 1;
                        }
                    }
                }
            };

            lvSelectObj.SelectedIndexChanged += (sender, e) =>
            {
                if (lvSelectObj.SelectedIndex < 0) return;
                if (!adding)
                {
                    if (Table.ObjectList.ContainsKey(lvObjects.SelectedValue.ToString()))
                    {
                        Table.ObjectList[lvObjects.SelectedValue.ToString()] = lvSelectObj.SelectedIndex == 0 ? true : false;
                    }
                }
            };

            lvSelectProp.SelectedIndexChanged += (sender, e) =>
            {
                if (lvSelectProp.SelectedIndex < 0) return;
                if (!adding)
                {
                    if (Table.PropertyList.ContainsKey(lvProps.SelectedKey))
                    {
                        Table.PropertyList[lvProps.SelectedKey] = lvSelectProp.SelectedIndex == 0 ? true : false;
                    }
                }
            };


            btnOrderUp.Click += (sender, e) =>
            {
                int index = 0;
                if (this.lvObjects.SelectedValue != null)
                {
                    index = this.lvObjects.SelectedIndex;
                    if (index != 0)
                    {
                        ListItem lvi = new ListItem { Text = lvObjects.SelectedValue.ToString(), Key = lvObjects.SelectedKey };
                        this.lvObjects.Items.RemoveAt(index);
                        this.lvObjects.Items.Insert(index - 1, lvi);
                        this.lvObjects.SelectedIndex = index - 1;
                    }
                }
            };

            btnOrderDown.Click += (sender, e) =>
            {
                int index = 0;
                if (this.lvObjects.SelectedValue != null)
                {
                    index = this.lvObjects.SelectedIndex;
                    if (index != lvObjects.Items.Count - 1)
                    {
                        ListItem lvi = new ListItem { Text = lvObjects.SelectedValue.ToString(), Key = lvObjects.SelectedKey };
                        this.lvObjects.Items.RemoveAt(index);
                        this.lvObjects.Items.Insert(index + 1, lvi);
                        this.lvObjects.SelectedIndex = index + 1;
                    }
                }
            };

            Load += (sender, e) =>
            {
                header.Text = Table.HeaderText;

                var names = Enum.GetNames(Table.ObjectType.GetType());
                foreach (var name in names)
                {
                    cbObjectType.Items.Add(name);
                }

                var sitems = Table.SortableItems;
                foreach (var item in sitems)
                {
                    cbOrderBy.Items.Add(item);
                }

                cbObjectType.SelectedIndex = names.ToList().IndexOf(Table.ObjectFamily.ToString());
                cbOrderBy.SelectedIndex = sitems.ToList().IndexOf(Table.SortBy);

                nsNumberOfLines.Value = Table.NumberOfLines;

            };

            Closed += (sender, e) =>
            {

                List<string> list = new List<string>();
                foreach (ListItem lvi in this.lvObjects.Items)
                {
                    list.Add(lvi.Text);
                }
                Table.SortedList = list;

            };

        }

        public void Populate()
        {
            lvObjects.Items.Clear();

            if (Table.SortBy == "Custom")
            {
                foreach (var item in Table.SortedList)
                {
                    if (Table.Flowsheet.GetFlowsheetSimulationObject(item) != null)
                    {
                        ListItem lvi = new ListItem { Text = item, Key = item };
                        lvObjects.Items.Add(lvi);
                    }
                }
                foreach (var obj in Table.Flowsheet.SimulationObjects.Values)
                {
                    if (obj.GraphicObject.ObjectType == Table.ObjectFamily & !Table.SortedList.Contains(obj.GraphicObject.Tag))
                    {
                        if (!Table.ObjectList.ContainsKey(obj.GraphicObject.Tag))
                            Table.ObjectList.Add(obj.GraphicObject.Tag, false);
                        ListItem lvi = new ListItem { Text = obj.GraphicObject.Tag };
                        lvi.Key = "Object|" + obj.Name;
                        lvObjects.Items.Add(lvi);
                    }
                }
            }
            else
            {
                foreach (var obj in Table.Flowsheet.SimulationObjects.Values)
                {
                    if (obj.GraphicObject.ObjectType == Table.ObjectFamily)
                    {
                        if (!Table.ObjectList.ContainsKey(obj.GraphicObject.Tag))
                            Table.ObjectList.Add(obj.GraphicObject.Tag, false);
                        ListItem lvi = new ListItem { Text = obj.GraphicObject.Tag };
                        lvi.Key = "Object|" + obj.Name;
                        lvObjects.Items.Add(lvi);
                    }
                }
            }

            string[] props = null;

            lvProps.Items.Clear();
            if (Table.ObjectList.Count > 0)
            {
                foreach (string s in Table.ObjectList.Keys)
                {
                    props = Table.Flowsheet.GetFlowsheetSimulationObject(s).GetProperties(PropertyType.ALL);
                    break;
                }
                foreach (string p in props)
                {
                    if (!Table.PropertyList.ContainsKey(p))
                        Table.PropertyList.Add(p, false);
                    ListItem lvi = new ListItem { Text = Table.Flowsheet.GetTranslatedString(p) };
                    lvi.Key = p;
                    lvProps.Items.Add(lvi);
                }
            }

        }

    }
}
