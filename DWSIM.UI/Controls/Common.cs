using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;

namespace DWSIM.UI.Forms.Controls
{
    static class Common
    {

        public static bool IsValidDouble(string s)
        {
            double d = 0;
            return double.TryParse(s, out d);
        }

        public static Form GetDefaultEditorForm(string title, int width, int height, DynamicLayout content)
        {
            content.CreateAndAddEmptySpace();
            content.EndVertical();
            content.Width = width - content.Padding.Value.Left * 2 - content.Padding.Value.Right * 2;
            return new Form()
            {
                Content = new Scrollable { Content = content },
                Title = title,
                Width = width,
                Height = height,
                ShowInTaskbar = false,
                Maximizable = false,
                Minimizable = false,
                Topmost = true,
                Resizable = true,
                Icon = Eto.Drawing.Icon.FromResource("DWSIM.UI.Forms.Resources.Icons.DWSIM_ico.ico")
            };
        }

        public static Form GetDefaultTabbedForm(string title, int width, int height, DynamicLayout[] contents)
        {

            List<TabPage> tabs = new List<TabPage>();

            foreach (var content in contents)
            {
                content.CreateAndAddEmptySpace();
                content.EndVertical();
                content.Width = width - content.Padding.Value.Left * 2 - content.Padding.Value.Right * 2;
                tabs.Add(new TabPage(content) { Text = (string)content.Tag });
            }
            var form = new Form()
            {
                Title = title,
                Width = width,
                Height = height,
                ShowInTaskbar = false,
                Maximizable = false,
                Minimizable = false,
                Topmost = true,
                Resizable = true,
                Icon = Eto.Drawing.Icon.FromResource("DWSIM.UI.Forms.Resources.Icons.DWSIM_ico.ico")
            };
        
            var tabctrl = new TabControl();
            foreach (var tab in tabs)
            {
                tabctrl.Pages.Add(tab);
            }

            form.Content = tabctrl;

            return form;

        }


        public static DynamicLayout GetDefaultContainer()
        {
            var content = new DynamicLayout();
            content.BeginVertical();
            content.Padding = 10;
            return content;
        }

        public static void AddEmptyRow(this DynamicLayout tbl)
        {
            tbl.AddRow(null);
        }

        public static DropDown CreateAndAddDropDownRow(this DynamicLayout container, String text, List<String> options, int position, Action<object, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var drop = new DropDown { Width = 100 };

            foreach (var item in options)
            {
                drop.Items.Add(item);
            }

            drop.SelectedIndex = position;

            if (command != null) drop.SelectedIndexChanged += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(txt, null, drop);

            container.AddRow(tr);

            return drop;

        }

        public static void CreateAndAddLabelRow(this DynamicLayout container, String text)
        {
            container.AddRow(new TableRow(new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word }));
            container.AddRow(new TableRow(new Label { Text = "", Height = 5 }));
        }
        public static void CreateAndAddDescriptionRow(this DynamicLayout container, String text)
        {
            container.AddRow(new TableRow(new Label { Text = text, Wrap = WrapMode.Word, Font = SystemFonts.Label(SystemFonts.Default().Size - 2.0f) }));
        }

        public static TextBox CreateAndAddTextBoxRow(this DynamicLayout container, String numberformat, String text, Double currval, Action<object, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval.ToString(numberformat), Width = 100 };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(txt, null, edittext);

            container.AddRow(tr);

            return edittext;

        }

        public static TextBox CreateAndAddStringEditorRow(this DynamicLayout container, String text, String currval, Action<object, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval, Width = 100 };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(txt, null, edittext);

            container.AddRow(tr);

            return edittext;

        }

        public static void CreateAndAddEmptySpace(this DynamicLayout container)
        {
            container.AddRow(new TableRow(new Label { Text = "", Height = 5 }));
        }

        public static TextBox CreateAndAddFullTextBoxRow(this DynamicLayout container, String text, Action<object, EventArgs> command)
        {

            var edittext = new TextBox { Text = text };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(edittext);

            container.AddRow(tr);

            return edittext;

        }

        public static Button CreateAndAddLabelAndButtonRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, Action<object, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            var btn = new Button { Width = 100, Text = buttonlabel };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(txt, null, btn);

            container.AddRow(tr);

            return btn;


        }

        public static Button CreateAndAddButtonRow(this DynamicLayout container, String buttonlabel, String imageResID, Action<object, EventArgs> command)
        {

            var btn = new Button { Width = 100, Text = buttonlabel };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke(sender, e);

            var tr = new TableRow(btn);

            container.AddRow(tr);

            return btn;


        }

        public static CheckBox CreateAndAddCheckBoxRow(this DynamicLayout container, String text, bool value, Action<object, EventArgs> command)
        {

            var check = new CheckBox { Text = text, Checked = value };

            if (command != null) check.CheckedChanged += (sender, e) => command.Invoke(sender, e);

            container.AddRow(new TableRow(check));

            return check;
        }


    }
}
