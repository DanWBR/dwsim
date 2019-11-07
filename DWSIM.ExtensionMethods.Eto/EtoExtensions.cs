using System;
using System.Collections.Generic;
using Eto.Drawing;
using Eto.Forms;
using DWSIM.ExtensionMethods;
using System.IO;
using VerticalAlignment = Eto.Forms.VerticalAlignment;
using DWSIM.ExtensionMethods.Eto;

namespace DWSIM.UI.Shared
{
    public static class Common
    {

        static double sf = GlobalSettings.Settings.UIScalingFactor;

        static string imgprefix = "DWSIM.ExtensionMethods.Eto.Resources.Icons.";

        public static byte[] ImageToByte(System.Drawing.Bitmap img)
        {
            using (var stream = new MemoryStream())
            {
                img.Save(stream, System.Drawing.Imaging.ImageFormat.Png);
                return stream.ToArray();
            }
        }

        public static bool IsValidDouble(string s)
        {
            double d = 0;
            return double.TryParse(s, out d);
        }

        public static int GetEditorFontSize()
        {
            if (GlobalSettings.Settings.EditorFontSize == -1) GlobalSettings.Settings.EditorFontSize = (int)(new Eto.Drawing.Font(Eto.Drawing.SystemFont.Label).Size);
            return GlobalSettings.Settings.EditorFontSize;
        }

        public static Form GetDefaultEditorForm(string title, int width, int height, DynamicLayout content)
        {
            //content.CreateAndAddEmptySpace();
            //content.CreateAndAddEmptySpace();
            //content.CreateAndAddEmptySpace();
            //content.CreateAndAddEmptySpace();
            //content.CreateAndAddEmptySpace();
            //content.EndVertical();
            content.Width = width - content.Padding.Value.Left * 2 - content.Padding.Value.Right * 2;
            height += 10;
            return new Form()
            {
                Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                Content = new Scrollable { Content = content, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true },
                Title = title,
                ClientSize = new Size((int)(sf * width), (int)(sf * height))
                //ShowInTaskbar = false,
                //Maximizable = true,
                //Minimizable = false,
                //Topmost = true,
                //Resizable = true
            };
        }

        public static Form GetDefaultEditorForm(string title, int width, int height, TableLayout content, bool scrollable)
        {
            var form = new Form()
            {
                Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                Title = title,
                ClientSize = new Size((int)(sf * width), (int)(sf * height)),
                ShowInTaskbar = true
            };
            if (scrollable)
            {
                form.Content = new Scrollable { Content = content, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true };
            }
            else
            {
                form.Content = content;
            }
            return form;
        }

        public static Form GetDefaultEditorForm(string title, int width, int height, Control content, bool scrollable)
        {
            var form = new Form()
            {
                Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                Title = title,
                ClientSize = new Size((int)(sf * width), (int)(sf * height)),
                ShowInTaskbar = true
            };
            if (scrollable)
            {
                form.Content = new Scrollable { Content = content, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true };
            }
            else
            {
                form.Content = content;
            }
            return form;
        }

        public static Form GetDefaultTabbedForm(string title, int width, int height, Control[] contents)
        {
            List<TabPage> tabs = new List<TabPage>();

            foreach (var content in contents)
            {
                if (content is DynamicLayout)
                {
                    var dyncontent = (DynamicLayout)content;
                    dyncontent.CreateAndAddEmptySpace();
                    dyncontent.EndVertical();
                    dyncontent.Width = width - dyncontent.Padding.Value.Left * 2 - dyncontent.Padding.Value.Right * 2;
                }
                tabs.Add(new TabPage(new Scrollable { Content = content, Border = BorderType.None }) { Text = (string)content.Tag });
            }

            var form = new Form()
            {
                Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                Title = title,
                ClientSize = new Size((int)(sf * width), (int)(sf * height)),
                ShowInTaskbar = true
                //Maximizable = false,
                //Minimizable = false,
                //Topmost = true,
                //Resizable = true
            };

            var tabctrl = new TabControl();
            foreach (var tab in tabs)
            {
                tabctrl.Pages.Add(tab);
            }

            form.Content = tabctrl;

            return form;

        }

        public static Dialog CreateDialog(Control content, String title, int width = 0, int height = 0)
        {
            var alert = new Eto.Forms.Dialog();
            alert.Content = content;
            if (height != 0) alert.Height = (int)(sf * height);
            if (width != 0) alert.Width = (int)(sf * width);
            alert.Title = title;
            alert.Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico");
            return alert;
        }

        public static DynamicLayout GetDefaultContainer()
        {
            var content = new DynamicLayout();
            content.BeginVertical();
            content.Padding = 10;
            return content;
        }

        public static ComboBox CreateAndAddEditableDropDownRow(this DynamicLayout container, String text, List<String> options, int position, Action<ComboBox, EventArgs> selectionchangedcommand, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var drop = new ComboBox();
            drop.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (!Eto.Forms.Application.Instance.Platform.IsGtk)
            {
                if (GlobalSettings.Settings.EditorTextBoxFixedSize) drop.Width = (int)(sf * 140);
            }

            foreach (var item in options)
            {
                drop.Items.Add(new ListItem() { Key = item, Text = item });
            }

            drop.SelectedIndex = position;

            if (selectionchangedcommand != null) drop.SelectedIndexChanged += (sender, e) => selectionchangedcommand.Invoke((ComboBox)sender, e);
            if (keypress != null) drop.KeyUp += (sender, e) => { if (e.Key == Keys.Enter) keypress.Invoke(); };

            var tr = new TableRow(txt, null, drop);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return drop;

        }

        public static DropDown CreateAndAddDropDownRow(this DynamicLayout container, String text, List<String> options, int position, Action<DropDown, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var drop = new DropDown();
            drop.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (!Eto.Forms.Application.Instance.Platform.IsGtk)
            {
                if (GlobalSettings.Settings.EditorTextBoxFixedSize) drop.Width = (int)(sf * 140);
            }

            foreach (var item in options)
            {
                drop.Items.Add(new ListItem() { Key = item, Text = item });
            }

            drop.SelectedIndex = position;

            if (command != null) drop.SelectedIndexChanged += (sender, e) => command.Invoke((DropDown)sender, e);
            if (keypress != null) drop.SelectedIndexChanged += (sender, e) => keypress.Invoke();

            var tr = new TableRow(txt, null, drop);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return drop;

        }

        public static DropDown CreateAndAddDropDownRow(this TableLayout container, String text, List<String> options, int position, Action<DropDown, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var drop = new DropDown();
            drop.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (!Eto.Forms.Application.Instance.Platform.IsGtk)
            {
                if (GlobalSettings.Settings.EditorTextBoxFixedSize) drop.Width = (int)(sf * 140);
            }

            foreach (var item in options)
            {
                drop.Items.Add(new ListItem() { Key = item, Text = item });
            }

            drop.SelectedIndex = position;

            if (command != null) drop.SelectedIndexChanged += (sender, e) => command.Invoke((DropDown)sender, e);
            if (keypress != null) drop.SelectedIndexChanged += (sender, e) => keypress.Invoke();

            var tr = new TableRow(txt, null, drop);

            container.Rows.Add(tr);

            return drop;

        }

        public static Label CreateAndAddLabelRow(this DynamicLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            label.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            container.CreateAndAddEmptySpace();
            container.AddRow(new TableRow(label));
            container.CreateAndAddEmptySpace();
            container.AddRow(new TableRow(new Border { BorderThickness = 1}));
            container.CreateAndAddEmptySpace();
            return label;
        }

        public static Label CreateAndAddLabelRow(this TableLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            label.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            container.Rows.Add(new TableRow(label));
            container.Rows.Add(new TableRow(new Border { BorderThickness = 1 }));
            return label;
        }

        public static Label CreateAndAddLabelRow3(this DynamicLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            label.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            container.AddRow(new TableRow(label));
            container.CreateAndAddEmptySpace();
            return label;
        }

        public static Label CreateAndAddLabelRow3(this TableLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            label.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            container.Rows.Add(new TableRow(label));
            return label;
        }

        public static Label CreateAndAddLabelRow2(this DynamicLayout container, String text)
        {
            var lbl = new Label { Text = text, Wrap = WrapMode.Word };
            lbl.Font = new Font(SystemFont.Default, GetEditorFontSize());
            container.CreateAndAddEmptySpace();
            container.AddRow(new TableRow(lbl));
            container.CreateAndAddEmptySpace();
            return lbl;
        }

        public static Label CreateAndAddLabelRow2(this TableLayout container, String text)
        {
            var lbl = new Label { Text = text, Wrap = WrapMode.Word };
            lbl.Font = new Font(SystemFont.Default, GetEditorFontSize());
            container.Rows.Add(new TableRow(lbl));
            return lbl;
        }

        public static Label CreateAndAddDescriptionRow(this DynamicLayout container, String text, bool forceLabel = false)
        {
            var label = new Label();
            if (Application.Instance.Platform.IsWinForms && !forceLabel)
            {
                var textarea = new TextArea
                {
                    Text = text,
                    ReadOnly = true,
                    Font = SystemFonts.Label(SystemFonts.Default().Size - 0.5f),
                    BackgroundColor = container.BackgroundColor,
                    TextAlignment = Eto.Forms.TextAlignment.Left,
                    Wrap = true
                };
                textarea.Style = "labeldescription";
                container.AddRow(new TableRow(textarea));
            }
            else
            {
                label = new Label { Text = text, Wrap = WrapMode.Word };
                label.Font = new Font(SystemFont.Default, GetEditorFontSize() - 2);
                label.TextColor = Color.FromArgb(SystemColors.ControlText.Rb, SystemColors.ControlText.Gb, SystemColors.ControlText.Bb, 180);
                container.AddRow(new TableRow(label));
            }
            container.CreateAndAddEmptySpace();
            container.CreateAndAddEmptySpace();
            return label;
        }

        public static Label CreateAndAddDescriptionRow(this TableLayout container, String text, bool forceLabel = false)
        {
            var label = new Label();
            if (Application.Instance.Platform.IsWinForms && !forceLabel)
            {
                var textarea = new TextArea
                {
                    Text = text,
                    ReadOnly = true,
                    Font = SystemFonts.Label(SystemFonts.Default().Size - 0.5f),
                    BackgroundColor = container.BackgroundColor,
                    TextAlignment = Eto.Forms.TextAlignment.Left,
                    Wrap = true
                };
                textarea.Style = "labeldescription";
                container.Rows.Add(new TableRow(textarea));
            }
            else
            {
                label = new Label { Text = text, Wrap = WrapMode.Word };
                label.Font = new Font(SystemFont.Default, GetEditorFontSize() - 2);
                label.TextColor = Color.FromArgb(SystemColors.ControlText.Rb, SystemColors.ControlText.Gb, SystemColors.ControlText.Bb, 180);
                container.Rows.Add(new TableRow(label));
            }
            return label;
        }

        public static TableRow CreateAndAddControlRow(this DynamicLayout container, Control control)
        {
            var tr = new TableRow(control);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;
        }

        public static TableRow CreateAndAddControlRow(this TableLayout container, Control control)
        {
            var tr = new TableRow(control);
            container.Rows.Add(tr);
            return tr;
        }

        public static TextBox CreateAndAddTextBoxRow(this DynamicLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval.ToString(numberformat), Style = "textbox-rightalign" };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (text.Contains("(") && text.Contains(")"))
            {
                var si = new SharedClasses.SystemsOfUnits.SI();
                string extractedunits = text.Split('(', ')')[1];
                var unittype = si.GetUnitType(extractedunits);
                if (unittype != Interfaces.Enums.UnitOfMeasure.none)
                {
                    var ctxmenu = new ContextMenu();
                    foreach (var item in si.GetUnitSet(unittype))
                    {
                        var mi = new ButtonMenuItem { Text = item };
                        mi.Click += (sender, e) => { edittext.Text = SharedClasses.SystemsOfUnits.Converter.Convert(item, extractedunits, edittext.Text.ParseExpressionToDouble()).ToString(); };
                        ctxmenu.Items.Add(mi);
                    }
                    edittext.KeyUp += (sender, e) =>
                    {
                        if (e.Key == Keys.Space)
                        {
                            edittext.Text = edittext.Text.Replace(" ", "");
                            ctxmenu.Show(edittext);
                        }
                    };
                }
            }

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (keypress != null) edittext.KeyUp += (sender, e) => { if (e.Key == Keys.Enter) keypress.Invoke(); };

            var tr = new TableRow(txt, null, edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextBox CreateAndAddTextBoxRow(this TableLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval.ToString(numberformat), Style = "textbox-rightalign" };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (text.Contains("(") && text.Contains(")"))
            {
                var si = new SharedClasses.SystemsOfUnits.SI();
                string extractedunits = text.Split('(', ')')[1];
                var unittype = si.GetUnitType(extractedunits);
                if (unittype != Interfaces.Enums.UnitOfMeasure.none)
                {
                    var ctxmenu = new ContextMenu();
                    foreach (var item in si.GetUnitSet(unittype))
                    {
                        var mi = new ButtonMenuItem { Text = item };
                        mi.Click += (sender, e) => { edittext.Text = SharedClasses.SystemsOfUnits.Converter.Convert(item, extractedunits, edittext.Text.ParseExpressionToDouble()).ToString(); };
                        ctxmenu.Items.Add(mi);
                    }
                    edittext.KeyUp += (sender, e) =>
                    {
                        if (e.Key == Keys.Space)
                        {
                            edittext.Text = edittext.Text.Replace(" ", "");
                            ctxmenu.Show(edittext);
                        }
                    };
                }
            }

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (keypress != null) edittext.KeyUp += (sender, e) => { if (e.Key == Keys.Enter) keypress.Invoke(); };

            var tr = new TableRow(txt, null, edittext);

            container.Rows.Add(tr);

            return edittext;

        }

        public static ColorPicker CreateAndAddColorPickerRow(this DynamicLayout container, String text, Color currval, Action<ColorPicker, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var editor = new ColorPicker { Value = currval };
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) editor.Width = (int)(sf * 140);

            if (command != null) editor.ValueChanged += (sender, e) => command.Invoke((ColorPicker)sender, e);

            var tr = new TableRow(txt, null, editor);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return editor;

        }

        public static NumericStepper CreateAndAddNumericEditorRow(this DynamicLayout container, String text, double currval, double minval, double maxval, int decimalplaces, Action<NumericStepper, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var editor = new NumericStepper { Value = currval, DecimalPlaces = decimalplaces, MinValue = minval, MaxValue = maxval };
            editor.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) editor.Width = (int)(sf * 140);

            if (command != null) editor.ValueChanged += (sender, e) => command.Invoke((NumericStepper)sender, e);

            var tr = new TableRow(txt, null, editor);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return editor;

        }

        public static NumericStepper CreateAndAddNumericEditorRow(this TableLayout container, String text, double currval, double minval, double maxval, int decimalplaces, Action<NumericStepper, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var editor = new NumericStepper { Value = currval, DecimalPlaces = decimalplaces, MinValue = minval, MaxValue = maxval };
            editor.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) editor.Width = (int)(sf * 140);

            if (command != null) editor.ValueChanged += (sender, e) => command.Invoke((NumericStepper)sender, e);

            var tr = new TableRow(txt, null, editor);

            container.Rows.Add(tr);

            return editor;

        }


        public static TextBox CreateAndAddTextBoxRow2(this DynamicLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval.ToString(numberformat), Style = "textbox-rightalign" };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextBox CreateAndAddTextBoxRow2(this TableLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval.ToString(numberformat), Style = "textbox-rightalign" };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, edittext);

            container.Rows.Add(tr);

            return edittext;

        }

        public static TextBox CreateAndAddDoubleTextBoxRow(this DynamicLayout container, String numberformat, String text, String currval1, Double currval2, Action<TextBox, EventArgs> command, Action<TextBox, EventArgs> command2)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var edittext = new TextBox { Text = currval1, Width = (int)(sf * 100) };
            var edittext2 = new TextBox { Text = currval2.ToString(numberformat), Width = (int)(sf * 100) };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            edittext2.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (command2 != null) edittext2.TextChanged += (sender, e) => command2.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, null, edittext, edittext2);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext2;

        }

        public static TextBox CreateAndAddDoubleTextBoxRow(this TableLayout container, String numberformat, String text, String currval1, Double currval2, Action<TextBox, EventArgs> command, Action<TextBox, EventArgs> command2)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var edittext = new TextBox { Text = currval1, Width = (int)(sf * 100) };
            var edittext2 = new TextBox { Text = currval2.ToString(numberformat), Width = (int)(sf * 100) };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            edittext2.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (command2 != null) edittext2.TextChanged += (sender, e) => command2.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, null, edittext, edittext2);

            container.Rows.Add(tr);

            return edittext2;

        }

        public static TextArea CreateAndAddMultilineTextBoxRow(this DynamicLayout container, String text, bool ro, bool autosized, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);
            tr.ScaleHeight = autosized;
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextArea CreateAndAddMultilineTextBoxRow(this TableLayout container, String text, bool ro, bool autosized, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);
            tr.ScaleHeight = autosized;
            container.Rows.Add(tr);

            return edittext;

        }

        public static TextArea CreateAndAddMultilineMonoSpaceTextBoxRow(this DynamicLayout container, String text, int height, bool ro, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro, Height = height };

            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                edittext.Font = new Font("Menlo", GlobalSettings.Settings.ResultsReportFontSize);
            }
            else
            {
                edittext.Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize);
            }

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextArea CreateAndAddMultilineMonoSpaceTextBoxRow(this TableLayout container, String text, int height, bool ro, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro, Height = height };

            if (GlobalSettings.Settings.RunningPlatform() == GlobalSettings.Settings.Platform.Mac)
            {
                edittext.Font = new Font("Menlo", GlobalSettings.Settings.ResultsReportFontSize);
            }
            else
            {
                edittext.Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize);
            }

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);

            container.Rows.Add(tr);

            return edittext;

        }

        public static ListBox CreateAndAddListBoxRow(this DynamicLayout container, int height, String[] listitems, Action<ListBox, EventArgs> command)
        {

            var lbox = new ListBox { Height = height };
            lbox.Font = new Font(SystemFont.Default, GetEditorFontSize());

            foreach (var item in listitems)
            {
                lbox.Items.Add(item);
            }

            if (command != null) lbox.SelectedIndexChanged += (sender, e) => command.Invoke((ListBox)sender, e);

            var tr = new TableRow(lbox);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return lbox;

        }

        public static ListBox CreateAndAddListBoxRow(this  TableLayout container, int height, String[] listitems, Action<ListBox, EventArgs> command)
        {

            var lbox = new ListBox { Height = height };
            lbox.Font = new Font(SystemFont.Default, GetEditorFontSize());

            foreach (var item in listitems)
            {
                lbox.Items.Add(item);
            }

            if (command != null) lbox.SelectedIndexChanged += (sender, e) => command.Invoke((ListBox)sender, e);

            var tr = new TableRow(lbox);

            container.Rows.Add(tr);

            return lbox;

        }

        public static TextBox CreateAndAddStringEditorRow(this DynamicLayout container, String text, String currval, Action<TextBox, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval, Width = (int)(sf * 140) };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (keypress != null) edittext.KeyUp += (sender, e) => { if (e.Key == Keys.Enter) keypress.Invoke(); };

            var tr = new TableRow(txt, null, edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextBox CreateAndAddStringEditorRow(this TableLayout container, String text, String currval, Action<TextBox, EventArgs> command, Action keypress = null)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval, Width = (int)(sf * 140) };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) edittext.Width = (int)(sf * 140);

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (keypress != null) edittext.KeyUp += (sender, e) => { if (e.Key == Keys.Enter) keypress.Invoke(); };

            var tr = new TableRow(txt, null, edittext);

            container.Rows.Add(tr);

            return edittext;

        }

        public static Label CreateAndAddTwoLabelsRow(this DynamicLayout container, String text1, String text2)
        {

            var txt = new Label { Text = text1, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var txt2 = new Label { Text = text2, Width = (int)(sf * 140), VerticalAlignment = VerticalAlignment.Center };
            txt2.Font = new Font(SystemFont.Default, GetEditorFontSize());

            var tr = new TableRow(txt, null, txt2);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return txt2;

        }

        public static Label CreateAndAddThreeLabelsRow(this DynamicLayout container, String text1, String text2, String text3)
        {

            var txt = new Label { Text = text1, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var txt2 = new Label { Text = text2, Width = (int)(sf * 140), VerticalAlignment = VerticalAlignment.Center, TextAlignment = TextAlignment.Right };
            txt2.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var txt3 = new Label { Text = text3, Width = (int)(sf * 140), VerticalAlignment = VerticalAlignment.Center };
            txt3.Font = new Font(SystemFont.Default, GetEditorFontSize());

            var tr = new TableRow(txt, null, txt2, new Label {Text = " " }, txt3);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return txt2;

        }

        public static Label CreateAndAddTwoLabelsRow2(this DynamicLayout container, String text1, String text2)
        {

            var txt = new Label { Text = text1, VerticalAlignment = VerticalAlignment.Center, Font = SystemFonts.Bold(null, FontDecoration.None) };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var txt2 = new Label { Text = text2, Width = (int)(sf * 350), VerticalAlignment = VerticalAlignment.Center };
            txt2.Font = new Font(SystemFont.Default, GetEditorFontSize());

            var tr = new TableRow(txt, null, txt2);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return txt2;

        }

        public static TextBox CreateAndAddStringEditorRow2(this DynamicLayout container, String text, String placeholder, String currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var edittext = new TextBox { Text = currval, PlaceholderText = placeholder };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static void CreateAndAddEmptySpace(this DynamicLayout container)
        {

            var height = GlobalSettings.Settings.CrossPlatformUIItemSpacing;

            var h = height * GetEditorFontSize() / (int)(new Eto.Drawing.Font(Eto.Drawing.SystemFont.Label).Size);

            container.AddRow(new TableRow(new Label { Text = "", Height = (int)(sf * h) }));
        }

        public static TextBox CreateAndAddFullTextBoxRow(this DynamicLayout container, String text, Action<TextBox, EventArgs> command)
        {

            var edittext = new TextBox { Text = text };
            edittext.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static Button CreateAndAddLabelAndButtonRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Text = buttonlabel };
            btn.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 140);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(txt, null, btn);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return btn;


        }

        public static Button CreateAndAddLabelAndButtonRow(this TableLayout container, String label, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Text = buttonlabel };
            btn.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 140);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(txt, null, btn);

            container.Rows.Add(tr);

            return btn;


        }

        public static void CreateAndAddLabelAndControlRow(this DynamicLayout container, String label, Control control)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) control.Width = (int)(sf * 140);

            var tr = new TableRow(txt, null, control);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

        }

        public static Button CreateAndAddBoldLabelAndButtonRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center, Font = SystemFonts.Bold() };
            txt.Font = new Font(SystemFont.Bold, GetEditorFontSize());
            var btn = new Button { Text = buttonlabel };
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 140);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(txt, null, btn);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return btn;


        }

        public static Label GetPlaceHolderLabel()
        {
            return new Label { Text = " " };
        }

        public static TextBox CreateAndAddLabelAndTextBoxAndButtonRow(this DynamicLayout container, String label, String textboxvalue, String buttonlabel, String imageResID, Action<TextBox, EventArgs> txteditcommand, Action<Button, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var tbox = new TextBox { Text = textboxvalue };
            tbox.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Width = 80, Text = buttonlabel };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (txteditcommand != null) tbox.TextChanged += (sender, e) => txteditcommand.Invoke((TextBox)sender, e);
            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), tbox, GetPlaceHolderLabel(), btn);

            tr.Cells[2].ScaleWidth = true;

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return tbox;


        }

        public static TableRow CreateAndAddTextBoxAndTwoButtonsRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, String buttonlabel2, String imageResID2, Action<TextBox, EventArgs> command0, Action<Button, EventArgs> command, Action<Button, EventArgs> command2)
        {

            var txt = new TextBox { Width = (int)(sf * 250), Text = label };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Width = (int)(sf * 100), Text = buttonlabel };
            var btn2 = new Button { Width = (int)(sf * 100), Text = buttonlabel2 };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command0 != null) txt.TextChanged += (sender, e) => command0.Invoke((TextBox)sender, e);
            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), null, btn, GetPlaceHolderLabel(), btn2);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static TableRow CreateAndAddTextBoxAndThreeButtonsRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, String buttonlabel2, String imageResID2, String buttonlabel3, String imageResID3, Action<TextBox, EventArgs> command0, Action<Button, EventArgs> command, Action<Button, EventArgs> command2, Action<Button, EventArgs> command3)
        {

            var txt = new TextBox { Width = (int)(sf * 300), Text = label };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Width = (int)(sf * 100), Text = buttonlabel };
            var btn2 = new Button { Width = (int)(sf * 100), Text = buttonlabel2 };
            var btn3 = new Button { Width = (int)(sf * 100), Text = buttonlabel3 };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);
            if (imageResID2 != null) btn2.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);
            if (imageResID3 != null) btn3.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID3), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command0 != null) txt.TextChanged += (sender, e) => command0.Invoke((TextBox)sender, e);
            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);
            if (command3 != null) btn3.Click += (sender, e) => command3.Invoke((Button)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), null, btn, GetPlaceHolderLabel(), btn2, GetPlaceHolderLabel(), btn3);
            if (Application.Instance.Platform.IsMac)
            {
                txt.Height = (int)(sf * 28);
            }
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static TableRow CreateAndAddTwoButtonsRow(this DynamicLayout container, String buttonlabel, String imageResID, String buttonlabel2, String imageResID2, Action<Button, EventArgs> command, Action<Button, EventArgs> command2)
        {

            var btn = new Button { Width = (int)(sf * 100), Text = buttonlabel };
            var btn2 = new Button { Width = (int)(sf * 100), Text = buttonlabel2 };
            btn.Font = new Font(SystemFont.Default, GetEditorFontSize());
            btn2.Font = new Font(SystemFont.Default, GetEditorFontSize());

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);

            var tr = new TableRow(null, btn, GetPlaceHolderLabel(), btn2);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static TableRow CreateAndAddLabelAndTwoButtonsRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, String buttonlabel2, String imageResID2, Action<Button, EventArgs> command, Action<Button, EventArgs> command2)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            txt.Font = new Font(SystemFont.Default, GetEditorFontSize());
            var btn = new Button { Text = buttonlabel };
            var btn2 = new Button { Text = buttonlabel2 };

            btn.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 100);

            btn2.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn2.Width = (int)(sf * 100);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);

            var tr = new TableRow(txt, null, btn, GetPlaceHolderLabel(), btn2);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static Button CreateAndAddButtonRow(this DynamicLayout container, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var btn = new Button { Text = buttonlabel };
            btn.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 140);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(btn);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return btn;


        }

        public static Button CreateAndAddButtonRow(this TableLayout container, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var btn = new Button { Text = buttonlabel };
            btn.Font = new Font(SystemFont.Default, GetEditorFontSize());
            if (GlobalSettings.Settings.EditorTextBoxFixedSize) btn.Width = (int)(sf * 140);

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), (int)(sf * 22), (int)(sf * 22), ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(btn);

            container.Rows.Add(tr);

            return btn;
            
        }

        public static CheckBox CreateAndAddCheckBoxRow(this DynamicLayout container, String text, bool value, Action<CheckBox, EventArgs> command, Action keypress = null)
        {

            var check = new CheckBox { Text = text, Checked = value };
            check.Font = new Font(SystemFont.Bold, GetEditorFontSize());

            if (command != null) check.CheckedChanged += (sender, e) => command.Invoke((CheckBox)sender, e);
            if (keypress != null) check.CheckedChanged += (sender, e) => keypress.Invoke();

            container.AddRow(new TableRow(check));
            container.CreateAndAddEmptySpace();
            container.CreateAndAddEmptySpace();

            return check;
        }

        public static CheckBox CreateAndAddCheckBoxRow(this TableLayout container, String text, bool value, Action<CheckBox, EventArgs> command, Action keypress = null)
        {

            var check = new CheckBox { Text = text, Checked = value };
            check.Font = new Font(SystemFont.Bold, GetEditorFontSize());

            if (command != null) check.CheckedChanged += (sender, e) => command.Invoke((CheckBox)sender, e);
            if (keypress != null) check.CheckedChanged += (sender, e) => keypress.Invoke();

            container.Rows.Add(new TableRow(check));

            return check;
        }

        public static OxyPlot.PlotModel CreatePlotModel(double[] x, double[] y, string title, string subtitle, string xtitle, string ytitle)
        {

            var model = new OxyPlot.PlotModel() { Subtitle = subtitle, Title = title };
            model.Background = OxyPlot.OxyColors.White;
            model.TitleFontSize = 14;
            model.SubtitleFontSize = 12;
            model.Axes.Add(new OxyPlot.Axes.LinearAxis()
            {
                MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                Position = OxyPlot.Axes.AxisPosition.Bottom,
                FontSize = 12,
                Title = xtitle,
                Key = "x",
            });
            model.Axes.Add(new OxyPlot.Axes.LinearAxis()
            {
                MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                Position = OxyPlot.Axes.AxisPosition.Left,
                FontSize = 12,
                Title = ytitle
            });
            model.LegendFontSize = 11;
            model.LegendPlacement = OxyPlot.LegendPlacement.Outside;
            model.LegendOrientation = OxyPlot.LegendOrientation.Vertical;
            model.LegendPosition = OxyPlot.LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = OxyPlot.TitleHorizontalAlignment.CenteredWithinView;
            model.AddLineSeries(x, y, ytitle);

            return model;

        }

        public static OxyPlot.PlotModel CreatePlotModel(double[] x, double[] y1, double[] y2, string title, string subtitle, string xtitle, string ytitle, string ytitle1, string ytitle2)
        {

            var model = new OxyPlot.PlotModel() { Subtitle = subtitle, Title = title };
            model.Background = OxyPlot.OxyColors.White;
            model.TitleFontSize = 14;
            model.SubtitleFontSize = 12;
            model.Axes.Add(new OxyPlot.Axes.LinearAxis()
            {
                MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                Position = OxyPlot.Axes.AxisPosition.Bottom,
                FontSize = 12,
                Title = xtitle
            });
            model.Axes.Add(new OxyPlot.Axes.LinearAxis()
            {
                MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                Position = OxyPlot.Axes.AxisPosition.Left,
                FontSize = 12,
                Title = ytitle
            });
            model.LegendFontSize = 11;
            model.LegendPlacement = OxyPlot.LegendPlacement.Outside;
            model.LegendOrientation = OxyPlot.LegendOrientation.Vertical;
            model.LegendPosition = OxyPlot.LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = OxyPlot.TitleHorizontalAlignment.CenteredWithinView;
            model.AddLineSeries(x, y1, ytitle1);
            model.AddLineSeries(x, y2, ytitle2);

            return model;

        }

    }
}
