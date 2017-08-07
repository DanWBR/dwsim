using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Eto.Drawing;
using Eto.Forms;

using DWSIM.ExtensionMethods;

namespace DWSIM.UI.Shared
{
    public static class Common
    {

        static string imgprefix = "DWSIM.UI.Desktop.Shared.Resources.Icons.";
            
        public static bool IsValidDouble(string s)
        {
            double d = 0;
            return double.TryParse(s, out d);
        }

        public static Form GetDefaultEditorForm(string title, int width, int height, DynamicLayout content)
        {
            content.CreateAndAddEmptySpace();
            content.CreateAndAddEmptySpace();
            content.CreateAndAddEmptySpace();
            content.CreateAndAddEmptySpace();
            content.CreateAndAddEmptySpace();
            content.EndVertical();
            content.Width = width - content.Padding.Value.Left * 2 - content.Padding.Value.Right * 2;
            height += 10;
            return new Form()
            {
                Icon = Eto.Drawing.Icon.FromResource(imgprefix + "DWSIM_ico.ico"),
                Content = new Scrollable { Content = content, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true },
                Title = title,
                ClientSize = new Size(width, height)
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
                ClientSize = new Size(width, height),
                ShowInTaskbar = true
            };
            if (scrollable)
            {
                form.Content = new Scrollable { Content = content, Border = BorderType.None, ExpandContentWidth = true, ExpandContentHeight = true };
            }else{
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
                ClientSize = new Size(width, height),
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
                ClientSize = new Size(width, height),
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
            if (height != 0) alert.Height = height;
            if (width != 0) alert.Width = width;
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

        public static DropDown CreateAndAddDropDownRow(this DynamicLayout container, String text, List<String> options, int position, Action<DropDown, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var drop = new DropDown { Width = 200 };

            foreach (var item in options)
            {
                drop.Items.Add(item);
            }

            drop.SelectedIndex = position;

            if (command != null) drop.SelectedIndexChanged += (sender, e) => command.Invoke((DropDown)sender, e);

            var tr = new TableRow(txt, null, drop);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
        
            return drop;

        }

        public static Label CreateAndAddLabelRow(this DynamicLayout container, String text)
        {
            var label = new Label { Text = text, Font = SystemFonts.Bold(null, FontDecoration.None), Wrap = WrapMode.Word };
            container.AddRow(new TableRow(label));
            container.CreateAndAddEmptySpace();
            return label;
        }

        public static Label CreateAndAddLabelRow2(this DynamicLayout container, String text)
        {
            var lbl = new Label { Text = text, Wrap = WrapMode.Word };
            container.AddRow(new TableRow(lbl));
            container.CreateAndAddEmptySpace();
            return lbl;
        }

        public static void CreateAndAddDescriptionRow(this DynamicLayout container, String text, bool forceLabel = false)
        {
            if (Application.Instance.Platform.IsWinForms && !forceLabel)
            {
                var textarea = new TextArea { Text = text, ReadOnly = true, 
                    Font = SystemFonts.Label(SystemFonts.Default().Size - 0.5f), 
                    BackgroundColor = container.BackgroundColor,
                    TextAlignment = Eto.Forms.TextAlignment.Left, Wrap = true };
                textarea.Style = "labeldescription";
                container.AddRow(new TableRow(textarea));
            }
            else {
                var label = new Label { Text = text, Wrap = WrapMode.Word, Font = SystemFonts.Label(SystemFonts.Default().Size - 2.0f) };
                container.AddRow(new TableRow(label));
            }
            container.CreateAndAddEmptySpace();
        }

        public static TableRow CreateAndAddControlRow(this DynamicLayout container, Control control)
        {
            var tr = new TableRow(control);
            container.AddRow(tr);
            return tr;
        }

        public static TextBox CreateAndAddTextBoxRow(this DynamicLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval.ToString(numberformat), Width = 200, Style = "textbox-rightalign" };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, null, edittext);
            
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextBox CreateAndAddTextBoxRow2(this DynamicLayout container, String numberformat, String text, Double currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval.ToString(numberformat), Width = 200, Style = "textbox-rightalign" };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextBox CreateAndAddDoubleTextBoxRow(this DynamicLayout container, String numberformat, String text, String currval1, Double currval2, Action<TextBox, EventArgs> command, Action<TextBox, EventArgs> command2)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval1, Width = 100 };
            var edittext2 = new TextBox { Text = currval2.ToString(numberformat), Width = 100 };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);
            if (command2 != null) edittext2.TextChanged += (sender, e) => command2.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, null, edittext, edittext2);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }
        public static TextArea CreateAndAddMultilineTextBoxRow(this DynamicLayout container, String text, bool ro, bool autosized, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro};

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);
            tr.ScaleHeight = autosized;
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static TextArea CreateAndAddMultilineMonoSpaceTextBoxRow(this DynamicLayout container, String text, int height, bool ro, Action<TextArea, EventArgs> command)
        {

            var edittext = new TextArea { Text = text, ReadOnly = ro, Height = height };

            edittext.Font = Fonts.Monospace(GlobalSettings.Settings.ResultsReportFontSize);
            
            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextArea)sender, e);

            var tr = new TableRow(edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static ListBox CreateAndAddListBoxRow(this DynamicLayout container, int height, String[] listitems, Action<ListBox, EventArgs> command)
        {

            var lbox = new ListBox { Height = height };

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

        public static TextBox CreateAndAddStringEditorRow(this DynamicLayout container, String text, String currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval, Width = 200 };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, null, edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static Label CreateAndAddTwoLabelsRow(this DynamicLayout container, String text1, String text2)
        {

            var txt = new Label { Text = text1, VerticalAlignment = VerticalAlignment.Center };
            var txt2 = new Label { Text = text2, Width = 200, VerticalAlignment = VerticalAlignment.Center };

            var tr = new TableRow(txt, null, txt2);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return txt2;

        }

        public static TextBox CreateAndAddStringEditorRow2(this DynamicLayout container, String text, String placeholder, String currval, Action<TextBox, EventArgs> command)
        {

            var txt = new Label { Text = text, VerticalAlignment = VerticalAlignment.Center };
            var edittext = new TextBox { Text = currval, PlaceholderText = placeholder};

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static void CreateAndAddEmptySpace(this DynamicLayout container)
        {
            container.AddRow(new TableRow(new Label { Text = "", Height = 10 }));
        }

        public static TextBox CreateAndAddFullTextBoxRow(this DynamicLayout container, String text, Action<TextBox, EventArgs> command)
        {

            var edittext = new TextBox { Text = text };

            if (command != null) edittext.TextChanged += (sender, e) => command.Invoke((TextBox)sender, e);

            var tr = new TableRow(edittext);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();

            return edittext;

        }

        public static Button CreateAndAddLabelAndButtonRow(this DynamicLayout container, String label, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var txt = new Label { Text = label, VerticalAlignment = VerticalAlignment.Center };
            var btn = new Button { Width = 200, Text = buttonlabel };

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
            var tbox = new TextBox { Text = textboxvalue };
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

            var txt = new TextBox { Width = 250, Text = label };
            var btn = new Button { Width = 100, Text = buttonlabel };
            var btn2 = new Button { Width = 100, Text = buttonlabel2 };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), 22, 22, ImageInterpolation.Default);

            if (command0 != null) txt.TextChanged += (sender, e) => command0.Invoke((TextBox)sender, e);
            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);

            var tr = new TableRow(txt, GetPlaceHolderLabel(), null, btn, GetPlaceHolderLabel(), btn2);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static TableRow CreateAndAddTwoButtonsRow(this DynamicLayout container, String buttonlabel, String imageResID, String buttonlabel2, String imageResID2, Action<Button, EventArgs> command, Action<Button, EventArgs> command2)
        {

            var btn = new Button { Width = 100, Text = buttonlabel };
            var btn2 = new Button { Width = 100, Text = buttonlabel2 };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), 22, 22, ImageInterpolation.Default);

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
            var btn = new Button { Width = 100, Text = buttonlabel };
            var btn2 = new Button { Width = 100, Text = buttonlabel2 };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);
            if (imageResID2 != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID2), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);
            if (command2 != null) btn2.Click += (sender, e) => command2.Invoke((Button)sender, e);

            var tr = new TableRow(txt, null, btn, GetPlaceHolderLabel(), btn2);
            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
            return tr;

        }

        public static Button CreateAndAddButtonRow(this DynamicLayout container, String buttonlabel, String imageResID, Action<Button, EventArgs> command)
        {

            var btn = new Button { Width = 200, Text = buttonlabel };

            if (imageResID != null) btn.Image = new Bitmap(Eto.Drawing.Bitmap.FromResource(imageResID), 22, 22, ImageInterpolation.Default);

            if (command != null) btn.Click += (sender, e) => command.Invoke((Button)sender, e);

            var tr = new TableRow(btn);

            container.AddRow(tr);
            container.CreateAndAddEmptySpace();
        
            return btn;


        }

        public static CheckBox CreateAndAddCheckBoxRow(this DynamicLayout container, String text, bool value, Action<CheckBox, EventArgs> command)
        {

            var check = new CheckBox { Text = text, Checked = value };

            if (command != null) check.CheckedChanged += (sender, e) => command.Invoke((CheckBox)sender, e);

            container.AddRow(new TableRow(check));
            container.CreateAndAddEmptySpace();
        
            return check;
        }

        public static OxyPlot.PlotModel CreatePlotModel(double[] x, double[] y, string title, string subtitle, string xtitle, string ytitle)
        {

            var model = new OxyPlot.PlotModel() { Subtitle = subtitle, Title = title };
            model.TitleFontSize = 18;
            model.SubtitleFontSize = 16;
            model.Axes.Add(new OxyPlot.Axes.LinearAxis()
            {
                MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                Position = OxyPlot.Axes.AxisPosition.Bottom,
                FontSize = 16,
                Title = xtitle
            });
            if (Math.Abs(y[0] - 1.0f) < 0.0001)
            {
                model.Axes.Add(new OxyPlot.Axes.LinearAxis()
                {
                    MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                    MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                    Position = OxyPlot.Axes.AxisPosition.Left,
                    FontSize = 16,
                    Title = ytitle,
                    StartPosition = 1,
                    EndPosition = 0,
                    MajorStep = 1.0f,
                    MinorStep = 0.5f
                });
            }
            else
            {
                model.Axes.Add(new OxyPlot.Axes.LinearAxis()
                {
                    MajorGridlineStyle = OxyPlot.LineStyle.Dash,
                    MinorGridlineStyle = OxyPlot.LineStyle.Dot,
                    Position = OxyPlot.Axes.AxisPosition.Left,
                    FontSize = 16,
                    Title = ytitle
                });
            }
            model.LegendFontSize = 16;
            model.LegendPlacement = OxyPlot.LegendPlacement.Outside;
            model.LegendOrientation = OxyPlot.LegendOrientation.Vertical;
            model.LegendPosition = OxyPlot.LegendPosition.BottomCenter;
            model.TitleHorizontalAlignment = OxyPlot.TitleHorizontalAlignment.CenteredWithinView;
            model.AddLineSeries(x, y);

            return model;

        }

    }
}
