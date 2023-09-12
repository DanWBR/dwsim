using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using DWSIM.UI.Shared;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.ExtensionMethods.Eto
{
    /// <summary>
    /// Eto.Forms extension methods
    /// </summary>
    public static class Extensions2
    {

        /// <summary>
        /// Gets a standard form window to display 
        /// </summary>
        /// <param name="title">Title of the form</param>
        /// <param name="width">Width in pixels</param>
        /// <param name="height">Height in pixels</param>
        /// <param name="content">Form contents (DynamicLayout)</param>
        /// <returns></returns>
        public static Form GetStandardForm(string title, int width, int height, DynamicLayout content)
        {
            return Common.GetDefaultEditorForm(title, width, height, content);
        }

        /// <summary>
        /// Gets a standard form window to display with scrollable contents
        /// </summary>
        /// <param name="title">Title of the form</param>
        /// <param name="width">Width in pixels</param>
        /// <param name="height">Height in pixels</param>
        /// <param name="content">Form contents (DynamicLayout)</param>
        /// <param name="scrollable">Contents are scrollable</param>
        /// <returns></returns>
        public static Form GetStandardForm(string title, int width, int height, TableLayout content, bool scrollable)
        {
            return Common.GetDefaultEditorForm(title, width, height, content, scrollable);
        }

        /// <summary>
        /// Gets a Form window for multiple tabbed contents
        /// </summary>
        /// <param name="title">Title of the form</param>
        /// <param name="width">Width in pixels</param>
        /// <param name="height">Height in pixels</param>
        /// <param name="contents">List of controls to display in the tabs. Set the Tag property of the control to display as the tab text.</param>
        /// <returns></returns>
        public static Form GetTabbedForm(string title, int width, int height, Control[] contents)
        {
            return Common.GetDefaultTabbedForm(title, width, height, contents);
        }

        /// <summary>
        /// Centers the form on the screen.
        /// </summary>
        /// <param name="form">Form (window)</param>
        public static void Center(this Form form)
        {
            Common.Center(form);
        }

        /// <summary>
        /// Gets the standard control container (DynamicLayout)
        /// </summary>
        /// <returns></returns>
        public static DynamicLayout GetStandardContainer()
        {
            return Common.GetDefaultContainer();
        }

        /// <summary>
        /// Creates and returns a Dialog window.
        /// </summary>
        /// <param name="content">Control to display in the dialog.</param>
        /// <param name="title">Title of the dialog.</param>
        /// <param name="width">Width in pixels</param>
        /// <param name="height">Height in pixels</param>
        /// <returns></returns>
        public static Dialog CreateDialog(Control content, String title, int width = 0, int height = 0)
        {
            return Common.CreateDialog(content, title, width, height);
        }

        /// <summary>
        /// Adds a header text to the container.
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Label text</param>
        /// <returns></returns>
        public static Label AddHeader(DynamicLayout container, string text)
        {
            return Common.CreateAndAddLabelRow(container, text);
        }

        /// <summary>
        /// Adds a description row to the container.
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Description text</param>
        /// <returns></returns>
        public static Label AddDescription(DynamicLayout container, string text)
        {
            return Common.CreateAndAddDescriptionRow(container, text);
        }

        /// <summary>
        /// Adds a textbox for editing a double (numeric) value.
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Text (description)</param>
        /// <param name="currentvalue">Current value to display on the textbox</param>
        /// <param name="numberformat">Number format (i.e. 'N2')</param>
        /// <param name="textchangedhandler">Handler for the TextChanged event.</param>
        /// <returns></returns>
        public static TextBox AddEditTextBox(DynamicLayout container, string text, double currentvalue,
            string numberformat = "", Action<TextBox, EventArgs> textchangedhandler = null)
        {
            if (numberformat == "") numberformat = "N2";
            return Common.CreateAndAddTextBoxRow(container, numberformat, text, currentvalue, textchangedhandler);
        }

        /// <summary>
        /// Adds a DropDown with selectable items
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Text (description)</param>
        /// <param name="options">Selectable items</param>
        /// <param name="selected">Index of the currently selected item (zero-based)</param>
        /// <param name="selectedindexchangedhandler">Handler for the SelectedIndexChanged event.</param>
        /// <returns></returns>
        public static DropDown AddDropDown(DynamicLayout container, string text, List<string> options, int selected,
            Action<DropDown, EventArgs> selectedindexchangedhandler = null)
        {
            return Common.CreateAndAddDropDownRow(container, text, options, selected, selectedindexchangedhandler);
        }

        /// <summary>
        /// Adds a CheckBox
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Text (description)</param>
        /// <param name="ischecked"></param>
        /// <param name="checkedchangedhandler">Handler for the CheckedChanged event.</param>
        /// <returns></returns>
        public static CheckBox AddCheckBox(DynamicLayout container, string text, bool ischecked,
            Action<CheckBox, EventArgs> checkedchangedhandler = null)
        {
            return Common.CreateAndAddCheckBoxRow(container, text, ischecked, checkedchangedhandler);
        }

        /// <summary>
        /// Adds a numeric stepper (selector)
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Text (description)</param>
        /// <param name="value">The current value of the stepper</param>
        /// <param name="minvalue">Maximum selectable value</param>
        /// <param name="maxvalue">Minimum selectable value</param>
        /// <param name="decimalplaces">Decimal places</param>
        /// <param name="valuechangedhandler">Handler for the ValueChanged event</param>
        /// <returns></returns>
        public static TextBox AddNumericStepper(DynamicLayout container, string text, double value,
            double minvalue, double maxvalue, int decimalplaces, Action<TextBox, EventArgs> valuechangedhandler = null)
        {
            return Common.CreateAndAddNumericEditorRow2(container, text, value, minvalue, maxvalue, decimalplaces, valuechangedhandler);
        }

        /// <summary>
        /// Adds a button with a label.
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="text">Text (description)</param>
        /// <param name="buttontext">Text to be displayed in the button</param>
        /// <param name="clickhandler">Handler for the Click event.</param>
        /// <returns></returns>
        public static Button AddButtonWithLabel(DynamicLayout container, string text, string buttontext, Action<Button, EventArgs> clickhandler = null)
        {
            return Common.CreateAndAddLabelAndButtonRow(container, text, buttontext, null, clickhandler);
        }

        /// <summary>
        /// Adds a button
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="buttontext">Text to be displayed in the button</param>
        /// <param name="clickhandler">Handler for the Click event.</param>
        /// <returns></returns>
        public static Button AddButton(DynamicLayout container, string buttontext, Action<Button, EventArgs> clickhandler = null)
        {
            return Common.CreateAndAddButtonRow(container, buttontext, null, clickhandler);
        }

        /// <summary>
        /// Adds a control to the container.
        /// </summary>
        /// <param name="container">Layout container</param>
        /// <param name="control">Control to add</param>
        public static void AddControl(DynamicLayout container, Control control)
        {
            Common.CreateAndAddControlRow(container, control);
        }

        /// <summary>
        /// Adds an empty row to the container for spacing purposes.
        /// </summary>
        /// <param name="container">Layout container</param>
        public static void AddEmptySpace(DynamicLayout container)
        {
            Common.CreateAndAddEmptySpace(container);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="control"></param>
        /// <returns></returns>
        public static IEnumerable<Control> GetAllChildren(Control control)
        {
            var controls = control.VisualControls;
            if (control is Panel) controls = ((Panel)control).Controls;
            if (control is DocumentControl) controls = ((DocumentControl)control).Pages;
            return controls.SelectMany(ctrl => GetAllChildren(ctrl)).Concat(controls);
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="form"></param>
        public static void SetFontAndPadding(this Form form)
        {

            var sysfont = System.Drawing.SystemFonts.MessageBoxFont;
            var regularfont = new Font(sysfont.FontFamily.Name, sysfont.SizeInPoints);
            var boldfont = new Font(sysfont.FontFamily.Name, sysfont.SizeInPoints, FontStyle.Bold);

            var allcontrols = GetAllChildren(form);
            foreach (var control in allcontrols)
            {
                if (control is CommonControl)
                {
                    var font = ((CommonControl)control).Font;
                    if (font.Bold)
                    {
                        ((CommonControl)control).Font = boldfont;
                    }
                    else
                    {
                        ((CommonControl)control).Font = regularfont;
                    }
                }
                else if (control is TableLayout)
                {
                    ((TableLayout)control).Padding = new Padding((int)(2 * GlobalSettings.Settings.DpiScale));
                }
                if (control is TextBox)
                {
                    var tb = (TextBox)control;
                    var d = 0.0;
                    if (Double.TryParse(tb.Text, out d))
                    {
                        tb.TextAlignment = TextAlignment.Right;
                    }
                }
            }

        }

    }
}
