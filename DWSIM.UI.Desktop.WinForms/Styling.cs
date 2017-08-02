using System.Reflection;
using System.Windows.Forms;
using System.Drawing;

namespace DWSIM.UI.Desktop.WinForms
{

    public static class StyleSetter
    {

        public static void SetStyles()
        {

            Eto.Style.Add<Eto.Forms.TextArea>("labeldescription", label =>
            {
                var wflabel = (RichTextBox)label.ControlObject;
                wflabel.BackColor = SystemColors.Control;
                wflabel.ScrollBars = RichTextBoxScrollBars.None;
                wflabel.BorderStyle = BorderStyle.None;
                var wftl = (TableLayoutPanel)((Eto.WinForms.Forms.Controls.TextAreaHandler)label.Handler).ContainerControl;
                wftl.BackColor = SystemColors.Control;
                wftl.BorderStyle = BorderStyle.None;
                wftl.Dock = DockStyle.Fill;
            });

            Eto.Style.Add<Eto.Forms.Button>("main", button =>
            {
                var wfbutton = (Button)button.ControlObject;
            });

            Eto.Style.Add<Eto.Forms.Panel>("transparent-form", control =>
            {
                var wfwnd = (System.Windows.Forms.Form)control.ControlObject;
            });

            Eto.Style.Add<Eto.Forms.TextBox>("textbox-rightalign", control =>
            {
                var tbox = (System.Windows.Forms.TextBox)control.ControlObject;
                tbox.TextAlign = HorizontalAlignment.Right;
            });

            Eto.Style.Add<Eto.Forms.GridView>("spreadsheet", control =>
            {
                var grid = (System.Windows.Forms.DataGridView)control.ControlObject;
                grid.SelectionMode =  DataGridViewSelectionMode.CellSelect;
                grid.ColumnHeadersDefaultCellStyle.Alignment = DataGridViewContentAlignment.MiddleCenter;
                grid.RowHeadersVisible = true;
                grid.RowHeadersWidth = 50;
                foreach (DataGridViewRow row in grid.Rows)
                {
                    row.HeaderCell.Value = (row.Index + 1).ToString();
                };
            });
            
        }
    }
}
