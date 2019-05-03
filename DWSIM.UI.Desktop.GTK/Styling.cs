using System;
using System.Collections.Generic;
using System.Windows;
using Eto.Drawing;
using Eto.GtkSharp.Forms;
using Eto.GtkSharp.Forms.Controls;
using Gtk;

namespace DWSIM.UI.Desktop.GTK
{

    public static class StyleSetter
    {

        public static void SetStyles()
        {

            Eto.Style.Add<Eto.Forms.Label>("splashlabels1", label =>
            {
                label.BackgroundColor = Color.FromArgb(232, 232, 232);
                label.TextAlignment = Eto.Forms.TextAlignment.Left;
            });

            Eto.Style.Add<Eto.Forms.Label>("splashlabels2", label =>
            {
                label.BackgroundColor = new Color(0.051f, 0.447f, 0.651f);
            });

            Eto.Style.Add<LabelHandler>("fixedwidth", label =>
            {
                label.Control.MaxWidthChars = 100;
                label.BackgroundColor = Color.FromArgb(232, 232, 232);
            });

            Eto.Style.Add<Eto.Forms.Panel>("transparent-form", control =>
            {
                var gtkwnd = (Gtk.Window)control.ControlObject;
                gtkwnd.BorderWidth = 0;
            });

            Eto.Style.Add<Eto.Forms.GridView>("spreadsheet", control =>
            {
                var gtkgrid = (Gtk.TreeView)((Gtk.ScrolledWindow)control.ControlObject).Child;
                gtkgrid.Columns[0].Resizable = false;
                gtkgrid.Columns[0].Clickable = false;
                foreach (var column in gtkgrid.Columns)
                {
                    if (column.Widget != null) ((Gtk.Label)column.Widget).SetAlignment(0.5f, 0.5f);
                }
            });

            Eto.Style.Add<Eto.Forms.TextBox>("textbox-rightalign", control =>
            {
                var tbox = (Gtk.Entry)control.ControlObject;
                tbox.Alignment = 1.0f;
            });


        }

    }

}
