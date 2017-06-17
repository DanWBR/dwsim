using System;
using System.Collections.Generic;
using System.Windows;
using Eto.Drawing;
using Eto.GtkSharp.Forms.Controls;

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

        }
        
    }

}
