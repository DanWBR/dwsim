using System;
using System.Collections.Generic;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace DWSIM.UI.Desktop.WPF
{

    public static class StyleSetter
    {

        public static void SetMainButtonStyle()
        {

            Eto.Style.Add<Eto.Forms.Button>("main", button =>
            {
                var wpfbutton = (Button)button.ControlObject;
                wpfbutton.BorderThickness = new Thickness(0.0);
                var img = (Image)((Grid)wpfbutton.Content).Children[0];
                img.Margin = new Thickness(5.0d);
                var label = (Label)((Grid)wpfbutton.Content).Children[1];
                label.HorizontalAlignment = HorizontalAlignment.Left;
                TextOptions.SetTextRenderingMode(label, TextRenderingMode.Auto);
            });
        
        }

    
    }

}
