using System;
using System.Collections.Generic;
using System.Reflection;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;

namespace DWSIM.UI.Desktop.WPF
{

    public static class StyleSetter
    {

        public static void SetStyles()
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

            Eto.Style.Add<Eto.Forms.Panel>("transparent-form", control =>
            {
                var wpfwnd = (System.Windows.Window)control.ControlObject;
                TextOptions.SetTextRenderingMode(wpfwnd, TextRenderingMode.Auto);
                wpfwnd.AllowsTransparency = true;
                wpfwnd.Background = Brushes.Transparent;
            });

        }

        /// <summary>
        /// Sets the WPF system theme.
        /// </summary>
        /// <param name="themeName">The name of the theme. (ie "aero")</param>
        /// <param name="themeColor">The name of the color. (ie "normalcolor")</param>
        public static void SetTheme(string themeName, string themeColor)
        {
            const BindingFlags staticNonPublic = BindingFlags.Static | BindingFlags.NonPublic;

            var presentationFrameworkAsm = Assembly.GetAssembly(typeof(Window));

            var themeWrapper = presentationFrameworkAsm.GetType("MS.Win32.UxThemeWrapper");

            var isActiveField = themeWrapper.GetField("_isActive", staticNonPublic);
            var themeColorField = themeWrapper.GetField("_themeColor", staticNonPublic);
            var themeNameField = themeWrapper.GetField("_themeName", staticNonPublic);

            // Set this to true so WPF doesn't default to classic.
            isActiveField.SetValue(null, true);

            themeColorField.SetValue(null, themeColor);
            themeNameField.SetValue(null, themeName);
        }
    }
}
