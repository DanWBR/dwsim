using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Forms
{
    static class Shared
    {

        static public System.Resources.ResourceManager _ResourceManager;

        /// <summary>
        /// Returns the localized version of this string.
        /// </summary>
        /// <param name="str">The string to be localized.</param>
        /// <returns>A localized version of the string.</returns>
        public static string Localize(this String str)
        {
            return GetLocalizedString(str);
        }

        public static string GetLocalizedString(string text)
        {
            if (_ResourceManager == null)
            {
                System.Threading.Thread.CurrentThread.CurrentUICulture = new CultureInfo(GlobalSettings.Settings.CultureInfo);
                //loads the resource manager
                _ResourceManager = new System.Resources.ResourceManager("DWSIM.UI.Forms.Localization.Strings", System.Reflection.Assembly.GetExecutingAssembly());
            }
            
            if (!string.IsNullOrEmpty(text))
            {
                if (System.Threading.Thread.CurrentThread.CurrentUICulture.Name != GlobalSettings.Settings.CultureInfo)
                {
                    System.Threading.Thread.CurrentThread.CurrentUICulture = new CultureInfo(GlobalSettings.Settings.CultureInfo);
                }

                string retstr = _ResourceManager.GetString(text, System.Threading.Thread.CurrentThread.CurrentUICulture);
                if (retstr == null)
                    return text;
                else
                    return retstr;
            }
            else
            {
                return "";

            }

        }


        static public string AssemblyCopyright
        {
            get
            {
                object[] attributes = Assembly.GetExecutingAssembly().GetCustomAttributes(typeof(AssemblyCopyrightAttribute), false);
                if (attributes.Length == 0)
                {
                    return "";
                }
                return ((AssemblyCopyrightAttribute)attributes[0]).Copyright;
            }
        }



    }
}
