using AppKit;
using Foundation;

namespace DWSIM.UI.Desktop.Mac
{

    public static class StyleSetter
    {

        public static void SetStyles()
        {
            
            Eto.Style.Add<Eto.Forms.Panel>("transparent-form", control =>
            {
                System.Console.WriteLine(control.ControlObject.GetType().ToString());
                var cocoawnd = ((Eto.Mac.Forms.EtoWindow)control.ControlObject);
                cocoawnd.IsOpaque = false;
                cocoawnd.BackgroundColor = NSColor.FromCalibratedWhite(1.0f, 0.0f);
                cocoawnd.StyleMask = NSWindowStyle.Borderless;
                cocoawnd.HasShadow = false;
            });

            Eto.Style.Add<Eto.Forms.TextBox>("textbox-rightalign", control =>
            {
                var tbox = (NSTextField)control.ControlObject;
                tbox.Alignment = NSTextAlignment.Right;
            });

        }

        public static void FinishedLaunching()
        {
            if (NSProcessInfo.ProcessInfo.IsOperatingSystemAtLeastVersion(new NSOperatingSystemVersion(10, 12, 2)))
            {
                NSApplication.SharedApplication.SetAutomaticCustomizeTouchBarMenuItemEnabled(true);
            }
        }

    }

}
