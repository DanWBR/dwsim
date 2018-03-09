using AppKit;

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
            });

        }

    }

}
