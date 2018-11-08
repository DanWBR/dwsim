using System;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.UI.Forms
{
    public class FlowsheetObjectPanelItem : TableLayout
    {

        public ImageView imgIcon;
        public Label txtName;

        public static int width = (int)(GlobalSettings.Settings.UIScalingFactor * 95);

        public FlowsheetObjectPanelItem()
        {
            
            int height = (int)(GlobalSettings.Settings.UIScalingFactor * 70);

            int iconsize = (int)(GlobalSettings.Settings.UIScalingFactor * 32);

            Size = new Size(width, height);

            imgIcon = new ImageView() { Size = new Eto.Drawing.Size(iconsize, iconsize) };
            txtName = new Label() { Text = "Name", Width = width, Font = SystemFonts.Bold(), TextAlignment = TextAlignment.Center };
            txtName.Font = new Font(SystemFont.Bold, 9);

            Rows.Add(imgIcon);
            Rows.Add(txtName);

            MouseEnter += FlowsheetObjectPanelItem_MouseEnter;

            MouseLeave += FlowsheetObjectPanelItem_MouseLeave;
            
        }

        private void FlowsheetObjectPanelItem_MouseLeave(object sender, MouseEventArgs e)
        {
            BackgroundColor = SystemColors.ControlBackground;
        }

        private void FlowsheetObjectPanelItem_MouseEnter(object sender, MouseEventArgs e)
        {
            if (GlobalSettings.Settings.DarkMode)
            {
                BackgroundColor = Colors.DarkGray;
            }
            else
            {
                BackgroundColor = Colors.LightSteelBlue;
            }

        }
    }
}
