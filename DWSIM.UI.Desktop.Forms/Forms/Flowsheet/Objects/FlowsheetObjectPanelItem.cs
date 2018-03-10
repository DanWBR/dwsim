using System;
using Eto.Forms;
using Eto.Drawing;

namespace DWSIM.UI.Forms
{
    public class FlowsheetObjectPanelItem: PixelLayout  
    {

        public ImageView imgIcon;
        public Label txtName, txtDescription;

        public static int width = 300;

        public FlowsheetObjectPanelItem()
        {

            int padding = 5;
            int height = 70;

            int iconsize = height-6*padding;

            Size = new Size(width, height);

            imgIcon = new ImageView() { Size = new Eto.Drawing.Size(iconsize, iconsize) };
            txtName = new Label() { Text = "Name", Font = SystemFonts.Bold() };
            txtDescription = new Label() { Text = "Description", Size = new Size(padding + width - 10 - iconsize, height - 20)};

            txtName.Font = new Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize());
            txtDescription.Font = new Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize());

            Add(imgIcon, padding, 3*padding);
            Add(txtName, padding+iconsize + 6, padding);
            Add(txtDescription, padding+iconsize + 6, padding+16);

            MouseEnter += FlowsheetObjectPanelItem_MouseEnter;

            MouseLeave += FlowsheetObjectPanelItem_MouseLeave;

            BackgroundColor = Colors.White;

        }

        private void FlowsheetObjectPanelItem_MouseLeave(object sender, MouseEventArgs e)
        {
            BackgroundColor = Colors.White;
        }

        private void FlowsheetObjectPanelItem_MouseEnter(object sender, MouseEventArgs e)
        {
            BackgroundColor = Colors.LightSteelBlue;
        }
    }
}
