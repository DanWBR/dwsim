using DWSIM.CrossPlatform.UI.Controls.ReoGrid;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.UI.Desktop.Editors
{
    class GridControl
    {

        public static ReoGridFullControl GetGridControl()
        {

            var ReoGridControl = new ReoGridFullControl(false);
            ReoGridControl.GridControl.bottomPanel.Visible = false;
            var stacks = ReoGridControl.Children.Where(x => x is StackLayout).ToList();
            foreach (var stack in stacks)
            {
                stack.Visible = false;
            }
            return ReoGridControl;
        }


    }
}
