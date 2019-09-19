using Eto.Drawing;
using Eto.Forms;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace DWSIM.ExtensionMethods.Eto
{
    public class Border : Panel
    {
        private readonly Panel containingPanel;

        private readonly Panel borderPanel;

        public Border()
        {
            this.containingPanel = new Panel();
            this.borderPanel = new Panel();

            this.borderPanel.Content = this.containingPanel;
            base.Content = this.borderPanel;

            this.borderPanel.BackgroundColor = Colors.Black;
            this.containingPanel.BackgroundColor = Colors.White;
        }

        public new Color BackgroundColor
        {
            get
            {
                return this.containingPanel.BackgroundColor;
            }

            set
            {
                this.containingPanel.BackgroundColor = value;
            }
        }

        public Color BorderColor
        {
            get
            {
                return this.borderPanel.BackgroundColor;
            }

            set
            {
                this.borderPanel.BackgroundColor = value;
            }
        }

        public int BorderThickness
        {
            get
            {
                return this.borderPanel.Padding.Left;
            }

            set
            {
                this.borderPanel.Padding = new Padding(value);
            }
        }

        public new Control Content
        {
            get
            {
                return this.containingPanel.Content;
            }

            set
            {
                this.containingPanel.Content = value;
            }
        }
    }

}
