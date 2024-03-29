using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace DWSIM.ProFeatures
{
    [Microsoft.VisualBasic.CompilerServices.DesignerGenerated()]
    public partial class ListItem : System.Windows.Forms.UserControl
    {

        // UserControl overrides dispose to clean up the component list.
        [DebuggerNonUserCode()]
        protected override void Dispose(bool disposing)
        {
            try
            {
                if (disposing && components is not null)
                {
                    components.Dispose();
                }
            }
            finally
            {
                base.Dispose(disposing);
            }
        }

        // Required by the Windows Form Designer
        private System.ComponentModel.IContainer components;

        // NOTE: The following procedure is required by the Windows Form Designer
        // It can be modified using the Windows Form Designer.  
        // Do not modify it using the code editor.
        [DebuggerStepThrough()]
        private void InitializeComponent()
        {
            components = new System.ComponentModel.Container();
            Image = new System.Windows.Forms.PictureBox();
            Image.MouseDown += new System.Windows.Forms.MouseEventHandler(ListItem_MouseDown);
            Image.MouseUp += new System.Windows.Forms.MouseEventHandler(ListItem_MouseUp);
            _lblName = new System.Windows.Forms.Label();
            _lblName.MouseDown += new System.Windows.Forms.MouseEventHandler(ListItem_MouseDown);
            _lblName.MouseUp += new System.Windows.Forms.MouseEventHandler(ListItem_MouseUp);
            ToolTip1 = new System.Windows.Forms.ToolTip(components);
            ((System.ComponentModel.ISupportInitialize)Image).BeginInit();
            SuspendLayout();
            // 
            // Image
            // 
            Image.Location = new System.Drawing.Point(49, 6);
            Image.Margin = new System.Windows.Forms.Padding(0);
            Image.Name = "Image";
            Image.Size = new System.Drawing.Size(32, 32);
            Image.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            Image.TabIndex = 0;
            Image.TabStop = false;
            // 
            // lblName
            // 
            _lblName.Anchor = System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left | System.Windows.Forms.AnchorStyles.Right;
            _lblName.Font = new System.Drawing.Font("Arial", 6.75f, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 0);
            _lblName.Location = new System.Drawing.Point(6, 44);
            _lblName.Margin = new System.Windows.Forms.Padding(6, 0, 6, 0);
            _lblName.Name = "_lblName";
            _lblName.Size = new System.Drawing.Size(120, 24);
            _lblName.TabIndex = 1;
            _lblName.Text = "Label1";
            _lblName.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // ListItem
            // 
            AutoScaleDimensions = new System.Drawing.SizeF(96.0f, 96.0f);
            AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            BackColor = System.Drawing.Color.White;
            Controls.Add(_lblName);
            Controls.Add(Image);
            DoubleBuffered = true;
            Margin = new System.Windows.Forms.Padding(0);
            Name = "ListItem";
            Size = new System.Drawing.Size(132, 75);
            ((System.ComponentModel.ISupportInitialize)Image).EndInit();
            GiveFeedback += new System.Windows.Forms.GiveFeedbackEventHandler(ListItem_GiveFeedback);
            MouseDown += new System.Windows.Forms.MouseEventHandler(ListItem_MouseDown);
            MouseUp += new System.Windows.Forms.MouseEventHandler(ListItem_MouseUp);
            ResumeLayout(false);

        }
        internal System.Windows.Forms.PictureBox Image;
        private System.Windows.Forms.Label _lblName;

        public virtual System.Windows.Forms.Label lblName
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            get
            {
                return _lblName;
            }

            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                if (_lblName != null)
                {
                    _lblName.MouseDown -= ListItem_MouseDown;
                    _lblName.MouseUp -= ListItem_MouseUp;
                }

                _lblName = value;
                if (_lblName != null)
                {
                    _lblName.MouseDown += ListItem_MouseDown;
                    _lblName.MouseUp += ListItem_MouseUp;
                }
            }
        }
        internal System.Windows.Forms.ToolTip ToolTip1;
    }
}