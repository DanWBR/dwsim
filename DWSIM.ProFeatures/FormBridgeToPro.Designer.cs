using System;
using System.Diagnostics;
using System.Runtime.CompilerServices;

namespace DWSIM.ProFeatures
{
    [Microsoft.VisualBasic.CompilerServices.DesignerGenerated()]
    public partial class FormBridgeToPro : System.Windows.Forms.Form
    {

        // Form overrides dispose to clean up the component list.
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
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(FormBridgeToPro));
            this.PictureBox1 = new System.Windows.Forms.PictureBox();
            this.Label1 = new System.Windows.Forms.Label();
            this.Button1 = new System.Windows.Forms.Button();
            this.Button2 = new System.Windows.Forms.Button();
            this.Label4 = new System.Windows.Forms.Label();
            this._lblFeature = new System.Windows.Forms.Label();
            this.PictureBox2 = new System.Windows.Forms.PictureBox();
            this.PictureBox3 = new System.Windows.Forms.PictureBox();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.BridgeToProPanel = new System.Windows.Forms.Panel();
            this.formPortal = new DWSIM.ProFeatures.FormPortal();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox2)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox3)).BeginInit();
            this.flowLayoutPanel1.SuspendLayout();
            this.BridgeToProPanel.SuspendLayout();
            this.SuspendLayout();
            // 
            // PictureBox1
            // 
            this.PictureBox1.Image = global::DWSIM.ProFeatures.My.Resources.Resources.Icon512;
            this.PictureBox1.Location = new System.Drawing.Point(470, 98);
            this.PictureBox1.Name = "PictureBox1";
            this.PictureBox1.Size = new System.Drawing.Size(133, 197);
            this.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.PictureBox1.TabIndex = 0;
            this.PictureBox1.TabStop = false;
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label1.Location = new System.Drawing.Point(-3, 56);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(317, 25);
            this.Label1.TabIndex = 1;
            this.Label1.Text = "This is a DWSIM Pro feature.";
            // 
            // Button1
            // 
            this.Button1.BackColor = System.Drawing.Color.SteelBlue;
            this.Button1.Font = new System.Drawing.Font("Calibri", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Button1.ForeColor = System.Drawing.Color.White;
            this.Button1.Location = new System.Drawing.Point(86, 301);
            this.Button1.Name = "Button1";
            this.Button1.Size = new System.Drawing.Size(182, 72);
            this.Button1.TabIndex = 2;
            this.Button1.Text = "Continue on DWSIM";
            this.Button1.UseVisualStyleBackColor = false;
            this.Button1.Visible = false;
            this.Button1.Click += new System.EventHandler(this.Button1_Click);
            // 
            // Button2
            // 
            this.Button2.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(31)))), ((int)(((byte)(166)))), ((int)(((byte)(13)))));
            this.Button2.Font = new System.Drawing.Font("Calibri", 15.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Button2.ForeColor = System.Drawing.Color.White;
            this.Button2.Location = new System.Drawing.Point(430, 301);
            this.Button2.Name = "Button2";
            this.Button2.Size = new System.Drawing.Size(209, 72);
            this.Button2.TabIndex = 3;
            this.Button2.Text = "Switch to DWSIM Pro";
            this.Button2.UseVisualStyleBackColor = false;
            this.Button2.Click += new System.EventHandler(this.Button2_Click);
            // 
            // Label4
            // 
            this.Label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.Label4.Location = new System.Drawing.Point(400, 376);
            this.Label4.Name = "Label4";
            this.Label4.Size = new System.Drawing.Size(284, 64);
            this.Label4.TabIndex = 6;
            this.Label4.Text = "Your flowsheet will be automatically saved on Simulate365 Dashboard";
            this.Label4.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // _lblFeature
            // 
            this._lblFeature.AutoEllipsis = true;
            this._lblFeature.Font = new System.Drawing.Font("Microsoft Sans Serif", 20.25F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this._lblFeature.Location = new System.Drawing.Point(-1, 0);
            this._lblFeature.Name = "_lblFeature";
            this._lblFeature.Size = new System.Drawing.Size(739, 48);
            this._lblFeature.TabIndex = 7;
            this._lblFeature.Text = "FEATURE";
            // 
            // PictureBox2
            // 
            this.PictureBox2.Image = global::DWSIM.ProFeatures.My.Resources.Resources.DWSIM_Icon_Vector_Transp;
            this.PictureBox2.Location = new System.Drawing.Point(106, 98);
            this.PictureBox2.Name = "PictureBox2";
            this.PictureBox2.Size = new System.Drawing.Size(133, 197);
            this.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.PictureBox2.TabIndex = 8;
            this.PictureBox2.TabStop = false;
            // 
            // PictureBox3
            // 
            this.PictureBox3.Image = global::DWSIM.ProFeatures.My.Resources.Resources.right_60px;
            this.PictureBox3.Location = new System.Drawing.Point(338, 166);
            this.PictureBox3.Name = "PictureBox3";
            this.PictureBox3.Size = new System.Drawing.Size(34, 64);
            this.PictureBox3.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.PictureBox3.TabIndex = 9;
            this.PictureBox3.TabStop = false;
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.Controls.Add(this.BridgeToProPanel);
            this.flowLayoutPanel1.Controls.Add(this.formPortal);
            this.flowLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(0, 0);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(753, 512);
            this.flowLayoutPanel1.TabIndex = 10;
            // 
            // BridgeToProPanel
            // 
            this.BridgeToProPanel.Controls.Add(this._lblFeature);
            this.BridgeToProPanel.Controls.Add(this.PictureBox3);
            this.BridgeToProPanel.Controls.Add(this.PictureBox1);
            this.BridgeToProPanel.Controls.Add(this.PictureBox2);
            this.BridgeToProPanel.Controls.Add(this.Label1);
            this.BridgeToProPanel.Controls.Add(this.Button1);
            this.BridgeToProPanel.Controls.Add(this.Label4);
            this.BridgeToProPanel.Controls.Add(this.Button2);
            this.BridgeToProPanel.Location = new System.Drawing.Point(3, 3);
            this.BridgeToProPanel.Name = "BridgeToProPanel";
            this.BridgeToProPanel.Size = new System.Drawing.Size(750, 482);
            this.BridgeToProPanel.TabIndex = 0;
            // 
            // formPortal
            // 
            this.formPortal.BackColor = System.Drawing.Color.White;
            this.formPortal.Location = new System.Drawing.Point(3, 491);
            this.formPortal.Name = "formPortal";
            this.formPortal.Size = new System.Drawing.Size(807, 689);
            this.formPortal.TabIndex = 1;
            this.formPortal.Visible = false;
            this.formPortal.VisibleChanged += new System.EventHandler(this.formPortal_VisibleChanged);
            // 
            // FormBridgeToPro
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.BackColor = System.Drawing.Color.White;
            this.ClientSize = new System.Drawing.Size(753, 512);
            this.Controls.Add(this.flowLayoutPanel1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.MinimizeBox = false;
            this.Name = "FormBridgeToPro";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Continue on DWSIM Pro";
            this.FormClosing += new System.Windows.Forms.FormClosingEventHandler(this.FormBridgeToPro_FormClosing);
            this.Load += new System.EventHandler(this.FormBridgeToPro_Load);
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox2)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.PictureBox3)).EndInit();
            this.flowLayoutPanel1.ResumeLayout(false);
            this.BridgeToProPanel.ResumeLayout(false);
            this.BridgeToProPanel.PerformLayout();
            this.ResumeLayout(false);

        }

        internal System.Windows.Forms.PictureBox PictureBox1;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.Button Button1;
        internal System.Windows.Forms.Button Button2;
        internal System.Windows.Forms.Label Label4;
        private System.Windows.Forms.Label _lblFeature;

        public virtual System.Windows.Forms.Label lblFeature
        {
            [MethodImpl(MethodImplOptions.Synchronized)]
            get
            {
                return _lblFeature;
            }

            [MethodImpl(MethodImplOptions.Synchronized)]
            set
            {
                _lblFeature = value;
            }
        }
        internal System.Windows.Forms.PictureBox PictureBox2;
        internal System.Windows.Forms.PictureBox PictureBox3;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Panel BridgeToProPanel;
        private FormPortal formPortal;
    }
}