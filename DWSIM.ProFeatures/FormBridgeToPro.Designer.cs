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
            var resources = new System.ComponentModel.ComponentResourceManager(typeof(FormBridgeToPro));
            PictureBox1 = new System.Windows.Forms.PictureBox();
            Label1 = new System.Windows.Forms.Label();
            Button1 = new System.Windows.Forms.Button();
            Button1.Click += new EventHandler(Button1_Click);
            Button2 = new System.Windows.Forms.Button();
            Button2.Click += new EventHandler(Button2_Click);
            Label4 = new System.Windows.Forms.Label();
            _lblFeature = new System.Windows.Forms.Label();
            PictureBox2 = new System.Windows.Forms.PictureBox();
            PictureBox3 = new System.Windows.Forms.PictureBox();
            ((System.ComponentModel.ISupportInitialize)PictureBox1).BeginInit();
            ((System.ComponentModel.ISupportInitialize)PictureBox2).BeginInit();
            ((System.ComponentModel.ISupportInitialize)PictureBox3).BeginInit();
            SuspendLayout();
            // 
            // PictureBox1
            // 
            PictureBox1.Image = My.Resources.Resources.Icon512;
            PictureBox1.Location = new System.Drawing.Point(469, 110);
            PictureBox1.Name = "PictureBox1";
            PictureBox1.Size = new System.Drawing.Size(197, 197);
            PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            PictureBox1.TabIndex = 0;
            PictureBox1.TabStop = false;
            // 
            // Label1
            // 
            Label1.AutoSize = true;
            Label1.Font = new System.Drawing.Font("Microsoft Sans Serif", 15.75f, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 0);
            Label1.Location = new System.Drawing.Point(12, 68);
            Label1.Name = "Label1";
            Label1.Size = new System.Drawing.Size(317, 25);
            Label1.TabIndex = 1;
            Label1.Text = "This is a DWSIM Pro feature.";
            // 
            // Button1
            // 
            Button1.BackColor = System.Drawing.Color.SteelBlue;
            Button1.Font = new System.Drawing.Font("Calibri", 14.25f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 0);
            Button1.ForeColor = System.Drawing.Color.White;
            Button1.Location = new System.Drawing.Point(97, 313);
            Button1.Name = "Button1";
            Button1.Size = new System.Drawing.Size(246, 72);
            Button1.TabIndex = 2;
            Button1.Text = "Continue on DWSIM";
            Button1.UseVisualStyleBackColor = false;
            Button1.Visible = false;
            // 
            // Button2
            // 
            Button2.BackColor = System.Drawing.Color.FromArgb(31, 166, 13);
            Button2.Font = new System.Drawing.Font("Calibri", 15.75f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 0);
            Button2.ForeColor = System.Drawing.Color.White;
            Button2.Location = new System.Drawing.Point(445, 313);
            Button2.Name = "Button2";
            Button2.Size = new System.Drawing.Size(246, 72);
            Button2.TabIndex = 3;
            Button2.Text = "Switch to DWSIM Pro";
            Button2.UseVisualStyleBackColor = false;
            // 
            // Label4
            // 
            Label4.Font = new System.Drawing.Font("Microsoft Sans Serif", 12.0f, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, 0);
            Label4.Location = new System.Drawing.Point(393, 391);
            Label4.Name = "Label4";
            Label4.Size = new System.Drawing.Size(348, 64);
            Label4.TabIndex = 6;
            Label4.Text = "Your flowsheet will be automatically saved on Simulate365 Dashboard";
            Label4.TextAlign = System.Drawing.ContentAlignment.TopCenter;
            // 
            // lblFeature
            // 
            _lblFeature.AutoEllipsis = true;
            _lblFeature.Font = new System.Drawing.Font("Microsoft Sans Serif", 20.25f, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, 0);
            _lblFeature.Location = new System.Drawing.Point(14, 12);
            _lblFeature.Name = "_lblFeature";
            _lblFeature.Size = new System.Drawing.Size(803, 48);
            _lblFeature.TabIndex = 7;
            _lblFeature.Text = "FEATURE";
            // 
            // PictureBox2
            // 
            PictureBox2.Image = My.Resources.Resources.DWSIM_Icon_Vector_Transp;
            PictureBox2.Location = new System.Drawing.Point(121, 110);
            PictureBox2.Name = "PictureBox2";
            PictureBox2.Size = new System.Drawing.Size(197, 197);
            PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            PictureBox2.TabIndex = 8;
            PictureBox2.TabStop = false;
            // 
            // PictureBox3
            // 
            PictureBox3.Image = My.Resources.Resources.right_60px;
            PictureBox3.Location = new System.Drawing.Point(361, 180);
            PictureBox3.Name = "PictureBox3";
            PictureBox3.Size = new System.Drawing.Size(65, 64);
            PictureBox3.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            PictureBox3.TabIndex = 9;
            PictureBox3.TabStop = false;
            // 
            // FormBridgeToPro
            // 
            AutoScaleDimensions = new System.Drawing.SizeF(96.0f, 96.0f);
            AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            BackColor = System.Drawing.Color.White;
            ClientSize = new System.Drawing.Size(792, 474);
            Controls.Add(PictureBox3);
            Controls.Add(PictureBox2);
            Controls.Add(_lblFeature);
            Controls.Add(Label4);
            Controls.Add(Button2);
            Controls.Add(Button1);
            Controls.Add(Label1);
            Controls.Add(PictureBox1);
            FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            Icon = (System.Drawing.Icon)resources.GetObject("$this.Icon");
            MaximizeBox = false;
            MinimizeBox = false;
            Name = "FormBridgeToPro";
            StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            Text = "Continue on DWSIM Pro";
            ((System.ComponentModel.ISupportInitialize)PictureBox1).EndInit();
            ((System.ComponentModel.ISupportInitialize)PictureBox2).EndInit();
            ((System.ComponentModel.ISupportInitialize)PictureBox3).EndInit();
            Load += new EventHandler(FormBridgeToPro_Load);
            FormClosing += new System.Windows.Forms.FormClosingEventHandler(FormBridgeToPro_FormClosing);
            ResumeLayout(false);
            PerformLayout();

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
    }
}