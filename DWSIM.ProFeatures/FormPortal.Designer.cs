using System;
using System.Diagnostics;

namespace DWSIM.ProFeatures
{
    [Microsoft.VisualBasic.CompilerServices.DesignerGenerated()]
    public partial class FormPortal : System.Windows.Forms.UserControl
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
            this.StatusMessage = new System.Windows.Forms.Label();
            this.Label1 = new System.Windows.Forms.Label();
            this.NoLicensePanel = new System.Windows.Forms.Panel();
            this.button1 = new System.Windows.Forms.Button();
            this.LoadingPanel = new System.Windows.Forms.Panel();
            this.flowLayoutPanel1 = new System.Windows.Forms.FlowLayoutPanel();
            this.notLoggedInPanel = new System.Windows.Forms.Panel();
            this.label3 = new System.Windows.Forms.Label();
            this.label4 = new System.Windows.Forms.Label();
            this.linkLabel2 = new System.Windows.Forms.LinkLabel();
            this.SuccessPanel = new System.Windows.Forms.Panel();
            this.button3 = new System.Windows.Forms.Button();
            this.button2 = new System.Windows.Forms.Button();
            this.label10 = new System.Windows.Forms.Label();
            this.label9 = new System.Windows.Forms.Label();
            this.label8 = new System.Windows.Forms.Label();
            this.label7 = new System.Windows.Forms.Label();
            this.label5 = new System.Windows.Forms.Label();
            this.pictureBox1 = new System.Windows.Forms.PictureBox();
            this.pictureBox2 = new System.Windows.Forms.PictureBox();
            this.label6 = new System.Windows.Forms.Label();
            this.NoLicensePanel.SuspendLayout();
            this.LoadingPanel.SuspendLayout();
            this.flowLayoutPanel1.SuspendLayout();
            this.notLoggedInPanel.SuspendLayout();
            this.SuccessPanel.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).BeginInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox2)).BeginInit();
            this.SuspendLayout();
            // 
            // StatusMessage
            // 
            this.StatusMessage.AutoSize = true;
            this.StatusMessage.Location = new System.Drawing.Point(4, 4);
            this.StatusMessage.Name = "StatusMessage";
            this.StatusMessage.Size = new System.Drawing.Size(123, 13);
            this.StatusMessage.TabIndex = 1;
            this.StatusMessage.Text = "This is a progress label...";
            // 
            // Label1
            // 
            this.Label1.AutoSize = true;
            this.Label1.Location = new System.Drawing.Point(4, 4);
            this.Label1.Name = "Label1";
            this.Label1.Size = new System.Drawing.Size(402, 13);
            this.Label1.TabIndex = 3;
            this.Label1.Text = "You don\'t have an active DWSIM Pro license. Please go to Shop to get the license." +
    "";
            // 
            // NoLicensePanel
            // 
            this.NoLicensePanel.Controls.Add(this.button1);
            this.NoLicensePanel.Controls.Add(this.Label1);
            this.NoLicensePanel.Location = new System.Drawing.Point(3, 33);
            this.NoLicensePanel.Name = "NoLicensePanel";
            this.NoLicensePanel.Size = new System.Drawing.Size(652, 48);
            this.NoLicensePanel.TabIndex = 4;
            this.NoLicensePanel.Visible = false;
            // 
            // button1
            // 
            this.button1.Location = new System.Drawing.Point(7, 21);
            this.button1.Name = "button1";
            this.button1.Size = new System.Drawing.Size(75, 23);
            this.button1.TabIndex = 4;
            this.button1.Text = "Go to Shop";
            this.button1.UseVisualStyleBackColor = true;
            this.button1.Click += new System.EventHandler(this.button1_Click);
            // 
            // LoadingPanel
            // 
            this.LoadingPanel.AutoScroll = true;
            this.LoadingPanel.Controls.Add(this.StatusMessage);
            this.LoadingPanel.Location = new System.Drawing.Point(3, 3);
            this.LoadingPanel.Name = "LoadingPanel";
            this.LoadingPanel.Size = new System.Drawing.Size(652, 24);
            this.LoadingPanel.TabIndex = 5;
            this.LoadingPanel.Visible = false;
            // 
            // flowLayoutPanel1
            // 
            this.flowLayoutPanel1.Controls.Add(this.LoadingPanel);
            this.flowLayoutPanel1.Controls.Add(this.NoLicensePanel);
            this.flowLayoutPanel1.Controls.Add(this.notLoggedInPanel);
            this.flowLayoutPanel1.Controls.Add(this.SuccessPanel);
            this.flowLayoutPanel1.FlowDirection = System.Windows.Forms.FlowDirection.TopDown;
            this.flowLayoutPanel1.Location = new System.Drawing.Point(87, 381);
            this.flowLayoutPanel1.Name = "flowLayoutPanel1";
            this.flowLayoutPanel1.Size = new System.Drawing.Size(655, 256);
            this.flowLayoutPanel1.TabIndex = 6;
            // 
            // notLoggedInPanel
            // 
            this.notLoggedInPanel.Controls.Add(this.label3);
            this.notLoggedInPanel.Controls.Add(this.label4);
            this.notLoggedInPanel.Controls.Add(this.linkLabel2);
            this.notLoggedInPanel.Location = new System.Drawing.Point(3, 87);
            this.notLoggedInPanel.Name = "notLoggedInPanel";
            this.notLoggedInPanel.Size = new System.Drawing.Size(652, 22);
            this.notLoggedInPanel.TabIndex = 6;
            // 
            // label3
            // 
            this.label3.AutoSize = true;
            this.label3.Location = new System.Drawing.Point(77, 3);
            this.label3.Name = "label3";
            this.label3.Size = new System.Drawing.Size(63, 13);
            this.label3.TabIndex = 7;
            this.label3.Text = "to continue.";
            // 
            // label4
            // 
            this.label4.AutoSize = true;
            this.label4.Location = new System.Drawing.Point(3, 3);
            this.label4.Name = "label4";
            this.label4.Size = new System.Drawing.Size(39, 13);
            this.label4.TabIndex = 6;
            this.label4.Text = "Please";
            // 
            // linkLabel2
            // 
            this.linkLabel2.AutoSize = true;
            this.linkLabel2.Location = new System.Drawing.Point(39, 3);
            this.linkLabel2.Margin = new System.Windows.Forms.Padding(0);
            this.linkLabel2.Name = "linkLabel2";
            this.linkLabel2.Size = new System.Drawing.Size(36, 13);
            this.linkLabel2.TabIndex = 5;
            this.linkLabel2.TabStop = true;
            this.linkLabel2.Text = "Log in";
            this.linkLabel2.LinkClicked += new System.Windows.Forms.LinkLabelLinkClickedEventHandler(this.linkLabel2_LinkClicked);
            // 
            // SuccessPanel
            // 
            this.SuccessPanel.Controls.Add(this.button3);
            this.SuccessPanel.Controls.Add(this.button2);
            this.SuccessPanel.Controls.Add(this.label10);
            this.SuccessPanel.Controls.Add(this.label9);
            this.SuccessPanel.Controls.Add(this.label8);
            this.SuccessPanel.Controls.Add(this.label7);
            this.SuccessPanel.Controls.Add(this.label5);
            this.SuccessPanel.Location = new System.Drawing.Point(3, 115);
            this.SuccessPanel.Name = "SuccessPanel";
            this.SuccessPanel.Size = new System.Drawing.Size(652, 135);
            this.SuccessPanel.TabIndex = 7;
            this.SuccessPanel.Visible = false;
            // 
            // button3
            // 
            this.button3.Location = new System.Drawing.Point(274, 103);
            this.button3.Name = "button3";
            this.button3.Size = new System.Drawing.Size(190, 23);
            this.button3.TabIndex = 8;
            this.button3.Text = "Open in Incognito/Private mode";
            this.button3.UseVisualStyleBackColor = true;
            this.button3.Click += new System.EventHandler(this.button3_Click);
            // 
            // button2
            // 
            this.button2.Location = new System.Drawing.Point(80, 103);
            this.button2.Name = "button2";
            this.button2.Size = new System.Drawing.Size(167, 23);
            this.button2.TabIndex = 7;
            this.button2.Text = "Open with default browser";
            this.button2.UseVisualStyleBackColor = true;
            this.button2.Click += new System.EventHandler(this.button2_Click);
            // 
            // label10
            // 
            this.label10.AutoSize = true;
            this.label10.Location = new System.Drawing.Point(4, 81);
            this.label10.Name = "label10";
            this.label10.Size = new System.Drawing.Size(589, 13);
            this.label10.TabIndex = 4;
            this.label10.Text = "Launch private browsing mode to access DWSIM Pro: “Incognito” for Chrome, “InPriv" +
    "ate” for Edge and “Private” for Firefox.";
            // 
            // label9
            // 
            this.label9.AutoSize = true;
            this.label9.Location = new System.Drawing.Point(2, 59);
            this.label9.Name = "label9";
            this.label9.Size = new System.Drawing.Size(240, 13);
            this.label9.TabIndex = 3;
            this.label9.Text = "\"Your administrator hasn’t set up any resources\". ";
            // 
            // label8
            // 
            this.label8.AutoSize = true;
            this.label8.Location = new System.Drawing.Point(4, 41);
            this.label8.Name = "label8";
            this.label8.Size = new System.Drawing.Size(525, 13);
            this.label8.TabIndex = 2;
            this.label8.Text = "If you are logged in on your PC to your company or personal Microsoft account, yo" +
    "u can get an error message:";
            // 
            // label7
            // 
            this.label7.AutoSize = true;
            this.label7.Location = new System.Drawing.Point(2, 21);
            this.label7.Name = "label7";
            this.label7.Size = new System.Drawing.Size(245, 13);
            this.label7.TabIndex = 1;
            this.label7.Text = "Your Simulate 365 account is a Microsoft account.";
            // 
            // label5
            // 
            this.label5.AutoSize = true;
            this.label5.Location = new System.Drawing.Point(2, 3);
            this.label5.Name = "label5";
            this.label5.Size = new System.Drawing.Size(280, 13);
            this.label5.TabIndex = 0;
            this.label5.Text = "You log in to DWSIM Pro with your Simulate 365 account.";
            // 
            // pictureBox1
            // 
            this.pictureBox1.Image = global::DWSIM.ProFeatures.My.Resources.Resources._2149311493;
            this.pictureBox1.Location = new System.Drawing.Point(0, -1);
            this.pictureBox1.Name = "pictureBox1";
            this.pictureBox1.Size = new System.Drawing.Size(745, 353);
            this.pictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.StretchImage;
            this.pictureBox1.TabIndex = 7;
            this.pictureBox1.TabStop = false;
            // 
            // pictureBox2
            // 
            this.pictureBox2.Image = global::DWSIM.ProFeatures.My.Resources.Resources.Icon1281;
            this.pictureBox2.Location = new System.Drawing.Point(-1, 358);
            this.pictureBox2.Name = "pictureBox2";
            this.pictureBox2.Size = new System.Drawing.Size(89, 86);
            this.pictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom;
            this.pictureBox2.TabIndex = 8;
            this.pictureBox2.TabStop = false;
            // 
            // label6
            // 
            this.label6.AutoSize = true;
            this.label6.Font = new System.Drawing.Font("Microsoft Sans Serif", 12F, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.label6.Location = new System.Drawing.Point(91, 360);
            this.label6.Name = "label6";
            this.label6.Size = new System.Drawing.Size(216, 20);
            this.label6.TabIndex = 9;
            this.label6.Text = "Continue with DWSIM Pro";
            // 
            // FormPortal
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(96F, 96F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi;
            this.BackColor = System.Drawing.Color.White;
            this.Controls.Add(this.label6);
            this.Controls.Add(this.pictureBox2);
            this.Controls.Add(this.pictureBox1);
            this.Controls.Add(this.flowLayoutPanel1);
            this.DoubleBuffered = true;
            this.Name = "FormPortal";
            this.Size = new System.Drawing.Size(745, 534);
            this.Load += new System.EventHandler(this.FormPortal_Load);
            this.NoLicensePanel.ResumeLayout(false);
            this.NoLicensePanel.PerformLayout();
            this.LoadingPanel.ResumeLayout(false);
            this.LoadingPanel.PerformLayout();
            this.flowLayoutPanel1.ResumeLayout(false);
            this.notLoggedInPanel.ResumeLayout(false);
            this.notLoggedInPanel.PerformLayout();
            this.SuccessPanel.ResumeLayout(false);
            this.SuccessPanel.PerformLayout();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox1)).EndInit();
            ((System.ComponentModel.ISupportInitialize)(this.pictureBox2)).EndInit();
            this.ResumeLayout(false);
            this.PerformLayout();

        }
        internal System.Windows.Forms.Label StatusMessage;
        internal System.Windows.Forms.Label Label1;
        internal System.Windows.Forms.Panel NoLicensePanel;
        internal System.Windows.Forms.Panel LoadingPanel;
        private System.Windows.Forms.FlowLayoutPanel flowLayoutPanel1;
        private System.Windows.Forms.Panel notLoggedInPanel;
        internal System.Windows.Forms.Label label3;
        internal System.Windows.Forms.Label label4;
        internal System.Windows.Forms.LinkLabel linkLabel2;
        private System.Windows.Forms.Panel SuccessPanel;
        private System.Windows.Forms.Label label5;
        private System.Windows.Forms.PictureBox pictureBox1;
        private System.Windows.Forms.PictureBox pictureBox2;
        private System.Windows.Forms.Label label6;
        private System.Windows.Forms.Label label10;
        private System.Windows.Forms.Label label9;
        private System.Windows.Forms.Label label8;
        private System.Windows.Forms.Label label7;
        private System.Windows.Forms.Button button1;
        private System.Windows.Forms.Button button3;
        private System.Windows.Forms.Button button2;
    }
}