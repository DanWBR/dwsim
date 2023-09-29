namespace DWSIM.SharedClassesCSharp.ConnectionsEditor
{
    partial class ConnectionsEditor
    {
        /// <summary> 
        /// Required designer variable.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary> 
        /// Clean up any resources being used.
        /// </summary>
        /// <param name="disposing">true if managed resources should be disposed; otherwise, false.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Component Designer generated code

        /// <summary> 
        /// Required method for Designer support - do not modify 
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            this.PropertiesLayout = new System.Windows.Forms.TableLayoutPanel();
            this.SuspendLayout();
            // 
            // PropertiesLayout
            // 
            this.PropertiesLayout.AutoScroll = true;
            this.PropertiesLayout.ColumnCount = 1;
            this.PropertiesLayout.ColumnStyles.Add(new System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100F));
            this.PropertiesLayout.Dock = System.Windows.Forms.DockStyle.Fill;
            this.PropertiesLayout.Location = new System.Drawing.Point(0, 0);
            this.PropertiesLayout.Margin = new System.Windows.Forms.Padding(6);
            this.PropertiesLayout.Name = "PropertiesLayout";
            this.PropertiesLayout.Padding = new System.Windows.Forms.Padding(10);
            this.PropertiesLayout.RowCount = 1;
            this.PropertiesLayout.RowStyles.Add(new System.Windows.Forms.RowStyle());
            this.PropertiesLayout.Size = new System.Drawing.Size(368, 256);
            this.PropertiesLayout.TabIndex = 1;
            // 
            // ConnectionsEditor
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.Controls.Add(this.PropertiesLayout);
            this.Name = "ConnectionsEditor";
            this.Size = new System.Drawing.Size(368, 256);
            this.Load += new System.EventHandler(this.ConnectionsEditor_Load);
            this.ResumeLayout(false);

        }

        #endregion

        public System.Windows.Forms.TableLayoutPanel PropertiesLayout;
    }
}
