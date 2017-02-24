namespace DWSIM.Thermodynamics.AdvancedEOS.EditingForms
{
    partial class PHSC_Editor
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

        #region Windows Form Designer generated code

        /// <summary>
        /// Required method for Designer support - do not modify
        /// the contents of this method with the code editor.
        /// </summary>
        private void InitializeComponent()
        {
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle24 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle19 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle20 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle21 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle22 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle23 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle27 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle25 = new System.Windows.Forms.DataGridViewCellStyle();
            System.Windows.Forms.DataGridViewCellStyle dataGridViewCellStyle26 = new System.Windows.Forms.DataGridViewCellStyle();
            this.tabControl1 = new System.Windows.Forms.TabControl();
            this.tabPage2 = new System.Windows.Forms.TabPage();
            this.dgvparams = new System.Windows.Forms.DataGridView();
            this.Column3 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column4 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column5 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column6 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.Column7 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.tabPage1 = new System.Windows.Forms.TabPage();
            this.dgvkij = new System.Windows.Forms.DataGridView();
            this.DataGridViewTextBoxColumn1 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DataGridViewTextBoxColumn2 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.DataGridViewTextBoxColumn3 = new System.Windows.Forms.DataGridViewTextBoxColumn();
            this.tabPage3 = new System.Windows.Forms.TabPage();
            this.chkUseLK = new System.Windows.Forms.CheckBox();
            this.tabControl1.SuspendLayout();
            this.tabPage2.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgvparams)).BeginInit();
            this.tabPage1.SuspendLayout();
            ((System.ComponentModel.ISupportInitialize)(this.dgvkij)).BeginInit();
            this.tabPage3.SuspendLayout();
            this.SuspendLayout();
            // 
            // tabControl1
            // 
            this.tabControl1.Controls.Add(this.tabPage2);
            this.tabControl1.Controls.Add(this.tabPage1);
            this.tabControl1.Controls.Add(this.tabPage3);
            this.tabControl1.Dock = System.Windows.Forms.DockStyle.Fill;
            this.tabControl1.Location = new System.Drawing.Point(0, 0);
            this.tabControl1.Name = "tabControl1";
            this.tabControl1.SelectedIndex = 0;
            this.tabControl1.Size = new System.Drawing.Size(869, 391);
            this.tabControl1.TabIndex = 1;
            // 
            // tabPage2
            // 
            this.tabPage2.Controls.Add(this.dgvparams);
            this.tabPage2.Location = new System.Drawing.Point(4, 22);
            this.tabPage2.Name = "tabPage2";
            this.tabPage2.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage2.Size = new System.Drawing.Size(861, 365);
            this.tabPage2.TabIndex = 1;
            this.tabPage2.Text = "Compound Parameters";
            this.tabPage2.UseVisualStyleBackColor = true;
            // 
            // dgvparams
            // 
            this.dgvparams.AllowUserToAddRows = false;
            this.dgvparams.AllowUserToDeleteRows = false;
            this.dgvparams.AllowUserToResizeColumns = false;
            this.dgvparams.AllowUserToResizeRows = false;
            this.dgvparams.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.dgvparams.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            this.dgvparams.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.Column3,
            this.Column4,
            this.Column5,
            this.Column6,
            this.Column7});
            this.dgvparams.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dgvparams.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter;
            this.dgvparams.Location = new System.Drawing.Point(3, 3);
            this.dgvparams.MultiSelect = false;
            this.dgvparams.Name = "dgvparams";
            this.dgvparams.RowHeadersVisible = false;
            this.dgvparams.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders;
            dataGridViewCellStyle24.Format = "N5";
            this.dgvparams.RowsDefaultCellStyle = dataGridViewCellStyle24;
            this.dgvparams.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.dgvparams.Size = new System.Drawing.Size(855, 359);
            this.dgvparams.TabIndex = 5;
            this.dgvparams.CellEndEdit += new System.Windows.Forms.DataGridViewCellEventHandler(this.dgvparams_CellEndEdit);
            // 
            // Column3
            // 
            dataGridViewCellStyle19.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle19.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
            this.Column3.DefaultCellStyle = dataGridViewCellStyle19;
            this.Column3.HeaderText = "Compound";
            this.Column3.Name = "Column3";
            this.Column3.ReadOnly = true;
            // 
            // Column4
            // 
            dataGridViewCellStyle20.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft;
            dataGridViewCellStyle20.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
            this.Column4.DefaultCellStyle = dataGridViewCellStyle20;
            this.Column4.HeaderText = "CAS ID";
            this.Column4.Name = "Column4";
            this.Column4.ReadOnly = true;
            // 
            // Column5
            // 
            dataGridViewCellStyle21.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            this.Column5.DefaultCellStyle = dataGridViewCellStyle21;
            this.Column5.HeaderText = "Volume (m³/mol)";
            this.Column5.Name = "Column5";
            this.Column5.ToolTipText = "Volume (m³/mol)";
            // 
            // Column6
            // 
            dataGridViewCellStyle22.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            this.Column6.DefaultCellStyle = dataGridViewCellStyle22;
            this.Column6.HeaderText = "Surface Area (m²/mol)";
            this.Column6.Name = "Column6";
            this.Column6.ToolTipText = "Surface Area (m²/mol)";
            // 
            // Column7
            // 
            dataGridViewCellStyle23.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            this.Column7.DefaultCellStyle = dataGridViewCellStyle23;
            this.Column7.HeaderText = "Cohesive Energy (J/mol)";
            this.Column7.Name = "Column7";
            this.Column7.ToolTipText = "Cohesive Energy (J/mol)";
            // 
            // tabPage1
            // 
            this.tabPage1.Controls.Add(this.dgvkij);
            this.tabPage1.Location = new System.Drawing.Point(4, 22);
            this.tabPage1.Name = "tabPage1";
            this.tabPage1.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage1.Size = new System.Drawing.Size(861, 365);
            this.tabPage1.TabIndex = 0;
            this.tabPage1.Text = "Interaction Parameters";
            this.tabPage1.UseVisualStyleBackColor = true;
            // 
            // dgvkij
            // 
            this.dgvkij.AllowUserToAddRows = false;
            this.dgvkij.AllowUserToDeleteRows = false;
            this.dgvkij.AllowUserToResizeColumns = false;
            this.dgvkij.AllowUserToResizeRows = false;
            this.dgvkij.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill;
            this.dgvkij.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing;
            this.dgvkij.Columns.AddRange(new System.Windows.Forms.DataGridViewColumn[] {
            this.DataGridViewTextBoxColumn1,
            this.DataGridViewTextBoxColumn2,
            this.DataGridViewTextBoxColumn3});
            this.dgvkij.Dock = System.Windows.Forms.DockStyle.Fill;
            this.dgvkij.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter;
            this.dgvkij.Location = new System.Drawing.Point(3, 3);
            this.dgvkij.MultiSelect = false;
            this.dgvkij.Name = "dgvkij";
            this.dgvkij.RowHeadersVisible = false;
            this.dgvkij.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders;
            dataGridViewCellStyle27.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight;
            dataGridViewCellStyle27.Format = "N5";
            this.dgvkij.RowsDefaultCellStyle = dataGridViewCellStyle27;
            this.dgvkij.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect;
            this.dgvkij.Size = new System.Drawing.Size(855, 359);
            this.dgvkij.TabIndex = 7;
            this.dgvkij.CellEndEdit += new System.Windows.Forms.DataGridViewCellEventHandler(this.dgvkij_CellEndEdit);
            // 
            // DataGridViewTextBoxColumn1
            // 
            dataGridViewCellStyle25.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
            this.DataGridViewTextBoxColumn1.DefaultCellStyle = dataGridViewCellStyle25;
            this.DataGridViewTextBoxColumn1.HeaderText = "Compound 1";
            this.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1";
            this.DataGridViewTextBoxColumn1.ReadOnly = true;
            // 
            // DataGridViewTextBoxColumn2
            // 
            dataGridViewCellStyle26.BackColor = System.Drawing.Color.FromArgb(((int)(((byte)(224)))), ((int)(((byte)(224)))), ((int)(((byte)(224)))));
            this.DataGridViewTextBoxColumn2.DefaultCellStyle = dataGridViewCellStyle26;
            this.DataGridViewTextBoxColumn2.HeaderText = "Compound 2";
            this.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2";
            this.DataGridViewTextBoxColumn2.ReadOnly = true;
            // 
            // DataGridViewTextBoxColumn3
            // 
            this.DataGridViewTextBoxColumn3.HeaderText = "kij";
            this.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3";
            // 
            // tabPage3
            // 
            this.tabPage3.Controls.Add(this.chkUseLK);
            this.tabPage3.Location = new System.Drawing.Point(4, 22);
            this.tabPage3.Name = "tabPage3";
            this.tabPage3.Padding = new System.Windows.Forms.Padding(3);
            this.tabPage3.Size = new System.Drawing.Size(861, 365);
            this.tabPage3.TabIndex = 2;
            this.tabPage3.Text = "General Options";
            this.tabPage3.UseVisualStyleBackColor = true;
            // 
            // chkUseLK
            // 
            this.chkUseLK.AutoSize = true;
            this.chkUseLK.Location = new System.Drawing.Point(18, 22);
            this.chkUseLK.Name = "chkUseLK";
            this.chkUseLK.Size = new System.Drawing.Size(364, 17);
            this.chkUseLK.TabIndex = 1;
            this.chkUseLK.Text = "Use Lee-Kesler model to calculate Enthalpy, Entropy and Heat Capacity";
            this.chkUseLK.UseVisualStyleBackColor = true;
            this.chkUseLK.CheckedChanged += new System.EventHandler(this.chkUseLK_CheckedChanged);
            // 
            // PHSC_Editor
            // 
            this.AutoScaleDimensions = new System.Drawing.SizeF(6F, 13F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.ClientSize = new System.Drawing.Size(869, 391);
            this.Controls.Add(this.tabControl1);
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow;
            this.Name = "PHSC_Editor";
            this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
            this.Text = "Edit Perturbed Hard-Sphere Chain Property Package";
            this.Load += new System.EventHandler(this.PHSC_Editor_Load);
            this.tabControl1.ResumeLayout(false);
            this.tabPage2.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dgvparams)).EndInit();
            this.tabPage1.ResumeLayout(false);
            ((System.ComponentModel.ISupportInitialize)(this.dgvkij)).EndInit();
            this.tabPage3.ResumeLayout(false);
            this.tabPage3.PerformLayout();
            this.ResumeLayout(false);

        }

        #endregion

        private System.Windows.Forms.TabControl tabControl1;
        private System.Windows.Forms.TabPage tabPage2;
        public System.Windows.Forms.DataGridView dgvparams;
        private System.Windows.Forms.TabPage tabPage1;
        public System.Windows.Forms.DataGridView dgvkij;
        private System.Windows.Forms.DataGridViewTextBoxColumn DataGridViewTextBoxColumn1;
        private System.Windows.Forms.DataGridViewTextBoxColumn DataGridViewTextBoxColumn2;
        private System.Windows.Forms.DataGridViewTextBoxColumn DataGridViewTextBoxColumn3;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column3;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column4;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column5;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column6;
        private System.Windows.Forms.DataGridViewTextBoxColumn Column7;
        private System.Windows.Forms.TabPage tabPage3;
        private System.Windows.Forms.CheckBox chkUseLK;
    }
}