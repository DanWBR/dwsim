<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class SettingsForm
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        Try
            If disposing AndAlso components IsNot Nothing Then
                components.Dispose()
            End If
        Finally
            MyBase.Dispose(disposing)
        End Try
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.chkEnableSIMD = New System.Windows.Forms.CheckBox()
        Me.chkEnableParallelCalcs = New System.Windows.Forms.CheckBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbParallelism = New System.Windows.Forms.ComboBox()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.dgvdb = New System.Windows.Forms.DataGridView()
        Me.Column12 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column14 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column15 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column13 = New System.Windows.Forms.DataGridViewImageColumn()
        Me.Button7 = New System.Windows.Forms.Button()
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.dgvIPDB = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewImageColumn1 = New System.Windows.Forms.DataGridViewImageColumn()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.btnSelectOctavePath = New System.Windows.Forms.Button()
        Me.tbOctavePath = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbErrorHandlingMode = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbConfigFilePath = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.FolderBrowserDialog1 = New System.Windows.Forms.FolderBrowserDialog()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage3.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        CType(Me.dgvdb, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage5.SuspendLayout()
        CType(Me.dgvIPDB, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage4.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(0, 0)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(596, 298)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.PictureBox2)
        Me.TabPage1.Controls.Add(Me.PictureBox1)
        Me.TabPage1.Controls.Add(Me.chkEnableSIMD)
        Me.TabPage1.Controls.Add(Me.chkEnableParallelCalcs)
        Me.TabPage1.Controls.Add(Me.Label6)
        Me.TabPage1.Controls.Add(Me.cbParallelism)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(588, 272)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Calculation Engine"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.information
        Me.PictureBox2.Location = New System.Drawing.Point(195, 95)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(16, 16)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox2.TabIndex = 10
        Me.PictureBox2.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox2, "Enables acceleration of math vector operations through CPU SIMD extensions.")
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.information
        Me.PictureBox1.Location = New System.Drawing.Point(195, 17)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(16, 16)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 9
        Me.PictureBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox1, "Enables parallel CPU calculations. Maximum Parallelism is the number of processor" &
        " cores to be used at the same time.")
        '
        'chkEnableSIMD
        '
        Me.chkEnableSIMD.AutoSize = True
        Me.chkEnableSIMD.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkEnableSIMD.Location = New System.Drawing.Point(13, 95)
        Me.chkEnableSIMD.Name = "chkEnableSIMD"
        Me.chkEnableSIMD.Size = New System.Drawing.Size(176, 17)
        Me.chkEnableSIMD.TabIndex = 7
        Me.chkEnableSIMD.Text = "Enable CPU SIMD Acceleration"
        '
        'chkEnableParallelCalcs
        '
        Me.chkEnableParallelCalcs.AutoSize = True
        Me.chkEnableParallelCalcs.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkEnableParallelCalcs.Location = New System.Drawing.Point(13, 16)
        Me.chkEnableParallelCalcs.Name = "chkEnableParallelCalcs"
        Me.chkEnableParallelCalcs.Size = New System.Drawing.Size(176, 17)
        Me.chkEnableParallelCalcs.TabIndex = 0
        Me.chkEnableParallelCalcs.Text = "Enable CPU Parallel Processing"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(32, 48)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(103, 13)
        Me.Label6.TabIndex = 1
        Me.Label6.Text = "Maximum Parallelism"
        '
        'cbParallelism
        '
        Me.cbParallelism.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbParallelism.FormattingEnabled = True
        Me.cbParallelism.Location = New System.Drawing.Point(146, 45)
        Me.cbParallelism.Name = "cbParallelism"
        Me.cbParallelism.Size = New System.Drawing.Size(117, 21)
        Me.cbParallelism.TabIndex = 2
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.Button1)
        Me.TabPage3.Location = New System.Drawing.Point(4, 22)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage3.Size = New System.Drawing.Size(588, 272)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Flash Settings"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.Button1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Button1.Location = New System.Drawing.Point(130, 116)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(346, 23)
        Me.Button1.TabIndex = 1
        Me.Button1.Text = "Configure Flash Algorithm Settings"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.dgvdb)
        Me.TabPage2.Controls.Add(Me.Button7)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Size = New System.Drawing.Size(588, 272)
        Me.TabPage2.TabIndex = 4
        Me.TabPage2.Text = "User Compounds"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'dgvdb
        '
        Me.dgvdb.AllowUserToAddRows = False
        Me.dgvdb.AllowUserToDeleteRows = False
        Me.dgvdb.AllowUserToResizeColumns = False
        Me.dgvdb.AllowUserToResizeRows = False
        Me.dgvdb.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvdb.BackgroundColor = System.Drawing.SystemColors.Control
        Me.dgvdb.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.dgvdb.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.SingleHorizontal
        Me.dgvdb.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvdb.ColumnHeadersVisible = False
        Me.dgvdb.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column12, Me.Column14, Me.Column15, Me.Column13})
        Me.dgvdb.Dock = System.Windows.Forms.DockStyle.Fill
        Me.dgvdb.GridColor = System.Drawing.SystemColors.Control
        Me.dgvdb.Location = New System.Drawing.Point(0, 0)
        Me.dgvdb.Name = "dgvdb"
        Me.dgvdb.ReadOnly = True
        Me.dgvdb.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.None
        Me.dgvdb.RowHeadersVisible = False
        Me.dgvdb.RowHeadersWidth = 4
        Me.dgvdb.RowTemplate.DefaultCellStyle.Padding = New System.Windows.Forms.Padding(0, 5, 0, 5)
        Me.dgvdb.RowTemplate.Height = 38
        Me.dgvdb.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.dgvdb.ShowCellErrors = False
        Me.dgvdb.Size = New System.Drawing.Size(588, 249)
        Me.dgvdb.TabIndex = 0
        '
        'Column12
        '
        Me.Column12.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.Column12.FillWeight = 10.0!
        Me.Column12.HeaderText = "ID"
        Me.Column12.Name = "Column12"
        Me.Column12.ReadOnly = True
        Me.Column12.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.Column12.Width = 5
        '
        'Column14
        '
        Me.Column14.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.Column14.FillWeight = 30.0!
        Me.Column14.HeaderText = "Database"
        Me.Column14.Name = "Column14"
        Me.Column14.ReadOnly = True
        Me.Column14.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.Column14.Width = 5
        '
        'Column15
        '
        Me.Column15.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.Column15.HeaderText = "Path"
        Me.Column15.Name = "Column15"
        Me.Column15.ReadOnly = True
        Me.Column15.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        '
        'Column13
        '
        Me.Column13.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.None
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        DataGridViewCellStyle1.NullValue = Nothing
        Me.Column13.DefaultCellStyle = DataGridViewCellStyle1
        Me.Column13.FillWeight = 10.0!
        Me.Column13.HeaderText = "Button"
        Me.Column13.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.bullet_cross
        Me.Column13.MinimumWidth = 50
        Me.Column13.Name = "Column13"
        Me.Column13.ReadOnly = True
        Me.Column13.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.Column13.Width = 50
        '
        'Button7
        '
        Me.Button7.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Button7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Button7.Location = New System.Drawing.Point(0, 249)
        Me.Button7.Name = "Button7"
        Me.Button7.Size = New System.Drawing.Size(588, 23)
        Me.Button7.TabIndex = 0
        Me.Button7.Text = "Add User Dataset"
        Me.Button7.UseVisualStyleBackColor = True
        '
        'TabPage5
        '
        Me.TabPage5.Controls.Add(Me.dgvIPDB)
        Me.TabPage5.Controls.Add(Me.Button4)
        Me.TabPage5.Location = New System.Drawing.Point(4, 22)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.Size = New System.Drawing.Size(588, 272)
        Me.TabPage5.TabIndex = 5
        Me.TabPage5.Text = "User Interaction Parameters"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'dgvIPDB
        '
        Me.dgvIPDB.AllowUserToAddRows = False
        Me.dgvIPDB.AllowUserToDeleteRows = False
        Me.dgvIPDB.AllowUserToResizeColumns = False
        Me.dgvIPDB.AllowUserToResizeRows = False
        Me.dgvIPDB.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvIPDB.BackgroundColor = System.Drawing.SystemColors.Control
        Me.dgvIPDB.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.dgvIPDB.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.SingleHorizontal
        Me.dgvIPDB.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvIPDB.ColumnHeadersVisible = False
        Me.dgvIPDB.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.DataGridViewTextBoxColumn3, Me.DataGridViewImageColumn1})
        Me.dgvIPDB.Dock = System.Windows.Forms.DockStyle.Fill
        Me.dgvIPDB.GridColor = System.Drawing.SystemColors.Control
        Me.dgvIPDB.Location = New System.Drawing.Point(0, 0)
        Me.dgvIPDB.Name = "dgvIPDB"
        Me.dgvIPDB.ReadOnly = True
        Me.dgvIPDB.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.None
        Me.dgvIPDB.RowHeadersVisible = False
        Me.dgvIPDB.RowHeadersWidth = 4
        Me.dgvIPDB.RowTemplate.DefaultCellStyle.Padding = New System.Windows.Forms.Padding(0, 5, 0, 5)
        Me.dgvIPDB.RowTemplate.Height = 38
        Me.dgvIPDB.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.dgvIPDB.ShowCellErrors = False
        Me.dgvIPDB.Size = New System.Drawing.Size(588, 249)
        Me.dgvIPDB.TabIndex = 1
        '
        'DataGridViewTextBoxColumn1
        '
        Me.DataGridViewTextBoxColumn1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.DataGridViewTextBoxColumn1.FillWeight = 10.0!
        Me.DataGridViewTextBoxColumn1.HeaderText = "ID"
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        Me.DataGridViewTextBoxColumn1.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewTextBoxColumn1.Width = 5
        '
        'DataGridViewTextBoxColumn2
        '
        Me.DataGridViewTextBoxColumn2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.DataGridViewTextBoxColumn2.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn2.HeaderText = "Database"
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        Me.DataGridViewTextBoxColumn2.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewTextBoxColumn2.Width = 5
        '
        'DataGridViewTextBoxColumn3
        '
        Me.DataGridViewTextBoxColumn3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewTextBoxColumn3.HeaderText = "Path"
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.ReadOnly = True
        Me.DataGridViewTextBoxColumn3.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        '
        'DataGridViewImageColumn1
        '
        Me.DataGridViewImageColumn1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.None
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        DataGridViewCellStyle2.NullValue = Nothing
        Me.DataGridViewImageColumn1.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridViewImageColumn1.FillWeight = 10.0!
        Me.DataGridViewImageColumn1.HeaderText = "Button"
        Me.DataGridViewImageColumn1.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.bullet_cross
        Me.DataGridViewImageColumn1.MinimumWidth = 50
        Me.DataGridViewImageColumn1.Name = "DataGridViewImageColumn1"
        Me.DataGridViewImageColumn1.ReadOnly = True
        Me.DataGridViewImageColumn1.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridViewImageColumn1.Width = 50
        '
        'Button4
        '
        Me.Button4.Dock = System.Windows.Forms.DockStyle.Bottom
        Me.Button4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Button4.Location = New System.Drawing.Point(0, 249)
        Me.Button4.Name = "Button4"
        Me.Button4.Size = New System.Drawing.Size(588, 23)
        Me.Button4.TabIndex = 1
        Me.Button4.Text = "Add User IP Dataset"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'TabPage4
        '
        Me.TabPage4.Controls.Add(Me.btnSelectOctavePath)
        Me.TabPage4.Controls.Add(Me.tbOctavePath)
        Me.TabPage4.Controls.Add(Me.Label3)
        Me.TabPage4.Controls.Add(Me.cbErrorHandlingMode)
        Me.TabPage4.Controls.Add(Me.Label2)
        Me.TabPage4.Controls.Add(Me.tbConfigFilePath)
        Me.TabPage4.Controls.Add(Me.Label1)
        Me.TabPage4.Location = New System.Drawing.Point(4, 22)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage4.Size = New System.Drawing.Size(588, 272)
        Me.TabPage4.TabIndex = 3
        Me.TabPage4.Text = "Miscellaneous"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'btnSelectOctavePath
        '
        Me.btnSelectOctavePath.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnSelectOctavePath.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnSelectOctavePath.Location = New System.Drawing.Point(503, 116)
        Me.btnSelectOctavePath.Name = "btnSelectOctavePath"
        Me.btnSelectOctavePath.Size = New System.Drawing.Size(68, 23)
        Me.btnSelectOctavePath.TabIndex = 33
        Me.btnSelectOctavePath.Text = "Select"
        Me.btnSelectOctavePath.UseVisualStyleBackColor = True
        '
        'tbOctavePath
        '
        Me.tbOctavePath.Location = New System.Drawing.Point(136, 118)
        Me.tbOctavePath.Name = "tbOctavePath"
        Me.tbOctavePath.ReadOnly = True
        Me.tbOctavePath.Size = New System.Drawing.Size(361, 20)
        Me.tbOctavePath.TabIndex = 32
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(12, 122)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(107, 13)
        Me.Label3.TabIndex = 31
        Me.Label3.Text = "Octave Binaries Path"
        '
        'cbErrorHandlingMode
        '
        Me.cbErrorHandlingMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbErrorHandlingMode.FormattingEnabled = True
        Me.cbErrorHandlingMode.Items.AddRange(New Object() {"Return Error Message to Cell's Value (Simple)", "Return Error Message to Cell's Value (Detailed)", "Show Error Handler Window"})
        Me.cbErrorHandlingMode.Location = New System.Drawing.Point(136, 70)
        Me.cbErrorHandlingMode.Name = "cbErrorHandlingMode"
        Me.cbErrorHandlingMode.Size = New System.Drawing.Size(285, 21)
        Me.cbErrorHandlingMode.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 74)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(104, 13)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "Error Handling Mode"
        '
        'tbConfigFilePath
        '
        Me.tbConfigFilePath.Location = New System.Drawing.Point(136, 28)
        Me.tbConfigFilePath.Name = "tbConfigFilePath"
        Me.tbConfigFilePath.ReadOnly = True
        Me.tbConfigFilePath.Size = New System.Drawing.Size(435, 20)
        Me.tbConfigFilePath.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 31)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(113, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Configuration File Path"
        '
        'ToolTip1
        '
        Me.ToolTip1.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTip1.ToolTipTitle = "Information"
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.Filter = "XML File|*.xml"
        Me.OpenFileDialog1.SupportMultiDottedExtensions = True
        Me.OpenFileDialog1.Title = "Add User Database"
        '
        'FolderBrowserDialog1
        '
        Me.FolderBrowserDialog1.Description = "Select a folder..."
        '
        'SettingsForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(596, 298)
        Me.Controls.Add(Me.TabControl1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "SettingsForm"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "DWSIM Excel Add-In Settings"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        CType(Me.dgvdb, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage5.ResumeLayout(False)
        CType(Me.dgvIPDB, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage4.ResumeLayout(False)
        Me.TabPage4.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Public WithEvents chkEnableSIMD As System.Windows.Forms.CheckBox
    Public WithEvents chkEnableParallelCalcs As System.Windows.Forms.CheckBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cbParallelism As System.Windows.Forms.ComboBox
    Friend WithEvents cbErrorHandlingMode As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbConfigFilePath As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents PictureBox2 As System.Windows.Forms.PictureBox
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents TabPage5 As TabPage
    Public WithEvents dgvdb As DataGridView
    Public WithEvents Button7 As Button
    Public WithEvents Button4 As Button
    Public WithEvents dgvIPDB As DataGridView
    Public WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents Column13 As DataGridViewImageColumn
    Friend WithEvents Column15 As DataGridViewTextBoxColumn
    Friend WithEvents Column14 As DataGridViewTextBoxColumn
    Friend WithEvents Column12 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewImageColumn1 As DataGridViewImageColumn
    Friend WithEvents DataGridViewTextBoxColumn3 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn1 As DataGridViewTextBoxColumn
    Public WithEvents btnSelectOctavePath As System.Windows.Forms.Button
    Friend WithEvents tbOctavePath As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents FolderBrowserDialog1 As System.Windows.Forms.FolderBrowserDialog
End Class
