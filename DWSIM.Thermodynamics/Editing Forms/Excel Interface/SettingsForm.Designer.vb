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
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.PictureBox3 = New System.Windows.Forms.PictureBox()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.tbGPUCaps = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.chkEnableSIMD = New System.Windows.Forms.CheckBox()
        Me.chkEnableParallelCalcs = New System.Windows.Forms.CheckBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbGPU = New System.Windows.Forms.ComboBox()
        Me.cbParallelism = New System.Windows.Forms.ComboBox()
        Me.chkEnableGPUProcessing = New System.Windows.Forms.CheckBox()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.cbErrorHandlingMode = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbConfigFilePath = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox8.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage3)
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
        Me.TabPage1.Controls.Add(Me.PictureBox3)
        Me.TabPage1.Controls.Add(Me.PictureBox2)
        Me.TabPage1.Controls.Add(Me.PictureBox1)
        Me.TabPage1.Controls.Add(Me.GroupBox8)
        Me.TabPage1.Controls.Add(Me.Label4)
        Me.TabPage1.Controls.Add(Me.chkEnableSIMD)
        Me.TabPage1.Controls.Add(Me.chkEnableParallelCalcs)
        Me.TabPage1.Controls.Add(Me.Label6)
        Me.TabPage1.Controls.Add(Me.cbGPU)
        Me.TabPage1.Controls.Add(Me.cbParallelism)
        Me.TabPage1.Controls.Add(Me.chkEnableGPUProcessing)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(588, 272)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Calculation Engine"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'PictureBox3
        '
        Me.PictureBox3.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.information
        Me.PictureBox3.Location = New System.Drawing.Point(247, 75)
        Me.PictureBox3.Name = "PictureBox3"
        Me.PictureBox3.Size = New System.Drawing.Size(16, 16)
        Me.PictureBox3.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox3.TabIndex = 11
        Me.PictureBox3.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox3, "Enables acceleration of certain parallel calculation routines by running them on " & _
        "a GPGPU device.")
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.information
        Me.PictureBox2.Location = New System.Drawing.Point(195, 45)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(16, 16)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
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
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox1.TabIndex = 9
        Me.PictureBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.PictureBox1, "Enables parallel CPU calculations. Maximum Parallelism is the number of processor" & _
        " cores to be used at the same time.")
        '
        'GroupBox8
        '
        Me.GroupBox8.Controls.Add(Me.tbGPUCaps)
        Me.GroupBox8.Location = New System.Drawing.Point(12, 108)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.Size = New System.Drawing.Size(563, 151)
        Me.GroupBox8.TabIndex = 6
        Me.GroupBox8.TabStop = False
        Me.GroupBox8.Text = "OpenCL/CUDA device info"
        '
        'tbGPUCaps
        '
        Me.tbGPUCaps.Dock = System.Windows.Forms.DockStyle.Fill
        Me.tbGPUCaps.Font = New System.Drawing.Font("Consolas", 8.25!)
        Me.tbGPUCaps.Location = New System.Drawing.Point(3, 16)
        Me.tbGPUCaps.Multiline = True
        Me.tbGPUCaps.Name = "tbGPUCaps"
        Me.tbGPUCaps.ReadOnly = True
        Me.tbGPUCaps.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.tbGPUCaps.Size = New System.Drawing.Size(557, 132)
        Me.tbGPUCaps.TabIndex = 0
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(271, 75)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(82, 13)
        Me.Label4.TabIndex = 8
        Me.Label4.Text = "GPGPU Device"
        '
        'chkEnableSIMD
        '
        Me.chkEnableSIMD.AutoSize = True
        Me.chkEnableSIMD.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkEnableSIMD.Location = New System.Drawing.Point(13, 45)
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
        Me.Label6.Location = New System.Drawing.Point(217, 17)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(103, 13)
        Me.Label6.TabIndex = 1
        Me.Label6.Text = "Maximum Parallelism"
        '
        'cbGPU
        '
        Me.cbGPU.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbGPU.FormattingEnabled = True
        Me.cbGPU.Location = New System.Drawing.Point(359, 70)
        Me.cbGPU.Name = "cbGPU"
        Me.cbGPU.Size = New System.Drawing.Size(216, 21)
        Me.cbGPU.TabIndex = 5
        '
        'cbParallelism
        '
        Me.cbParallelism.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbParallelism.FormattingEnabled = True
        Me.cbParallelism.Location = New System.Drawing.Point(331, 14)
        Me.cbParallelism.Name = "cbParallelism"
        Me.cbParallelism.Size = New System.Drawing.Size(117, 21)
        Me.cbParallelism.TabIndex = 2
        '
        'chkEnableGPUProcessing
        '
        Me.chkEnableGPUProcessing.AutoSize = True
        Me.chkEnableGPUProcessing.Enabled = False
        Me.chkEnableGPUProcessing.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkEnableGPUProcessing.Location = New System.Drawing.Point(13, 74)
        Me.chkEnableGPUProcessing.Name = "chkEnableGPUProcessing"
        Me.chkEnableGPUProcessing.Size = New System.Drawing.Size(228, 17)
        Me.chkEnableGPUProcessing.TabIndex = 3
        Me.chkEnableGPUProcessing.Text = "Enable OpenCL/CUDA Parallel Processing"
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
        'TabPage4
        '
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
        Me.Label2.Location = New System.Drawing.Point(11, 74)
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
        Me.Label1.Location = New System.Drawing.Point(11, 31)
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
        'SettingsForm
        '
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Inherit
        Me.ClientSize = New System.Drawing.Size(596, 298)
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "SettingsForm"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "DWSIM Excel Add-In Settings"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.TabPage4.PerformLayout()
        Me.ResumeLayout(False)

End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Friend WithEvents tbGPUCaps As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents chkEnableSIMD As System.Windows.Forms.CheckBox
    Public WithEvents chkEnableParallelCalcs As System.Windows.Forms.CheckBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cbGPU As System.Windows.Forms.ComboBox
    Friend WithEvents cbParallelism As System.Windows.Forms.ComboBox
    Public WithEvents chkEnableGPUProcessing As System.Windows.Forms.CheckBox
    Friend WithEvents cbErrorHandlingMode As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbConfigFilePath As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents PictureBox3 As System.Windows.Forms.PictureBox
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents PictureBox2 As System.Windows.Forms.PictureBox
End Class
