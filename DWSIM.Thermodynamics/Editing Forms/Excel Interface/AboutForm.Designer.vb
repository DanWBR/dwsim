<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class AboutForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(AboutForm))
        Me.Copyright = New System.Windows.Forms.Label()
        Me.Version = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.lblGPGPUinfo = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.Lblcpusimd = New System.Windows.Forms.Label()
        Me.Lblcpuinfo = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel()
        Me.LblCLRInfo = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.LabelLicense = New System.Windows.Forms.Label()
        Me.LblOSInfo = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Button1 = New System.Windows.Forms.Button()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Copyright
        '
        Me.Copyright.AutoSize = True
        Me.Copyright.BackColor = System.Drawing.Color.Transparent
        Me.Copyright.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Copyright.Location = New System.Drawing.Point(76, 57)
        Me.Copyright.Name = "Copyright"
        Me.Copyright.Size = New System.Drawing.Size(54, 13)
        Me.Copyright.TabIndex = 26
        Me.Copyright.Text = "Copyright "
        '
        'Version
        '
        Me.Version.AutoSize = True
        Me.Version.BackColor = System.Drawing.Color.Transparent
        Me.Version.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Version.Location = New System.Drawing.Point(76, 36)
        Me.Version.Name = "Version"
        Me.Version.Size = New System.Drawing.Size(121, 13)
        Me.Version.TabIndex = 25
        Me.Version.Text = "Version X, Build Y (date)"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold)
        Me.Label1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label1.Location = New System.Drawing.Point(76, 15)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(404, 13)
        Me.Label1.TabIndex = 24
        Me.Label1.Text = "DWSIM - Open Source Process Simulation, Modeling and Optimization"
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.DWSIM4_icon
        Me.PictureBox1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.PictureBox1.Location = New System.Drawing.Point(12, 12)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(57, 60)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 27
        Me.PictureBox1.TabStop = False
        '
        'lblGPGPUinfo
        '
        Me.lblGPGPUinfo.BackColor = System.Drawing.Color.Transparent
        Me.lblGPGPUinfo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblGPGPUinfo.Location = New System.Drawing.Point(136, 258)
        Me.lblGPGPUinfo.Name = "lblGPGPUinfo"
        Me.lblGPGPUinfo.Size = New System.Drawing.Size(608, 33)
        Me.lblGPGPUinfo.TabIndex = 52
        Me.lblGPGPUinfo.Text = "[gpgpuinfo]"
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.BackColor = System.Drawing.Color.Transparent
        Me.Label14.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label14.Location = New System.Drawing.Point(16, 258)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(100, 13)
        Me.Label14.TabIndex = 51
        Me.Label14.Text = "Computing devices:"
        '
        'Lblcpusimd
        '
        Me.Lblcpusimd.BackColor = System.Drawing.Color.Transparent
        Me.Lblcpusimd.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Lblcpusimd.Location = New System.Drawing.Point(136, 216)
        Me.Lblcpusimd.Name = "Lblcpusimd"
        Me.Lblcpusimd.Size = New System.Drawing.Size(608, 33)
        Me.Lblcpusimd.TabIndex = 50
        Me.Lblcpusimd.Text = "[meminfo]"
        '
        'Lblcpuinfo
        '
        Me.Lblcpuinfo.BackColor = System.Drawing.Color.Transparent
        Me.Lblcpuinfo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Lblcpuinfo.Location = New System.Drawing.Point(136, 189)
        Me.Lblcpuinfo.Name = "Lblcpuinfo"
        Me.Lblcpuinfo.Size = New System.Drawing.Size(608, 20)
        Me.Lblcpuinfo.TabIndex = 49
        Me.Lblcpuinfo.Text = "[meminfo]"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.BackColor = System.Drawing.Color.Transparent
        Me.Label13.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label13.Location = New System.Drawing.Point(16, 216)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(116, 13)
        Me.Label13.TabIndex = 48
        Me.Label13.Text = "CPU SIMD Extensions:"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.BackColor = System.Drawing.Color.Transparent
        Me.Label12.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label12.Location = New System.Drawing.Point(16, 189)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(53, 13)
        Me.Label12.TabIndex = 47
        Me.Label12.Text = "CPU Info:"
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.BackColor = System.Drawing.Color.Transparent
        Me.Label9.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label9.Location = New System.Drawing.Point(136, 81)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(253, 13)
        Me.Label9.TabIndex = 44
        Me.Label9.Text = "Daniel Wagner, Gustavo León  and Gregor Reichert"
        '
        'LinkLabel1
        '
        Me.LinkLabel1.AutoSize = True
        Me.LinkLabel1.BackColor = System.Drawing.Color.Transparent
        Me.LinkLabel1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LinkLabel1.Location = New System.Drawing.Point(136, 108)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(144, 13)
        Me.LinkLabel1.TabIndex = 30
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "http://dwsim.inforside.com.br"
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.BackColor = System.Drawing.Color.Transparent
        Me.Label10.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label10.Location = New System.Drawing.Point(16, 81)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(64, 13)
        Me.Label10.TabIndex = 43
        Me.Label10.Text = "Developers:"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(16, 108)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(54, 13)
        Me.Label2.TabIndex = 31
        Me.Label2.Text = "Websites:"
        '
        'LinkLabel2
        '
        Me.LinkLabel2.AutoSize = True
        Me.LinkLabel2.BackColor = System.Drawing.Color.Transparent
        Me.LinkLabel2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LinkLabel2.Location = New System.Drawing.Point(292, 108)
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.Size = New System.Drawing.Size(215, 13)
        Me.LinkLabel2.TabIndex = 32
        Me.LinkLabel2.TabStop = True
        Me.LinkLabel2.Text = "http://www.sourceforge.net/projects/dwsim"
        '
        'LblCLRInfo
        '
        Me.LblCLRInfo.AutoSize = True
        Me.LblCLRInfo.BackColor = System.Drawing.Color.Transparent
        Me.LblCLRInfo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LblCLRInfo.Location = New System.Drawing.Point(136, 162)
        Me.LblCLRInfo.Name = "LblCLRInfo"
        Me.LblCLRInfo.Size = New System.Drawing.Size(41, 13)
        Me.LblCLRInfo.TabIndex = 40
        Me.LblCLRInfo.Text = "[clrinfo]"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.BackColor = System.Drawing.Color.Transparent
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(16, 162)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(52, 13)
        Me.Label4.TabIndex = 39
        Me.Label4.Text = "CLR Info:"
        '
        'LabelLicense
        '
        Me.LabelLicense.AutoSize = True
        Me.LabelLicense.BackColor = System.Drawing.Color.Transparent
        Me.LabelLicense.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.LabelLicense.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        Me.LabelLicense.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LabelLicense.Location = New System.Drawing.Point(16, 301)
        Me.LabelLicense.Name = "LabelLicense"
        Me.LabelLicense.Size = New System.Drawing.Size(422, 13)
        Me.LabelLicense.TabIndex = 35
        Me.LabelLicense.Text = "DWSIM is released under the terms of the GNU General Public License (GPL) version" & _
    " 3:"
        '
        'LblOSInfo
        '
        Me.LblOSInfo.AutoSize = True
        Me.LblOSInfo.BackColor = System.Drawing.Color.Transparent
        Me.LblOSInfo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.LblOSInfo.Location = New System.Drawing.Point(136, 135)
        Me.LblOSInfo.Name = "LblOSInfo"
        Me.LblOSInfo.Size = New System.Drawing.Size(41, 13)
        Me.LblOSInfo.TabIndex = 38
        Me.LblOSInfo.Text = "[osinfo]"
        '
        'TextBox1
        '
        Me.TextBox1.BackColor = System.Drawing.Color.White
        Me.TextBox1.Font = New System.Drawing.Font("Courier New", 8.25!)
        Me.TextBox1.Location = New System.Drawing.Point(18, 322)
        Me.TextBox1.Multiline = True
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.ReadOnly = True
        Me.TextBox1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.TextBox1.Size = New System.Drawing.Size(726, 186)
        Me.TextBox1.TabIndex = 36
        Me.TextBox1.Text = resources.GetString("TextBox1.Text")
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.BackColor = System.Drawing.Color.Transparent
        Me.Label7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label7.Location = New System.Drawing.Point(16, 135)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(46, 13)
        Me.Label7.TabIndex = 37
        Me.Label7.Text = "OS Info:"
        '
        'Button1
        '
        Me.Button1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Button1.Location = New System.Drawing.Point(669, 514)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 23)
        Me.Button1.TabIndex = 53
        Me.Button1.Text = "OK"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'AboutForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(756, 544)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.lblGPGPUinfo)
        Me.Controls.Add(Me.Label14)
        Me.Controls.Add(Me.Lblcpusimd)
        Me.Controls.Add(Me.Lblcpuinfo)
        Me.Controls.Add(Me.Label13)
        Me.Controls.Add(Me.Label12)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.LinkLabel1)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.LinkLabel2)
        Me.Controls.Add(Me.LblCLRInfo)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.LabelLicense)
        Me.Controls.Add(Me.LblOSInfo)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Copyright)
        Me.Controls.Add(Me.Version)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "AboutForm"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "About DWSIM Excel Add-In"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Public WithEvents Copyright As System.Windows.Forms.Label
    Public WithEvents Version As System.Windows.Forms.Label
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents lblGPGPUinfo As System.Windows.Forms.Label
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents Lblcpusimd As System.Windows.Forms.Label
    Public WithEvents Lblcpuinfo As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents LinkLabel1 As System.Windows.Forms.LinkLabel
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents LinkLabel2 As System.Windows.Forms.LinkLabel
    Public WithEvents LblCLRInfo As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents LabelLicense As System.Windows.Forms.Label
    Public WithEvents LblOSInfo As System.Windows.Forms.Label
    Public WithEvents TextBox1 As System.Windows.Forms.TextBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Button1 As System.Windows.Forms.Button
End Class
