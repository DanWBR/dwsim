<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormPortal
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormPortal))
        Me.ProgressBar1 = New System.Windows.Forms.ProgressBar()
        Me.StatusMessage = New System.Windows.Forms.Label()
        Me.LinkLabel1 = New System.Windows.Forms.LinkLabel()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.NoLicensePanel = New System.Windows.Forms.Panel()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.LoadingPanel = New System.Windows.Forms.Panel()
        Me.NoLicensePanel.SuspendLayout()
        Me.LoadingPanel.SuspendLayout()
        Me.SuspendLayout()
        '
        'ProgressBar1
        '
        Me.ProgressBar1.Location = New System.Drawing.Point(6, 15)
        Me.ProgressBar1.Name = "ProgressBar1"
        Me.ProgressBar1.Size = New System.Drawing.Size(451, 23)
        Me.ProgressBar1.TabIndex = 0
        '
        'StatusMessage
        '
        Me.StatusMessage.AutoSize = True
        Me.StatusMessage.Location = New System.Drawing.Point(152, 41)
        Me.StatusMessage.Name = "StatusMessage"
        Me.StatusMessage.Size = New System.Drawing.Size(123, 13)
        Me.StatusMessage.TabIndex = 1
        Me.StatusMessage.Text = "This is a progress label..."
        '
        'LinkLabel1
        '
        Me.LinkLabel1.AutoSize = True
        Me.LinkLabel1.Location = New System.Drawing.Point(289, 13)
        Me.LinkLabel1.Name = "LinkLabel1"
        Me.LinkLabel1.Size = New System.Drawing.Size(32, 13)
        Me.LinkLabel1.TabIndex = 2
        Me.LinkLabel1.TabStop = True
        Me.LinkLabel1.Text = "Shop"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(3, 13)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(290, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "You don't have an active DWSIM Pro license. Please go to "
        '
        'NoLicensePanel
        '
        Me.NoLicensePanel.Controls.Add(Me.Label2)
        Me.NoLicensePanel.Controls.Add(Me.Label1)
        Me.NoLicensePanel.Controls.Add(Me.LinkLabel1)
        Me.NoLicensePanel.Location = New System.Drawing.Point(172, 196)
        Me.NoLicensePanel.Name = "NoLicensePanel"
        Me.NoLicensePanel.Size = New System.Drawing.Size(468, 47)
        Me.NoLicensePanel.TabIndex = 4
        Me.NoLicensePanel.Visible = False
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(321, 14)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(82, 13)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "to get a license."
        '
        'LoadingPanel
        '
        Me.LoadingPanel.Controls.Add(Me.ProgressBar1)
        Me.LoadingPanel.Controls.Add(Me.StatusMessage)
        Me.LoadingPanel.Location = New System.Drawing.Point(172, 97)
        Me.LoadingPanel.Name = "LoadingPanel"
        Me.LoadingPanel.Size = New System.Drawing.Size(475, 77)
        Me.LoadingPanel.TabIndex = 5
        '
        'FormPortal
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(800, 450)
        Me.Controls.Add(Me.LoadingPanel)
        Me.Controls.Add(Me.NoLicensePanel)
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormPortal"
        Me.Text = "FormPortal"
        Me.NoLicensePanel.ResumeLayout(False)
        Me.NoLicensePanel.PerformLayout()
        Me.LoadingPanel.ResumeLayout(False)
        Me.LoadingPanel.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents ProgressBar1 As Windows.Forms.ProgressBar
    Friend WithEvents StatusMessage As Windows.Forms.Label
    Friend WithEvents LinkLabel1 As Windows.Forms.LinkLabel
    Friend WithEvents Label1 As Windows.Forms.Label
    Friend WithEvents NoLicensePanel As Windows.Forms.Panel
    Friend WithEvents LoadingPanel As Windows.Forms.Panel
    Friend WithEvents Label2 As Windows.Forms.Label
End Class
