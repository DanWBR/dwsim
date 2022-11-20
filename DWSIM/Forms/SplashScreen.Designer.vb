<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class SplashScreen
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(SplashScreen))
        Me.lblVersion = New System.Windows.Forms.Label()
        Me.LabelLicense = New System.Windows.Forms.Label()
        Me.lblCopyright = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.lblPatrons = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'lblVersion
        '
        resources.ApplyResources(Me.lblVersion, "lblVersion")
        Me.lblVersion.BackColor = System.Drawing.Color.Transparent
        Me.lblVersion.ForeColor = System.Drawing.Color.FromArgb(CType(CType(13, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(166, Byte), Integer))
        Me.lblVersion.Name = "lblVersion"
        '
        'LabelLicense
        '
        resources.ApplyResources(Me.LabelLicense, "LabelLicense")
        Me.LabelLicense.BackColor = System.Drawing.Color.Transparent
        Me.LabelLicense.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.LabelLicense.ForeColor = System.Drawing.Color.FromArgb(CType(CType(13, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(166, Byte), Integer))
        Me.LabelLicense.Name = "LabelLicense"
        '
        'lblCopyright
        '
        resources.ApplyResources(Me.lblCopyright, "lblCopyright")
        Me.lblCopyright.BackColor = System.Drawing.Color.Transparent
        Me.lblCopyright.ForeColor = System.Drawing.Color.FromArgb(CType(CType(13, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(166, Byte), Integer))
        Me.lblCopyright.Name = "lblCopyright"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.BackColor = System.Drawing.Color.Transparent
        Me.Label2.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        Me.Label2.ForeColor = System.Drawing.Color.FromArgb(CType(CType(13, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(166, Byte), Integer))
        Me.Label2.Name = "Label2"
        '
        'lblPatrons
        '
        resources.ApplyResources(Me.lblPatrons, "lblPatrons")
        Me.lblPatrons.BackColor = System.Drawing.Color.Transparent
        Me.lblPatrons.ForeColor = System.Drawing.Color.FromArgb(CType(CType(13, Byte), Integer), CType(CType(114, Byte), Integer), CType(CType(166, Byte), Integer))
        Me.lblPatrons.Name = "lblPatrons"
        '
        'SplashScreen
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ControlBox = False
        Me.Controls.Add(Me.lblPatrons)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.lblCopyright)
        Me.Controls.Add(Me.LabelLicense)
        Me.Controls.Add(Me.lblVersion)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.None
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "SplashScreen"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents lblVersion As System.Windows.Forms.Label
    Public WithEvents LabelLicense As System.Windows.Forms.Label
    Public WithEvents lblCopyright As System.Windows.Forms.Label
    Public WithEvents Label2 As Label
    Public WithEvents lblPatrons As Label
End Class
