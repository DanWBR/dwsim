<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormPIDCPEditor
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormPIDCPEditor))
        Me.tbSP = New System.Windows.Forms.TextBox()
        Me.tbMV = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbPV = New System.Windows.Forms.TextBox()
        Me.chkAuto = New System.Windows.Forms.CheckBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.SuspendLayout()
        '
        'tbSP
        '
        Me.tbSP.Location = New System.Drawing.Point(52, 71)
        Me.tbSP.Name = "tbSP"
        Me.tbSP.Size = New System.Drawing.Size(100, 20)
        Me.tbSP.TabIndex = 0
        Me.tbSP.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'tbMV
        '
        Me.tbMV.Location = New System.Drawing.Point(52, 123)
        Me.tbMV.Name = "tbMV"
        Me.tbMV.ReadOnly = True
        Me.tbMV.Size = New System.Drawing.Size(100, 20)
        Me.tbMV.TabIndex = 1
        Me.tbMV.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(12, 75)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(21, 13)
        Me.Label1.TabIndex = 2
        Me.Label1.Text = "SP"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(12, 127)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(23, 13)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = "MV"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(12, 101)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(21, 13)
        Me.Label3.TabIndex = 5
        Me.Label3.Text = "PV"
        '
        'tbPV
        '
        Me.tbPV.Location = New System.Drawing.Point(52, 97)
        Me.tbPV.Name = "tbPV"
        Me.tbPV.ReadOnly = True
        Me.tbPV.Size = New System.Drawing.Size(100, 20)
        Me.tbPV.TabIndex = 4
        Me.tbPV.TextAlign = System.Windows.Forms.HorizontalAlignment.Center
        '
        'chkAuto
        '
        Me.chkAuto.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkAuto.BackColor = System.Drawing.Color.Green
        Me.chkAuto.Checked = True
        Me.chkAuto.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkAuto.ForeColor = System.Drawing.Color.White
        Me.chkAuto.Location = New System.Drawing.Point(12, 39)
        Me.chkAuto.Name = "chkAuto"
        Me.chkAuto.Size = New System.Drawing.Size(140, 24)
        Me.chkAuto.TabIndex = 6
        Me.chkAuto.Text = "AUTO"
        Me.chkAuto.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.chkAuto.UseVisualStyleBackColor = False
        '
        'chkActive
        '
        Me.chkActive.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkActive.BackColor = System.Drawing.Color.Green
        Me.chkActive.Checked = True
        Me.chkActive.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkActive.ForeColor = System.Drawing.Color.White
        Me.chkActive.Location = New System.Drawing.Point(12, 9)
        Me.chkActive.Name = "chkActive"
        Me.chkActive.Size = New System.Drawing.Size(140, 24)
        Me.chkActive.TabIndex = 7
        Me.chkActive.Text = "ON"
        Me.chkActive.TextAlign = System.Drawing.ContentAlignment.MiddleCenter
        Me.chkActive.UseVisualStyleBackColor = False
        '
        'FormPIDCPEditor
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(169, 154)
        Me.Controls.Add(Me.chkActive)
        Me.Controls.Add(Me.chkAuto)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.tbPV)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.tbMV)
        Me.Controls.Add(Me.tbSP)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FormPIDCPEditor"
        Me.Text = "FormPIDCPEditor"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents tbSP As TextBox
    Friend WithEvents tbMV As TextBox
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents tbPV As TextBox
    Friend WithEvents chkAuto As CheckBox
    Friend WithEvents chkActive As CheckBox
End Class
