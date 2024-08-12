<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class Form1
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
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbStream = New System.Windows.Forms.ComboBox()
        Me.lblUnits = New System.Windows.Forms.Label()
        Me.lblUnits2 = New System.Windows.Forms.Label()
        Me.tbMassLHV = New System.Windows.Forms.TextBox()
        Me.tbMolarLHV = New System.Windows.Forms.TextBox()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.Plugins.HeatOfCombustion.My.Resources.Resources.icons8_charcoal
        Me.PictureBox1.Location = New System.Drawing.Point(1, 1)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(118, 126)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(127, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(80, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Material Stream"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(127, 40)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(158, 13)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "Mass Heat of Combustion (LHV)"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(127, 70)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(159, 13)
        Me.Label3.TabIndex = 3
        Me.Label3.Text = "Molar Heat of Combustion (LHV)"
        '
        'cbStream
        '
        Me.cbStream.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbStream.FormattingEnabled = True
        Me.cbStream.Location = New System.Drawing.Point(326, 8)
        Me.cbStream.Name = "cbStream"
        Me.cbStream.Size = New System.Drawing.Size(185, 21)
        Me.cbStream.TabIndex = 4
        '
        'lblUnits
        '
        Me.lblUnits.AutoSize = True
        Me.lblUnits.Location = New System.Drawing.Point(517, 40)
        Me.lblUnits.Name = "lblUnits"
        Me.lblUnits.Size = New System.Drawing.Size(40, 13)
        Me.lblUnits.TabIndex = 8
        Me.lblUnits.Text = "UNITS"
        '
        'lblUnits2
        '
        Me.lblUnits2.AutoSize = True
        Me.lblUnits2.Location = New System.Drawing.Point(517, 69)
        Me.lblUnits2.Name = "lblUnits2"
        Me.lblUnits2.Size = New System.Drawing.Size(40, 13)
        Me.lblUnits2.TabIndex = 9
        Me.lblUnits2.Text = "UNITS"
        '
        'tbMassLHV
        '
        Me.tbMassLHV.Location = New System.Drawing.Point(421, 37)
        Me.tbMassLHV.Name = "tbMassLHV"
        Me.tbMassLHV.ReadOnly = True
        Me.tbMassLHV.Size = New System.Drawing.Size(90, 20)
        Me.tbMassLHV.TabIndex = 10
        Me.tbMassLHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbMolarLHV
        '
        Me.tbMolarLHV.Location = New System.Drawing.Point(422, 66)
        Me.tbMolarLHV.Name = "tbMolarLHV"
        Me.tbMolarLHV.ReadOnly = True
        Me.tbMolarLHV.Size = New System.Drawing.Size(89, 20)
        Me.tbMolarLHV.TabIndex = 11
        Me.tbMolarLHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(600, 131)
        Me.Controls.Add(Me.tbMolarLHV)
        Me.Controls.Add(Me.tbMassLHV)
        Me.Controls.Add(Me.lblUnits2)
        Me.Controls.Add(Me.lblUnits)
        Me.Controls.Add(Me.cbStream)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.PictureBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Standard Net Heat of Combustion Calculator"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents PictureBox1 As Windows.Forms.PictureBox
    Friend WithEvents Label1 As Windows.Forms.Label
    Friend WithEvents Label2 As Windows.Forms.Label
    Friend WithEvents Label3 As Windows.Forms.Label
    Friend WithEvents cbStream As Windows.Forms.ComboBox
    Friend WithEvents lblUnits As Windows.Forms.Label
    Friend WithEvents lblUnits2 As Windows.Forms.Label
    Friend WithEvents tbMassLHV As Windows.Forms.TextBox
    Friend WithEvents tbMolarLHV As Windows.Forms.TextBox
End Class
