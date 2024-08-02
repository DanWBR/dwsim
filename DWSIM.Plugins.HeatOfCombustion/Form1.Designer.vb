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
        Me.tbMassHHV = New System.Windows.Forms.TextBox()
        Me.tbMolarHHV = New System.Windows.Forms.TextBox()
        Me.lblUnits = New System.Windows.Forms.Label()
        Me.lblUnits2 = New System.Windows.Forms.Label()
        Me.tbMassLHV = New System.Windows.Forms.TextBox()
        Me.tbMolarLHV = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.Plugins.HeatOfCombustion.My.Resources.Resources.icons8_charcoal
        Me.PictureBox1.Location = New System.Drawing.Point(2, 2)
        Me.PictureBox1.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(236, 252)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(254, 22)
        Me.Label1.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(163, 25)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Material Stream"
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(254, 80)
        Me.Label2.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(370, 25)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "Mass Heat of Combustion (HHV/LHV)"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(254, 140)
        Me.Label3.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(372, 25)
        Me.Label3.TabIndex = 3
        Me.Label3.Text = "Molar Heat of Combustion (HHV/LHV)"
        '
        'cbStream
        '
        Me.cbStream.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbStream.FormattingEnabled = True
        Me.cbStream.Location = New System.Drawing.Point(652, 16)
        Me.cbStream.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.cbStream.Name = "cbStream"
        Me.cbStream.Size = New System.Drawing.Size(366, 33)
        Me.cbStream.TabIndex = 4
        '
        'tbMassHHV
        '
        Me.tbMassHHV.Location = New System.Drawing.Point(650, 74)
        Me.tbMassHHV.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.tbMassHHV.Name = "tbMassHHV"
        Me.tbMassHHV.ReadOnly = True
        Me.tbMassHHV.Size = New System.Drawing.Size(176, 31)
        Me.tbMassHHV.TabIndex = 6
        Me.tbMassHHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbMolarHHV
        '
        Me.tbMolarHHV.Location = New System.Drawing.Point(652, 132)
        Me.tbMolarHHV.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.tbMolarHHV.Name = "tbMolarHHV"
        Me.tbMolarHHV.ReadOnly = True
        Me.tbMolarHHV.Size = New System.Drawing.Size(174, 31)
        Me.tbMolarHHV.TabIndex = 7
        Me.tbMolarHHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblUnits
        '
        Me.lblUnits.AutoSize = True
        Me.lblUnits.Location = New System.Drawing.Point(1034, 80)
        Me.lblUnits.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.lblUnits.Name = "lblUnits"
        Me.lblUnits.Size = New System.Drawing.Size(74, 25)
        Me.lblUnits.TabIndex = 8
        Me.lblUnits.Text = "UNITS"
        '
        'lblUnits2
        '
        Me.lblUnits2.AutoSize = True
        Me.lblUnits2.Location = New System.Drawing.Point(1034, 138)
        Me.lblUnits2.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.lblUnits2.Name = "lblUnits2"
        Me.lblUnits2.Size = New System.Drawing.Size(74, 25)
        Me.lblUnits2.TabIndex = 9
        Me.lblUnits2.Text = "UNITS"
        '
        'tbMassLHV
        '
        Me.tbMassLHV.Location = New System.Drawing.Point(842, 74)
        Me.tbMassLHV.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.tbMassLHV.Name = "tbMassLHV"
        Me.tbMassLHV.ReadOnly = True
        Me.tbMassLHV.Size = New System.Drawing.Size(176, 31)
        Me.tbMassLHV.TabIndex = 10
        Me.tbMassLHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbMolarLHV
        '
        Me.tbMolarLHV.Location = New System.Drawing.Point(844, 132)
        Me.tbMolarLHV.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.tbMolarLHV.Name = "tbMolarLHV"
        Me.tbMolarLHV.ReadOnly = True
        Me.tbMolarLHV.Size = New System.Drawing.Size(174, 31)
        Me.tbMolarLHV.TabIndex = 11
        Me.tbMolarLHV.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.Location = New System.Drawing.Point(254, 202)
        Me.Label4.Margin = New System.Windows.Forms.Padding(6, 0, 6, 0)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(538, 25)
        Me.Label4.TabIndex = 12
        Me.Label4.Text = "All values given at 298.15 K and 1 atm, ideal gas state."
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(192.0!, 192.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(1200, 262)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.tbMolarLHV)
        Me.Controls.Add(Me.tbMassLHV)
        Me.Controls.Add(Me.lblUnits2)
        Me.Controls.Add(Me.lblUnits)
        Me.Controls.Add(Me.tbMolarHHV)
        Me.Controls.Add(Me.tbMassHHV)
        Me.Controls.Add(Me.cbStream)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.PictureBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Margin = New System.Windows.Forms.Padding(6, 6, 6, 6)
        Me.Name = "Form1"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Hydrocarbon Heat of Combustion Calculator"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents PictureBox1 As Windows.Forms.PictureBox
    Friend WithEvents Label1 As Windows.Forms.Label
    Friend WithEvents Label2 As Windows.Forms.Label
    Friend WithEvents Label3 As Windows.Forms.Label
    Friend WithEvents cbStream As Windows.Forms.ComboBox
    Friend WithEvents tbMassHHV As Windows.Forms.TextBox
    Friend WithEvents tbMolarHHV As Windows.Forms.TextBox
    Friend WithEvents lblUnits As Windows.Forms.Label
    Friend WithEvents lblUnits2 As Windows.Forms.Label
    Friend WithEvents tbMassLHV As Windows.Forms.TextBox
    Friend WithEvents tbMolarLHV As Windows.Forms.TextBox
    Friend WithEvents Label4 As Windows.Forms.Label
End Class
