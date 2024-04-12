<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormHeatMaps
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.TableLayoutPanel8 = New System.Windows.Forms.TableLayoutPanel()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.PictureBox6 = New System.Windows.Forms.PictureBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.TableLayoutPanel8.SuspendLayout()
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel8
        '
        Me.TableLayoutPanel8.ColumnCount = 1
        Me.TableLayoutPanel8.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel8.Controls.Add(Me.Label12, 0, 0)
        Me.TableLayoutPanel8.Controls.Add(Me.Label15, 0, 1)
        Me.TableLayoutPanel8.Controls.Add(Me.PictureBox6, 0, 2)
        Me.TableLayoutPanel8.Controls.Add(Me.Panel1, 0, 3)
        Me.TableLayoutPanel8.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel8.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel8.Name = "TableLayoutPanel8"
        Me.TableLayoutPanel8.RowCount = 4
        Me.TableLayoutPanel8.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 50.0!))
        Me.TableLayoutPanel8.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 57.0!))
        Me.TableLayoutPanel8.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel8.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 117.0!))
        Me.TableLayoutPanel8.Size = New System.Drawing.Size(621, 543)
        Me.TableLayoutPanel8.TabIndex = 7
        '
        'Label12
        '
        Me.Label12.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Label12.Font = New System.Drawing.Font("Segoe UI Semibold", 15.75!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label12.Location = New System.Drawing.Point(3, 0)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(615, 50)
        Me.Label12.TabIndex = 0
        Me.Label12.Text = "Heatmap Layers"
        Me.Label12.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label15
        '
        Me.Label15.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Label15.Font = New System.Drawing.Font("Segoe UI", 12.0!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label15.Location = New System.Drawing.Point(3, 50)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(615, 57)
        Me.Label15.TabIndex = 1
        Me.Label15.Text = "This feature is available in DWSIM Pro. Add a Heatmap Layer to the flowsheet to v" &
    "isualize Temperature, Pressure, Flow and Compound Concentration changes across t" &
    "he process model."
        '
        'PictureBox6
        '
        Me.PictureBox6.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PictureBox6.Image = Global.DWSIM.My.Resources.Resources.heatmaps
        Me.PictureBox6.Location = New System.Drawing.Point(3, 110)
        Me.PictureBox6.Name = "PictureBox6"
        Me.PictureBox6.Size = New System.Drawing.Size(615, 313)
        Me.PictureBox6.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox6.TabIndex = 2
        Me.PictureBox6.TabStop = False
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.Button2)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel1.Location = New System.Drawing.Point(3, 429)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(615, 111)
        Me.Panel1.TabIndex = 3
        '
        'Label4
        '
        Me.Label4.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label4.Font = New System.Drawing.Font("Microsoft Sans Serif", 9.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label4.Location = New System.Drawing.Point(378, 61)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(228, 46)
        Me.Label4.TabIndex = 14
        Me.Label4.Text = "Your flowsheet will be automatically saved on Simulate 365 Dashboard"
        Me.Label4.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'Button2
        '
        Me.Button2.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button2.BackColor = System.Drawing.Color.FromArgb(CType(CType(31, Byte), Integer), CType(CType(166, Byte), Integer), CType(CType(13, Byte), Integer))
        Me.Button2.Font = New System.Drawing.Font("Calibri", 15.75!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Button2.ForeColor = System.Drawing.Color.White
        Me.Button2.Location = New System.Drawing.Point(378, 8)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(228, 45)
        Me.Button2.TabIndex = 13
        Me.Button2.Text = "Switch to DWSIM Pro"
        Me.Button2.UseVisualStyleBackColor = False
        '
        'FormHeatMaps
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(621, 543)
        Me.Controls.Add(Me.TableLayoutPanel8)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormHeatMaps"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "DWSIM"
        Me.TableLayoutPanel8.ResumeLayout(False)
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).EndInit()
        Me.Panel1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents TableLayoutPanel8 As TableLayoutPanel
    Friend WithEvents Label12 As Label
    Friend WithEvents Label15 As Label
    Friend WithEvents PictureBox6 As PictureBox
    Friend WithEvents Panel1 As Panel
    Friend WithEvents Label4 As Label
    Friend WithEvents Button2 As Button
End Class
