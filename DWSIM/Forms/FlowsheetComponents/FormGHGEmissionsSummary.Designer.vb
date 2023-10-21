<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormGHGEmissionsSummary

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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormGHGEmissionsSummary))
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.gridEmissions = New System.Windows.Forms.DataGridView()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbTotalEmission = New System.Windows.Forms.TextBox()
        Me.tbTotalEmissionCO2eq = New System.Windows.Forms.TextBox()
        Me.lblEmissionUnits = New System.Windows.Forms.Label()
        Me.tbTotalEmissionMolar = New System.Windows.Forms.TextBox()
        Me.tbTotalEmissionCO2eqMolar = New System.Windows.Forms.TextBox()
        Me.lblEmissionMolarUnits = New System.Windows.Forms.Label()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridEmissions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'PictureBox1
        '
        Me.PictureBox1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources._28903492_7521999
        Me.PictureBox1.Location = New System.Drawing.Point(6, 298)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(243, 177)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 0
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 12.0!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(13, 15)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(169, 40)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Greenhouse Gas " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "Emissions Summary"
        '
        'gridEmissions
        '
        Me.gridEmissions.AllowUserToAddRows = False
        Me.gridEmissions.AllowUserToDeleteRows = False
        Me.gridEmissions.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.gridEmissions.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridEmissions.BackgroundColor = System.Drawing.Color.White
        Me.gridEmissions.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.gridEmissions.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridEmissions.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2, Me.Column3, Me.Column4, Me.Column5, Me.Column6, Me.Column7, Me.Column8})
        Me.gridEmissions.Location = New System.Drawing.Point(253, 15)
        Me.gridEmissions.Name = "gridEmissions"
        Me.gridEmissions.ReadOnly = True
        Me.gridEmissions.RowHeadersVisible = False
        Me.gridEmissions.Size = New System.Drawing.Size(579, 399)
        Me.gridEmissions.TabIndex = 2
        '
        'Label2
        '
        Me.Label2.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(256, 428)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(107, 13)
        Me.Label2.TabIndex = 3
        Me.Label2.Text = "Total GHG Emissions"
        '
        'Label3
        '
        Me.Label3.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label3.AutoSize = True
        Me.Label3.Location = New System.Drawing.Point(495, 428)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(149, 13)
        Me.Label3.TabIndex = 4
        Me.Label3.Text = "Total GHG Emissions (CO2eq)"
        '
        'tbTotalEmission
        '
        Me.tbTotalEmission.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTotalEmission.Location = New System.Drawing.Point(369, 425)
        Me.tbTotalEmission.Name = "tbTotalEmission"
        Me.tbTotalEmission.ReadOnly = True
        Me.tbTotalEmission.Size = New System.Drawing.Size(107, 20)
        Me.tbTotalEmission.TabIndex = 5
        Me.tbTotalEmission.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbTotalEmissionCO2eq
        '
        Me.tbTotalEmissionCO2eq.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTotalEmissionCO2eq.Location = New System.Drawing.Point(650, 425)
        Me.tbTotalEmissionCO2eq.Name = "tbTotalEmissionCO2eq"
        Me.tbTotalEmissionCO2eq.ReadOnly = True
        Me.tbTotalEmissionCO2eq.Size = New System.Drawing.Size(100, 20)
        Me.tbTotalEmissionCO2eq.TabIndex = 6
        Me.tbTotalEmissionCO2eq.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblEmissionUnits
        '
        Me.lblEmissionUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblEmissionUnits.AutoSize = True
        Me.lblEmissionUnits.Location = New System.Drawing.Point(756, 428)
        Me.lblEmissionUnits.Name = "lblEmissionUnits"
        Me.lblEmissionUnits.Size = New System.Drawing.Size(39, 13)
        Me.lblEmissionUnits.TabIndex = 7
        Me.lblEmissionUnits.Text = "Label4"
        '
        'tbTotalEmissionMolar
        '
        Me.tbTotalEmissionMolar.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTotalEmissionMolar.Location = New System.Drawing.Point(369, 451)
        Me.tbTotalEmissionMolar.Name = "tbTotalEmissionMolar"
        Me.tbTotalEmissionMolar.ReadOnly = True
        Me.tbTotalEmissionMolar.Size = New System.Drawing.Size(107, 20)
        Me.tbTotalEmissionMolar.TabIndex = 8
        Me.tbTotalEmissionMolar.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbTotalEmissionCO2eqMolar
        '
        Me.tbTotalEmissionCO2eqMolar.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTotalEmissionCO2eqMolar.Location = New System.Drawing.Point(650, 451)
        Me.tbTotalEmissionCO2eqMolar.Name = "tbTotalEmissionCO2eqMolar"
        Me.tbTotalEmissionCO2eqMolar.ReadOnly = True
        Me.tbTotalEmissionCO2eqMolar.Size = New System.Drawing.Size(100, 20)
        Me.tbTotalEmissionCO2eqMolar.TabIndex = 9
        Me.tbTotalEmissionCO2eqMolar.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lblEmissionMolarUnits
        '
        Me.lblEmissionMolarUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblEmissionMolarUnits.AutoSize = True
        Me.lblEmissionMolarUnits.Location = New System.Drawing.Point(756, 454)
        Me.lblEmissionMolarUnits.Name = "lblEmissionMolarUnits"
        Me.lblEmissionMolarUnits.Size = New System.Drawing.Size(39, 13)
        Me.lblEmissionMolarUnits.TabIndex = 10
        Me.lblEmissionMolarUnits.Text = "Label4"
        '
        'Column1
        '
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle1
        Me.Column1.HeaderText = "Object"
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'Column2
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        Me.Column2.DefaultCellStyle = DataGridViewCellStyle2
        Me.Column2.HeaderText = "Type"
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        '
        'Column3
        '
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column3.DefaultCellStyle = DataGridViewCellStyle3
        Me.Column3.HeaderText = "Reference Power/Heat"
        Me.Column3.Name = "Column3"
        Me.Column3.ReadOnly = True
        '
        'Column4
        '
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column4.DefaultCellStyle = DataGridViewCellStyle4
        Me.Column4.HeaderText = "Emission Factor"
        Me.Column4.Name = "Column4"
        Me.Column4.ReadOnly = True
        '
        'Column5
        '
        DataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column5.DefaultCellStyle = DataGridViewCellStyle5
        Me.Column5.HeaderText = "Mass GHG Emissions"
        Me.Column5.Name = "Column5"
        Me.Column5.ReadOnly = True
        '
        'Column6
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column6.DefaultCellStyle = DataGridViewCellStyle6
        Me.Column6.HeaderText = "Mass GHG Emissions (CO2eq)"
        Me.Column6.Name = "Column6"
        Me.Column6.ReadOnly = True
        '
        'Column7
        '
        DataGridViewCellStyle7.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column7.DefaultCellStyle = DataGridViewCellStyle7
        Me.Column7.HeaderText = "Molar GHG Emissions"
        Me.Column7.Name = "Column7"
        Me.Column7.ReadOnly = True
        '
        'Column8
        '
        DataGridViewCellStyle8.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.Column8.DefaultCellStyle = DataGridViewCellStyle8
        Me.Column8.HeaderText = "Molar GHG Emissions (CO2eq)"
        Me.Column8.Name = "Column8"
        Me.Column8.ReadOnly = True
        '
        'FormGHGEmissionsSummary
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(844, 479)
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.lblEmissionMolarUnits)
        Me.Controls.Add(Me.tbTotalEmissionCO2eqMolar)
        Me.Controls.Add(Me.tbTotalEmissionMolar)
        Me.Controls.Add(Me.lblEmissionUnits)
        Me.Controls.Add(Me.tbTotalEmissionCO2eq)
        Me.Controls.Add(Me.tbTotalEmission)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.gridEmissions)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.PictureBox1)
        Me.DoubleBuffered = True
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "FormGHGEmissionsSummary"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabText = "GHG Emissions"
        Me.Text = "GHG Emissions"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridEmissions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents Label1 As Label
    Friend WithEvents gridEmissions As DataGridView
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents tbTotalEmission As TextBox
    Friend WithEvents tbTotalEmissionCO2eq As TextBox
    Friend WithEvents lblEmissionUnits As Label
    Friend WithEvents tbTotalEmissionMolar As TextBox
    Friend WithEvents tbTotalEmissionCO2eqMolar As TextBox
    Friend WithEvents lblEmissionMolarUnits As Label
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents Column2 As DataGridViewTextBoxColumn
    Friend WithEvents Column3 As DataGridViewTextBoxColumn
    Friend WithEvents Column4 As DataGridViewTextBoxColumn
    Friend WithEvents Column5 As DataGridViewTextBoxColumn
    Friend WithEvents Column6 As DataGridViewTextBoxColumn
    Friend WithEvents Column7 As DataGridViewTextBoxColumn
    Friend WithEvents Column8 As DataGridViewTextBoxColumn
End Class
