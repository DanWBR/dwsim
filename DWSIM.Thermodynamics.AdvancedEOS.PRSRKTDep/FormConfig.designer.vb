Imports DWSIM.Thermodynamics

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormConfig
    Inherits FormConfigPropertyPackageBase

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
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.FaTabStrip1 = New System.Windows.Forms.TabControl()
        Me.FaTabStripItem2 = New System.Windows.Forms.TabPage()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.KryptonDataGridView2 = New System.Windows.Forms.DataGridView()
        Me.LabelWithDivider2 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.FaTabStripItem1 = New System.Windows.Forms.TabPage()
        Me.IPGrid = New System.Windows.Forms.DataGridView()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem2.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.KryptonDataGridView2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        Me.FaTabStrip1.Controls.Add(Me.FaTabStripItem2)
        Me.FaTabStrip1.Controls.Add(Me.FaTabStripItem1)
        Me.FaTabStrip1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.FaTabStrip1.Font = New System.Drawing.Font("Tahoma", 8.25!)
        Me.FaTabStrip1.Location = New System.Drawing.Point(0, 0)
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedIndex = 0
        Me.FaTabStrip1.Size = New System.Drawing.Size(749, 393)
        Me.FaTabStrip1.TabIndex = 0
        Me.FaTabStrip1.Text = "FaTabStrip1"
        '
        'FaTabStripItem2
        '
        Me.FaTabStripItem2.Controls.Add(Me.GroupBox2)
        Me.FaTabStripItem2.Controls.Add(Me.Label2)
        Me.FaTabStripItem2.Location = New System.Drawing.Point(4, 22)
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        Me.FaTabStripItem2.Size = New System.Drawing.Size(741, 367)
        Me.FaTabStripItem2.TabIndex = 1
        Me.FaTabStripItem2.Text = "Interaction Parameters"
        '
        'GroupBox2
        '
        Me.GroupBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox2.Controls.Add(Me.Label1)
        Me.GroupBox2.Controls.Add(Me.Button3)
        Me.GroupBox2.Controls.Add(Me.KryptonDataGridView2)
        Me.GroupBox2.Controls.Add(Me.LabelWithDivider2)
        Me.GroupBox2.Location = New System.Drawing.Point(5, 3)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(729, 359)
        Me.GroupBox2.TabIndex = 6
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Interaction Parameters"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(9, 22)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(524, 13)
        Me.Label1.TabIndex = 9
        Me.Label1.Text = "Expressions must be entered with numbers using dot as the decimal separator and n" &
    "o thousands separator."
        '
        'Button3
        '
        Me.Button3.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Button3.Location = New System.Drawing.Point(579, 17)
        Me.Button3.Name = "Button3"
        Me.Button3.Size = New System.Drawing.Size(143, 23)
        Me.Button3.TabIndex = 8
        Me.Button3.Text = "Estimate Selected"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'KryptonDataGridView2
        '
        Me.KryptonDataGridView2.AllowUserToAddRows = False
        Me.KryptonDataGridView2.AllowUserToDeleteRows = False
        Me.KryptonDataGridView2.AllowUserToResizeColumns = False
        Me.KryptonDataGridView2.AllowUserToResizeRows = False
        Me.KryptonDataGridView2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.KryptonDataGridView2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.KryptonDataGridView2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.DisableResizing
        Me.KryptonDataGridView2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column3, Me.Column4, Me.Column5})
        Me.KryptonDataGridView2.Location = New System.Drawing.Point(8, 46)
        Me.KryptonDataGridView2.Name = "KryptonDataGridView2"
        Me.KryptonDataGridView2.RowHeadersVisible = False
        Me.KryptonDataGridView2.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        Me.KryptonDataGridView2.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.KryptonDataGridView2.Size = New System.Drawing.Size(714, 307)
        Me.KryptonDataGridView2.TabIndex = 4
        '
        'LabelWithDivider2
        '
        Me.LabelWithDivider2.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.LabelWithDivider2.Location = New System.Drawing.Point(6, 48)
        Me.LabelWithDivider2.Name = "LabelWithDivider2"
        Me.LabelWithDivider2.Size = New System.Drawing.Size(716, 10)
        Me.LabelWithDivider2.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(179, 144)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(127, 13)
        Me.Label2.TabIndex = 5
        Me.Label2.Text = "Recurso em implantação."
        '
        'FaTabStripItem1
        '
        Me.FaTabStripItem1.Location = New System.Drawing.Point(4, 22)
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Size = New System.Drawing.Size(741, 367)
        Me.FaTabStripItem1.TabIndex = 2
        Me.FaTabStripItem1.Text = "General"
        '
        'IPGrid
        '
        Me.IPGrid.AllowUserToAddRows = False
        Me.IPGrid.AllowUserToDeleteRows = False
        Me.IPGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.IPGrid.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.IPGrid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.IPGrid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column6})
        Me.IPGrid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.IPGrid.Location = New System.Drawing.Point(0, 0)
        Me.IPGrid.MultiSelect = False
        Me.IPGrid.Name = "IPGrid"
        Me.IPGrid.ReadOnly = True
        Me.IPGrid.Size = New System.Drawing.Size(558, 326)
        Me.IPGrid.TabIndex = 0
        '
        'Column6
        '
        Me.Column6.HeaderText = ""
        Me.Column6.Name = "Column6"
        Me.Column6.ReadOnly = True
        '
        'Column3
        '
        DataGridViewCellStyle4.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Column3.DefaultCellStyle = DataGridViewCellStyle4
        Me.Column3.HeaderText = "ID1"
        Me.Column3.Name = "Column3"
        Me.Column3.ReadOnly = True
        '
        'Column4
        '
        DataGridViewCellStyle5.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Column4.DefaultCellStyle = DataGridViewCellStyle5
        Me.Column4.HeaderText = "ID2"
        Me.Column4.Name = "Column4"
        Me.Column4.ReadOnly = True
        '
        'Column5
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle6.Padding = New System.Windows.Forms.Padding(5, 0, 0, 0)
        Me.Column5.DefaultCellStyle = DataGridViewCellStyle6
        Me.Column5.FillWeight = 200.0!
        Me.Column5.HeaderText = "kij = f(T,P)"
        Me.Column5.Name = "Column5"
        '
        'FormConfig
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(749, 393)
        Me.Controls.Add(Me.FaTabStrip1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormConfig"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Configure Property Package"
        Me.TopMost = True
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem2.ResumeLayout(False)
        Me.FaTabStripItem2.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.KryptonDataGridView2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents KryptonDataGridView2 As System.Windows.Forms.DataGridView
    Public WithEvents LabelWithDivider2 As System.Windows.Forms.Label
    Private WithEvents FaTabStrip1 As System.Windows.Forms.TabControl
    Private WithEvents FaTabStripItem1 As System.Windows.Forms.TabPage
    Private WithEvents FaTabStripItem2 As System.Windows.Forms.TabPage
    Public WithEvents Button3 As System.Windows.Forms.Button
    Friend WithEvents IPGrid As System.Windows.Forms.DataGridView
    Friend WithEvents Column6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Label1 As Windows.Forms.Label
    Friend WithEvents Column3 As Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column4 As Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column5 As Windows.Forms.DataGridViewTextBoxColumn
End Class
