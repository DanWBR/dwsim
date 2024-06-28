<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormObjectList

    Inherits UserControl

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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.OutlookGrid1 = New OutlookStyleControls.OutlookGrid()
        Me.ColumnID = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewButtonColumn()
        CType(Me.OutlookGrid1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(6, 11)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(41, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Search"
        '
        'TextBox1
        '
        Me.TextBox1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.TextBox1.Location = New System.Drawing.Point(54, 8)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(364, 20)
        Me.TextBox1.TabIndex = 1
        '
        'OutlookGrid1
        '
        Me.OutlookGrid1.AllowUserToAddRows = False
        Me.OutlookGrid1.AllowUserToDeleteRows = False
        Me.OutlookGrid1.AllowUserToResizeColumns = False
        Me.OutlookGrid1.AllowUserToResizeRows = False
        Me.OutlookGrid1.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.OutlookGrid1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.OutlookGrid1.BackgroundColor = System.Drawing.Color.White
        Me.OutlookGrid1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.OutlookGrid1.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.SingleHorizontal
        Me.OutlookGrid1.CollapseIcon = Global.DWSIM.My.Resources.Resources.collapse_arrow_48px
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.OutlookGrid1.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.OutlookGrid1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.OutlookGrid1.ColumnHeadersVisible = False
        Me.OutlookGrid1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ColumnID, Me.Column1, Me.Column2, Me.Column5, Me.Column3, Me.Column4})
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Window
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.ControlText
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        Me.OutlookGrid1.DefaultCellStyle = DataGridViewCellStyle2
        Me.OutlookGrid1.ExpandIcon = Global.DWSIM.My.Resources.Resources.expand_arrow_48px
        Me.OutlookGrid1.GridColor = System.Drawing.Color.WhiteSmoke
        Me.OutlookGrid1.Location = New System.Drawing.Point(9, 34)
        Me.OutlookGrid1.MultiSelect = False
        Me.OutlookGrid1.Name = "OutlookGrid1"
        Me.OutlookGrid1.RowHeadersVisible = False
        Me.OutlookGrid1.ScrollBars = System.Windows.Forms.ScrollBars.Vertical
        Me.OutlookGrid1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.OutlookGrid1.Size = New System.Drawing.Size(409, 542)
        Me.OutlookGrid1.TabIndex = 2
        '
        'ColumnID
        '
        Me.ColumnID.HeaderText = "ID"
        Me.ColumnID.Name = "ColumnID"
        Me.ColumnID.Visible = False
        '
        'Column1
        '
        Me.Column1.FillWeight = 140.0!
        Me.Column1.HeaderText = "Name"
        Me.Column1.Name = "Column1"
        '
        'Column2
        '
        Me.Column2.HeaderText = "Type"
        Me.Column2.Name = "Column2"
        Me.Column2.Visible = False
        '
        'Column5
        '
        Me.Column5.FillWeight = 60.0!
        Me.Column5.HeaderText = "Center"
        Me.Column5.Name = "Column5"
        Me.Column5.Text = "Center"
        Me.Column5.UseColumnTextForButtonValue = True
        '
        'Column3
        '
        Me.Column3.FillWeight = 60.0!
        Me.Column3.HeaderText = "Edit"
        Me.Column3.Name = "Column3"
        Me.Column3.Text = "Edit"
        Me.Column3.UseColumnTextForButtonValue = True
        '
        'Column4
        '
        Me.Column4.FillWeight = 60.0!
        Me.Column4.HeaderText = "Delete"
        Me.Column4.Name = "Column4"
        Me.Column4.Text = "Delete"
        Me.Column4.UseColumnTextForButtonValue = True
        '
        'FormObjectList
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.Controls.Add(Me.OutlookGrid1)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Label1)
        Me.DoubleBuffered = True
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "FormObjectList"
        Me.Size = New System.Drawing.Size(430, 588)
        CType(Me.OutlookGrid1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents TextBox1 As TextBox
    Friend WithEvents OutlookGrid1 As OutlookStyleControls.OutlookGrid
    Friend WithEvents ColumnID As DataGridViewTextBoxColumn
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents Column2 As DataGridViewTextBoxColumn
    Friend WithEvents Column5 As DataGridViewButtonColumn
    Friend WithEvents Column3 As DataGridViewButtonColumn
    Friend WithEvents Column4 As DataGridViewButtonColumn
End Class
