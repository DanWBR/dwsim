<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormExtraProperties

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormExtraProperties))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.grid1 = New System.Windows.Forms.DataGridView()
        Me.colname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colvalue = New System.Windows.Forms.DataGridViewTextBoxColumn()
        CType(Me.grid1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'grid1
        '
        resources.ApplyResources(Me.grid1, "grid1")
        Me.grid1.AllowUserToAddRows = False
        Me.grid1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.grid1.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.None
        Me.grid1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colname, Me.colvalue})
        Me.grid1.Name = "grid1"
        Me.grid1.RowHeadersVisible = False
        Me.grid1.RowTemplate.Height = 24
        '
        'colname
        '
        Me.colname.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        Me.colname.DefaultCellStyle = DataGridViewCellStyle1
        Me.colname.FillWeight = 50.0!
        resources.ApplyResources(Me.colname, "colname")
        Me.colname.Name = "colname"
        Me.colname.ReadOnly = True
        '
        'colvalue
        '
        Me.colvalue.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        Me.colvalue.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.colvalue, "colvalue")
        Me.colvalue.Name = "colvalue"
        '
        'FormExtraProperties
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.Controls.Add(Me.grid1)
        Me.Name = "FormExtraProperties"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        CType(Me.grid1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents grid1 As DataGridView
    Friend WithEvents colname As DataGridViewTextBoxColumn
    Friend WithEvents colvalue As DataGridViewTextBoxColumn
End Class
