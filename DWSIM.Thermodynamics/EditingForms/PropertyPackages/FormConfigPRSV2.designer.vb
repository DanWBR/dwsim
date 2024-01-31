<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormConfigPRSV2

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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigPRSV2))
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem3 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.dgvu1 = New System.Windows.Forms.DataGridView()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.LabelWithDivider3 = New System.Windows.Forms.Label()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Button6 = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.KryptonDataGridView2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem3.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        CType(Me.dgvu1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStripItem2.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.KryptonDataGridView2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem3, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem2
        '
        'FaTabStripItem3
        '
        Me.FaTabStripItem3.CanClose = False
        Me.FaTabStripItem3.Controls.Add(Me.GroupBox3)
        Me.FaTabStripItem3.IsDrawn = True
        Me.FaTabStripItem3.Name = "FaTabStripItem3"
        resources.ApplyResources(Me.FaTabStripItem3, "FaTabStripItem3")
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.Label1)
        Me.GroupBox3.Controls.Add(Me.dgvu1)
        Me.GroupBox3.Controls.Add(Me.LabelWithDivider3)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'dgvu1
        '
        Me.dgvu1.AllowUserToAddRows = False
        Me.dgvu1.AllowUserToDeleteRows = False
        Me.dgvu1.AllowUserToResizeColumns = False
        Me.dgvu1.AllowUserToResizeRows = False
        resources.ApplyResources(Me.dgvu1, "dgvu1")
        Me.dgvu1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvu1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvu1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column3, Me.Column5, Me.Column6, Me.Column7})
        Me.dgvu1.MultiSelect = False
        Me.dgvu1.Name = "dgvu1"
        Me.dgvu1.RowHeadersVisible = False
        Me.dgvu1.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        DataGridViewCellStyle5.Format = "N5"
        Me.dgvu1.RowsDefaultCellStyle = DataGridViewCellStyle5
        Me.dgvu1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'Column3
        '
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.Column3.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        Me.Column3.ReadOnly = True
        '
        'Column5
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        DataGridViewCellStyle2.Format = "N5"
        Me.Column5.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        '
        'Column6
        '
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        DataGridViewCellStyle3.Format = "N5"
        Me.Column6.DefaultCellStyle = DataGridViewCellStyle3
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        '
        'Column7
        '
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        DataGridViewCellStyle4.Format = "N5"
        Me.Column7.DefaultCellStyle = DataGridViewCellStyle4
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        '
        'LabelWithDivider3
        '
        resources.ApplyResources(Me.LabelWithDivider3, "LabelWithDivider3")
        Me.LabelWithDivider3.Name = "LabelWithDivider3"
        '
        'FaTabStripItem2
        '
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.GroupBox2)
        Me.FaTabStripItem2.Controls.Add(Me.Label2)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        Me.FaTabStripItem2.Selected = True
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.Button5)
        Me.GroupBox2.Controls.Add(Me.Button6)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.KryptonDataGridView2)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'Button5
        '
        resources.ApplyResources(Me.Button5, "Button5")
        Me.Button5.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.export_48px
        Me.Button5.Name = "Button5"
        Me.ToolTip1.SetToolTip(Me.Button5, resources.GetString("Button5.ToolTip"))
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Button6
        '
        resources.ApplyResources(Me.Button6, "Button6")
        Me.Button6.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.import_48px
        Me.Button6.Name = "Button6"
        Me.ToolTip1.SetToolTip(Me.Button6, resources.GetString("Button6.ToolTip"))
        Me.Button6.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'KryptonDataGridView2
        '
        Me.KryptonDataGridView2.AllowUserToAddRows = False
        Me.KryptonDataGridView2.AllowUserToDeleteRows = False
        Me.KryptonDataGridView2.AllowUserToResizeColumns = False
        Me.KryptonDataGridView2.AllowUserToResizeRows = False
        resources.ApplyResources(Me.KryptonDataGridView2, "KryptonDataGridView2")
        Me.KryptonDataGridView2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.KryptonDataGridView2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.KryptonDataGridView2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.DataGridViewTextBoxColumn3, Me.Column4})
        Me.KryptonDataGridView2.MultiSelect = False
        Me.KryptonDataGridView2.Name = "KryptonDataGridView2"
        Me.KryptonDataGridView2.RowHeadersVisible = False
        Me.KryptonDataGridView2.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        DataGridViewCellStyle8.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        DataGridViewCellStyle8.Format = "N5"
        Me.KryptonDataGridView2.RowsDefaultCellStyle = DataGridViewCellStyle8
        Me.KryptonDataGridView2.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'DataGridViewTextBoxColumn1
        '
        DataGridViewCellStyle6.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.DataGridViewTextBoxColumn1.DefaultCellStyle = DataGridViewCellStyle6
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle7.BackColor = System.Drawing.Color.FromArgb(CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer), CType(CType(224, Byte), Integer))
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle7
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'DataGridViewTextBoxColumn3
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        '
        'Column4
        '
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'FaTabStripItem1
        '
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Selected = True
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        '
        'FormConfigPRSV2
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.FaTabStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormConfigPRSV2"
        Me.TopMost = True
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem3.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.dgvu1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStripItem2.ResumeLayout(False)
        Me.FaTabStripItem2.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.KryptonDataGridView2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Private WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem3 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents dgvu1 As System.Windows.Forms.DataGridView
    Public WithEvents LabelWithDivider3 As System.Windows.Forms.Label
    Private WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents KryptonDataGridView2 As System.Windows.Forms.DataGridView
    Public WithEvents Label2 As System.Windows.Forms.Label
    Private WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Button5 As Button
    Public WithEvents Button6 As Button
    Friend WithEvents ToolTip1 As ToolTip
End Class
