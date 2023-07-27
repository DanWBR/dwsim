<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Flowsheet_Editor
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Flowsheet_Editor))
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.dgvOutputLinks = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewComboBoxColumn1 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.dgvInputLinks = New System.Windows.Forms.DataGridView()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.dgvOutputPars = New System.Windows.Forms.DataGridView()
        Me.Column4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewComboBoxColumn2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewComboBoxColumn4 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton6 = New System.Windows.Forms.ToolStripButton()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.dgvInputPars = New System.Windows.Forms.DataGridView()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewComboBoxColumn3 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.ToolStrip2 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.dgmap = New System.Windows.Forms.DataGridView()
        Me.id1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.col1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.id2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.col2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbMassTransferMode = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.btnInitialize = New System.Windows.Forms.Button()
        Me.lblInit = New System.Windows.Forms.Label()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.dgvOutputLinks, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        CType(Me.dgvInputLinks, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        CType(Me.dgvOutputPars, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        CType(Me.dgvInputPars, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip2.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        CType(Me.dgmap, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage3.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.GroupBox2)
        Me.TabPage1.Controls.Add(Me.GroupBox1)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.dgvOutputLinks)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'dgvOutputLinks
        '
        Me.dgvOutputLinks.AllowUserToAddRows = False
        Me.dgvOutputLinks.AllowUserToDeleteRows = False
        Me.dgvOutputLinks.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvOutputLinks.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvOutputLinks.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewComboBoxColumn1})
        resources.ApplyResources(Me.dgvOutputLinks, "dgvOutputLinks")
        Me.dgvOutputLinks.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.dgvOutputLinks.Name = "dgvOutputLinks"
        Me.dgvOutputLinks.RowHeadersVisible = False
        '
        'DataGridViewTextBoxColumn1
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewComboBoxColumn1
        '
        resources.ApplyResources(Me.DataGridViewComboBoxColumn1, "DataGridViewComboBoxColumn1")
        Me.DataGridViewComboBoxColumn1.Name = "DataGridViewComboBoxColumn1"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.dgvInputLinks)
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'dgvInputLinks
        '
        Me.dgvInputLinks.AllowUserToAddRows = False
        Me.dgvInputLinks.AllowUserToDeleteRows = False
        Me.dgvInputLinks.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvInputLinks.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvInputLinks.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2})
        resources.ApplyResources(Me.dgvInputLinks, "dgvInputLinks")
        Me.dgvInputLinks.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.dgvInputLinks.Name = "dgvInputLinks"
        Me.dgvInputLinks.RowHeadersVisible = False
        '
        'Column1
        '
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'Column2
        '
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.GroupBox3)
        Me.TabPage2.Controls.Add(Me.GroupBox4)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.dgvOutputPars)
        Me.GroupBox3.Controls.Add(Me.ToolStrip1)
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'dgvOutputPars
        '
        Me.dgvOutputPars.AllowUserToAddRows = False
        Me.dgvOutputPars.AllowUserToDeleteRows = False
        Me.dgvOutputPars.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvOutputPars.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvOutputPars.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column4, Me.DataGridViewComboBoxColumn2, Me.DataGridViewComboBoxColumn4})
        resources.ApplyResources(Me.dgvOutputPars, "dgvOutputPars")
        Me.dgvOutputPars.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.dgvOutputPars.MultiSelect = False
        Me.dgvOutputPars.Name = "dgvOutputPars"
        '
        'Column4
        '
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        '
        'DataGridViewComboBoxColumn2
        '
        Me.DataGridViewComboBoxColumn2.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn2, "DataGridViewComboBoxColumn2")
        Me.DataGridViewComboBoxColumn2.Name = "DataGridViewComboBoxColumn2"
        Me.DataGridViewComboBoxColumn2.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridViewComboBoxColumn2.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'DataGridViewComboBoxColumn4
        '
        resources.ApplyResources(Me.DataGridViewComboBoxColumn4, "DataGridViewComboBoxColumn4")
        Me.DataGridViewComboBoxColumn4.Name = "DataGridViewComboBoxColumn4"
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton3, Me.ToolStripButton4, Me.ToolStripButton6})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripButton6
        '
        Me.ToolStripButton6.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton6.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.tag_blue
        resources.ApplyResources(Me.ToolStripButton6, "ToolStripButton6")
        Me.ToolStripButton6.Name = "ToolStripButton6"
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.dgvInputPars)
        Me.GroupBox4.Controls.Add(Me.ToolStrip2)
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'dgvInputPars
        '
        Me.dgvInputPars.AllowUserToAddRows = False
        Me.dgvInputPars.AllowUserToDeleteRows = False
        Me.dgvInputPars.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvInputPars.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvInputPars.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column3, Me.DataGridViewTextBoxColumn3, Me.DataGridViewComboBoxColumn3})
        resources.ApplyResources(Me.dgvInputPars, "dgvInputPars")
        Me.dgvInputPars.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.dgvInputPars.MultiSelect = False
        Me.dgvInputPars.Name = "dgvInputPars"
        '
        'Column3
        '
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        '
        'DataGridViewTextBoxColumn3
        '
        Me.DataGridViewTextBoxColumn3.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridViewTextBoxColumn3.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'DataGridViewComboBoxColumn3
        '
        resources.ApplyResources(Me.DataGridViewComboBoxColumn3, "DataGridViewComboBoxColumn3")
        Me.DataGridViewComboBoxColumn3.Name = "DataGridViewComboBoxColumn3"
        '
        'ToolStrip2
        '
        Me.ToolStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2, Me.ToolStripButton5})
        resources.ApplyResources(Me.ToolStrip2, "ToolStrip2")
        Me.ToolStrip2.Name = "ToolStrip2"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton5
        '
        Me.ToolStripButton5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton5.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.tag_blue
        resources.ApplyResources(Me.ToolStripButton5, "ToolStripButton5")
        Me.ToolStripButton5.Name = "ToolStripButton5"
        '
        'TabPage4
        '
        Me.TabPage4.Controls.Add(Me.dgmap)
        resources.ApplyResources(Me.TabPage4, "TabPage4")
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'dgmap
        '
        Me.dgmap.AllowUserToAddRows = False
        Me.dgmap.AllowUserToDeleteRows = False
        Me.dgmap.AllowUserToOrderColumns = True
        Me.dgmap.AllowUserToResizeColumns = False
        Me.dgmap.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgmap.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgmap.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.id1, Me.col1, Me.id2, Me.col2})
        resources.ApplyResources(Me.dgmap, "dgmap")
        Me.dgmap.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.dgmap.MultiSelect = False
        Me.dgmap.Name = "dgmap"
        Me.dgmap.RowHeadersVisible = False
        '
        'id1
        '
        resources.ApplyResources(Me.id1, "id1")
        Me.id1.Name = "id1"
        Me.id1.ReadOnly = True
        '
        'col1
        '
        resources.ApplyResources(Me.col1, "col1")
        Me.col1.Name = "col1"
        Me.col1.ReadOnly = True
        '
        'id2
        '
        resources.ApplyResources(Me.id2, "id2")
        Me.id2.Name = "id2"
        '
        'col2
        '
        resources.ApplyResources(Me.col2, "col2")
        Me.col2.Name = "col2"
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.Label2)
        Me.TabPage3.Controls.Add(Me.cbMassTransferMode)
        Me.TabPage3.Controls.Add(Me.Label1)
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbMassTransferMode
        '
        Me.cbMassTransferMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMassTransferMode.FormattingEnabled = True
        Me.cbMassTransferMode.Items.AddRange(New Object() {resources.GetString("cbMassTransferMode.Items"), resources.GetString("cbMassTransferMode.Items1"), resources.GetString("cbMassTransferMode.Items2"), resources.GetString("cbMassTransferMode.Items3")})
        resources.ApplyResources(Me.cbMassTransferMode, "cbMassTransferMode")
        Me.cbMassTransferMode.Name = "cbMassTransferMode"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'btnInitialize
        '
        resources.ApplyResources(Me.btnInitialize, "btnInitialize")
        Me.btnInitialize.Name = "btnInitialize"
        Me.btnInitialize.UseVisualStyleBackColor = True
        '
        'lblInit
        '
        resources.ApplyResources(Me.lblInit, "lblInit")
        Me.lblInit.Name = "lblInit"
        '
        'EditingForm_Flowsheet_Editor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.lblInit)
        Me.Controls.Add(Me.btnInitialize)
        Me.Controls.Add(Me.TabControl1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "EditingForm_Flowsheet_Editor"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.dgvOutputLinks, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        CType(Me.dgvInputLinks, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.dgvOutputPars, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        CType(Me.dgvInputPars, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip2.ResumeLayout(False)
        Me.ToolStrip2.PerformLayout()
        Me.TabPage4.ResumeLayout(False)
        CType(Me.dgmap, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage3.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents TabControl1 As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents TabPage2 As System.Windows.Forms.TabPage
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents dgvOutputLinks As System.Windows.Forms.DataGridView
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents dgvInputLinks As System.Windows.Forms.DataGridView
    Public WithEvents TabPage3 As System.Windows.Forms.TabPage
    Public WithEvents btnInitialize As System.Windows.Forms.Button
    Public WithEvents lblInit As System.Windows.Forms.Label
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents ToolStrip2 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents dgvInputPars As System.Windows.Forms.DataGridView
    Public WithEvents dgvOutputPars As System.Windows.Forms.DataGridView
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Public WithEvents cbMassTransferMode As System.Windows.Forms.ComboBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column2 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents ToolStripButton6 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
    Public WithEvents TabPage4 As System.Windows.Forms.TabPage
    Public WithEvents dgmap As System.Windows.Forms.DataGridView
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewComboBoxColumn1 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents Column4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewComboBoxColumn2 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents DataGridViewComboBoxColumn4 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents Column3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents DataGridViewComboBoxColumn3 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents id1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents col1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents id2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents col2 As System.Windows.Forms.DataGridViewComboBoxColumn
End Class
