<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormDynamicsManager

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormDynamicsManager))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.gridselectedset = New System.Windows.Forms.DataGridView()
        Me.id = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.active = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.itemname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.coleventstamp = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.coleventtype = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.associatedobject = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.associatedproperty = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.failsafevalue = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.unit = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip2 = New System.Windows.Forms.ToolStrip()
        Me.btnAddEvent = New System.Windows.Forms.ToolStripButton()
        Me.btnCopyEvent = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveEvent = New System.Windows.Forms.ToolStripButton()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.gridsets = New System.Windows.Forms.DataGridView()
        Me.colid = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.btnAddEventSet = New System.Windows.Forms.ToolStripButton()
        Me.btnCopyEventSet = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveEventSet = New System.Windows.Forms.ToolStripButton()
        Me.TabPage6 = New System.Windows.Forms.TabPage()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.grdiselmatrix = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewCheckBoxColumn1 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colmatind = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.colalarm = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewComboBoxColumn2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewComboBoxColumn3 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewComboBoxColumn4 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.ToolStrip3 = New System.Windows.Forms.ToolStrip()
        Me.btnAddMatrixItem = New System.Windows.Forms.ToolStripButton()
        Me.btnCopyMatrixItem = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveMatrixItem = New System.Windows.Forms.ToolStripButton()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.gridmatrices = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip4 = New System.Windows.Forms.ToolStrip()
        Me.btnAddMatrix = New System.Windows.Forms.ToolStripButton()
        Me.btnCopyMatrix = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveMatrix = New System.Windows.Forms.ToolStripButton()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.panelSelIntegrator = New System.Windows.Forms.Panel()
        Me.dtpIntegratorDuration = New System.Windows.Forms.DateTimePicker()
        Me.dtpIntegrationStep = New System.Windows.Forms.DateTimePicker()
        Me.nupCalcControlFreq = New System.Windows.Forms.NumericUpDown()
        Me.nupCalcBalFreq = New System.Windows.Forms.NumericUpDown()
        Me.nupCalcEqFreq = New System.Windows.Forms.NumericUpDown()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.gridintegrators = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn11 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn12 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip6 = New System.Windows.Forms.ToolStrip()
        Me.btnAddIntegrator = New System.Windows.Forms.ToolStripButton()
        Me.btnCopyIntegrator = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveIntegrator = New System.Windows.Forms.ToolStripButton()
        Me.TabPage7 = New System.Windows.Forms.TabPage()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.panelSelSchedule = New System.Windows.Forms.Panel()
        Me.cbScheduleInitialState = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbAssociatedIntegrator = New System.Windows.Forms.ComboBox()
        Me.cbSelectedEventSet = New System.Windows.Forms.ComboBox()
        Me.chkIntegratorUseMatrix = New System.Windows.Forms.CheckBox()
        Me.cbSelectedCauseAndEffectMatrix = New System.Windows.Forms.ComboBox()
        Me.chkIntegratorUseEventSet = New System.Windows.Forms.CheckBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.gridschedules = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip5 = New System.Windows.Forms.ToolStrip()
        Me.btnAddSchedule = New System.Windows.Forms.ToolStripButton()
        Me.btnCopySchedule = New System.Windows.Forms.ToolStripButton()
        Me.btnRemoveSchedule = New System.Windows.Forms.ToolStripButton()
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.chkDynamics = New System.Windows.Forms.CheckBox()
        Me.TabControl1.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.gridselectedset, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip2.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.gridsets, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.TabPage6.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        CType(Me.grdiselmatrix, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip3.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        CType(Me.gridmatrices, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip4.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.panelSelIntegrator.SuspendLayout()
        CType(Me.nupCalcControlFreq, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.nupCalcBalFreq, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.nupCalcEqFreq, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox6.SuspendLayout()
        CType(Me.gridintegrators, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip6.SuspendLayout()
        Me.TabPage7.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.panelSelSchedule.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        CType(Me.gridschedules, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip5.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage6)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage7)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Multiline = True
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.GroupBox1)
        Me.TabPage3.Controls.Add(Me.GroupBox2)
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.gridselectedset)
        Me.GroupBox1.Controls.Add(Me.ToolStrip2)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'gridselectedset
        '
        Me.gridselectedset.AllowUserToAddRows = False
        Me.gridselectedset.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
        Me.gridselectedset.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.id, Me.active, Me.itemname, Me.coleventstamp, Me.coleventtype, Me.associatedobject, Me.associatedproperty, Me.failsafevalue, Me.unit})
        resources.ApplyResources(Me.gridselectedset, "gridselectedset")
        Me.gridselectedset.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.gridselectedset.Name = "gridselectedset"
        Me.gridselectedset.RowHeadersVisible = False
        '
        'id
        '
        resources.ApplyResources(Me.id, "id")
        Me.id.Name = "id"
        '
        'active
        '
        Me.active.FalseValue = "False"
        Me.active.FillWeight = 15.0!
        resources.ApplyResources(Me.active, "active")
        Me.active.Name = "active"
        Me.active.TrueValue = "True"
        '
        'itemname
        '
        Me.itemname.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.itemname.FillWeight = 54.21845!
        resources.ApplyResources(Me.itemname, "itemname")
        Me.itemname.Name = "itemname"
        '
        'coleventstamp
        '
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.Format = "T"
        DataGridViewCellStyle1.NullValue = "00:00:00"
        Me.coleventstamp.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.coleventstamp, "coleventstamp")
        Me.coleventstamp.Name = "coleventstamp"
        '
        'coleventtype
        '
        resources.ApplyResources(Me.coleventtype, "coleventtype")
        Me.coleventtype.Name = "coleventtype"
        '
        'associatedobject
        '
        Me.associatedobject.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        resources.ApplyResources(Me.associatedobject, "associatedobject")
        Me.associatedobject.Name = "associatedobject"
        Me.associatedobject.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.associatedobject.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'associatedproperty
        '
        Me.associatedproperty.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.associatedproperty.FillWeight = 200.0!
        resources.ApplyResources(Me.associatedproperty, "associatedproperty")
        Me.associatedproperty.Name = "associatedproperty"
        Me.associatedproperty.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.associatedproperty.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'failsafevalue
        '
        Me.failsafevalue.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.failsafevalue.DefaultCellStyle = DataGridViewCellStyle2
        Me.failsafevalue.FillWeight = 54.21845!
        resources.ApplyResources(Me.failsafevalue, "failsafevalue")
        Me.failsafevalue.Name = "failsafevalue"
        '
        'unit
        '
        Me.unit.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.unit.FillWeight = 45.18204!
        resources.ApplyResources(Me.unit, "unit")
        Me.unit.Name = "unit"
        Me.unit.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'ToolStrip2
        '
        Me.ToolStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddEvent, Me.btnCopyEvent, Me.btnRemoveEvent})
        resources.ApplyResources(Me.ToolStrip2, "ToolStrip2")
        Me.ToolStrip2.Name = "ToolStrip2"
        '
        'btnAddEvent
        '
        Me.btnAddEvent.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddEvent.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddEvent, "btnAddEvent")
        Me.btnAddEvent.Name = "btnAddEvent"
        '
        'btnCopyEvent
        '
        Me.btnCopyEvent.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopyEvent.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopyEvent, "btnCopyEvent")
        Me.btnCopyEvent.Name = "btnCopyEvent"
        '
        'btnRemoveEvent
        '
        Me.btnRemoveEvent.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveEvent.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveEvent, "btnRemoveEvent")
        Me.btnRemoveEvent.Name = "btnRemoveEvent"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.gridsets)
        Me.GroupBox2.Controls.Add(Me.ToolStrip1)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'gridsets
        '
        Me.gridsets.AllowUserToAddRows = False
        Me.gridsets.AllowUserToDeleteRows = False
        Me.gridsets.AllowUserToResizeColumns = False
        Me.gridsets.AllowUserToResizeRows = False
        Me.gridsets.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridsets.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.gridsets.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridsets.ColumnHeadersVisible = False
        Me.gridsets.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colid, Me.colname})
        resources.ApplyResources(Me.gridsets, "gridsets")
        Me.gridsets.MultiSelect = False
        Me.gridsets.Name = "gridsets"
        Me.gridsets.RowHeadersVisible = False
        Me.gridsets.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'colid
        '
        resources.ApplyResources(Me.colid, "colid")
        Me.colid.Name = "colid"
        Me.colid.ReadOnly = True
        '
        'colname
        '
        resources.ApplyResources(Me.colname, "colname")
        Me.colname.Name = "colname"
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddEventSet, Me.btnCopyEventSet, Me.btnRemoveEventSet})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'btnAddEventSet
        '
        Me.btnAddEventSet.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddEventSet.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddEventSet, "btnAddEventSet")
        Me.btnAddEventSet.Name = "btnAddEventSet"
        '
        'btnCopyEventSet
        '
        Me.btnCopyEventSet.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopyEventSet.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopyEventSet, "btnCopyEventSet")
        Me.btnCopyEventSet.Name = "btnCopyEventSet"
        '
        'btnRemoveEventSet
        '
        Me.btnRemoveEventSet.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveEventSet.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveEventSet, "btnRemoveEventSet")
        Me.btnRemoveEventSet.Name = "btnRemoveEventSet"
        '
        'TabPage6
        '
        Me.TabPage6.Controls.Add(Me.GroupBox3)
        Me.TabPage6.Controls.Add(Me.GroupBox4)
        resources.ApplyResources(Me.TabPage6, "TabPage6")
        Me.TabPage6.Name = "TabPage6"
        Me.TabPage6.UseVisualStyleBackColor = True
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.grdiselmatrix)
        Me.GroupBox3.Controls.Add(Me.ToolStrip3)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'grdiselmatrix
        '
        Me.grdiselmatrix.AllowUserToAddRows = False
        Me.grdiselmatrix.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.DisplayedCells
        Me.grdiselmatrix.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewCheckBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.colmatind, Me.colalarm, Me.DataGridViewComboBoxColumn2, Me.DataGridViewComboBoxColumn3, Me.DataGridViewTextBoxColumn5, Me.DataGridViewComboBoxColumn4})
        resources.ApplyResources(Me.grdiselmatrix, "grdiselmatrix")
        Me.grdiselmatrix.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.grdiselmatrix.Name = "grdiselmatrix"
        Me.grdiselmatrix.RowHeadersVisible = False
        '
        'DataGridViewTextBoxColumn1
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        '
        'DataGridViewCheckBoxColumn1
        '
        Me.DataGridViewCheckBoxColumn1.FalseValue = "False"
        Me.DataGridViewCheckBoxColumn1.FillWeight = 15.0!
        resources.ApplyResources(Me.DataGridViewCheckBoxColumn1, "DataGridViewCheckBoxColumn1")
        Me.DataGridViewCheckBoxColumn1.Name = "DataGridViewCheckBoxColumn1"
        Me.DataGridViewCheckBoxColumn1.TrueValue = "True"
        '
        'DataGridViewTextBoxColumn2
        '
        Me.DataGridViewTextBoxColumn2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.DataGridViewTextBoxColumn2.FillWeight = 54.21845!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        '
        'colmatind
        '
        resources.ApplyResources(Me.colmatind, "colmatind")
        Me.colmatind.Name = "colmatind"
        '
        'colalarm
        '
        resources.ApplyResources(Me.colalarm, "colalarm")
        Me.colalarm.Name = "colalarm"
        '
        'DataGridViewComboBoxColumn2
        '
        Me.DataGridViewComboBoxColumn2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        resources.ApplyResources(Me.DataGridViewComboBoxColumn2, "DataGridViewComboBoxColumn2")
        Me.DataGridViewComboBoxColumn2.Name = "DataGridViewComboBoxColumn2"
        Me.DataGridViewComboBoxColumn2.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridViewComboBoxColumn2.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'DataGridViewComboBoxColumn3
        '
        Me.DataGridViewComboBoxColumn3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.DataGridViewComboBoxColumn3.FillWeight = 200.0!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn3, "DataGridViewComboBoxColumn3")
        Me.DataGridViewComboBoxColumn3.Name = "DataGridViewComboBoxColumn3"
        Me.DataGridViewComboBoxColumn3.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridViewComboBoxColumn3.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'DataGridViewTextBoxColumn5
        '
        Me.DataGridViewTextBoxColumn5.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn5.DefaultCellStyle = DataGridViewCellStyle3
        Me.DataGridViewTextBoxColumn5.FillWeight = 54.21845!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        '
        'DataGridViewComboBoxColumn4
        '
        Me.DataGridViewComboBoxColumn4.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCells
        Me.DataGridViewComboBoxColumn4.FillWeight = 45.18204!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn4, "DataGridViewComboBoxColumn4")
        Me.DataGridViewComboBoxColumn4.Name = "DataGridViewComboBoxColumn4"
        Me.DataGridViewComboBoxColumn4.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridViewComboBoxColumn4.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'ToolStrip3
        '
        Me.ToolStrip3.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddMatrixItem, Me.btnCopyMatrixItem, Me.btnRemoveMatrixItem})
        resources.ApplyResources(Me.ToolStrip3, "ToolStrip3")
        Me.ToolStrip3.Name = "ToolStrip3"
        '
        'btnAddMatrixItem
        '
        Me.btnAddMatrixItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddMatrixItem.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddMatrixItem, "btnAddMatrixItem")
        Me.btnAddMatrixItem.Name = "btnAddMatrixItem"
        '
        'btnCopyMatrixItem
        '
        Me.btnCopyMatrixItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopyMatrixItem.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopyMatrixItem, "btnCopyMatrixItem")
        Me.btnCopyMatrixItem.Name = "btnCopyMatrixItem"
        '
        'btnRemoveMatrixItem
        '
        Me.btnRemoveMatrixItem.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveMatrixItem.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveMatrixItem, "btnRemoveMatrixItem")
        Me.btnRemoveMatrixItem.Name = "btnRemoveMatrixItem"
        '
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.gridmatrices)
        Me.GroupBox4.Controls.Add(Me.ToolStrip4)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'gridmatrices
        '
        Me.gridmatrices.AllowUserToAddRows = False
        Me.gridmatrices.AllowUserToDeleteRows = False
        Me.gridmatrices.AllowUserToResizeColumns = False
        Me.gridmatrices.AllowUserToResizeRows = False
        Me.gridmatrices.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridmatrices.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.gridmatrices.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridmatrices.ColumnHeadersVisible = False
        Me.gridmatrices.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn7, Me.DataGridViewTextBoxColumn8})
        resources.ApplyResources(Me.gridmatrices, "gridmatrices")
        Me.gridmatrices.MultiSelect = False
        Me.gridmatrices.Name = "gridmatrices"
        Me.gridmatrices.RowHeadersVisible = False
        Me.gridmatrices.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'DataGridViewTextBoxColumn7
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn7, "DataGridViewTextBoxColumn7")
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        Me.DataGridViewTextBoxColumn7.ReadOnly = True
        '
        'DataGridViewTextBoxColumn8
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn8, "DataGridViewTextBoxColumn8")
        Me.DataGridViewTextBoxColumn8.Name = "DataGridViewTextBoxColumn8"
        '
        'ToolStrip4
        '
        Me.ToolStrip4.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddMatrix, Me.btnCopyMatrix, Me.btnRemoveMatrix})
        resources.ApplyResources(Me.ToolStrip4, "ToolStrip4")
        Me.ToolStrip4.Name = "ToolStrip4"
        '
        'btnAddMatrix
        '
        Me.btnAddMatrix.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddMatrix.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddMatrix, "btnAddMatrix")
        Me.btnAddMatrix.Name = "btnAddMatrix"
        '
        'btnCopyMatrix
        '
        Me.btnCopyMatrix.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopyMatrix.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopyMatrix, "btnCopyMatrix")
        Me.btnCopyMatrix.Name = "btnCopyMatrix"
        '
        'btnRemoveMatrix
        '
        Me.btnRemoveMatrix.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveMatrix.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveMatrix, "btnRemoveMatrix")
        Me.btnRemoveMatrix.Name = "btnRemoveMatrix"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.GroupBox5)
        Me.TabPage2.Controls.Add(Me.GroupBox6)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.panelSelIntegrator)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        '
        'panelSelIntegrator
        '
        Me.panelSelIntegrator.Controls.Add(Me.dtpIntegratorDuration)
        Me.panelSelIntegrator.Controls.Add(Me.dtpIntegrationStep)
        Me.panelSelIntegrator.Controls.Add(Me.nupCalcControlFreq)
        Me.panelSelIntegrator.Controls.Add(Me.nupCalcBalFreq)
        Me.panelSelIntegrator.Controls.Add(Me.nupCalcEqFreq)
        Me.panelSelIntegrator.Controls.Add(Me.Label8)
        Me.panelSelIntegrator.Controls.Add(Me.Label7)
        Me.panelSelIntegrator.Controls.Add(Me.Label6)
        Me.panelSelIntegrator.Controls.Add(Me.Label5)
        Me.panelSelIntegrator.Controls.Add(Me.Label4)
        Me.panelSelIntegrator.Controls.Add(Me.Label2)
        Me.panelSelIntegrator.Controls.Add(Me.Label1)
        resources.ApplyResources(Me.panelSelIntegrator, "panelSelIntegrator")
        Me.panelSelIntegrator.Name = "panelSelIntegrator"
        '
        'dtpIntegratorDuration
        '
        Me.dtpIntegratorDuration.Format = System.Windows.Forms.DateTimePickerFormat.Time
        resources.ApplyResources(Me.dtpIntegratorDuration, "dtpIntegratorDuration")
        Me.dtpIntegratorDuration.Name = "dtpIntegratorDuration"
        Me.dtpIntegratorDuration.Value = New Date(1753, 1, 1, 0, 0, 0, 0)
        '
        'dtpIntegrationStep
        '
        Me.dtpIntegrationStep.Format = System.Windows.Forms.DateTimePickerFormat.Time
        resources.ApplyResources(Me.dtpIntegrationStep, "dtpIntegrationStep")
        Me.dtpIntegrationStep.Name = "dtpIntegrationStep"
        Me.dtpIntegrationStep.Value = New Date(1753, 1, 1, 0, 0, 0, 0)
        '
        'nupCalcControlFreq
        '
        resources.ApplyResources(Me.nupCalcControlFreq, "nupCalcControlFreq")
        Me.nupCalcControlFreq.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nupCalcControlFreq.Name = "nupCalcControlFreq"
        Me.nupCalcControlFreq.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'nupCalcBalFreq
        '
        resources.ApplyResources(Me.nupCalcBalFreq, "nupCalcBalFreq")
        Me.nupCalcBalFreq.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nupCalcBalFreq.Name = "nupCalcBalFreq"
        Me.nupCalcBalFreq.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'nupCalcEqFreq
        '
        resources.ApplyResources(Me.nupCalcEqFreq, "nupCalcEqFreq")
        Me.nupCalcEqFreq.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nupCalcEqFreq.Name = "nupCalcEqFreq"
        Me.nupCalcEqFreq.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.gridintegrators)
        Me.GroupBox6.Controls.Add(Me.ToolStrip6)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'gridintegrators
        '
        Me.gridintegrators.AllowUserToAddRows = False
        Me.gridintegrators.AllowUserToDeleteRows = False
        Me.gridintegrators.AllowUserToResizeColumns = False
        Me.gridintegrators.AllowUserToResizeRows = False
        Me.gridintegrators.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridintegrators.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.gridintegrators.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridintegrators.ColumnHeadersVisible = False
        Me.gridintegrators.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn11, Me.DataGridViewTextBoxColumn12})
        resources.ApplyResources(Me.gridintegrators, "gridintegrators")
        Me.gridintegrators.MultiSelect = False
        Me.gridintegrators.Name = "gridintegrators"
        Me.gridintegrators.RowHeadersVisible = False
        Me.gridintegrators.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'DataGridViewTextBoxColumn11
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn11, "DataGridViewTextBoxColumn11")
        Me.DataGridViewTextBoxColumn11.Name = "DataGridViewTextBoxColumn11"
        Me.DataGridViewTextBoxColumn11.ReadOnly = True
        '
        'DataGridViewTextBoxColumn12
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn12, "DataGridViewTextBoxColumn12")
        Me.DataGridViewTextBoxColumn12.Name = "DataGridViewTextBoxColumn12"
        '
        'ToolStrip6
        '
        Me.ToolStrip6.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddIntegrator, Me.btnCopyIntegrator, Me.btnRemoveIntegrator})
        resources.ApplyResources(Me.ToolStrip6, "ToolStrip6")
        Me.ToolStrip6.Name = "ToolStrip6"
        '
        'btnAddIntegrator
        '
        Me.btnAddIntegrator.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddIntegrator.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddIntegrator, "btnAddIntegrator")
        Me.btnAddIntegrator.Name = "btnAddIntegrator"
        '
        'btnCopyIntegrator
        '
        Me.btnCopyIntegrator.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopyIntegrator.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopyIntegrator, "btnCopyIntegrator")
        Me.btnCopyIntegrator.Name = "btnCopyIntegrator"
        '
        'btnRemoveIntegrator
        '
        Me.btnRemoveIntegrator.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveIntegrator.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveIntegrator, "btnRemoveIntegrator")
        Me.btnRemoveIntegrator.Name = "btnRemoveIntegrator"
        '
        'TabPage7
        '
        Me.TabPage7.Controls.Add(Me.GroupBox7)
        Me.TabPage7.Controls.Add(Me.GroupBox8)
        resources.ApplyResources(Me.TabPage7, "TabPage7")
        Me.TabPage7.Name = "TabPage7"
        Me.TabPage7.UseVisualStyleBackColor = True
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.panelSelSchedule)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        '
        'panelSelSchedule
        '
        Me.panelSelSchedule.Controls.Add(Me.cbScheduleInitialState)
        Me.panelSelSchedule.Controls.Add(Me.Label3)
        Me.panelSelSchedule.Controls.Add(Me.cbAssociatedIntegrator)
        Me.panelSelSchedule.Controls.Add(Me.cbSelectedEventSet)
        Me.panelSelSchedule.Controls.Add(Me.chkIntegratorUseMatrix)
        Me.panelSelSchedule.Controls.Add(Me.cbSelectedCauseAndEffectMatrix)
        Me.panelSelSchedule.Controls.Add(Me.chkIntegratorUseEventSet)
        Me.panelSelSchedule.Controls.Add(Me.Label9)
        resources.ApplyResources(Me.panelSelSchedule, "panelSelSchedule")
        Me.panelSelSchedule.Name = "panelSelSchedule"
        '
        'cbScheduleInitialState
        '
        Me.cbScheduleInitialState.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScheduleInitialState.FormattingEnabled = True
        resources.ApplyResources(Me.cbScheduleInitialState, "cbScheduleInitialState")
        Me.cbScheduleInitialState.Name = "cbScheduleInitialState"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbAssociatedIntegrator
        '
        Me.cbAssociatedIntegrator.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbAssociatedIntegrator.FormattingEnabled = True
        resources.ApplyResources(Me.cbAssociatedIntegrator, "cbAssociatedIntegrator")
        Me.cbAssociatedIntegrator.Name = "cbAssociatedIntegrator"
        '
        'cbSelectedEventSet
        '
        Me.cbSelectedEventSet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSelectedEventSet.FormattingEnabled = True
        resources.ApplyResources(Me.cbSelectedEventSet, "cbSelectedEventSet")
        Me.cbSelectedEventSet.Name = "cbSelectedEventSet"
        '
        'chkIntegratorUseMatrix
        '
        resources.ApplyResources(Me.chkIntegratorUseMatrix, "chkIntegratorUseMatrix")
        Me.chkIntegratorUseMatrix.Name = "chkIntegratorUseMatrix"
        Me.chkIntegratorUseMatrix.UseVisualStyleBackColor = True
        '
        'cbSelectedCauseAndEffectMatrix
        '
        Me.cbSelectedCauseAndEffectMatrix.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSelectedCauseAndEffectMatrix.FormattingEnabled = True
        resources.ApplyResources(Me.cbSelectedCauseAndEffectMatrix, "cbSelectedCauseAndEffectMatrix")
        Me.cbSelectedCauseAndEffectMatrix.Name = "cbSelectedCauseAndEffectMatrix"
        '
        'chkIntegratorUseEventSet
        '
        resources.ApplyResources(Me.chkIntegratorUseEventSet, "chkIntegratorUseEventSet")
        Me.chkIntegratorUseEventSet.Name = "chkIntegratorUseEventSet"
        Me.chkIntegratorUseEventSet.UseVisualStyleBackColor = True
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'GroupBox8
        '
        resources.ApplyResources(Me.GroupBox8, "GroupBox8")
        Me.GroupBox8.Controls.Add(Me.gridschedules)
        Me.GroupBox8.Controls.Add(Me.ToolStrip5)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.TabStop = False
        '
        'gridschedules
        '
        Me.gridschedules.AllowUserToAddRows = False
        Me.gridschedules.AllowUserToDeleteRows = False
        Me.gridschedules.AllowUserToResizeColumns = False
        Me.gridschedules.AllowUserToResizeRows = False
        Me.gridschedules.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridschedules.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.gridschedules.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridschedules.ColumnHeadersVisible = False
        Me.gridschedules.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn6})
        resources.ApplyResources(Me.gridschedules, "gridschedules")
        Me.gridschedules.MultiSelect = False
        Me.gridschedules.Name = "gridschedules"
        Me.gridschedules.RowHeadersVisible = False
        Me.gridschedules.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'DataGridViewTextBoxColumn3
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.ReadOnly = True
        '
        'DataGridViewTextBoxColumn6
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        '
        'ToolStrip5
        '
        Me.ToolStrip5.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.btnAddSchedule, Me.btnCopySchedule, Me.btnRemoveSchedule})
        resources.ApplyResources(Me.ToolStrip5, "ToolStrip5")
        Me.ToolStrip5.Name = "ToolStrip5"
        '
        'btnAddSchedule
        '
        Me.btnAddSchedule.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnAddSchedule.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.btnAddSchedule, "btnAddSchedule")
        Me.btnAddSchedule.Name = "btnAddSchedule"
        '
        'btnCopySchedule
        '
        Me.btnCopySchedule.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnCopySchedule.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.btnCopySchedule, "btnCopySchedule")
        Me.btnCopySchedule.Name = "btnCopySchedule"
        '
        'btnRemoveSchedule
        '
        Me.btnRemoveSchedule.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRemoveSchedule.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.btnRemoveSchedule, "btnRemoveSchedule")
        Me.btnRemoveSchedule.Name = "btnRemoveSchedule"
        '
        'TabPage5
        '
        resources.ApplyResources(Me.TabPage5, "TabPage5")
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        '
        'chkDynamics
        '
        resources.ApplyResources(Me.chkDynamics, "chkDynamics")
        Me.chkDynamics.BackColor = System.Drawing.Color.DarkRed
        Me.chkDynamics.ForeColor = System.Drawing.Color.White
        Me.chkDynamics.Name = "chkDynamics"
        Me.chkDynamics.UseVisualStyleBackColor = False
        '
        'FormDynamicsManager
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.chkDynamics)
        Me.Controls.Add(Me.lblStatus)
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "FormDynamicsManager"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.gridselectedset, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip2.ResumeLayout(False)
        Me.ToolStrip2.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.gridsets, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.TabPage6.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.grdiselmatrix, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip3.ResumeLayout(False)
        Me.ToolStrip3.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        CType(Me.gridmatrices, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip4.ResumeLayout(False)
        Me.ToolStrip4.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.GroupBox5.ResumeLayout(False)
        Me.panelSelIntegrator.ResumeLayout(False)
        Me.panelSelIntegrator.PerformLayout()
        CType(Me.nupCalcControlFreq, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.nupCalcBalFreq, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.nupCalcEqFreq, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        CType(Me.gridintegrators, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip6.ResumeLayout(False)
        Me.ToolStrip6.PerformLayout()
        Me.TabPage7.ResumeLayout(False)
        Me.GroupBox7.ResumeLayout(False)
        Me.panelSelSchedule.ResumeLayout(False)
        Me.panelSelSchedule.PerformLayout()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        CType(Me.gridschedules, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip5.ResumeLayout(False)
        Me.ToolStrip5.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents TabPage5 As TabPage
    Friend WithEvents lblStatus As Label
    Friend WithEvents chkDynamics As CheckBox
    Friend WithEvents Button1 As Button
    Friend WithEvents TabPage6 As TabPage
    Friend WithEvents TabPage7 As TabPage
    Friend WithEvents GroupBox2 As GroupBox
    Friend WithEvents gridsets As DataGridView
    Friend WithEvents colid As DataGridViewTextBoxColumn
    Friend WithEvents colname As DataGridViewTextBoxColumn
    Public WithEvents ToolStrip1 As ToolStrip
    Friend WithEvents btnAddEventSet As ToolStripButton
    Public WithEvents btnCopyEventSet As ToolStripButton
    Public WithEvents btnRemoveEventSet As ToolStripButton
    Friend WithEvents GroupBox1 As GroupBox
    Friend WithEvents gridselectedset As DataGridView
    Public WithEvents ToolStrip2 As ToolStrip
    Friend WithEvents btnAddEvent As ToolStripButton
    Public WithEvents btnCopyEvent As ToolStripButton
    Public WithEvents btnRemoveEvent As ToolStripButton
    Friend WithEvents GroupBox3 As GroupBox
    Friend WithEvents grdiselmatrix As DataGridView
    Public WithEvents ToolStrip3 As ToolStrip
    Friend WithEvents btnAddMatrixItem As ToolStripButton
    Public WithEvents btnCopyMatrixItem As ToolStripButton
    Public WithEvents btnRemoveMatrixItem As ToolStripButton
    Friend WithEvents GroupBox4 As GroupBox
    Friend WithEvents gridmatrices As DataGridView
    Friend WithEvents DataGridViewTextBoxColumn7 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn8 As DataGridViewTextBoxColumn
    Public WithEvents ToolStrip4 As ToolStrip
    Friend WithEvents btnAddMatrix As ToolStripButton
    Public WithEvents btnCopyMatrix As ToolStripButton
    Public WithEvents btnRemoveMatrix As ToolStripButton
    Friend WithEvents GroupBox5 As GroupBox
    Friend WithEvents GroupBox6 As GroupBox
    Friend WithEvents gridintegrators As DataGridView
    Friend WithEvents DataGridViewTextBoxColumn11 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn12 As DataGridViewTextBoxColumn
    Public WithEvents ToolStrip6 As ToolStrip
    Friend WithEvents btnAddIntegrator As ToolStripButton
    Public WithEvents btnCopyIntegrator As ToolStripButton
    Public WithEvents btnRemoveIntegrator As ToolStripButton
    Friend WithEvents GroupBox7 As GroupBox
    Friend WithEvents GroupBox8 As GroupBox
    Friend WithEvents gridschedules As DataGridView
    Friend WithEvents DataGridViewTextBoxColumn3 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn6 As DataGridViewTextBoxColumn
    Public WithEvents ToolStrip5 As ToolStrip
    Friend WithEvents btnAddSchedule As ToolStripButton
    Public WithEvents btnCopySchedule As ToolStripButton
    Public WithEvents btnRemoveSchedule As ToolStripButton
    Friend WithEvents panelSelIntegrator As Panel
    Friend WithEvents panelSelSchedule As Panel
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents Label8 As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents Label6 As Label
    Friend WithEvents Label5 As Label
    Friend WithEvents Label4 As Label
    Friend WithEvents dtpIntegratorDuration As DateTimePicker
    Friend WithEvents dtpIntegrationStep As DateTimePicker
    Friend WithEvents nupCalcControlFreq As NumericUpDown
    Friend WithEvents nupCalcBalFreq As NumericUpDown
    Friend WithEvents nupCalcEqFreq As NumericUpDown
    Friend WithEvents Label9 As Label
    Friend WithEvents chkIntegratorUseMatrix As CheckBox
    Friend WithEvents cbSelectedCauseAndEffectMatrix As ComboBox
    Friend WithEvents chkIntegratorUseEventSet As CheckBox
    Friend WithEvents cbAssociatedIntegrator As ComboBox
    Friend WithEvents cbSelectedEventSet As ComboBox
    Friend WithEvents DataGridViewTextBoxColumn1 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewCheckBoxColumn1 As DataGridViewCheckBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As DataGridViewTextBoxColumn
    Friend WithEvents colmatind As DataGridViewComboBoxColumn
    Friend WithEvents colalarm As DataGridViewComboBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn2 As DataGridViewComboBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn3 As DataGridViewComboBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn5 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn4 As DataGridViewComboBoxColumn
    Friend WithEvents cbScheduleInitialState As ComboBox
    Friend WithEvents Label3 As Label
    Friend WithEvents id As DataGridViewTextBoxColumn
    Friend WithEvents active As DataGridViewCheckBoxColumn
    Friend WithEvents itemname As DataGridViewTextBoxColumn
    Friend WithEvents coleventstamp As DataGridViewTextBoxColumn
    Friend WithEvents coleventtype As DataGridViewComboBoxColumn
    Friend WithEvents associatedobject As DataGridViewComboBoxColumn
    Friend WithEvents associatedproperty As DataGridViewComboBoxColumn
    Friend WithEvents failsafevalue As DataGridViewTextBoxColumn
    Friend WithEvents unit As DataGridViewTextBoxColumn
End Class
