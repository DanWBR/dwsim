<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormOptimization
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormOptimization))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.btnDeleteCase = New System.Windows.Forms.Button()
        Me.btnSaveCase = New System.Windows.Forms.Button()
        Me.btnCopyCase = New System.Windows.Forms.Button()
        Me.btnNewCase = New System.Windows.Forms.Button()
        Me.lbCases = New System.Windows.Forms.ListBox()
        Me.tbCaseDesc = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbCaseName = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GBvars = New System.Windows.Forms.GroupBox()
        Me.dgVariables = New System.Windows.Forms.DataGridView()
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column10 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.tsbAddVar = New System.Windows.Forms.ToolStripButton()
        Me.tsbDelVar = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.GBconfig = New System.Windows.Forms.GroupBox()
        Me.FlowLayoutPanel2 = New System.Windows.Forms.FlowLayoutPanel()
        Me.rbVariable = New System.Windows.Forms.RadioButton()
        Me.rbExpression = New System.Windows.Forms.RadioButton()
        Me.FlowLayoutPanel1 = New System.Windows.Forms.FlowLayoutPanel()
        Me.rbMinimize = New System.Windows.Forms.RadioButton()
        Me.rbMaximize = New System.Windows.Forms.RadioButton()
        Me.tbToleranceValue = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.tbMaxIterations = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.GBexpression = New System.Windows.Forms.GroupBox()
        Me.btnClear = New System.Windows.Forms.Button()
        Me.btnVerify = New System.Windows.Forms.Button()
        Me.tbCurrentValue = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.tbExpression = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.tbH = New System.Windows.Forms.TextBox()
        Me.tbBarrierMultiplier = New System.Windows.Forms.TextBox()
        Me.rb4PointDeriv = New System.Windows.Forms.RadioButton()
        Me.rb2PointDeriv = New System.Windows.Forms.RadioButton()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.ComboBox1 = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.btnRun = New System.Windows.Forms.Button()
        Me.btnAbort = New System.Windows.Forms.Button()
        Me.btnRestore = New System.Windows.Forms.Button()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.grProgress = New ZedGraph.ZedGraphControl()
        Me.dckMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.FloatToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockLeftToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockRightToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockTopToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockBottomToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockLeftAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockRightAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockTopAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockBottomAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DocumentToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GroupBox2.SuspendLayout()
        Me.GBvars.SuspendLayout()
        CType(Me.dgVariables, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.GBconfig.SuspendLayout()
        Me.FlowLayoutPanel2.SuspendLayout()
        Me.FlowLayoutPanel1.SuspendLayout()
        Me.GBexpression.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.btnDeleteCase)
        Me.GroupBox2.Controls.Add(Me.btnSaveCase)
        Me.GroupBox2.Controls.Add(Me.btnCopyCase)
        Me.GroupBox2.Controls.Add(Me.btnNewCase)
        Me.GroupBox2.Controls.Add(Me.lbCases)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'btnDeleteCase
        '
        resources.ApplyResources(Me.btnDeleteCase, "btnDeleteCase")
        Me.btnDeleteCase.Name = "btnDeleteCase"
        Me.btnDeleteCase.UseVisualStyleBackColor = True
        '
        'btnSaveCase
        '
        resources.ApplyResources(Me.btnSaveCase, "btnSaveCase")
        Me.btnSaveCase.Name = "btnSaveCase"
        Me.btnSaveCase.UseVisualStyleBackColor = True
        '
        'btnCopyCase
        '
        resources.ApplyResources(Me.btnCopyCase, "btnCopyCase")
        Me.btnCopyCase.Name = "btnCopyCase"
        Me.btnCopyCase.UseVisualStyleBackColor = True
        '
        'btnNewCase
        '
        resources.ApplyResources(Me.btnNewCase, "btnNewCase")
        Me.btnNewCase.Name = "btnNewCase"
        Me.btnNewCase.UseVisualStyleBackColor = True
        '
        'lbCases
        '
        resources.ApplyResources(Me.lbCases, "lbCases")
        Me.lbCases.FormattingEnabled = True
        Me.lbCases.Name = "lbCases"
        '
        'tbCaseDesc
        '
        resources.ApplyResources(Me.tbCaseDesc, "tbCaseDesc")
        Me.tbCaseDesc.Name = "tbCaseDesc"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'tbCaseName
        '
        resources.ApplyResources(Me.tbCaseName, "tbCaseName")
        Me.tbCaseName.Name = "tbCaseName"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'GBvars
        '
        resources.ApplyResources(Me.GBvars, "GBvars")
        Me.GBvars.Controls.Add(Me.dgVariables)
        Me.GBvars.Controls.Add(Me.ToolStrip1)
        Me.GBvars.Name = "GBvars"
        Me.GBvars.TabStop = False
        '
        'dgVariables
        '
        resources.ApplyResources(Me.dgVariables, "dgVariables")
        Me.dgVariables.AllowUserToAddRows = False
        Me.dgVariables.AllowUserToDeleteRows = False
        Me.dgVariables.AllowUserToResizeRows = False
        Me.dgVariables.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgVariables.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedCells
        Me.dgVariables.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        Me.dgVariables.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        Me.dgVariables.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.dgVariables.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgVariables.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column8, Me.Column1, Me.Column2, Me.Column3, Me.Column4, Me.Column6, Me.Column7, Me.Column10, Me.Column5, Me.Column9})
        Me.dgVariables.Name = "dgVariables"
        Me.dgVariables.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        '
        'Column8
        '
        resources.ApplyResources(Me.Column8, "Column8")
        Me.Column8.Name = "Column8"
        Me.Column8.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column1
        '
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column2
        '
        Me.Column2.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        '
        'Column3
        '
        Me.Column3.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        Me.Column3.Sorted = True
        '
        'Column4
        '
        Me.Column4.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        '
        'Column6
        '
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        Me.Column6.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column7
        '
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        Me.Column7.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column10
        '
        resources.ApplyResources(Me.Column10, "Column10")
        Me.Column10.Name = "Column10"
        Me.Column10.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column5
        '
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        Me.Column5.ReadOnly = True
        Me.Column5.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column9
        '
        resources.ApplyResources(Me.Column9, "Column9")
        Me.Column9.Name = "Column9"
        Me.Column9.ReadOnly = True
        Me.Column9.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbAddVar, Me.tsbDelVar, Me.ToolStripSeparator1, Me.ToolStripButton1})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'tsbAddVar
        '
        resources.ApplyResources(Me.tsbAddVar, "tsbAddVar")
        Me.tsbAddVar.Image = Global.DWSIM.My.Resources.Resources.add
        Me.tsbAddVar.Name = "tsbAddVar"
        '
        'tsbDelVar
        '
        resources.ApplyResources(Me.tsbDelVar, "tsbDelVar")
        Me.tsbDelVar.Image = Global.DWSIM.My.Resources.Resources.delete1
        Me.tsbDelVar.Name = "tsbDelVar"
        '
        'ToolStripSeparator1
        '
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'GBconfig
        '
        resources.ApplyResources(Me.GBconfig, "GBconfig")
        Me.GBconfig.Controls.Add(Me.FlowLayoutPanel2)
        Me.GBconfig.Controls.Add(Me.FlowLayoutPanel1)
        Me.GBconfig.Controls.Add(Me.tbToleranceValue)
        Me.GBconfig.Controls.Add(Me.Label6)
        Me.GBconfig.Controls.Add(Me.tbMaxIterations)
        Me.GBconfig.Controls.Add(Me.Label5)
        Me.GBconfig.Controls.Add(Me.Label3)
        Me.GBconfig.Controls.Add(Me.Label2)
        Me.GBconfig.Name = "GBconfig"
        Me.GBconfig.TabStop = False
        '
        'FlowLayoutPanel2
        '
        resources.ApplyResources(Me.FlowLayoutPanel2, "FlowLayoutPanel2")
        Me.FlowLayoutPanel2.Controls.Add(Me.rbVariable)
        Me.FlowLayoutPanel2.Controls.Add(Me.rbExpression)
        Me.FlowLayoutPanel2.Name = "FlowLayoutPanel2"
        '
        'rbVariable
        '
        resources.ApplyResources(Me.rbVariable, "rbVariable")
        Me.rbVariable.Checked = True
        Me.rbVariable.Name = "rbVariable"
        Me.rbVariable.TabStop = True
        Me.rbVariable.UseVisualStyleBackColor = True
        '
        'rbExpression
        '
        resources.ApplyResources(Me.rbExpression, "rbExpression")
        Me.rbExpression.Name = "rbExpression"
        Me.rbExpression.UseVisualStyleBackColor = True
        '
        'FlowLayoutPanel1
        '
        resources.ApplyResources(Me.FlowLayoutPanel1, "FlowLayoutPanel1")
        Me.FlowLayoutPanel1.Controls.Add(Me.rbMinimize)
        Me.FlowLayoutPanel1.Controls.Add(Me.rbMaximize)
        Me.FlowLayoutPanel1.Name = "FlowLayoutPanel1"
        '
        'rbMinimize
        '
        resources.ApplyResources(Me.rbMinimize, "rbMinimize")
        Me.rbMinimize.Checked = True
        Me.rbMinimize.Name = "rbMinimize"
        Me.rbMinimize.TabStop = True
        Me.rbMinimize.UseVisualStyleBackColor = True
        '
        'rbMaximize
        '
        resources.ApplyResources(Me.rbMaximize, "rbMaximize")
        Me.rbMaximize.Name = "rbMaximize"
        Me.rbMaximize.UseVisualStyleBackColor = True
        '
        'tbToleranceValue
        '
        resources.ApplyResources(Me.tbToleranceValue, "tbToleranceValue")
        Me.tbToleranceValue.Name = "tbToleranceValue"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'tbMaxIterations
        '
        resources.ApplyResources(Me.tbMaxIterations, "tbMaxIterations")
        Me.tbMaxIterations.Name = "tbMaxIterations"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'GBexpression
        '
        resources.ApplyResources(Me.GBexpression, "GBexpression")
        Me.GBexpression.Controls.Add(Me.btnClear)
        Me.GBexpression.Controls.Add(Me.btnVerify)
        Me.GBexpression.Controls.Add(Me.tbCurrentValue)
        Me.GBexpression.Controls.Add(Me.Label8)
        Me.GBexpression.Controls.Add(Me.tbExpression)
        Me.GBexpression.Controls.Add(Me.Label7)
        Me.GBexpression.Name = "GBexpression"
        Me.GBexpression.TabStop = False
        '
        'btnClear
        '
        resources.ApplyResources(Me.btnClear, "btnClear")
        Me.btnClear.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.btnClear.Name = "btnClear"
        Me.btnClear.UseVisualStyleBackColor = True
        '
        'btnVerify
        '
        resources.ApplyResources(Me.btnVerify, "btnVerify")
        Me.btnVerify.Image = Global.DWSIM.My.Resources.Resources.tick1
        Me.btnVerify.Name = "btnVerify"
        Me.btnVerify.UseVisualStyleBackColor = True
        '
        'tbCurrentValue
        '
        resources.ApplyResources(Me.tbCurrentValue, "tbCurrentValue")
        Me.tbCurrentValue.Name = "tbCurrentValue"
        Me.tbCurrentValue.ReadOnly = True
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'tbExpression
        '
        resources.ApplyResources(Me.tbExpression, "tbExpression")
        Me.tbExpression.Name = "tbExpression"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.tbCaseDesc)
        Me.Panel1.Controls.Add(Me.GroupBox8)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.GroupBox7)
        Me.Panel1.Controls.Add(Me.tbCaseName)
        Me.Panel1.Controls.Add(Me.GBexpression)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.GBvars)
        Me.Panel1.Controls.Add(Me.GBconfig)
        Me.Panel1.Name = "Panel1"
        '
        'GroupBox8
        '
        resources.ApplyResources(Me.GroupBox8, "GroupBox8")
        Me.GroupBox8.Controls.Add(Me.Label12)
        Me.GroupBox8.Controls.Add(Me.tbH)
        Me.GroupBox8.Controls.Add(Me.tbBarrierMultiplier)
        Me.GroupBox8.Controls.Add(Me.rb4PointDeriv)
        Me.GroupBox8.Controls.Add(Me.rb2PointDeriv)
        Me.GroupBox8.Controls.Add(Me.Label11)
        Me.GroupBox8.Controls.Add(Me.Label10)
        Me.GroupBox8.Controls.Add(Me.ComboBox1)
        Me.GroupBox8.Controls.Add(Me.Label9)
        Me.GroupBox8.Controls.Add(Me.btnRun)
        Me.GroupBox8.Controls.Add(Me.btnAbort)
        Me.GroupBox8.Controls.Add(Me.btnRestore)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.TabStop = False
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'tbH
        '
        resources.ApplyResources(Me.tbH, "tbH")
        Me.tbH.Name = "tbH"
        '
        'tbBarrierMultiplier
        '
        resources.ApplyResources(Me.tbBarrierMultiplier, "tbBarrierMultiplier")
        Me.tbBarrierMultiplier.Name = "tbBarrierMultiplier"
        '
        'rb4PointDeriv
        '
        resources.ApplyResources(Me.rb4PointDeriv, "rb4PointDeriv")
        Me.rb4PointDeriv.Name = "rb4PointDeriv"
        Me.rb4PointDeriv.TabStop = True
        Me.rb4PointDeriv.UseVisualStyleBackColor = True
        '
        'rb2PointDeriv
        '
        resources.ApplyResources(Me.rb2PointDeriv, "rb2PointDeriv")
        Me.rb2PointDeriv.Name = "rb2PointDeriv"
        Me.rb2PointDeriv.TabStop = True
        Me.rb2PointDeriv.UseVisualStyleBackColor = True
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'ComboBox1
        '
        resources.ApplyResources(Me.ComboBox1, "ComboBox1")
        Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox1.FormattingEnabled = True
        Me.ComboBox1.Items.AddRange(New Object() {resources.GetString("ComboBox1.Items"), resources.GetString("ComboBox1.Items1"), resources.GetString("ComboBox1.Items2"), resources.GetString("ComboBox1.Items3"), resources.GetString("ComboBox1.Items4"), resources.GetString("ComboBox1.Items5"), resources.GetString("ComboBox1.Items6"), resources.GetString("ComboBox1.Items7"), resources.GetString("ComboBox1.Items8"), resources.GetString("ComboBox1.Items9"), resources.GetString("ComboBox1.Items10")})
        Me.ComboBox1.Name = "ComboBox1"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'btnRun
        '
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.Name = "btnRun"
        Me.btnRun.UseVisualStyleBackColor = True
        '
        'btnAbort
        '
        resources.ApplyResources(Me.btnAbort, "btnAbort")
        Me.btnAbort.Name = "btnAbort"
        Me.btnAbort.UseVisualStyleBackColor = True
        '
        'btnRestore
        '
        resources.ApplyResources(Me.btnRestore, "btnRestore")
        Me.btnRestore.Name = "btnRestore"
        Me.btnRestore.UseVisualStyleBackColor = True
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.grProgress)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        '
        'grProgress
        '
        resources.ApplyResources(Me.grProgress, "grProgress")
        Me.grProgress.IsAntiAlias = True
        Me.grProgress.IsAutoScrollRange = True
        Me.grProgress.IsShowCopyMessage = False
        Me.grProgress.Name = "grProgress"
        Me.grProgress.ScrollGrace = 0R
        Me.grProgress.ScrollMaxX = 0R
        Me.grProgress.ScrollMaxY = 0R
        Me.grProgress.ScrollMaxY2 = 0R
        Me.grProgress.ScrollMinX = 0R
        Me.grProgress.ScrollMinY = 0R
        Me.grProgress.ScrollMinY2 = 0R
        '
        'dckMenu
        '
        resources.ApplyResources(Me.dckMenu, "dckMenu")
        Me.dckMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FloatToolStripMenuItem, Me.DockLeftToolStripMenuItem, Me.DockRightToolStripMenuItem, Me.DockTopToolStripMenuItem, Me.DockBottomToolStripMenuItem, Me.DockLeftAutoHideToolStripMenuItem, Me.DockRightAutoHideToolStripMenuItem, Me.DockTopAutoHideToolStripMenuItem, Me.DockBottomAutoHideToolStripMenuItem, Me.DocumentToolStripMenuItem})
        Me.dckMenu.Name = "dckMenu"
        '
        'FloatToolStripMenuItem
        '
        resources.ApplyResources(Me.FloatToolStripMenuItem, "FloatToolStripMenuItem")
        Me.FloatToolStripMenuItem.Name = "FloatToolStripMenuItem"
        '
        'DockLeftToolStripMenuItem
        '
        resources.ApplyResources(Me.DockLeftToolStripMenuItem, "DockLeftToolStripMenuItem")
        Me.DockLeftToolStripMenuItem.Name = "DockLeftToolStripMenuItem"
        '
        'DockRightToolStripMenuItem
        '
        resources.ApplyResources(Me.DockRightToolStripMenuItem, "DockRightToolStripMenuItem")
        Me.DockRightToolStripMenuItem.Name = "DockRightToolStripMenuItem"
        '
        'DockTopToolStripMenuItem
        '
        resources.ApplyResources(Me.DockTopToolStripMenuItem, "DockTopToolStripMenuItem")
        Me.DockTopToolStripMenuItem.Name = "DockTopToolStripMenuItem"
        '
        'DockBottomToolStripMenuItem
        '
        resources.ApplyResources(Me.DockBottomToolStripMenuItem, "DockBottomToolStripMenuItem")
        Me.DockBottomToolStripMenuItem.Name = "DockBottomToolStripMenuItem"
        '
        'DockLeftAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockLeftAutoHideToolStripMenuItem, "DockLeftAutoHideToolStripMenuItem")
        Me.DockLeftAutoHideToolStripMenuItem.Name = "DockLeftAutoHideToolStripMenuItem"
        '
        'DockRightAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockRightAutoHideToolStripMenuItem, "DockRightAutoHideToolStripMenuItem")
        Me.DockRightAutoHideToolStripMenuItem.Name = "DockRightAutoHideToolStripMenuItem"
        '
        'DockTopAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockTopAutoHideToolStripMenuItem, "DockTopAutoHideToolStripMenuItem")
        Me.DockTopAutoHideToolStripMenuItem.Name = "DockTopAutoHideToolStripMenuItem"
        '
        'DockBottomAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockBottomAutoHideToolStripMenuItem, "DockBottomAutoHideToolStripMenuItem")
        Me.DockBottomAutoHideToolStripMenuItem.Name = "DockBottomAutoHideToolStripMenuItem"
        '
        'DocumentToolStripMenuItem
        '
        resources.ApplyResources(Me.DocumentToolStripMenuItem, "DocumentToolStripMenuItem")
        Me.DocumentToolStripMenuItem.Name = "DocumentToolStripMenuItem"
        '
        'FormOptimization
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.GroupBox2)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.HideOnClose = True
        Me.Name = "FormOptimization"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.GroupBox2.ResumeLayout(False)
        Me.GBvars.ResumeLayout(False)
        Me.GBvars.PerformLayout()
        CType(Me.dgVariables, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.GBconfig.ResumeLayout(False)
        Me.GBconfig.PerformLayout()
        Me.FlowLayoutPanel2.ResumeLayout(False)
        Me.FlowLayoutPanel2.PerformLayout()
        Me.FlowLayoutPanel1.ResumeLayout(False)
        Me.FlowLayoutPanel1.PerformLayout()
        Me.GBexpression.ResumeLayout(False)
        Me.GBexpression.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        Me.GroupBox7.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents btnDeleteCase As System.Windows.Forms.Button
    Public WithEvents btnSaveCase As System.Windows.Forms.Button
    Public WithEvents btnCopyCase As System.Windows.Forms.Button
    Public WithEvents btnNewCase As System.Windows.Forms.Button
    Public WithEvents lbCases As System.Windows.Forms.ListBox
    Public WithEvents tbCaseDesc As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents tbCaseName As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents GBvars As System.Windows.Forms.GroupBox
    Public WithEvents GBconfig As System.Windows.Forms.GroupBox
    Public WithEvents rbExpression As System.Windows.Forms.RadioButton
    Public WithEvents rbVariable As System.Windows.Forms.RadioButton
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents rbMaximize As System.Windows.Forms.RadioButton
    Public WithEvents rbMinimize As System.Windows.Forms.RadioButton
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents tbToleranceValue As System.Windows.Forms.TextBox
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents tbMaxIterations As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents GBexpression As System.Windows.Forms.GroupBox
    Public WithEvents tbCurrentValue As System.Windows.Forms.TextBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents tbExpression As System.Windows.Forms.TextBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents btnVerify As System.Windows.Forms.Button
    Public WithEvents btnClear As System.Windows.Forms.Button
    Public WithEvents Panel1 As System.Windows.Forms.Panel
    Public WithEvents btnRun As System.Windows.Forms.Button
    Public WithEvents btnRestore As System.Windows.Forms.Button
    Public WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Public WithEvents grProgress As ZedGraph.ZedGraphControl
    Public WithEvents FlowLayoutPanel2 As System.Windows.Forms.FlowLayoutPanel
    Public WithEvents FlowLayoutPanel1 As System.Windows.Forms.FlowLayoutPanel
    Public WithEvents dgVariables As System.Windows.Forms.DataGridView
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents tsbAddVar As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbDelVar As System.Windows.Forms.ToolStripButton
    Public WithEvents btnAbort As System.Windows.Forms.Button
    Public WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents ComboBox1 As System.Windows.Forms.ComboBox
    Public WithEvents tbBarrierMultiplier As System.Windows.Forms.TextBox
    Public WithEvents rb4PointDeriv As System.Windows.Forms.RadioButton
    Public WithEvents rb2PointDeriv As System.Windows.Forms.RadioButton
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents tbH As System.Windows.Forms.TextBox
    Friend WithEvents dckMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents FloatToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockLeftToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockRightToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockTopToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockBottomToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockLeftAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockRightAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockTopAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockBottomAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DocumentToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents ToolStripButton1 As ToolStripButton
    Friend WithEvents Column8 As DataGridViewTextBoxColumn
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents Column2 As DataGridViewComboBoxColumn
    Friend WithEvents Column3 As DataGridViewComboBoxColumn
    Friend WithEvents Column4 As DataGridViewComboBoxColumn
    Friend WithEvents Column6 As DataGridViewTextBoxColumn
    Friend WithEvents Column7 As DataGridViewTextBoxColumn
    Friend WithEvents Column10 As DataGridViewTextBoxColumn
    Friend WithEvents Column5 As DataGridViewTextBoxColumn
    Friend WithEvents Column9 As DataGridViewTextBoxColumn
End Class
