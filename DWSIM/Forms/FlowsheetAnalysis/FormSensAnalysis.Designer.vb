<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormSensAnalysis
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormSensAnalysis))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.btnRun = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbObjIndVar1 = New System.Windows.Forms.ComboBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.tbStats = New System.Windows.Forms.TextBox()
        Me.dgvResults = New System.Windows.Forms.DataGridView()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.btnDeleteCase = New System.Windows.Forms.Button()
        Me.btnSaveCase = New System.Windows.Forms.Button()
        Me.btnCopyCase = New System.Windows.Forms.Button()
        Me.btnNewCase = New System.Windows.Forms.Button()
        Me.lbCases = New System.Windows.Forms.ListBox()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.tbCaseDesc = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbCaseName = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.gbIndVar1 = New System.Windows.Forms.GroupBox()
        Me.tbCurrValIndVar1 = New System.Windows.Forms.TextBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.tbUpperLimIndVar1 = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.tbUnitIndVar1 = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbLowerLimIndVar1 = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.nuNumPointsIndVar1 = New System.Windows.Forms.NumericUpDown()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbPropIndVar1 = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.gbIndVar2 = New System.Windows.Forms.GroupBox()
        Me.tbCurrValIndVar2 = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbObjIndVar2 = New System.Windows.Forms.ComboBox()
        Me.tbUpperLimIndVar2 = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.tbLowerLimIndVar2 = New System.Windows.Forms.TextBox()
        Me.cbPropIndVar2 = New System.Windows.Forms.ComboBox()
        Me.nuNumPointsIndVar2 = New System.Windows.Forms.NumericUpDown()
        Me.tbUnitIndVar2 = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.chkIndVar2 = New System.Windows.Forms.CheckBox()
        Me.rbExp = New System.Windows.Forms.RadioButton()
        Me.rbVar = New System.Windows.Forms.RadioButton()
        Me.GroupBox9 = New System.Windows.Forms.GroupBox()
        Me.dgVariables = New System.Windows.Forms.DataGridView()
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewComboBoxColumn2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column10 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.tsbAddVar = New System.Windows.Forms.ToolStripButton()
        Me.tsbDelVar = New System.Windows.Forms.ToolStripButton()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.btnClear = New System.Windows.Forms.Button()
        Me.btnVerify = New System.Windows.Forms.Button()
        Me.tbCurrentValue = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.tbExpression = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.GroupBox10 = New System.Windows.Forms.GroupBox()
        Me.dgDepVariables = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewComboBoxColumn1 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewComboBoxColumn3 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip2 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.btnRegressData = New System.Windows.Forms.Button()
        Me.btnAbort = New System.Windows.Forms.Button()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.btnExportToNewSheet = New System.Windows.Forms.Button()
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.CbCrtPar = New System.Windows.Forms.ComboBox()
        Me.LblParam = New System.Windows.Forms.Label()
        Me.BtnDrawChart = New System.Windows.Forms.Button()
        Me.CbCrtY = New System.Windows.Forms.ComboBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.CbCrtX = New System.Windows.Forms.ComboBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.graph = New ZedGraph.ZedGraphControl()
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
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        CType(Me.dgvResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.gbIndVar1.SuspendLayout()
        CType(Me.nuNumPointsIndVar1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.gbIndVar2.SuspendLayout()
        CType(Me.nuNumPointsIndVar2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox9.SuspendLayout()
        CType(Me.dgVariables, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        Me.GroupBox10.SuspendLayout()
        CType(Me.dgDepVariables, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip2.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'btnRun
        '
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.Name = "btnRun"
        Me.btnRun.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbObjIndVar1
        '
        resources.ApplyResources(Me.cbObjIndVar1, "cbObjIndVar1")
        Me.cbObjIndVar1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbObjIndVar1.DropDownWidth = 250
        Me.cbObjIndVar1.FormattingEnabled = True
        Me.cbObjIndVar1.Name = "cbObjIndVar1"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.GroupBox7)
        Me.GroupBox1.Controls.Add(Me.dgvResults)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.tbStats)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        '
        'tbStats
        '
        resources.ApplyResources(Me.tbStats, "tbStats")
        Me.tbStats.Name = "tbStats"
        Me.tbStats.ReadOnly = True
        '
        'dgvResults
        '
        resources.ApplyResources(Me.dgvResults, "dgvResults")
        Me.dgvResults.AllowUserToAddRows = False
        Me.dgvResults.AllowUserToDeleteRows = False
        Me.dgvResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvResults.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.dgvResults.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.dgvResults.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvResults.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2})
        Me.dgvResults.Name = "dgvResults"
        Me.dgvResults.ReadOnly = True
        Me.dgvResults.RowHeadersVisible = False
        Me.dgvResults.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgvResults.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'Column1
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'Column2
        '
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        Me.Column2.DefaultCellStyle = DataGridViewCellStyle3
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
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
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.tbCaseDesc)
        Me.GroupBox3.Controls.Add(Me.Label4)
        Me.GroupBox3.Controls.Add(Me.tbCaseName)
        Me.GroupBox3.Controls.Add(Me.Label1)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
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
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'gbIndVar1
        '
        resources.ApplyResources(Me.gbIndVar1, "gbIndVar1")
        Me.gbIndVar1.Controls.Add(Me.tbCurrValIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label19)
        Me.gbIndVar1.Controls.Add(Me.tbUpperLimIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label8)
        Me.gbIndVar1.Controls.Add(Me.tbUnitIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label2)
        Me.gbIndVar1.Controls.Add(Me.tbLowerLimIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label7)
        Me.gbIndVar1.Controls.Add(Me.nuNumPointsIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label6)
        Me.gbIndVar1.Controls.Add(Me.cbPropIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label5)
        Me.gbIndVar1.Controls.Add(Me.cbObjIndVar1)
        Me.gbIndVar1.Controls.Add(Me.Label3)
        Me.gbIndVar1.Name = "gbIndVar1"
        Me.gbIndVar1.TabStop = False
        '
        'tbCurrValIndVar1
        '
        resources.ApplyResources(Me.tbCurrValIndVar1, "tbCurrValIndVar1")
        Me.tbCurrValIndVar1.Name = "tbCurrValIndVar1"
        Me.tbCurrValIndVar1.ReadOnly = True
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'tbUpperLimIndVar1
        '
        resources.ApplyResources(Me.tbUpperLimIndVar1, "tbUpperLimIndVar1")
        Me.tbUpperLimIndVar1.Name = "tbUpperLimIndVar1"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'tbUnitIndVar1
        '
        resources.ApplyResources(Me.tbUnitIndVar1, "tbUnitIndVar1")
        Me.tbUnitIndVar1.Name = "tbUnitIndVar1"
        Me.tbUnitIndVar1.ReadOnly = True
        Me.tbUnitIndVar1.TabStop = False
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'tbLowerLimIndVar1
        '
        resources.ApplyResources(Me.tbLowerLimIndVar1, "tbLowerLimIndVar1")
        Me.tbLowerLimIndVar1.Name = "tbLowerLimIndVar1"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'nuNumPointsIndVar1
        '
        resources.ApplyResources(Me.nuNumPointsIndVar1, "nuNumPointsIndVar1")
        Me.nuNumPointsIndVar1.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nuNumPointsIndVar1.Name = "nuNumPointsIndVar1"
        Me.nuNumPointsIndVar1.Value = New Decimal(New Integer() {10, 0, 0, 0})
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'cbPropIndVar1
        '
        resources.ApplyResources(Me.cbPropIndVar1, "cbPropIndVar1")
        Me.cbPropIndVar1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropIndVar1.DropDownWidth = 400
        Me.cbPropIndVar1.FormattingEnabled = True
        Me.cbPropIndVar1.Name = "cbPropIndVar1"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'gbIndVar2
        '
        resources.ApplyResources(Me.gbIndVar2, "gbIndVar2")
        Me.gbIndVar2.Controls.Add(Me.tbCurrValIndVar2)
        Me.gbIndVar2.Controls.Add(Me.Label20)
        Me.gbIndVar2.Controls.Add(Me.Label14)
        Me.gbIndVar2.Controls.Add(Me.cbObjIndVar2)
        Me.gbIndVar2.Controls.Add(Me.tbUpperLimIndVar2)
        Me.gbIndVar2.Controls.Add(Me.Label13)
        Me.gbIndVar2.Controls.Add(Me.Label11)
        Me.gbIndVar2.Controls.Add(Me.Label9)
        Me.gbIndVar2.Controls.Add(Me.tbLowerLimIndVar2)
        Me.gbIndVar2.Controls.Add(Me.cbPropIndVar2)
        Me.gbIndVar2.Controls.Add(Me.nuNumPointsIndVar2)
        Me.gbIndVar2.Controls.Add(Me.tbUnitIndVar2)
        Me.gbIndVar2.Controls.Add(Me.Label10)
        Me.gbIndVar2.Controls.Add(Me.Label12)
        Me.gbIndVar2.Name = "gbIndVar2"
        Me.gbIndVar2.TabStop = False
        '
        'tbCurrValIndVar2
        '
        resources.ApplyResources(Me.tbCurrValIndVar2, "tbCurrValIndVar2")
        Me.tbCurrValIndVar2.Name = "tbCurrValIndVar2"
        Me.tbCurrValIndVar2.ReadOnly = True
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbObjIndVar2
        '
        resources.ApplyResources(Me.cbObjIndVar2, "cbObjIndVar2")
        Me.cbObjIndVar2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbObjIndVar2.DropDownWidth = 250
        Me.cbObjIndVar2.FormattingEnabled = True
        Me.cbObjIndVar2.Name = "cbObjIndVar2"
        '
        'tbUpperLimIndVar2
        '
        resources.ApplyResources(Me.tbUpperLimIndVar2, "tbUpperLimIndVar2")
        Me.tbUpperLimIndVar2.Name = "tbUpperLimIndVar2"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'tbLowerLimIndVar2
        '
        resources.ApplyResources(Me.tbLowerLimIndVar2, "tbLowerLimIndVar2")
        Me.tbLowerLimIndVar2.Name = "tbLowerLimIndVar2"
        '
        'cbPropIndVar2
        '
        resources.ApplyResources(Me.cbPropIndVar2, "cbPropIndVar2")
        Me.cbPropIndVar2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropIndVar2.DropDownWidth = 400
        Me.cbPropIndVar2.FormattingEnabled = True
        Me.cbPropIndVar2.Name = "cbPropIndVar2"
        '
        'nuNumPointsIndVar2
        '
        resources.ApplyResources(Me.nuNumPointsIndVar2, "nuNumPointsIndVar2")
        Me.nuNumPointsIndVar2.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nuNumPointsIndVar2.Name = "nuNumPointsIndVar2"
        Me.nuNumPointsIndVar2.Value = New Decimal(New Integer() {10, 0, 0, 0})
        '
        'tbUnitIndVar2
        '
        resources.ApplyResources(Me.tbUnitIndVar2, "tbUnitIndVar2")
        Me.tbUnitIndVar2.Name = "tbUnitIndVar2"
        Me.tbUnitIndVar2.ReadOnly = True
        Me.tbUnitIndVar2.TabStop = False
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'chkIndVar2
        '
        resources.ApplyResources(Me.chkIndVar2, "chkIndVar2")
        Me.chkIndVar2.Name = "chkIndVar2"
        Me.chkIndVar2.UseVisualStyleBackColor = True
        '
        'rbExp
        '
        resources.ApplyResources(Me.rbExp, "rbExp")
        Me.rbExp.Name = "rbExp"
        Me.rbExp.UseVisualStyleBackColor = True
        '
        'rbVar
        '
        resources.ApplyResources(Me.rbVar, "rbVar")
        Me.rbVar.Checked = True
        Me.rbVar.Name = "rbVar"
        Me.rbVar.TabStop = True
        Me.rbVar.UseVisualStyleBackColor = True
        '
        'GroupBox9
        '
        resources.ApplyResources(Me.GroupBox9, "GroupBox9")
        Me.GroupBox9.Controls.Add(Me.dgVariables)
        Me.GroupBox9.Controls.Add(Me.ToolStrip1)
        Me.GroupBox9.Name = "GroupBox9"
        Me.GroupBox9.TabStop = False
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
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle4.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle4.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle4.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle4.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle4.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        Me.dgVariables.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle4
        Me.dgVariables.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgVariables.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column8, Me.DataGridViewTextBoxColumn1, Me.DataGridViewComboBoxColumn2, Me.Column4, Me.Column10, Me.Column9})
        Me.dgVariables.Name = "dgVariables"
        Me.dgVariables.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        '
        'Column8
        '
        resources.ApplyResources(Me.Column8, "Column8")
        Me.Column8.Name = "Column8"
        Me.Column8.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'DataGridViewTextBoxColumn1
        '
        Me.DataGridViewTextBoxColumn1.FillWeight = 20.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'DataGridViewComboBoxColumn2
        '
        Me.DataGridViewComboBoxColumn2.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        Me.DataGridViewComboBoxColumn2.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn2, "DataGridViewComboBoxColumn2")
        Me.DataGridViewComboBoxColumn2.Name = "DataGridViewComboBoxColumn2"
        Me.DataGridViewComboBoxColumn2.Sorted = True
        '
        'Column4
        '
        Me.Column4.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        Me.Column4.DropDownWidth = 400
        Me.Column4.FillWeight = 40.0!
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        '
        'Column10
        '
        Me.Column10.FillWeight = 20.0!
        resources.ApplyResources(Me.Column10, "Column10")
        Me.Column10.Name = "Column10"
        Me.Column10.ReadOnly = True
        Me.Column10.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column9
        '
        Me.Column9.FillWeight = 15.0!
        resources.ApplyResources(Me.Column9, "Column9")
        Me.Column9.Name = "Column9"
        Me.Column9.ReadOnly = True
        Me.Column9.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbAddVar, Me.tsbDelVar})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'tsbAddVar
        '
        resources.ApplyResources(Me.tsbAddVar, "tsbAddVar")
        Me.tsbAddVar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAddVar.Image = Global.DWSIM.My.Resources.Resources.add
        Me.tsbAddVar.Name = "tsbAddVar"
        '
        'tsbDelVar
        '
        resources.ApplyResources(Me.tsbDelVar, "tsbDelVar")
        Me.tsbDelVar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDelVar.Image = Global.DWSIM.My.Resources.Resources.delete1
        Me.tsbDelVar.Name = "tsbDelVar"
        '
        'GroupBox8
        '
        resources.ApplyResources(Me.GroupBox8, "GroupBox8")
        Me.GroupBox8.Controls.Add(Me.btnClear)
        Me.GroupBox8.Controls.Add(Me.btnVerify)
        Me.GroupBox8.Controls.Add(Me.tbCurrentValue)
        Me.GroupBox8.Controls.Add(Me.Label15)
        Me.GroupBox8.Controls.Add(Me.tbExpression)
        Me.GroupBox8.Controls.Add(Me.Label17)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.TabStop = False
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
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'tbExpression
        '
        resources.ApplyResources(Me.tbExpression, "tbExpression")
        Me.tbExpression.Name = "tbExpression"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        '
        'GroupBox10
        '
        resources.ApplyResources(Me.GroupBox10, "GroupBox10")
        Me.GroupBox10.Controls.Add(Me.dgDepVariables)
        Me.GroupBox10.Controls.Add(Me.ToolStrip2)
        Me.GroupBox10.Name = "GroupBox10"
        Me.GroupBox10.TabStop = False
        '
        'dgDepVariables
        '
        resources.ApplyResources(Me.dgDepVariables, "dgDepVariables")
        Me.dgDepVariables.AllowUserToAddRows = False
        Me.dgDepVariables.AllowUserToDeleteRows = False
        Me.dgDepVariables.AllowUserToResizeRows = False
        Me.dgDepVariables.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgDepVariables.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.DisplayedCells
        Me.dgDepVariables.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        Me.dgDepVariables.ColumnHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        DataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle5.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle5.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle5.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle5.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle5.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        Me.dgDepVariables.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle5
        Me.dgDepVariables.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgDepVariables.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn2, Me.DataGridViewComboBoxColumn1, Me.DataGridViewComboBoxColumn3, Me.DataGridViewTextBoxColumn5})
        Me.dgDepVariables.Name = "dgDepVariables"
        Me.dgDepVariables.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        '
        'DataGridViewTextBoxColumn2
        '
        Me.DataGridViewTextBoxColumn2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewTextBoxColumn2.FillWeight = 5.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'DataGridViewComboBoxColumn1
        '
        Me.DataGridViewComboBoxColumn1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewComboBoxColumn1.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        Me.DataGridViewComboBoxColumn1.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn1, "DataGridViewComboBoxColumn1")
        Me.DataGridViewComboBoxColumn1.Name = "DataGridViewComboBoxColumn1"
        Me.DataGridViewComboBoxColumn1.Sorted = True
        '
        'DataGridViewComboBoxColumn3
        '
        Me.DataGridViewComboBoxColumn3.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewComboBoxColumn3.DisplayStyle = System.Windows.Forms.DataGridViewComboBoxDisplayStyle.ComboBox
        Me.DataGridViewComboBoxColumn3.DropDownWidth = 400
        Me.DataGridViewComboBoxColumn3.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewComboBoxColumn3, "DataGridViewComboBoxColumn3")
        Me.DataGridViewComboBoxColumn3.Name = "DataGridViewComboBoxColumn3"
        '
        'DataGridViewTextBoxColumn5
        '
        Me.DataGridViewTextBoxColumn5.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        Me.DataGridViewTextBoxColumn5.FillWeight = 15.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        Me.DataGridViewTextBoxColumn5.ReadOnly = True
        Me.DataGridViewTextBoxColumn5.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ToolStrip2
        '
        resources.ApplyResources(Me.ToolStrip2, "ToolStrip2")
        Me.ToolStrip2.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2})
        Me.ToolStrip2.Name = "ToolStrip2"
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.add
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.delete1
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'btnRegressData
        '
        resources.ApplyResources(Me.btnRegressData, "btnRegressData")
        Me.btnRegressData.Name = "btnRegressData"
        Me.btnRegressData.UseVisualStyleBackColor = True
        '
        'btnAbort
        '
        resources.ApplyResources(Me.btnAbort, "btnAbort")
        Me.btnAbort.Name = "btnAbort"
        Me.btnAbort.UseVisualStyleBackColor = True
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.BackColor = System.Drawing.SystemColors.Control
        Me.TabPage2.Controls.Add(Me.chkIndVar2)
        Me.TabPage2.Controls.Add(Me.gbIndVar1)
        Me.TabPage2.Controls.Add(Me.gbIndVar2)
        Me.TabPage2.Name = "TabPage2"
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.BackColor = System.Drawing.SystemColors.Control
        Me.TabPage3.Controls.Add(Me.rbExp)
        Me.TabPage3.Controls.Add(Me.rbVar)
        Me.TabPage3.Controls.Add(Me.GroupBox9)
        Me.TabPage3.Controls.Add(Me.GroupBox8)
        Me.TabPage3.Controls.Add(Me.GroupBox10)
        Me.TabPage3.Name = "TabPage3"
        '
        'TabPage4
        '
        resources.ApplyResources(Me.TabPage4, "TabPage4")
        Me.TabPage4.BackColor = System.Drawing.SystemColors.Control
        Me.TabPage4.Controls.Add(Me.btnExportToNewSheet)
        Me.TabPage4.Controls.Add(Me.btnRegressData)
        Me.TabPage4.Controls.Add(Me.GroupBox1)
        Me.TabPage4.Controls.Add(Me.btnRun)
        Me.TabPage4.Controls.Add(Me.btnAbort)
        Me.TabPage4.Name = "TabPage4"
        '
        'btnExportToNewSheet
        '
        resources.ApplyResources(Me.btnExportToNewSheet, "btnExportToNewSheet")
        Me.btnExportToNewSheet.Name = "btnExportToNewSheet"
        Me.btnExportToNewSheet.UseVisualStyleBackColor = True
        '
        'TabPage5
        '
        resources.ApplyResources(Me.TabPage5, "TabPage5")
        Me.TabPage5.BackColor = System.Drawing.SystemColors.Control
        Me.TabPage5.Controls.Add(Me.Panel1)
        Me.TabPage5.Controls.Add(Me.graph)
        Me.TabPage5.Name = "TabPage5"
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.CbCrtPar)
        Me.Panel1.Controls.Add(Me.LblParam)
        Me.Panel1.Controls.Add(Me.BtnDrawChart)
        Me.Panel1.Controls.Add(Me.CbCrtY)
        Me.Panel1.Controls.Add(Me.Label18)
        Me.Panel1.Controls.Add(Me.CbCrtX)
        Me.Panel1.Controls.Add(Me.Label16)
        Me.Panel1.Name = "Panel1"
        '
        'CbCrtPar
        '
        resources.ApplyResources(Me.CbCrtPar, "CbCrtPar")
        Me.CbCrtPar.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CbCrtPar.DropDownWidth = 250
        Me.CbCrtPar.FormattingEnabled = True
        Me.CbCrtPar.Name = "CbCrtPar"
        '
        'LblParam
        '
        resources.ApplyResources(Me.LblParam, "LblParam")
        Me.LblParam.Name = "LblParam"
        '
        'BtnDrawChart
        '
        resources.ApplyResources(Me.BtnDrawChart, "BtnDrawChart")
        Me.BtnDrawChart.Image = Global.DWSIM.My.Resources.Resources.chart_curve
        Me.BtnDrawChart.Name = "BtnDrawChart"
        Me.BtnDrawChart.UseVisualStyleBackColor = True
        '
        'CbCrtY
        '
        resources.ApplyResources(Me.CbCrtY, "CbCrtY")
        Me.CbCrtY.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CbCrtY.DropDownWidth = 250
        Me.CbCrtY.FormattingEnabled = True
        Me.CbCrtY.Name = "CbCrtY"
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        '
        'CbCrtX
        '
        resources.ApplyResources(Me.CbCrtX, "CbCrtX")
        Me.CbCrtX.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CbCrtX.DropDownWidth = 250
        Me.CbCrtX.FormattingEnabled = True
        Me.CbCrtX.Name = "CbCrtX"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'graph
        '
        resources.ApplyResources(Me.graph, "graph")
        Me.graph.IsShowCopyMessage = False
        Me.graph.Name = "graph"
        Me.graph.ScrollGrace = 0R
        Me.graph.ScrollMaxX = 0R
        Me.graph.ScrollMaxY = 0R
        Me.graph.ScrollMaxY2 = 0R
        Me.graph.ScrollMinX = 0R
        Me.graph.ScrollMinY = 0R
        Me.graph.ScrollMinY2 = 0R
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
        'FormSensAnalysis
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.SystemColors.Control
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormSensAnalysis"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox7.ResumeLayout(False)
        Me.GroupBox7.PerformLayout()
        CType(Me.dgvResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.gbIndVar1.ResumeLayout(False)
        Me.gbIndVar1.PerformLayout()
        CType(Me.nuNumPointsIndVar1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.gbIndVar2.ResumeLayout(False)
        Me.gbIndVar2.PerformLayout()
        CType(Me.nuNumPointsIndVar2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox9.ResumeLayout(False)
        Me.GroupBox9.PerformLayout()
        CType(Me.dgVariables, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        Me.GroupBox10.ResumeLayout(False)
        Me.GroupBox10.PerformLayout()
        CType(Me.dgDepVariables, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip2.ResumeLayout(False)
        Me.ToolStrip2.PerformLayout()
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage3.PerformLayout()
        Me.TabPage4.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents btnRun As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbObjIndVar1 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents dgvResults As System.Windows.Forms.DataGridView
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents btnDeleteCase As System.Windows.Forms.Button
    Public WithEvents btnSaveCase As System.Windows.Forms.Button
    Public WithEvents btnCopyCase As System.Windows.Forms.Button
    Public WithEvents btnNewCase As System.Windows.Forms.Button
    Public WithEvents lbCases As System.Windows.Forms.ListBox
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents tbCaseName As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents tbCaseDesc As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents gbIndVar1 As System.Windows.Forms.GroupBox
    Public WithEvents cbPropIndVar1 As System.Windows.Forms.ComboBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents tbUpperLimIndVar1 As System.Windows.Forms.TextBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents tbUnitIndVar1 As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents tbLowerLimIndVar1 As System.Windows.Forms.TextBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents nuNumPointsIndVar1 As System.Windows.Forms.NumericUpDown
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents gbIndVar2 As System.Windows.Forms.GroupBox
    Public WithEvents tbUpperLimIndVar2 As System.Windows.Forms.TextBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents tbUnitIndVar2 As System.Windows.Forms.TextBox
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents tbLowerLimIndVar2 As System.Windows.Forms.TextBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents nuNumPointsIndVar2 As System.Windows.Forms.NumericUpDown
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents cbPropIndVar2 As System.Windows.Forms.ComboBox
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents cbObjIndVar2 As System.Windows.Forms.ComboBox
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Public WithEvents tbStats As System.Windows.Forms.TextBox
    Public WithEvents btnAbort As System.Windows.Forms.Button
    Public WithEvents chkIndVar2 As System.Windows.Forms.CheckBox
    Public WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Public WithEvents btnClear As System.Windows.Forms.Button
    Public WithEvents btnVerify As System.Windows.Forms.Button
    Public WithEvents tbCurrentValue As System.Windows.Forms.TextBox
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents tbExpression As System.Windows.Forms.TextBox
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents GroupBox9 As System.Windows.Forms.GroupBox
    Public WithEvents dgVariables As System.Windows.Forms.DataGridView
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents tsbAddVar As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbDelVar As System.Windows.Forms.ToolStripButton
    Public WithEvents rbVar As System.Windows.Forms.RadioButton
    Public WithEvents rbExp As System.Windows.Forms.RadioButton
    Public WithEvents GroupBox10 As System.Windows.Forms.GroupBox
    Public WithEvents dgDepVariables As System.Windows.Forms.DataGridView
    Public WithEvents ToolStrip2 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Friend WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents btnRegressData As System.Windows.Forms.Button
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage5 As System.Windows.Forms.TabPage
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents graph As ZedGraph.ZedGraphControl
    Public WithEvents CbCrtY As System.Windows.Forms.ComboBox
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents CbCrtX As System.Windows.Forms.ComboBox
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents BtnDrawChart As System.Windows.Forms.Button
    Public WithEvents CbCrtPar As System.Windows.Forms.ComboBox
    Friend WithEvents LblParam As System.Windows.Forms.Label
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
    Public WithEvents tbCurrValIndVar1 As TextBox
    Public WithEvents Label19 As Label
    Public WithEvents tbCurrValIndVar2 As TextBox
    Public WithEvents Label20 As Label
    Public WithEvents btnExportToNewSheet As Button
    Friend WithEvents Column8 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn1 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn2 As DataGridViewComboBoxColumn
    Friend WithEvents Column4 As DataGridViewComboBoxColumn
    Friend WithEvents Column10 As DataGridViewTextBoxColumn
    Friend WithEvents Column9 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn1 As DataGridViewComboBoxColumn
    Friend WithEvents DataGridViewComboBoxColumn3 As DataGridViewComboBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn5 As DataGridViewTextBoxColumn
End Class
