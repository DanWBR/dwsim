<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_CompressorExpander_Curves
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_CompressorExpander_Curves))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.tb3 = New System.Windows.Forms.TextBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.cb8 = New System.Windows.Forms.ComboBox()
        Me.cb3 = New System.Windows.Forms.ComboBox()
        Me.ch3 = New System.Windows.Forms.CheckBox()
        Me.dgv3 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tb1 = New System.Windows.Forms.TextBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.cb6 = New System.Windows.Forms.ComboBox()
        Me.dgv1 = New System.Windows.Forms.DataGridView()
        Me.vap = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.temp = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ch1 = New System.Windows.Forms.CheckBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cb1 = New System.Windows.Forms.ComboBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.chart1 = New ZedGraph.ZedGraphControl()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.TSCBTypes = New System.Windows.Forms.ToolStripComboBox()
        Me.TSBtnSaveToDB = New System.Windows.Forms.ToolStripButton()
        Me.TSBtnDeletePump = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel2 = New System.Windows.Forms.ToolStripLabel()
        Me.TsTBDatabase = New System.Windows.Forms.ToolStripTextBox()
        Me.TSBtnConnectDB = New System.Windows.Forms.ToolStripButton()
        Me.TSBtnDisconnectDB = New System.Windows.Forms.ToolStripButton()
        Me.TSBtnNewDB = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.TSTBStatus = New System.Windows.Forms.ToolStripTextBox()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem1.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStripItem2.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem1, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem1
        Me.ToolTip1.SetToolTip(Me.FaTabStrip1, resources.GetString("FaTabStrip1.ToolTip"))
        '
        'FaTabStripItem1
        '
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox3)
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox1)
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Selected = True
        Me.ToolTip1.SetToolTip(Me.FaTabStripItem1, resources.GetString("FaTabStripItem1.ToolTip"))
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.tb3)
        Me.GroupBox3.Controls.Add(Me.Label13)
        Me.GroupBox3.Controls.Add(Me.cb8)
        Me.GroupBox3.Controls.Add(Me.cb3)
        Me.GroupBox3.Controls.Add(Me.ch3)
        Me.GroupBox3.Controls.Add(Me.dgv3)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Controls.Add(Me.Label4)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip"))
        '
        'tb3
        '
        resources.ApplyResources(Me.tb3, "tb3")
        Me.tb3.Name = "tb3"
        Me.ToolTip1.SetToolTip(Me.tb3, resources.GetString("tb3.ToolTip"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTip1.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        '
        'cb8
        '
        resources.ApplyResources(Me.cb8, "cb8")
        Me.cb8.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb8.FormattingEnabled = True
        Me.cb8.Items.AddRange(New Object() {resources.GetString("cb8.Items"), resources.GetString("cb8.Items1")})
        Me.cb8.Name = "cb8"
        Me.ToolTip1.SetToolTip(Me.cb8, resources.GetString("cb8.ToolTip"))
        '
        'cb3
        '
        resources.ApplyResources(Me.cb3, "cb3")
        Me.cb3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb3.FormattingEnabled = True
        Me.cb3.Items.AddRange(New Object() {resources.GetString("cb3.Items"), resources.GetString("cb3.Items1"), resources.GetString("cb3.Items2"), resources.GetString("cb3.Items3"), resources.GetString("cb3.Items4"), resources.GetString("cb3.Items5"), resources.GetString("cb3.Items6"), resources.GetString("cb3.Items7"), resources.GetString("cb3.Items8"), resources.GetString("cb3.Items9"), resources.GetString("cb3.Items10"), resources.GetString("cb3.Items11"), resources.GetString("cb3.Items12"), resources.GetString("cb3.Items13"), resources.GetString("cb3.Items14"), resources.GetString("cb3.Items15"), resources.GetString("cb3.Items16"), resources.GetString("cb3.Items17"), resources.GetString("cb3.Items18"), resources.GetString("cb3.Items19")})
        Me.cb3.Name = "cb3"
        Me.ToolTip1.SetToolTip(Me.cb3, resources.GetString("cb3.ToolTip"))
        '
        'ch3
        '
        resources.ApplyResources(Me.ch3, "ch3")
        Me.ch3.Name = "ch3"
        Me.ToolTip1.SetToolTip(Me.ch3, resources.GetString("ch3.ToolTip"))
        Me.ch3.UseVisualStyleBackColor = True
        '
        'dgv3
        '
        resources.ApplyResources(Me.dgv3, "dgv3")
        Me.dgv3.AllowDrop = True
        Me.dgv3.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv3.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.dgv3.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv3.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv3.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv3.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn5, Me.DataGridViewTextBoxColumn6})
        Me.dgv3.Name = "dgv3"
        Me.dgv3.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv3.RowHeadersVisible = False
        Me.dgv3.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv3.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.dgv3, resources.GetString("dgv3.ToolTip"))
        '
        'DataGridViewTextBoxColumn5
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        '
        'DataGridViewTextBoxColumn6
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTip1.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.tb1)
        Me.GroupBox1.Controls.Add(Me.Label11)
        Me.GroupBox1.Controls.Add(Me.cb6)
        Me.GroupBox1.Controls.Add(Me.dgv1)
        Me.GroupBox1.Controls.Add(Me.ch1)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.cb1)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip"))
        '
        'tb1
        '
        resources.ApplyResources(Me.tb1, "tb1")
        Me.tb1.Name = "tb1"
        Me.ToolTip1.SetToolTip(Me.tb1, resources.GetString("tb1.ToolTip"))
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTip1.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        '
        'cb6
        '
        resources.ApplyResources(Me.cb6, "cb6")
        Me.cb6.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb6.FormattingEnabled = True
        Me.cb6.Items.AddRange(New Object() {resources.GetString("cb6.Items"), resources.GetString("cb6.Items1"), resources.GetString("cb6.Items2"), resources.GetString("cb6.Items3"), resources.GetString("cb6.Items4")})
        Me.cb6.Name = "cb6"
        Me.ToolTip1.SetToolTip(Me.cb6, resources.GetString("cb6.ToolTip"))
        '
        'dgv1
        '
        resources.ApplyResources(Me.dgv1, "dgv1")
        Me.dgv1.AllowDrop = True
        Me.dgv1.AllowUserToResizeRows = False
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle2
        Me.dgv1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv1.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.vap, Me.temp})
        Me.dgv1.Name = "dgv1"
        Me.dgv1.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv1.RowHeadersVisible = False
        Me.dgv1.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.dgv1, resources.GetString("dgv1.ToolTip"))
        '
        'vap
        '
        resources.ApplyResources(Me.vap, "vap")
        Me.vap.Name = "vap"
        '
        'temp
        '
        resources.ApplyResources(Me.temp, "temp")
        Me.temp.Name = "temp"
        '
        'ch1
        '
        resources.ApplyResources(Me.ch1, "ch1")
        Me.ch1.Name = "ch1"
        Me.ToolTip1.SetToolTip(Me.ch1, resources.GetString("ch1.ToolTip"))
        Me.ch1.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        '
        'cb1
        '
        resources.ApplyResources(Me.cb1, "cb1")
        Me.cb1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb1.FormattingEnabled = True
        Me.cb1.Items.AddRange(New Object() {resources.GetString("cb1.Items"), resources.GetString("cb1.Items1"), resources.GetString("cb1.Items2"), resources.GetString("cb1.Items3"), resources.GetString("cb1.Items4"), resources.GetString("cb1.Items5"), resources.GetString("cb1.Items6"), resources.GetString("cb1.Items7"), resources.GetString("cb1.Items8"), resources.GetString("cb1.Items9"), resources.GetString("cb1.Items10"), resources.GetString("cb1.Items11"), resources.GetString("cb1.Items12"), resources.GetString("cb1.Items13"), resources.GetString("cb1.Items14"), resources.GetString("cb1.Items15"), resources.GetString("cb1.Items16"), resources.GetString("cb1.Items17"), resources.GetString("cb1.Items18"), resources.GetString("cb1.Items19")})
        Me.cb1.Name = "cb1"
        Me.ToolTip1.SetToolTip(Me.cb1, resources.GetString("cb1.ToolTip"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        '
        'FaTabStripItem2
        '
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.GroupBox7)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        Me.ToolTip1.SetToolTip(Me.FaTabStripItem2, resources.GetString("FaTabStripItem2.ToolTip"))
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.chart1)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox7, resources.GetString("GroupBox7.ToolTip"))
        '
        'chart1
        '
        resources.ApplyResources(Me.chart1, "chart1")
        Me.chart1.IsAntiAlias = True
        Me.chart1.IsAutoScrollRange = True
        Me.chart1.IsShowCopyMessage = False
        Me.chart1.Name = "chart1"
        Me.chart1.ScrollGrace = 0R
        Me.chart1.ScrollMaxX = 0R
        Me.chart1.ScrollMaxY = 0R
        Me.chart1.ScrollMaxY2 = 0R
        Me.chart1.ScrollMinX = 0R
        Me.chart1.ScrollMinY = 0R
        Me.chart1.ScrollMinY2 = 0R
        Me.ToolTip1.SetToolTip(Me.chart1, resources.GetString("chart1.ToolTip"))
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripLabel1, Me.TSCBTypes, Me.TSBtnSaveToDB, Me.TSBtnDeletePump, Me.ToolStripSeparator1, Me.ToolStripLabel2, Me.TsTBDatabase, Me.TSBtnConnectDB, Me.TSBtnDisconnectDB, Me.TSBtnNewDB, Me.ToolStripSeparator2, Me.TSTBStatus})
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        Me.ToolTip1.SetToolTip(Me.ToolStrip1, resources.GetString("ToolStrip1.ToolTip"))
        '
        'ToolStripLabel1
        '
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        '
        'TSCBTypes
        '
        resources.ApplyResources(Me.TSCBTypes, "TSCBTypes")
        Me.TSCBTypes.Name = "TSCBTypes"
        '
        'TSBtnSaveToDB
        '
        resources.ApplyResources(Me.TSBtnSaveToDB, "TSBtnSaveToDB")
        Me.TSBtnSaveToDB.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtnSaveToDB.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disk
        Me.TSBtnSaveToDB.Name = "TSBtnSaveToDB"
        '
        'TSBtnDeletePump
        '
        resources.ApplyResources(Me.TSBtnDeletePump, "TSBtnDeletePump")
        Me.TSBtnDeletePump.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtnDeletePump.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        Me.TSBtnDeletePump.Name = "TSBtnDeletePump"
        '
        'ToolStripSeparator1
        '
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        '
        'ToolStripLabel2
        '
        resources.ApplyResources(Me.ToolStripLabel2, "ToolStripLabel2")
        Me.ToolStripLabel2.Name = "ToolStripLabel2"
        '
        'TsTBDatabase
        '
        resources.ApplyResources(Me.TsTBDatabase, "TsTBDatabase")
        Me.TsTBDatabase.Name = "TsTBDatabase"
        Me.TsTBDatabase.ReadOnly = True
        '
        'TSBtnConnectDB
        '
        resources.ApplyResources(Me.TSBtnConnectDB, "TSBtnConnectDB")
        Me.TSBtnConnectDB.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtnConnectDB.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.connect
        Me.TSBtnConnectDB.Name = "TSBtnConnectDB"
        '
        'TSBtnDisconnectDB
        '
        resources.ApplyResources(Me.TSBtnDisconnectDB, "TSBtnDisconnectDB")
        Me.TSBtnDisconnectDB.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtnDisconnectDB.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.TSBtnDisconnectDB.Name = "TSBtnDisconnectDB"
        '
        'TSBtnNewDB
        '
        resources.ApplyResources(Me.TSBtnNewDB, "TSBtnNewDB")
        Me.TSBtnNewDB.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtnNewDB.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        Me.TSBtnNewDB.Name = "TSBtnNewDB"
        '
        'ToolStripSeparator2
        '
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        '
        'TSTBStatus
        '
        resources.ApplyResources(Me.TSTBStatus, "TSTBStatus")
        Me.TSTBStatus.BackColor = System.Drawing.Color.Lime
        Me.TSTBStatus.Name = "TSTBStatus"
        Me.TSTBStatus.ReadOnly = True
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.CheckFileExists = False
        Me.OpenFileDialog1.DefaultExt = "*.cxml"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'EditingForm_CompressorExpander_Curves
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.FaTabStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_CompressorExpander_Curves"
        Me.ShowInTaskbar = False
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem1.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStripItem2.ResumeLayout(False)
        Me.GroupBox7.ResumeLayout(False)
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents ch1 As System.Windows.Forms.CheckBox
    Public WithEvents cb1 As System.Windows.Forms.ComboBox
    Public WithEvents dgv1 As System.Windows.Forms.DataGridView
    Public WithEvents ch3 As System.Windows.Forms.CheckBox
    Public WithEvents dgv3 As System.Windows.Forms.DataGridView
    Public WithEvents cb6 As System.Windows.Forms.ComboBox
    Public WithEvents cb8 As System.Windows.Forms.ComboBox
    Public WithEvents cb3 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Public WithEvents chart1 As ZedGraph.ZedGraphControl
    Public WithEvents tb3 As System.Windows.Forms.TextBox
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents tb1 As System.Windows.Forms.TextBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents vap As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents temp As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripLabel1 As System.Windows.Forms.ToolStripLabel
    Public WithEvents TSCBTypes As System.Windows.Forms.ToolStripComboBox
    Public WithEvents TSBtnSaveToDB As System.Windows.Forms.ToolStripButton
    Public WithEvents TSBtnDeletePump As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripLabel2 As System.Windows.Forms.ToolStripLabel
    Public WithEvents TsTBDatabase As System.Windows.Forms.ToolStripTextBox
    Public WithEvents TSBtnConnectDB As System.Windows.Forms.ToolStripButton
    Public WithEvents TSBtnDisconnectDB As System.Windows.Forms.ToolStripButton
    Public WithEvents TSBtnNewDB As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents TSTBStatus As System.Windows.Forms.ToolStripTextBox
    Public WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents DataGridViewTextBoxColumn5 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn6 As DataGridViewTextBoxColumn
End Class
