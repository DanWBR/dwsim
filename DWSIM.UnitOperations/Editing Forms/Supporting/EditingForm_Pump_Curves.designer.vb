<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Pump_Curves
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Pump_Curves))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.tb5 = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cb10 = New System.Windows.Forms.ComboBox()
        Me.cb5 = New System.Windows.Forms.ComboBox()
        Me.dgv5 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.ch5 = New System.Windows.Forms.CheckBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.tb4 = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cb9 = New System.Windows.Forms.ComboBox()
        Me.ch4 = New System.Windows.Forms.CheckBox()
        Me.cb4 = New System.Windows.Forms.ComboBox()
        Me.dgv4 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
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
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.tb2 = New System.Windows.Forms.TextBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.cb7 = New System.Windows.Forms.ComboBox()
        Me.ch2 = New System.Windows.Forms.CheckBox()
        Me.cb2 = New System.Windows.Forms.ComboBox()
        Me.dgv2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
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
        Me.Label17 = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.cbeffunit = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.cbpowerunit = New System.Windows.Forms.ComboBox()
        Me.cbheadunit = New System.Windows.Forms.ComboBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.cbflowunit = New System.Windows.Forms.ComboBox()
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
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.TBImpellerSpeed = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.CBDiameterUnit = New System.Windows.Forms.ComboBox()
        Me.TBImpellerDiam = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem1.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        CType(Me.dgv5, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox4.SuspendLayout()
        CType(Me.dgv4, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox3.SuspendLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox2.SuspendLayout()
        CType(Me.dgv2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStripItem2.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem1, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem2
        Me.ToolTip1.SetToolTip(Me.FaTabStrip1, resources.GetString("FaTabStrip1.ToolTip"))
        '
        'FaTabStripItem1
        '
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox5)
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox4)
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox3)
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox2)
        Me.FaTabStripItem1.Controls.Add(Me.GroupBox1)
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.ToolTip1.SetToolTip(Me.FaTabStripItem1, resources.GetString("FaTabStripItem1.ToolTip"))
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.tb5)
        Me.GroupBox5.Controls.Add(Me.Label15)
        Me.GroupBox5.Controls.Add(Me.cb10)
        Me.GroupBox5.Controls.Add(Me.cb5)
        Me.GroupBox5.Controls.Add(Me.dgv5)
        Me.GroupBox5.Controls.Add(Me.Label10)
        Me.GroupBox5.Controls.Add(Me.ch5)
        Me.GroupBox5.Controls.Add(Me.Label5)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip"))
        '
        'tb5
        '
        resources.ApplyResources(Me.tb5, "tb5")
        Me.tb5.Name = "tb5"
        Me.ToolTip1.SetToolTip(Me.tb5, resources.GetString("tb5.ToolTip"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTip1.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        '
        'cb10
        '
        resources.ApplyResources(Me.cb10, "cb10")
        Me.cb10.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb10.FormattingEnabled = True
        Me.cb10.Items.AddRange(New Object() {resources.GetString("cb10.Items"), resources.GetString("cb10.Items1"), resources.GetString("cb10.Items2"), resources.GetString("cb10.Items3"), resources.GetString("cb10.Items4")})
        Me.cb10.Name = "cb10"
        Me.ToolTip1.SetToolTip(Me.cb10, resources.GetString("cb10.ToolTip"))
        '
        'cb5
        '
        resources.ApplyResources(Me.cb5, "cb5")
        Me.cb5.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb5.FormattingEnabled = True
        Me.cb5.Items.AddRange(New Object() {resources.GetString("cb5.Items"), resources.GetString("cb5.Items1"), resources.GetString("cb5.Items2"), resources.GetString("cb5.Items3"), resources.GetString("cb5.Items4"), resources.GetString("cb5.Items5"), resources.GetString("cb5.Items6"), resources.GetString("cb5.Items7"), resources.GetString("cb5.Items8"), resources.GetString("cb5.Items9"), resources.GetString("cb5.Items10"), resources.GetString("cb5.Items11"), resources.GetString("cb5.Items12"), resources.GetString("cb5.Items13"), resources.GetString("cb5.Items14")})
        Me.cb5.Name = "cb5"
        Me.ToolTip1.SetToolTip(Me.cb5, resources.GetString("cb5.ToolTip"))
        '
        'dgv5
        '
        resources.ApplyResources(Me.dgv5, "dgv5")
        Me.dgv5.AllowDrop = True
        Me.dgv5.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv5.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.dgv5.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv5.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv5.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv5.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn7, Me.DataGridViewTextBoxColumn8})
        Me.dgv5.Name = "dgv5"
        Me.dgv5.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv5.RowHeadersVisible = False
        Me.dgv5.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv5.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.dgv5, resources.GetString("dgv5.ToolTip"))
        '
        'DataGridViewTextBoxColumn7
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn7, "DataGridViewTextBoxColumn7")
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        '
        'DataGridViewTextBoxColumn8
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn8, "DataGridViewTextBoxColumn8")
        Me.DataGridViewTextBoxColumn8.Name = "DataGridViewTextBoxColumn8"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        '
        'ch5
        '
        resources.ApplyResources(Me.ch5, "ch5")
        Me.ch5.Name = "ch5"
        Me.ToolTip1.SetToolTip(Me.ch5, resources.GetString("ch5.ToolTip"))
        Me.ch5.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        '
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.tb4)
        Me.GroupBox4.Controls.Add(Me.Label14)
        Me.GroupBox4.Controls.Add(Me.cb9)
        Me.GroupBox4.Controls.Add(Me.ch4)
        Me.GroupBox4.Controls.Add(Me.cb4)
        Me.GroupBox4.Controls.Add(Me.dgv4)
        Me.GroupBox4.Controls.Add(Me.Label8)
        Me.GroupBox4.Controls.Add(Me.Label1)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip"))
        '
        'tb4
        '
        resources.ApplyResources(Me.tb4, "tb4")
        Me.tb4.Name = "tb4"
        Me.ToolTip1.SetToolTip(Me.tb4, resources.GetString("tb4.ToolTip"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        '
        'cb9
        '
        resources.ApplyResources(Me.cb9, "cb9")
        Me.cb9.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb9.FormattingEnabled = True
        Me.cb9.Items.AddRange(New Object() {resources.GetString("cb9.Items"), resources.GetString("cb9.Items1"), resources.GetString("cb9.Items2"), resources.GetString("cb9.Items3"), resources.GetString("cb9.Items4")})
        Me.cb9.Name = "cb9"
        Me.ToolTip1.SetToolTip(Me.cb9, resources.GetString("cb9.ToolTip"))
        '
        'ch4
        '
        resources.ApplyResources(Me.ch4, "ch4")
        Me.ch4.Name = "ch4"
        Me.ToolTip1.SetToolTip(Me.ch4, resources.GetString("ch4.ToolTip"))
        Me.ch4.UseVisualStyleBackColor = True
        '
        'cb4
        '
        resources.ApplyResources(Me.cb4, "cb4")
        Me.cb4.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb4.FormattingEnabled = True
        Me.cb4.Items.AddRange(New Object() {resources.GetString("cb4.Items"), resources.GetString("cb4.Items1"), resources.GetString("cb4.Items2"), resources.GetString("cb4.Items3"), resources.GetString("cb4.Items4"), resources.GetString("cb4.Items5"), resources.GetString("cb4.Items6"), resources.GetString("cb4.Items7"), resources.GetString("cb4.Items8"), resources.GetString("cb4.Items9"), resources.GetString("cb4.Items10"), resources.GetString("cb4.Items11"), resources.GetString("cb4.Items12"), resources.GetString("cb4.Items13"), resources.GetString("cb4.Items14")})
        Me.cb4.Name = "cb4"
        Me.ToolTip1.SetToolTip(Me.cb4, resources.GetString("cb4.ToolTip"))
        '
        'dgv4
        '
        resources.ApplyResources(Me.dgv4, "dgv4")
        Me.dgv4.AllowDrop = True
        Me.dgv4.AllowUserToResizeRows = False
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv4.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle2
        Me.dgv4.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv4.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv4.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv4.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4})
        Me.dgv4.Name = "dgv4"
        Me.dgv4.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv4.RowHeadersVisible = False
        Me.dgv4.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv4.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.dgv4, resources.GetString("dgv4.ToolTip"))
        '
        'DataGridViewTextBoxColumn3
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        '
        'DataGridViewTextBoxColumn4
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn4, "DataGridViewTextBoxColumn4")
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
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
        Me.cb3.Items.AddRange(New Object() {resources.GetString("cb3.Items"), resources.GetString("cb3.Items1"), resources.GetString("cb3.Items2"), resources.GetString("cb3.Items3"), resources.GetString("cb3.Items4"), resources.GetString("cb3.Items5"), resources.GetString("cb3.Items6"), resources.GetString("cb3.Items7"), resources.GetString("cb3.Items8"), resources.GetString("cb3.Items9"), resources.GetString("cb3.Items10"), resources.GetString("cb3.Items11"), resources.GetString("cb3.Items12"), resources.GetString("cb3.Items13"), resources.GetString("cb3.Items14")})
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
        DataGridViewCellStyle3.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv3.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle3
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
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.tb2)
        Me.GroupBox2.Controls.Add(Me.Label12)
        Me.GroupBox2.Controls.Add(Me.cb7)
        Me.GroupBox2.Controls.Add(Me.ch2)
        Me.GroupBox2.Controls.Add(Me.cb2)
        Me.GroupBox2.Controls.Add(Me.dgv2)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.Label7)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox2, resources.GetString("GroupBox2.ToolTip"))
        '
        'tb2
        '
        resources.ApplyResources(Me.tb2, "tb2")
        Me.tb2.Name = "tb2"
        Me.ToolTip1.SetToolTip(Me.tb2, resources.GetString("tb2.ToolTip"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTip1.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        '
        'cb7
        '
        resources.ApplyResources(Me.cb7, "cb7")
        Me.cb7.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb7.FormattingEnabled = True
        Me.cb7.Items.AddRange(New Object() {resources.GetString("cb7.Items"), resources.GetString("cb7.Items1"), resources.GetString("cb7.Items2"), resources.GetString("cb7.Items3"), resources.GetString("cb7.Items4"), resources.GetString("cb7.Items5"), resources.GetString("cb7.Items6"), resources.GetString("cb7.Items7"), resources.GetString("cb7.Items8"), resources.GetString("cb7.Items9")})
        Me.cb7.Name = "cb7"
        Me.ToolTip1.SetToolTip(Me.cb7, resources.GetString("cb7.ToolTip"))
        '
        'ch2
        '
        resources.ApplyResources(Me.ch2, "ch2")
        Me.ch2.Name = "ch2"
        Me.ToolTip1.SetToolTip(Me.ch2, resources.GetString("ch2.ToolTip"))
        Me.ch2.UseVisualStyleBackColor = True
        '
        'cb2
        '
        resources.ApplyResources(Me.cb2, "cb2")
        Me.cb2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb2.FormattingEnabled = True
        Me.cb2.Items.AddRange(New Object() {resources.GetString("cb2.Items"), resources.GetString("cb2.Items1"), resources.GetString("cb2.Items2"), resources.GetString("cb2.Items3"), resources.GetString("cb2.Items4"), resources.GetString("cb2.Items5"), resources.GetString("cb2.Items6"), resources.GetString("cb2.Items7"), resources.GetString("cb2.Items8"), resources.GetString("cb2.Items9"), resources.GetString("cb2.Items10"), resources.GetString("cb2.Items11"), resources.GetString("cb2.Items12"), resources.GetString("cb2.Items13"), resources.GetString("cb2.Items14")})
        Me.cb2.Name = "cb2"
        Me.ToolTip1.SetToolTip(Me.cb2, resources.GetString("cb2.ToolTip"))
        '
        'dgv2
        '
        resources.ApplyResources(Me.dgv2, "dgv2")
        Me.dgv2.AllowDrop = True
        Me.dgv2.AllowUserToResizeRows = False
        DataGridViewCellStyle4.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv2.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle4
        Me.dgv2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv2.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2})
        Me.dgv2.Name = "dgv2"
        Me.dgv2.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv2.RowHeadersVisible = False
        Me.dgv2.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv2.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.dgv2, resources.GetString("dgv2.ToolTip"))
        '
        'DataGridViewTextBoxColumn1
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        '
        'DataGridViewTextBoxColumn2
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
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
        DataGridViewCellStyle5.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle5
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
        Me.cb1.Items.AddRange(New Object() {resources.GetString("cb1.Items"), resources.GetString("cb1.Items1"), resources.GetString("cb1.Items2"), resources.GetString("cb1.Items3"), resources.GetString("cb1.Items4"), resources.GetString("cb1.Items5"), resources.GetString("cb1.Items6"), resources.GetString("cb1.Items7"), resources.GetString("cb1.Items8"), resources.GetString("cb1.Items9"), resources.GetString("cb1.Items10"), resources.GetString("cb1.Items11"), resources.GetString("cb1.Items12"), resources.GetString("cb1.Items13"), resources.GetString("cb1.Items14")})
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
        Me.FaTabStripItem2.Selected = True
        Me.ToolTip1.SetToolTip(Me.FaTabStripItem2, resources.GetString("FaTabStripItem2.ToolTip"))
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.Label17)
        Me.GroupBox7.Controls.Add(Me.Label18)
        Me.GroupBox7.Controls.Add(Me.cbeffunit)
        Me.GroupBox7.Controls.Add(Me.Label19)
        Me.GroupBox7.Controls.Add(Me.cbpowerunit)
        Me.GroupBox7.Controls.Add(Me.cbheadunit)
        Me.GroupBox7.Controls.Add(Me.Label16)
        Me.GroupBox7.Controls.Add(Me.cbflowunit)
        Me.GroupBox7.Controls.Add(Me.chart1)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox7, resources.GetString("GroupBox7.ToolTip"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTip1.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        '
        'cbeffunit
        '
        resources.ApplyResources(Me.cbeffunit, "cbeffunit")
        Me.cbeffunit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbeffunit.FormattingEnabled = True
        Me.cbeffunit.Items.AddRange(New Object() {resources.GetString("cbeffunit.Items"), resources.GetString("cbeffunit.Items1")})
        Me.cbeffunit.Name = "cbeffunit"
        Me.ToolTip1.SetToolTip(Me.cbeffunit, resources.GetString("cbeffunit.ToolTip"))
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTip1.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        '
        'cbpowerunit
        '
        resources.ApplyResources(Me.cbpowerunit, "cbpowerunit")
        Me.cbpowerunit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbpowerunit.FormattingEnabled = True
        Me.cbpowerunit.Items.AddRange(New Object() {resources.GetString("cbpowerunit.Items"), resources.GetString("cbpowerunit.Items1"), resources.GetString("cbpowerunit.Items2"), resources.GetString("cbpowerunit.Items3"), resources.GetString("cbpowerunit.Items4"), resources.GetString("cbpowerunit.Items5"), resources.GetString("cbpowerunit.Items6"), resources.GetString("cbpowerunit.Items7"), resources.GetString("cbpowerunit.Items8"), resources.GetString("cbpowerunit.Items9")})
        Me.cbpowerunit.Name = "cbpowerunit"
        Me.ToolTip1.SetToolTip(Me.cbpowerunit, resources.GetString("cbpowerunit.ToolTip"))
        '
        'cbheadunit
        '
        resources.ApplyResources(Me.cbheadunit, "cbheadunit")
        Me.cbheadunit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbheadunit.FormattingEnabled = True
        Me.cbheadunit.Items.AddRange(New Object() {resources.GetString("cbheadunit.Items"), resources.GetString("cbheadunit.Items1"), resources.GetString("cbheadunit.Items2"), resources.GetString("cbheadunit.Items3"), resources.GetString("cbheadunit.Items4")})
        Me.cbheadunit.Name = "cbheadunit"
        Me.ToolTip1.SetToolTip(Me.cbheadunit, resources.GetString("cbheadunit.ToolTip"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        '
        'cbflowunit
        '
        resources.ApplyResources(Me.cbflowunit, "cbflowunit")
        Me.cbflowunit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbflowunit.FormattingEnabled = True
        Me.cbflowunit.Items.AddRange(New Object() {resources.GetString("cbflowunit.Items"), resources.GetString("cbflowunit.Items1"), resources.GetString("cbflowunit.Items2"), resources.GetString("cbflowunit.Items3"), resources.GetString("cbflowunit.Items4"), resources.GetString("cbflowunit.Items5"), resources.GetString("cbflowunit.Items6"), resources.GetString("cbflowunit.Items7"), resources.GetString("cbflowunit.Items8"), resources.GetString("cbflowunit.Items9"), resources.GetString("cbflowunit.Items10"), resources.GetString("cbflowunit.Items11"), resources.GetString("cbflowunit.Items12"), resources.GetString("cbflowunit.Items13"), resources.GetString("cbflowunit.Items14")})
        Me.cbflowunit.Name = "cbflowunit"
        Me.ToolTip1.SetToolTip(Me.cbflowunit, resources.GetString("cbflowunit.ToolTip"))
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
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.Label22)
        Me.Panel1.Controls.Add(Me.TBImpellerSpeed)
        Me.Panel1.Controls.Add(Me.Label21)
        Me.Panel1.Controls.Add(Me.CBDiameterUnit)
        Me.Panel1.Controls.Add(Me.TBImpellerDiam)
        Me.Panel1.Controls.Add(Me.Label20)
        Me.Panel1.Name = "Panel1"
        Me.ToolTip1.SetToolTip(Me.Panel1, resources.GetString("Panel1.ToolTip"))
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        Me.ToolTip1.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip"))
        '
        'TBImpellerSpeed
        '
        resources.ApplyResources(Me.TBImpellerSpeed, "TBImpellerSpeed")
        Me.TBImpellerSpeed.Name = "TBImpellerSpeed"
        Me.ToolTip1.SetToolTip(Me.TBImpellerSpeed, resources.GetString("TBImpellerSpeed.ToolTip"))
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTip1.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        '
        'CBDiameterUnit
        '
        resources.ApplyResources(Me.CBDiameterUnit, "CBDiameterUnit")
        Me.CBDiameterUnit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CBDiameterUnit.FormattingEnabled = True
        Me.CBDiameterUnit.Items.AddRange(New Object() {resources.GetString("CBDiameterUnit.Items"), resources.GetString("CBDiameterUnit.Items1"), resources.GetString("CBDiameterUnit.Items2"), resources.GetString("CBDiameterUnit.Items3"), resources.GetString("CBDiameterUnit.Items4")})
        Me.CBDiameterUnit.Name = "CBDiameterUnit"
        Me.ToolTip1.SetToolTip(Me.CBDiameterUnit, resources.GetString("CBDiameterUnit.ToolTip"))
        '
        'TBImpellerDiam
        '
        resources.ApplyResources(Me.TBImpellerDiam, "TBImpellerDiam")
        Me.TBImpellerDiam.Name = "TBImpellerDiam"
        Me.ToolTip1.SetToolTip(Me.TBImpellerDiam, resources.GetString("TBImpellerDiam.ToolTip"))
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTip1.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.DefaultExt = "*.pxml"
        Me.OpenFileDialog1.FileName = "OpenFileDialog1"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'EditingForm_Pump_Curves
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.FaTabStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_Pump_Curves"
        Me.ShowInTaskbar = False
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem1.ResumeLayout(False)
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        CType(Me.dgv5, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        CType(Me.dgv4, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.dgv2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStripItem2.ResumeLayout(False)
        Me.GroupBox7.ResumeLayout(False)
        Me.GroupBox7.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents ch1 As System.Windows.Forms.CheckBox
    Public WithEvents cb1 As System.Windows.Forms.ComboBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents dgv1 As System.Windows.Forms.DataGridView
    Public WithEvents dgv5 As System.Windows.Forms.DataGridView
    Public WithEvents ch5 As System.Windows.Forms.CheckBox
    Public WithEvents ch4 As System.Windows.Forms.CheckBox
    Public WithEvents dgv4 As System.Windows.Forms.DataGridView
    Public WithEvents ch3 As System.Windows.Forms.CheckBox
    Public WithEvents dgv3 As System.Windows.Forms.DataGridView
    Public WithEvents ch2 As System.Windows.Forms.CheckBox
    Public WithEvents dgv2 As System.Windows.Forms.DataGridView
    Public WithEvents cb6 As System.Windows.Forms.ComboBox
    Public WithEvents cb10 As System.Windows.Forms.ComboBox
    Public WithEvents cb5 As System.Windows.Forms.ComboBox
    Public WithEvents cb9 As System.Windows.Forms.ComboBox
    Public WithEvents cb4 As System.Windows.Forms.ComboBox
    Public WithEvents cb8 As System.Windows.Forms.ComboBox
    Public WithEvents cb3 As System.Windows.Forms.ComboBox
    Public WithEvents cb7 As System.Windows.Forms.ComboBox
    Public WithEvents cb2 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Public WithEvents chart1 As ZedGraph.ZedGraphControl
    Public WithEvents tb5 As System.Windows.Forms.TextBox
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents tb4 As System.Windows.Forms.TextBox
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents tb3 As System.Windows.Forms.TextBox
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents tb2 As System.Windows.Forms.TextBox
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents tb1 As System.Windows.Forms.TextBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents cbeffunit As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents cbpowerunit As System.Windows.Forms.ComboBox
    Public WithEvents cbheadunit As System.Windows.Forms.ComboBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents cbflowunit As System.Windows.Forms.ComboBox
    Public WithEvents DataGridViewTextBoxColumn5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents vap As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents temp As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn8 As System.Windows.Forms.DataGridViewTextBoxColumn
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
    Public WithEvents Panel1 As System.Windows.Forms.Panel
    Public WithEvents Label22 As System.Windows.Forms.Label
    Public WithEvents TBImpellerSpeed As System.Windows.Forms.TextBox
    Public WithEvents Label21 As System.Windows.Forms.Label
    Public WithEvents TBImpellerDiam As System.Windows.Forms.TextBox
    Public WithEvents Label20 As System.Windows.Forms.Label
    Public WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents CBDiameterUnit As System.Windows.Forms.ComboBox
End Class
