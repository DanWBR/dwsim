<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormSimulWizard
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormSimulWizard))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.StepWizardControl1 = New AeroWizard.StepWizardControl()
        Me.WizardPage1 = New AeroWizard.WizardPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.WizardPage2 = New AeroWizard.WizardPage()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.btnInfoLeft = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.ogc1 = New System.Windows.Forms.DataGridView()
        Me.Column11 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colAdd = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.casno = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.WizardPage3 = New AeroWizard.WizardPage()
        Me.LinkLabel2 = New System.Windows.Forms.LinkLabel()
        Me.LinkLabelPropertyMethods = New System.Windows.Forms.LinkLabel()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.ListViewPP = New System.Windows.Forms.ListBox()
        Me.Button8 = New System.Windows.Forms.Button()
        Me.GroupBox12 = New System.Windows.Forms.GroupBox()
        Me.dgvpp = New System.Windows.Forms.DataGridView()
        Me.Column18 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column16 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column17 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column12 = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.WizardPage5 = New AeroWizard.WizardPage()
        Me.btnCreateNewUnits = New System.Windows.Forms.Button()
        Me.btnCloneUnits = New System.Windows.Forms.Button()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column2 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewComboBoxColumn()
        Me.ComboBox2 = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.cmsAddComps = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem4 = New System.Windows.Forms.ToolStripMenuItem()
        Me.CriarAPartirDeEstruturaUNIFACToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        CType(Me.StepWizardControl1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.WizardPage1.SuspendLayout()
        Me.WizardPage2.SuspendLayout()
        CType(Me.ogc1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.WizardPage3.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        Me.GroupBox12.SuspendLayout()
        CType(Me.dgvpp, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.WizardPage5.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.cmsAddComps.SuspendLayout()
        Me.SuspendLayout()
        '
        'StepWizardControl1
        '
        resources.ApplyResources(Me.StepWizardControl1, "StepWizardControl1")
        Me.StepWizardControl1.Name = "StepWizardControl1"
        Me.StepWizardControl1.Pages.Add(Me.WizardPage1)
        Me.StepWizardControl1.Pages.Add(Me.WizardPage2)
        Me.StepWizardControl1.Pages.Add(Me.WizardPage3)
        Me.StepWizardControl1.Pages.Add(Me.WizardPage5)
        Me.StepWizardControl1.ShowProgressInTaskbarIcon = True
        Me.ToolTip1.SetToolTip(Me.StepWizardControl1, resources.GetString("StepWizardControl1.ToolTip"))
        '
        'WizardPage1
        '
        resources.ApplyResources(Me.WizardPage1, "WizardPage1")
        Me.WizardPage1.AllowBack = False
        Me.WizardPage1.Controls.Add(Me.Button1)
        Me.WizardPage1.Controls.Add(Me.Label1)
        Me.WizardPage1.Name = "WizardPage1"
        Me.WizardPage1.NextPage = Me.WizardPage2
        Me.ToolTip1.SetToolTip(Me.WizardPage1, resources.GetString("WizardPage1.ToolTip"))
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.ToolTip1.SetToolTip(Me.Button1, resources.GetString("Button1.ToolTip"))
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        '
        'WizardPage2
        '
        resources.ApplyResources(Me.WizardPage2, "WizardPage2")
        Me.WizardPage2.Controls.Add(Me.Button4)
        Me.WizardPage2.Controls.Add(Me.TextBox1)
        Me.WizardPage2.Controls.Add(Me.btnInfoLeft)
        Me.WizardPage2.Controls.Add(Me.Label2)
        Me.WizardPage2.Controls.Add(Me.Label3)
        Me.WizardPage2.Controls.Add(Me.ogc1)
        Me.WizardPage2.Name = "WizardPage2"
        Me.WizardPage2.NextPage = Me.WizardPage3
        Me.ToolTip1.SetToolTip(Me.WizardPage2, resources.GetString("WizardPage2.ToolTip"))
        '
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.Name = "Button4"
        Me.ToolTip1.SetToolTip(Me.Button4, resources.GetString("Button4.ToolTip"))
        Me.Button4.UseVisualStyleBackColor = True
        '
        'TextBox1
        '
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TextBox1.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        Me.TextBox1.Name = "TextBox1"
        Me.ToolTip1.SetToolTip(Me.TextBox1, resources.GetString("TextBox1.ToolTip"))
        '
        'btnInfoLeft
        '
        resources.ApplyResources(Me.btnInfoLeft, "btnInfoLeft")
        Me.btnInfoLeft.Image = Global.DWSIM.My.Resources.Resources.information
        Me.btnInfoLeft.Name = "btnInfoLeft"
        Me.ToolTip1.SetToolTip(Me.btnInfoLeft, resources.GetString("btnInfoLeft.ToolTip"))
        Me.btnInfoLeft.UseVisualStyleBackColor = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        '
        'ogc1
        '
        resources.ApplyResources(Me.ogc1, "ogc1")
        Me.ogc1.AllowUserToAddRows = False
        Me.ogc1.AllowUserToDeleteRows = False
        Me.ogc1.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.FromArgb(CType(CType(240, Byte), Integer), CType(CType(240, Byte), Integer), CType(CType(240, Byte), Integer))
        Me.ogc1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.ogc1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.ogc1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.ogc1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column11, Me.colAdd, Me.Column6, Me.casno, Me.Column8, Me.Column9, Me.Column7, Me.Column5})
        Me.ogc1.Name = "ogc1"
        Me.ogc1.RowHeadersVisible = False
        Me.ogc1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.ogc1.ShowCellErrors = False
        Me.ogc1.ShowEditingIcon = False
        Me.ogc1.ShowRowErrors = False
        Me.ToolTip1.SetToolTip(Me.ogc1, resources.GetString("ogc1.ToolTip"))
        '
        'Column11
        '
        Me.Column11.FillWeight = 5.0!
        resources.ApplyResources(Me.Column11, "Column11")
        Me.Column11.Name = "Column11"
        Me.Column11.ReadOnly = True
        '
        'colAdd
        '
        Me.colAdd.FalseValue = ""
        Me.colAdd.FillWeight = 15.0!
        resources.ApplyResources(Me.colAdd, "colAdd")
        Me.colAdd.IndeterminateValue = ""
        Me.colAdd.Name = "colAdd"
        Me.colAdd.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        Me.colAdd.TrueValue = ""
        '
        'Column6
        '
        Me.Column6.FillWeight = 40.0!
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        Me.Column6.ReadOnly = True
        '
        'casno
        '
        Me.casno.FillWeight = 25.0!
        resources.ApplyResources(Me.casno, "casno")
        Me.casno.Name = "casno"
        Me.casno.ReadOnly = True
        '
        'Column8
        '
        Me.Column8.FillWeight = 20.0!
        resources.ApplyResources(Me.Column8, "Column8")
        Me.Column8.Name = "Column8"
        Me.Column8.ReadOnly = True
        '
        'Column9
        '
        Me.Column9.FillWeight = 20.0!
        resources.ApplyResources(Me.Column9, "Column9")
        Me.Column9.Name = "Column9"
        Me.Column9.ReadOnly = True
        '
        'Column7
        '
        Me.Column7.FillWeight = 25.0!
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        Me.Column7.ReadOnly = True
        '
        'Column5
        '
        Me.Column5.FalseValue = "False"
        Me.Column5.FillWeight = 10.0!
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        Me.Column5.ReadOnly = True
        Me.Column5.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.Column5.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        Me.Column5.TrueValue = "True"
        '
        'WizardPage3
        '
        resources.ApplyResources(Me.WizardPage3, "WizardPage3")
        Me.WizardPage3.Controls.Add(Me.LinkLabel2)
        Me.WizardPage3.Controls.Add(Me.LinkLabelPropertyMethods)
        Me.WizardPage3.Controls.Add(Me.GroupBox6)
        Me.WizardPage3.Controls.Add(Me.GroupBox12)
        Me.WizardPage3.Controls.Add(Me.Label4)
        Me.WizardPage3.Name = "WizardPage3"
        Me.ToolTip1.SetToolTip(Me.WizardPage3, resources.GetString("WizardPage3.ToolTip"))
        '
        'LinkLabel2
        '
        resources.ApplyResources(Me.LinkLabel2, "LinkLabel2")
        Me.LinkLabel2.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabel2.Name = "LinkLabel2"
        Me.LinkLabel2.TabStop = True
        Me.ToolTip1.SetToolTip(Me.LinkLabel2, resources.GetString("LinkLabel2.ToolTip"))
        '
        'LinkLabelPropertyMethods
        '
        resources.ApplyResources(Me.LinkLabelPropertyMethods, "LinkLabelPropertyMethods")
        Me.LinkLabelPropertyMethods.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.LinkLabelPropertyMethods.Name = "LinkLabelPropertyMethods"
        Me.LinkLabelPropertyMethods.TabStop = True
        Me.ToolTip1.SetToolTip(Me.LinkLabelPropertyMethods, resources.GetString("LinkLabelPropertyMethods.ToolTip"))
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.ListViewPP)
        Me.GroupBox6.Controls.Add(Me.Button8)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox6, resources.GetString("GroupBox6.ToolTip"))
        '
        'ListViewPP
        '
        resources.ApplyResources(Me.ListViewPP, "ListViewPP")
        Me.ListViewPP.FormattingEnabled = True
        Me.ListViewPP.Name = "ListViewPP"
        Me.ToolTip1.SetToolTip(Me.ListViewPP, resources.GetString("ListViewPP.ToolTip"))
        '
        'Button8
        '
        resources.ApplyResources(Me.Button8, "Button8")
        Me.Button8.Name = "Button8"
        Me.ToolTip1.SetToolTip(Me.Button8, resources.GetString("Button8.ToolTip"))
        Me.Button8.UseVisualStyleBackColor = True
        '
        'GroupBox12
        '
        resources.ApplyResources(Me.GroupBox12, "GroupBox12")
        Me.GroupBox12.Controls.Add(Me.dgvpp)
        Me.GroupBox12.Name = "GroupBox12"
        Me.GroupBox12.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox12, resources.GetString("GroupBox12.ToolTip"))
        '
        'dgvpp
        '
        resources.ApplyResources(Me.dgvpp, "dgvpp")
        Me.dgvpp.AllowUserToAddRows = False
        Me.dgvpp.AllowUserToDeleteRows = False
        Me.dgvpp.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgvpp.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgvpp.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.dgvpp.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgvpp.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column18, Me.Column16, Me.Column17, Me.Column12})
        Me.dgvpp.MultiSelect = False
        Me.dgvpp.Name = "dgvpp"
        Me.dgvpp.ReadOnly = True
        Me.dgvpp.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.None
        Me.dgvpp.RowHeadersVisible = False
        Me.dgvpp.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        Me.ToolTip1.SetToolTip(Me.dgvpp, resources.GetString("dgvpp.ToolTip"))
        '
        'Column18
        '
        resources.ApplyResources(Me.Column18, "Column18")
        Me.Column18.Name = "Column18"
        Me.Column18.ReadOnly = True
        '
        'Column16
        '
        Me.Column16.FillWeight = 62.8934!
        resources.ApplyResources(Me.Column16, "Column16")
        Me.Column16.Name = "Column16"
        Me.Column16.ReadOnly = True
        '
        'Column17
        '
        Me.Column17.FillWeight = 116.802!
        resources.ApplyResources(Me.Column17, "Column17")
        Me.Column17.Name = "Column17"
        Me.Column17.ReadOnly = True
        '
        'Column12
        '
        Me.Column12.FillWeight = 15.0!
        resources.ApplyResources(Me.Column12, "Column12")
        Me.Column12.Name = "Column12"
        Me.Column12.ReadOnly = True
        Me.Column12.Text = "..."
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        '
        'WizardPage5
        '
        resources.ApplyResources(Me.WizardPage5, "WizardPage5")
        Me.WizardPage5.AllowCancel = False
        Me.WizardPage5.Controls.Add(Me.btnCreateNewUnits)
        Me.WizardPage5.Controls.Add(Me.btnCloneUnits)
        Me.WizardPage5.Controls.Add(Me.DataGridView1)
        Me.WizardPage5.Controls.Add(Me.ComboBox2)
        Me.WizardPage5.Controls.Add(Me.Label8)
        Me.WizardPage5.Controls.Add(Me.Label7)
        Me.WizardPage5.IsFinishPage = True
        Me.WizardPage5.Name = "WizardPage5"
        Me.ToolTip1.SetToolTip(Me.WizardPage5, resources.GetString("WizardPage5.ToolTip"))
        '
        'btnCreateNewUnits
        '
        resources.ApplyResources(Me.btnCreateNewUnits, "btnCreateNewUnits")
        Me.btnCreateNewUnits.Name = "btnCreateNewUnits"
        Me.ToolTip1.SetToolTip(Me.btnCreateNewUnits, resources.GetString("btnCreateNewUnits.ToolTip"))
        Me.btnCreateNewUnits.UseVisualStyleBackColor = True
        '
        'btnCloneUnits
        '
        resources.ApplyResources(Me.btnCloneUnits, "btnCloneUnits")
        Me.btnCloneUnits.Name = "btnCloneUnits"
        Me.ToolTip1.SetToolTip(Me.btnCloneUnits, resources.GetString("btnCloneUnits.ToolTip"))
        Me.btnCloneUnits.UseVisualStyleBackColor = True
        '
        'DataGridView1
        '
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        Me.DataGridView1.AllowUserToResizeColumns = False
        Me.DataGridView1.AllowUserToResizeRows = False
        Me.DataGridView1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.DataGridView1.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2, Me.Column3, Me.Column4})
        Me.DataGridView1.EditMode = System.Windows.Forms.DataGridViewEditMode.EditOnEnter
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.RowHeadersVisible = False
        Me.DataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.ToolTip1.SetToolTip(Me.DataGridView1, resources.GetString("DataGridView1.ToolTip"))
        '
        'Column1
        '
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.Gainsboro
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle2
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        Me.Column1.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column2
        '
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        Me.Column2.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'Column3
        '
        DataGridViewCellStyle3.BackColor = System.Drawing.Color.Gainsboro
        Me.Column3.DefaultCellStyle = DataGridViewCellStyle3
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        Me.Column3.ReadOnly = True
        Me.Column3.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Column4
        '
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        Me.Column4.ReadOnly = True
        Me.Column4.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        '
        'ComboBox2
        '
        resources.ApplyResources(Me.ComboBox2, "ComboBox2")
        Me.ComboBox2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox2.DropDownWidth = 205
        Me.ComboBox2.Name = "ComboBox2"
        Me.ToolTip1.SetToolTip(Me.ComboBox2, resources.GetString("ComboBox2.ToolTip"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        '
        'ToolTip1
        '
        Me.ToolTip1.IsBalloon = True
        Me.ToolTip1.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTip1.ToolTipTitle = "Help"
        '
        'cmsAddComps
        '
        resources.ApplyResources(Me.cmsAddComps, "cmsAddComps")
        Me.cmsAddComps.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1, Me.ToolStripMenuItem2, Me.ToolStripMenuItem3, Me.ToolStripMenuItem4, Me.CriarAPartirDeEstruturaUNIFACToolStripMenuItem})
        Me.cmsAddComps.Name = "cmsAddComps"
        Me.cmsAddComps.ShowImageMargin = False
        Me.ToolTip1.SetToolTip(Me.cmsAddComps, resources.GetString("cmsAddComps.ToolTip"))
        '
        'ToolStripMenuItem1
        '
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        '
        'ToolStripMenuItem2
        '
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        '
        'ToolStripMenuItem3
        '
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        '
        'ToolStripMenuItem4
        '
        resources.ApplyResources(Me.ToolStripMenuItem4, "ToolStripMenuItem4")
        Me.ToolStripMenuItem4.Name = "ToolStripMenuItem4"
        '
        'CriarAPartirDeEstruturaUNIFACToolStripMenuItem
        '
        resources.ApplyResources(Me.CriarAPartirDeEstruturaUNIFACToolStripMenuItem, "CriarAPartirDeEstruturaUNIFACToolStripMenuItem")
        Me.CriarAPartirDeEstruturaUNIFACToolStripMenuItem.Name = "CriarAPartirDeEstruturaUNIFACToolStripMenuItem"
        '
        'OpenFileDialog1
        '
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        Me.OpenFileDialog1.Multiselect = True
        '
        'FormSimulWizard
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ControlBox = False
        Me.Controls.Add(Me.StepWizardControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FormSimulWizard"
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        CType(Me.StepWizardControl1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.WizardPage1.ResumeLayout(False)
        Me.WizardPage2.ResumeLayout(False)
        Me.WizardPage2.PerformLayout()
        CType(Me.ogc1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.WizardPage3.ResumeLayout(False)
        Me.WizardPage3.PerformLayout()
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        Me.GroupBox12.ResumeLayout(False)
        CType(Me.dgvpp, System.ComponentModel.ISupportInitialize).EndInit()
        Me.WizardPage5.ResumeLayout(False)
        Me.WizardPage5.PerformLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.cmsAddComps.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents StepWizardControl1 As AeroWizard.StepWizardControl
    Friend WithEvents WizardPage1 As AeroWizard.WizardPage
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents WizardPage2 As AeroWizard.WizardPage
    Friend WithEvents WizardPage3 As AeroWizard.WizardPage
    Friend WithEvents WizardPage5 As AeroWizard.WizardPage
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents ogc1 As System.Windows.Forms.DataGridView
    Public WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents LinkLabelPropertyMethods As System.Windows.Forms.LinkLabel
    Public WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Public WithEvents Button8 As System.Windows.Forms.Button
    Public WithEvents GroupBox12 As System.Windows.Forms.GroupBox
    Public WithEvents dgvpp As System.Windows.Forms.DataGridView
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Public WithEvents ComboBox2 As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Column18 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column16 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column17 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column12 As System.Windows.Forms.DataGridViewButtonColumn
    Public WithEvents btnInfoLeft As System.Windows.Forms.Button
    Friend WithEvents LinkLabel2 As System.Windows.Forms.LinkLabel
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents Column2 As DataGridViewComboBoxColumn
    Friend WithEvents Column3 As DataGridViewTextBoxColumn
    Friend WithEvents Column4 As DataGridViewComboBoxColumn
    Friend WithEvents btnCreateNewUnits As Button
    Friend WithEvents btnCloneUnits As Button
    Friend WithEvents Column11 As DataGridViewTextBoxColumn
    Friend WithEvents colAdd As DataGridViewCheckBoxColumn
    Friend WithEvents Column6 As DataGridViewTextBoxColumn
    Friend WithEvents casno As DataGridViewTextBoxColumn
    Friend WithEvents Column8 As DataGridViewTextBoxColumn
    Friend WithEvents Column9 As DataGridViewTextBoxColumn
    Friend WithEvents Column7 As DataGridViewTextBoxColumn
    Friend WithEvents Column5 As DataGridViewCheckBoxColumn
    Friend WithEvents Button4 As Button
    Friend WithEvents cmsAddComps As ContextMenuStrip
    Friend WithEvents ToolStripMenuItem1 As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem2 As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem3 As ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem4 As ToolStripMenuItem
    Friend WithEvents ListViewPP As ListBox
    Friend WithEvents CriarAPartirDeEstruturaUNIFACToolStripMenuItem As ToolStripMenuItem
End Class
