<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormAssayManager
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormAssayManager))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBoxBulk = New System.Windows.Forms.GroupBox()
        Me.LabelNBP = New System.Windows.Forms.Label()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.LabelV2 = New System.Windows.Forms.Label()
        Me.LabelV1 = New System.Windows.Forms.Label()
        Me.LabelT2 = New System.Windows.Forms.Label()
        Me.LabelT1 = New System.Windows.Forms.Label()
        Me.tb_v2 = New System.Windows.Forms.TextBox()
        Me.tb_v1 = New System.Windows.Forms.TextBox()
        Me.tb_t2 = New System.Windows.Forms.TextBox()
        Me.tb_t1 = New System.Windows.Forms.TextBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tb_wk = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.tb_sg = New System.Windows.Forms.TextBox()
        Me.tb_mw = New System.Windows.Forms.TextBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.GroupBoxCurves = New System.Windows.Forms.GroupBox()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.TextBoxKAPI = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label45 = New System.Windows.Forms.Label()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.TextBoxBulkMW = New System.Windows.Forms.TextBox()
        Me.CheckBoxMW = New System.Windows.Forms.CheckBox()
        Me.TextBoxBulkD = New System.Windows.Forms.TextBox()
        Me.CheckBoxSG = New System.Windows.Forms.CheckBox()
        Me.Label44 = New System.Windows.Forms.Label()
        Me.CheckBoxVISC = New System.Windows.Forms.CheckBox()
        Me.Label43 = New System.Windows.Forms.Label()
        Me.ComboBoxDistMethod = New System.Windows.Forms.ComboBox()
        Me.Label38 = New System.Windows.Forms.Label()
        Me.Label39 = New System.Windows.Forms.Label()
        Me.ComboBoxBasis = New System.Windows.Forms.ComboBox()
        Me.TextBoxVT1 = New System.Windows.Forms.TextBox()
        Me.Label42 = New System.Windows.Forms.Label()
        Me.Label40 = New System.Windows.Forms.Label()
        Me.RadioButtonD20 = New System.Windows.Forms.RadioButton()
        Me.TextBoxVT2 = New System.Windows.Forms.TextBox()
        Me.RadioButtonD60 = New System.Windows.Forms.RadioButton()
        Me.LabelT3 = New System.Windows.Forms.Label()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.gridcurves = New System.Windows.Forms.DataGridView()
        Me.vap = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.temp = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.mm = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.dens = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.visc1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.visc2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.gridassays = New System.Windows.Forms.DataGridView()
        Me.colid = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.colname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tipo = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.GroupBoxBulk.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBoxCurves.SuspendLayout()
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem1.SuspendLayout()
        Me.FaTabStripItem2.SuspendLayout()
        CType(Me.gridcurves, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox2.SuspendLayout()
        CType(Me.gridassays, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxBulk
        '
        Me.GroupBoxBulk.BackColor = System.Drawing.Color.Transparent
        Me.GroupBoxBulk.Controls.Add(Me.LabelNBP)
        Me.GroupBoxBulk.Controls.Add(Me.GroupBox5)
        Me.GroupBoxBulk.Controls.Add(Me.tb_wk)
        Me.GroupBoxBulk.Controls.Add(Me.Label16)
        Me.GroupBoxBulk.Controls.Add(Me.Label17)
        Me.GroupBoxBulk.Controls.Add(Me.tb_sg)
        Me.GroupBoxBulk.Controls.Add(Me.tb_mw)
        Me.GroupBoxBulk.Controls.Add(Me.Label19)
        Me.GroupBoxBulk.Controls.Add(Me.Label20)
        Me.GroupBoxBulk.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        resources.ApplyResources(Me.GroupBoxBulk, "GroupBoxBulk")
        Me.GroupBoxBulk.Name = "GroupBoxBulk"
        Me.GroupBoxBulk.TabStop = False
        '
        'LabelNBP
        '
        resources.ApplyResources(Me.LabelNBP, "LabelNBP")
        Me.LabelNBP.Name = "LabelNBP"
        '
        'GroupBox5
        '
        Me.GroupBox5.Controls.Add(Me.LabelV2)
        Me.GroupBox5.Controls.Add(Me.LabelV1)
        Me.GroupBox5.Controls.Add(Me.LabelT2)
        Me.GroupBox5.Controls.Add(Me.LabelT1)
        Me.GroupBox5.Controls.Add(Me.tb_v2)
        Me.GroupBox5.Controls.Add(Me.tb_v1)
        Me.GroupBox5.Controls.Add(Me.tb_t2)
        Me.GroupBox5.Controls.Add(Me.tb_t1)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Controls.Add(Me.Label8)
        Me.GroupBox5.Controls.Add(Me.Label9)
        Me.GroupBox5.Controls.Add(Me.Label10)
        Me.GroupBox5.FlatStyle = System.Windows.Forms.FlatStyle.Flat
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        '
        'LabelV2
        '
        resources.ApplyResources(Me.LabelV2, "LabelV2")
        Me.LabelV2.Name = "LabelV2"
        '
        'LabelV1
        '
        resources.ApplyResources(Me.LabelV1, "LabelV1")
        Me.LabelV1.Name = "LabelV1"
        '
        'LabelT2
        '
        resources.ApplyResources(Me.LabelT2, "LabelT2")
        Me.LabelT2.Name = "LabelT2"
        '
        'LabelT1
        '
        resources.ApplyResources(Me.LabelT1, "LabelT1")
        Me.LabelT1.Name = "LabelT1"
        '
        'tb_v2
        '
        resources.ApplyResources(Me.tb_v2, "tb_v2")
        Me.tb_v2.Name = "tb_v2"
        '
        'tb_v1
        '
        resources.ApplyResources(Me.tb_v1, "tb_v1")
        Me.tb_v1.Name = "tb_v1"
        '
        'tb_t2
        '
        resources.ApplyResources(Me.tb_t2, "tb_t2")
        Me.tb_t2.Name = "tb_t2"
        '
        'tb_t1
        '
        resources.ApplyResources(Me.tb_t1, "tb_t1")
        Me.tb_t1.Name = "tb_t1"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'tb_wk
        '
        resources.ApplyResources(Me.tb_wk, "tb_wk")
        Me.tb_wk.Name = "tb_wk"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        '
        'tb_sg
        '
        resources.ApplyResources(Me.tb_sg, "tb_sg")
        Me.tb_sg.Name = "tb_sg"
        '
        'tb_mw
        '
        resources.ApplyResources(Me.tb_mw, "tb_mw")
        Me.tb_mw.Name = "tb_mw"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        '
        'GroupBoxCurves
        '
        Me.GroupBoxCurves.Controls.Add(Me.FaTabStrip1)
        resources.ApplyResources(Me.GroupBoxCurves, "GroupBoxCurves")
        Me.GroupBoxCurves.Name = "GroupBoxCurves"
        Me.GroupBoxCurves.TabStop = False
        '
        'FaTabStrip1
        '
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem1, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem1
        '
        'FaTabStripItem1
        '
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.Controls.Add(Me.TextBoxKAPI)
        Me.FaTabStripItem1.Controls.Add(Me.Label7)
        Me.FaTabStripItem1.Controls.Add(Me.Label45)
        Me.FaTabStripItem1.Controls.Add(Me.CheckBox1)
        Me.FaTabStripItem1.Controls.Add(Me.TextBoxBulkMW)
        Me.FaTabStripItem1.Controls.Add(Me.CheckBoxMW)
        Me.FaTabStripItem1.Controls.Add(Me.TextBoxBulkD)
        Me.FaTabStripItem1.Controls.Add(Me.CheckBoxSG)
        Me.FaTabStripItem1.Controls.Add(Me.Label44)
        Me.FaTabStripItem1.Controls.Add(Me.CheckBoxVISC)
        Me.FaTabStripItem1.Controls.Add(Me.Label43)
        Me.FaTabStripItem1.Controls.Add(Me.ComboBoxDistMethod)
        Me.FaTabStripItem1.Controls.Add(Me.Label38)
        Me.FaTabStripItem1.Controls.Add(Me.Label39)
        Me.FaTabStripItem1.Controls.Add(Me.ComboBoxBasis)
        Me.FaTabStripItem1.Controls.Add(Me.TextBoxVT1)
        Me.FaTabStripItem1.Controls.Add(Me.Label42)
        Me.FaTabStripItem1.Controls.Add(Me.Label40)
        Me.FaTabStripItem1.Controls.Add(Me.RadioButtonD20)
        Me.FaTabStripItem1.Controls.Add(Me.TextBoxVT2)
        Me.FaTabStripItem1.Controls.Add(Me.RadioButtonD60)
        Me.FaTabStripItem1.Controls.Add(Me.LabelT3)
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Selected = True
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        '
        'TextBoxKAPI
        '
        resources.ApplyResources(Me.TextBoxKAPI, "TextBoxKAPI")
        Me.TextBoxKAPI.Name = "TextBoxKAPI"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Label45
        '
        resources.ApplyResources(Me.Label45, "Label45")
        Me.Label45.Name = "Label45"
        '
        'CheckBox1
        '
        resources.ApplyResources(Me.CheckBox1, "CheckBox1")
        Me.CheckBox1.Checked = True
        Me.CheckBox1.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'TextBoxBulkMW
        '
        resources.ApplyResources(Me.TextBoxBulkMW, "TextBoxBulkMW")
        Me.TextBoxBulkMW.Name = "TextBoxBulkMW"
        '
        'CheckBoxMW
        '
        resources.ApplyResources(Me.CheckBoxMW, "CheckBoxMW")
        Me.CheckBoxMW.Name = "CheckBoxMW"
        Me.CheckBoxMW.UseVisualStyleBackColor = True
        '
        'TextBoxBulkD
        '
        resources.ApplyResources(Me.TextBoxBulkD, "TextBoxBulkD")
        Me.TextBoxBulkD.Name = "TextBoxBulkD"
        '
        'CheckBoxSG
        '
        resources.ApplyResources(Me.CheckBoxSG, "CheckBoxSG")
        Me.CheckBoxSG.Name = "CheckBoxSG"
        Me.CheckBoxSG.UseVisualStyleBackColor = True
        '
        'Label44
        '
        resources.ApplyResources(Me.Label44, "Label44")
        Me.Label44.Name = "Label44"
        '
        'CheckBoxVISC
        '
        resources.ApplyResources(Me.CheckBoxVISC, "CheckBoxVISC")
        Me.CheckBoxVISC.Name = "CheckBoxVISC"
        Me.CheckBoxVISC.UseVisualStyleBackColor = True
        '
        'Label43
        '
        resources.ApplyResources(Me.Label43, "Label43")
        Me.Label43.Name = "Label43"
        '
        'ComboBoxDistMethod
        '
        Me.ComboBoxDistMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxDistMethod.FormattingEnabled = True
        Me.ComboBoxDistMethod.Items.AddRange(New Object() {resources.GetString("ComboBoxDistMethod.Items"), resources.GetString("ComboBoxDistMethod.Items1"), resources.GetString("ComboBoxDistMethod.Items2"), resources.GetString("ComboBoxDistMethod.Items3")})
        resources.ApplyResources(Me.ComboBoxDistMethod, "ComboBoxDistMethod")
        Me.ComboBoxDistMethod.Name = "ComboBoxDistMethod"
        '
        'Label38
        '
        resources.ApplyResources(Me.Label38, "Label38")
        Me.Label38.Name = "Label38"
        '
        'Label39
        '
        resources.ApplyResources(Me.Label39, "Label39")
        Me.Label39.Name = "Label39"
        '
        'ComboBoxBasis
        '
        Me.ComboBoxBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBoxBasis.FormattingEnabled = True
        Me.ComboBoxBasis.Items.AddRange(New Object() {resources.GetString("ComboBoxBasis.Items"), resources.GetString("ComboBoxBasis.Items1"), resources.GetString("ComboBoxBasis.Items2")})
        resources.ApplyResources(Me.ComboBoxBasis, "ComboBoxBasis")
        Me.ComboBoxBasis.Name = "ComboBoxBasis"
        '
        'TextBoxVT1
        '
        resources.ApplyResources(Me.TextBoxVT1, "TextBoxVT1")
        Me.TextBoxVT1.Name = "TextBoxVT1"
        '
        'Label42
        '
        resources.ApplyResources(Me.Label42, "Label42")
        Me.Label42.Name = "Label42"
        '
        'Label40
        '
        resources.ApplyResources(Me.Label40, "Label40")
        Me.Label40.Name = "Label40"
        '
        'RadioButtonD20
        '
        resources.ApplyResources(Me.RadioButtonD20, "RadioButtonD20")
        Me.RadioButtonD20.Name = "RadioButtonD20"
        Me.RadioButtonD20.UseVisualStyleBackColor = True
        '
        'TextBoxVT2
        '
        resources.ApplyResources(Me.TextBoxVT2, "TextBoxVT2")
        Me.TextBoxVT2.Name = "TextBoxVT2"
        '
        'RadioButtonD60
        '
        resources.ApplyResources(Me.RadioButtonD60, "RadioButtonD60")
        Me.RadioButtonD60.Checked = True
        Me.RadioButtonD60.Name = "RadioButtonD60"
        Me.RadioButtonD60.TabStop = True
        Me.RadioButtonD60.UseVisualStyleBackColor = True
        '
        'LabelT3
        '
        resources.ApplyResources(Me.LabelT3, "LabelT3")
        Me.LabelT3.Name = "LabelT3"
        '
        'FaTabStripItem2
        '
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.gridcurves)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        '
        'gridcurves
        '
        Me.gridcurves.AllowUserToAddRows = False
        Me.gridcurves.AllowUserToDeleteRows = False
        Me.gridcurves.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.gridcurves.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.gridcurves.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridcurves.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.gridcurves.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridcurves.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.vap, Me.temp, Me.mm, Me.dens, Me.visc1, Me.visc2})
        resources.ApplyResources(Me.gridcurves, "gridcurves")
        Me.gridcurves.Name = "gridcurves"
        Me.gridcurves.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.gridcurves.RowHeadersVisible = False
        Me.gridcurves.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.gridcurves.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
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
        'mm
        '
        resources.ApplyResources(Me.mm, "mm")
        Me.mm.Name = "mm"
        '
        'dens
        '
        resources.ApplyResources(Me.dens, "dens")
        Me.dens.Name = "dens"
        '
        'visc1
        '
        resources.ApplyResources(Me.visc1, "visc1")
        Me.visc1.Name = "visc1"
        '
        'visc2
        '
        resources.ApplyResources(Me.visc2, "visc2")
        Me.visc2.Name = "visc2"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.gridassays)
        Me.GroupBox2.Controls.Add(Me.ToolStrip1)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'gridassays
        '
        Me.gridassays.AllowUserToAddRows = False
        Me.gridassays.AllowUserToDeleteRows = False
        Me.gridassays.AllowUserToResizeColumns = False
        Me.gridassays.AllowUserToResizeRows = False
        Me.gridassays.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridassays.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.gridassays.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridassays.ColumnHeadersVisible = False
        Me.gridassays.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.colid, Me.colname, Me.tipo})
        resources.ApplyResources(Me.gridassays, "gridassays")
        Me.gridassays.MultiSelect = False
        Me.gridassays.Name = "gridassays"
        Me.gridassays.RowHeadersVisible = False
        Me.gridassays.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
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
        'tipo
        '
        resources.ApplyResources(Me.tipo, "tipo")
        Me.tipo.Name = "tipo"
        Me.tipo.ReadOnly = True
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton3, Me.ToolStripButton4, Me.ToolStripButton1, Me.ToolStripButton2})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.arrow_down1
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.DialogResult = System.Windows.Forms.DialogResult.Cancel
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'SaveFileDialog1
        '
        Me.SaveFileDialog1.DefaultExt = "dwasf"
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.DefaultExt = "dwasf"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        '
        'FormAssayManager
        '
        Me.AcceptButton = Me.Button1
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.CancelButton = Me.Button2
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBoxCurves)
        Me.Controls.Add(Me.GroupBoxBulk)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormAssayManager"
        Me.ShowInTaskbar = False
        Me.GroupBoxBulk.ResumeLayout(False)
        Me.GroupBoxBulk.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBoxCurves.ResumeLayout(False)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem1.ResumeLayout(False)
        Me.FaTabStripItem1.PerformLayout()
        Me.FaTabStripItem2.ResumeLayout(False)
        CType(Me.gridcurves, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.gridassays, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBoxBulk As System.Windows.Forms.GroupBox
    Public WithEvents LabelNBP As System.Windows.Forms.Label
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents LabelV2 As System.Windows.Forms.Label
    Public WithEvents LabelV1 As System.Windows.Forms.Label
    Public WithEvents LabelT2 As System.Windows.Forms.Label
    Public WithEvents LabelT1 As System.Windows.Forms.Label
    Public WithEvents tb_v2 As System.Windows.Forms.TextBox
    Public WithEvents tb_v1 As System.Windows.Forms.TextBox
    Public WithEvents tb_t2 As System.Windows.Forms.TextBox
    Public WithEvents tb_t1 As System.Windows.Forms.TextBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents tb_wk As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents tb_sg As System.Windows.Forms.TextBox
    Public WithEvents tb_mw As System.Windows.Forms.TextBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents GroupBoxCurves As System.Windows.Forms.GroupBox
    Public WithEvents TextBoxKAPI As System.Windows.Forms.TextBox
    Public WithEvents Label45 As System.Windows.Forms.Label
    Public WithEvents TextBoxBulkMW As System.Windows.Forms.TextBox
    Public WithEvents TextBoxBulkD As System.Windows.Forms.TextBox
    Public WithEvents Label44 As System.Windows.Forms.Label
    Public WithEvents Label43 As System.Windows.Forms.Label
    Public WithEvents Label38 As System.Windows.Forms.Label
    Public WithEvents ComboBoxBasis As System.Windows.Forms.ComboBox
    Public WithEvents Label42 As System.Windows.Forms.Label
    Public WithEvents RadioButtonD20 As System.Windows.Forms.RadioButton
    Public WithEvents RadioButtonD60 As System.Windows.Forms.RadioButton
    Public WithEvents LabelT3 As System.Windows.Forms.Label
    Public WithEvents TextBoxVT2 As System.Windows.Forms.TextBox
    Public WithEvents Label40 As System.Windows.Forms.Label
    Public WithEvents TextBoxVT1 As System.Windows.Forms.TextBox
    Public WithEvents Label39 As System.Windows.Forms.Label
    Public WithEvents ComboBoxDistMethod As System.Windows.Forms.ComboBox
    Public WithEvents CheckBoxVISC As System.Windows.Forms.CheckBox
    Public WithEvents CheckBoxSG As System.Windows.Forms.CheckBox
    Public WithEvents CheckBoxMW As System.Windows.Forms.CheckBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents CheckBox1 As System.Windows.Forms.CheckBox
    Friend WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Friend WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents gridcurves As System.Windows.Forms.DataGridView
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents gridassays As System.Windows.Forms.DataGridView
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Friend WithEvents colid As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents colname As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents tipo As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Public WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Friend WithEvents vap As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents temp As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents mm As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents dens As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents visc1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents visc2 As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
