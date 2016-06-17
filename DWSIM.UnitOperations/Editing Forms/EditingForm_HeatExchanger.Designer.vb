<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_HeatExchanger
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_HeatExchanger))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.btnConfigureFlashAlg = New System.Windows.Forms.Button()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.cbMITA = New System.Windows.Forms.ComboBox()
        Me.tbMITA = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.btnEditSTProps = New System.Windows.Forms.Button()
        Me.cbHeat = New System.Windows.Forms.ComboBox()
        Me.tbHeat = New System.Windows.Forms.TextBox()
        Me.cbArea = New System.Windows.Forms.ComboBox()
        Me.tbArea = New System.Windows.Forms.TextBox()
        Me.cbOverallHTC = New System.Windows.Forms.ComboBox()
        Me.tbOverallU = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbFlowDir = New System.Windows.Forms.ComboBox()
        Me.chkIgnoreLMTD = New System.Windows.Forms.CheckBox()
        Me.cbHotFluidOutletT = New System.Windows.Forms.ComboBox()
        Me.tbHotFluidOutletT = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.cbColdFluidOutletT = New System.Windows.Forms.ComboBox()
        Me.tbColdFluidOutletT = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbHotFluidPDrop = New System.Windows.Forms.ComboBox()
        Me.tbHotFluidPDrop = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbColdFluidPDrop = New System.Windows.Forms.ComboBox()
        Me.tbColdFluidPDrop = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet2 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnDisconnect2 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbOutlet2 = New System.Windows.Forms.ComboBox()
        Me.cbInlet2 = New System.Windows.Forms.ComboBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.btnViewProfile = New System.Windows.Forms.Button()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.lblConnectedTo)
        Me.GroupBox5.Controls.Add(Me.lblStatus)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip"))
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
        Me.ToolTip1.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip"))
        '
        'chkActive
        '
        resources.ApplyResources(Me.chkActive, "chkActive")
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.Name = "chkActive"
        Me.ToolTip1.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.ToolTip1.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip"))
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        Me.ToolTip1.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTip1.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTip1.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTip1.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.btnConfigureFlashAlg)
        Me.GroupBox3.Controls.Add(Me.btnConfigurePP)
        Me.GroupBox3.Controls.Add(Me.cbFlashAlg)
        Me.GroupBox3.Controls.Add(Me.Label10)
        Me.GroupBox3.Controls.Add(Me.cbPropPack)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip"))
        '
        'btnConfigureFlashAlg
        '
        resources.ApplyResources(Me.btnConfigureFlashAlg, "btnConfigureFlashAlg")
        Me.btnConfigureFlashAlg.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigureFlashAlg.Name = "btnConfigureFlashAlg"
        Me.ToolTip1.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip"))
        Me.btnConfigureFlashAlg.UseVisualStyleBackColor = True
        '
        'btnConfigurePP
        '
        resources.ApplyResources(Me.btnConfigurePP, "btnConfigurePP")
        Me.btnConfigurePP.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigurePP.Name = "btnConfigurePP"
        Me.ToolTip1.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip"))
        Me.btnConfigurePP.UseVisualStyleBackColor = True
        '
        'cbFlashAlg
        '
        resources.ApplyResources(Me.cbFlashAlg, "cbFlashAlg")
        Me.cbFlashAlg.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlashAlg.FormattingEnabled = True
        Me.cbFlashAlg.Name = "cbFlashAlg"
        Me.ToolTip1.SetToolTip(Me.cbFlashAlg, resources.GetString("cbFlashAlg.ToolTip"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        '
        'cbPropPack
        '
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Name = "cbPropPack"
        Me.ToolTip1.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTip1.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.cbMITA)
        Me.GroupBox2.Controls.Add(Me.tbMITA)
        Me.GroupBox2.Controls.Add(Me.Label18)
        Me.GroupBox2.Controls.Add(Me.btnEditSTProps)
        Me.GroupBox2.Controls.Add(Me.cbHeat)
        Me.GroupBox2.Controls.Add(Me.tbHeat)
        Me.GroupBox2.Controls.Add(Me.cbArea)
        Me.GroupBox2.Controls.Add(Me.tbArea)
        Me.GroupBox2.Controls.Add(Me.cbOverallHTC)
        Me.GroupBox2.Controls.Add(Me.tbOverallU)
        Me.GroupBox2.Controls.Add(Me.Label17)
        Me.GroupBox2.Controls.Add(Me.Label16)
        Me.GroupBox2.Controls.Add(Me.Label15)
        Me.GroupBox2.Controls.Add(Me.Label14)
        Me.GroupBox2.Controls.Add(Me.cbFlowDir)
        Me.GroupBox2.Controls.Add(Me.chkIgnoreLMTD)
        Me.GroupBox2.Controls.Add(Me.cbHotFluidOutletT)
        Me.GroupBox2.Controls.Add(Me.tbHotFluidOutletT)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Controls.Add(Me.cbColdFluidOutletT)
        Me.GroupBox2.Controls.Add(Me.tbColdFluidOutletT)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.cbHotFluidPDrop)
        Me.GroupBox2.Controls.Add(Me.tbHotFluidPDrop)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.cbColdFluidPDrop)
        Me.GroupBox2.Controls.Add(Me.tbColdFluidPDrop)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.cbCalcMode)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox2, resources.GetString("GroupBox2.ToolTip"))
        '
        'cbMITA
        '
        resources.ApplyResources(Me.cbMITA, "cbMITA")
        Me.cbMITA.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMITA.FormattingEnabled = True
        Me.cbMITA.Items.AddRange(New Object() {resources.GetString("cbMITA.Items"), resources.GetString("cbMITA.Items1"), resources.GetString("cbMITA.Items2")})
        Me.cbMITA.Name = "cbMITA"
        Me.ToolTip1.SetToolTip(Me.cbMITA, resources.GetString("cbMITA.ToolTip"))
        '
        'tbMITA
        '
        resources.ApplyResources(Me.tbMITA, "tbMITA")
        Me.tbMITA.Name = "tbMITA"
        Me.ToolTip1.SetToolTip(Me.tbMITA, resources.GetString("tbMITA.ToolTip"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        '
        'btnEditSTProps
        '
        resources.ApplyResources(Me.btnEditSTProps, "btnEditSTProps")
        Me.btnEditSTProps.Name = "btnEditSTProps"
        Me.ToolTip1.SetToolTip(Me.btnEditSTProps, resources.GetString("btnEditSTProps.ToolTip"))
        Me.btnEditSTProps.UseVisualStyleBackColor = True
        '
        'cbHeat
        '
        resources.ApplyResources(Me.cbHeat, "cbHeat")
        Me.cbHeat.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHeat.FormattingEnabled = True
        Me.cbHeat.Items.AddRange(New Object() {resources.GetString("cbHeat.Items"), resources.GetString("cbHeat.Items1"), resources.GetString("cbHeat.Items2")})
        Me.cbHeat.Name = "cbHeat"
        Me.ToolTip1.SetToolTip(Me.cbHeat, resources.GetString("cbHeat.ToolTip"))
        '
        'tbHeat
        '
        resources.ApplyResources(Me.tbHeat, "tbHeat")
        Me.tbHeat.Name = "tbHeat"
        Me.ToolTip1.SetToolTip(Me.tbHeat, resources.GetString("tbHeat.ToolTip"))
        '
        'cbArea
        '
        resources.ApplyResources(Me.cbArea, "cbArea")
        Me.cbArea.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbArea.FormattingEnabled = True
        Me.cbArea.Items.AddRange(New Object() {resources.GetString("cbArea.Items"), resources.GetString("cbArea.Items1"), resources.GetString("cbArea.Items2")})
        Me.cbArea.Name = "cbArea"
        Me.ToolTip1.SetToolTip(Me.cbArea, resources.GetString("cbArea.ToolTip"))
        '
        'tbArea
        '
        resources.ApplyResources(Me.tbArea, "tbArea")
        Me.tbArea.Name = "tbArea"
        Me.ToolTip1.SetToolTip(Me.tbArea, resources.GetString("tbArea.ToolTip"))
        '
        'cbOverallHTC
        '
        resources.ApplyResources(Me.cbOverallHTC, "cbOverallHTC")
        Me.cbOverallHTC.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOverallHTC.FormattingEnabled = True
        Me.cbOverallHTC.Items.AddRange(New Object() {resources.GetString("cbOverallHTC.Items"), resources.GetString("cbOverallHTC.Items1"), resources.GetString("cbOverallHTC.Items2")})
        Me.cbOverallHTC.Name = "cbOverallHTC"
        Me.ToolTip1.SetToolTip(Me.cbOverallHTC, resources.GetString("cbOverallHTC.ToolTip"))
        '
        'tbOverallU
        '
        resources.ApplyResources(Me.tbOverallU, "tbOverallU")
        Me.tbOverallU.Name = "tbOverallU"
        Me.ToolTip1.SetToolTip(Me.tbOverallU, resources.GetString("tbOverallU.ToolTip"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTip1.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTip1.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        '
        'cbFlowDir
        '
        resources.ApplyResources(Me.cbFlowDir, "cbFlowDir")
        Me.cbFlowDir.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlowDir.FormattingEnabled = True
        Me.cbFlowDir.Items.AddRange(New Object() {resources.GetString("cbFlowDir.Items"), resources.GetString("cbFlowDir.Items1")})
        Me.cbFlowDir.Name = "cbFlowDir"
        Me.ToolTip1.SetToolTip(Me.cbFlowDir, resources.GetString("cbFlowDir.ToolTip"))
        '
        'chkIgnoreLMTD
        '
        resources.ApplyResources(Me.chkIgnoreLMTD, "chkIgnoreLMTD")
        Me.chkIgnoreLMTD.Name = "chkIgnoreLMTD"
        Me.ToolTip1.SetToolTip(Me.chkIgnoreLMTD, resources.GetString("chkIgnoreLMTD.ToolTip"))
        Me.chkIgnoreLMTD.UseVisualStyleBackColor = True
        '
        'cbHotFluidOutletT
        '
        resources.ApplyResources(Me.cbHotFluidOutletT, "cbHotFluidOutletT")
        Me.cbHotFluidOutletT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHotFluidOutletT.FormattingEnabled = True
        Me.cbHotFluidOutletT.Items.AddRange(New Object() {resources.GetString("cbHotFluidOutletT.Items"), resources.GetString("cbHotFluidOutletT.Items1"), resources.GetString("cbHotFluidOutletT.Items2")})
        Me.cbHotFluidOutletT.Name = "cbHotFluidOutletT"
        Me.ToolTip1.SetToolTip(Me.cbHotFluidOutletT, resources.GetString("cbHotFluidOutletT.ToolTip"))
        '
        'tbHotFluidOutletT
        '
        resources.ApplyResources(Me.tbHotFluidOutletT, "tbHotFluidOutletT")
        Me.tbHotFluidOutletT.Name = "tbHotFluidOutletT"
        Me.ToolTip1.SetToolTip(Me.tbHotFluidOutletT, resources.GetString("tbHotFluidOutletT.ToolTip"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        '
        'cbColdFluidOutletT
        '
        resources.ApplyResources(Me.cbColdFluidOutletT, "cbColdFluidOutletT")
        Me.cbColdFluidOutletT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbColdFluidOutletT.FormattingEnabled = True
        Me.cbColdFluidOutletT.Items.AddRange(New Object() {resources.GetString("cbColdFluidOutletT.Items"), resources.GetString("cbColdFluidOutletT.Items1"), resources.GetString("cbColdFluidOutletT.Items2")})
        Me.cbColdFluidOutletT.Name = "cbColdFluidOutletT"
        Me.ToolTip1.SetToolTip(Me.cbColdFluidOutletT, resources.GetString("cbColdFluidOutletT.ToolTip"))
        '
        'tbColdFluidOutletT
        '
        resources.ApplyResources(Me.tbColdFluidOutletT, "tbColdFluidOutletT")
        Me.tbColdFluidOutletT.Name = "tbColdFluidOutletT"
        Me.ToolTip1.SetToolTip(Me.tbColdFluidOutletT, resources.GetString("tbColdFluidOutletT.ToolTip"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        '
        'cbHotFluidPDrop
        '
        resources.ApplyResources(Me.cbHotFluidPDrop, "cbHotFluidPDrop")
        Me.cbHotFluidPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHotFluidPDrop.FormattingEnabled = True
        Me.cbHotFluidPDrop.Items.AddRange(New Object() {resources.GetString("cbHotFluidPDrop.Items"), resources.GetString("cbHotFluidPDrop.Items1"), resources.GetString("cbHotFluidPDrop.Items2")})
        Me.cbHotFluidPDrop.Name = "cbHotFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.cbHotFluidPDrop, resources.GetString("cbHotFluidPDrop.ToolTip"))
        '
        'tbHotFluidPDrop
        '
        resources.ApplyResources(Me.tbHotFluidPDrop, "tbHotFluidPDrop")
        Me.tbHotFluidPDrop.Name = "tbHotFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.tbHotFluidPDrop, resources.GetString("tbHotFluidPDrop.ToolTip"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        '
        'cbColdFluidPDrop
        '
        resources.ApplyResources(Me.cbColdFluidPDrop, "cbColdFluidPDrop")
        Me.cbColdFluidPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbColdFluidPDrop.FormattingEnabled = True
        Me.cbColdFluidPDrop.Items.AddRange(New Object() {resources.GetString("cbColdFluidPDrop.Items"), resources.GetString("cbColdFluidPDrop.Items1"), resources.GetString("cbColdFluidPDrop.Items2")})
        Me.cbColdFluidPDrop.Name = "cbColdFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.cbColdFluidPDrop, resources.GetString("cbColdFluidPDrop.ToolTip"))
        '
        'tbColdFluidPDrop
        '
        resources.ApplyResources(Me.tbColdFluidPDrop, "tbColdFluidPDrop")
        Me.tbColdFluidPDrop.Name = "tbColdFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.tbColdFluidPDrop, resources.GetString("tbColdFluidPDrop.ToolTip"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        '
        'cbCalcMode
        '
        resources.ApplyResources(Me.cbCalcMode, "cbCalcMode")
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2"), resources.GetString("cbCalcMode.Items3"), resources.GetString("cbCalcMode.Items4"), resources.GetString("cbCalcMode.Items5"), resources.GetString("cbCalcMode.Items6"), resources.GetString("cbCalcMode.Items7")})
        Me.cbCalcMode.Name = "cbCalcMode"
        Me.ToolTip1.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.cbOutlet2)
        Me.GroupBox1.Controls.Add(Me.cbInlet2)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect1)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbOutlet1)
        Me.GroupBox1.Controls.Add(Me.cbInlet1)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip"))
        '
        'btnCreateAndConnectOutlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet2, "btnCreateAndConnectOutlet2")
        Me.btnCreateAndConnectOutlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet2.Name = "btnCreateAndConnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip"))
        Me.btnCreateAndConnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet2, "btnCreateAndConnectInlet2")
        Me.btnCreateAndConnectInlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet2.Name = "btnCreateAndConnectInlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip"))
        Me.btnCreateAndConnectInlet2.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet2
        '
        resources.ApplyResources(Me.btnDisconnectOutlet2, "btnDisconnectOutlet2")
        Me.btnDisconnectOutlet2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet2.Name = "btnDisconnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip"))
        Me.btnDisconnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnDisconnect2
        '
        resources.ApplyResources(Me.btnDisconnect2, "btnDisconnect2")
        Me.btnDisconnect2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect2.Name = "btnDisconnect2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip"))
        Me.btnDisconnect2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        '
        'cbOutlet2
        '
        resources.ApplyResources(Me.cbOutlet2, "cbOutlet2")
        Me.cbOutlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet2.FormattingEnabled = True
        Me.cbOutlet2.Name = "cbOutlet2"
        Me.ToolTip1.SetToolTip(Me.cbOutlet2, resources.GetString("cbOutlet2.ToolTip"))
        '
        'cbInlet2
        '
        resources.ApplyResources(Me.cbInlet2, "cbInlet2")
        Me.cbInlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet2.FormattingEnabled = True
        Me.cbInlet2.Name = "cbInlet2"
        Me.ToolTip1.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        '
        'btnCreateAndConnectOutlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet1, "btnCreateAndConnectOutlet1")
        Me.btnCreateAndConnectOutlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip"))
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet1, "btnCreateAndConnectInlet1")
        Me.btnCreateAndConnectInlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip"))
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet1
        '
        resources.ApplyResources(Me.btnDisconnectOutlet1, "btnDisconnectOutlet1")
        Me.btnDisconnectOutlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip"))
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        resources.ApplyResources(Me.btnDisconnect1, "btnDisconnect1")
        Me.btnDisconnect1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip"))
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        '
        'cbOutlet1
        '
        resources.ApplyResources(Me.cbOutlet1, "cbOutlet1")
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Name = "cbOutlet1"
        Me.ToolTip1.SetToolTip(Me.cbOutlet1, resources.GetString("cbOutlet1.ToolTip"))
        '
        'cbInlet1
        '
        resources.ApplyResources(Me.cbInlet1, "cbInlet1")
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Name = "cbInlet1"
        Me.ToolTip1.SetToolTip(Me.cbInlet1, resources.GetString("cbInlet1.ToolTip"))
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTip1.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.btnViewProfile)
        Me.GroupBox6.Controls.Add(Me.gridResults)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        Me.ToolTip1.SetToolTip(Me.GroupBox6, resources.GetString("GroupBox6.ToolTip"))
        '
        'btnViewProfile
        '
        resources.ApplyResources(Me.btnViewProfile, "btnViewProfile")
        Me.btnViewProfile.Name = "btnViewProfile"
        Me.ToolTip1.SetToolTip(Me.btnViewProfile, resources.GetString("btnViewProfile.ToolTip"))
        Me.btnViewProfile.UseVisualStyleBackColor = True
        '
        'gridResults
        '
        resources.ApplyResources(Me.gridResults, "gridResults")
        Me.gridResults.AllowUserToAddRows = False
        Me.gridResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridResults.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridResults.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.Column1})
        Me.gridResults.Name = "gridResults"
        Me.gridResults.ReadOnly = True
        Me.gridResults.RowHeadersVisible = False
        Me.ToolTip1.SetToolTip(Me.gridResults, resources.GetString("gridResults.ToolTip"))
        '
        'DataGridViewTextBoxColumn1
        '
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn1.DefaultCellStyle = DataGridViewCellStyle1
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'Column1
        '
        DataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Control
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle3
        Me.Column1.FillWeight = 30.0!
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'EditingForm_HeatExchanger
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox6)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Name = "EditingForm_HeatExchanger"
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox6.ResumeLayout(False)
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents chkActive As System.Windows.Forms.CheckBox
    Friend WithEvents lblConnectedTo As System.Windows.Forms.Label
    Friend WithEvents lblStatus As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents cbColdFluidPDrop As System.Windows.Forms.ComboBox
    Friend WithEvents tbColdFluidPDrop As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
    Friend WithEvents btnConfigurePP As System.Windows.Forms.Button
    Friend WithEvents cbHotFluidPDrop As System.Windows.Forms.ComboBox
    Friend WithEvents tbHotFluidPDrop As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Friend WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Friend WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents lblTag As System.Windows.Forms.TextBox
    Friend WithEvents cbHotFluidOutletT As System.Windows.Forms.ComboBox
    Friend WithEvents tbHotFluidOutletT As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents cbColdFluidOutletT As System.Windows.Forms.ComboBox
    Friend WithEvents tbColdFluidOutletT As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Friend WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Friend WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Friend WithEvents btnCreateAndConnectInlet2 As System.Windows.Forms.Button
    Friend WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Friend WithEvents btnDisconnect2 As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Friend WithEvents cbInlet2 As System.Windows.Forms.ComboBox
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents btnEditSTProps As System.Windows.Forms.Button
    Friend WithEvents cbHeat As System.Windows.Forms.ComboBox
    Friend WithEvents tbHeat As System.Windows.Forms.TextBox
    Friend WithEvents cbArea As System.Windows.Forms.ComboBox
    Friend WithEvents tbArea As System.Windows.Forms.TextBox
    Friend WithEvents cbOverallHTC As System.Windows.Forms.ComboBox
    Friend WithEvents tbOverallU As System.Windows.Forms.TextBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents cbFlowDir As System.Windows.Forms.ComboBox
    Friend WithEvents chkIgnoreLMTD As System.Windows.Forms.CheckBox
    Friend WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Friend WithEvents gridResults As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents cbMITA As System.Windows.Forms.ComboBox
    Friend WithEvents tbMITA As System.Windows.Forms.TextBox
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents btnViewProfile As System.Windows.Forms.Button
End Class
