<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_HeatExchanger
    Inherits SharedClasses.ObjectEditorForm

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
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle9 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.chkCalculateProfile = New System.Windows.Forms.CheckBox()
        Me.chkForcePinchToOutlets = New System.Windows.Forms.CheckBox()
        Me.tbOVF2 = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tbOVF1 = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.cbEfficiency = New System.Windows.Forms.ComboBox()
        Me.tbEfficiency = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.cbHeatLoss = New System.Windows.Forms.ComboBox()
        Me.tbHeatLoss = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
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
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
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
        Me.GroupBoxResults = New System.Windows.Forms.GroupBox()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.btnViewProfile = New System.Windows.Forms.Button()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBoxResults.SuspendLayout()
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
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip2"))
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
        Me.ToolTipValues.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip2"))
        '
        'chkActive
        '
        resources.ApplyResources(Me.chkActive, "chkActive")
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.Name = "chkActive"
        Me.ToolTip1.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip2"))
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.ToolTip1.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip2"))
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        Me.ToolTip1.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip2"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTip1.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip2"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTip1.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip2"))
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTip1.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip2"))
        '
        'GroupBoxParameters
        '
        resources.ApplyResources(Me.GroupBoxParameters, "GroupBoxParameters")
        Me.GroupBoxParameters.Controls.Add(Me.chkCalculateProfile)
        Me.GroupBoxParameters.Controls.Add(Me.chkForcePinchToOutlets)
        Me.GroupBoxParameters.Controls.Add(Me.tbOVF2)
        Me.GroupBoxParameters.Controls.Add(Me.Label10)
        Me.GroupBoxParameters.Controls.Add(Me.tbOVF1)
        Me.GroupBoxParameters.Controls.Add(Me.Label9)
        Me.GroupBoxParameters.Controls.Add(Me.cbEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.tbEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.Label21)
        Me.GroupBoxParameters.Controls.Add(Me.cbHeatLoss)
        Me.GroupBoxParameters.Controls.Add(Me.tbHeatLoss)
        Me.GroupBoxParameters.Controls.Add(Me.Label20)
        Me.GroupBoxParameters.Controls.Add(Me.cbMITA)
        Me.GroupBoxParameters.Controls.Add(Me.tbMITA)
        Me.GroupBoxParameters.Controls.Add(Me.Label18)
        Me.GroupBoxParameters.Controls.Add(Me.btnEditSTProps)
        Me.GroupBoxParameters.Controls.Add(Me.cbHeat)
        Me.GroupBoxParameters.Controls.Add(Me.tbHeat)
        Me.GroupBoxParameters.Controls.Add(Me.cbArea)
        Me.GroupBoxParameters.Controls.Add(Me.tbArea)
        Me.GroupBoxParameters.Controls.Add(Me.cbOverallHTC)
        Me.GroupBoxParameters.Controls.Add(Me.tbOverallU)
        Me.GroupBoxParameters.Controls.Add(Me.Label17)
        Me.GroupBoxParameters.Controls.Add(Me.Label16)
        Me.GroupBoxParameters.Controls.Add(Me.Label15)
        Me.GroupBoxParameters.Controls.Add(Me.Label14)
        Me.GroupBoxParameters.Controls.Add(Me.cbFlowDir)
        Me.GroupBoxParameters.Controls.Add(Me.chkIgnoreLMTD)
        Me.GroupBoxParameters.Controls.Add(Me.cbHotFluidOutletT)
        Me.GroupBoxParameters.Controls.Add(Me.tbHotFluidOutletT)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.cbColdFluidOutletT)
        Me.GroupBoxParameters.Controls.Add(Me.tbColdFluidOutletT)
        Me.GroupBoxParameters.Controls.Add(Me.Label4)
        Me.GroupBoxParameters.Controls.Add(Me.cbHotFluidPDrop)
        Me.GroupBoxParameters.Controls.Add(Me.tbHotFluidPDrop)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.cbColdFluidPDrop)
        Me.GroupBoxParameters.Controls.Add(Me.tbColdFluidPDrop)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.cbCalcMode)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'chkCalculateProfile
        '
        resources.ApplyResources(Me.chkCalculateProfile, "chkCalculateProfile")
        Me.chkCalculateProfile.Name = "chkCalculateProfile"
        Me.ToolTip1.SetToolTip(Me.chkCalculateProfile, resources.GetString("chkCalculateProfile.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkCalculateProfile, resources.GetString("chkCalculateProfile.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkCalculateProfile, resources.GetString("chkCalculateProfile.ToolTip2"))
        Me.chkCalculateProfile.UseVisualStyleBackColor = True
        '
        'chkForcePinchToOutlets
        '
        resources.ApplyResources(Me.chkForcePinchToOutlets, "chkForcePinchToOutlets")
        Me.chkForcePinchToOutlets.Name = "chkForcePinchToOutlets"
        Me.ToolTip1.SetToolTip(Me.chkForcePinchToOutlets, resources.GetString("chkForcePinchToOutlets.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkForcePinchToOutlets, resources.GetString("chkForcePinchToOutlets.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkForcePinchToOutlets, resources.GetString("chkForcePinchToOutlets.ToolTip2"))
        Me.chkForcePinchToOutlets.UseVisualStyleBackColor = True
        '
        'tbOVF2
        '
        resources.ApplyResources(Me.tbOVF2, "tbOVF2")
        Me.tbOVF2.Name = "tbOVF2"
        Me.ToolTipValues.SetToolTip(Me.tbOVF2, resources.GetString("tbOVF2.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOVF2, resources.GetString("tbOVF2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOVF2, resources.GetString("tbOVF2.ToolTip2"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip2"))
        '
        'tbOVF1
        '
        resources.ApplyResources(Me.tbOVF1, "tbOVF1")
        Me.tbOVF1.Name = "tbOVF1"
        Me.ToolTipValues.SetToolTip(Me.tbOVF1, resources.GetString("tbOVF1.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOVF1, resources.GetString("tbOVF1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOVF1, resources.GetString("tbOVF1.ToolTip2"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTip1.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip2"))
        '
        'cbEfficiency
        '
        resources.ApplyResources(Me.cbEfficiency, "cbEfficiency")
        Me.cbEfficiency.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEfficiency.FormattingEnabled = True
        Me.cbEfficiency.Items.AddRange(New Object() {resources.GetString("cbEfficiency.Items")})
        Me.cbEfficiency.Name = "cbEfficiency"
        Me.ToolTip1.SetToolTip(Me.cbEfficiency, resources.GetString("cbEfficiency.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbEfficiency, resources.GetString("cbEfficiency.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbEfficiency, resources.GetString("cbEfficiency.ToolTip2"))
        '
        'tbEfficiency
        '
        resources.ApplyResources(Me.tbEfficiency, "tbEfficiency")
        Me.tbEfficiency.Name = "tbEfficiency"
        Me.ToolTipValues.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip2"))
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTip1.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip2"))
        '
        'cbHeatLoss
        '
        resources.ApplyResources(Me.cbHeatLoss, "cbHeatLoss")
        Me.cbHeatLoss.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHeatLoss.FormattingEnabled = True
        Me.cbHeatLoss.Items.AddRange(New Object() {resources.GetString("cbHeatLoss.Items"), resources.GetString("cbHeatLoss.Items1"), resources.GetString("cbHeatLoss.Items2")})
        Me.cbHeatLoss.Name = "cbHeatLoss"
        Me.ToolTip1.SetToolTip(Me.cbHeatLoss, resources.GetString("cbHeatLoss.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbHeatLoss, resources.GetString("cbHeatLoss.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbHeatLoss, resources.GetString("cbHeatLoss.ToolTip2"))
        '
        'tbHeatLoss
        '
        resources.ApplyResources(Me.tbHeatLoss, "tbHeatLoss")
        Me.tbHeatLoss.Name = "tbHeatLoss"
        Me.ToolTipValues.SetToolTip(Me.tbHeatLoss, resources.GetString("tbHeatLoss.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbHeatLoss, resources.GetString("tbHeatLoss.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbHeatLoss, resources.GetString("tbHeatLoss.ToolTip2"))
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTip1.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip2"))
        '
        'cbMITA
        '
        resources.ApplyResources(Me.cbMITA, "cbMITA")
        Me.cbMITA.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMITA.FormattingEnabled = True
        Me.cbMITA.Items.AddRange(New Object() {resources.GetString("cbMITA.Items"), resources.GetString("cbMITA.Items1"), resources.GetString("cbMITA.Items2")})
        Me.cbMITA.Name = "cbMITA"
        Me.ToolTip1.SetToolTip(Me.cbMITA, resources.GetString("cbMITA.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbMITA, resources.GetString("cbMITA.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbMITA, resources.GetString("cbMITA.ToolTip2"))
        '
        'tbMITA
        '
        resources.ApplyResources(Me.tbMITA, "tbMITA")
        Me.tbMITA.Name = "tbMITA"
        Me.ToolTipValues.SetToolTip(Me.tbMITA, resources.GetString("tbMITA.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbMITA, resources.GetString("tbMITA.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbMITA, resources.GetString("tbMITA.ToolTip2"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip2"))
        '
        'btnEditSTProps
        '
        resources.ApplyResources(Me.btnEditSTProps, "btnEditSTProps")
        Me.btnEditSTProps.Name = "btnEditSTProps"
        Me.ToolTip1.SetToolTip(Me.btnEditSTProps, resources.GetString("btnEditSTProps.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnEditSTProps, resources.GetString("btnEditSTProps.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnEditSTProps, resources.GetString("btnEditSTProps.ToolTip2"))
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
        Me.ToolTipValues.SetToolTip(Me.cbHeat, resources.GetString("cbHeat.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbHeat, resources.GetString("cbHeat.ToolTip2"))
        '
        'tbHeat
        '
        resources.ApplyResources(Me.tbHeat, "tbHeat")
        Me.tbHeat.Name = "tbHeat"
        Me.ToolTipValues.SetToolTip(Me.tbHeat, resources.GetString("tbHeat.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbHeat, resources.GetString("tbHeat.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbHeat, resources.GetString("tbHeat.ToolTip2"))
        '
        'cbArea
        '
        resources.ApplyResources(Me.cbArea, "cbArea")
        Me.cbArea.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbArea.FormattingEnabled = True
        Me.cbArea.Items.AddRange(New Object() {resources.GetString("cbArea.Items"), resources.GetString("cbArea.Items1"), resources.GetString("cbArea.Items2")})
        Me.cbArea.Name = "cbArea"
        Me.ToolTip1.SetToolTip(Me.cbArea, resources.GetString("cbArea.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbArea, resources.GetString("cbArea.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbArea, resources.GetString("cbArea.ToolTip2"))
        '
        'tbArea
        '
        resources.ApplyResources(Me.tbArea, "tbArea")
        Me.tbArea.Name = "tbArea"
        Me.ToolTipValues.SetToolTip(Me.tbArea, resources.GetString("tbArea.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbArea, resources.GetString("tbArea.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbArea, resources.GetString("tbArea.ToolTip2"))
        '
        'cbOverallHTC
        '
        resources.ApplyResources(Me.cbOverallHTC, "cbOverallHTC")
        Me.cbOverallHTC.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOverallHTC.FormattingEnabled = True
        Me.cbOverallHTC.Items.AddRange(New Object() {resources.GetString("cbOverallHTC.Items"), resources.GetString("cbOverallHTC.Items1"), resources.GetString("cbOverallHTC.Items2")})
        Me.cbOverallHTC.Name = "cbOverallHTC"
        Me.ToolTip1.SetToolTip(Me.cbOverallHTC, resources.GetString("cbOverallHTC.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOverallHTC, resources.GetString("cbOverallHTC.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOverallHTC, resources.GetString("cbOverallHTC.ToolTip2"))
        '
        'tbOverallU
        '
        resources.ApplyResources(Me.tbOverallU, "tbOverallU")
        Me.tbOverallU.Name = "tbOverallU"
        Me.ToolTipValues.SetToolTip(Me.tbOverallU, resources.GetString("tbOverallU.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOverallU, resources.GetString("tbOverallU.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOverallU, resources.GetString("tbOverallU.ToolTip2"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTip1.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip2"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip2"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTip1.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip2"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip2"))
        '
        'cbFlowDir
        '
        resources.ApplyResources(Me.cbFlowDir, "cbFlowDir")
        Me.cbFlowDir.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlowDir.FormattingEnabled = True
        Me.cbFlowDir.Items.AddRange(New Object() {resources.GetString("cbFlowDir.Items"), resources.GetString("cbFlowDir.Items1")})
        Me.cbFlowDir.Name = "cbFlowDir"
        Me.ToolTip1.SetToolTip(Me.cbFlowDir, resources.GetString("cbFlowDir.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbFlowDir, resources.GetString("cbFlowDir.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbFlowDir, resources.GetString("cbFlowDir.ToolTip2"))
        '
        'chkIgnoreLMTD
        '
        resources.ApplyResources(Me.chkIgnoreLMTD, "chkIgnoreLMTD")
        Me.chkIgnoreLMTD.Name = "chkIgnoreLMTD"
        Me.ToolTip1.SetToolTip(Me.chkIgnoreLMTD, resources.GetString("chkIgnoreLMTD.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkIgnoreLMTD, resources.GetString("chkIgnoreLMTD.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkIgnoreLMTD, resources.GetString("chkIgnoreLMTD.ToolTip2"))
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
        Me.ToolTipValues.SetToolTip(Me.cbHotFluidOutletT, resources.GetString("cbHotFluidOutletT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbHotFluidOutletT, resources.GetString("cbHotFluidOutletT.ToolTip2"))
        '
        'tbHotFluidOutletT
        '
        resources.ApplyResources(Me.tbHotFluidOutletT, "tbHotFluidOutletT")
        Me.tbHotFluidOutletT.Name = "tbHotFluidOutletT"
        Me.ToolTipValues.SetToolTip(Me.tbHotFluidOutletT, resources.GetString("tbHotFluidOutletT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbHotFluidOutletT, resources.GetString("tbHotFluidOutletT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbHotFluidOutletT, resources.GetString("tbHotFluidOutletT.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'cbColdFluidOutletT
        '
        resources.ApplyResources(Me.cbColdFluidOutletT, "cbColdFluidOutletT")
        Me.cbColdFluidOutletT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbColdFluidOutletT.FormattingEnabled = True
        Me.cbColdFluidOutletT.Items.AddRange(New Object() {resources.GetString("cbColdFluidOutletT.Items"), resources.GetString("cbColdFluidOutletT.Items1"), resources.GetString("cbColdFluidOutletT.Items2")})
        Me.cbColdFluidOutletT.Name = "cbColdFluidOutletT"
        Me.ToolTip1.SetToolTip(Me.cbColdFluidOutletT, resources.GetString("cbColdFluidOutletT.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbColdFluidOutletT, resources.GetString("cbColdFluidOutletT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbColdFluidOutletT, resources.GetString("cbColdFluidOutletT.ToolTip2"))
        '
        'tbColdFluidOutletT
        '
        resources.ApplyResources(Me.tbColdFluidOutletT, "tbColdFluidOutletT")
        Me.tbColdFluidOutletT.Name = "tbColdFluidOutletT"
        Me.ToolTipValues.SetToolTip(Me.tbColdFluidOutletT, resources.GetString("tbColdFluidOutletT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbColdFluidOutletT, resources.GetString("tbColdFluidOutletT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbColdFluidOutletT, resources.GetString("tbColdFluidOutletT.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'cbHotFluidPDrop
        '
        resources.ApplyResources(Me.cbHotFluidPDrop, "cbHotFluidPDrop")
        Me.cbHotFluidPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHotFluidPDrop.FormattingEnabled = True
        Me.cbHotFluidPDrop.Items.AddRange(New Object() {resources.GetString("cbHotFluidPDrop.Items"), resources.GetString("cbHotFluidPDrop.Items1"), resources.GetString("cbHotFluidPDrop.Items2")})
        Me.cbHotFluidPDrop.Name = "cbHotFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.cbHotFluidPDrop, resources.GetString("cbHotFluidPDrop.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbHotFluidPDrop, resources.GetString("cbHotFluidPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbHotFluidPDrop, resources.GetString("cbHotFluidPDrop.ToolTip2"))
        '
        'tbHotFluidPDrop
        '
        resources.ApplyResources(Me.tbHotFluidPDrop, "tbHotFluidPDrop")
        Me.tbHotFluidPDrop.Name = "tbHotFluidPDrop"
        Me.ToolTipValues.SetToolTip(Me.tbHotFluidPDrop, resources.GetString("tbHotFluidPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbHotFluidPDrop, resources.GetString("tbHotFluidPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbHotFluidPDrop, resources.GetString("tbHotFluidPDrop.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbColdFluidPDrop
        '
        resources.ApplyResources(Me.cbColdFluidPDrop, "cbColdFluidPDrop")
        Me.cbColdFluidPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbColdFluidPDrop.FormattingEnabled = True
        Me.cbColdFluidPDrop.Items.AddRange(New Object() {resources.GetString("cbColdFluidPDrop.Items"), resources.GetString("cbColdFluidPDrop.Items1"), resources.GetString("cbColdFluidPDrop.Items2")})
        Me.cbColdFluidPDrop.Name = "cbColdFluidPDrop"
        Me.ToolTip1.SetToolTip(Me.cbColdFluidPDrop, resources.GetString("cbColdFluidPDrop.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbColdFluidPDrop, resources.GetString("cbColdFluidPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbColdFluidPDrop, resources.GetString("cbColdFluidPDrop.ToolTip2"))
        '
        'tbColdFluidPDrop
        '
        resources.ApplyResources(Me.tbColdFluidPDrop, "tbColdFluidPDrop")
        Me.tbColdFluidPDrop.Name = "tbColdFluidPDrop"
        Me.ToolTipValues.SetToolTip(Me.tbColdFluidPDrop, resources.GetString("tbColdFluidPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbColdFluidPDrop, resources.GetString("tbColdFluidPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbColdFluidPDrop, resources.GetString("tbColdFluidPDrop.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'cbCalcMode
        '
        resources.ApplyResources(Me.cbCalcMode, "cbCalcMode")
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2"), resources.GetString("cbCalcMode.Items3"), resources.GetString("cbCalcMode.Items4"), resources.GetString("cbCalcMode.Items5"), resources.GetString("cbCalcMode.Items6"), resources.GetString("cbCalcMode.Items7"), resources.GetString("cbCalcMode.Items8"), resources.GetString("cbCalcMode.Items9"), resources.GetString("cbCalcMode.Items10")})
        Me.cbCalcMode.Name = "cbCalcMode"
        Me.ToolTip1.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip2"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip2"))
        '
        'GroupBoxConnections
        '
        resources.ApplyResources(Me.GroupBoxConnections, "GroupBoxConnections")
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect2)
        Me.GroupBoxConnections.Controls.Add(Me.Label1)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet2)
        Me.GroupBoxConnections.Controls.Add(Me.Label6)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect1)
        Me.GroupBoxConnections.Controls.Add(Me.Label7)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.Label19)
        Me.GroupBoxConnections.Name = "GroupBoxConnections"
        Me.GroupBoxConnections.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxConnections, resources.GetString("GroupBoxConnections.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxConnections, resources.GetString("GroupBoxConnections.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxConnections, resources.GetString("GroupBoxConnections.ToolTip2"))
        '
        'btnCreateAndConnectOutlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet2, "btnCreateAndConnectOutlet2")
        Me.btnCreateAndConnectOutlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet2.Name = "btnCreateAndConnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip2"))
        Me.btnCreateAndConnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet2, "btnCreateAndConnectInlet2")
        Me.btnCreateAndConnectInlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet2.Name = "btnCreateAndConnectInlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip2"))
        Me.btnCreateAndConnectInlet2.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet2
        '
        resources.ApplyResources(Me.btnDisconnectOutlet2, "btnDisconnectOutlet2")
        Me.btnDisconnectOutlet2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet2.Name = "btnDisconnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip2"))
        Me.btnDisconnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnDisconnect2
        '
        resources.ApplyResources(Me.btnDisconnect2, "btnDisconnect2")
        Me.btnDisconnect2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect2.Name = "btnDisconnect2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip2"))
        Me.btnDisconnect2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'cbOutlet2
        '
        resources.ApplyResources(Me.cbOutlet2, "cbOutlet2")
        Me.cbOutlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet2.FormattingEnabled = True
        Me.cbOutlet2.Name = "cbOutlet2"
        Me.ToolTip1.SetToolTip(Me.cbOutlet2, resources.GetString("cbOutlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOutlet2, resources.GetString("cbOutlet2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOutlet2, resources.GetString("cbOutlet2.ToolTip2"))
        '
        'cbInlet2
        '
        resources.ApplyResources(Me.cbInlet2, "cbInlet2")
        Me.cbInlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet2.FormattingEnabled = True
        Me.cbInlet2.Name = "cbInlet2"
        Me.ToolTip1.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip2"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'btnCreateAndConnectOutlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet1, "btnCreateAndConnectOutlet1")
        Me.btnCreateAndConnectOutlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip2"))
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet1, "btnCreateAndConnectInlet1")
        Me.btnCreateAndConnectInlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip2"))
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet1
        '
        resources.ApplyResources(Me.btnDisconnectOutlet1, "btnDisconnectOutlet1")
        Me.btnDisconnectOutlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip2"))
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        resources.ApplyResources(Me.btnDisconnect1, "btnDisconnect1")
        Me.btnDisconnect1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip2"))
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip2"))
        '
        'cbOutlet1
        '
        resources.ApplyResources(Me.cbOutlet1, "cbOutlet1")
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Name = "cbOutlet1"
        Me.ToolTip1.SetToolTip(Me.cbOutlet1, resources.GetString("cbOutlet1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOutlet1, resources.GetString("cbOutlet1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOutlet1, resources.GetString("cbOutlet1.ToolTip2"))
        '
        'cbInlet1
        '
        resources.ApplyResources(Me.cbInlet1, "cbInlet1")
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Name = "cbInlet1"
        Me.ToolTip1.SetToolTip(Me.cbInlet1, resources.GetString("cbInlet1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet1, resources.GetString("cbInlet1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet1, resources.GetString("cbInlet1.ToolTip2"))
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTip1.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip2"))
        '
        'GroupBoxResults
        '
        resources.ApplyResources(Me.GroupBoxResults, "GroupBoxResults")
        Me.GroupBoxResults.Controls.Add(Me.gridResults)
        Me.GroupBoxResults.Controls.Add(Me.btnViewProfile)
        Me.GroupBoxResults.Name = "GroupBoxResults"
        Me.GroupBoxResults.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip2"))
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
        Me.ToolTipChangeTag.SetToolTip(Me.gridResults, resources.GetString("gridResults.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.gridResults, resources.GetString("gridResults.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.gridResults, resources.GetString("gridResults.ToolTip2"))
        '
        'DataGridViewTextBoxColumn1
        '
        DataGridViewCellStyle7.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn1.DefaultCellStyle = DataGridViewCellStyle7
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle8.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle8
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'Column1
        '
        DataGridViewCellStyle9.BackColor = System.Drawing.SystemColors.Control
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle9
        Me.Column1.FillWeight = 30.0!
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'btnViewProfile
        '
        resources.ApplyResources(Me.btnViewProfile, "btnViewProfile")
        Me.btnViewProfile.Name = "btnViewProfile"
        Me.ToolTip1.SetToolTip(Me.btnViewProfile, resources.GetString("btnViewProfile.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnViewProfile, resources.GetString("btnViewProfile.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnViewProfile, resources.GetString("btnViewProfile.ToolTip2"))
        Me.btnViewProfile.UseVisualStyleBackColor = True
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_HeatExchanger
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxResults)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_HeatExchanger"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBoxResults.ResumeLayout(False)
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbColdFluidPDrop As System.Windows.Forms.ComboBox
    Public WithEvents tbColdFluidPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbHotFluidPDrop As System.Windows.Forms.ComboBox
    Public WithEvents tbHotFluidPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents cbHotFluidOutletT As System.Windows.Forms.ComboBox
    Public WithEvents tbHotFluidOutletT As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents cbColdFluidOutletT As System.Windows.Forms.ComboBox
    Public WithEvents tbColdFluidOutletT As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet2 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet2 As System.Windows.Forms.ComboBox
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents btnEditSTProps As System.Windows.Forms.Button
    Public WithEvents cbHeat As System.Windows.Forms.ComboBox
    Public WithEvents tbHeat As System.Windows.Forms.TextBox
    Public WithEvents cbArea As System.Windows.Forms.ComboBox
    Public WithEvents tbArea As System.Windows.Forms.TextBox
    Public WithEvents cbOverallHTC As System.Windows.Forms.ComboBox
    Public WithEvents tbOverallU As System.Windows.Forms.TextBox
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents cbFlowDir As System.Windows.Forms.ComboBox
    Public WithEvents chkIgnoreLMTD As System.Windows.Forms.CheckBox
    Public WithEvents GroupBoxResults As System.Windows.Forms.GroupBox
    Public WithEvents gridResults As System.Windows.Forms.DataGridView
    Public WithEvents cbMITA As System.Windows.Forms.ComboBox
    Public WithEvents tbMITA As System.Windows.Forms.TextBox
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents btnViewProfile As System.Windows.Forms.Button
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents cbHeatLoss As ComboBox
    Public WithEvents tbHeatLoss As TextBox
    Public WithEvents Label20 As Label
    Public WithEvents cbEfficiency As ComboBox
    Public WithEvents tbEfficiency As TextBox
    Public WithEvents Label21 As Label
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents tbOVF2 As TextBox
    Public WithEvents Label10 As Label
    Public WithEvents tbOVF1 As TextBox
    Public WithEvents Label9 As Label
    Public WithEvents chkForcePinchToOutlets As CheckBox
    Public WithEvents chkCalculateProfile As CheckBox
End Class
