<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_ShortcutColumn
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_ShortcutColumn))
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
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.rbPartialCond = New System.Windows.Forms.RadioButton()
        Me.rbTotalCond = New System.Windows.Forms.RadioButton()
        Me.cbRebPressureUnits = New System.Windows.Forms.ComboBox()
        Me.tbRebPressure = New System.Windows.Forms.TextBox()
        Me.cbCondPressureUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondPressure = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbHKey = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbRefluxRatio = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbHKmolfrac = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbLKmolfrac = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbLKey = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy2 = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy2 = New System.Windows.Forms.Button()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.cbEnergy2 = New System.Windows.Forms.ComboBox()
        Me.btnCreateAndConnectEnergy1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet2 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbEnergy1 = New System.Windows.Forms.ComboBox()
        Me.cbOutlet2 = New System.Windows.Forms.ComboBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectInlet1 = New System.Windows.Forms.Button()
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
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.gbGHG = New System.Windows.Forms.GroupBox()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
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
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.btnConfigurePP)
        Me.GroupBox3.Controls.Add(Me.cbPropPack)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip2"))
        '
        'btnConfigurePP
        '
        resources.ApplyResources(Me.btnConfigurePP, "btnConfigurePP")
        Me.btnConfigurePP.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigurePP.Name = "btnConfigurePP"
        Me.ToolTip1.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip2"))
        Me.btnConfigurePP.UseVisualStyleBackColor = True
        '
        'cbPropPack
        '
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Name = "cbPropPack"
        Me.ToolTip1.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip2"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTip1.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip2"))
        '
        'GroupBoxParameters
        '
        resources.ApplyResources(Me.GroupBoxParameters, "GroupBoxParameters")
        Me.GroupBoxParameters.Controls.Add(Me.rbPartialCond)
        Me.GroupBoxParameters.Controls.Add(Me.rbTotalCond)
        Me.GroupBoxParameters.Controls.Add(Me.cbRebPressureUnits)
        Me.GroupBoxParameters.Controls.Add(Me.tbRebPressure)
        Me.GroupBoxParameters.Controls.Add(Me.cbCondPressureUnits)
        Me.GroupBoxParameters.Controls.Add(Me.tbCondPressure)
        Me.GroupBoxParameters.Controls.Add(Me.Label16)
        Me.GroupBoxParameters.Controls.Add(Me.Label15)
        Me.GroupBoxParameters.Controls.Add(Me.Label14)
        Me.GroupBoxParameters.Controls.Add(Me.cbHKey)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.tbRefluxRatio)
        Me.GroupBoxParameters.Controls.Add(Me.Label4)
        Me.GroupBoxParameters.Controls.Add(Me.tbHKmolfrac)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.tbLKmolfrac)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.cbLKey)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'rbPartialCond
        '
        resources.ApplyResources(Me.rbPartialCond, "rbPartialCond")
        Me.rbPartialCond.Name = "rbPartialCond"
        Me.rbPartialCond.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbPartialCond, resources.GetString("rbPartialCond.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbPartialCond, resources.GetString("rbPartialCond.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbPartialCond, resources.GetString("rbPartialCond.ToolTip2"))
        Me.rbPartialCond.UseVisualStyleBackColor = True
        '
        'rbTotalCond
        '
        resources.ApplyResources(Me.rbTotalCond, "rbTotalCond")
        Me.rbTotalCond.Name = "rbTotalCond"
        Me.rbTotalCond.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbTotalCond, resources.GetString("rbTotalCond.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbTotalCond, resources.GetString("rbTotalCond.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbTotalCond, resources.GetString("rbTotalCond.ToolTip2"))
        Me.rbTotalCond.UseVisualStyleBackColor = True
        '
        'cbRebPressureUnits
        '
        resources.ApplyResources(Me.cbRebPressureUnits, "cbRebPressureUnits")
        Me.cbRebPressureUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebPressureUnits.FormattingEnabled = True
        Me.cbRebPressureUnits.Items.AddRange(New Object() {resources.GetString("cbRebPressureUnits.Items"), resources.GetString("cbRebPressureUnits.Items1"), resources.GetString("cbRebPressureUnits.Items2")})
        Me.cbRebPressureUnits.Name = "cbRebPressureUnits"
        Me.ToolTip1.SetToolTip(Me.cbRebPressureUnits, resources.GetString("cbRebPressureUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRebPressureUnits, resources.GetString("cbRebPressureUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRebPressureUnits, resources.GetString("cbRebPressureUnits.ToolTip2"))
        '
        'tbRebPressure
        '
        resources.ApplyResources(Me.tbRebPressure, "tbRebPressure")
        Me.tbRebPressure.Name = "tbRebPressure"
        Me.ToolTipValues.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip2"))
        '
        'cbCondPressureUnits
        '
        resources.ApplyResources(Me.cbCondPressureUnits, "cbCondPressureUnits")
        Me.cbCondPressureUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondPressureUnits.FormattingEnabled = True
        Me.cbCondPressureUnits.Items.AddRange(New Object() {resources.GetString("cbCondPressureUnits.Items"), resources.GetString("cbCondPressureUnits.Items1"), resources.GetString("cbCondPressureUnits.Items2")})
        Me.cbCondPressureUnits.Name = "cbCondPressureUnits"
        Me.ToolTip1.SetToolTip(Me.cbCondPressureUnits, resources.GetString("cbCondPressureUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondPressureUnits, resources.GetString("cbCondPressureUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondPressureUnits, resources.GetString("cbCondPressureUnits.ToolTip2"))
        '
        'tbCondPressure
        '
        resources.ApplyResources(Me.tbCondPressure, "tbCondPressure")
        Me.tbCondPressure.Name = "tbCondPressure"
        Me.ToolTipValues.SetToolTip(Me.tbCondPressure, resources.GetString("tbCondPressure.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondPressure, resources.GetString("tbCondPressure.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondPressure, resources.GetString("tbCondPressure.ToolTip2"))
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
        'cbHKey
        '
        resources.ApplyResources(Me.cbHKey, "cbHKey")
        Me.cbHKey.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHKey.FormattingEnabled = True
        Me.cbHKey.Items.AddRange(New Object() {resources.GetString("cbHKey.Items"), resources.GetString("cbHKey.Items1")})
        Me.cbHKey.Name = "cbHKey"
        Me.ToolTip1.SetToolTip(Me.cbHKey, resources.GetString("cbHKey.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbHKey, resources.GetString("cbHKey.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbHKey, resources.GetString("cbHKey.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'tbRefluxRatio
        '
        resources.ApplyResources(Me.tbRefluxRatio, "tbRefluxRatio")
        Me.tbRefluxRatio.Name = "tbRefluxRatio"
        Me.ToolTipValues.SetToolTip(Me.tbRefluxRatio, resources.GetString("tbRefluxRatio.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRefluxRatio, resources.GetString("tbRefluxRatio.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRefluxRatio, resources.GetString("tbRefluxRatio.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'tbHKmolfrac
        '
        resources.ApplyResources(Me.tbHKmolfrac, "tbHKmolfrac")
        Me.tbHKmolfrac.Name = "tbHKmolfrac"
        Me.ToolTipValues.SetToolTip(Me.tbHKmolfrac, resources.GetString("tbHKmolfrac.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbHKmolfrac, resources.GetString("tbHKmolfrac.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbHKmolfrac, resources.GetString("tbHKmolfrac.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'tbLKmolfrac
        '
        resources.ApplyResources(Me.tbLKmolfrac, "tbLKmolfrac")
        Me.tbLKmolfrac.Name = "tbLKmolfrac"
        Me.ToolTipValues.SetToolTip(Me.tbLKmolfrac, resources.GetString("tbLKmolfrac.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbLKmolfrac, resources.GetString("tbLKmolfrac.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbLKmolfrac, resources.GetString("tbLKmolfrac.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'cbLKey
        '
        resources.ApplyResources(Me.cbLKey, "cbLKey")
        Me.cbLKey.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbLKey.FormattingEnabled = True
        Me.cbLKey.Items.AddRange(New Object() {resources.GetString("cbLKey.Items"), resources.GetString("cbLKey.Items1"), resources.GetString("cbLKey.Items2"), resources.GetString("cbLKey.Items3"), resources.GetString("cbLKey.Items4"), resources.GetString("cbLKey.Items5"), resources.GetString("cbLKey.Items6"), resources.GetString("cbLKey.Items7")})
        Me.cbLKey.Name = "cbLKey"
        Me.ToolTip1.SetToolTip(Me.cbLKey, resources.GetString("cbLKey.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbLKey, resources.GetString("cbLKey.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbLKey, resources.GetString("cbLKey.ToolTip2"))
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
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy2)
        Me.GroupBoxConnections.Controls.Add(Me.Label20)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy2)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.Label1)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy1)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.Label6)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectInlet1)
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
        'btnCreateAndConnectEnergy2
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy2, "btnCreateAndConnectEnergy2")
        Me.btnCreateAndConnectEnergy2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy2.Name = "btnCreateAndConnectEnergy2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy2, resources.GetString("btnCreateAndConnectEnergy2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectEnergy2, resources.GetString("btnCreateAndConnectEnergy2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectEnergy2, resources.GetString("btnCreateAndConnectEnergy2.ToolTip2"))
        Me.btnCreateAndConnectEnergy2.UseVisualStyleBackColor = True
        '
        'btnDisconnectEnergy2
        '
        resources.ApplyResources(Me.btnDisconnectEnergy2, "btnDisconnectEnergy2")
        Me.btnDisconnectEnergy2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy2.Name = "btnDisconnectEnergy2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy2, resources.GetString("btnDisconnectEnergy2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectEnergy2, resources.GetString("btnDisconnectEnergy2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectEnergy2, resources.GetString("btnDisconnectEnergy2.ToolTip2"))
        Me.btnDisconnectEnergy2.UseVisualStyleBackColor = True
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTip1.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip2"))
        '
        'cbEnergy2
        '
        resources.ApplyResources(Me.cbEnergy2, "cbEnergy2")
        Me.cbEnergy2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy2.FormattingEnabled = True
        Me.cbEnergy2.Name = "cbEnergy2"
        Me.ToolTip1.SetToolTip(Me.cbEnergy2, resources.GetString("cbEnergy2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbEnergy2, resources.GetString("cbEnergy2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbEnergy2, resources.GetString("cbEnergy2.ToolTip2"))
        '
        'btnCreateAndConnectEnergy1
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy1, "btnCreateAndConnectEnergy1")
        Me.btnCreateAndConnectEnergy1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy1.Name = "btnCreateAndConnectEnergy1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy1, resources.GetString("btnCreateAndConnectEnergy1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectEnergy1, resources.GetString("btnCreateAndConnectEnergy1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectEnergy1, resources.GetString("btnCreateAndConnectEnergy1.ToolTip2"))
        Me.btnCreateAndConnectEnergy1.UseVisualStyleBackColor = True
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
        'btnDisconnectEnergy1
        '
        resources.ApplyResources(Me.btnDisconnectEnergy1, "btnDisconnectEnergy1")
        Me.btnDisconnectEnergy1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy1.Name = "btnDisconnectEnergy1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy1, resources.GetString("btnDisconnectEnergy1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectEnergy1, resources.GetString("btnDisconnectEnergy1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectEnergy1, resources.GetString("btnDisconnectEnergy1.ToolTip2"))
        Me.btnDisconnectEnergy1.UseVisualStyleBackColor = True
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
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'cbEnergy1
        '
        resources.ApplyResources(Me.cbEnergy1, "cbEnergy1")
        Me.cbEnergy1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy1.FormattingEnabled = True
        Me.cbEnergy1.Name = "cbEnergy1"
        Me.ToolTip1.SetToolTip(Me.cbEnergy1, resources.GetString("cbEnergy1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbEnergy1, resources.GetString("cbEnergy1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbEnergy1, resources.GetString("cbEnergy1.ToolTip2"))
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
        'btnDisconnectInlet1
        '
        resources.ApplyResources(Me.btnDisconnectInlet1, "btnDisconnectInlet1")
        Me.btnDisconnectInlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectInlet1.Name = "btnDisconnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectInlet1, resources.GetString("btnDisconnectInlet1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectInlet1, resources.GetString("btnDisconnectInlet1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectInlet1, resources.GetString("btnDisconnectInlet1.ToolTip2"))
        Me.btnDisconnectInlet1.UseVisualStyleBackColor = True
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'gbGHG
        '
        resources.ApplyResources(Me.gbGHG, "gbGHG")
        Me.gbGHG.Name = "gbGHG"
        Me.gbGHG.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip2"))
        '
        'EditingForm_ShortcutColumn
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.gbGHG)
        Me.Controls.Add(Me.GroupBoxResults)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_ShortcutColumn"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
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
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents cbLKey As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents tbLKmolfrac As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents tbHKmolfrac As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectInlet1 As System.Windows.Forms.Button
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents tbRefluxRatio As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectEnergy1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectEnergy1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbEnergy1 As System.Windows.Forms.ComboBox
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents cbRebPressureUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbRebPressure As System.Windows.Forms.TextBox
    Public WithEvents cbCondPressureUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbCondPressure As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents cbHKey As System.Windows.Forms.ComboBox
    Public WithEvents GroupBoxResults As System.Windows.Forms.GroupBox
    Public WithEvents gridResults As System.Windows.Forms.DataGridView
    Public WithEvents rbPartialCond As System.Windows.Forms.RadioButton
    Public WithEvents rbTotalCond As System.Windows.Forms.RadioButton
    Public WithEvents btnCreateAndConnectEnergy2 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectEnergy2 As System.Windows.Forms.Button
    Public WithEvents Label20 As System.Windows.Forms.Label
    Public WithEvents cbEnergy2 As System.Windows.Forms.ComboBox
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents gbGHG As GroupBox
End Class
