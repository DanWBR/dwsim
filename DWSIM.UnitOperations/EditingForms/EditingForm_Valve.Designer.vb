<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Valve
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Valve))
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.btnUtils = New System.Windows.Forms.Button()
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
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.gbTable = New System.Windows.Forms.GroupBox()
        Me.grid1 = New unvell.ReoGrid.ReoGridControl()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbCharParam = New System.Windows.Forms.TextBox()
        Me.tbKvOpRel = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.tbOp = New System.Windows.Forms.TextBox()
        Me.cbOpeningKvRelType = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.rbCv = New System.Windows.Forms.RadioButton()
        Me.rbKv = New System.Windows.Forms.RadioButton()
        Me.btnCalcKv = New System.Windows.Forms.Button()
        Me.chkEnableKvOpRel = New System.Windows.Forms.CheckBox()
        Me.tbKv = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbPress = New System.Windows.Forms.ComboBox()
        Me.tbOutletPressure = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbPressureDropU = New System.Windows.Forms.ComboBox()
        Me.tbPressureDrop = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.UtilitiesCtxMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.AddUtilityTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.sizingtsmi = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.gbTable.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
        Me.UtilitiesCtxMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.btnUtils)
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
        'btnUtils
        '
        resources.ApplyResources(Me.btnUtils, "btnUtils")
        Me.btnUtils.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_sparkle
        Me.btnUtils.Name = "btnUtils"
        Me.ToolTip1.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip2"))
        Me.btnUtils.UseVisualStyleBackColor = True
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
        Me.GroupBoxParameters.Controls.Add(Me.Panel1)
        Me.GroupBoxParameters.Controls.Add(Me.Label6)
        Me.GroupBoxParameters.Controls.Add(Me.rbCv)
        Me.GroupBoxParameters.Controls.Add(Me.rbKv)
        Me.GroupBoxParameters.Controls.Add(Me.btnCalcKv)
        Me.GroupBoxParameters.Controls.Add(Me.chkEnableKvOpRel)
        Me.GroupBoxParameters.Controls.Add(Me.tbKv)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.cbPress)
        Me.GroupBoxParameters.Controls.Add(Me.tbOutletPressure)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.cbPressureDropU)
        Me.GroupBoxParameters.Controls.Add(Me.tbPressureDrop)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.cbCalcMode)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.Label5)
        Me.Panel1.Controls.Add(Me.gbTable)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.tbCharParam)
        Me.Panel1.Controls.Add(Me.tbKvOpRel)
        Me.Panel1.Controls.Add(Me.Label14)
        Me.Panel1.Controls.Add(Me.tbOp)
        Me.Panel1.Controls.Add(Me.cbOpeningKvRelType)
        Me.Panel1.Controls.Add(Me.Label10)
        Me.Panel1.Name = "Panel1"
        Me.ToolTipValues.SetToolTip(Me.Panel1, resources.GetString("Panel1.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.Panel1, resources.GetString("Panel1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.Panel1, resources.GetString("Panel1.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'gbTable
        '
        resources.ApplyResources(Me.gbTable, "gbTable")
        Me.gbTable.Controls.Add(Me.grid1)
        Me.gbTable.Name = "gbTable"
        Me.gbTable.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.gbTable, resources.GetString("gbTable.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.gbTable, resources.GetString("gbTable.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.gbTable, resources.GetString("gbTable.ToolTip2"))
        '
        'grid1
        '
        resources.ApplyResources(Me.grid1, "grid1")
        Me.grid1.BackColor = System.Drawing.Color.White
        Me.grid1.ColumnHeaderContextMenuStrip = Nothing
        Me.grid1.LeadHeaderContextMenuStrip = Nothing
        Me.grid1.Name = "grid1"
        Me.grid1.RowHeaderContextMenuStrip = Nothing
        Me.grid1.Script = Nothing
        Me.grid1.SheetTabContextMenuStrip = Nothing
        Me.grid1.SheetTabNewButtonVisible = False
        Me.grid1.SheetTabVisible = False
        Me.grid1.SheetTabWidth = 60
        Me.grid1.ShowScrollEndSpacing = True
        Me.ToolTip1.SetToolTip(Me.grid1, resources.GetString("grid1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.grid1, resources.GetString("grid1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.grid1, resources.GetString("grid1.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'tbCharParam
        '
        resources.ApplyResources(Me.tbCharParam, "tbCharParam")
        Me.tbCharParam.Name = "tbCharParam"
        Me.ToolTipValues.SetToolTip(Me.tbCharParam, resources.GetString("tbCharParam.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCharParam, resources.GetString("tbCharParam.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCharParam, resources.GetString("tbCharParam.ToolTip2"))
        '
        'tbKvOpRel
        '
        resources.ApplyResources(Me.tbKvOpRel, "tbKvOpRel")
        Me.tbKvOpRel.Name = "tbKvOpRel"
        Me.ToolTipValues.SetToolTip(Me.tbKvOpRel, resources.GetString("tbKvOpRel.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbKvOpRel, resources.GetString("tbKvOpRel.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbKvOpRel, resources.GetString("tbKvOpRel.ToolTip2"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip2"))
        '
        'tbOp
        '
        resources.ApplyResources(Me.tbOp, "tbOp")
        Me.tbOp.Name = "tbOp"
        Me.ToolTipValues.SetToolTip(Me.tbOp, resources.GetString("tbOp.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOp, resources.GetString("tbOp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOp, resources.GetString("tbOp.ToolTip2"))
        '
        'cbOpeningKvRelType
        '
        resources.ApplyResources(Me.cbOpeningKvRelType, "cbOpeningKvRelType")
        Me.cbOpeningKvRelType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOpeningKvRelType.FormattingEnabled = True
        Me.cbOpeningKvRelType.Items.AddRange(New Object() {resources.GetString("cbOpeningKvRelType.Items"), resources.GetString("cbOpeningKvRelType.Items1"), resources.GetString("cbOpeningKvRelType.Items2"), resources.GetString("cbOpeningKvRelType.Items3"), resources.GetString("cbOpeningKvRelType.Items4")})
        Me.cbOpeningKvRelType.Name = "cbOpeningKvRelType"
        Me.ToolTip1.SetToolTip(Me.cbOpeningKvRelType, resources.GetString("cbOpeningKvRelType.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOpeningKvRelType, resources.GetString("cbOpeningKvRelType.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOpeningKvRelType, resources.GetString("cbOpeningKvRelType.ToolTip2"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip2"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'rbCv
        '
        resources.ApplyResources(Me.rbCv, "rbCv")
        Me.rbCv.Name = "rbCv"
        Me.rbCv.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbCv, resources.GetString("rbCv.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbCv, resources.GetString("rbCv.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbCv, resources.GetString("rbCv.ToolTip2"))
        Me.rbCv.UseVisualStyleBackColor = True
        '
        'rbKv
        '
        resources.ApplyResources(Me.rbKv, "rbKv")
        Me.rbKv.Name = "rbKv"
        Me.rbKv.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbKv, resources.GetString("rbKv.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbKv, resources.GetString("rbKv.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbKv, resources.GetString("rbKv.ToolTip2"))
        Me.rbKv.UseVisualStyleBackColor = True
        '
        'btnCalcKv
        '
        resources.ApplyResources(Me.btnCalcKv, "btnCalcKv")
        Me.btnCalcKv.Name = "btnCalcKv"
        Me.ToolTip1.SetToolTip(Me.btnCalcKv, resources.GetString("btnCalcKv.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCalcKv, resources.GetString("btnCalcKv.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCalcKv, resources.GetString("btnCalcKv.ToolTip2"))
        Me.btnCalcKv.UseVisualStyleBackColor = True
        '
        'chkEnableKvOpRel
        '
        resources.ApplyResources(Me.chkEnableKvOpRel, "chkEnableKvOpRel")
        Me.chkEnableKvOpRel.Name = "chkEnableKvOpRel"
        Me.ToolTip1.SetToolTip(Me.chkEnableKvOpRel, resources.GetString("chkEnableKvOpRel.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkEnableKvOpRel, resources.GetString("chkEnableKvOpRel.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkEnableKvOpRel, resources.GetString("chkEnableKvOpRel.ToolTip2"))
        Me.chkEnableKvOpRel.UseVisualStyleBackColor = True
        '
        'tbKv
        '
        resources.ApplyResources(Me.tbKv, "tbKv")
        Me.tbKv.Name = "tbKv"
        Me.ToolTipValues.SetToolTip(Me.tbKv, resources.GetString("tbKv.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbKv, resources.GetString("tbKv.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbKv, resources.GetString("tbKv.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'cbPress
        '
        resources.ApplyResources(Me.cbPress, "cbPress")
        Me.cbPress.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPress.FormattingEnabled = True
        Me.cbPress.Items.AddRange(New Object() {resources.GetString("cbPress.Items"), resources.GetString("cbPress.Items1"), resources.GetString("cbPress.Items2")})
        Me.cbPress.Name = "cbPress"
        Me.ToolTip1.SetToolTip(Me.cbPress, resources.GetString("cbPress.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPress, resources.GetString("cbPress.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPress, resources.GetString("cbPress.ToolTip2"))
        '
        'tbOutletPressure
        '
        resources.ApplyResources(Me.tbOutletPressure, "tbOutletPressure")
        Me.tbOutletPressure.Name = "tbOutletPressure"
        Me.ToolTipValues.SetToolTip(Me.tbOutletPressure, resources.GetString("tbOutletPressure.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOutletPressure, resources.GetString("tbOutletPressure.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOutletPressure, resources.GetString("tbOutletPressure.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbPressureDropU
        '
        resources.ApplyResources(Me.cbPressureDropU, "cbPressureDropU")
        Me.cbPressureDropU.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPressureDropU.FormattingEnabled = True
        Me.cbPressureDropU.Items.AddRange(New Object() {resources.GetString("cbPressureDropU.Items"), resources.GetString("cbPressureDropU.Items1"), resources.GetString("cbPressureDropU.Items2")})
        Me.cbPressureDropU.Name = "cbPressureDropU"
        Me.ToolTip1.SetToolTip(Me.cbPressureDropU, resources.GetString("cbPressureDropU.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPressureDropU, resources.GetString("cbPressureDropU.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPressureDropU, resources.GetString("cbPressureDropU.ToolTip2"))
        '
        'tbPressureDrop
        '
        resources.ApplyResources(Me.tbPressureDrop, "tbPressureDrop")
        Me.tbPressureDrop.Name = "tbPressureDrop"
        Me.ToolTipValues.SetToolTip(Me.tbPressureDrop, resources.GetString("tbPressureDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPressureDrop, resources.GetString("tbPressureDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPressureDrop, resources.GetString("tbPressureDrop.ToolTip2"))
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
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2"), resources.GetString("cbCalcMode.Items3"), resources.GetString("cbCalcMode.Items4"), resources.GetString("cbCalcMode.Items5")})
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
        'UtilitiesCtxMenu
        '
        resources.ApplyResources(Me.UtilitiesCtxMenu, "UtilitiesCtxMenu")
        Me.UtilitiesCtxMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddUtilityTSMI})
        Me.UtilitiesCtxMenu.Name = "ContextMenuStrip1"
        Me.ToolTip1.SetToolTip(Me.UtilitiesCtxMenu, resources.GetString("UtilitiesCtxMenu.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.UtilitiesCtxMenu, resources.GetString("UtilitiesCtxMenu.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.UtilitiesCtxMenu, resources.GetString("UtilitiesCtxMenu.ToolTip2"))
        '
        'AddUtilityTSMI
        '
        resources.ApplyResources(Me.AddUtilityTSMI, "AddUtilityTSMI")
        Me.AddUtilityTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.sizingtsmi})
        Me.AddUtilityTSMI.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        Me.AddUtilityTSMI.Name = "AddUtilityTSMI"
        '
        'sizingtsmi
        '
        resources.ApplyResources(Me.sizingtsmi, "sizingtsmi")
        Me.sizingtsmi.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.sizingtsmi.Name = "sizingtsmi"
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_Valve
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_Valve"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.gbTable.ResumeLayout(False)
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.UtilitiesCtxMenu.ResumeLayout(False)
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
    Public WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbPressureDropU As System.Windows.Forms.ComboBox
    Public WithEvents tbPressureDrop As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents cbPress As System.Windows.Forms.ComboBox
    Public WithEvents tbOutletPressure As System.Windows.Forms.TextBox
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
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents btnUtils As System.Windows.Forms.Button
    Public WithEvents UtilitiesCtxMenu As System.Windows.Forms.ContextMenuStrip
    Public WithEvents AddUtilityTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents sizingtsmi As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents tbKv As TextBox
    Public WithEvents Label1 As Label
    Public WithEvents tbKvOpRel As TextBox
    Public WithEvents Label4 As Label
    Public WithEvents chkEnableKvOpRel As CheckBox
    Public WithEvents tbOp As TextBox
    Public WithEvents Label5 As Label
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents btnCalcKv As Button
    Public WithEvents Label6 As Label
    Friend WithEvents rbCv As RadioButton
    Friend WithEvents rbKv As RadioButton
    Public WithEvents cbOpeningKvRelType As ComboBox
    Public WithEvents Label10 As Label
    Friend WithEvents gbTable As GroupBox
    Public WithEvents tbCharParam As TextBox
    Public WithEvents Label14 As Label
    Friend WithEvents grid1 As unvell.ReoGrid.ReoGridControl
    Friend WithEvents Panel1 As Panel
End Class
