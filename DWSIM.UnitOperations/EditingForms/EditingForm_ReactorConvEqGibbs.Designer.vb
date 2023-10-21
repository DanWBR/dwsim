<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_ReactorConvEqGibbs

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_ReactorConvEqGibbs))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
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
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.TabControlParameters = New System.Windows.Forms.TabControl()
        Me.TabPageParams = New System.Windows.Forms.TabPage()
        Me.btnConfigExtSolver = New System.Windows.Forms.Button()
        Me.cbExternalSolver = New System.Windows.Forms.ComboBox()
        Me.LabelExternalSolver = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbPDrop = New System.Windows.Forms.ComboBox()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.tbPDrop = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbOutletTemperature = New System.Windows.Forms.TextBox()
        Me.cbReacSet = New System.Windows.Forms.ComboBox()
        Me.cbTemp = New System.Windows.Forms.ComboBox()
        Me.TabPageCompounds = New System.Windows.Forms.TabPage()
        Me.ListViewCompounds = New System.Windows.Forms.ListView()
        Me.TabPageElements = New System.Windows.Forms.TabPage()
        Me.TabPageGibbsParams = New System.Windows.Forms.TabPage()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbReactivePhaseBehavior = New System.Windows.Forms.ComboBox()
        Me.chkGibbsUseAlternSolver = New System.Windows.Forms.CheckBox()
        Me.chkGibbsUseIPOPT = New System.Windows.Forms.CheckBox()
        Me.chkGibbsUsePreviousSolution = New System.Windows.Forms.CheckBox()
        Me.tbIntLoopTol = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.tbIntLoopMaxIts = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.TabPageEqParams = New System.Windows.Forms.TabPage()
        Me.tbExtLoopTolEq = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.tbIntLoopTolEq = New System.Windows.Forms.TextBox()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.tbIntLoopMaxItsEq = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.tbExtLoopMaxItsEq = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.chkInitializeExtents = New System.Windows.Forms.CheckBox()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet2 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbOutlet2 = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectEnergy = New System.Windows.Forms.Button()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbEnergy = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBoxResults = New System.Windows.Forms.GroupBox()
        Me.tabstrip1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.gridReactions = New System.Windows.Forms.DataGridView()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.gridConversions = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.gbGHG = New System.Windows.Forms.GroupBox()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.TabControlParameters.SuspendLayout()
        Me.TabPageParams.SuspendLayout()
        Me.TabPageCompounds.SuspendLayout()
        Me.TabPageGibbsParams.SuspendLayout()
        Me.TabPageEqParams.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBoxResults.SuspendLayout()
        Me.tabstrip1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        CType(Me.gridReactions, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage3.SuspendLayout()
        CType(Me.gridConversions, System.ComponentModel.ISupportInitialize).BeginInit()
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
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip2"))
        '
        'rtbAnnotations
        '
        resources.ApplyResources(Me.rtbAnnotations, "rtbAnnotations")
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.22621}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.ToolTipValues.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip2"))
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
        Me.GroupBoxParameters.Controls.Add(Me.TabControlParameters)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'TabControlParameters
        '
        resources.ApplyResources(Me.TabControlParameters, "TabControlParameters")
        Me.TabControlParameters.Controls.Add(Me.TabPageParams)
        Me.TabControlParameters.Controls.Add(Me.TabPageCompounds)
        Me.TabControlParameters.Controls.Add(Me.TabPageElements)
        Me.TabControlParameters.Controls.Add(Me.TabPageGibbsParams)
        Me.TabControlParameters.Controls.Add(Me.TabPageEqParams)
        Me.TabControlParameters.Name = "TabControlParameters"
        Me.TabControlParameters.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.TabControlParameters, resources.GetString("TabControlParameters.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.TabControlParameters, resources.GetString("TabControlParameters.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.TabControlParameters, resources.GetString("TabControlParameters.ToolTip2"))
        '
        'TabPageParams
        '
        resources.ApplyResources(Me.TabPageParams, "TabPageParams")
        Me.TabPageParams.Controls.Add(Me.btnConfigExtSolver)
        Me.TabPageParams.Controls.Add(Me.cbExternalSolver)
        Me.TabPageParams.Controls.Add(Me.LabelExternalSolver)
        Me.TabPageParams.Controls.Add(Me.btnConfigurePP)
        Me.TabPageParams.Controls.Add(Me.Label4)
        Me.TabPageParams.Controls.Add(Me.cbPropPack)
        Me.TabPageParams.Controls.Add(Me.Label9)
        Me.TabPageParams.Controls.Add(Me.Label2)
        Me.TabPageParams.Controls.Add(Me.cbPDrop)
        Me.TabPageParams.Controls.Add(Me.cbCalcMode)
        Me.TabPageParams.Controls.Add(Me.tbPDrop)
        Me.TabPageParams.Controls.Add(Me.Label3)
        Me.TabPageParams.Controls.Add(Me.Label5)
        Me.TabPageParams.Controls.Add(Me.tbOutletTemperature)
        Me.TabPageParams.Controls.Add(Me.cbReacSet)
        Me.TabPageParams.Controls.Add(Me.cbTemp)
        Me.TabPageParams.Name = "TabPageParams"
        Me.ToolTip1.SetToolTip(Me.TabPageParams, resources.GetString("TabPageParams.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageParams, resources.GetString("TabPageParams.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageParams, resources.GetString("TabPageParams.ToolTip2"))
        Me.TabPageParams.UseVisualStyleBackColor = True
        '
        'btnConfigExtSolver
        '
        resources.ApplyResources(Me.btnConfigExtSolver, "btnConfigExtSolver")
        Me.btnConfigExtSolver.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigExtSolver.Name = "btnConfigExtSolver"
        Me.ToolTip1.SetToolTip(Me.btnConfigExtSolver, resources.GetString("btnConfigExtSolver.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnConfigExtSolver, resources.GetString("btnConfigExtSolver.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnConfigExtSolver, resources.GetString("btnConfigExtSolver.ToolTip2"))
        Me.btnConfigExtSolver.UseVisualStyleBackColor = True
        '
        'cbExternalSolver
        '
        resources.ApplyResources(Me.cbExternalSolver, "cbExternalSolver")
        Me.cbExternalSolver.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbExternalSolver.FormattingEnabled = True
        Me.cbExternalSolver.Name = "cbExternalSolver"
        Me.ToolTip1.SetToolTip(Me.cbExternalSolver, resources.GetString("cbExternalSolver.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbExternalSolver, resources.GetString("cbExternalSolver.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbExternalSolver, resources.GetString("cbExternalSolver.ToolTip2"))
        '
        'LabelExternalSolver
        '
        resources.ApplyResources(Me.LabelExternalSolver, "LabelExternalSolver")
        Me.LabelExternalSolver.Name = "LabelExternalSolver"
        Me.ToolTip1.SetToolTip(Me.LabelExternalSolver, resources.GetString("LabelExternalSolver.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.LabelExternalSolver, resources.GetString("LabelExternalSolver.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.LabelExternalSolver, resources.GetString("LabelExternalSolver.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbPDrop
        '
        resources.ApplyResources(Me.cbPDrop, "cbPDrop")
        Me.cbPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPDrop.FormattingEnabled = True
        Me.cbPDrop.Items.AddRange(New Object() {resources.GetString("cbPDrop.Items"), resources.GetString("cbPDrop.Items1"), resources.GetString("cbPDrop.Items2")})
        Me.cbPDrop.Name = "cbPDrop"
        Me.ToolTip1.SetToolTip(Me.cbPDrop, resources.GetString("cbPDrop.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPDrop, resources.GetString("cbPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPDrop, resources.GetString("cbPDrop.ToolTip2"))
        '
        'cbCalcMode
        '
        resources.ApplyResources(Me.cbCalcMode, "cbCalcMode")
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2")})
        Me.cbCalcMode.Name = "cbCalcMode"
        Me.ToolTip1.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCalcMode, resources.GetString("cbCalcMode.ToolTip2"))
        '
        'tbPDrop
        '
        resources.ApplyResources(Me.tbPDrop, "tbPDrop")
        Me.tbPDrop.Name = "tbPDrop"
        Me.ToolTipValues.SetToolTip(Me.tbPDrop, resources.GetString("tbPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPDrop, resources.GetString("tbPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPDrop, resources.GetString("tbPDrop.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'tbOutletTemperature
        '
        resources.ApplyResources(Me.tbOutletTemperature, "tbOutletTemperature")
        Me.tbOutletTemperature.Name = "tbOutletTemperature"
        Me.ToolTipValues.SetToolTip(Me.tbOutletTemperature, resources.GetString("tbOutletTemperature.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOutletTemperature, resources.GetString("tbOutletTemperature.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOutletTemperature, resources.GetString("tbOutletTemperature.ToolTip2"))
        '
        'cbReacSet
        '
        resources.ApplyResources(Me.cbReacSet, "cbReacSet")
        Me.cbReacSet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbReacSet.FormattingEnabled = True
        Me.cbReacSet.Items.AddRange(New Object() {resources.GetString("cbReacSet.Items"), resources.GetString("cbReacSet.Items1"), resources.GetString("cbReacSet.Items2")})
        Me.cbReacSet.Name = "cbReacSet"
        Me.ToolTip1.SetToolTip(Me.cbReacSet, resources.GetString("cbReacSet.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbReacSet, resources.GetString("cbReacSet.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbReacSet, resources.GetString("cbReacSet.ToolTip2"))
        '
        'cbTemp
        '
        resources.ApplyResources(Me.cbTemp, "cbTemp")
        Me.cbTemp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTemp.FormattingEnabled = True
        Me.cbTemp.Items.AddRange(New Object() {resources.GetString("cbTemp.Items"), resources.GetString("cbTemp.Items1"), resources.GetString("cbTemp.Items2")})
        Me.cbTemp.Name = "cbTemp"
        Me.ToolTip1.SetToolTip(Me.cbTemp, resources.GetString("cbTemp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTemp, resources.GetString("cbTemp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbTemp, resources.GetString("cbTemp.ToolTip2"))
        '
        'TabPageCompounds
        '
        resources.ApplyResources(Me.TabPageCompounds, "TabPageCompounds")
        Me.TabPageCompounds.Controls.Add(Me.ListViewCompounds)
        Me.TabPageCompounds.Name = "TabPageCompounds"
        Me.ToolTip1.SetToolTip(Me.TabPageCompounds, resources.GetString("TabPageCompounds.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageCompounds, resources.GetString("TabPageCompounds.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageCompounds, resources.GetString("TabPageCompounds.ToolTip2"))
        Me.TabPageCompounds.UseVisualStyleBackColor = True
        '
        'ListViewCompounds
        '
        resources.ApplyResources(Me.ListViewCompounds, "ListViewCompounds")
        Me.ListViewCompounds.CheckBoxes = True
        Me.ListViewCompounds.HideSelection = False
        Me.ListViewCompounds.Name = "ListViewCompounds"
        Me.ToolTip1.SetToolTip(Me.ListViewCompounds, resources.GetString("ListViewCompounds.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.ListViewCompounds, resources.GetString("ListViewCompounds.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.ListViewCompounds, resources.GetString("ListViewCompounds.ToolTip2"))
        Me.ListViewCompounds.UseCompatibleStateImageBehavior = False
        Me.ListViewCompounds.View = System.Windows.Forms.View.List
        '
        'TabPageElements
        '
        resources.ApplyResources(Me.TabPageElements, "TabPageElements")
        Me.TabPageElements.Name = "TabPageElements"
        Me.ToolTip1.SetToolTip(Me.TabPageElements, resources.GetString("TabPageElements.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageElements, resources.GetString("TabPageElements.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageElements, resources.GetString("TabPageElements.ToolTip2"))
        Me.TabPageElements.UseVisualStyleBackColor = True
        '
        'TabPageGibbsParams
        '
        resources.ApplyResources(Me.TabPageGibbsParams, "TabPageGibbsParams")
        Me.TabPageGibbsParams.Controls.Add(Me.Label6)
        Me.TabPageGibbsParams.Controls.Add(Me.cbReactivePhaseBehavior)
        Me.TabPageGibbsParams.Controls.Add(Me.chkGibbsUseAlternSolver)
        Me.TabPageGibbsParams.Controls.Add(Me.chkGibbsUseIPOPT)
        Me.TabPageGibbsParams.Controls.Add(Me.chkGibbsUsePreviousSolution)
        Me.TabPageGibbsParams.Controls.Add(Me.tbIntLoopTol)
        Me.TabPageGibbsParams.Controls.Add(Me.Label18)
        Me.TabPageGibbsParams.Controls.Add(Me.tbIntLoopMaxIts)
        Me.TabPageGibbsParams.Controls.Add(Me.Label16)
        Me.TabPageGibbsParams.Name = "TabPageGibbsParams"
        Me.ToolTip1.SetToolTip(Me.TabPageGibbsParams, resources.GetString("TabPageGibbsParams.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageGibbsParams, resources.GetString("TabPageGibbsParams.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageGibbsParams, resources.GetString("TabPageGibbsParams.ToolTip2"))
        Me.TabPageGibbsParams.UseVisualStyleBackColor = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'cbReactivePhaseBehavior
        '
        resources.ApplyResources(Me.cbReactivePhaseBehavior, "cbReactivePhaseBehavior")
        Me.cbReactivePhaseBehavior.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbReactivePhaseBehavior.FormattingEnabled = True
        Me.cbReactivePhaseBehavior.Items.AddRange(New Object() {resources.GetString("cbReactivePhaseBehavior.Items"), resources.GetString("cbReactivePhaseBehavior.Items1"), resources.GetString("cbReactivePhaseBehavior.Items2"), resources.GetString("cbReactivePhaseBehavior.Items3")})
        Me.cbReactivePhaseBehavior.Name = "cbReactivePhaseBehavior"
        Me.ToolTip1.SetToolTip(Me.cbReactivePhaseBehavior, resources.GetString("cbReactivePhaseBehavior.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbReactivePhaseBehavior, resources.GetString("cbReactivePhaseBehavior.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbReactivePhaseBehavior, resources.GetString("cbReactivePhaseBehavior.ToolTip2"))
        '
        'chkGibbsUseAlternSolver
        '
        resources.ApplyResources(Me.chkGibbsUseAlternSolver, "chkGibbsUseAlternSolver")
        Me.chkGibbsUseAlternSolver.Name = "chkGibbsUseAlternSolver"
        Me.ToolTip1.SetToolTip(Me.chkGibbsUseAlternSolver, resources.GetString("chkGibbsUseAlternSolver.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkGibbsUseAlternSolver, resources.GetString("chkGibbsUseAlternSolver.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkGibbsUseAlternSolver, resources.GetString("chkGibbsUseAlternSolver.ToolTip2"))
        Me.chkGibbsUseAlternSolver.UseVisualStyleBackColor = True
        '
        'chkGibbsUseIPOPT
        '
        resources.ApplyResources(Me.chkGibbsUseIPOPT, "chkGibbsUseIPOPT")
        Me.chkGibbsUseIPOPT.Name = "chkGibbsUseIPOPT"
        Me.ToolTip1.SetToolTip(Me.chkGibbsUseIPOPT, resources.GetString("chkGibbsUseIPOPT.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkGibbsUseIPOPT, resources.GetString("chkGibbsUseIPOPT.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkGibbsUseIPOPT, resources.GetString("chkGibbsUseIPOPT.ToolTip2"))
        Me.chkGibbsUseIPOPT.UseVisualStyleBackColor = True
        '
        'chkGibbsUsePreviousSolution
        '
        resources.ApplyResources(Me.chkGibbsUsePreviousSolution, "chkGibbsUsePreviousSolution")
        Me.chkGibbsUsePreviousSolution.Name = "chkGibbsUsePreviousSolution"
        Me.ToolTip1.SetToolTip(Me.chkGibbsUsePreviousSolution, resources.GetString("chkGibbsUsePreviousSolution.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkGibbsUsePreviousSolution, resources.GetString("chkGibbsUsePreviousSolution.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkGibbsUsePreviousSolution, resources.GetString("chkGibbsUsePreviousSolution.ToolTip2"))
        Me.chkGibbsUsePreviousSolution.UseVisualStyleBackColor = True
        '
        'tbIntLoopTol
        '
        resources.ApplyResources(Me.tbIntLoopTol, "tbIntLoopTol")
        Me.tbIntLoopTol.Name = "tbIntLoopTol"
        Me.ToolTipValues.SetToolTip(Me.tbIntLoopTol, resources.GetString("tbIntLoopTol.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbIntLoopTol, resources.GetString("tbIntLoopTol.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbIntLoopTol, resources.GetString("tbIntLoopTol.ToolTip2"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip2"))
        '
        'tbIntLoopMaxIts
        '
        resources.ApplyResources(Me.tbIntLoopMaxIts, "tbIntLoopMaxIts")
        Me.tbIntLoopMaxIts.Name = "tbIntLoopMaxIts"
        Me.ToolTipValues.SetToolTip(Me.tbIntLoopMaxIts, resources.GetString("tbIntLoopMaxIts.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbIntLoopMaxIts, resources.GetString("tbIntLoopMaxIts.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbIntLoopMaxIts, resources.GetString("tbIntLoopMaxIts.ToolTip2"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip2"))
        '
        'TabPageEqParams
        '
        resources.ApplyResources(Me.TabPageEqParams, "TabPageEqParams")
        Me.TabPageEqParams.Controls.Add(Me.tbExtLoopTolEq)
        Me.TabPageEqParams.Controls.Add(Me.Label21)
        Me.TabPageEqParams.Controls.Add(Me.tbIntLoopTolEq)
        Me.TabPageEqParams.Controls.Add(Me.Label22)
        Me.TabPageEqParams.Controls.Add(Me.tbIntLoopMaxItsEq)
        Me.TabPageEqParams.Controls.Add(Me.Label23)
        Me.TabPageEqParams.Controls.Add(Me.tbExtLoopMaxItsEq)
        Me.TabPageEqParams.Controls.Add(Me.Label24)
        Me.TabPageEqParams.Controls.Add(Me.chkInitializeExtents)
        Me.TabPageEqParams.Name = "TabPageEqParams"
        Me.ToolTip1.SetToolTip(Me.TabPageEqParams, resources.GetString("TabPageEqParams.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageEqParams, resources.GetString("TabPageEqParams.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageEqParams, resources.GetString("TabPageEqParams.ToolTip2"))
        Me.TabPageEqParams.UseVisualStyleBackColor = True
        '
        'tbExtLoopTolEq
        '
        resources.ApplyResources(Me.tbExtLoopTolEq, "tbExtLoopTolEq")
        Me.tbExtLoopTolEq.Name = "tbExtLoopTolEq"
        Me.ToolTipValues.SetToolTip(Me.tbExtLoopTolEq, resources.GetString("tbExtLoopTolEq.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbExtLoopTolEq, resources.GetString("tbExtLoopTolEq.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbExtLoopTolEq, resources.GetString("tbExtLoopTolEq.ToolTip2"))
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTip1.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip2"))
        '
        'tbIntLoopTolEq
        '
        resources.ApplyResources(Me.tbIntLoopTolEq, "tbIntLoopTolEq")
        Me.tbIntLoopTolEq.Name = "tbIntLoopTolEq"
        Me.ToolTipValues.SetToolTip(Me.tbIntLoopTolEq, resources.GetString("tbIntLoopTolEq.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbIntLoopTolEq, resources.GetString("tbIntLoopTolEq.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbIntLoopTolEq, resources.GetString("tbIntLoopTolEq.ToolTip2"))
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        Me.ToolTip1.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip2"))
        '
        'tbIntLoopMaxItsEq
        '
        resources.ApplyResources(Me.tbIntLoopMaxItsEq, "tbIntLoopMaxItsEq")
        Me.tbIntLoopMaxItsEq.Name = "tbIntLoopMaxItsEq"
        Me.ToolTipValues.SetToolTip(Me.tbIntLoopMaxItsEq, resources.GetString("tbIntLoopMaxItsEq.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbIntLoopMaxItsEq, resources.GetString("tbIntLoopMaxItsEq.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbIntLoopMaxItsEq, resources.GetString("tbIntLoopMaxItsEq.ToolTip2"))
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        Me.ToolTip1.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip2"))
        '
        'tbExtLoopMaxItsEq
        '
        resources.ApplyResources(Me.tbExtLoopMaxItsEq, "tbExtLoopMaxItsEq")
        Me.tbExtLoopMaxItsEq.Name = "tbExtLoopMaxItsEq"
        Me.ToolTipValues.SetToolTip(Me.tbExtLoopMaxItsEq, resources.GetString("tbExtLoopMaxItsEq.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbExtLoopMaxItsEq, resources.GetString("tbExtLoopMaxItsEq.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbExtLoopMaxItsEq, resources.GetString("tbExtLoopMaxItsEq.ToolTip2"))
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        Me.ToolTip1.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip2"))
        '
        'chkInitializeExtents
        '
        resources.ApplyResources(Me.chkInitializeExtents, "chkInitializeExtents")
        Me.chkInitializeExtents.Name = "chkInitializeExtents"
        Me.ToolTip1.SetToolTip(Me.chkInitializeExtents, resources.GetString("chkInitializeExtents.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkInitializeExtents, resources.GetString("chkInitializeExtents.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkInitializeExtents, resources.GetString("chkInitializeExtents.ToolTip2"))
        Me.chkInitializeExtents.UseVisualStyleBackColor = True
        '
        'GroupBoxConnections
        '
        resources.ApplyResources(Me.GroupBoxConnections, "GroupBoxConnections")
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.Label1)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.Label14)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy)
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
        'btnCreateAndConnectEnergy
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy, "btnCreateAndConnectEnergy")
        Me.btnCreateAndConnectEnergy.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy.Name = "btnCreateAndConnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip2"))
        Me.btnCreateAndConnectEnergy.UseVisualStyleBackColor = True
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
        'btnDisconnectEnergy
        '
        resources.ApplyResources(Me.btnDisconnectEnergy, "btnDisconnectEnergy")
        Me.btnDisconnectEnergy.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip2"))
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip2"))
        '
        'cbEnergy
        '
        resources.ApplyResources(Me.cbEnergy, "cbEnergy")
        Me.cbEnergy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy.FormattingEnabled = True
        Me.cbEnergy.Name = "cbEnergy"
        Me.ToolTip1.SetToolTip(Me.cbEnergy, resources.GetString("cbEnergy.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbEnergy, resources.GetString("cbEnergy.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbEnergy, resources.GetString("cbEnergy.ToolTip2"))
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
        Me.GroupBoxResults.Controls.Add(Me.tabstrip1)
        Me.GroupBoxResults.Name = "GroupBoxResults"
        Me.GroupBoxResults.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxResults, resources.GetString("GroupBoxResults.ToolTip2"))
        '
        'tabstrip1
        '
        resources.ApplyResources(Me.tabstrip1, "tabstrip1")
        Me.tabstrip1.Controls.Add(Me.TabPage1)
        Me.tabstrip1.Controls.Add(Me.TabPage2)
        Me.tabstrip1.Controls.Add(Me.TabPage3)
        Me.tabstrip1.Name = "tabstrip1"
        Me.tabstrip1.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip2"))
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.gridResults)
        Me.TabPage1.Name = "TabPage1"
        Me.ToolTip1.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip2"))
        Me.TabPage1.UseVisualStyleBackColor = True
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
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.gridReactions)
        Me.TabPage2.Name = "TabPage2"
        Me.ToolTip1.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip2"))
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'gridReactions
        '
        resources.ApplyResources(Me.gridReactions, "gridReactions")
        Me.gridReactions.AllowUserToAddRows = False
        Me.gridReactions.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridReactions.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridReactions.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column2, Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4, Me.DataGridViewTextBoxColumn5})
        Me.gridReactions.Name = "gridReactions"
        Me.gridReactions.ReadOnly = True
        Me.gridReactions.RowHeadersVisible = False
        Me.ToolTipChangeTag.SetToolTip(Me.gridReactions, resources.GetString("gridReactions.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.gridReactions, resources.GetString("gridReactions.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.gridReactions, resources.GetString("gridReactions.ToolTip2"))
        '
        'Column2
        '
        DataGridViewCellStyle4.BackColor = System.Drawing.SystemColors.Control
        Me.Column2.DefaultCellStyle = DataGridViewCellStyle4
        Me.Column2.FillWeight = 40.0!
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        '
        'DataGridViewTextBoxColumn3
        '
        DataGridViewCellStyle5.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn3.DefaultCellStyle = DataGridViewCellStyle5
        Me.DataGridViewTextBoxColumn3.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        Me.DataGridViewTextBoxColumn3.ReadOnly = True
        '
        'DataGridViewTextBoxColumn4
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn4.DefaultCellStyle = DataGridViewCellStyle6
        Me.DataGridViewTextBoxColumn4.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn4, "DataGridViewTextBoxColumn4")
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        Me.DataGridViewTextBoxColumn4.ReadOnly = True
        '
        'DataGridViewTextBoxColumn5
        '
        DataGridViewCellStyle7.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn5.DefaultCellStyle = DataGridViewCellStyle7
        Me.DataGridViewTextBoxColumn5.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        Me.DataGridViewTextBoxColumn5.ReadOnly = True
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Controls.Add(Me.gridConversions)
        Me.TabPage3.Name = "TabPage3"
        Me.ToolTip1.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip2"))
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'gridConversions
        '
        resources.ApplyResources(Me.gridConversions, "gridConversions")
        Me.gridConversions.AllowUserToAddRows = False
        Me.gridConversions.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridConversions.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridConversions.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn6, Me.DataGridViewTextBoxColumn7})
        Me.gridConversions.Name = "gridConversions"
        Me.gridConversions.ReadOnly = True
        Me.gridConversions.RowHeadersVisible = False
        Me.ToolTipChangeTag.SetToolTip(Me.gridConversions, resources.GetString("gridConversions.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.gridConversions, resources.GetString("gridConversions.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.gridConversions, resources.GetString("gridConversions.ToolTip2"))
        '
        'DataGridViewTextBoxColumn6
        '
        DataGridViewCellStyle8.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn6.DefaultCellStyle = DataGridViewCellStyle8
        Me.DataGridViewTextBoxColumn6.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        Me.DataGridViewTextBoxColumn6.ReadOnly = True
        '
        'DataGridViewTextBoxColumn7
        '
        DataGridViewCellStyle9.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn7.DefaultCellStyle = DataGridViewCellStyle9
        Me.DataGridViewTextBoxColumn7.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn7, "DataGridViewTextBoxColumn7")
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        Me.DataGridViewTextBoxColumn7.ReadOnly = True
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
        'EditingForm_ReactorConvEqGibbs
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.gbGHG)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBoxResults)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_ReactorConvEqGibbs"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.TabControlParameters.ResumeLayout(False)
        Me.TabPageParams.ResumeLayout(False)
        Me.TabPageParams.PerformLayout()
        Me.TabPageCompounds.ResumeLayout(False)
        Me.TabPageGibbsParams.ResumeLayout(False)
        Me.TabPageGibbsParams.PerformLayout()
        Me.TabPageEqParams.ResumeLayout(False)
        Me.TabPageEqParams.PerformLayout()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBoxResults.ResumeLayout(False)
        Me.tabstrip1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        CType(Me.gridReactions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage3.ResumeLayout(False)
        CType(Me.gridConversions, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectEnergy As System.Windows.Forms.Button
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents cbEnergy As System.Windows.Forms.ComboBox
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBoxResults As System.Windows.Forms.GroupBox
    Public WithEvents tabstrip1 As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents TabPage2 As System.Windows.Forms.TabPage
    Public WithEvents TabPage3 As System.Windows.Forms.TabPage
    Public WithEvents gridResults As System.Windows.Forms.DataGridView
    Public WithEvents gridReactions As System.Windows.Forms.DataGridView
    Public WithEvents gridConversions As System.Windows.Forms.DataGridView
    Public WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbReacSet As System.Windows.Forms.ComboBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents cbTemp As System.Windows.Forms.ComboBox
    Public WithEvents tbOutletTemperature As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbPDrop As System.Windows.Forms.ComboBox
    Public WithEvents tbPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectEnergy As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents TabControlParameters As System.Windows.Forms.TabControl
    Public WithEvents TabPageParams As System.Windows.Forms.TabPage
    Public WithEvents TabPageCompounds As System.Windows.Forms.TabPage
    Public WithEvents TabPageElements As System.Windows.Forms.TabPage
    Public WithEvents ListViewCompounds As System.Windows.Forms.ListView
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents TabPageGibbsParams As System.Windows.Forms.TabPage
    Public WithEvents tbIntLoopTol As System.Windows.Forms.TextBox
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents tbIntLoopMaxIts As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents TabPageEqParams As TabPage
    Public WithEvents tbExtLoopMaxItsEq As TextBox
    Public WithEvents Label21 As Label
    Public WithEvents tbIntLoopMaxItsEq As TextBox
    Public WithEvents Label22 As Label
    Public WithEvents tbExtLoopTolEq As TextBox
    Public WithEvents Label23 As Label
    Public WithEvents tbIntLoopTolEq As TextBox
    Public WithEvents Label24 As Label
    Public WithEvents chkInitializeExtents As CheckBox
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents chkGibbsUsePreviousSolution As CheckBox
    Public WithEvents cbExternalSolver As ComboBox
    Public WithEvents LabelExternalSolver As Label
    Public WithEvents btnConfigExtSolver As Button
    Public WithEvents chkGibbsUseIPOPT As CheckBox
    Public WithEvents chkGibbsUseAlternSolver As CheckBox
    Public WithEvents Label6 As Label
    Public WithEvents cbReactivePhaseBehavior As ComboBox
    Public WithEvents gbGHG As GroupBox
End Class
