﻿<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Column

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Column))
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
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabGeneral = New System.Windows.Forms.TabPage()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.tbConvTol = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbAbsorberMode = New System.Windows.Forms.ComboBox()
        Me.tbMaxIt = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbNStages = New System.Windows.Forms.TextBox()
        Me.TabCondenser = New System.Windows.Forms.TabPage()
        Me.chkNoCondenser = New System.Windows.Forms.CheckBox()
        Me.PanelCondenser = New System.Windows.Forms.Panel()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.cbCondComp = New System.Windows.Forms.ComboBox()
        Me.cbCondType = New System.Windows.Forms.ComboBox()
        Me.tbCondPDrop = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.cbCondPDropUnits = New System.Windows.Forms.ComboBox()
        Me.cbCondVapFlowUnits = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbCondVapFlow = New System.Windows.Forms.TextBox()
        Me.cbCondSpec = New System.Windows.Forms.ComboBox()
        Me.cbCondSpecUnits = New System.Windows.Forms.ComboBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.tbCondSpec = New System.Windows.Forms.TextBox()
        Me.cbCondPressureUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondPressure = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.TabReboiler = New System.Windows.Forms.TabPage()
        Me.PanelReboiler = New System.Windows.Forms.Panel()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbRebSpec = New System.Windows.Forms.ComboBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.tbRebSpecValue = New System.Windows.Forms.TextBox()
        Me.cbRebComp = New System.Windows.Forms.ComboBox()
        Me.cbRebSpecUnits = New System.Windows.Forms.ComboBox()
        Me.chkNoReboiler = New System.Windows.Forms.CheckBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.cbRebPressure = New System.Windows.Forms.ComboBox()
        Me.tbRebPressure = New System.Windows.Forms.TextBox()
        Me.TabControl2 = New System.Windows.Forms.TabControl()
        Me.TabConnections = New System.Windows.Forms.TabPage()
        Me.TabStages = New System.Windows.Forms.TabPage()
        Me.TabInitialEstimates = New System.Windows.Forms.TabPage()
        Me.InitialEstimatesPanel = New System.Windows.Forms.Panel()
        Me.chkUseIE_C = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_VF = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_LF = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_T = New System.Windows.Forms.CheckBox()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.tabstrip1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.btnResults = New System.Windows.Forms.Button()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabControl3 = New System.Windows.Forms.TabControl()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabGeneral.SuspendLayout()
        Me.TabCondenser.SuspendLayout()
        Me.PanelCondenser.SuspendLayout()
        Me.TabReboiler.SuspendLayout()
        Me.PanelReboiler.SuspendLayout()
        Me.TabControl2.SuspendLayout()
        Me.TabInitialEstimates.SuspendLayout()
        Me.tabstrip1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabControl3.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
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
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabGeneral)
        Me.TabControl1.Controls.Add(Me.TabCondenser)
        Me.TabControl1.Controls.Add(Me.TabReboiler)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.TabControl1, resources.GetString("TabControl1.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.TabControl1, resources.GetString("TabControl1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.TabControl1, resources.GetString("TabControl1.ToolTip2"))
        '
        'TabGeneral
        '
        resources.ApplyResources(Me.TabGeneral, "TabGeneral")
        Me.TabGeneral.Controls.Add(Me.Label16)
        Me.TabGeneral.Controls.Add(Me.tbConvTol)
        Me.TabGeneral.Controls.Add(Me.btnConfigurePP)
        Me.TabGeneral.Controls.Add(Me.Label14)
        Me.TabGeneral.Controls.Add(Me.cbAbsorberMode)
        Me.TabGeneral.Controls.Add(Me.tbMaxIt)
        Me.TabGeneral.Controls.Add(Me.cbPropPack)
        Me.TabGeneral.Controls.Add(Me.Label9)
        Me.TabGeneral.Controls.Add(Me.Label6)
        Me.TabGeneral.Controls.Add(Me.Label1)
        Me.TabGeneral.Controls.Add(Me.tbNStages)
        Me.TabGeneral.Name = "TabGeneral"
        Me.ToolTip1.SetToolTip(Me.TabGeneral, resources.GetString("TabGeneral.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabGeneral, resources.GetString("TabGeneral.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabGeneral, resources.GetString("TabGeneral.ToolTip2"))
        Me.TabGeneral.UseVisualStyleBackColor = True
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip2"))
        '
        'tbConvTol
        '
        resources.ApplyResources(Me.tbConvTol, "tbConvTol")
        Me.tbConvTol.Name = "tbConvTol"
        Me.ToolTipValues.SetToolTip(Me.tbConvTol, resources.GetString("tbConvTol.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbConvTol, resources.GetString("tbConvTol.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbConvTol, resources.GetString("tbConvTol.ToolTip2"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip2"))
        '
        'cbAbsorberMode
        '
        resources.ApplyResources(Me.cbAbsorberMode, "cbAbsorberMode")
        Me.cbAbsorberMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbAbsorberMode.FormattingEnabled = True
        Me.cbAbsorberMode.Items.AddRange(New Object() {resources.GetString("cbAbsorberMode.Items"), resources.GetString("cbAbsorberMode.Items1")})
        Me.cbAbsorberMode.Name = "cbAbsorberMode"
        Me.ToolTip1.SetToolTip(Me.cbAbsorberMode, resources.GetString("cbAbsorberMode.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbAbsorberMode, resources.GetString("cbAbsorberMode.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbAbsorberMode, resources.GetString("cbAbsorberMode.ToolTip2"))
        '
        'tbMaxIt
        '
        resources.ApplyResources(Me.tbMaxIt, "tbMaxIt")
        Me.tbMaxIt.Name = "tbMaxIt"
        Me.ToolTipValues.SetToolTip(Me.tbMaxIt, resources.GetString("tbMaxIt.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbMaxIt, resources.GetString("tbMaxIt.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbMaxIt, resources.GetString("tbMaxIt.ToolTip2"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'tbNStages
        '
        resources.ApplyResources(Me.tbNStages, "tbNStages")
        Me.tbNStages.Name = "tbNStages"
        Me.ToolTipValues.SetToolTip(Me.tbNStages, resources.GetString("tbNStages.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbNStages, resources.GetString("tbNStages.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbNStages, resources.GetString("tbNStages.ToolTip2"))
        '
        'TabCondenser
        '
        resources.ApplyResources(Me.TabCondenser, "TabCondenser")
        Me.TabCondenser.Controls.Add(Me.chkNoCondenser)
        Me.TabCondenser.Controls.Add(Me.PanelCondenser)
        Me.TabCondenser.Controls.Add(Me.cbCondPressureUnits)
        Me.TabCondenser.Controls.Add(Me.tbCondPressure)
        Me.TabCondenser.Controls.Add(Me.Label4)
        Me.TabCondenser.Name = "TabCondenser"
        Me.ToolTip1.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip2"))
        Me.TabCondenser.UseVisualStyleBackColor = True
        '
        'chkNoCondenser
        '
        resources.ApplyResources(Me.chkNoCondenser, "chkNoCondenser")
        Me.chkNoCondenser.Name = "chkNoCondenser"
        Me.ToolTip1.SetToolTip(Me.chkNoCondenser, resources.GetString("chkNoCondenser.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkNoCondenser, resources.GetString("chkNoCondenser.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkNoCondenser, resources.GetString("chkNoCondenser.ToolTip2"))
        Me.chkNoCondenser.UseVisualStyleBackColor = True
        '
        'PanelCondenser
        '
        resources.ApplyResources(Me.PanelCondenser, "PanelCondenser")
        Me.PanelCondenser.Controls.Add(Me.Label2)
        Me.PanelCondenser.Controls.Add(Me.Label19)
        Me.PanelCondenser.Controls.Add(Me.cbCondComp)
        Me.PanelCondenser.Controls.Add(Me.cbCondType)
        Me.PanelCondenser.Controls.Add(Me.tbCondPDrop)
        Me.PanelCondenser.Controls.Add(Me.Label8)
        Me.PanelCondenser.Controls.Add(Me.cbCondPDropUnits)
        Me.PanelCondenser.Controls.Add(Me.cbCondVapFlowUnits)
        Me.PanelCondenser.Controls.Add(Me.Label5)
        Me.PanelCondenser.Controls.Add(Me.tbCondVapFlow)
        Me.PanelCondenser.Controls.Add(Me.cbCondSpec)
        Me.PanelCondenser.Controls.Add(Me.cbCondSpecUnits)
        Me.PanelCondenser.Controls.Add(Me.Label7)
        Me.PanelCondenser.Controls.Add(Me.tbCondSpec)
        Me.PanelCondenser.Name = "PanelCondenser"
        Me.ToolTipValues.SetToolTip(Me.PanelCondenser, resources.GetString("PanelCondenser.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.PanelCondenser, resources.GetString("PanelCondenser.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.PanelCondenser, resources.GetString("PanelCondenser.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTip1.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip2"))
        '
        'cbCondComp
        '
        resources.ApplyResources(Me.cbCondComp, "cbCondComp")
        Me.cbCondComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondComp.DropDownWidth = 250
        Me.cbCondComp.FormattingEnabled = True
        Me.cbCondComp.Items.AddRange(New Object() {resources.GetString("cbCondComp.Items"), resources.GetString("cbCondComp.Items1"), resources.GetString("cbCondComp.Items2"), resources.GetString("cbCondComp.Items3"), resources.GetString("cbCondComp.Items4"), resources.GetString("cbCondComp.Items5"), resources.GetString("cbCondComp.Items6"), resources.GetString("cbCondComp.Items7"), resources.GetString("cbCondComp.Items8")})
        Me.cbCondComp.Name = "cbCondComp"
        Me.ToolTip1.SetToolTip(Me.cbCondComp, resources.GetString("cbCondComp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondComp, resources.GetString("cbCondComp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondComp, resources.GetString("cbCondComp.ToolTip2"))
        '
        'cbCondType
        '
        resources.ApplyResources(Me.cbCondType, "cbCondType")
        Me.cbCondType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondType.FormattingEnabled = True
        Me.cbCondType.Items.AddRange(New Object() {resources.GetString("cbCondType.Items"), resources.GetString("cbCondType.Items1"), resources.GetString("cbCondType.Items2")})
        Me.cbCondType.Name = "cbCondType"
        Me.ToolTip1.SetToolTip(Me.cbCondType, resources.GetString("cbCondType.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondType, resources.GetString("cbCondType.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondType, resources.GetString("cbCondType.ToolTip2"))
        '
        'tbCondPDrop
        '
        resources.ApplyResources(Me.tbCondPDrop, "tbCondPDrop")
        Me.tbCondPDrop.Name = "tbCondPDrop"
        Me.ToolTipValues.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip2"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip2"))
        '
        'cbCondPDropUnits
        '
        resources.ApplyResources(Me.cbCondPDropUnits, "cbCondPDropUnits")
        Me.cbCondPDropUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondPDropUnits.FormattingEnabled = True
        Me.cbCondPDropUnits.Items.AddRange(New Object() {resources.GetString("cbCondPDropUnits.Items"), resources.GetString("cbCondPDropUnits.Items1"), resources.GetString("cbCondPDropUnits.Items2")})
        Me.cbCondPDropUnits.Name = "cbCondPDropUnits"
        Me.ToolTip1.SetToolTip(Me.cbCondPDropUnits, resources.GetString("cbCondPDropUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondPDropUnits, resources.GetString("cbCondPDropUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondPDropUnits, resources.GetString("cbCondPDropUnits.ToolTip2"))
        '
        'cbCondVapFlowUnits
        '
        resources.ApplyResources(Me.cbCondVapFlowUnits, "cbCondVapFlowUnits")
        Me.cbCondVapFlowUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondVapFlowUnits.FormattingEnabled = True
        Me.cbCondVapFlowUnits.Items.AddRange(New Object() {resources.GetString("cbCondVapFlowUnits.Items"), resources.GetString("cbCondVapFlowUnits.Items1"), resources.GetString("cbCondVapFlowUnits.Items2")})
        Me.cbCondVapFlowUnits.Name = "cbCondVapFlowUnits"
        Me.ToolTip1.SetToolTip(Me.cbCondVapFlowUnits, resources.GetString("cbCondVapFlowUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondVapFlowUnits, resources.GetString("cbCondVapFlowUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondVapFlowUnits, resources.GetString("cbCondVapFlowUnits.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'tbCondVapFlow
        '
        resources.ApplyResources(Me.tbCondVapFlow, "tbCondVapFlow")
        Me.tbCondVapFlow.Name = "tbCondVapFlow"
        Me.ToolTipValues.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip2"))
        '
        'cbCondSpec
        '
        resources.ApplyResources(Me.cbCondSpec, "cbCondSpec")
        Me.cbCondSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondSpec.DropDownWidth = 250
        Me.cbCondSpec.FormattingEnabled = True
        Me.cbCondSpec.Items.AddRange(New Object() {resources.GetString("cbCondSpec.Items"), resources.GetString("cbCondSpec.Items1"), resources.GetString("cbCondSpec.Items2"), resources.GetString("cbCondSpec.Items3"), resources.GetString("cbCondSpec.Items4"), resources.GetString("cbCondSpec.Items5"), resources.GetString("cbCondSpec.Items6"), resources.GetString("cbCondSpec.Items7"), resources.GetString("cbCondSpec.Items8")})
        Me.cbCondSpec.Name = "cbCondSpec"
        Me.ToolTip1.SetToolTip(Me.cbCondSpec, resources.GetString("cbCondSpec.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondSpec, resources.GetString("cbCondSpec.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondSpec, resources.GetString("cbCondSpec.ToolTip2"))
        '
        'cbCondSpecUnits
        '
        resources.ApplyResources(Me.cbCondSpecUnits, "cbCondSpecUnits")
        Me.cbCondSpecUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondSpecUnits.FormattingEnabled = True
        Me.cbCondSpecUnits.Items.AddRange(New Object() {resources.GetString("cbCondSpecUnits.Items"), resources.GetString("cbCondSpecUnits.Items1"), resources.GetString("cbCondSpecUnits.Items2")})
        Me.cbCondSpecUnits.Name = "cbCondSpecUnits"
        Me.ToolTip1.SetToolTip(Me.cbCondSpecUnits, resources.GetString("cbCondSpecUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCondSpecUnits, resources.GetString("cbCondSpecUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbCondSpecUnits, resources.GetString("cbCondSpecUnits.ToolTip2"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip2"))
        '
        'tbCondSpec
        '
        resources.ApplyResources(Me.tbCondSpec, "tbCondSpec")
        Me.tbCondSpec.Name = "tbCondSpec"
        Me.ToolTipValues.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip2"))
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
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'TabReboiler
        '
        resources.ApplyResources(Me.TabReboiler, "TabReboiler")
        Me.TabReboiler.Controls.Add(Me.PanelReboiler)
        Me.TabReboiler.Controls.Add(Me.chkNoReboiler)
        Me.TabReboiler.Controls.Add(Me.Label17)
        Me.TabReboiler.Controls.Add(Me.cbRebPressure)
        Me.TabReboiler.Controls.Add(Me.tbRebPressure)
        Me.TabReboiler.Name = "TabReboiler"
        Me.ToolTip1.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip2"))
        Me.TabReboiler.UseVisualStyleBackColor = True
        '
        'PanelReboiler
        '
        resources.ApplyResources(Me.PanelReboiler, "PanelReboiler")
        Me.PanelReboiler.Controls.Add(Me.Label15)
        Me.PanelReboiler.Controls.Add(Me.cbRebSpec)
        Me.PanelReboiler.Controls.Add(Me.Label24)
        Me.PanelReboiler.Controls.Add(Me.tbRebSpecValue)
        Me.PanelReboiler.Controls.Add(Me.cbRebComp)
        Me.PanelReboiler.Controls.Add(Me.cbRebSpecUnits)
        Me.PanelReboiler.Name = "PanelReboiler"
        Me.ToolTipValues.SetToolTip(Me.PanelReboiler, resources.GetString("PanelReboiler.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.PanelReboiler, resources.GetString("PanelReboiler.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.PanelReboiler, resources.GetString("PanelReboiler.ToolTip2"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTip1.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip2"))
        '
        'cbRebSpec
        '
        resources.ApplyResources(Me.cbRebSpec, "cbRebSpec")
        Me.cbRebSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebSpec.DropDownWidth = 250
        Me.cbRebSpec.FormattingEnabled = True
        Me.cbRebSpec.Items.AddRange(New Object() {resources.GetString("cbRebSpec.Items"), resources.GetString("cbRebSpec.Items1"), resources.GetString("cbRebSpec.Items2"), resources.GetString("cbRebSpec.Items3"), resources.GetString("cbRebSpec.Items4"), resources.GetString("cbRebSpec.Items5"), resources.GetString("cbRebSpec.Items6"), resources.GetString("cbRebSpec.Items7"), resources.GetString("cbRebSpec.Items8")})
        Me.cbRebSpec.Name = "cbRebSpec"
        Me.ToolTip1.SetToolTip(Me.cbRebSpec, resources.GetString("cbRebSpec.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRebSpec, resources.GetString("cbRebSpec.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRebSpec, resources.GetString("cbRebSpec.ToolTip2"))
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        Me.ToolTip1.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip2"))
        '
        'tbRebSpecValue
        '
        resources.ApplyResources(Me.tbRebSpecValue, "tbRebSpecValue")
        Me.tbRebSpecValue.Name = "tbRebSpecValue"
        Me.ToolTipValues.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip2"))
        '
        'cbRebComp
        '
        resources.ApplyResources(Me.cbRebComp, "cbRebComp")
        Me.cbRebComp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebComp.DropDownWidth = 250
        Me.cbRebComp.FormattingEnabled = True
        Me.cbRebComp.Items.AddRange(New Object() {resources.GetString("cbRebComp.Items"), resources.GetString("cbRebComp.Items1"), resources.GetString("cbRebComp.Items2"), resources.GetString("cbRebComp.Items3"), resources.GetString("cbRebComp.Items4"), resources.GetString("cbRebComp.Items5"), resources.GetString("cbRebComp.Items6"), resources.GetString("cbRebComp.Items7"), resources.GetString("cbRebComp.Items8")})
        Me.cbRebComp.Name = "cbRebComp"
        Me.ToolTip1.SetToolTip(Me.cbRebComp, resources.GetString("cbRebComp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRebComp, resources.GetString("cbRebComp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRebComp, resources.GetString("cbRebComp.ToolTip2"))
        '
        'cbRebSpecUnits
        '
        resources.ApplyResources(Me.cbRebSpecUnits, "cbRebSpecUnits")
        Me.cbRebSpecUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebSpecUnits.FormattingEnabled = True
        Me.cbRebSpecUnits.Items.AddRange(New Object() {resources.GetString("cbRebSpecUnits.Items"), resources.GetString("cbRebSpecUnits.Items1"), resources.GetString("cbRebSpecUnits.Items2")})
        Me.cbRebSpecUnits.Name = "cbRebSpecUnits"
        Me.ToolTip1.SetToolTip(Me.cbRebSpecUnits, resources.GetString("cbRebSpecUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRebSpecUnits, resources.GetString("cbRebSpecUnits.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRebSpecUnits, resources.GetString("cbRebSpecUnits.ToolTip2"))
        '
        'chkNoReboiler
        '
        resources.ApplyResources(Me.chkNoReboiler, "chkNoReboiler")
        Me.chkNoReboiler.Name = "chkNoReboiler"
        Me.ToolTip1.SetToolTip(Me.chkNoReboiler, resources.GetString("chkNoReboiler.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkNoReboiler, resources.GetString("chkNoReboiler.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkNoReboiler, resources.GetString("chkNoReboiler.ToolTip2"))
        Me.chkNoReboiler.UseVisualStyleBackColor = True
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTip1.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip2"))
        '
        'cbRebPressure
        '
        resources.ApplyResources(Me.cbRebPressure, "cbRebPressure")
        Me.cbRebPressure.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebPressure.FormattingEnabled = True
        Me.cbRebPressure.Items.AddRange(New Object() {resources.GetString("cbRebPressure.Items"), resources.GetString("cbRebPressure.Items1"), resources.GetString("cbRebPressure.Items2")})
        Me.cbRebPressure.Name = "cbRebPressure"
        Me.ToolTip1.SetToolTip(Me.cbRebPressure, resources.GetString("cbRebPressure.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRebPressure, resources.GetString("cbRebPressure.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRebPressure, resources.GetString("cbRebPressure.ToolTip2"))
        '
        'tbRebPressure
        '
        resources.ApplyResources(Me.tbRebPressure, "tbRebPressure")
        Me.tbRebPressure.Name = "tbRebPressure"
        Me.ToolTipValues.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRebPressure, resources.GetString("tbRebPressure.ToolTip2"))
        '
        'TabControl2
        '
        resources.ApplyResources(Me.TabControl2, "TabControl2")
        Me.TabControl2.Controls.Add(Me.TabConnections)
        Me.TabControl2.Controls.Add(Me.TabStages)
        Me.TabControl2.Controls.Add(Me.TabInitialEstimates)
        Me.TabControl2.Name = "TabControl2"
        Me.TabControl2.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.TabControl2, resources.GetString("TabControl2.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.TabControl2, resources.GetString("TabControl2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.TabControl2, resources.GetString("TabControl2.ToolTip2"))
        '
        'TabConnections
        '
        resources.ApplyResources(Me.TabConnections, "TabConnections")
        Me.TabConnections.Name = "TabConnections"
        Me.ToolTip1.SetToolTip(Me.TabConnections, resources.GetString("TabConnections.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabConnections, resources.GetString("TabConnections.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabConnections, resources.GetString("TabConnections.ToolTip2"))
        Me.TabConnections.UseVisualStyleBackColor = True
        '
        'TabStages
        '
        resources.ApplyResources(Me.TabStages, "TabStages")
        Me.TabStages.Name = "TabStages"
        Me.ToolTip1.SetToolTip(Me.TabStages, resources.GetString("TabStages.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabStages, resources.GetString("TabStages.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabStages, resources.GetString("TabStages.ToolTip2"))
        Me.TabStages.UseVisualStyleBackColor = True
        '
        'TabInitialEstimates
        '
        resources.ApplyResources(Me.TabInitialEstimates, "TabInitialEstimates")
        Me.TabInitialEstimates.Controls.Add(Me.InitialEstimatesPanel)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_C)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_VF)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_LF)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_T)
        Me.TabInitialEstimates.Name = "TabInitialEstimates"
        Me.ToolTip1.SetToolTip(Me.TabInitialEstimates, resources.GetString("TabInitialEstimates.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabInitialEstimates, resources.GetString("TabInitialEstimates.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabInitialEstimates, resources.GetString("TabInitialEstimates.ToolTip2"))
        Me.TabInitialEstimates.UseVisualStyleBackColor = True
        '
        'InitialEstimatesPanel
        '
        resources.ApplyResources(Me.InitialEstimatesPanel, "InitialEstimatesPanel")
        Me.InitialEstimatesPanel.Name = "InitialEstimatesPanel"
        Me.ToolTipValues.SetToolTip(Me.InitialEstimatesPanel, resources.GetString("InitialEstimatesPanel.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.InitialEstimatesPanel, resources.GetString("InitialEstimatesPanel.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.InitialEstimatesPanel, resources.GetString("InitialEstimatesPanel.ToolTip2"))
        '
        'chkUseIE_C
        '
        resources.ApplyResources(Me.chkUseIE_C, "chkUseIE_C")
        Me.chkUseIE_C.Name = "chkUseIE_C"
        Me.ToolTip1.SetToolTip(Me.chkUseIE_C, resources.GetString("chkUseIE_C.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkUseIE_C, resources.GetString("chkUseIE_C.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkUseIE_C, resources.GetString("chkUseIE_C.ToolTip2"))
        Me.chkUseIE_C.UseVisualStyleBackColor = True
        '
        'chkUseIE_VF
        '
        resources.ApplyResources(Me.chkUseIE_VF, "chkUseIE_VF")
        Me.chkUseIE_VF.Name = "chkUseIE_VF"
        Me.ToolTip1.SetToolTip(Me.chkUseIE_VF, resources.GetString("chkUseIE_VF.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkUseIE_VF, resources.GetString("chkUseIE_VF.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkUseIE_VF, resources.GetString("chkUseIE_VF.ToolTip2"))
        Me.chkUseIE_VF.UseVisualStyleBackColor = True
        '
        'chkUseIE_LF
        '
        resources.ApplyResources(Me.chkUseIE_LF, "chkUseIE_LF")
        Me.chkUseIE_LF.Name = "chkUseIE_LF"
        Me.ToolTip1.SetToolTip(Me.chkUseIE_LF, resources.GetString("chkUseIE_LF.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkUseIE_LF, resources.GetString("chkUseIE_LF.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkUseIE_LF, resources.GetString("chkUseIE_LF.ToolTip2"))
        Me.chkUseIE_LF.UseVisualStyleBackColor = True
        '
        'chkUseIE_T
        '
        resources.ApplyResources(Me.chkUseIE_T, "chkUseIE_T")
        Me.chkUseIE_T.Name = "chkUseIE_T"
        Me.ToolTip1.SetToolTip(Me.chkUseIE_T, resources.GetString("chkUseIE_T.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkUseIE_T, resources.GetString("chkUseIE_T.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkUseIE_T, resources.GetString("chkUseIE_T.ToolTip2"))
        Me.chkUseIE_T.UseVisualStyleBackColor = True
        '
        'tabstrip1
        '
        resources.ApplyResources(Me.tabstrip1, "tabstrip1")
        Me.tabstrip1.Controls.Add(Me.TabPage1)
        Me.tabstrip1.Name = "tabstrip1"
        Me.tabstrip1.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.tabstrip1, resources.GetString("tabstrip1.ToolTip2"))
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.btnResults)
        Me.TabPage1.Controls.Add(Me.gridResults)
        Me.TabPage1.Name = "TabPage1"
        Me.ToolTip1.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip2"))
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'btnResults
        '
        resources.ApplyResources(Me.btnResults, "btnResults")
        Me.btnResults.Name = "btnResults"
        Me.ToolTip1.SetToolTip(Me.btnResults, resources.GetString("btnResults.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnResults, resources.GetString("btnResults.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnResults, resources.GetString("btnResults.ToolTip2"))
        Me.btnResults.UseVisualStyleBackColor = True
        '
        'gridResults
        '
        resources.ApplyResources(Me.gridResults, "gridResults")
        Me.gridResults.AllowUserToAddRows = False
        Me.gridResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridResults.BorderStyle = System.Windows.Forms.BorderStyle.None
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
        'TabControl3
        '
        resources.ApplyResources(Me.TabControl3, "TabControl3")
        Me.TabControl3.Controls.Add(Me.TabPage2)
        Me.TabControl3.Controls.Add(Me.TabPage3)
        Me.TabControl3.Controls.Add(Me.TabPage4)
        Me.TabControl3.Name = "TabControl3"
        Me.TabControl3.SelectedIndex = 0
        Me.ToolTipChangeTag.SetToolTip(Me.TabControl3, resources.GetString("TabControl3.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.TabControl3, resources.GetString("TabControl3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.TabControl3, resources.GetString("TabControl3.ToolTip2"))
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.TabControl1)
        Me.TabPage2.Name = "TabPage2"
        Me.ToolTip1.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip2"))
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Controls.Add(Me.TabControl2)
        Me.TabPage3.Name = "TabPage3"
        Me.ToolTip1.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip2"))
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'TabPage4
        '
        resources.ApplyResources(Me.TabPage4, "TabPage4")
        Me.TabPage4.Controls.Add(Me.tabstrip1)
        Me.TabPage4.Name = "TabPage4"
        Me.ToolTip1.SetToolTip(Me.TabPage4, resources.GetString("TabPage4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage4, resources.GetString("TabPage4.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage4, resources.GetString("TabPage4.ToolTip2"))
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_Column
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TabControl3)
        Me.Controls.Add(Me.GroupBox5)
        Me.Name = "EditingForm_Column"
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.TabControl1.ResumeLayout(False)
        Me.TabGeneral.ResumeLayout(False)
        Me.TabGeneral.PerformLayout()
        Me.TabCondenser.ResumeLayout(False)
        Me.TabCondenser.PerformLayout()
        Me.PanelCondenser.ResumeLayout(False)
        Me.PanelCondenser.PerformLayout()
        Me.TabReboiler.ResumeLayout(False)
        Me.TabReboiler.PerformLayout()
        Me.PanelReboiler.ResumeLayout(False)
        Me.PanelReboiler.PerformLayout()
        Me.TabControl2.ResumeLayout(False)
        Me.TabInitialEstimates.ResumeLayout(False)
        Me.TabInitialEstimates.PerformLayout()
        Me.tabstrip1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        CType(Me.gridResults, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabControl3.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents tabstrip1 As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents gridResults As System.Windows.Forms.DataGridView
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents tbNStages As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents TabControl1 As System.Windows.Forms.TabControl
    Public WithEvents TabGeneral As System.Windows.Forms.TabPage
    Public WithEvents TabCondenser As System.Windows.Forms.TabPage
    Public WithEvents TabReboiler As System.Windows.Forms.TabPage
    Public WithEvents cbAbsorberMode As System.Windows.Forms.ComboBox
    Public WithEvents TabControl2 As System.Windows.Forms.TabControl
    Public WithEvents TabConnections As System.Windows.Forms.TabPage
    Public WithEvents TabStages As System.Windows.Forms.TabPage
    Public WithEvents TabInitialEstimates As System.Windows.Forms.TabPage
    Public WithEvents cbCondType As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbCondVapFlowUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbCondVapFlow As System.Windows.Forms.TextBox
    Public WithEvents cbCondSpecUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbCondSpec As System.Windows.Forms.TextBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbCondSpec As System.Windows.Forms.ComboBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents cbCondPDropUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbCondPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents cbCondPressureUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbCondPressure As System.Windows.Forms.TextBox
    Public WithEvents cbRebSpecUnits As System.Windows.Forms.ComboBox
    Public WithEvents tbRebSpecValue As System.Windows.Forms.TextBox
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents cbRebSpec As System.Windows.Forms.ComboBox
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents cbRebPressure As System.Windows.Forms.ComboBox
    Public WithEvents tbRebPressure As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents tbConvTol As System.Windows.Forms.TextBox
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents tbMaxIt As System.Windows.Forms.TextBox
    Public WithEvents InitialEstimatesPanel As System.Windows.Forms.Panel
    Public WithEvents chkUseIE_C As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_VF As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_LF As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_T As System.Windows.Forms.CheckBox
    Public WithEvents btnResults As System.Windows.Forms.Button
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents cbCondComp As System.Windows.Forms.ComboBox
    Public WithEvents Label24 As System.Windows.Forms.Label
    Public WithEvents cbRebComp As System.Windows.Forms.ComboBox
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents TabControl3 As TabControl
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents TabPage4 As TabPage
    Friend WithEvents chkNoCondenser As CheckBox
    Friend WithEvents PanelCondenser As Panel
    Friend WithEvents PanelReboiler As Panel
    Friend WithEvents chkNoReboiler As CheckBox
End Class
