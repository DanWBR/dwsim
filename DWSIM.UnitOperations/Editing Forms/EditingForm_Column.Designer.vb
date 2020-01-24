<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
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
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.btnConfigureFlashAlg = New System.Windows.Forms.Button()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabGeneral = New System.Windows.Forms.TabPage()
        Me.lblTemperatureUnit = New System.Windows.Forms.Label()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.tbMaxTChange = New System.Windows.Forms.TextBox()
        Me.cbSolverScheme = New System.Windows.Forms.ComboBox()
        Me.Label27 = New System.Windows.Forms.Label()
        Me.cbSolvingMethod = New System.Windows.Forms.ComboBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.tbConvTol = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbAbsorberMode = New System.Windows.Forms.ComboBox()
        Me.tbMaxIt = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbNStages = New System.Windows.Forms.TextBox()
        Me.TabCondenser = New System.Windows.Forms.TabPage()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.cbCondComp = New System.Windows.Forms.ComboBox()
        Me.cbCondType = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.cbCondVapFlowUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondVapFlow = New System.Windows.Forms.TextBox()
        Me.cbCondSpecUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondSpec = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbCondSpec = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.cbCondPDropUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondPDrop = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbCondPressureUnits = New System.Windows.Forms.ComboBox()
        Me.tbCondPressure = New System.Windows.Forms.TextBox()
        Me.TabReboiler = New System.Windows.Forms.TabPage()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.cbRebComp = New System.Windows.Forms.ComboBox()
        Me.cbRebSpecUnits = New System.Windows.Forms.ComboBox()
        Me.tbRebSpecValue = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbRebSpec = New System.Windows.Forms.ComboBox()
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
        Me.TabSolverBP = New System.Windows.Forms.TabPage()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbBPStopAtIter = New System.Windows.Forms.TextBox()
        Me.TabSolverNS = New System.Windows.Forms.TabPage()
        Me.tb_NS_NumDeriv = New System.Windows.Forms.TextBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.cbMinMethodNS = New System.Windows.Forms.ComboBox()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.tb_NS_UpperBound = New System.Windows.Forms.TextBox()
        Me.tb_NS_LowerBound = New System.Windows.Forms.TextBox()
        Me.cbNSPreconditioning = New System.Windows.Forms.CheckBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.TabSolverIO = New System.Windows.Forms.TabPage()
        Me.tb_IO_NumDeriv = New System.Windows.Forms.TextBox()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.cbMinMethodIO = New System.Windows.Forms.ComboBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.tb_IO_UpperBound = New System.Windows.Forms.TextBox()
        Me.tb_IO_LowerBound = New System.Windows.Forms.TextBox()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.chkIOAverageKb = New System.Windows.Forms.CheckBox()
        Me.chkIOAdjustSb = New System.Windows.Forms.CheckBox()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.tabstrip1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.btnResults = New System.Windows.Forms.Button()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.TabControl3 = New System.Windows.Forms.TabControl()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.GroupBox5.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabGeneral.SuspendLayout()
        Me.TabCondenser.SuspendLayout()
        Me.TabReboiler.SuspendLayout()
        Me.TabControl2.SuspendLayout()
        Me.TabInitialEstimates.SuspendLayout()
        Me.TabSolverBP.SuspendLayout()
        Me.TabSolverNS.SuspendLayout()
        Me.TabSolverIO.SuspendLayout()
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
        'btnConfigureFlashAlg
        '
        resources.ApplyResources(Me.btnConfigureFlashAlg, "btnConfigureFlashAlg")
        Me.btnConfigureFlashAlg.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigureFlashAlg.Name = "btnConfigureFlashAlg"
        Me.ToolTip1.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip2"))
        Me.btnConfigureFlashAlg.UseVisualStyleBackColor = True
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
        'cbFlashAlg
        '
        resources.ApplyResources(Me.cbFlashAlg, "cbFlashAlg")
        Me.cbFlashAlg.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlashAlg.FormattingEnabled = True
        Me.cbFlashAlg.Name = "cbFlashAlg"
        Me.ToolTip1.SetToolTip(Me.cbFlashAlg, resources.GetString("cbFlashAlg.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbFlashAlg, resources.GetString("cbFlashAlg.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbFlashAlg, resources.GetString("cbFlashAlg.ToolTip2"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip2"))
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
        Me.TabGeneral.Controls.Add(Me.lblTemperatureUnit)
        Me.TabGeneral.Controls.Add(Me.Label30)
        Me.TabGeneral.Controls.Add(Me.tbMaxTChange)
        Me.TabGeneral.Controls.Add(Me.cbSolverScheme)
        Me.TabGeneral.Controls.Add(Me.Label27)
        Me.TabGeneral.Controls.Add(Me.cbSolvingMethod)
        Me.TabGeneral.Controls.Add(Me.Label18)
        Me.TabGeneral.Controls.Add(Me.Label16)
        Me.TabGeneral.Controls.Add(Me.btnConfigureFlashAlg)
        Me.TabGeneral.Controls.Add(Me.tbConvTol)
        Me.TabGeneral.Controls.Add(Me.btnConfigurePP)
        Me.TabGeneral.Controls.Add(Me.Label14)
        Me.TabGeneral.Controls.Add(Me.cbAbsorberMode)
        Me.TabGeneral.Controls.Add(Me.tbMaxIt)
        Me.TabGeneral.Controls.Add(Me.cbFlashAlg)
        Me.TabGeneral.Controls.Add(Me.Label10)
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
        'lblTemperatureUnit
        '
        resources.ApplyResources(Me.lblTemperatureUnit, "lblTemperatureUnit")
        Me.lblTemperatureUnit.Name = "lblTemperatureUnit"
        Me.ToolTip1.SetToolTip(Me.lblTemperatureUnit, resources.GetString("lblTemperatureUnit.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblTemperatureUnit, resources.GetString("lblTemperatureUnit.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblTemperatureUnit, resources.GetString("lblTemperatureUnit.ToolTip2"))
        '
        'Label30
        '
        resources.ApplyResources(Me.Label30, "Label30")
        Me.Label30.Name = "Label30"
        Me.ToolTip1.SetToolTip(Me.Label30, resources.GetString("Label30.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label30, resources.GetString("Label30.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label30, resources.GetString("Label30.ToolTip2"))
        '
        'tbMaxTChange
        '
        resources.ApplyResources(Me.tbMaxTChange, "tbMaxTChange")
        Me.tbMaxTChange.Name = "tbMaxTChange"
        Me.ToolTipValues.SetToolTip(Me.tbMaxTChange, resources.GetString("tbMaxTChange.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbMaxTChange, resources.GetString("tbMaxTChange.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbMaxTChange, resources.GetString("tbMaxTChange.ToolTip2"))
        '
        'cbSolverScheme
        '
        resources.ApplyResources(Me.cbSolverScheme, "cbSolverScheme")
        Me.cbSolverScheme.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSolverScheme.FormattingEnabled = True
        Me.cbSolverScheme.Items.AddRange(New Object() {resources.GetString("cbSolverScheme.Items"), resources.GetString("cbSolverScheme.Items1"), resources.GetString("cbSolverScheme.Items2"), resources.GetString("cbSolverScheme.Items3")})
        Me.cbSolverScheme.Name = "cbSolverScheme"
        Me.ToolTip1.SetToolTip(Me.cbSolverScheme, resources.GetString("cbSolverScheme.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSolverScheme, resources.GetString("cbSolverScheme.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbSolverScheme, resources.GetString("cbSolverScheme.ToolTip2"))
        '
        'Label27
        '
        resources.ApplyResources(Me.Label27, "Label27")
        Me.Label27.Name = "Label27"
        Me.ToolTip1.SetToolTip(Me.Label27, resources.GetString("Label27.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label27, resources.GetString("Label27.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label27, resources.GetString("Label27.ToolTip2"))
        '
        'cbSolvingMethod
        '
        resources.ApplyResources(Me.cbSolvingMethod, "cbSolvingMethod")
        Me.cbSolvingMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSolvingMethod.DropDownWidth = 250
        Me.cbSolvingMethod.FormattingEnabled = True
        Me.cbSolvingMethod.Items.AddRange(New Object() {resources.GetString("cbSolvingMethod.Items"), resources.GetString("cbSolvingMethod.Items1"), resources.GetString("cbSolvingMethod.Items2"), resources.GetString("cbSolvingMethod.Items3")})
        Me.cbSolvingMethod.Name = "cbSolvingMethod"
        Me.ToolTip1.SetToolTip(Me.cbSolvingMethod, resources.GetString("cbSolvingMethod.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSolvingMethod, resources.GetString("cbSolvingMethod.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbSolvingMethod, resources.GetString("cbSolvingMethod.ToolTip2"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip2"))
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
        Me.TabCondenser.Controls.Add(Me.Label19)
        Me.TabCondenser.Controls.Add(Me.cbCondComp)
        Me.TabCondenser.Controls.Add(Me.cbCondType)
        Me.TabCondenser.Controls.Add(Me.Label2)
        Me.TabCondenser.Controls.Add(Me.Label8)
        Me.TabCondenser.Controls.Add(Me.cbCondVapFlowUnits)
        Me.TabCondenser.Controls.Add(Me.tbCondVapFlow)
        Me.TabCondenser.Controls.Add(Me.cbCondSpecUnits)
        Me.TabCondenser.Controls.Add(Me.tbCondSpec)
        Me.TabCondenser.Controls.Add(Me.Label7)
        Me.TabCondenser.Controls.Add(Me.cbCondSpec)
        Me.TabCondenser.Controls.Add(Me.Label5)
        Me.TabCondenser.Controls.Add(Me.cbCondPDropUnits)
        Me.TabCondenser.Controls.Add(Me.tbCondPDrop)
        Me.TabCondenser.Controls.Add(Me.Label4)
        Me.TabCondenser.Controls.Add(Me.cbCondPressureUnits)
        Me.TabCondenser.Controls.Add(Me.tbCondPressure)
        Me.TabCondenser.Name = "TabCondenser"
        Me.ToolTip1.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabCondenser, resources.GetString("TabCondenser.ToolTip2"))
        Me.TabCondenser.UseVisualStyleBackColor = True
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
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTip1.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip2"))
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
        'tbCondVapFlow
        '
        resources.ApplyResources(Me.tbCondVapFlow, "tbCondVapFlow")
        Me.tbCondVapFlow.Name = "tbCondVapFlow"
        Me.ToolTipValues.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondVapFlow, resources.GetString("tbCondVapFlow.ToolTip2"))
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
        'tbCondSpec
        '
        resources.ApplyResources(Me.tbCondSpec, "tbCondSpec")
        Me.tbCondSpec.Name = "tbCondSpec"
        Me.ToolTipValues.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondSpec, resources.GetString("tbCondSpec.ToolTip2"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip2"))
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
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
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
        'tbCondPDrop
        '
        resources.ApplyResources(Me.tbCondPDrop, "tbCondPDrop")
        Me.tbCondPDrop.Name = "tbCondPDrop"
        Me.ToolTipValues.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCondPDrop, resources.GetString("tbCondPDrop.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
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
        'TabReboiler
        '
        resources.ApplyResources(Me.TabReboiler, "TabReboiler")
        Me.TabReboiler.Controls.Add(Me.Label24)
        Me.TabReboiler.Controls.Add(Me.cbRebComp)
        Me.TabReboiler.Controls.Add(Me.cbRebSpecUnits)
        Me.TabReboiler.Controls.Add(Me.tbRebSpecValue)
        Me.TabReboiler.Controls.Add(Me.Label15)
        Me.TabReboiler.Controls.Add(Me.cbRebSpec)
        Me.TabReboiler.Controls.Add(Me.Label17)
        Me.TabReboiler.Controls.Add(Me.cbRebPressure)
        Me.TabReboiler.Controls.Add(Me.tbRebPressure)
        Me.TabReboiler.Name = "TabReboiler"
        Me.ToolTip1.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabReboiler, resources.GetString("TabReboiler.ToolTip2"))
        Me.TabReboiler.UseVisualStyleBackColor = True
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        Me.ToolTip1.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip2"))
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
        'tbRebSpecValue
        '
        resources.ApplyResources(Me.tbRebSpecValue, "tbRebSpecValue")
        Me.tbRebSpecValue.Name = "tbRebSpecValue"
        Me.ToolTipValues.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRebSpecValue, resources.GetString("tbRebSpecValue.ToolTip2"))
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
        Me.TabControl2.Controls.Add(Me.TabSolverBP)
        Me.TabControl2.Controls.Add(Me.TabSolverNS)
        Me.TabControl2.Controls.Add(Me.TabSolverIO)
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
        'TabSolverBP
        '
        resources.ApplyResources(Me.TabSolverBP, "TabSolverBP")
        Me.TabSolverBP.Controls.Add(Me.Label3)
        Me.TabSolverBP.Controls.Add(Me.tbBPStopAtIter)
        Me.TabSolverBP.Name = "TabSolverBP"
        Me.ToolTip1.SetToolTip(Me.TabSolverBP, resources.GetString("TabSolverBP.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabSolverBP, resources.GetString("TabSolverBP.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabSolverBP, resources.GetString("TabSolverBP.ToolTip2"))
        Me.TabSolverBP.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'tbBPStopAtIter
        '
        resources.ApplyResources(Me.tbBPStopAtIter, "tbBPStopAtIter")
        Me.tbBPStopAtIter.Name = "tbBPStopAtIter"
        Me.ToolTipValues.SetToolTip(Me.tbBPStopAtIter, resources.GetString("tbBPStopAtIter.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbBPStopAtIter, resources.GetString("tbBPStopAtIter.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbBPStopAtIter, resources.GetString("tbBPStopAtIter.ToolTip2"))
        '
        'TabSolverNS
        '
        resources.ApplyResources(Me.TabSolverNS, "TabSolverNS")
        Me.TabSolverNS.Controls.Add(Me.tb_NS_NumDeriv)
        Me.TabSolverNS.Controls.Add(Me.Label28)
        Me.TabSolverNS.Controls.Add(Me.cbMinMethodNS)
        Me.TabSolverNS.Controls.Add(Me.Label25)
        Me.TabSolverNS.Controls.Add(Me.tb_NS_UpperBound)
        Me.TabSolverNS.Controls.Add(Me.tb_NS_LowerBound)
        Me.TabSolverNS.Controls.Add(Me.cbNSPreconditioning)
        Me.TabSolverNS.Controls.Add(Me.Label21)
        Me.TabSolverNS.Controls.Add(Me.Label20)
        Me.TabSolverNS.Name = "TabSolverNS"
        Me.ToolTip1.SetToolTip(Me.TabSolverNS, resources.GetString("TabSolverNS.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabSolverNS, resources.GetString("TabSolverNS.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabSolverNS, resources.GetString("TabSolverNS.ToolTip2"))
        Me.TabSolverNS.UseVisualStyleBackColor = True
        '
        'tb_NS_NumDeriv
        '
        resources.ApplyResources(Me.tb_NS_NumDeriv, "tb_NS_NumDeriv")
        Me.tb_NS_NumDeriv.Name = "tb_NS_NumDeriv"
        Me.ToolTipValues.SetToolTip(Me.tb_NS_NumDeriv, resources.GetString("tb_NS_NumDeriv.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_NS_NumDeriv, resources.GetString("tb_NS_NumDeriv.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_NS_NumDeriv, resources.GetString("tb_NS_NumDeriv.ToolTip2"))
        '
        'Label28
        '
        resources.ApplyResources(Me.Label28, "Label28")
        Me.Label28.Name = "Label28"
        Me.ToolTip1.SetToolTip(Me.Label28, resources.GetString("Label28.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label28, resources.GetString("Label28.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label28, resources.GetString("Label28.ToolTip2"))
        '
        'cbMinMethodNS
        '
        resources.ApplyResources(Me.cbMinMethodNS, "cbMinMethodNS")
        Me.cbMinMethodNS.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMinMethodNS.FormattingEnabled = True
        Me.cbMinMethodNS.Items.AddRange(New Object() {resources.GetString("cbMinMethodNS.Items"), resources.GetString("cbMinMethodNS.Items1"), resources.GetString("cbMinMethodNS.Items2"), resources.GetString("cbMinMethodNS.Items3"), resources.GetString("cbMinMethodNS.Items4"), resources.GetString("cbMinMethodNS.Items5"), resources.GetString("cbMinMethodNS.Items6"), resources.GetString("cbMinMethodNS.Items7"), resources.GetString("cbMinMethodNS.Items8"), resources.GetString("cbMinMethodNS.Items9"), resources.GetString("cbMinMethodNS.Items10")})
        Me.cbMinMethodNS.Name = "cbMinMethodNS"
        Me.ToolTip1.SetToolTip(Me.cbMinMethodNS, resources.GetString("cbMinMethodNS.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbMinMethodNS, resources.GetString("cbMinMethodNS.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbMinMethodNS, resources.GetString("cbMinMethodNS.ToolTip2"))
        '
        'Label25
        '
        resources.ApplyResources(Me.Label25, "Label25")
        Me.Label25.Name = "Label25"
        Me.ToolTip1.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip2"))
        '
        'tb_NS_UpperBound
        '
        resources.ApplyResources(Me.tb_NS_UpperBound, "tb_NS_UpperBound")
        Me.tb_NS_UpperBound.Name = "tb_NS_UpperBound"
        Me.ToolTipValues.SetToolTip(Me.tb_NS_UpperBound, resources.GetString("tb_NS_UpperBound.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_NS_UpperBound, resources.GetString("tb_NS_UpperBound.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_NS_UpperBound, resources.GetString("tb_NS_UpperBound.ToolTip2"))
        '
        'tb_NS_LowerBound
        '
        resources.ApplyResources(Me.tb_NS_LowerBound, "tb_NS_LowerBound")
        Me.tb_NS_LowerBound.Name = "tb_NS_LowerBound"
        Me.ToolTipValues.SetToolTip(Me.tb_NS_LowerBound, resources.GetString("tb_NS_LowerBound.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_NS_LowerBound, resources.GetString("tb_NS_LowerBound.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_NS_LowerBound, resources.GetString("tb_NS_LowerBound.ToolTip2"))
        '
        'cbNSPreconditioning
        '
        resources.ApplyResources(Me.cbNSPreconditioning, "cbNSPreconditioning")
        Me.cbNSPreconditioning.Name = "cbNSPreconditioning"
        Me.ToolTip1.SetToolTip(Me.cbNSPreconditioning, resources.GetString("cbNSPreconditioning.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbNSPreconditioning, resources.GetString("cbNSPreconditioning.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.cbNSPreconditioning, resources.GetString("cbNSPreconditioning.ToolTip2"))
        Me.cbNSPreconditioning.UseVisualStyleBackColor = True
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTip1.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip2"))
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTip1.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip2"))
        '
        'TabSolverIO
        '
        resources.ApplyResources(Me.TabSolverIO, "TabSolverIO")
        Me.TabSolverIO.Controls.Add(Me.tb_IO_NumDeriv)
        Me.TabSolverIO.Controls.Add(Me.Label29)
        Me.TabSolverIO.Controls.Add(Me.cbMinMethodIO)
        Me.TabSolverIO.Controls.Add(Me.Label26)
        Me.TabSolverIO.Controls.Add(Me.tb_IO_UpperBound)
        Me.TabSolverIO.Controls.Add(Me.tb_IO_LowerBound)
        Me.TabSolverIO.Controls.Add(Me.Label22)
        Me.TabSolverIO.Controls.Add(Me.Label23)
        Me.TabSolverIO.Controls.Add(Me.chkIOAverageKb)
        Me.TabSolverIO.Controls.Add(Me.chkIOAdjustSb)
        Me.TabSolverIO.Name = "TabSolverIO"
        Me.ToolTip1.SetToolTip(Me.TabSolverIO, resources.GetString("TabSolverIO.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabSolverIO, resources.GetString("TabSolverIO.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabSolverIO, resources.GetString("TabSolverIO.ToolTip2"))
        Me.TabSolverIO.UseVisualStyleBackColor = True
        '
        'tb_IO_NumDeriv
        '
        resources.ApplyResources(Me.tb_IO_NumDeriv, "tb_IO_NumDeriv")
        Me.tb_IO_NumDeriv.Name = "tb_IO_NumDeriv"
        Me.ToolTipValues.SetToolTip(Me.tb_IO_NumDeriv, resources.GetString("tb_IO_NumDeriv.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_IO_NumDeriv, resources.GetString("tb_IO_NumDeriv.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_IO_NumDeriv, resources.GetString("tb_IO_NumDeriv.ToolTip2"))
        '
        'Label29
        '
        resources.ApplyResources(Me.Label29, "Label29")
        Me.Label29.Name = "Label29"
        Me.ToolTip1.SetToolTip(Me.Label29, resources.GetString("Label29.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label29, resources.GetString("Label29.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label29, resources.GetString("Label29.ToolTip2"))
        '
        'cbMinMethodIO
        '
        resources.ApplyResources(Me.cbMinMethodIO, "cbMinMethodIO")
        Me.cbMinMethodIO.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMinMethodIO.FormattingEnabled = True
        Me.cbMinMethodIO.Items.AddRange(New Object() {resources.GetString("cbMinMethodIO.Items"), resources.GetString("cbMinMethodIO.Items1"), resources.GetString("cbMinMethodIO.Items2"), resources.GetString("cbMinMethodIO.Items3"), resources.GetString("cbMinMethodIO.Items4"), resources.GetString("cbMinMethodIO.Items5"), resources.GetString("cbMinMethodIO.Items6"), resources.GetString("cbMinMethodIO.Items7"), resources.GetString("cbMinMethodIO.Items8"), resources.GetString("cbMinMethodIO.Items9"), resources.GetString("cbMinMethodIO.Items10")})
        Me.cbMinMethodIO.Name = "cbMinMethodIO"
        Me.ToolTip1.SetToolTip(Me.cbMinMethodIO, resources.GetString("cbMinMethodIO.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbMinMethodIO, resources.GetString("cbMinMethodIO.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbMinMethodIO, resources.GetString("cbMinMethodIO.ToolTip2"))
        '
        'Label26
        '
        resources.ApplyResources(Me.Label26, "Label26")
        Me.Label26.Name = "Label26"
        Me.ToolTip1.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip2"))
        '
        'tb_IO_UpperBound
        '
        resources.ApplyResources(Me.tb_IO_UpperBound, "tb_IO_UpperBound")
        Me.tb_IO_UpperBound.Name = "tb_IO_UpperBound"
        Me.ToolTipValues.SetToolTip(Me.tb_IO_UpperBound, resources.GetString("tb_IO_UpperBound.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_IO_UpperBound, resources.GetString("tb_IO_UpperBound.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_IO_UpperBound, resources.GetString("tb_IO_UpperBound.ToolTip2"))
        '
        'tb_IO_LowerBound
        '
        resources.ApplyResources(Me.tb_IO_LowerBound, "tb_IO_LowerBound")
        Me.tb_IO_LowerBound.Name = "tb_IO_LowerBound"
        Me.ToolTipValues.SetToolTip(Me.tb_IO_LowerBound, resources.GetString("tb_IO_LowerBound.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tb_IO_LowerBound, resources.GetString("tb_IO_LowerBound.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tb_IO_LowerBound, resources.GetString("tb_IO_LowerBound.ToolTip2"))
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        Me.ToolTip1.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip2"))
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        Me.ToolTip1.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip2"))
        '
        'chkIOAverageKb
        '
        resources.ApplyResources(Me.chkIOAverageKb, "chkIOAverageKb")
        Me.chkIOAverageKb.Name = "chkIOAverageKb"
        Me.ToolTip1.SetToolTip(Me.chkIOAverageKb, resources.GetString("chkIOAverageKb.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkIOAverageKb, resources.GetString("chkIOAverageKb.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkIOAverageKb, resources.GetString("chkIOAverageKb.ToolTip2"))
        Me.chkIOAverageKb.UseVisualStyleBackColor = True
        '
        'chkIOAdjustSb
        '
        resources.ApplyResources(Me.chkIOAdjustSb, "chkIOAdjustSb")
        Me.chkIOAdjustSb.Name = "chkIOAdjustSb"
        Me.ToolTip1.SetToolTip(Me.chkIOAdjustSb, resources.GetString("chkIOAdjustSb.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkIOAdjustSb, resources.GetString("chkIOAdjustSb.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkIOAdjustSb, resources.GetString("chkIOAdjustSb.ToolTip2"))
        Me.chkIOAdjustSb.UseVisualStyleBackColor = True
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
        DataGridViewCellStyle4.BackColor = System.Drawing.SystemColors.Control
        Me.DataGridViewTextBoxColumn1.DefaultCellStyle = DataGridViewCellStyle4
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        Me.DataGridViewTextBoxColumn1.ReadOnly = True
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle5
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        Me.DataGridViewTextBoxColumn2.ReadOnly = True
        '
        'Column1
        '
        DataGridViewCellStyle6.BackColor = System.Drawing.SystemColors.Control
        Me.Column1.DefaultCellStyle = DataGridViewCellStyle6
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
        Me.TabReboiler.ResumeLayout(False)
        Me.TabReboiler.PerformLayout()
        Me.TabControl2.ResumeLayout(False)
        Me.TabInitialEstimates.ResumeLayout(False)
        Me.TabInitialEstimates.PerformLayout()
        Me.TabSolverBP.ResumeLayout(False)
        Me.TabSolverBP.PerformLayout()
        Me.TabSolverNS.ResumeLayout(False)
        Me.TabSolverNS.PerformLayout()
        Me.TabSolverIO.ResumeLayout(False)
        Me.TabSolverIO.PerformLayout()
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
    Public WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
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
    Public WithEvents TabSolverBP As System.Windows.Forms.TabPage
    Public WithEvents TabInitialEstimates As System.Windows.Forms.TabPage
    Public WithEvents TabSolverNS As System.Windows.Forms.TabPage
    Public WithEvents TabSolverIO As System.Windows.Forms.TabPage
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
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents tbBPStopAtIter As System.Windows.Forms.TextBox
    Public WithEvents Label21 As System.Windows.Forms.Label
    Public WithEvents Label20 As System.Windows.Forms.Label
    Public WithEvents chkIOAverageKb As System.Windows.Forms.CheckBox
    Public WithEvents chkIOAdjustSb As System.Windows.Forms.CheckBox
    Public WithEvents InitialEstimatesPanel As System.Windows.Forms.Panel
    Public WithEvents chkUseIE_C As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_VF As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_LF As System.Windows.Forms.CheckBox
    Public WithEvents chkUseIE_T As System.Windows.Forms.CheckBox
    Public WithEvents cbSolvingMethod As System.Windows.Forms.ComboBox
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents btnResults As System.Windows.Forms.Button
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents cbCondComp As System.Windows.Forms.ComboBox
    Public WithEvents Label24 As System.Windows.Forms.Label
    Public WithEvents cbRebComp As System.Windows.Forms.ComboBox
    Public WithEvents tb_NS_UpperBound As System.Windows.Forms.TextBox
    Public WithEvents tb_NS_LowerBound As System.Windows.Forms.TextBox
    Public WithEvents cbNSPreconditioning As System.Windows.Forms.CheckBox
    Public WithEvents tb_IO_UpperBound As System.Windows.Forms.TextBox
    Public WithEvents tb_IO_LowerBound As System.Windows.Forms.TextBox
    Public WithEvents Label22 As System.Windows.Forms.Label
    Public WithEvents Label23 As System.Windows.Forms.Label
    Public WithEvents cbMinMethodNS As System.Windows.Forms.ComboBox
    Public WithEvents Label25 As System.Windows.Forms.Label
    Public WithEvents cbMinMethodIO As System.Windows.Forms.ComboBox
    Public WithEvents Label26 As System.Windows.Forms.Label
    Public WithEvents cbSolverScheme As System.Windows.Forms.ComboBox
    Public WithEvents Label27 As System.Windows.Forms.Label
    Public WithEvents tb_NS_NumDeriv As System.Windows.Forms.TextBox
    Public WithEvents Label28 As System.Windows.Forms.Label
    Public WithEvents tb_IO_NumDeriv As System.Windows.Forms.TextBox
    Public WithEvents Label29 As System.Windows.Forms.Label
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents lblTemperatureUnit As Label
    Public WithEvents Label30 As Label
    Public WithEvents tbMaxTChange As TextBox
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents TabControl3 As TabControl
    Friend WithEvents TabPage2 As TabPage
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents TabPage4 As TabPage
End Class
