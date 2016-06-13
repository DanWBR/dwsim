<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Column

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
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabGeneral = New System.Windows.Forms.TabPage()
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
        Me.cbRebSpecUnits = New System.Windows.Forms.ComboBox()
        Me.tbRebSpecValue = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbRebSpec = New System.Windows.Forms.ComboBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.cbRebPressure = New System.Windows.Forms.ComboBox()
        Me.tbRebPressure = New System.Windows.Forms.TextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.TabControl2 = New System.Windows.Forms.TabControl()
        Me.TabConnections = New System.Windows.Forms.TabPage()
        Me.TabStages = New System.Windows.Forms.TabPage()
        Me.TabInitialEstimates = New System.Windows.Forms.TabPage()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.chkUseIE_C = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_VF = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_LF = New System.Windows.Forms.CheckBox()
        Me.chkUseIE_T = New System.Windows.Forms.CheckBox()
        Me.TabSolverBP = New System.Windows.Forms.TabPage()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbBPStopAtIter = New System.Windows.Forms.TextBox()
        Me.TabSolverNS = New System.Windows.Forms.TabPage()
        Me.chkNSJacobian = New System.Windows.Forms.CheckBox()
        Me.chkNSUseNewton = New System.Windows.Forms.CheckBox()
        Me.chkNSUseDampingFactor = New System.Windows.Forms.CheckBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.cbNSMaximumDeltaT = New System.Windows.Forms.ComboBox()
        Me.tbNSMaximumDeltaT = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.tbNSNumericalDerivativeStep = New System.Windows.Forms.TextBox()
        Me.TabSolverIO = New System.Windows.Forms.TabPage()
        Me.chkIOAverageKb = New System.Windows.Forms.CheckBox()
        Me.chkIOAdjustSb = New System.Windows.Forms.CheckBox()
        Me.tbIOMaxDamping = New System.Windows.Forms.TextBox()
        Me.tbIOMinDamping = New System.Windows.Forms.TextBox()
        Me.chkIOJacobian = New System.Windows.Forms.CheckBox()
        Me.chkIONewton = New System.Windows.Forms.CheckBox()
        Me.chkIOUseDampingFactor = New System.Windows.Forms.CheckBox()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.cbIOTempPerturbationUnits = New System.Windows.Forms.ComboBox()
        Me.tbIOTempPerturbation = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.tbIONumericalDerivativeStep = New System.Windows.Forms.TextBox()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.tabstrip1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.gridResults = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabResults = New System.Windows.Forms.TabPage()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabGeneral.SuspendLayout()
        Me.TabCondenser.SuspendLayout()
        Me.TabReboiler.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.TabControl2.SuspendLayout()
        Me.TabInitialEstimates.SuspendLayout()
        Me.TabSolverBP.SuspendLayout()
        Me.TabSolverNS.SuspendLayout()
        Me.TabSolverIO.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        Me.tabstrip1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
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
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
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
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
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
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'cbPropPack
        '
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Name = "cbPropPack"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.TabControl1)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabGeneral)
        Me.TabControl1.Controls.Add(Me.TabCondenser)
        Me.TabControl1.Controls.Add(Me.TabReboiler)
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabGeneral
        '
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
        resources.ApplyResources(Me.TabGeneral, "TabGeneral")
        Me.TabGeneral.Name = "TabGeneral"
        Me.TabGeneral.UseVisualStyleBackColor = True
        '
        'cbSolvingMethod
        '
        resources.ApplyResources(Me.cbSolvingMethod, "cbSolvingMethod")
        Me.cbSolvingMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSolvingMethod.FormattingEnabled = True
        Me.cbSolvingMethod.Items.AddRange(New Object() {resources.GetString("cbSolvingMethod.Items"), resources.GetString("cbSolvingMethod.Items1"), resources.GetString("cbSolvingMethod.Items2")})
        Me.cbSolvingMethod.Name = "cbSolvingMethod"
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'tbConvTol
        '
        resources.ApplyResources(Me.tbConvTol, "tbConvTol")
        Me.tbConvTol.Name = "tbConvTol"
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbAbsorberMode
        '
        resources.ApplyResources(Me.cbAbsorberMode, "cbAbsorberMode")
        Me.cbAbsorberMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbAbsorberMode.FormattingEnabled = True
        Me.cbAbsorberMode.Items.AddRange(New Object() {resources.GetString("cbAbsorberMode.Items"), resources.GetString("cbAbsorberMode.Items1")})
        Me.cbAbsorberMode.Name = "cbAbsorberMode"
        '
        'tbMaxIt
        '
        resources.ApplyResources(Me.tbMaxIt, "tbMaxIt")
        Me.tbMaxIt.Name = "tbMaxIt"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbNStages
        '
        resources.ApplyResources(Me.tbNStages, "tbNStages")
        Me.tbNStages.Name = "tbNStages"
        '
        'TabCondenser
        '
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
        resources.ApplyResources(Me.TabCondenser, "TabCondenser")
        Me.TabCondenser.Name = "TabCondenser"
        Me.TabCondenser.UseVisualStyleBackColor = True
        '
        'cbCondType
        '
        resources.ApplyResources(Me.cbCondType, "cbCondType")
        Me.cbCondType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondType.FormattingEnabled = True
        Me.cbCondType.Items.AddRange(New Object() {resources.GetString("cbCondType.Items"), resources.GetString("cbCondType.Items1"), resources.GetString("cbCondType.Items2")})
        Me.cbCondType.Name = "cbCondType"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'cbCondVapFlowUnits
        '
        resources.ApplyResources(Me.cbCondVapFlowUnits, "cbCondVapFlowUnits")
        Me.cbCondVapFlowUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondVapFlowUnits.FormattingEnabled = True
        Me.cbCondVapFlowUnits.Items.AddRange(New Object() {resources.GetString("cbCondVapFlowUnits.Items"), resources.GetString("cbCondVapFlowUnits.Items1"), resources.GetString("cbCondVapFlowUnits.Items2")})
        Me.cbCondVapFlowUnits.Name = "cbCondVapFlowUnits"
        '
        'tbCondVapFlow
        '
        resources.ApplyResources(Me.tbCondVapFlow, "tbCondVapFlow")
        Me.tbCondVapFlow.Name = "tbCondVapFlow"
        '
        'cbCondSpecUnits
        '
        resources.ApplyResources(Me.cbCondSpecUnits, "cbCondSpecUnits")
        Me.cbCondSpecUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondSpecUnits.FormattingEnabled = True
        Me.cbCondSpecUnits.Items.AddRange(New Object() {resources.GetString("cbCondSpecUnits.Items"), resources.GetString("cbCondSpecUnits.Items1"), resources.GetString("cbCondSpecUnits.Items2")})
        Me.cbCondSpecUnits.Name = "cbCondSpecUnits"
        '
        'tbCondSpec
        '
        resources.ApplyResources(Me.tbCondSpec, "tbCondSpec")
        Me.tbCondSpec.Name = "tbCondSpec"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'cbCondSpec
        '
        resources.ApplyResources(Me.cbCondSpec, "cbCondSpec")
        Me.cbCondSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondSpec.FormattingEnabled = True
        Me.cbCondSpec.Items.AddRange(New Object() {resources.GetString("cbCondSpec.Items"), resources.GetString("cbCondSpec.Items1"), resources.GetString("cbCondSpec.Items2"), resources.GetString("cbCondSpec.Items3"), resources.GetString("cbCondSpec.Items4"), resources.GetString("cbCondSpec.Items5"), resources.GetString("cbCondSpec.Items6"), resources.GetString("cbCondSpec.Items7"), resources.GetString("cbCondSpec.Items8")})
        Me.cbCondSpec.Name = "cbCondSpec"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'cbCondPDropUnits
        '
        resources.ApplyResources(Me.cbCondPDropUnits, "cbCondPDropUnits")
        Me.cbCondPDropUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondPDropUnits.FormattingEnabled = True
        Me.cbCondPDropUnits.Items.AddRange(New Object() {resources.GetString("cbCondPDropUnits.Items"), resources.GetString("cbCondPDropUnits.Items1"), resources.GetString("cbCondPDropUnits.Items2")})
        Me.cbCondPDropUnits.Name = "cbCondPDropUnits"
        '
        'tbCondPDrop
        '
        resources.ApplyResources(Me.tbCondPDrop, "tbCondPDrop")
        Me.tbCondPDrop.Name = "tbCondPDrop"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'cbCondPressureUnits
        '
        resources.ApplyResources(Me.cbCondPressureUnits, "cbCondPressureUnits")
        Me.cbCondPressureUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCondPressureUnits.FormattingEnabled = True
        Me.cbCondPressureUnits.Items.AddRange(New Object() {resources.GetString("cbCondPressureUnits.Items"), resources.GetString("cbCondPressureUnits.Items1"), resources.GetString("cbCondPressureUnits.Items2")})
        Me.cbCondPressureUnits.Name = "cbCondPressureUnits"
        '
        'tbCondPressure
        '
        resources.ApplyResources(Me.tbCondPressure, "tbCondPressure")
        Me.tbCondPressure.Name = "tbCondPressure"
        '
        'TabReboiler
        '
        Me.TabReboiler.Controls.Add(Me.cbRebSpecUnits)
        Me.TabReboiler.Controls.Add(Me.tbRebSpecValue)
        Me.TabReboiler.Controls.Add(Me.Label15)
        Me.TabReboiler.Controls.Add(Me.cbRebSpec)
        Me.TabReboiler.Controls.Add(Me.Label17)
        Me.TabReboiler.Controls.Add(Me.cbRebPressure)
        Me.TabReboiler.Controls.Add(Me.tbRebPressure)
        resources.ApplyResources(Me.TabReboiler, "TabReboiler")
        Me.TabReboiler.Name = "TabReboiler"
        Me.TabReboiler.UseVisualStyleBackColor = True
        '
        'cbRebSpecUnits
        '
        resources.ApplyResources(Me.cbRebSpecUnits, "cbRebSpecUnits")
        Me.cbRebSpecUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebSpecUnits.FormattingEnabled = True
        Me.cbRebSpecUnits.Items.AddRange(New Object() {resources.GetString("cbRebSpecUnits.Items"), resources.GetString("cbRebSpecUnits.Items1"), resources.GetString("cbRebSpecUnits.Items2")})
        Me.cbRebSpecUnits.Name = "cbRebSpecUnits"
        '
        'tbRebSpecValue
        '
        resources.ApplyResources(Me.tbRebSpecValue, "tbRebSpecValue")
        Me.tbRebSpecValue.Name = "tbRebSpecValue"
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'cbRebSpec
        '
        resources.ApplyResources(Me.cbRebSpec, "cbRebSpec")
        Me.cbRebSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebSpec.FormattingEnabled = True
        Me.cbRebSpec.Items.AddRange(New Object() {resources.GetString("cbRebSpec.Items"), resources.GetString("cbRebSpec.Items1"), resources.GetString("cbRebSpec.Items2"), resources.GetString("cbRebSpec.Items3"), resources.GetString("cbRebSpec.Items4"), resources.GetString("cbRebSpec.Items5"), resources.GetString("cbRebSpec.Items6"), resources.GetString("cbRebSpec.Items7"), resources.GetString("cbRebSpec.Items8")})
        Me.cbRebSpec.Name = "cbRebSpec"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        '
        'cbRebPressure
        '
        resources.ApplyResources(Me.cbRebPressure, "cbRebPressure")
        Me.cbRebPressure.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRebPressure.FormattingEnabled = True
        Me.cbRebPressure.Items.AddRange(New Object() {resources.GetString("cbRebPressure.Items"), resources.GetString("cbRebPressure.Items1"), resources.GetString("cbRebPressure.Items2")})
        Me.cbRebPressure.Name = "cbRebPressure"
        '
        'tbRebPressure
        '
        resources.ApplyResources(Me.tbRebPressure, "tbRebPressure")
        Me.tbRebPressure.Name = "tbRebPressure"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.TabControl2)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'TabControl2
        '
        Me.TabControl2.Controls.Add(Me.TabConnections)
        Me.TabControl2.Controls.Add(Me.TabStages)
        Me.TabControl2.Controls.Add(Me.TabInitialEstimates)
        Me.TabControl2.Controls.Add(Me.TabSolverBP)
        Me.TabControl2.Controls.Add(Me.TabSolverNS)
        Me.TabControl2.Controls.Add(Me.TabSolverIO)
        resources.ApplyResources(Me.TabControl2, "TabControl2")
        Me.TabControl2.Name = "TabControl2"
        Me.TabControl2.SelectedIndex = 0
        '
        'TabConnections
        '
        resources.ApplyResources(Me.TabConnections, "TabConnections")
        Me.TabConnections.Name = "TabConnections"
        Me.TabConnections.UseVisualStyleBackColor = True
        '
        'TabStages
        '
        resources.ApplyResources(Me.TabStages, "TabStages")
        Me.TabStages.Name = "TabStages"
        Me.TabStages.UseVisualStyleBackColor = True
        '
        'TabInitialEstimates
        '
        Me.TabInitialEstimates.Controls.Add(Me.Panel1)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_C)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_VF)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_LF)
        Me.TabInitialEstimates.Controls.Add(Me.chkUseIE_T)
        resources.ApplyResources(Me.TabInitialEstimates, "TabInitialEstimates")
        Me.TabInitialEstimates.Name = "TabInitialEstimates"
        Me.TabInitialEstimates.UseVisualStyleBackColor = True
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'chkUseIE_C
        '
        resources.ApplyResources(Me.chkUseIE_C, "chkUseIE_C")
        Me.chkUseIE_C.Name = "chkUseIE_C"
        Me.chkUseIE_C.UseVisualStyleBackColor = True
        '
        'chkUseIE_VF
        '
        resources.ApplyResources(Me.chkUseIE_VF, "chkUseIE_VF")
        Me.chkUseIE_VF.Name = "chkUseIE_VF"
        Me.chkUseIE_VF.UseVisualStyleBackColor = True
        '
        'chkUseIE_LF
        '
        resources.ApplyResources(Me.chkUseIE_LF, "chkUseIE_LF")
        Me.chkUseIE_LF.Name = "chkUseIE_LF"
        Me.chkUseIE_LF.UseVisualStyleBackColor = True
        '
        'chkUseIE_T
        '
        resources.ApplyResources(Me.chkUseIE_T, "chkUseIE_T")
        Me.chkUseIE_T.Name = "chkUseIE_T"
        Me.chkUseIE_T.UseVisualStyleBackColor = True
        '
        'TabSolverBP
        '
        Me.TabSolverBP.Controls.Add(Me.Label3)
        Me.TabSolverBP.Controls.Add(Me.tbBPStopAtIter)
        resources.ApplyResources(Me.TabSolverBP, "TabSolverBP")
        Me.TabSolverBP.Name = "TabSolverBP"
        Me.TabSolverBP.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'tbBPStopAtIter
        '
        resources.ApplyResources(Me.tbBPStopAtIter, "tbBPStopAtIter")
        Me.tbBPStopAtIter.Name = "tbBPStopAtIter"
        '
        'TabSolverNS
        '
        Me.TabSolverNS.Controls.Add(Me.chkNSJacobian)
        Me.TabSolverNS.Controls.Add(Me.chkNSUseNewton)
        Me.TabSolverNS.Controls.Add(Me.chkNSUseDampingFactor)
        Me.TabSolverNS.Controls.Add(Me.Label21)
        Me.TabSolverNS.Controls.Add(Me.cbNSMaximumDeltaT)
        Me.TabSolverNS.Controls.Add(Me.tbNSMaximumDeltaT)
        Me.TabSolverNS.Controls.Add(Me.Label20)
        Me.TabSolverNS.Controls.Add(Me.tbNSNumericalDerivativeStep)
        resources.ApplyResources(Me.TabSolverNS, "TabSolverNS")
        Me.TabSolverNS.Name = "TabSolverNS"
        Me.TabSolverNS.UseVisualStyleBackColor = True
        '
        'chkNSJacobian
        '
        resources.ApplyResources(Me.chkNSJacobian, "chkNSJacobian")
        Me.chkNSJacobian.Name = "chkNSJacobian"
        Me.chkNSJacobian.UseVisualStyleBackColor = True
        '
        'chkNSUseNewton
        '
        resources.ApplyResources(Me.chkNSUseNewton, "chkNSUseNewton")
        Me.chkNSUseNewton.Name = "chkNSUseNewton"
        Me.chkNSUseNewton.UseVisualStyleBackColor = True
        '
        'chkNSUseDampingFactor
        '
        resources.ApplyResources(Me.chkNSUseDampingFactor, "chkNSUseDampingFactor")
        Me.chkNSUseDampingFactor.Name = "chkNSUseDampingFactor"
        Me.chkNSUseDampingFactor.UseVisualStyleBackColor = True
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        '
        'cbNSMaximumDeltaT
        '
        resources.ApplyResources(Me.cbNSMaximumDeltaT, "cbNSMaximumDeltaT")
        Me.cbNSMaximumDeltaT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbNSMaximumDeltaT.FormattingEnabled = True
        Me.cbNSMaximumDeltaT.Items.AddRange(New Object() {resources.GetString("cbNSMaximumDeltaT.Items"), resources.GetString("cbNSMaximumDeltaT.Items1"), resources.GetString("cbNSMaximumDeltaT.Items2")})
        Me.cbNSMaximumDeltaT.Name = "cbNSMaximumDeltaT"
        '
        'tbNSMaximumDeltaT
        '
        resources.ApplyResources(Me.tbNSMaximumDeltaT, "tbNSMaximumDeltaT")
        Me.tbNSMaximumDeltaT.Name = "tbNSMaximumDeltaT"
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        '
        'tbNSNumericalDerivativeStep
        '
        resources.ApplyResources(Me.tbNSNumericalDerivativeStep, "tbNSNumericalDerivativeStep")
        Me.tbNSNumericalDerivativeStep.Name = "tbNSNumericalDerivativeStep"
        '
        'TabSolverIO
        '
        Me.TabSolverIO.Controls.Add(Me.chkIOAverageKb)
        Me.TabSolverIO.Controls.Add(Me.chkIOAdjustSb)
        Me.TabSolverIO.Controls.Add(Me.tbIOMaxDamping)
        Me.TabSolverIO.Controls.Add(Me.tbIOMinDamping)
        Me.TabSolverIO.Controls.Add(Me.chkIOJacobian)
        Me.TabSolverIO.Controls.Add(Me.chkIONewton)
        Me.TabSolverIO.Controls.Add(Me.chkIOUseDampingFactor)
        Me.TabSolverIO.Controls.Add(Me.Label22)
        Me.TabSolverIO.Controls.Add(Me.cbIOTempPerturbationUnits)
        Me.TabSolverIO.Controls.Add(Me.tbIOTempPerturbation)
        Me.TabSolverIO.Controls.Add(Me.Label23)
        Me.TabSolverIO.Controls.Add(Me.tbIONumericalDerivativeStep)
        resources.ApplyResources(Me.TabSolverIO, "TabSolverIO")
        Me.TabSolverIO.Name = "TabSolverIO"
        Me.TabSolverIO.UseVisualStyleBackColor = True
        '
        'chkIOAverageKb
        '
        resources.ApplyResources(Me.chkIOAverageKb, "chkIOAverageKb")
        Me.chkIOAverageKb.Name = "chkIOAverageKb"
        Me.chkIOAverageKb.UseVisualStyleBackColor = True
        '
        'chkIOAdjustSb
        '
        resources.ApplyResources(Me.chkIOAdjustSb, "chkIOAdjustSb")
        Me.chkIOAdjustSb.Name = "chkIOAdjustSb"
        Me.chkIOAdjustSb.UseVisualStyleBackColor = True
        '
        'tbIOMaxDamping
        '
        resources.ApplyResources(Me.tbIOMaxDamping, "tbIOMaxDamping")
        Me.tbIOMaxDamping.Name = "tbIOMaxDamping"
        '
        'tbIOMinDamping
        '
        resources.ApplyResources(Me.tbIOMinDamping, "tbIOMinDamping")
        Me.tbIOMinDamping.Name = "tbIOMinDamping"
        '
        'chkIOJacobian
        '
        resources.ApplyResources(Me.chkIOJacobian, "chkIOJacobian")
        Me.chkIOJacobian.Name = "chkIOJacobian"
        Me.chkIOJacobian.UseVisualStyleBackColor = True
        '
        'chkIONewton
        '
        resources.ApplyResources(Me.chkIONewton, "chkIONewton")
        Me.chkIONewton.Name = "chkIONewton"
        Me.chkIONewton.UseVisualStyleBackColor = True
        '
        'chkIOUseDampingFactor
        '
        resources.ApplyResources(Me.chkIOUseDampingFactor, "chkIOUseDampingFactor")
        Me.chkIOUseDampingFactor.Name = "chkIOUseDampingFactor"
        Me.chkIOUseDampingFactor.UseVisualStyleBackColor = True
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        '
        'cbIOTempPerturbationUnits
        '
        resources.ApplyResources(Me.cbIOTempPerturbationUnits, "cbIOTempPerturbationUnits")
        Me.cbIOTempPerturbationUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbIOTempPerturbationUnits.FormattingEnabled = True
        Me.cbIOTempPerturbationUnits.Items.AddRange(New Object() {resources.GetString("cbIOTempPerturbationUnits.Items"), resources.GetString("cbIOTempPerturbationUnits.Items1"), resources.GetString("cbIOTempPerturbationUnits.Items2")})
        Me.cbIOTempPerturbationUnits.Name = "cbIOTempPerturbationUnits"
        '
        'tbIOTempPerturbation
        '
        resources.ApplyResources(Me.tbIOTempPerturbation, "tbIOTempPerturbation")
        Me.tbIOTempPerturbation.Name = "tbIOTempPerturbation"
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        '
        'tbIONumericalDerivativeStep
        '
        resources.ApplyResources(Me.tbIONumericalDerivativeStep, "tbIONumericalDerivativeStep")
        Me.tbIONumericalDerivativeStep.Name = "tbIONumericalDerivativeStep"
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.tabstrip1)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'tabstrip1
        '
        Me.tabstrip1.Controls.Add(Me.TabPage1)
        Me.tabstrip1.Controls.Add(Me.TabResults)
        resources.ApplyResources(Me.tabstrip1, "tabstrip1")
        Me.tabstrip1.Name = "tabstrip1"
        Me.tabstrip1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.gridResults)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'gridResults
        '
        Me.gridResults.AllowUserToAddRows = False
        Me.gridResults.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridResults.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.gridResults.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridResults.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.Column1})
        resources.ApplyResources(Me.gridResults, "gridResults")
        Me.gridResults.Name = "gridResults"
        Me.gridResults.ReadOnly = True
        Me.gridResults.RowHeadersVisible = False
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
        'TabResults
        '
        resources.ApplyResources(Me.TabResults, "TabResults")
        Me.TabResults.Name = "TabResults"
        Me.TabResults.UseVisualStyleBackColor = True
        '
        'EditingForm_Column
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox6)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox2)
        Me.Name = "EditingForm_Column"
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabGeneral.ResumeLayout(False)
        Me.TabGeneral.PerformLayout()
        Me.TabCondenser.ResumeLayout(False)
        Me.TabCondenser.PerformLayout()
        Me.TabReboiler.ResumeLayout(False)
        Me.TabReboiler.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.TabControl2.ResumeLayout(False)
        Me.TabInitialEstimates.ResumeLayout(False)
        Me.TabInitialEstimates.PerformLayout()
        Me.TabSolverBP.ResumeLayout(False)
        Me.TabSolverBP.PerformLayout()
        Me.TabSolverNS.ResumeLayout(False)
        Me.TabSolverNS.PerformLayout()
        Me.TabSolverIO.ResumeLayout(False)
        Me.TabSolverIO.PerformLayout()
        Me.GroupBox6.ResumeLayout(False)
        Me.tabstrip1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
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
    Friend WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
    Friend WithEvents btnConfigurePP As System.Windows.Forms.Button
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Friend WithEvents lblTag As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Friend WithEvents tabstrip1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents TabResults As System.Windows.Forms.TabPage
    Friend WithEvents gridResults As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents tbNStages As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabGeneral As System.Windows.Forms.TabPage
    Friend WithEvents TabCondenser As System.Windows.Forms.TabPage
    Friend WithEvents TabReboiler As System.Windows.Forms.TabPage
    Friend WithEvents cbAbsorberMode As System.Windows.Forms.ComboBox
    Friend WithEvents TabControl2 As System.Windows.Forms.TabControl
    Friend WithEvents TabConnections As System.Windows.Forms.TabPage
    Friend WithEvents TabStages As System.Windows.Forms.TabPage
    Friend WithEvents TabSolverBP As System.Windows.Forms.TabPage
    Friend WithEvents TabInitialEstimates As System.Windows.Forms.TabPage
    Friend WithEvents TabSolverNS As System.Windows.Forms.TabPage
    Friend WithEvents TabSolverIO As System.Windows.Forms.TabPage
    Friend WithEvents cbCondType As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents cbCondVapFlowUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbCondVapFlow As System.Windows.Forms.TextBox
    Friend WithEvents cbCondSpecUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbCondSpec As System.Windows.Forms.TextBox
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents cbCondSpec As System.Windows.Forms.ComboBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents cbCondPDropUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbCondPDrop As System.Windows.Forms.TextBox
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents cbCondPressureUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbCondPressure As System.Windows.Forms.TextBox
    Friend WithEvents cbRebSpecUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbRebSpecValue As System.Windows.Forms.TextBox
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents cbRebSpec As System.Windows.Forms.ComboBox
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents cbRebPressure As System.Windows.Forms.ComboBox
    Friend WithEvents tbRebPressure As System.Windows.Forms.TextBox
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents tbConvTol As System.Windows.Forms.TextBox
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents tbMaxIt As System.Windows.Forms.TextBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents tbBPStopAtIter As System.Windows.Forms.TextBox
    Friend WithEvents chkNSJacobian As System.Windows.Forms.CheckBox
    Friend WithEvents chkNSUseNewton As System.Windows.Forms.CheckBox
    Friend WithEvents chkNSUseDampingFactor As System.Windows.Forms.CheckBox
    Friend WithEvents Label21 As System.Windows.Forms.Label
    Friend WithEvents cbNSMaximumDeltaT As System.Windows.Forms.ComboBox
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents tbNSNumericalDerivativeStep As System.Windows.Forms.TextBox
    Friend WithEvents chkIOAverageKb As System.Windows.Forms.CheckBox
    Friend WithEvents chkIOAdjustSb As System.Windows.Forms.CheckBox
    Friend WithEvents tbIOMaxDamping As System.Windows.Forms.TextBox
    Friend WithEvents tbIOMinDamping As System.Windows.Forms.TextBox
    Friend WithEvents chkIOJacobian As System.Windows.Forms.CheckBox
    Friend WithEvents chkIONewton As System.Windows.Forms.CheckBox
    Friend WithEvents chkIOUseDampingFactor As System.Windows.Forms.CheckBox
    Friend WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents cbIOTempPerturbationUnits As System.Windows.Forms.ComboBox
    Friend WithEvents tbIOTempPerturbation As System.Windows.Forms.TextBox
    Friend WithEvents Label23 As System.Windows.Forms.Label
    Friend WithEvents tbIONumericalDerivativeStep As System.Windows.Forms.TextBox
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents chkUseIE_C As System.Windows.Forms.CheckBox
    Friend WithEvents chkUseIE_VF As System.Windows.Forms.CheckBox
    Friend WithEvents chkUseIE_LF As System.Windows.Forms.CheckBox
    Friend WithEvents chkUseIE_T As System.Windows.Forms.CheckBox
    Friend WithEvents tbNSMaximumDeltaT As System.Windows.Forms.TextBox
    Friend WithEvents cbSolvingMethod As System.Windows.Forms.ComboBox
    Friend WithEvents Label18 As System.Windows.Forms.Label
End Class
