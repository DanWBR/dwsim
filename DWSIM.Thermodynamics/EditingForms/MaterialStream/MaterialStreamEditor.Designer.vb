<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class MaterialStreamEditor

    Inherits SharedClasses.ObjectEditorForm

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MaterialStreamEditor))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle9 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle10 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle11 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle12 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle13 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle14 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle15 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle16 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle17 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle18 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.TabControlMain0 = New System.Windows.Forms.TabControl()
        Me.TabPageInputPane = New System.Windows.Forms.TabPage()
        Me.TabControl2 = New System.Windows.Forms.TabControl()
        Me.TabPageInputConditions = New System.Windows.Forms.TabPage()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.cbForcePhase = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tbFracSpec = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.cbUnitsM = New System.Windows.Forms.ComboBox()
        Me.tbMoleFlow = New System.Windows.Forms.TextBox()
        Me.cbSpec = New System.Windows.Forms.ComboBox()
        Me.tbVolFlow = New System.Windows.Forms.TextBox()
        Me.cbUnitsW = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbUnitsQ = New System.Windows.Forms.ComboBox()
        Me.tbMassFlow = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.cbUnitsP = New System.Windows.Forms.ComboBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbEnth = New System.Windows.Forms.TextBox()
        Me.tbPressure = New System.Windows.Forms.TextBox()
        Me.tbTemp = New System.Windows.Forms.TextBox()
        Me.cbUnitsH = New System.Windows.Forms.ComboBox()
        Me.cbUnitsS = New System.Windows.Forms.ComboBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbUnitsT = New System.Windows.Forms.ComboBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.tbEntr = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.TabPageInputComposition = New System.Windows.Forms.TabPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.btnCompAcceptChanges = New System.Windows.Forms.Button()
        Me.cbSolvent = New System.Windows.Forms.ComboBox()
        Me.btnEraseInput = New System.Windows.Forms.Button()
        Me.lblInputAmount = New System.Windows.Forms.Label()
        Me.btnEqualizeInput = New System.Windows.Forms.Button()
        Me.btnNormalizeInput = New System.Windows.Forms.Button()
        Me.lblSolvent = New System.Windows.Forms.Label()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.cbCompBasis = New System.Windows.Forms.ComboBox()
        Me.gridInputComposition = New System.Windows.Forms.DataGridView()
        Me.compname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.compamount = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPageResultsPane = New System.Windows.Forms.TabPage()
        Me.TabControlMain = New System.Windows.Forms.TabControl()
        Me.TabPageResultsComp = New System.Windows.Forms.TabPage()
        Me.TabControlCompound = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.gridPhaseTotal = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn30 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn31 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.chkShowAsPercentage = New System.Windows.Forms.CheckBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.TabPhaseComps = New System.Windows.Forms.TabControl()
        Me.tabCompMix = New System.Windows.Forms.TabPage()
        Me.gridCompMixture = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn18 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn19 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabCompVapor = New System.Windows.Forms.TabPage()
        Me.gridCompVapor = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn20 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn21 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabCompLiqMix = New System.Windows.Forms.TabPage()
        Me.gridCompLiqMix = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn22 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn23 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabCompLiq1 = New System.Windows.Forms.TabPage()
        Me.gridCompLiq1 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn24 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn25 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabCompLiq2 = New System.Windows.Forms.TabPage()
        Me.gridCompLiq2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn26 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn27 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabCompSolid = New System.Windows.Forms.TabPage()
        Me.gridCompSolid = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn28 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn29 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.cbCalculatedAmountsBasis = New System.Windows.Forms.ComboBox()
        Me.lblAmountTotal = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.lblCompPropUnits = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.TabCompoundPhaseProps = New System.Windows.Forms.TabControl()
        Me.TabCompPropVapor = New System.Windows.Forms.TabPage()
        Me.gridCompPropVapor = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn32 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn33 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabCompPropLiq1 = New System.Windows.Forms.TabPage()
        Me.gridCompPropLiq1 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn36 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn37 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabCompPropLiq2 = New System.Windows.Forms.TabPage()
        Me.gridCompPropLiq2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn38 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn39 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabCompPropSolid = New System.Windows.Forms.TabPage()
        Me.gridCompPropSolid = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn40 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn41 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.cbCompoundPhaseProperties = New System.Windows.Forms.ComboBox()
        Me.TabPageResultsProps = New System.Windows.Forms.TabPage()
        Me.TabPhaseProps = New System.Windows.Forms.TabControl()
        Me.tabPropsMix = New System.Windows.Forms.TabPage()
        Me.gridPropertiesMixture = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabPropsVapor = New System.Windows.Forms.TabPage()
        Me.gridPropertiesVapor = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabPropsLiqMix = New System.Windows.Forms.TabPage()
        Me.gridPropertiesLiqMix = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabPropsLiq1 = New System.Windows.Forms.TabPage()
        Me.gridPropertiesLiq1 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn10 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn11 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabPropsLiq2 = New System.Windows.Forms.TabPage()
        Me.gridPropertiesLiq2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn12 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn13 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn14 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tabPropsSolid = New System.Windows.Forms.TabPage()
        Me.gridPropertiesSolid = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn15 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn16 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn17 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabPageAnnotations = New System.Windows.Forms.TabPage()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.TabPageDynamics = New System.Windows.Forms.TabPage()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.cbDynSpec = New System.Windows.Forms.ComboBox()
        Me.TabPageFloatingTables = New System.Windows.Forms.TabPage()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.cbFloatingTableCompoundAmountBasis = New System.Windows.Forms.ComboBox()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblStatus = New System.Windows.Forms.TextBox()
        Me.btnUtils = New System.Windows.Forms.Button()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnDisconnectO = New System.Windows.Forms.Button()
        Me.btnDisconnectI = New System.Windows.Forms.Button()
        Me.cbOutlet = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbInlet = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.UtilitiesCtxMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.AddUtilityTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.DiagramaDeFasesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.BinaryTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.TernaryTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.PetroleumPropsTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.HydratesTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.TCPTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        Me.TabControlMain0.SuspendLayout()
        Me.TabPageInputPane.SuspendLayout()
        Me.TabControl2.SuspendLayout()
        Me.TabPageInputConditions.SuspendLayout()
        Me.TabPageInputComposition.SuspendLayout()
        CType(Me.gridInputComposition, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPageResultsPane.SuspendLayout()
        Me.TabControlMain.SuspendLayout()
        Me.TabPageResultsComp.SuspendLayout()
        Me.TabControlCompound.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.gridPhaseTotal, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPhaseComps.SuspendLayout()
        Me.tabCompMix.SuspendLayout()
        CType(Me.gridCompMixture, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabCompVapor.SuspendLayout()
        CType(Me.gridCompVapor, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabCompLiqMix.SuspendLayout()
        CType(Me.gridCompLiqMix, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabCompLiq1.SuspendLayout()
        CType(Me.gridCompLiq1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabCompLiq2.SuspendLayout()
        CType(Me.gridCompLiq2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabCompSolid.SuspendLayout()
        CType(Me.gridCompSolid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.TabCompoundPhaseProps.SuspendLayout()
        Me.TabCompPropVapor.SuspendLayout()
        CType(Me.gridCompPropVapor, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabCompPropLiq1.SuspendLayout()
        CType(Me.gridCompPropLiq1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabCompPropLiq2.SuspendLayout()
        CType(Me.gridCompPropLiq2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabCompPropSolid.SuspendLayout()
        CType(Me.gridCompPropSolid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPageResultsProps.SuspendLayout()
        Me.TabPhaseProps.SuspendLayout()
        Me.tabPropsMix.SuspendLayout()
        CType(Me.gridPropertiesMixture, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabPropsVapor.SuspendLayout()
        CType(Me.gridPropertiesVapor, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabPropsLiqMix.SuspendLayout()
        CType(Me.gridPropertiesLiqMix, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabPropsLiq1.SuspendLayout()
        CType(Me.gridPropertiesLiq1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabPropsLiq2.SuspendLayout()
        CType(Me.gridPropertiesLiq2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.tabPropsSolid.SuspendLayout()
        CType(Me.gridPropertiesSolid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPageAnnotations.SuspendLayout()
        Me.TabPageDynamics.SuspendLayout()
        Me.TabPageFloatingTables.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.UtilitiesCtxMenu.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.SuspendLayout()
        '
        'SplitContainer1
        '
        resources.ApplyResources(Me.SplitContainer1, "SplitContainer1")
        Me.SplitContainer1.Name = "SplitContainer1"
        '
        'SplitContainer1.Panel1
        '
        resources.ApplyResources(Me.SplitContainer1.Panel1, "SplitContainer1.Panel1")
        Me.SplitContainer1.Panel1.Controls.Add(Me.TabControlMain0)
        Me.ToolTipChangeTag.SetToolTip(Me.SplitContainer1.Panel1, resources.GetString("SplitContainer1.Panel1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.SplitContainer1.Panel1, resources.GetString("SplitContainer1.Panel1.ToolTip1"))
        '
        'SplitContainer1.Panel2
        '
        resources.ApplyResources(Me.SplitContainer1.Panel2, "SplitContainer1.Panel2")
        Me.ToolTipChangeTag.SetToolTip(Me.SplitContainer1.Panel2, resources.GetString("SplitContainer1.Panel2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.SplitContainer1.Panel2, resources.GetString("SplitContainer1.Panel2.ToolTip1"))
        Me.SplitContainer1.Panel2Collapsed = True
        Me.ToolTipValues.SetToolTip(Me.SplitContainer1, resources.GetString("SplitContainer1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.SplitContainer1, resources.GetString("SplitContainer1.ToolTip1"))
        '
        'TabControlMain0
        '
        resources.ApplyResources(Me.TabControlMain0, "TabControlMain0")
        Me.TabControlMain0.Controls.Add(Me.TabPageInputPane)
        Me.TabControlMain0.Controls.Add(Me.TabPageResultsPane)
        Me.TabControlMain0.Controls.Add(Me.TabPageAnnotations)
        Me.TabControlMain0.Controls.Add(Me.TabPageDynamics)
        Me.TabControlMain0.Controls.Add(Me.TabPageFloatingTables)
        Me.TabControlMain0.Name = "TabControlMain0"
        Me.TabControlMain0.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabControlMain0, resources.GetString("TabControlMain0.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabControlMain0, resources.GetString("TabControlMain0.ToolTip1"))
        '
        'TabPageInputPane
        '
        resources.ApplyResources(Me.TabPageInputPane, "TabPageInputPane")
        Me.TabPageInputPane.Controls.Add(Me.TabControl2)
        Me.TabPageInputPane.Name = "TabPageInputPane"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageInputPane, resources.GetString("TabPageInputPane.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageInputPane, resources.GetString("TabPageInputPane.ToolTip1"))
        Me.TabPageInputPane.UseVisualStyleBackColor = True
        '
        'TabControl2
        '
        resources.ApplyResources(Me.TabControl2, "TabControl2")
        Me.TabControl2.Controls.Add(Me.TabPageInputConditions)
        Me.TabControl2.Controls.Add(Me.TabPageInputComposition)
        Me.TabControl2.Name = "TabControl2"
        Me.TabControl2.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabControl2, resources.GetString("TabControl2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabControl2, resources.GetString("TabControl2.ToolTip1"))
        '
        'TabPageInputConditions
        '
        resources.ApplyResources(Me.TabPageInputConditions, "TabPageInputConditions")
        Me.TabPageInputConditions.Controls.Add(Me.Label22)
        Me.TabPageInputConditions.Controls.Add(Me.cbForcePhase)
        Me.TabPageInputConditions.Controls.Add(Me.Label10)
        Me.TabPageInputConditions.Controls.Add(Me.tbFracSpec)
        Me.TabPageInputConditions.Controls.Add(Me.Label8)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsM)
        Me.TabPageInputConditions.Controls.Add(Me.tbMoleFlow)
        Me.TabPageInputConditions.Controls.Add(Me.cbSpec)
        Me.TabPageInputConditions.Controls.Add(Me.tbVolFlow)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsW)
        Me.TabPageInputConditions.Controls.Add(Me.Label3)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsQ)
        Me.TabPageInputConditions.Controls.Add(Me.tbMassFlow)
        Me.TabPageInputConditions.Controls.Add(Me.Label4)
        Me.TabPageInputConditions.Controls.Add(Me.Label14)
        Me.TabPageInputConditions.Controls.Add(Me.Label17)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsP)
        Me.TabPageInputConditions.Controls.Add(Me.Label5)
        Me.TabPageInputConditions.Controls.Add(Me.tbEnth)
        Me.TabPageInputConditions.Controls.Add(Me.tbPressure)
        Me.TabPageInputConditions.Controls.Add(Me.tbTemp)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsH)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsS)
        Me.TabPageInputConditions.Controls.Add(Me.Label7)
        Me.TabPageInputConditions.Controls.Add(Me.cbUnitsT)
        Me.TabPageInputConditions.Controls.Add(Me.Label15)
        Me.TabPageInputConditions.Controls.Add(Me.tbEntr)
        Me.TabPageInputConditions.Controls.Add(Me.Label6)
        Me.TabPageInputConditions.Name = "TabPageInputConditions"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageInputConditions, resources.GetString("TabPageInputConditions.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageInputConditions, resources.GetString("TabPageInputConditions.ToolTip1"))
        Me.TabPageInputConditions.UseVisualStyleBackColor = True
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        Me.ToolTipValues.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip1"))
        '
        'cbForcePhase
        '
        resources.ApplyResources(Me.cbForcePhase, "cbForcePhase")
        Me.cbForcePhase.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbForcePhase.FormattingEnabled = True
        Me.cbForcePhase.Items.AddRange(New Object() {resources.GetString("cbForcePhase.Items"), resources.GetString("cbForcePhase.Items1"), resources.GetString("cbForcePhase.Items2"), resources.GetString("cbForcePhase.Items3")})
        Me.cbForcePhase.Name = "cbForcePhase"
        Me.ToolTipChangeTag.SetToolTip(Me.cbForcePhase, resources.GetString("cbForcePhase.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbForcePhase, resources.GetString("cbForcePhase.ToolTip1"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        '
        'tbFracSpec
        '
        resources.ApplyResources(Me.tbFracSpec, "tbFracSpec")
        Me.tbFracSpec.Name = "tbFracSpec"
        Me.ToolTipChangeTag.SetToolTip(Me.tbFracSpec, resources.GetString("tbFracSpec.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbFracSpec, resources.GetString("tbFracSpec.ToolTip1"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTipValues.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip1"))
        '
        'cbUnitsM
        '
        resources.ApplyResources(Me.cbUnitsM, "cbUnitsM")
        Me.cbUnitsM.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsM.FormattingEnabled = True
        Me.cbUnitsM.Items.AddRange(New Object() {resources.GetString("cbUnitsM.Items"), resources.GetString("cbUnitsM.Items1"), resources.GetString("cbUnitsM.Items2")})
        Me.cbUnitsM.Name = "cbUnitsM"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsM, resources.GetString("cbUnitsM.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsM, resources.GetString("cbUnitsM.ToolTip1"))
        '
        'tbMoleFlow
        '
        resources.ApplyResources(Me.tbMoleFlow, "tbMoleFlow")
        Me.tbMoleFlow.Name = "tbMoleFlow"
        Me.ToolTipChangeTag.SetToolTip(Me.tbMoleFlow, resources.GetString("tbMoleFlow.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbMoleFlow, resources.GetString("tbMoleFlow.ToolTip1"))
        '
        'cbSpec
        '
        resources.ApplyResources(Me.cbSpec, "cbSpec")
        Me.cbSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSpec.FormattingEnabled = True
        Me.cbSpec.Items.AddRange(New Object() {resources.GetString("cbSpec.Items"), resources.GetString("cbSpec.Items1"), resources.GetString("cbSpec.Items2"), resources.GetString("cbSpec.Items3"), resources.GetString("cbSpec.Items4")})
        Me.cbSpec.Name = "cbSpec"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSpec, resources.GetString("cbSpec.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSpec, resources.GetString("cbSpec.ToolTip1"))
        '
        'tbVolFlow
        '
        resources.ApplyResources(Me.tbVolFlow, "tbVolFlow")
        Me.tbVolFlow.Name = "tbVolFlow"
        Me.ToolTipChangeTag.SetToolTip(Me.tbVolFlow, resources.GetString("tbVolFlow.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbVolFlow, resources.GetString("tbVolFlow.ToolTip1"))
        '
        'cbUnitsW
        '
        resources.ApplyResources(Me.cbUnitsW, "cbUnitsW")
        Me.cbUnitsW.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsW.FormattingEnabled = True
        Me.cbUnitsW.Items.AddRange(New Object() {resources.GetString("cbUnitsW.Items"), resources.GetString("cbUnitsW.Items1"), resources.GetString("cbUnitsW.Items2")})
        Me.cbUnitsW.Name = "cbUnitsW"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsW, resources.GetString("cbUnitsW.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsW, resources.GetString("cbUnitsW.ToolTip1"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        '
        'cbUnitsQ
        '
        resources.ApplyResources(Me.cbUnitsQ, "cbUnitsQ")
        Me.cbUnitsQ.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsQ.FormattingEnabled = True
        Me.cbUnitsQ.Items.AddRange(New Object() {resources.GetString("cbUnitsQ.Items"), resources.GetString("cbUnitsQ.Items1"), resources.GetString("cbUnitsQ.Items2")})
        Me.cbUnitsQ.Name = "cbUnitsQ"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsQ, resources.GetString("cbUnitsQ.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsQ, resources.GetString("cbUnitsQ.ToolTip1"))
        '
        'tbMassFlow
        '
        resources.ApplyResources(Me.tbMassFlow, "tbMassFlow")
        Me.tbMassFlow.Name = "tbMassFlow"
        Me.ToolTipChangeTag.SetToolTip(Me.tbMassFlow, resources.GetString("tbMassFlow.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbMassFlow, resources.GetString("tbMassFlow.ToolTip1"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
        '
        'cbUnitsP
        '
        resources.ApplyResources(Me.cbUnitsP, "cbUnitsP")
        Me.cbUnitsP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsP.FormattingEnabled = True
        Me.cbUnitsP.Items.AddRange(New Object() {resources.GetString("cbUnitsP.Items"), resources.GetString("cbUnitsP.Items1"), resources.GetString("cbUnitsP.Items2")})
        Me.cbUnitsP.Name = "cbUnitsP"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsP, resources.GetString("cbUnitsP.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsP, resources.GetString("cbUnitsP.ToolTip1"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        '
        'tbEnth
        '
        resources.ApplyResources(Me.tbEnth, "tbEnth")
        Me.tbEnth.Name = "tbEnth"
        Me.ToolTipChangeTag.SetToolTip(Me.tbEnth, resources.GetString("tbEnth.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbEnth, resources.GetString("tbEnth.ToolTip1"))
        '
        'tbPressure
        '
        resources.ApplyResources(Me.tbPressure, "tbPressure")
        Me.tbPressure.Name = "tbPressure"
        Me.ToolTipChangeTag.SetToolTip(Me.tbPressure, resources.GetString("tbPressure.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbPressure, resources.GetString("tbPressure.ToolTip1"))
        '
        'tbTemp
        '
        resources.ApplyResources(Me.tbTemp, "tbTemp")
        Me.tbTemp.Name = "tbTemp"
        Me.ToolTipChangeTag.SetToolTip(Me.tbTemp, resources.GetString("tbTemp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbTemp, resources.GetString("tbTemp.ToolTip1"))
        '
        'cbUnitsH
        '
        resources.ApplyResources(Me.cbUnitsH, "cbUnitsH")
        Me.cbUnitsH.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsH.FormattingEnabled = True
        Me.cbUnitsH.Items.AddRange(New Object() {resources.GetString("cbUnitsH.Items"), resources.GetString("cbUnitsH.Items1"), resources.GetString("cbUnitsH.Items2")})
        Me.cbUnitsH.Name = "cbUnitsH"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsH, resources.GetString("cbUnitsH.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsH, resources.GetString("cbUnitsH.ToolTip1"))
        '
        'cbUnitsS
        '
        resources.ApplyResources(Me.cbUnitsS, "cbUnitsS")
        Me.cbUnitsS.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsS.FormattingEnabled = True
        Me.cbUnitsS.Items.AddRange(New Object() {resources.GetString("cbUnitsS.Items"), resources.GetString("cbUnitsS.Items1"), resources.GetString("cbUnitsS.Items2")})
        Me.cbUnitsS.Name = "cbUnitsS"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsS, resources.GetString("cbUnitsS.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsS, resources.GetString("cbUnitsS.ToolTip1"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        '
        'cbUnitsT
        '
        resources.ApplyResources(Me.cbUnitsT, "cbUnitsT")
        Me.cbUnitsT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsT.FormattingEnabled = True
        Me.cbUnitsT.Items.AddRange(New Object() {resources.GetString("cbUnitsT.Items"), resources.GetString("cbUnitsT.Items1"), resources.GetString("cbUnitsT.Items2")})
        Me.cbUnitsT.Name = "cbUnitsT"
        Me.ToolTipChangeTag.SetToolTip(Me.cbUnitsT, resources.GetString("cbUnitsT.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbUnitsT, resources.GetString("cbUnitsT.ToolTip1"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTipValues.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip1"))
        '
        'tbEntr
        '
        resources.ApplyResources(Me.tbEntr, "tbEntr")
        Me.tbEntr.Name = "tbEntr"
        Me.ToolTipChangeTag.SetToolTip(Me.tbEntr, resources.GetString("tbEntr.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbEntr, resources.GetString("tbEntr.ToolTip1"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        '
        'TabPageInputComposition
        '
        resources.ApplyResources(Me.TabPageInputComposition, "TabPageInputComposition")
        Me.TabPageInputComposition.Controls.Add(Me.Button1)
        Me.TabPageInputComposition.Controls.Add(Me.btnCompAcceptChanges)
        Me.TabPageInputComposition.Controls.Add(Me.cbSolvent)
        Me.TabPageInputComposition.Controls.Add(Me.btnEraseInput)
        Me.TabPageInputComposition.Controls.Add(Me.lblInputAmount)
        Me.TabPageInputComposition.Controls.Add(Me.btnEqualizeInput)
        Me.TabPageInputComposition.Controls.Add(Me.btnNormalizeInput)
        Me.TabPageInputComposition.Controls.Add(Me.lblSolvent)
        Me.TabPageInputComposition.Controls.Add(Me.Label16)
        Me.TabPageInputComposition.Controls.Add(Me.cbCompBasis)
        Me.TabPageInputComposition.Controls.Add(Me.gridInputComposition)
        Me.TabPageInputComposition.Name = "TabPageInputComposition"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageInputComposition, resources.GetString("TabPageInputComposition.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageInputComposition, resources.GetString("TabPageInputComposition.ToolTip1"))
        Me.TabPageInputComposition.UseVisualStyleBackColor = True
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.ToolTipValues.SetToolTip(Me.Button1, resources.GetString("Button1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Button1, resources.GetString("Button1.ToolTip1"))
        Me.Button1.UseVisualStyleBackColor = True
        '
        'btnCompAcceptChanges
        '
        resources.ApplyResources(Me.btnCompAcceptChanges, "btnCompAcceptChanges")
        Me.btnCompAcceptChanges.Name = "btnCompAcceptChanges"
        Me.ToolTipValues.SetToolTip(Me.btnCompAcceptChanges, resources.GetString("btnCompAcceptChanges.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCompAcceptChanges, resources.GetString("btnCompAcceptChanges.ToolTip1"))
        Me.btnCompAcceptChanges.UseVisualStyleBackColor = False
        '
        'cbSolvent
        '
        resources.ApplyResources(Me.cbSolvent, "cbSolvent")
        Me.cbSolvent.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSolvent.FormattingEnabled = True
        Me.cbSolvent.Items.AddRange(New Object() {resources.GetString("cbSolvent.Items"), resources.GetString("cbSolvent.Items1"), resources.GetString("cbSolvent.Items2"), resources.GetString("cbSolvent.Items3"), resources.GetString("cbSolvent.Items4"), resources.GetString("cbSolvent.Items5"), resources.GetString("cbSolvent.Items6")})
        Me.cbSolvent.Name = "cbSolvent"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSolvent, resources.GetString("cbSolvent.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSolvent, resources.GetString("cbSolvent.ToolTip1"))
        '
        'btnEraseInput
        '
        resources.ApplyResources(Me.btnEraseInput, "btnEraseInput")
        Me.btnEraseInput.Name = "btnEraseInput"
        Me.ToolTipValues.SetToolTip(Me.btnEraseInput, resources.GetString("btnEraseInput.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnEraseInput, resources.GetString("btnEraseInput.ToolTip1"))
        Me.btnEraseInput.UseVisualStyleBackColor = True
        '
        'lblInputAmount
        '
        resources.ApplyResources(Me.lblInputAmount, "lblInputAmount")
        Me.lblInputAmount.Name = "lblInputAmount"
        Me.ToolTipValues.SetToolTip(Me.lblInputAmount, resources.GetString("lblInputAmount.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblInputAmount, resources.GetString("lblInputAmount.ToolTip1"))
        '
        'btnEqualizeInput
        '
        resources.ApplyResources(Me.btnEqualizeInput, "btnEqualizeInput")
        Me.btnEqualizeInput.Name = "btnEqualizeInput"
        Me.ToolTipValues.SetToolTip(Me.btnEqualizeInput, resources.GetString("btnEqualizeInput.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnEqualizeInput, resources.GetString("btnEqualizeInput.ToolTip1"))
        Me.btnEqualizeInput.UseVisualStyleBackColor = True
        '
        'btnNormalizeInput
        '
        resources.ApplyResources(Me.btnNormalizeInput, "btnNormalizeInput")
        Me.btnNormalizeInput.Name = "btnNormalizeInput"
        Me.ToolTipValues.SetToolTip(Me.btnNormalizeInput, resources.GetString("btnNormalizeInput.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnNormalizeInput, resources.GetString("btnNormalizeInput.ToolTip1"))
        Me.btnNormalizeInput.UseVisualStyleBackColor = True
        '
        'lblSolvent
        '
        resources.ApplyResources(Me.lblSolvent, "lblSolvent")
        Me.lblSolvent.Name = "lblSolvent"
        Me.ToolTipValues.SetToolTip(Me.lblSolvent, resources.GetString("lblSolvent.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblSolvent, resources.GetString("lblSolvent.ToolTip1"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        '
        'cbCompBasis
        '
        resources.ApplyResources(Me.cbCompBasis, "cbCompBasis")
        Me.cbCompBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCompBasis.FormattingEnabled = True
        Me.cbCompBasis.Items.AddRange(New Object() {resources.GetString("cbCompBasis.Items"), resources.GetString("cbCompBasis.Items1"), resources.GetString("cbCompBasis.Items2"), resources.GetString("cbCompBasis.Items3"), resources.GetString("cbCompBasis.Items4"), resources.GetString("cbCompBasis.Items5"), resources.GetString("cbCompBasis.Items6")})
        Me.cbCompBasis.Name = "cbCompBasis"
        Me.ToolTipChangeTag.SetToolTip(Me.cbCompBasis, resources.GetString("cbCompBasis.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCompBasis, resources.GetString("cbCompBasis.ToolTip1"))
        '
        'gridInputComposition
        '
        resources.ApplyResources(Me.gridInputComposition, "gridInputComposition")
        Me.gridInputComposition.AllowUserToAddRows = False
        Me.gridInputComposition.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridInputComposition.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridInputComposition.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.compname, Me.compamount})
        Me.gridInputComposition.Name = "gridInputComposition"
        Me.gridInputComposition.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridInputComposition, resources.GetString("gridInputComposition.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridInputComposition, resources.GetString("gridInputComposition.ToolTip1"))
        '
        'compname
        '
        Me.compname.FillWeight = 50.0!
        resources.ApplyResources(Me.compname, "compname")
        Me.compname.Name = "compname"
        Me.compname.ReadOnly = True
        '
        'compamount
        '
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.compamount.DefaultCellStyle = DataGridViewCellStyle1
        Me.compamount.FillWeight = 50.0!
        resources.ApplyResources(Me.compamount, "compamount")
        Me.compamount.Name = "compamount"
        '
        'TabPageResultsPane
        '
        resources.ApplyResources(Me.TabPageResultsPane, "TabPageResultsPane")
        Me.TabPageResultsPane.Controls.Add(Me.TabControlMain)
        Me.TabPageResultsPane.Name = "TabPageResultsPane"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageResultsPane, resources.GetString("TabPageResultsPane.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageResultsPane, resources.GetString("TabPageResultsPane.ToolTip1"))
        Me.TabPageResultsPane.UseVisualStyleBackColor = True
        '
        'TabControlMain
        '
        resources.ApplyResources(Me.TabControlMain, "TabControlMain")
        Me.TabControlMain.Controls.Add(Me.TabPageResultsComp)
        Me.TabControlMain.Controls.Add(Me.TabPageResultsProps)
        Me.TabControlMain.Name = "TabControlMain"
        Me.TabControlMain.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabControlMain, resources.GetString("TabControlMain.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabControlMain, resources.GetString("TabControlMain.ToolTip1"))
        '
        'TabPageResultsComp
        '
        resources.ApplyResources(Me.TabPageResultsComp, "TabPageResultsComp")
        Me.TabPageResultsComp.Controls.Add(Me.TabControlCompound)
        Me.TabPageResultsComp.Name = "TabPageResultsComp"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageResultsComp, resources.GetString("TabPageResultsComp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageResultsComp, resources.GetString("TabPageResultsComp.ToolTip1"))
        Me.TabPageResultsComp.UseVisualStyleBackColor = True
        '
        'TabControlCompound
        '
        resources.ApplyResources(Me.TabControlCompound, "TabControlCompound")
        Me.TabControlCompound.Controls.Add(Me.TabPage1)
        Me.TabControlCompound.Controls.Add(Me.TabPage2)
        Me.TabControlCompound.Name = "TabControlCompound"
        Me.TabControlCompound.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabControlCompound, resources.GetString("TabControlCompound.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabControlCompound, resources.GetString("TabControlCompound.ToolTip1"))
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.gridPhaseTotal)
        Me.TabPage1.Controls.Add(Me.chkShowAsPercentage)
        Me.TabPage1.Controls.Add(Me.Label19)
        Me.TabPage1.Controls.Add(Me.TabPhaseComps)
        Me.TabPage1.Controls.Add(Me.cbCalculatedAmountsBasis)
        Me.TabPage1.Controls.Add(Me.lblAmountTotal)
        Me.TabPage1.Name = "TabPage1"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage1, resources.GetString("TabPage1.ToolTip1"))
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'gridPhaseTotal
        '
        resources.ApplyResources(Me.gridPhaseTotal, "gridPhaseTotal")
        Me.gridPhaseTotal.AllowUserToAddRows = False
        Me.gridPhaseTotal.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPhaseTotal.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPhaseTotal.ColumnHeadersVisible = False
        Me.gridPhaseTotal.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn30, Me.DataGridViewTextBoxColumn31})
        Me.gridPhaseTotal.Name = "gridPhaseTotal"
        Me.gridPhaseTotal.ReadOnly = True
        Me.gridPhaseTotal.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPhaseTotal, resources.GetString("gridPhaseTotal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPhaseTotal, resources.GetString("gridPhaseTotal.ToolTip1"))
        '
        'DataGridViewTextBoxColumn30
        '
        Me.DataGridViewTextBoxColumn30.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn30, "DataGridViewTextBoxColumn30")
        Me.DataGridViewTextBoxColumn30.Name = "DataGridViewTextBoxColumn30"
        Me.DataGridViewTextBoxColumn30.ReadOnly = True
        '
        'DataGridViewTextBoxColumn31
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn31.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridViewTextBoxColumn31.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn31, "DataGridViewTextBoxColumn31")
        Me.DataGridViewTextBoxColumn31.Name = "DataGridViewTextBoxColumn31"
        Me.DataGridViewTextBoxColumn31.ReadOnly = True
        '
        'chkShowAsPercentage
        '
        resources.ApplyResources(Me.chkShowAsPercentage, "chkShowAsPercentage")
        Me.chkShowAsPercentage.Name = "chkShowAsPercentage"
        Me.ToolTipValues.SetToolTip(Me.chkShowAsPercentage, resources.GetString("chkShowAsPercentage.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkShowAsPercentage, resources.GetString("chkShowAsPercentage.ToolTip1"))
        Me.chkShowAsPercentage.UseVisualStyleBackColor = True
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTipValues.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip1"))
        '
        'TabPhaseComps
        '
        resources.ApplyResources(Me.TabPhaseComps, "TabPhaseComps")
        Me.TabPhaseComps.Controls.Add(Me.tabCompMix)
        Me.TabPhaseComps.Controls.Add(Me.tabCompVapor)
        Me.TabPhaseComps.Controls.Add(Me.tabCompLiqMix)
        Me.TabPhaseComps.Controls.Add(Me.tabCompLiq1)
        Me.TabPhaseComps.Controls.Add(Me.tabCompLiq2)
        Me.TabPhaseComps.Controls.Add(Me.tabCompSolid)
        Me.TabPhaseComps.Name = "TabPhaseComps"
        Me.TabPhaseComps.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabPhaseComps, resources.GetString("TabPhaseComps.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPhaseComps, resources.GetString("TabPhaseComps.ToolTip1"))
        '
        'tabCompMix
        '
        resources.ApplyResources(Me.tabCompMix, "tabCompMix")
        Me.tabCompMix.Controls.Add(Me.gridCompMixture)
        Me.tabCompMix.Name = "tabCompMix"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompMix, resources.GetString("tabCompMix.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompMix, resources.GetString("tabCompMix.ToolTip1"))
        Me.tabCompMix.UseVisualStyleBackColor = True
        '
        'gridCompMixture
        '
        resources.ApplyResources(Me.gridCompMixture, "gridCompMixture")
        Me.gridCompMixture.AllowUserToAddRows = False
        Me.gridCompMixture.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompMixture.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompMixture.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn18, Me.DataGridViewTextBoxColumn19})
        Me.gridCompMixture.Name = "gridCompMixture"
        Me.gridCompMixture.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompMixture, resources.GetString("gridCompMixture.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompMixture, resources.GetString("gridCompMixture.ToolTip1"))
        '
        'DataGridViewTextBoxColumn18
        '
        Me.DataGridViewTextBoxColumn18.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn18, "DataGridViewTextBoxColumn18")
        Me.DataGridViewTextBoxColumn18.Name = "DataGridViewTextBoxColumn18"
        '
        'DataGridViewTextBoxColumn19
        '
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn19.DefaultCellStyle = DataGridViewCellStyle3
        Me.DataGridViewTextBoxColumn19.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn19, "DataGridViewTextBoxColumn19")
        Me.DataGridViewTextBoxColumn19.Name = "DataGridViewTextBoxColumn19"
        '
        'tabCompVapor
        '
        resources.ApplyResources(Me.tabCompVapor, "tabCompVapor")
        Me.tabCompVapor.Controls.Add(Me.gridCompVapor)
        Me.tabCompVapor.Name = "tabCompVapor"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompVapor, resources.GetString("tabCompVapor.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompVapor, resources.GetString("tabCompVapor.ToolTip1"))
        Me.tabCompVapor.UseVisualStyleBackColor = True
        '
        'gridCompVapor
        '
        resources.ApplyResources(Me.gridCompVapor, "gridCompVapor")
        Me.gridCompVapor.AllowUserToAddRows = False
        Me.gridCompVapor.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompVapor.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompVapor.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn20, Me.DataGridViewTextBoxColumn21})
        Me.gridCompVapor.Name = "gridCompVapor"
        Me.gridCompVapor.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompVapor, resources.GetString("gridCompVapor.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompVapor, resources.GetString("gridCompVapor.ToolTip1"))
        '
        'DataGridViewTextBoxColumn20
        '
        Me.DataGridViewTextBoxColumn20.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn20, "DataGridViewTextBoxColumn20")
        Me.DataGridViewTextBoxColumn20.Name = "DataGridViewTextBoxColumn20"
        '
        'DataGridViewTextBoxColumn21
        '
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn21.DefaultCellStyle = DataGridViewCellStyle4
        Me.DataGridViewTextBoxColumn21.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn21, "DataGridViewTextBoxColumn21")
        Me.DataGridViewTextBoxColumn21.Name = "DataGridViewTextBoxColumn21"
        '
        'tabCompLiqMix
        '
        resources.ApplyResources(Me.tabCompLiqMix, "tabCompLiqMix")
        Me.tabCompLiqMix.Controls.Add(Me.gridCompLiqMix)
        Me.tabCompLiqMix.Name = "tabCompLiqMix"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompLiqMix, resources.GetString("tabCompLiqMix.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompLiqMix, resources.GetString("tabCompLiqMix.ToolTip1"))
        Me.tabCompLiqMix.UseVisualStyleBackColor = True
        '
        'gridCompLiqMix
        '
        resources.ApplyResources(Me.gridCompLiqMix, "gridCompLiqMix")
        Me.gridCompLiqMix.AllowUserToAddRows = False
        Me.gridCompLiqMix.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiqMix.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiqMix.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn22, Me.DataGridViewTextBoxColumn23})
        Me.gridCompLiqMix.Name = "gridCompLiqMix"
        Me.gridCompLiqMix.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompLiqMix, resources.GetString("gridCompLiqMix.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompLiqMix, resources.GetString("gridCompLiqMix.ToolTip1"))
        '
        'DataGridViewTextBoxColumn22
        '
        Me.DataGridViewTextBoxColumn22.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn22, "DataGridViewTextBoxColumn22")
        Me.DataGridViewTextBoxColumn22.Name = "DataGridViewTextBoxColumn22"
        '
        'DataGridViewTextBoxColumn23
        '
        DataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn23.DefaultCellStyle = DataGridViewCellStyle5
        Me.DataGridViewTextBoxColumn23.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn23, "DataGridViewTextBoxColumn23")
        Me.DataGridViewTextBoxColumn23.Name = "DataGridViewTextBoxColumn23"
        '
        'tabCompLiq1
        '
        resources.ApplyResources(Me.tabCompLiq1, "tabCompLiq1")
        Me.tabCompLiq1.Controls.Add(Me.gridCompLiq1)
        Me.tabCompLiq1.Name = "tabCompLiq1"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompLiq1, resources.GetString("tabCompLiq1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompLiq1, resources.GetString("tabCompLiq1.ToolTip1"))
        Me.tabCompLiq1.UseVisualStyleBackColor = True
        '
        'gridCompLiq1
        '
        resources.ApplyResources(Me.gridCompLiq1, "gridCompLiq1")
        Me.gridCompLiq1.AllowUserToAddRows = False
        Me.gridCompLiq1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiq1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiq1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn24, Me.DataGridViewTextBoxColumn25})
        Me.gridCompLiq1.Name = "gridCompLiq1"
        Me.gridCompLiq1.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompLiq1, resources.GetString("gridCompLiq1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompLiq1, resources.GetString("gridCompLiq1.ToolTip1"))
        '
        'DataGridViewTextBoxColumn24
        '
        Me.DataGridViewTextBoxColumn24.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn24, "DataGridViewTextBoxColumn24")
        Me.DataGridViewTextBoxColumn24.Name = "DataGridViewTextBoxColumn24"
        '
        'DataGridViewTextBoxColumn25
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn25.DefaultCellStyle = DataGridViewCellStyle6
        Me.DataGridViewTextBoxColumn25.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn25, "DataGridViewTextBoxColumn25")
        Me.DataGridViewTextBoxColumn25.Name = "DataGridViewTextBoxColumn25"
        '
        'tabCompLiq2
        '
        resources.ApplyResources(Me.tabCompLiq2, "tabCompLiq2")
        Me.tabCompLiq2.Controls.Add(Me.gridCompLiq2)
        Me.tabCompLiq2.Name = "tabCompLiq2"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompLiq2, resources.GetString("tabCompLiq2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompLiq2, resources.GetString("tabCompLiq2.ToolTip1"))
        Me.tabCompLiq2.UseVisualStyleBackColor = True
        '
        'gridCompLiq2
        '
        resources.ApplyResources(Me.gridCompLiq2, "gridCompLiq2")
        Me.gridCompLiq2.AllowUserToAddRows = False
        Me.gridCompLiq2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiq2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiq2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn26, Me.DataGridViewTextBoxColumn27})
        Me.gridCompLiq2.Name = "gridCompLiq2"
        Me.gridCompLiq2.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompLiq2, resources.GetString("gridCompLiq2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompLiq2, resources.GetString("gridCompLiq2.ToolTip1"))
        '
        'DataGridViewTextBoxColumn26
        '
        Me.DataGridViewTextBoxColumn26.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn26, "DataGridViewTextBoxColumn26")
        Me.DataGridViewTextBoxColumn26.Name = "DataGridViewTextBoxColumn26"
        '
        'DataGridViewTextBoxColumn27
        '
        DataGridViewCellStyle7.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn27.DefaultCellStyle = DataGridViewCellStyle7
        Me.DataGridViewTextBoxColumn27.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn27, "DataGridViewTextBoxColumn27")
        Me.DataGridViewTextBoxColumn27.Name = "DataGridViewTextBoxColumn27"
        '
        'tabCompSolid
        '
        resources.ApplyResources(Me.tabCompSolid, "tabCompSolid")
        Me.tabCompSolid.Controls.Add(Me.gridCompSolid)
        Me.tabCompSolid.Name = "tabCompSolid"
        Me.ToolTipChangeTag.SetToolTip(Me.tabCompSolid, resources.GetString("tabCompSolid.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabCompSolid, resources.GetString("tabCompSolid.ToolTip1"))
        Me.tabCompSolid.UseVisualStyleBackColor = True
        '
        'gridCompSolid
        '
        resources.ApplyResources(Me.gridCompSolid, "gridCompSolid")
        Me.gridCompSolid.AllowUserToAddRows = False
        Me.gridCompSolid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompSolid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompSolid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn28, Me.DataGridViewTextBoxColumn29})
        Me.gridCompSolid.Name = "gridCompSolid"
        Me.gridCompSolid.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompSolid, resources.GetString("gridCompSolid.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompSolid, resources.GetString("gridCompSolid.ToolTip1"))
        '
        'DataGridViewTextBoxColumn28
        '
        Me.DataGridViewTextBoxColumn28.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn28, "DataGridViewTextBoxColumn28")
        Me.DataGridViewTextBoxColumn28.Name = "DataGridViewTextBoxColumn28"
        '
        'DataGridViewTextBoxColumn29
        '
        DataGridViewCellStyle8.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn29.DefaultCellStyle = DataGridViewCellStyle8
        Me.DataGridViewTextBoxColumn29.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn29, "DataGridViewTextBoxColumn29")
        Me.DataGridViewTextBoxColumn29.Name = "DataGridViewTextBoxColumn29"
        '
        'cbCalculatedAmountsBasis
        '
        resources.ApplyResources(Me.cbCalculatedAmountsBasis, "cbCalculatedAmountsBasis")
        Me.cbCalculatedAmountsBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalculatedAmountsBasis.FormattingEnabled = True
        Me.cbCalculatedAmountsBasis.Items.AddRange(New Object() {resources.GetString("cbCalculatedAmountsBasis.Items"), resources.GetString("cbCalculatedAmountsBasis.Items1"), resources.GetString("cbCalculatedAmountsBasis.Items2"), resources.GetString("cbCalculatedAmountsBasis.Items3"), resources.GetString("cbCalculatedAmountsBasis.Items4"), resources.GetString("cbCalculatedAmountsBasis.Items5"), resources.GetString("cbCalculatedAmountsBasis.Items6")})
        Me.cbCalculatedAmountsBasis.Name = "cbCalculatedAmountsBasis"
        Me.ToolTipChangeTag.SetToolTip(Me.cbCalculatedAmountsBasis, resources.GetString("cbCalculatedAmountsBasis.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCalculatedAmountsBasis, resources.GetString("cbCalculatedAmountsBasis.ToolTip1"))
        '
        'lblAmountTotal
        '
        resources.ApplyResources(Me.lblAmountTotal, "lblAmountTotal")
        Me.lblAmountTotal.Name = "lblAmountTotal"
        Me.ToolTipValues.SetToolTip(Me.lblAmountTotal, resources.GetString("lblAmountTotal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblAmountTotal, resources.GetString("lblAmountTotal.ToolTip1"))
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.lblCompPropUnits)
        Me.TabPage2.Controls.Add(Me.Label18)
        Me.TabPage2.Controls.Add(Me.TabCompoundPhaseProps)
        Me.TabPage2.Controls.Add(Me.cbCompoundPhaseProperties)
        Me.TabPage2.Name = "TabPage2"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage2, resources.GetString("TabPage2.ToolTip1"))
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'lblCompPropUnits
        '
        resources.ApplyResources(Me.lblCompPropUnits, "lblCompPropUnits")
        Me.lblCompPropUnits.Name = "lblCompPropUnits"
        Me.ToolTipValues.SetToolTip(Me.lblCompPropUnits, resources.GetString("lblCompPropUnits.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblCompPropUnits, resources.GetString("lblCompPropUnits.ToolTip1"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        '
        'TabCompoundPhaseProps
        '
        resources.ApplyResources(Me.TabCompoundPhaseProps, "TabCompoundPhaseProps")
        Me.TabCompoundPhaseProps.Controls.Add(Me.TabCompPropVapor)
        Me.TabCompoundPhaseProps.Controls.Add(Me.TabCompPropLiq1)
        Me.TabCompoundPhaseProps.Controls.Add(Me.TabCompPropLiq2)
        Me.TabCompoundPhaseProps.Controls.Add(Me.TabCompPropSolid)
        Me.TabCompoundPhaseProps.Name = "TabCompoundPhaseProps"
        Me.TabCompoundPhaseProps.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabCompoundPhaseProps, resources.GetString("TabCompoundPhaseProps.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabCompoundPhaseProps, resources.GetString("TabCompoundPhaseProps.ToolTip1"))
        '
        'TabCompPropVapor
        '
        resources.ApplyResources(Me.TabCompPropVapor, "TabCompPropVapor")
        Me.TabCompPropVapor.Controls.Add(Me.gridCompPropVapor)
        Me.TabCompPropVapor.Name = "TabCompPropVapor"
        Me.ToolTipChangeTag.SetToolTip(Me.TabCompPropVapor, resources.GetString("TabCompPropVapor.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCompPropVapor, resources.GetString("TabCompPropVapor.ToolTip1"))
        Me.TabCompPropVapor.UseVisualStyleBackColor = True
        '
        'gridCompPropVapor
        '
        resources.ApplyResources(Me.gridCompPropVapor, "gridCompPropVapor")
        Me.gridCompPropVapor.AllowUserToAddRows = False
        Me.gridCompPropVapor.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompPropVapor.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompPropVapor.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn32, Me.DataGridViewTextBoxColumn33})
        Me.gridCompPropVapor.Name = "gridCompPropVapor"
        Me.gridCompPropVapor.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompPropVapor, resources.GetString("gridCompPropVapor.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompPropVapor, resources.GetString("gridCompPropVapor.ToolTip1"))
        '
        'DataGridViewTextBoxColumn32
        '
        Me.DataGridViewTextBoxColumn32.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn32, "DataGridViewTextBoxColumn32")
        Me.DataGridViewTextBoxColumn32.Name = "DataGridViewTextBoxColumn32"
        '
        'DataGridViewTextBoxColumn33
        '
        DataGridViewCellStyle9.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn33.DefaultCellStyle = DataGridViewCellStyle9
        Me.DataGridViewTextBoxColumn33.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn33, "DataGridViewTextBoxColumn33")
        Me.DataGridViewTextBoxColumn33.Name = "DataGridViewTextBoxColumn33"
        '
        'TabCompPropLiq1
        '
        resources.ApplyResources(Me.TabCompPropLiq1, "TabCompPropLiq1")
        Me.TabCompPropLiq1.Controls.Add(Me.gridCompPropLiq1)
        Me.TabCompPropLiq1.Name = "TabCompPropLiq1"
        Me.ToolTipChangeTag.SetToolTip(Me.TabCompPropLiq1, resources.GetString("TabCompPropLiq1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCompPropLiq1, resources.GetString("TabCompPropLiq1.ToolTip1"))
        Me.TabCompPropLiq1.UseVisualStyleBackColor = True
        '
        'gridCompPropLiq1
        '
        resources.ApplyResources(Me.gridCompPropLiq1, "gridCompPropLiq1")
        Me.gridCompPropLiq1.AllowUserToAddRows = False
        Me.gridCompPropLiq1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompPropLiq1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompPropLiq1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn36, Me.DataGridViewTextBoxColumn37})
        Me.gridCompPropLiq1.Name = "gridCompPropLiq1"
        Me.gridCompPropLiq1.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompPropLiq1, resources.GetString("gridCompPropLiq1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompPropLiq1, resources.GetString("gridCompPropLiq1.ToolTip1"))
        '
        'DataGridViewTextBoxColumn36
        '
        Me.DataGridViewTextBoxColumn36.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn36, "DataGridViewTextBoxColumn36")
        Me.DataGridViewTextBoxColumn36.Name = "DataGridViewTextBoxColumn36"
        '
        'DataGridViewTextBoxColumn37
        '
        DataGridViewCellStyle10.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn37.DefaultCellStyle = DataGridViewCellStyle10
        Me.DataGridViewTextBoxColumn37.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn37, "DataGridViewTextBoxColumn37")
        Me.DataGridViewTextBoxColumn37.Name = "DataGridViewTextBoxColumn37"
        '
        'TabCompPropLiq2
        '
        resources.ApplyResources(Me.TabCompPropLiq2, "TabCompPropLiq2")
        Me.TabCompPropLiq2.Controls.Add(Me.gridCompPropLiq2)
        Me.TabCompPropLiq2.Name = "TabCompPropLiq2"
        Me.ToolTipChangeTag.SetToolTip(Me.TabCompPropLiq2, resources.GetString("TabCompPropLiq2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCompPropLiq2, resources.GetString("TabCompPropLiq2.ToolTip1"))
        Me.TabCompPropLiq2.UseVisualStyleBackColor = True
        '
        'gridCompPropLiq2
        '
        resources.ApplyResources(Me.gridCompPropLiq2, "gridCompPropLiq2")
        Me.gridCompPropLiq2.AllowUserToAddRows = False
        Me.gridCompPropLiq2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompPropLiq2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompPropLiq2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn38, Me.DataGridViewTextBoxColumn39})
        Me.gridCompPropLiq2.Name = "gridCompPropLiq2"
        Me.gridCompPropLiq2.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompPropLiq2, resources.GetString("gridCompPropLiq2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompPropLiq2, resources.GetString("gridCompPropLiq2.ToolTip1"))
        '
        'DataGridViewTextBoxColumn38
        '
        Me.DataGridViewTextBoxColumn38.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn38, "DataGridViewTextBoxColumn38")
        Me.DataGridViewTextBoxColumn38.Name = "DataGridViewTextBoxColumn38"
        '
        'DataGridViewTextBoxColumn39
        '
        DataGridViewCellStyle11.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn39.DefaultCellStyle = DataGridViewCellStyle11
        Me.DataGridViewTextBoxColumn39.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn39, "DataGridViewTextBoxColumn39")
        Me.DataGridViewTextBoxColumn39.Name = "DataGridViewTextBoxColumn39"
        '
        'TabCompPropSolid
        '
        resources.ApplyResources(Me.TabCompPropSolid, "TabCompPropSolid")
        Me.TabCompPropSolid.Controls.Add(Me.gridCompPropSolid)
        Me.TabCompPropSolid.Name = "TabCompPropSolid"
        Me.ToolTipChangeTag.SetToolTip(Me.TabCompPropSolid, resources.GetString("TabCompPropSolid.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabCompPropSolid, resources.GetString("TabCompPropSolid.ToolTip1"))
        Me.TabCompPropSolid.UseVisualStyleBackColor = True
        '
        'gridCompPropSolid
        '
        resources.ApplyResources(Me.gridCompPropSolid, "gridCompPropSolid")
        Me.gridCompPropSolid.AllowUserToAddRows = False
        Me.gridCompPropSolid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompPropSolid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompPropSolid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn40, Me.DataGridViewTextBoxColumn41})
        Me.gridCompPropSolid.Name = "gridCompPropSolid"
        Me.gridCompPropSolid.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridCompPropSolid, resources.GetString("gridCompPropSolid.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridCompPropSolid, resources.GetString("gridCompPropSolid.ToolTip1"))
        '
        'DataGridViewTextBoxColumn40
        '
        Me.DataGridViewTextBoxColumn40.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn40, "DataGridViewTextBoxColumn40")
        Me.DataGridViewTextBoxColumn40.Name = "DataGridViewTextBoxColumn40"
        '
        'DataGridViewTextBoxColumn41
        '
        DataGridViewCellStyle12.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn41.DefaultCellStyle = DataGridViewCellStyle12
        Me.DataGridViewTextBoxColumn41.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn41, "DataGridViewTextBoxColumn41")
        Me.DataGridViewTextBoxColumn41.Name = "DataGridViewTextBoxColumn41"
        '
        'cbCompoundPhaseProperties
        '
        resources.ApplyResources(Me.cbCompoundPhaseProperties, "cbCompoundPhaseProperties")
        Me.cbCompoundPhaseProperties.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCompoundPhaseProperties.FormattingEnabled = True
        Me.cbCompoundPhaseProperties.Items.AddRange(New Object() {resources.GetString("cbCompoundPhaseProperties.Items"), resources.GetString("cbCompoundPhaseProperties.Items1"), resources.GetString("cbCompoundPhaseProperties.Items2"), resources.GetString("cbCompoundPhaseProperties.Items3"), resources.GetString("cbCompoundPhaseProperties.Items4"), resources.GetString("cbCompoundPhaseProperties.Items5")})
        Me.cbCompoundPhaseProperties.Name = "cbCompoundPhaseProperties"
        Me.ToolTipChangeTag.SetToolTip(Me.cbCompoundPhaseProperties, resources.GetString("cbCompoundPhaseProperties.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbCompoundPhaseProperties, resources.GetString("cbCompoundPhaseProperties.ToolTip1"))
        '
        'TabPageResultsProps
        '
        resources.ApplyResources(Me.TabPageResultsProps, "TabPageResultsProps")
        Me.TabPageResultsProps.Controls.Add(Me.TabPhaseProps)
        Me.TabPageResultsProps.Name = "TabPageResultsProps"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageResultsProps, resources.GetString("TabPageResultsProps.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageResultsProps, resources.GetString("TabPageResultsProps.ToolTip1"))
        Me.TabPageResultsProps.UseVisualStyleBackColor = True
        '
        'TabPhaseProps
        '
        resources.ApplyResources(Me.TabPhaseProps, "TabPhaseProps")
        Me.TabPhaseProps.Controls.Add(Me.tabPropsMix)
        Me.TabPhaseProps.Controls.Add(Me.tabPropsVapor)
        Me.TabPhaseProps.Controls.Add(Me.tabPropsLiqMix)
        Me.TabPhaseProps.Controls.Add(Me.tabPropsLiq1)
        Me.TabPhaseProps.Controls.Add(Me.tabPropsLiq2)
        Me.TabPhaseProps.Controls.Add(Me.tabPropsSolid)
        Me.TabPhaseProps.Name = "TabPhaseProps"
        Me.TabPhaseProps.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabPhaseProps, resources.GetString("TabPhaseProps.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabPhaseProps, resources.GetString("TabPhaseProps.ToolTip1"))
        '
        'tabPropsMix
        '
        resources.ApplyResources(Me.tabPropsMix, "tabPropsMix")
        Me.tabPropsMix.Controls.Add(Me.gridPropertiesMixture)
        Me.tabPropsMix.Name = "tabPropsMix"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsMix, resources.GetString("tabPropsMix.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsMix, resources.GetString("tabPropsMix.ToolTip1"))
        Me.tabPropsMix.UseVisualStyleBackColor = True
        '
        'gridPropertiesMixture
        '
        resources.ApplyResources(Me.gridPropertiesMixture, "gridPropertiesMixture")
        Me.gridPropertiesMixture.AllowUserToAddRows = False
        Me.gridPropertiesMixture.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesMixture.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesMixture.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.Column1})
        Me.gridPropertiesMixture.Name = "gridPropertiesMixture"
        Me.gridPropertiesMixture.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesMixture, resources.GetString("gridPropertiesMixture.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesMixture, resources.GetString("gridPropertiesMixture.ToolTip1"))
        '
        'DataGridViewTextBoxColumn1
        '
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle13.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle13
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        '
        'Column1
        '
        Me.Column1.FillWeight = 30.0!
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        '
        'tabPropsVapor
        '
        resources.ApplyResources(Me.tabPropsVapor, "tabPropsVapor")
        Me.tabPropsVapor.Controls.Add(Me.gridPropertiesVapor)
        Me.tabPropsVapor.Name = "tabPropsVapor"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsVapor, resources.GetString("tabPropsVapor.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsVapor, resources.GetString("tabPropsVapor.ToolTip1"))
        Me.tabPropsVapor.UseVisualStyleBackColor = True
        '
        'gridPropertiesVapor
        '
        resources.ApplyResources(Me.gridPropertiesVapor, "gridPropertiesVapor")
        Me.gridPropertiesVapor.AllowUserToAddRows = False
        Me.gridPropertiesVapor.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesVapor.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesVapor.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4, Me.DataGridViewTextBoxColumn5})
        Me.gridPropertiesVapor.Name = "gridPropertiesVapor"
        Me.gridPropertiesVapor.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesVapor, resources.GetString("gridPropertiesVapor.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesVapor, resources.GetString("gridPropertiesVapor.ToolTip1"))
        '
        'DataGridViewTextBoxColumn3
        '
        Me.DataGridViewTextBoxColumn3.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn3, "DataGridViewTextBoxColumn3")
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        '
        'DataGridViewTextBoxColumn4
        '
        DataGridViewCellStyle14.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn4.DefaultCellStyle = DataGridViewCellStyle14
        Me.DataGridViewTextBoxColumn4.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn4, "DataGridViewTextBoxColumn4")
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        '
        'DataGridViewTextBoxColumn5
        '
        Me.DataGridViewTextBoxColumn5.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        '
        'tabPropsLiqMix
        '
        resources.ApplyResources(Me.tabPropsLiqMix, "tabPropsLiqMix")
        Me.tabPropsLiqMix.Controls.Add(Me.gridPropertiesLiqMix)
        Me.tabPropsLiqMix.Name = "tabPropsLiqMix"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsLiqMix, resources.GetString("tabPropsLiqMix.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsLiqMix, resources.GetString("tabPropsLiqMix.ToolTip1"))
        Me.tabPropsLiqMix.UseVisualStyleBackColor = True
        '
        'gridPropertiesLiqMix
        '
        resources.ApplyResources(Me.gridPropertiesLiqMix, "gridPropertiesLiqMix")
        Me.gridPropertiesLiqMix.AllowUserToAddRows = False
        Me.gridPropertiesLiqMix.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiqMix.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiqMix.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn6, Me.DataGridViewTextBoxColumn7, Me.DataGridViewTextBoxColumn8})
        Me.gridPropertiesLiqMix.Name = "gridPropertiesLiqMix"
        Me.gridPropertiesLiqMix.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesLiqMix, resources.GetString("gridPropertiesLiqMix.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesLiqMix, resources.GetString("gridPropertiesLiqMix.ToolTip1"))
        '
        'DataGridViewTextBoxColumn6
        '
        Me.DataGridViewTextBoxColumn6.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        '
        'DataGridViewTextBoxColumn7
        '
        DataGridViewCellStyle15.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn7.DefaultCellStyle = DataGridViewCellStyle15
        Me.DataGridViewTextBoxColumn7.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn7, "DataGridViewTextBoxColumn7")
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        '
        'DataGridViewTextBoxColumn8
        '
        Me.DataGridViewTextBoxColumn8.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn8, "DataGridViewTextBoxColumn8")
        Me.DataGridViewTextBoxColumn8.Name = "DataGridViewTextBoxColumn8"
        '
        'tabPropsLiq1
        '
        resources.ApplyResources(Me.tabPropsLiq1, "tabPropsLiq1")
        Me.tabPropsLiq1.Controls.Add(Me.gridPropertiesLiq1)
        Me.tabPropsLiq1.Name = "tabPropsLiq1"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsLiq1, resources.GetString("tabPropsLiq1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsLiq1, resources.GetString("tabPropsLiq1.ToolTip1"))
        Me.tabPropsLiq1.UseVisualStyleBackColor = True
        '
        'gridPropertiesLiq1
        '
        resources.ApplyResources(Me.gridPropertiesLiq1, "gridPropertiesLiq1")
        Me.gridPropertiesLiq1.AllowUserToAddRows = False
        Me.gridPropertiesLiq1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiq1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiq1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn9, Me.DataGridViewTextBoxColumn10, Me.DataGridViewTextBoxColumn11})
        Me.gridPropertiesLiq1.Name = "gridPropertiesLiq1"
        Me.gridPropertiesLiq1.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesLiq1, resources.GetString("gridPropertiesLiq1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesLiq1, resources.GetString("gridPropertiesLiq1.ToolTip1"))
        '
        'DataGridViewTextBoxColumn9
        '
        Me.DataGridViewTextBoxColumn9.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn9, "DataGridViewTextBoxColumn9")
        Me.DataGridViewTextBoxColumn9.Name = "DataGridViewTextBoxColumn9"
        '
        'DataGridViewTextBoxColumn10
        '
        DataGridViewCellStyle16.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn10.DefaultCellStyle = DataGridViewCellStyle16
        Me.DataGridViewTextBoxColumn10.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn10, "DataGridViewTextBoxColumn10")
        Me.DataGridViewTextBoxColumn10.Name = "DataGridViewTextBoxColumn10"
        '
        'DataGridViewTextBoxColumn11
        '
        Me.DataGridViewTextBoxColumn11.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn11, "DataGridViewTextBoxColumn11")
        Me.DataGridViewTextBoxColumn11.Name = "DataGridViewTextBoxColumn11"
        '
        'tabPropsLiq2
        '
        resources.ApplyResources(Me.tabPropsLiq2, "tabPropsLiq2")
        Me.tabPropsLiq2.Controls.Add(Me.gridPropertiesLiq2)
        Me.tabPropsLiq2.Name = "tabPropsLiq2"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsLiq2, resources.GetString("tabPropsLiq2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsLiq2, resources.GetString("tabPropsLiq2.ToolTip1"))
        Me.tabPropsLiq2.UseVisualStyleBackColor = True
        '
        'gridPropertiesLiq2
        '
        resources.ApplyResources(Me.gridPropertiesLiq2, "gridPropertiesLiq2")
        Me.gridPropertiesLiq2.AllowUserToAddRows = False
        Me.gridPropertiesLiq2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiq2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiq2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn12, Me.DataGridViewTextBoxColumn13, Me.DataGridViewTextBoxColumn14})
        Me.gridPropertiesLiq2.Name = "gridPropertiesLiq2"
        Me.gridPropertiesLiq2.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesLiq2, resources.GetString("gridPropertiesLiq2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesLiq2, resources.GetString("gridPropertiesLiq2.ToolTip1"))
        '
        'DataGridViewTextBoxColumn12
        '
        Me.DataGridViewTextBoxColumn12.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn12, "DataGridViewTextBoxColumn12")
        Me.DataGridViewTextBoxColumn12.Name = "DataGridViewTextBoxColumn12"
        '
        'DataGridViewTextBoxColumn13
        '
        DataGridViewCellStyle17.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn13.DefaultCellStyle = DataGridViewCellStyle17
        Me.DataGridViewTextBoxColumn13.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn13, "DataGridViewTextBoxColumn13")
        Me.DataGridViewTextBoxColumn13.Name = "DataGridViewTextBoxColumn13"
        '
        'DataGridViewTextBoxColumn14
        '
        Me.DataGridViewTextBoxColumn14.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn14, "DataGridViewTextBoxColumn14")
        Me.DataGridViewTextBoxColumn14.Name = "DataGridViewTextBoxColumn14"
        '
        'tabPropsSolid
        '
        resources.ApplyResources(Me.tabPropsSolid, "tabPropsSolid")
        Me.tabPropsSolid.Controls.Add(Me.gridPropertiesSolid)
        Me.tabPropsSolid.Name = "tabPropsSolid"
        Me.ToolTipChangeTag.SetToolTip(Me.tabPropsSolid, resources.GetString("tabPropsSolid.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tabPropsSolid, resources.GetString("tabPropsSolid.ToolTip1"))
        Me.tabPropsSolid.UseVisualStyleBackColor = True
        '
        'gridPropertiesSolid
        '
        resources.ApplyResources(Me.gridPropertiesSolid, "gridPropertiesSolid")
        Me.gridPropertiesSolid.AllowUserToAddRows = False
        Me.gridPropertiesSolid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesSolid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesSolid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn15, Me.DataGridViewTextBoxColumn16, Me.DataGridViewTextBoxColumn17})
        Me.gridPropertiesSolid.Name = "gridPropertiesSolid"
        Me.gridPropertiesSolid.RowHeadersVisible = False
        Me.ToolTipValues.SetToolTip(Me.gridPropertiesSolid, resources.GetString("gridPropertiesSolid.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.gridPropertiesSolid, resources.GetString("gridPropertiesSolid.ToolTip1"))
        '
        'DataGridViewTextBoxColumn15
        '
        Me.DataGridViewTextBoxColumn15.FillWeight = 60.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn15, "DataGridViewTextBoxColumn15")
        Me.DataGridViewTextBoxColumn15.Name = "DataGridViewTextBoxColumn15"
        '
        'DataGridViewTextBoxColumn16
        '
        DataGridViewCellStyle18.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn16.DefaultCellStyle = DataGridViewCellStyle18
        Me.DataGridViewTextBoxColumn16.FillWeight = 40.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn16, "DataGridViewTextBoxColumn16")
        Me.DataGridViewTextBoxColumn16.Name = "DataGridViewTextBoxColumn16"
        '
        'DataGridViewTextBoxColumn17
        '
        Me.DataGridViewTextBoxColumn17.FillWeight = 30.0!
        resources.ApplyResources(Me.DataGridViewTextBoxColumn17, "DataGridViewTextBoxColumn17")
        Me.DataGridViewTextBoxColumn17.Name = "DataGridViewTextBoxColumn17"
        '
        'TabPageAnnotations
        '
        resources.ApplyResources(Me.TabPageAnnotations, "TabPageAnnotations")
        Me.TabPageAnnotations.Controls.Add(Me.rtbAnnotations)
        Me.TabPageAnnotations.Name = "TabPageAnnotations"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageAnnotations, resources.GetString("TabPageAnnotations.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageAnnotations, resources.GetString("TabPageAnnotations.ToolTip1"))
        Me.TabPageAnnotations.UseVisualStyleBackColor = True
        '
        'rtbAnnotations
        '
        resources.ApplyResources(Me.rtbAnnotations, "rtbAnnotations")
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.22621}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowOpen = False
        Me.rtbAnnotations.ShowSave = False
        Me.rtbAnnotations.ToolbarVisible = False
        Me.ToolTipChangeTag.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip1"))
        '
        'TabPageDynamics
        '
        resources.ApplyResources(Me.TabPageDynamics, "TabPageDynamics")
        Me.TabPageDynamics.Controls.Add(Me.Label21)
        Me.TabPageDynamics.Controls.Add(Me.cbDynSpec)
        Me.TabPageDynamics.Name = "TabPageDynamics"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageDynamics, resources.GetString("TabPageDynamics.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageDynamics, resources.GetString("TabPageDynamics.ToolTip1"))
        Me.TabPageDynamics.UseVisualStyleBackColor = True
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTipValues.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip1"))
        '
        'cbDynSpec
        '
        resources.ApplyResources(Me.cbDynSpec, "cbDynSpec")
        Me.cbDynSpec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbDynSpec.FormattingEnabled = True
        Me.cbDynSpec.Items.AddRange(New Object() {resources.GetString("cbDynSpec.Items"), resources.GetString("cbDynSpec.Items1")})
        Me.cbDynSpec.Name = "cbDynSpec"
        Me.ToolTipChangeTag.SetToolTip(Me.cbDynSpec, resources.GetString("cbDynSpec.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbDynSpec, resources.GetString("cbDynSpec.ToolTip1"))
        '
        'TabPageFloatingTables
        '
        resources.ApplyResources(Me.TabPageFloatingTables, "TabPageFloatingTables")
        Me.TabPageFloatingTables.Controls.Add(Me.GroupBox2)
        Me.TabPageFloatingTables.Name = "TabPageFloatingTables"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPageFloatingTables, resources.GetString("TabPageFloatingTables.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPageFloatingTables, resources.GetString("TabPageFloatingTables.ToolTip1"))
        Me.TabPageFloatingTables.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.Label20)
        Me.GroupBox2.Controls.Add(Me.cbFloatingTableCompoundAmountBasis)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox2, resources.GetString("GroupBox2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox2, resources.GetString("GroupBox2.ToolTip1"))
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTipValues.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip1"))
        '
        'cbFloatingTableCompoundAmountBasis
        '
        resources.ApplyResources(Me.cbFloatingTableCompoundAmountBasis, "cbFloatingTableCompoundAmountBasis")
        Me.cbFloatingTableCompoundAmountBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFloatingTableCompoundAmountBasis.FormattingEnabled = True
        Me.cbFloatingTableCompoundAmountBasis.Items.AddRange(New Object() {resources.GetString("cbFloatingTableCompoundAmountBasis.Items"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items1"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items2"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items3"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items4"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items5"), resources.GetString("cbFloatingTableCompoundAmountBasis.Items6")})
        Me.cbFloatingTableCompoundAmountBasis.Name = "cbFloatingTableCompoundAmountBasis"
        Me.ToolTipChangeTag.SetToolTip(Me.cbFloatingTableCompoundAmountBasis, resources.GetString("cbFloatingTableCompoundAmountBasis.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbFloatingTableCompoundAmountBasis, resources.GetString("cbFloatingTableCompoundAmountBasis.ToolTip1"))
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblStatus)
        Me.GroupBox5.Controls.Add(Me.btnUtils)
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.lblConnectedTo)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip1"))
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        Me.lblStatus.ReadOnly = True
        Me.ToolTipChangeTag.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip1"))
        '
        'btnUtils
        '
        resources.ApplyResources(Me.btnUtils, "btnUtils")
        Me.btnUtils.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.bullet_sparkle
        Me.btnUtils.Name = "btnUtils"
        Me.ToolTipValues.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip1"))
        Me.btnUtils.UseVisualStyleBackColor = True
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
        Me.ToolTipChangeTag.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip1"))
        '
        'chkActive
        '
        resources.ApplyResources(Me.chkActive, "chkActive")
        Me.chkActive.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.bullet_tick
        Me.chkActive.Name = "chkActive"
        Me.ToolTipValues.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip1"))
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.ToolTipValues.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip1"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTipValues.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip1"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTipValues.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip1"))
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTipValues.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip1"))
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnDisconnectO)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectI)
        Me.GroupBox1.Controls.Add(Me.cbOutlet)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.cbInlet)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip1"))
        '
        'btnDisconnectO
        '
        resources.ApplyResources(Me.btnDisconnectO, "btnDisconnectO")
        Me.btnDisconnectO.Name = "btnDisconnectO"
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectO, resources.GetString("btnDisconnectO.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectO, resources.GetString("btnDisconnectO.ToolTip1"))
        Me.btnDisconnectO.UseVisualStyleBackColor = True
        '
        'btnDisconnectI
        '
        resources.ApplyResources(Me.btnDisconnectI, "btnDisconnectI")
        Me.btnDisconnectI.Name = "btnDisconnectI"
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectI, resources.GetString("btnDisconnectI.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnectI, resources.GetString("btnDisconnectI.ToolTip1"))
        Me.btnDisconnectI.UseVisualStyleBackColor = True
        '
        'cbOutlet
        '
        resources.ApplyResources(Me.cbOutlet, "cbOutlet")
        Me.cbOutlet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet.FormattingEnabled = True
        Me.cbOutlet.Name = "cbOutlet"
        Me.ToolTipChangeTag.SetToolTip(Me.cbOutlet, resources.GetString("cbOutlet.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOutlet, resources.GetString("cbOutlet.ToolTip1"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        '
        'cbInlet
        '
        resources.ApplyResources(Me.cbInlet, "cbInlet")
        Me.cbInlet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet.FormattingEnabled = True
        Me.cbInlet.Name = "cbInlet"
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet, resources.GetString("cbInlet.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet, resources.GetString("cbInlet.ToolTip1"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.btnConfigurePP)
        Me.GroupBox3.Controls.Add(Me.cbPropPack)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox3, resources.GetString("GroupBox3.ToolTip1"))
        '
        'btnConfigurePP
        '
        resources.ApplyResources(Me.btnConfigurePP, "btnConfigurePP")
        Me.btnConfigurePP.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.btnConfigurePP.Name = "btnConfigurePP"
        Me.ToolTipValues.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip1"))
        Me.btnConfigurePP.UseVisualStyleBackColor = True
        '
        'cbPropPack
        '
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Name = "cbPropPack"
        Me.ToolTipChangeTag.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPropPack, resources.GetString("cbPropPack.ToolTip1"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTipValues.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip1"))
        '
        'UtilitiesCtxMenu
        '
        resources.ApplyResources(Me.UtilitiesCtxMenu, "UtilitiesCtxMenu")
        Me.UtilitiesCtxMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddUtilityTSMI})
        Me.UtilitiesCtxMenu.Name = "ContextMenuStrip1"
        Me.ToolTipChangeTag.SetToolTip(Me.UtilitiesCtxMenu, resources.GetString("UtilitiesCtxMenu.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.UtilitiesCtxMenu, resources.GetString("UtilitiesCtxMenu.ToolTip1"))
        '
        'AddUtilityTSMI
        '
        resources.ApplyResources(Me.AddUtilityTSMI, "AddUtilityTSMI")
        Me.AddUtilityTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DiagramaDeFasesToolStripMenuItem, Me.BinaryTSMI, Me.TernaryTSMI, Me.PetroleumPropsTSMI, Me.HydratesTSMI, Me.TCPTSMI})
        Me.AddUtilityTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.add
        Me.AddUtilityTSMI.Name = "AddUtilityTSMI"
        '
        'DiagramaDeFasesToolStripMenuItem
        '
        resources.ApplyResources(Me.DiagramaDeFasesToolStripMenuItem, "DiagramaDeFasesToolStripMenuItem")
        Me.DiagramaDeFasesToolStripMenuItem.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.DiagramaDeFasesToolStripMenuItem.Name = "DiagramaDeFasesToolStripMenuItem"
        '
        'BinaryTSMI
        '
        resources.ApplyResources(Me.BinaryTSMI, "BinaryTSMI")
        Me.BinaryTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.BinaryTSMI.Name = "BinaryTSMI"
        '
        'TernaryTSMI
        '
        resources.ApplyResources(Me.TernaryTSMI, "TernaryTSMI")
        Me.TernaryTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.TernaryTSMI.Name = "TernaryTSMI"
        '
        'PetroleumPropsTSMI
        '
        resources.ApplyResources(Me.PetroleumPropsTSMI, "PetroleumPropsTSMI")
        Me.PetroleumPropsTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.PetroleumPropsTSMI.Name = "PetroleumPropsTSMI"
        '
        'HydratesTSMI
        '
        resources.ApplyResources(Me.HydratesTSMI, "HydratesTSMI")
        Me.HydratesTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.HydratesTSMI.Name = "HydratesTSMI"
        '
        'TCPTSMI
        '
        resources.ApplyResources(Me.TCPTSMI, "TCPTSMI")
        Me.TCPTSMI.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog
        Me.TCPTSMI.Name = "TCPTSMI"
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.ToolTipValues.SetToolTip(Me.TabControl1, resources.GetString("TabControl1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TabControl1, resources.GetString("TabControl1.ToolTip1"))
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Controls.Add(Me.GroupBox5)
        Me.TabPage3.Name = "TabPage3"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage3, resources.GetString("TabPage3.ToolTip1"))
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'TabPage4
        '
        resources.ApplyResources(Me.TabPage4, "TabPage4")
        Me.TabPage4.Controls.Add(Me.GroupBox1)
        Me.TabPage4.Name = "TabPage4"
        Me.ToolTipChangeTag.SetToolTip(Me.TabPage4, resources.GetString("TabPage4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.TabPage4, resources.GetString("TabPage4.ToolTip1"))
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'MaterialStreamEditor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.SplitContainer1)
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.GroupBox3)
        Me.Name = "MaterialStreamEditor"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.TabControlMain0.ResumeLayout(False)
        Me.TabPageInputPane.ResumeLayout(False)
        Me.TabControl2.ResumeLayout(False)
        Me.TabPageInputConditions.ResumeLayout(False)
        Me.TabPageInputConditions.PerformLayout()
        Me.TabPageInputComposition.ResumeLayout(False)
        Me.TabPageInputComposition.PerformLayout()
        CType(Me.gridInputComposition, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPageResultsPane.ResumeLayout(False)
        Me.TabControlMain.ResumeLayout(False)
        Me.TabPageResultsComp.ResumeLayout(False)
        Me.TabControlCompound.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        CType(Me.gridPhaseTotal, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPhaseComps.ResumeLayout(False)
        Me.tabCompMix.ResumeLayout(False)
        CType(Me.gridCompMixture, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabCompVapor.ResumeLayout(False)
        CType(Me.gridCompVapor, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabCompLiqMix.ResumeLayout(False)
        CType(Me.gridCompLiqMix, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabCompLiq1.ResumeLayout(False)
        CType(Me.gridCompLiq1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabCompLiq2.ResumeLayout(False)
        CType(Me.gridCompLiq2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabCompSolid.ResumeLayout(False)
        CType(Me.gridCompSolid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.TabCompoundPhaseProps.ResumeLayout(False)
        Me.TabCompPropVapor.ResumeLayout(False)
        CType(Me.gridCompPropVapor, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabCompPropLiq1.ResumeLayout(False)
        CType(Me.gridCompPropLiq1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabCompPropLiq2.ResumeLayout(False)
        CType(Me.gridCompPropLiq2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabCompPropSolid.ResumeLayout(False)
        CType(Me.gridCompPropSolid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPageResultsProps.ResumeLayout(False)
        Me.TabPhaseProps.ResumeLayout(False)
        Me.tabPropsMix.ResumeLayout(False)
        CType(Me.gridPropertiesMixture, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabPropsVapor.ResumeLayout(False)
        CType(Me.gridPropertiesVapor, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabPropsLiqMix.ResumeLayout(False)
        CType(Me.gridPropertiesLiqMix, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabPropsLiq1.ResumeLayout(False)
        CType(Me.gridPropertiesLiq1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabPropsLiq2.ResumeLayout(False)
        CType(Me.gridPropertiesLiq2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.tabPropsSolid.ResumeLayout(False)
        CType(Me.gridPropertiesSolid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPageAnnotations.ResumeLayout(False)
        Me.TabPageDynamics.ResumeLayout(False)
        Me.TabPageDynamics.PerformLayout()
        Me.TabPageFloatingTables.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.UtilitiesCtxMenu.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblObject As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectO As System.Windows.Forms.Button
    Public WithEvents btnDisconnectI As System.Windows.Forms.Button
    Public WithEvents cbOutlet As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbInlet As System.Windows.Forms.ComboBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbUnitsT As System.Windows.Forms.ComboBox
    Public WithEvents tbTemp As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbSpec As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbUnitsQ As System.Windows.Forms.ComboBox
    Public WithEvents tbVolFlow As System.Windows.Forms.TextBox
    Public WithEvents cbUnitsM As System.Windows.Forms.ComboBox
    Public WithEvents tbMoleFlow As System.Windows.Forms.TextBox
    Public WithEvents cbUnitsW As System.Windows.Forms.ComboBox
    Public WithEvents tbMassFlow As System.Windows.Forms.TextBox
    Public WithEvents cbUnitsP As System.Windows.Forms.ComboBox
    Public WithEvents tbPressure As System.Windows.Forms.TextBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents cbUnitsS As System.Windows.Forms.ComboBox
    Public WithEvents tbEntr As System.Windows.Forms.TextBox
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents cbUnitsH As System.Windows.Forms.ComboBox
    Public WithEvents tbEnth As System.Windows.Forms.TextBox
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents btnEraseInput As System.Windows.Forms.Button
    Public WithEvents btnEqualizeInput As System.Windows.Forms.Button
    Public WithEvents btnNormalizeInput As System.Windows.Forms.Button
    Public WithEvents gridInputComposition As System.Windows.Forms.DataGridView
    Public WithEvents cbCompBasis As System.Windows.Forms.ComboBox
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents lblInputAmount As System.Windows.Forms.Label
    Public WithEvents TabPhaseProps As System.Windows.Forms.TabControl
    Public WithEvents tabPropsMix As System.Windows.Forms.TabPage
    Public WithEvents gridPropertiesMixture As System.Windows.Forms.DataGridView
    Public WithEvents tabPropsVapor As System.Windows.Forms.TabPage
    Public WithEvents tabPropsLiqMix As System.Windows.Forms.TabPage
    Public WithEvents tabPropsLiq1 As System.Windows.Forms.TabPage
    Public WithEvents gridPropertiesLiq1 As System.Windows.Forms.DataGridView
    Public WithEvents tabPropsLiq2 As System.Windows.Forms.TabPage
    Public WithEvents gridPropertiesLiq2 As System.Windows.Forms.DataGridView
    Public WithEvents tabPropsSolid As System.Windows.Forms.TabPage
    Public WithEvents gridPropertiesSolid As System.Windows.Forms.DataGridView
    Public WithEvents TabPhaseComps As System.Windows.Forms.TabControl
    Public WithEvents tabCompMix As System.Windows.Forms.TabPage
    Public WithEvents gridCompMixture As System.Windows.Forms.DataGridView
    Public WithEvents tabCompVapor As System.Windows.Forms.TabPage
    Public WithEvents tabCompLiqMix As System.Windows.Forms.TabPage
    Public WithEvents tabCompLiq1 As System.Windows.Forms.TabPage
    Public WithEvents tabCompLiq2 As System.Windows.Forms.TabPage
    Public WithEvents tabCompSolid As System.Windows.Forms.TabPage
    Public WithEvents cbCalculatedAmountsBasis As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents gridCompVapor As System.Windows.Forms.DataGridView
    Public WithEvents gridCompLiqMix As System.Windows.Forms.DataGridView
    Public WithEvents gridCompLiq1 As System.Windows.Forms.DataGridView
    Public WithEvents gridCompLiq2 As System.Windows.Forms.DataGridView
    Public WithEvents gridCompSolid As System.Windows.Forms.DataGridView
    Public WithEvents tbFracSpec As System.Windows.Forms.TextBox
    Public WithEvents Label17 As System.Windows.Forms.Label
    Public WithEvents gridPropertiesVapor As System.Windows.Forms.DataGridView
    Public WithEvents gridPropertiesLiqMix As System.Windows.Forms.DataGridView
    Public WithEvents btnCompAcceptChanges As System.Windows.Forms.Button
    Public WithEvents lblAmountTotal As System.Windows.Forms.Label
    Public WithEvents UtilitiesCtxMenu As System.Windows.Forms.ContextMenuStrip
    Public WithEvents AddUtilityTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents DiagramaDeFasesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents BinaryTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TernaryTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents PetroleumPropsTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents HydratesTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TCPTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents btnUtils As System.Windows.Forms.Button
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents TabControlMain As System.Windows.Forms.TabControl
    Public WithEvents TabPageResultsComp As System.Windows.Forms.TabPage
    Public WithEvents TabPageResultsProps As System.Windows.Forms.TabPage
    Public WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn8 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn9 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn10 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn11 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn12 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn13 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn14 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn15 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn16 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn17 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn18 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn19 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn20 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn21 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn22 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn23 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn24 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn25 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn26 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn27 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn28 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn29 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents TabControlCompound As System.Windows.Forms.TabControl
    Public WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents TabPage2 As System.Windows.Forms.TabPage
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents TabCompoundPhaseProps As System.Windows.Forms.TabControl
    Public WithEvents TabCompPropVapor As System.Windows.Forms.TabPage
    Public WithEvents gridCompPropVapor As System.Windows.Forms.DataGridView
    Public WithEvents TabCompPropLiq1 As System.Windows.Forms.TabPage
    Public WithEvents gridCompPropLiq1 As System.Windows.Forms.DataGridView
    Public WithEvents TabCompPropLiq2 As System.Windows.Forms.TabPage
    Public WithEvents gridCompPropLiq2 As System.Windows.Forms.DataGridView
    Public WithEvents TabCompPropSolid As System.Windows.Forms.TabPage
    Public WithEvents gridCompPropSolid As System.Windows.Forms.DataGridView
    Public WithEvents cbCompoundPhaseProperties As System.Windows.Forms.ComboBox
    Public WithEvents lblCompPropUnits As System.Windows.Forms.Label
    Public WithEvents DataGridViewTextBoxColumn32 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn33 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn36 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn37 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn38 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn39 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn40 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn41 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents GroupBox2 As GroupBox
    Public WithEvents Label20 As Label
    Public WithEvents cbFloatingTableCompoundAmountBasis As ComboBox
    Public WithEvents TabControlMain0 As TabControl
    Public WithEvents TabPageInputPane As TabPage
    Public WithEvents TabControl2 As TabControl
    Public WithEvents TabPageInputConditions As TabPage
    Public WithEvents TabPageInputComposition As TabPage
    Public WithEvents TabPageResultsPane As TabPage
    Public WithEvents TabPageAnnotations As TabPage
    Public WithEvents TabPageFloatingTables As TabPage
    Friend WithEvents compname As DataGridViewTextBoxColumn
    Friend WithEvents compamount As DataGridViewTextBoxColumn
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents lblStatus As TextBox
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents TabPage4 As TabPage
    Friend WithEvents TabPageDynamics As TabPage
    Public WithEvents Label21 As Label
    Public WithEvents cbDynSpec As ComboBox
    Friend WithEvents SplitContainer1 As SplitContainer
    Friend WithEvents cbForcePhase As ComboBox
    Friend WithEvents Label10 As Label
    Friend WithEvents Label22 As Label
    Public WithEvents cbSolvent As ComboBox
    Public WithEvents lblSolvent As Label
    Friend WithEvents chkShowAsPercentage As CheckBox
    Public WithEvents gridPhaseTotal As DataGridView
    Public WithEvents DataGridViewTextBoxColumn30 As DataGridViewTextBoxColumn
    Public WithEvents DataGridViewTextBoxColumn31 As DataGridViewTextBoxColumn
    Public WithEvents Button1 As Button
End Class
