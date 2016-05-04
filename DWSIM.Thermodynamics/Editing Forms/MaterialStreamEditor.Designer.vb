<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class MaterialStreamEditor
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(MaterialStreamEditor))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle5 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle6 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle7 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle8 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle9 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle10 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle11 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle12 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle13 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
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
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.cbUnitsS = New System.Windows.Forms.ComboBox()
        Me.tbEntr = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbUnitsH = New System.Windows.Forms.ComboBox()
        Me.tbEnth = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbUnitsQ = New System.Windows.Forms.ComboBox()
        Me.tbVolFlow = New System.Windows.Forms.TextBox()
        Me.cbUnitsM = New System.Windows.Forms.ComboBox()
        Me.tbMoleFlow = New System.Windows.Forms.TextBox()
        Me.cbUnitsW = New System.Windows.Forms.ComboBox()
        Me.tbMassFlow = New System.Windows.Forms.TextBox()
        Me.cbUnitsP = New System.Windows.Forms.ComboBox()
        Me.tbPressure = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbUnitsT = New System.Windows.Forms.ComboBox()
        Me.tbTemp = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbSbec = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.cbCompBasis = New System.Windows.Forms.ComboBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.gridInputComposition = New System.Windows.Forms.DataGridView()
        Me.btnNormalizeInput = New System.Windows.Forms.Button()
        Me.btnEqualizeInput = New System.Windows.Forms.Button()
        Me.btnEraseInput = New System.Windows.Forms.Button()
        Me.lblInputAmount = New System.Windows.Forms.Label()
        Me.compname = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.compamount = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.btnExpand = New System.Windows.Forms.CheckBox()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.gridPropertiesMixture = New System.Windows.Forms.DataGridView()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.TabPage5 = New System.Windows.Forms.TabPage()
        Me.TabPage6 = New System.Windows.Forms.TabPage()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridPropertiesLiq1 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn9 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn10 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn11 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridPropertiesLiq2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn12 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn13 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn14 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridPropertiesSolid = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn15 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn16 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn17 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.TabControl2 = New System.Windows.Forms.TabControl()
        Me.TabPage7 = New System.Windows.Forms.TabPage()
        Me.gridCompMixture = New System.Windows.Forms.DataGridView()
        Me.TabPage8 = New System.Windows.Forms.TabPage()
        Me.TabPage9 = New System.Windows.Forms.TabPage()
        Me.TabPage10 = New System.Windows.Forms.TabPage()
        Me.TabPage11 = New System.Windows.Forms.TabPage()
        Me.TabPage12 = New System.Windows.Forms.TabPage()
        Me.cbCalculatedAmountsBasis = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.DataGridViewTextBoxColumn18 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn19 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridCompVapor = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn20 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn21 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridCompLiqMix = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn22 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn23 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridCompLiq1 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn24 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn25 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridCompLiq2 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn26 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn27 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridCompSolid = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn28 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn29 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.rbSpecVapor = New System.Windows.Forms.RadioButton()
        Me.rbSpecLiquid = New System.Windows.Forms.RadioButton()
        Me.rbSpecSolid = New System.Windows.Forms.RadioButton()
        Me.tbFracSpec = New System.Windows.Forms.TextBox()
        Me.gridPropertiesVapor = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn4 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.gridPropertiesLiqMix = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn8 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Panel1.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        CType(Me.gridInputComposition, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox7.SuspendLayout()
        CType(Me.gridPropertiesMixture, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.TabPage4.SuspendLayout()
        Me.TabPage5.SuspendLayout()
        Me.TabPage6.SuspendLayout()
        CType(Me.gridPropertiesLiq1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridPropertiesLiq2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridPropertiesSolid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox8.SuspendLayout()
        Me.TabControl2.SuspendLayout()
        Me.TabPage7.SuspendLayout()
        CType(Me.gridCompMixture, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage8.SuspendLayout()
        Me.TabPage9.SuspendLayout()
        Me.TabPage10.SuspendLayout()
        Me.TabPage11.SuspendLayout()
        Me.TabPage12.SuspendLayout()
        CType(Me.gridCompVapor, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridCompLiqMix, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridCompLiq1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridCompLiq2, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridCompSolid, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridPropertiesVapor, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.gridPropertiesLiqMix, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Panel1
        '
        Me.Panel1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.Panel1.AutoScroll = True
        Me.Panel1.AutoScrollMinSize = New System.Drawing.Size(380, 829)
        Me.Panel1.Controls.Add(Me.GroupBox5)
        Me.Panel1.Controls.Add(Me.GroupBox1)
        Me.Panel1.Controls.Add(Me.GroupBox3)
        Me.Panel1.Controls.Add(Me.GroupBox2)
        Me.Panel1.Location = New System.Drawing.Point(8, 8)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(399, 672)
        Me.Panel1.TabIndex = 3
        '
        'GroupBox5
        '
        Me.GroupBox5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.lblConnectedTo)
        Me.GroupBox5.Controls.Add(Me.lblStatus)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Location = New System.Drawing.Point(6, 5)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(368, 98)
        Me.GroupBox5.TabIndex = 4
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "Informações Gerais"
        '
        'chkActive
        '
        Me.chkActive.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.chkActive.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkActive.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.bullet_tick
        Me.chkActive.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.chkActive.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkActive.Location = New System.Drawing.Point(341, 43)
        Me.chkActive.Name = "chkActive"
        Me.chkActive.Size = New System.Drawing.Size(21, 21)
        Me.chkActive.TabIndex = 21
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        Me.lblConnectedTo.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblConnectedTo.AutoSize = True
        Me.lblConnectedTo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblConnectedTo.Location = New System.Drawing.Point(115, 72)
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.lblConnectedTo.Size = New System.Drawing.Size(38, 13)
        Me.lblConnectedTo.TabIndex = 20
        Me.lblConnectedTo.Text = "Objeto"
        '
        'lblStatus
        '
        Me.lblStatus.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblStatus.AutoSize = True
        Me.lblStatus.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblStatus.Location = New System.Drawing.Point(115, 47)
        Me.lblStatus.Name = "lblStatus"
        Me.lblStatus.Size = New System.Drawing.Size(38, 13)
        Me.lblStatus.TabIndex = 19
        Me.lblStatus.Text = "Objeto"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label13.Location = New System.Drawing.Point(9, 72)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(48, 13)
        Me.Label13.TabIndex = 17
        Me.Label13.Text = "Ligado a"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label12.Location = New System.Drawing.Point(9, 47)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(37, 13)
        Me.Label12.TabIndex = 16
        Me.Label12.Text = "Status"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label11.Location = New System.Drawing.Point(9, 22)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(38, 13)
        Me.Label11.TabIndex = 14
        Me.Label11.Text = "Objeto"
        '
        'GroupBox1
        '
        Me.GroupBox1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectO)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectI)
        Me.GroupBox1.Controls.Add(Me.cbOutlet)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.cbInlet)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Location = New System.Drawing.Point(6, 105)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.Size = New System.Drawing.Size(368, 86)
        Me.GroupBox1.TabIndex = 0
        Me.GroupBox1.TabStop = False
        Me.GroupBox1.Text = "Conexões"
        '
        'btnDisconnectO
        '
        Me.btnDisconnectO.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnectO.BackgroundImage = CType(resources.GetObject("btnDisconnectO.BackgroundImage"), System.Drawing.Image)
        Me.btnDisconnectO.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnDisconnectO.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnectO.Location = New System.Drawing.Point(341, 49)
        Me.btnDisconnectO.Name = "btnDisconnectO"
        Me.btnDisconnectO.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectO.TabIndex = 15
        Me.btnDisconnectO.UseVisualStyleBackColor = True
        '
        'btnDisconnectI
        '
        Me.btnDisconnectI.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnectI.BackgroundImage = CType(resources.GetObject("btnDisconnectI.BackgroundImage"), System.Drawing.Image)
        Me.btnDisconnectI.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnDisconnectI.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnectI.Location = New System.Drawing.Point(341, 23)
        Me.btnDisconnectI.Name = "btnDisconnectI"
        Me.btnDisconnectI.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectI.TabIndex = 14
        Me.btnDisconnectI.UseVisualStyleBackColor = True
        '
        'cbOutlet
        '
        Me.cbOutlet.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbOutlet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet.FormattingEnabled = True
        Me.cbOutlet.Location = New System.Drawing.Point(118, 49)
        Me.cbOutlet.Name = "cbOutlet"
        Me.cbOutlet.Size = New System.Drawing.Size(217, 21)
        Me.cbOutlet.TabIndex = 3
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(8, 52)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(95, 13)
        Me.Label2.TabIndex = 2
        Me.Label2.Text = "Conexão a jusante"
        '
        'cbInlet
        '
        Me.cbInlet.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbInlet.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet.FormattingEnabled = True
        Me.cbInlet.Location = New System.Drawing.Point(118, 23)
        Me.cbInlet.Name = "cbInlet"
        Me.cbInlet.Size = New System.Drawing.Size(217, 21)
        Me.cbInlet.TabIndex = 1
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label1.Location = New System.Drawing.Point(8, 26)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(105, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Conexão a montante"
        '
        'GroupBox2
        '
        Me.GroupBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox2.Controls.Add(Me.tbFracSpec)
        Me.GroupBox2.Controls.Add(Me.rbSpecSolid)
        Me.GroupBox2.Controls.Add(Me.rbSpecLiquid)
        Me.GroupBox2.Controls.Add(Me.rbSpecVapor)
        Me.GroupBox2.Controls.Add(Me.Label17)
        Me.GroupBox2.Controls.Add(Me.GroupBox6)
        Me.GroupBox2.Controls.Add(Me.cbUnitsS)
        Me.GroupBox2.Controls.Add(Me.tbEntr)
        Me.GroupBox2.Controls.Add(Me.Label15)
        Me.GroupBox2.Controls.Add(Me.cbUnitsH)
        Me.GroupBox2.Controls.Add(Me.tbEnth)
        Me.GroupBox2.Controls.Add(Me.Label14)
        Me.GroupBox2.Controls.Add(Me.cbUnitsQ)
        Me.GroupBox2.Controls.Add(Me.tbVolFlow)
        Me.GroupBox2.Controls.Add(Me.cbUnitsM)
        Me.GroupBox2.Controls.Add(Me.tbMoleFlow)
        Me.GroupBox2.Controls.Add(Me.cbUnitsW)
        Me.GroupBox2.Controls.Add(Me.tbMassFlow)
        Me.GroupBox2.Controls.Add(Me.cbUnitsP)
        Me.GroupBox2.Controls.Add(Me.tbPressure)
        Me.GroupBox2.Controls.Add(Me.Label7)
        Me.GroupBox2.Controls.Add(Me.Label6)
        Me.GroupBox2.Controls.Add(Me.cbUnitsT)
        Me.GroupBox2.Controls.Add(Me.tbTemp)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.cbSbec)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Location = New System.Drawing.Point(6, 193)
        Me.GroupBox2.MinimumSize = New System.Drawing.Size(385, 536)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(385, 536)
        Me.GroupBox2.TabIndex = 1
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Parâmetros de Entrada"
        '
        'cbUnitsS
        '
        Me.cbUnitsS.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsS.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsS.FormattingEnabled = True
        Me.cbUnitsS.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsS.Location = New System.Drawing.Point(275, 205)
        Me.cbUnitsS.Name = "cbUnitsS"
        Me.cbUnitsS.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsS.TabIndex = 39
        '
        'tbEntr
        '
        Me.tbEntr.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbEntr.Location = New System.Drawing.Point(115, 206)
        Me.tbEntr.Name = "tbEntr"
        Me.tbEntr.Size = New System.Drawing.Size(154, 20)
        Me.tbEntr.TabIndex = 38
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label15.Location = New System.Drawing.Point(9, 209)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(100, 13)
        Me.Label15.TabIndex = 37
        Me.Label15.Text = "Entropia Específica"
        '
        'cbUnitsH
        '
        Me.cbUnitsH.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsH.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsH.FormattingEnabled = True
        Me.cbUnitsH.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsH.Location = New System.Drawing.Point(275, 179)
        Me.cbUnitsH.Name = "cbUnitsH"
        Me.cbUnitsH.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsH.TabIndex = 36
        '
        'tbEnth
        '
        Me.tbEnth.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbEnth.Location = New System.Drawing.Point(115, 180)
        Me.tbEnth.Name = "tbEnth"
        Me.tbEnth.Size = New System.Drawing.Size(154, 20)
        Me.tbEnth.TabIndex = 35
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label14.Location = New System.Drawing.Point(9, 183)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(99, 13)
        Me.Label14.TabIndex = 34
        Me.Label14.Text = "Entalpia Específica"
        '
        'cbUnitsQ
        '
        Me.cbUnitsQ.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsQ.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsQ.FormattingEnabled = True
        Me.cbUnitsQ.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsQ.Location = New System.Drawing.Point(275, 152)
        Me.cbUnitsQ.Name = "cbUnitsQ"
        Me.cbUnitsQ.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsQ.TabIndex = 33
        '
        'tbVolFlow
        '
        Me.tbVolFlow.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbVolFlow.Location = New System.Drawing.Point(115, 153)
        Me.tbVolFlow.Name = "tbVolFlow"
        Me.tbVolFlow.Size = New System.Drawing.Size(154, 20)
        Me.tbVolFlow.TabIndex = 32
        '
        'cbUnitsM
        '
        Me.cbUnitsM.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsM.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsM.FormattingEnabled = True
        Me.cbUnitsM.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsM.Location = New System.Drawing.Point(275, 126)
        Me.cbUnitsM.Name = "cbUnitsM"
        Me.cbUnitsM.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsM.TabIndex = 31
        '
        'tbMoleFlow
        '
        Me.tbMoleFlow.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbMoleFlow.Location = New System.Drawing.Point(115, 127)
        Me.tbMoleFlow.Name = "tbMoleFlow"
        Me.tbMoleFlow.Size = New System.Drawing.Size(154, 20)
        Me.tbMoleFlow.TabIndex = 30
        '
        'cbUnitsW
        '
        Me.cbUnitsW.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsW.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsW.FormattingEnabled = True
        Me.cbUnitsW.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsW.Location = New System.Drawing.Point(275, 100)
        Me.cbUnitsW.Name = "cbUnitsW"
        Me.cbUnitsW.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsW.TabIndex = 29
        '
        'tbMassFlow
        '
        Me.tbMassFlow.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbMassFlow.Location = New System.Drawing.Point(115, 101)
        Me.tbMassFlow.Name = "tbMassFlow"
        Me.tbMassFlow.Size = New System.Drawing.Size(154, 20)
        Me.tbMassFlow.TabIndex = 28
        '
        'cbUnitsP
        '
        Me.cbUnitsP.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsP.FormattingEnabled = True
        Me.cbUnitsP.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsP.Location = New System.Drawing.Point(275, 74)
        Me.cbUnitsP.Name = "cbUnitsP"
        Me.cbUnitsP.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsP.TabIndex = 27
        '
        'tbPressure
        '
        Me.tbPressure.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbPressure.Location = New System.Drawing.Point(115, 75)
        Me.tbPressure.Name = "tbPressure"
        Me.tbPressure.Size = New System.Drawing.Size(154, 20)
        Me.tbPressure.TabIndex = 26
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label7.Location = New System.Drawing.Point(9, 156)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(95, 13)
        Me.Label7.TabIndex = 25
        Me.Label7.Text = "Vazão Volumétrica"
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(9, 130)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(66, 13)
        Me.Label6.TabIndex = 24
        Me.Label6.Text = "Vazão Molar"
        '
        'cbUnitsT
        '
        Me.cbUnitsT.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbUnitsT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbUnitsT.FormattingEnabled = True
        Me.cbUnitsT.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbUnitsT.Location = New System.Drawing.Point(275, 48)
        Me.cbUnitsT.Name = "cbUnitsT"
        Me.cbUnitsT.Size = New System.Drawing.Size(74, 21)
        Me.cbUnitsT.TabIndex = 23
        '
        'tbTemp
        '
        Me.tbTemp.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTemp.Location = New System.Drawing.Point(115, 49)
        Me.tbTemp.Name = "tbTemp"
        Me.tbTemp.Size = New System.Drawing.Size(154, 20)
        Me.tbTemp.TabIndex = 22
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(9, 104)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(79, 13)
        Me.Label5.TabIndex = 18
        Me.Label5.Text = "Vazão Mássica"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(9, 78)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(45, 13)
        Me.Label4.TabIndex = 17
        Me.Label4.Text = "Pressão"
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(9, 52)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(67, 13)
        Me.Label3.TabIndex = 16
        Me.Label3.Text = "Temperatura"
        '
        'cbSbec
        '
        Me.cbSbec.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbSbec.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSbec.FormattingEnabled = True
        Me.cbSbec.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbSbec.Location = New System.Drawing.Point(115, 22)
        Me.cbSbec.Name = "cbSbec"
        Me.cbSbec.Size = New System.Drawing.Size(234, 21)
        Me.cbSbec.TabIndex = 15
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label8.Location = New System.Drawing.Point(9, 25)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(74, 13)
        Me.Label8.TabIndex = 14
        Me.Label8.Text = "Especificação"
        '
        'GroupBox4
        '
        Me.GroupBox4.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Location = New System.Drawing.Point(410, 570)
        Me.GroupBox4.MinimumSize = New System.Drawing.Size(354, 140)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(473, 140)
        Me.GroupBox4.TabIndex = 3
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Anotações Gerais"
        '
        'rtbAnnotations
        '
        Me.rtbAnnotations.Dock = System.Windows.Forms.DockStyle.Fill
        Me.rtbAnnotations.Location = New System.Drawing.Point(3, 16)
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fnil\fcharset0 Microsoft S" & _
    "ans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\viewkind4\uc1\pard\f0\fs17\par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.rtbAnnotations.Size = New System.Drawing.Size(467, 121)
        Me.rtbAnnotations.TabIndex = 0
        '
        'GroupBox3
        '
        Me.GroupBox3.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox3.Controls.Add(Me.cbFlashAlg)
        Me.GroupBox3.Controls.Add(Me.Label10)
        Me.GroupBox3.Controls.Add(Me.cbPropPack)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Location = New System.Drawing.Point(6, 735)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.Size = New System.Drawing.Size(368, 88)
        Me.GroupBox3.TabIndex = 2
        Me.GroupBox3.TabStop = False
        Me.GroupBox3.Text = "Configurações do Pacote de Propriedades"
        '
        'cbFlashAlg
        '
        Me.cbFlashAlg.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbFlashAlg.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlashAlg.FormattingEnabled = True
        Me.cbFlashAlg.Location = New System.Drawing.Point(135, 53)
        Me.cbFlashAlg.Name = "cbFlashAlg"
        Me.cbFlashAlg.Size = New System.Drawing.Size(200, 21)
        Me.cbFlashAlg.TabIndex = 17
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label10.Location = New System.Drawing.Point(8, 56)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(78, 13)
        Me.Label10.TabIndex = 16
        Me.Label10.Text = "Algoritmo Flash"
        '
        'cbPropPack
        '
        Me.cbPropPack.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        Me.cbPropPack.Location = New System.Drawing.Point(135, 26)
        Me.cbPropPack.Name = "cbPropPack"
        Me.cbPropPack.Size = New System.Drawing.Size(200, 21)
        Me.cbPropPack.TabIndex = 15
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label9.Location = New System.Drawing.Point(8, 29)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(121, 13)
        Me.Label9.TabIndex = 14
        Me.Label9.Text = "Pacote de Propriedades"
        '
        'lblTag
        '
        Me.lblTag.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblTag.Location = New System.Drawing.Point(115, 19)
        Me.lblTag.Name = "lblTag"
        Me.lblTag.Size = New System.Drawing.Size(247, 20)
        Me.lblTag.TabIndex = 23
        '
        'GroupBox6
        '
        Me.GroupBox6.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox6.Controls.Add(Me.lblInputAmount)
        Me.GroupBox6.Controls.Add(Me.btnEraseInput)
        Me.GroupBox6.Controls.Add(Me.btnEqualizeInput)
        Me.GroupBox6.Controls.Add(Me.btnNormalizeInput)
        Me.GroupBox6.Controls.Add(Me.gridInputComposition)
        Me.GroupBox6.Controls.Add(Me.cbCompBasis)
        Me.GroupBox6.Controls.Add(Me.Label16)
        Me.GroupBox6.Location = New System.Drawing.Point(6, 258)
        Me.GroupBox6.MinimumSize = New System.Drawing.Size(373, 270)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.Size = New System.Drawing.Size(373, 270)
        Me.GroupBox6.TabIndex = 5
        Me.GroupBox6.TabStop = False
        Me.GroupBox6.Text = "Composição"
        '
        'cbCompBasis
        '
        Me.cbCompBasis.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbCompBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCompBasis.FormattingEnabled = True
        Me.cbCompBasis.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbCompBasis.Location = New System.Drawing.Point(109, 19)
        Me.cbCompBasis.Name = "cbCompBasis"
        Me.cbCompBasis.Size = New System.Drawing.Size(234, 21)
        Me.cbCompBasis.TabIndex = 17
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label16.Location = New System.Drawing.Point(10, 24)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(31, 13)
        Me.Label16.TabIndex = 16
        Me.Label16.Text = "Base"
        '
        'gridInputComposition
        '
        Me.gridInputComposition.AllowUserToAddRows = False
        Me.gridInputComposition.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.gridInputComposition.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridInputComposition.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridInputComposition.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.compname, Me.compamount})
        Me.gridInputComposition.Location = New System.Drawing.Point(13, 46)
        Me.gridInputComposition.Name = "gridInputComposition"
        Me.gridInputComposition.RowHeadersVisible = False
        Me.gridInputComposition.Size = New System.Drawing.Size(330, 192)
        Me.gridInputComposition.TabIndex = 18
        '
        'btnNormalizeInput
        '
        Me.btnNormalizeInput.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnNormalizeInput.BackgroundImage = CType(resources.GetObject("btnNormalizeInput.BackgroundImage"), System.Drawing.Image)
        Me.btnNormalizeInput.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnNormalizeInput.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnNormalizeInput.Location = New System.Drawing.Point(349, 46)
        Me.btnNormalizeInput.Name = "btnNormalizeInput"
        Me.btnNormalizeInput.Size = New System.Drawing.Size(21, 21)
        Me.btnNormalizeInput.TabIndex = 16
        Me.btnNormalizeInput.UseVisualStyleBackColor = True
        '
        'btnEqualizeInput
        '
        Me.btnEqualizeInput.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnEqualizeInput.BackgroundImage = CType(resources.GetObject("btnEqualizeInput.BackgroundImage"), System.Drawing.Image)
        Me.btnEqualizeInput.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnEqualizeInput.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnEqualizeInput.Location = New System.Drawing.Point(349, 73)
        Me.btnEqualizeInput.Name = "btnEqualizeInput"
        Me.btnEqualizeInput.Size = New System.Drawing.Size(21, 21)
        Me.btnEqualizeInput.TabIndex = 19
        Me.btnEqualizeInput.UseVisualStyleBackColor = True
        '
        'btnEraseInput
        '
        Me.btnEraseInput.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnEraseInput.BackgroundImage = CType(resources.GetObject("btnEraseInput.BackgroundImage"), System.Drawing.Image)
        Me.btnEraseInput.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnEraseInput.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnEraseInput.Location = New System.Drawing.Point(349, 100)
        Me.btnEraseInput.Name = "btnEraseInput"
        Me.btnEraseInput.Size = New System.Drawing.Size(21, 21)
        Me.btnEraseInput.TabIndex = 20
        Me.btnEraseInput.UseVisualStyleBackColor = True
        '
        'lblInputAmount
        '
        Me.lblInputAmount.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.lblInputAmount.AutoSize = True
        Me.lblInputAmount.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblInputAmount.Location = New System.Drawing.Point(12, 246)
        Me.lblInputAmount.Name = "lblInputAmount"
        Me.lblInputAmount.Size = New System.Drawing.Size(34, 13)
        Me.lblInputAmount.TabIndex = 26
        Me.lblInputAmount.Text = "Total:"
        '
        'compname
        '
        Me.compname.FillWeight = 60.0!
        Me.compname.HeaderText = "Substância"
        Me.compname.Name = "compname"
        Me.compname.ReadOnly = True
        '
        'compamount
        '
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.compamount.DefaultCellStyle = DataGridViewCellStyle1
        Me.compamount.FillWeight = 40.0!
        Me.compamount.HeaderText = "Quantidade"
        Me.compamount.Name = "compamount"
        '
        'Label18
        '
        Me.Label18.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(5, 692)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(235, 13)
        Me.Label18.TabIndex = 4
        Me.Label18.Text = "Clique para expandir e visualizar os resultados ->"
        '
        'btnExpand
        '
        Me.btnExpand.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.btnExpand.Appearance = System.Windows.Forms.Appearance.Button
        Me.btnExpand.AutoSize = True
        Me.btnExpand.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Center
        Me.btnExpand.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnExpand.Location = New System.Drawing.Point(334, 687)
        Me.btnExpand.Name = "btnExpand"
        Me.btnExpand.Size = New System.Drawing.Size(73, 23)
        Me.btnExpand.TabIndex = 22
        Me.btnExpand.Text = "Expandir >>"
        Me.btnExpand.UseVisualStyleBackColor = True
        '
        'GroupBox7
        '
        Me.GroupBox7.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.GroupBox7.Controls.Add(Me.TabControl1)
        Me.GroupBox7.Location = New System.Drawing.Point(410, 284)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.Size = New System.Drawing.Size(473, 286)
        Me.GroupBox7.TabIndex = 23
        Me.GroupBox7.TabStop = False
        Me.GroupBox7.Text = "Propriedades"
        '
        'gridPropertiesMixture
        '
        Me.gridPropertiesMixture.AllowUserToAddRows = False
        Me.gridPropertiesMixture.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesMixture.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesMixture.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2, Me.Column1})
        Me.gridPropertiesMixture.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesMixture.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesMixture.Name = "gridPropertiesMixture"
        Me.gridPropertiesMixture.RowHeadersVisible = False
        Me.gridPropertiesMixture.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesMixture.TabIndex = 18
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Controls.Add(Me.TabPage5)
        Me.TabControl1.Controls.Add(Me.TabPage6)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(3, 16)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(467, 267)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.gridPropertiesMixture)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Size = New System.Drawing.Size(459, 241)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Mistura"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.gridPropertiesVapor)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Margin = New System.Windows.Forms.Padding(0)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Size = New System.Drawing.Size(459, 241)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Fase Vapor"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.gridPropertiesLiqMix)
        Me.TabPage3.Location = New System.Drawing.Point(4, 22)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Size = New System.Drawing.Size(459, 241)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Fase Líquida (Mistura)"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'TabPage4
        '
        Me.TabPage4.Controls.Add(Me.gridPropertiesLiq1)
        Me.TabPage4.Location = New System.Drawing.Point(4, 22)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Size = New System.Drawing.Size(459, 241)
        Me.TabPage4.TabIndex = 3
        Me.TabPage4.Text = "Fase Líquida 1"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'TabPage5
        '
        Me.TabPage5.Controls.Add(Me.gridPropertiesLiq2)
        Me.TabPage5.Location = New System.Drawing.Point(4, 22)
        Me.TabPage5.Name = "TabPage5"
        Me.TabPage5.Size = New System.Drawing.Size(459, 241)
        Me.TabPage5.TabIndex = 4
        Me.TabPage5.Text = "Fase Líquida 2"
        Me.TabPage5.UseVisualStyleBackColor = True
        '
        'TabPage6
        '
        Me.TabPage6.Controls.Add(Me.gridPropertiesSolid)
        Me.TabPage6.Location = New System.Drawing.Point(4, 22)
        Me.TabPage6.Name = "TabPage6"
        Me.TabPage6.Size = New System.Drawing.Size(459, 241)
        Me.TabPage6.TabIndex = 5
        Me.TabPage6.Text = "Fase Sólida"
        Me.TabPage6.UseVisualStyleBackColor = True
        '
        'DataGridViewTextBoxColumn1
        '
        Me.DataGridViewTextBoxColumn1.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn1.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        '
        'DataGridViewTextBoxColumn2
        '
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn2.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridViewTextBoxColumn2.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn2.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        '
        'Column1
        '
        Me.Column1.FillWeight = 30.0!
        Me.Column1.HeaderText = "Unidade"
        Me.Column1.Name = "Column1"
        '
        'gridPropertiesLiq1
        '
        Me.gridPropertiesLiq1.AllowUserToAddRows = False
        Me.gridPropertiesLiq1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiq1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiq1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn9, Me.DataGridViewTextBoxColumn10, Me.DataGridViewTextBoxColumn11})
        Me.gridPropertiesLiq1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesLiq1.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesLiq1.Name = "gridPropertiesLiq1"
        Me.gridPropertiesLiq1.RowHeadersVisible = False
        Me.gridPropertiesLiq1.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesLiq1.TabIndex = 19
        '
        'DataGridViewTextBoxColumn9
        '
        Me.DataGridViewTextBoxColumn9.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn9.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn9.Name = "DataGridViewTextBoxColumn9"
        '
        'DataGridViewTextBoxColumn10
        '
        DataGridViewCellStyle5.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn10.DefaultCellStyle = DataGridViewCellStyle5
        Me.DataGridViewTextBoxColumn10.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn10.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn10.Name = "DataGridViewTextBoxColumn10"
        '
        'DataGridViewTextBoxColumn11
        '
        Me.DataGridViewTextBoxColumn11.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn11.HeaderText = "Unidade"
        Me.DataGridViewTextBoxColumn11.Name = "DataGridViewTextBoxColumn11"
        '
        'gridPropertiesLiq2
        '
        Me.gridPropertiesLiq2.AllowUserToAddRows = False
        Me.gridPropertiesLiq2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiq2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiq2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn12, Me.DataGridViewTextBoxColumn13, Me.DataGridViewTextBoxColumn14})
        Me.gridPropertiesLiq2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesLiq2.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesLiq2.Name = "gridPropertiesLiq2"
        Me.gridPropertiesLiq2.RowHeadersVisible = False
        Me.gridPropertiesLiq2.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesLiq2.TabIndex = 19
        '
        'DataGridViewTextBoxColumn12
        '
        Me.DataGridViewTextBoxColumn12.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn12.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn12.Name = "DataGridViewTextBoxColumn12"
        '
        'DataGridViewTextBoxColumn13
        '
        DataGridViewCellStyle6.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn13.DefaultCellStyle = DataGridViewCellStyle6
        Me.DataGridViewTextBoxColumn13.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn13.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn13.Name = "DataGridViewTextBoxColumn13"
        '
        'DataGridViewTextBoxColumn14
        '
        Me.DataGridViewTextBoxColumn14.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn14.HeaderText = "Unidade"
        Me.DataGridViewTextBoxColumn14.Name = "DataGridViewTextBoxColumn14"
        '
        'gridPropertiesSolid
        '
        Me.gridPropertiesSolid.AllowUserToAddRows = False
        Me.gridPropertiesSolid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesSolid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesSolid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn15, Me.DataGridViewTextBoxColumn16, Me.DataGridViewTextBoxColumn17})
        Me.gridPropertiesSolid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesSolid.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesSolid.Name = "gridPropertiesSolid"
        Me.gridPropertiesSolid.RowHeadersVisible = False
        Me.gridPropertiesSolid.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesSolid.TabIndex = 19
        '
        'DataGridViewTextBoxColumn15
        '
        Me.DataGridViewTextBoxColumn15.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn15.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn15.Name = "DataGridViewTextBoxColumn15"
        '
        'DataGridViewTextBoxColumn16
        '
        DataGridViewCellStyle7.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn16.DefaultCellStyle = DataGridViewCellStyle7
        Me.DataGridViewTextBoxColumn16.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn16.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn16.Name = "DataGridViewTextBoxColumn16"
        '
        'DataGridViewTextBoxColumn17
        '
        Me.DataGridViewTextBoxColumn17.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn17.HeaderText = "Unidade"
        Me.DataGridViewTextBoxColumn17.Name = "DataGridViewTextBoxColumn17"
        '
        'GroupBox8
        '
        Me.GroupBox8.Controls.Add(Me.cbCalculatedAmountsBasis)
        Me.GroupBox8.Controls.Add(Me.Label19)
        Me.GroupBox8.Controls.Add(Me.TabControl2)
        Me.GroupBox8.Location = New System.Drawing.Point(410, 13)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.Size = New System.Drawing.Size(473, 269)
        Me.GroupBox8.TabIndex = 24
        Me.GroupBox8.TabStop = False
        Me.GroupBox8.Text = "Composições das Fases"
        '
        'TabControl2
        '
        Me.TabControl2.Controls.Add(Me.TabPage7)
        Me.TabControl2.Controls.Add(Me.TabPage8)
        Me.TabControl2.Controls.Add(Me.TabPage9)
        Me.TabControl2.Controls.Add(Me.TabPage10)
        Me.TabControl2.Controls.Add(Me.TabPage11)
        Me.TabControl2.Controls.Add(Me.TabPage12)
        Me.TabControl2.Location = New System.Drawing.Point(3, 47)
        Me.TabControl2.Name = "TabControl2"
        Me.TabControl2.SelectedIndex = 0
        Me.TabControl2.Size = New System.Drawing.Size(467, 216)
        Me.TabControl2.TabIndex = 0
        '
        'TabPage7
        '
        Me.TabPage7.Controls.Add(Me.gridCompMixture)
        Me.TabPage7.Location = New System.Drawing.Point(4, 22)
        Me.TabPage7.Name = "TabPage7"
        Me.TabPage7.Size = New System.Drawing.Size(459, 190)
        Me.TabPage7.TabIndex = 0
        Me.TabPage7.Text = "Mistura"
        Me.TabPage7.UseVisualStyleBackColor = True
        '
        'gridCompMixture
        '
        Me.gridCompMixture.AllowUserToAddRows = False
        Me.gridCompMixture.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompMixture.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompMixture.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn18, Me.DataGridViewTextBoxColumn19})
        Me.gridCompMixture.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompMixture.Location = New System.Drawing.Point(0, 0)
        Me.gridCompMixture.Name = "gridCompMixture"
        Me.gridCompMixture.RowHeadersVisible = False
        Me.gridCompMixture.Size = New System.Drawing.Size(459, 190)
        Me.gridCompMixture.TabIndex = 18
        '
        'TabPage8
        '
        Me.TabPage8.Controls.Add(Me.gridCompVapor)
        Me.TabPage8.Location = New System.Drawing.Point(4, 22)
        Me.TabPage8.Name = "TabPage8"
        Me.TabPage8.Size = New System.Drawing.Size(459, 190)
        Me.TabPage8.TabIndex = 1
        Me.TabPage8.Text = "Fase Vapor"
        Me.TabPage8.UseVisualStyleBackColor = True
        '
        'TabPage9
        '
        Me.TabPage9.Controls.Add(Me.gridCompLiqMix)
        Me.TabPage9.Location = New System.Drawing.Point(4, 22)
        Me.TabPage9.Name = "TabPage9"
        Me.TabPage9.Size = New System.Drawing.Size(459, 190)
        Me.TabPage9.TabIndex = 2
        Me.TabPage9.Text = "Fase Líquida (Mistura)"
        Me.TabPage9.UseVisualStyleBackColor = True
        '
        'TabPage10
        '
        Me.TabPage10.Controls.Add(Me.gridCompLiq1)
        Me.TabPage10.Location = New System.Drawing.Point(4, 22)
        Me.TabPage10.Name = "TabPage10"
        Me.TabPage10.Size = New System.Drawing.Size(459, 190)
        Me.TabPage10.TabIndex = 3
        Me.TabPage10.Text = "Fase Líquida 1"
        Me.TabPage10.UseVisualStyleBackColor = True
        '
        'TabPage11
        '
        Me.TabPage11.Controls.Add(Me.gridCompLiq2)
        Me.TabPage11.Location = New System.Drawing.Point(4, 22)
        Me.TabPage11.Name = "TabPage11"
        Me.TabPage11.Size = New System.Drawing.Size(459, 190)
        Me.TabPage11.TabIndex = 4
        Me.TabPage11.Text = "Fase Líquida 2"
        Me.TabPage11.UseVisualStyleBackColor = True
        '
        'TabPage12
        '
        Me.TabPage12.Controls.Add(Me.gridCompSolid)
        Me.TabPage12.Location = New System.Drawing.Point(4, 22)
        Me.TabPage12.Name = "TabPage12"
        Me.TabPage12.Size = New System.Drawing.Size(459, 190)
        Me.TabPage12.TabIndex = 5
        Me.TabPage12.Text = "Fase Sólida"
        Me.TabPage12.UseVisualStyleBackColor = True
        '
        'cbCalculatedAmountsBasis
        '
        Me.cbCalculatedAmountsBasis.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbCalculatedAmountsBasis.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalculatedAmountsBasis.FormattingEnabled = True
        Me.cbCalculatedAmountsBasis.Items.AddRange(New Object() {"Mínima das Entradas", "Média das Entradas", "Máxima das Entradas"})
        Me.cbCalculatedAmountsBasis.Location = New System.Drawing.Point(44, 20)
        Me.cbCalculatedAmountsBasis.Name = "cbCalculatedAmountsBasis"
        Me.cbCalculatedAmountsBasis.Size = New System.Drawing.Size(211, 21)
        Me.cbCalculatedAmountsBasis.TabIndex = 19
        '
        'Label19
        '
        Me.Label19.AutoSize = True
        Me.Label19.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label19.Location = New System.Drawing.Point(7, 23)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(31, 13)
        Me.Label19.TabIndex = 18
        Me.Label19.Text = "Base"
        '
        'DataGridViewTextBoxColumn18
        '
        Me.DataGridViewTextBoxColumn18.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn18.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn18.Name = "DataGridViewTextBoxColumn18"
        '
        'DataGridViewTextBoxColumn19
        '
        DataGridViewCellStyle8.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn19.DefaultCellStyle = DataGridViewCellStyle8
        Me.DataGridViewTextBoxColumn19.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn19.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn19.Name = "DataGridViewTextBoxColumn19"
        '
        'gridCompVapor
        '
        Me.gridCompVapor.AllowUserToAddRows = False
        Me.gridCompVapor.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompVapor.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompVapor.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn20, Me.DataGridViewTextBoxColumn21})
        Me.gridCompVapor.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompVapor.Location = New System.Drawing.Point(0, 0)
        Me.gridCompVapor.Name = "gridCompVapor"
        Me.gridCompVapor.RowHeadersVisible = False
        Me.gridCompVapor.Size = New System.Drawing.Size(459, 190)
        Me.gridCompVapor.TabIndex = 19
        '
        'DataGridViewTextBoxColumn20
        '
        Me.DataGridViewTextBoxColumn20.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn20.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn20.Name = "DataGridViewTextBoxColumn20"
        '
        'DataGridViewTextBoxColumn21
        '
        DataGridViewCellStyle9.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn21.DefaultCellStyle = DataGridViewCellStyle9
        Me.DataGridViewTextBoxColumn21.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn21.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn21.Name = "DataGridViewTextBoxColumn21"
        '
        'gridCompLiqMix
        '
        Me.gridCompLiqMix.AllowUserToAddRows = False
        Me.gridCompLiqMix.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiqMix.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiqMix.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn22, Me.DataGridViewTextBoxColumn23})
        Me.gridCompLiqMix.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompLiqMix.Location = New System.Drawing.Point(0, 0)
        Me.gridCompLiqMix.Name = "gridCompLiqMix"
        Me.gridCompLiqMix.RowHeadersVisible = False
        Me.gridCompLiqMix.Size = New System.Drawing.Size(459, 190)
        Me.gridCompLiqMix.TabIndex = 19
        '
        'DataGridViewTextBoxColumn22
        '
        Me.DataGridViewTextBoxColumn22.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn22.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn22.Name = "DataGridViewTextBoxColumn22"
        '
        'DataGridViewTextBoxColumn23
        '
        DataGridViewCellStyle10.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn23.DefaultCellStyle = DataGridViewCellStyle10
        Me.DataGridViewTextBoxColumn23.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn23.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn23.Name = "DataGridViewTextBoxColumn23"
        '
        'gridCompLiq1
        '
        Me.gridCompLiq1.AllowUserToAddRows = False
        Me.gridCompLiq1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiq1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiq1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn24, Me.DataGridViewTextBoxColumn25})
        Me.gridCompLiq1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompLiq1.Location = New System.Drawing.Point(0, 0)
        Me.gridCompLiq1.Name = "gridCompLiq1"
        Me.gridCompLiq1.RowHeadersVisible = False
        Me.gridCompLiq1.Size = New System.Drawing.Size(459, 190)
        Me.gridCompLiq1.TabIndex = 19
        '
        'DataGridViewTextBoxColumn24
        '
        Me.DataGridViewTextBoxColumn24.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn24.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn24.Name = "DataGridViewTextBoxColumn24"
        '
        'DataGridViewTextBoxColumn25
        '
        DataGridViewCellStyle11.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn25.DefaultCellStyle = DataGridViewCellStyle11
        Me.DataGridViewTextBoxColumn25.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn25.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn25.Name = "DataGridViewTextBoxColumn25"
        '
        'gridCompLiq2
        '
        Me.gridCompLiq2.AllowUserToAddRows = False
        Me.gridCompLiq2.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompLiq2.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompLiq2.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn26, Me.DataGridViewTextBoxColumn27})
        Me.gridCompLiq2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompLiq2.Location = New System.Drawing.Point(0, 0)
        Me.gridCompLiq2.Name = "gridCompLiq2"
        Me.gridCompLiq2.RowHeadersVisible = False
        Me.gridCompLiq2.Size = New System.Drawing.Size(459, 190)
        Me.gridCompLiq2.TabIndex = 19
        '
        'DataGridViewTextBoxColumn26
        '
        Me.DataGridViewTextBoxColumn26.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn26.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn26.Name = "DataGridViewTextBoxColumn26"
        '
        'DataGridViewTextBoxColumn27
        '
        DataGridViewCellStyle12.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn27.DefaultCellStyle = DataGridViewCellStyle12
        Me.DataGridViewTextBoxColumn27.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn27.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn27.Name = "DataGridViewTextBoxColumn27"
        '
        'gridCompSolid
        '
        Me.gridCompSolid.AllowUserToAddRows = False
        Me.gridCompSolid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridCompSolid.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridCompSolid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn28, Me.DataGridViewTextBoxColumn29})
        Me.gridCompSolid.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridCompSolid.Location = New System.Drawing.Point(0, 0)
        Me.gridCompSolid.Name = "gridCompSolid"
        Me.gridCompSolid.RowHeadersVisible = False
        Me.gridCompSolid.Size = New System.Drawing.Size(459, 190)
        Me.gridCompSolid.TabIndex = 19
        '
        'DataGridViewTextBoxColumn28
        '
        Me.DataGridViewTextBoxColumn28.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn28.HeaderText = "Substância"
        Me.DataGridViewTextBoxColumn28.Name = "DataGridViewTextBoxColumn28"
        '
        'DataGridViewTextBoxColumn29
        '
        DataGridViewCellStyle13.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn29.DefaultCellStyle = DataGridViewCellStyle13
        Me.DataGridViewTextBoxColumn29.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn29.HeaderText = "Quantidade"
        Me.DataGridViewTextBoxColumn29.Name = "DataGridViewTextBoxColumn29"
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label17.Location = New System.Drawing.Point(9, 235)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(69, 13)
        Me.Label17.TabIndex = 40
        Me.Label17.Text = "Fração Molar"
        '
        'rbSpecVapor
        '
        Me.rbSpecVapor.AutoSize = True
        Me.rbSpecVapor.Location = New System.Drawing.Point(89, 233)
        Me.rbSpecVapor.Name = "rbSpecVapor"
        Me.rbSpecVapor.Size = New System.Drawing.Size(53, 17)
        Me.rbSpecVapor.TabIndex = 41
        Me.rbSpecVapor.TabStop = True
        Me.rbSpecVapor.Text = "Vapor"
        Me.rbSpecVapor.UseVisualStyleBackColor = True
        '
        'rbSpecLiquid
        '
        Me.rbSpecLiquid.AutoSize = True
        Me.rbSpecLiquid.Location = New System.Drawing.Point(148, 233)
        Me.rbSpecLiquid.Name = "rbSpecLiquid"
        Me.rbSpecLiquid.Size = New System.Drawing.Size(61, 17)
        Me.rbSpecLiquid.TabIndex = 42
        Me.rbSpecLiquid.TabStop = True
        Me.rbSpecLiquid.Text = "Líquido"
        Me.rbSpecLiquid.UseVisualStyleBackColor = True
        '
        'rbSpecSolid
        '
        Me.rbSpecSolid.AutoSize = True
        Me.rbSpecSolid.Location = New System.Drawing.Point(215, 233)
        Me.rbSpecSolid.Name = "rbSpecSolid"
        Me.rbSpecSolid.Size = New System.Drawing.Size(54, 17)
        Me.rbSpecSolid.TabIndex = 43
        Me.rbSpecSolid.TabStop = True
        Me.rbSpecSolid.Text = "Sólido"
        Me.rbSpecSolid.UseVisualStyleBackColor = True
        '
        'tbFracSpec
        '
        Me.tbFracSpec.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbFracSpec.Location = New System.Drawing.Point(275, 232)
        Me.tbFracSpec.Name = "tbFracSpec"
        Me.tbFracSpec.Size = New System.Drawing.Size(74, 20)
        Me.tbFracSpec.TabIndex = 44
        '
        'gridPropertiesVapor
        '
        Me.gridPropertiesVapor.AllowUserToAddRows = False
        Me.gridPropertiesVapor.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesVapor.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesVapor.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn3, Me.DataGridViewTextBoxColumn4, Me.DataGridViewTextBoxColumn5})
        Me.gridPropertiesVapor.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesVapor.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesVapor.Name = "gridPropertiesVapor"
        Me.gridPropertiesVapor.RowHeadersVisible = False
        Me.gridPropertiesVapor.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesVapor.TabIndex = 20
        '
        'DataGridViewTextBoxColumn3
        '
        Me.DataGridViewTextBoxColumn3.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn3.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn3.Name = "DataGridViewTextBoxColumn3"
        '
        'DataGridViewTextBoxColumn4
        '
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn4.DefaultCellStyle = DataGridViewCellStyle3
        Me.DataGridViewTextBoxColumn4.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn4.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn4.Name = "DataGridViewTextBoxColumn4"
        '
        'DataGridViewTextBoxColumn5
        '
        Me.DataGridViewTextBoxColumn5.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn5.HeaderText = "Unidade"
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        '
        'gridPropertiesLiqMix
        '
        Me.gridPropertiesLiqMix.AllowUserToAddRows = False
        Me.gridPropertiesLiqMix.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.gridPropertiesLiqMix.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.gridPropertiesLiqMix.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn6, Me.DataGridViewTextBoxColumn7, Me.DataGridViewTextBoxColumn8})
        Me.gridPropertiesLiqMix.Dock = System.Windows.Forms.DockStyle.Fill
        Me.gridPropertiesLiqMix.Location = New System.Drawing.Point(0, 0)
        Me.gridPropertiesLiqMix.Name = "gridPropertiesLiqMix"
        Me.gridPropertiesLiqMix.RowHeadersVisible = False
        Me.gridPropertiesLiqMix.Size = New System.Drawing.Size(459, 241)
        Me.gridPropertiesLiqMix.TabIndex = 20
        '
        'DataGridViewTextBoxColumn6
        '
        Me.DataGridViewTextBoxColumn6.FillWeight = 60.0!
        Me.DataGridViewTextBoxColumn6.HeaderText = "Propriedade"
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        '
        'DataGridViewTextBoxColumn7
        '
        DataGridViewCellStyle4.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridViewTextBoxColumn7.DefaultCellStyle = DataGridViewCellStyle4
        Me.DataGridViewTextBoxColumn7.FillWeight = 40.0!
        Me.DataGridViewTextBoxColumn7.HeaderText = "Valor"
        Me.DataGridViewTextBoxColumn7.Name = "DataGridViewTextBoxColumn7"
        '
        'DataGridViewTextBoxColumn8
        '
        Me.DataGridViewTextBoxColumn8.FillWeight = 30.0!
        Me.DataGridViewTextBoxColumn8.HeaderText = "Unidade"
        Me.DataGridViewTextBoxColumn8.Name = "DataGridViewTextBoxColumn8"
        '
        'MaterialStreamEditor
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(889, 717)
        Me.Controls.Add(Me.GroupBox8)
        Me.Controls.Add(Me.GroupBox7)
        Me.Controls.Add(Me.btnExpand)
        Me.Controls.Add(Me.Label18)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.GroupBox4)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "MaterialStreamEditor"
        Me.Text = "MaterialStreamEditor"
        Me.Panel1.ResumeLayout(False)
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        CType(Me.gridInputComposition, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox7.ResumeLayout(False)
        CType(Me.gridPropertiesMixture, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage4.ResumeLayout(False)
        Me.TabPage5.ResumeLayout(False)
        Me.TabPage6.ResumeLayout(False)
        CType(Me.gridPropertiesLiq1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridPropertiesLiq2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridPropertiesSolid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        Me.TabControl2.ResumeLayout(False)
        Me.TabPage7.ResumeLayout(False)
        CType(Me.gridCompMixture, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage8.ResumeLayout(False)
        Me.TabPage9.ResumeLayout(False)
        Me.TabPage10.ResumeLayout(False)
        Me.TabPage11.ResumeLayout(False)
        Me.TabPage12.ResumeLayout(False)
        CType(Me.gridCompVapor, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridCompLiqMix, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridCompLiq1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridCompLiq2, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridCompSolid, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridPropertiesVapor, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.gridPropertiesLiqMix, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents chkActive As System.Windows.Forms.CheckBox
    Friend WithEvents lblConnectedTo As System.Windows.Forms.Label
    Friend WithEvents lblStatus As System.Windows.Forms.Label
    Friend WithEvents lblObject As System.Windows.Forms.Label
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Friend WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents btnDisconnectO As System.Windows.Forms.Button
    Friend WithEvents btnDisconnectI As System.Windows.Forms.Button
    Friend WithEvents cbOutlet As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents cbInlet As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents cbUnitsT As System.Windows.Forms.ComboBox
    Friend WithEvents tbTemp As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents cbSbec As System.Windows.Forms.ComboBox
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents cbUnitsQ As System.Windows.Forms.ComboBox
    Friend WithEvents tbVolFlow As System.Windows.Forms.TextBox
    Friend WithEvents cbUnitsM As System.Windows.Forms.ComboBox
    Friend WithEvents tbMoleFlow As System.Windows.Forms.TextBox
    Friend WithEvents cbUnitsW As System.Windows.Forms.ComboBox
    Friend WithEvents tbMassFlow As System.Windows.Forms.TextBox
    Friend WithEvents cbUnitsP As System.Windows.Forms.ComboBox
    Friend WithEvents tbPressure As System.Windows.Forms.TextBox
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents cbUnitsS As System.Windows.Forms.ComboBox
    Friend WithEvents tbEntr As System.Windows.Forms.TextBox
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents cbUnitsH As System.Windows.Forms.ComboBox
    Friend WithEvents tbEnth As System.Windows.Forms.TextBox
    Friend WithEvents Label14 As System.Windows.Forms.Label
    Friend WithEvents lblTag As System.Windows.Forms.TextBox
    Friend WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Friend WithEvents btnEraseInput As System.Windows.Forms.Button
    Friend WithEvents btnEqualizeInput As System.Windows.Forms.Button
    Friend WithEvents btnNormalizeInput As System.Windows.Forms.Button
    Friend WithEvents gridInputComposition As System.Windows.Forms.DataGridView
    Friend WithEvents cbCompBasis As System.Windows.Forms.ComboBox
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents lblInputAmount As System.Windows.Forms.Label
    Friend WithEvents compname As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents compamount As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents btnExpand As System.Windows.Forms.CheckBox
    Friend WithEvents GroupBox7 As System.Windows.Forms.GroupBox
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents gridPropertiesMixture As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage4 As System.Windows.Forms.TabPage
    Friend WithEvents gridPropertiesLiq1 As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn9 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn10 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn11 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents TabPage5 As System.Windows.Forms.TabPage
    Friend WithEvents gridPropertiesLiq2 As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn12 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn13 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn14 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents TabPage6 As System.Windows.Forms.TabPage
    Friend WithEvents gridPropertiesSolid As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn15 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn16 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn17 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents GroupBox8 As System.Windows.Forms.GroupBox
    Friend WithEvents TabControl2 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage7 As System.Windows.Forms.TabPage
    Friend WithEvents gridCompMixture As System.Windows.Forms.DataGridView
    Friend WithEvents TabPage8 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage9 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage10 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage11 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage12 As System.Windows.Forms.TabPage
    Friend WithEvents cbCalculatedAmountsBasis As System.Windows.Forms.ComboBox
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents DataGridViewTextBoxColumn18 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn19 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridCompVapor As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn20 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn21 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridCompLiqMix As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn22 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn23 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridCompLiq1 As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn24 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn25 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridCompLiq2 As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn26 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn27 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridCompSolid As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn28 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn29 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents tbFracSpec As System.Windows.Forms.TextBox
    Friend WithEvents rbSpecSolid As System.Windows.Forms.RadioButton
    Friend WithEvents rbSpecLiquid As System.Windows.Forms.RadioButton
    Friend WithEvents rbSpecVapor As System.Windows.Forms.RadioButton
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents gridPropertiesVapor As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn3 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn4 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn5 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents gridPropertiesLiqMix As System.Windows.Forms.DataGridView
    Friend WithEvents DataGridViewTextBoxColumn6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn8 As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
