<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_AirCooler

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_AirCooler))
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.cbEnergy = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
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
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.rbUseGlobal = New System.Windows.Forms.RadioButton()
        Me.rbUserDef = New System.Windows.Forms.RadioButton()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbEff = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label43 = New System.Windows.Forms.Label()
        Me.tbOverallUA = New System.Windows.Forms.TextBox()
        Me.Label44 = New System.Windows.Forms.Label()
        Me.lblMaximumHeatExchange = New System.Windows.Forms.Label()
        Me.tbMaxHeatEx = New System.Windows.Forms.TextBox()
        Me.Label42 = New System.Windows.Forms.Label()
        Me.lblHeatExchanged = New System.Windows.Forms.Label()
        Me.tbHeatingChange = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.lblOutletAirTemperature = New System.Windows.Forms.Label()
        Me.tbOutletAirTemp = New System.Windows.Forms.TextBox()
        Me.Label40 = New System.Windows.Forms.Label()
        Me.lblInletAirPressure = New System.Windows.Forms.Label()
        Me.tbInletAirPre = New System.Windows.Forms.TextBox()
        Me.Label38 = New System.Windows.Forms.Label()
        Me.lblInletAirTemperature = New System.Windows.Forms.Label()
        Me.tbInletAirTemp = New System.Windows.Forms.TextBox()
        Me.Label36 = New System.Windows.Forms.Label()
        Me.lblOutletTemperature = New System.Windows.Forms.Label()
        Me.lblPressureDrop = New System.Windows.Forms.Label()
        Me.tbOutletTemperature = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbPressureDrop = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.lbuTubeThermalCond = New System.Windows.Forms.Label()
        Me.tbTubeThermalCond = New System.Windows.Forms.TextBox()
        Me.tbTubeDi = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label32 = New System.Windows.Forms.Label()
        Me.lbTubeRoughness = New System.Windows.Forms.Label()
        Me.lbuTubeDi = New System.Windows.Forms.Label()
        Me.tbTubeRoughness = New System.Windows.Forms.TextBox()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbTubeDe = New System.Windows.Forms.TextBox()
        Me.lbuTubePitch = New System.Windows.Forms.Label()
        Me.lbTubeDe = New System.Windows.Forms.Label()
        Me.tbTubePitch = New System.Windows.Forms.TextBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.tbTubeLength = New System.Windows.Forms.TextBox()
        Me.tbNumberOfTubesPerShell = New System.Windows.Forms.TextBox()
        Me.lbuTubeLength = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.tbTubePassesPerShell = New System.Windows.Forms.TextBox()
        Me.tbTubeFoulingFactor = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.lbuTubeFoulingFactor = New System.Windows.Forms.Label()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.lblElectricalLoad = New System.Windows.Forms.Label()
        Me.tbElecLoad = New System.Windows.Forms.TextBox()
        Me.Label34 = New System.Windows.Forms.Label()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.tbElecConv = New System.Windows.Forms.TextBox()
        Me.Label31 = New System.Windows.Forms.Label()
        Me.lblActualAirFlow = New System.Windows.Forms.Label()
        Me.tbActualAF = New System.Windows.Forms.TextBox()
        Me.Label27 = New System.Windows.Forms.Label()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.tbActualR = New System.Windows.Forms.TextBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.lblRefAirFlow = New System.Windows.Forms.Label()
        Me.tbRefAF = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.tbRefR = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.TabPage4 = New System.Windows.Forms.TabPage()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxConnections
        '
        Me.GroupBoxConnections.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.Label14)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect1)
        Me.GroupBoxConnections.Controls.Add(Me.Label7)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.Label19)
        Me.GroupBoxConnections.Location = New System.Drawing.Point(7, 105)
        Me.GroupBoxConnections.Name = "GroupBoxConnections"
        Me.GroupBoxConnections.Size = New System.Drawing.Size(344, 113)
        Me.GroupBoxConnections.TabIndex = 12
        Me.GroupBoxConnections.TabStop = False
        Me.GroupBoxConnections.Text = "Connections"
        '
        'btnCreateAndConnectEnergy
        '
        Me.btnCreateAndConnectEnergy.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCreateAndConnectEnergy.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.btnCreateAndConnectEnergy.Image = CType(resources.GetObject("btnCreateAndConnectEnergy.Image"), System.Drawing.Image)
        Me.btnCreateAndConnectEnergy.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnCreateAndConnectEnergy.Location = New System.Drawing.Point(288, 77)
        Me.btnCreateAndConnectEnergy.Name = "btnCreateAndConnectEnergy"
        Me.btnCreateAndConnectEnergy.Size = New System.Drawing.Size(21, 21)
        Me.btnCreateAndConnectEnergy.TabIndex = 42
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectEnergy, "Criar e Conectar")
        Me.btnCreateAndConnectEnergy.UseVisualStyleBackColor = True
        '
        'btnDisconnectEnergy
        '
        Me.btnDisconnectEnergy.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnectEnergy.Image = CType(resources.GetObject("btnDisconnectEnergy.Image"), System.Drawing.Image)
        Me.btnDisconnectEnergy.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnectEnergy.Location = New System.Drawing.Point(315, 77)
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.btnDisconnectEnergy.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectEnergy.TabIndex = 23
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectEnergy, "Desconectar")
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet1
        '
        Me.btnCreateAndConnectOutlet1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCreateAndConnectOutlet1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.btnCreateAndConnectOutlet1.Image = CType(resources.GetObject("btnCreateAndConnectOutlet1.Image"), System.Drawing.Image)
        Me.btnCreateAndConnectOutlet1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnCreateAndConnectOutlet1.Location = New System.Drawing.Point(288, 50)
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.btnCreateAndConnectOutlet1.Size = New System.Drawing.Size(21, 21)
        Me.btnCreateAndConnectOutlet1.TabIndex = 41
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet1, "Criar e Conectar")
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label14.Location = New System.Drawing.Point(9, 80)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(76, 13)
        Me.Label14.TabIndex = 22
        Me.Label14.Text = "Energy Stream"
        '
        'btnCreateAndConnectInlet1
        '
        Me.btnCreateAndConnectInlet1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCreateAndConnectInlet1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.btnCreateAndConnectInlet1.Image = CType(resources.GetObject("btnCreateAndConnectInlet1.Image"), System.Drawing.Image)
        Me.btnCreateAndConnectInlet1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnCreateAndConnectInlet1.Location = New System.Drawing.Point(288, 23)
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.btnCreateAndConnectInlet1.Size = New System.Drawing.Size(21, 21)
        Me.btnCreateAndConnectInlet1.TabIndex = 40
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet1, "Criar e Conectar")
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'cbEnergy
        '
        Me.cbEnergy.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbEnergy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy.FormattingEnabled = True
        Me.cbEnergy.Location = New System.Drawing.Point(147, 77)
        Me.cbEnergy.Name = "cbEnergy"
        Me.cbEnergy.Size = New System.Drawing.Size(135, 21)
        Me.cbEnergy.TabIndex = 21
        '
        'btnDisconnectOutlet1
        '
        Me.btnDisconnectOutlet1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnectOutlet1.Image = CType(resources.GetObject("btnDisconnectOutlet1.Image"), System.Drawing.Image)
        Me.btnDisconnectOutlet1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnectOutlet1.Location = New System.Drawing.Point(315, 50)
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.btnDisconnectOutlet1.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectOutlet1.TabIndex = 20
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet1, "Desconectar")
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        Me.btnDisconnect1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnect1.Image = CType(resources.GetObject("btnDisconnect1.Image"), System.Drawing.Image)
        Me.btnDisconnect1.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnect1.Location = New System.Drawing.Point(315, 23)
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.btnDisconnect1.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnect1.TabIndex = 14
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect1, "Desconectar")
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label7.Location = New System.Drawing.Point(9, 53)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(71, 13)
        Me.Label7.TabIndex = 13
        Me.Label7.Text = "Outlet Stream"
        '
        'cbOutlet1
        '
        Me.cbOutlet1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Location = New System.Drawing.Point(147, 50)
        Me.cbOutlet1.Name = "cbOutlet1"
        Me.cbOutlet1.Size = New System.Drawing.Size(135, 21)
        Me.cbOutlet1.TabIndex = 8
        '
        'cbInlet1
        '
        Me.cbInlet1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Location = New System.Drawing.Point(147, 23)
        Me.cbInlet1.Name = "cbInlet1"
        Me.cbInlet1.Size = New System.Drawing.Size(135, 21)
        Me.cbInlet1.TabIndex = 1
        '
        'Label19
        '
        Me.Label19.AutoSize = True
        Me.Label19.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label19.Location = New System.Drawing.Point(8, 26)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(63, 13)
        Me.Label19.TabIndex = 0
        Me.Label19.Text = "Inlet Stream"
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
        Me.GroupBox5.Location = New System.Drawing.Point(7, 5)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(344, 98)
        Me.GroupBox5.TabIndex = 11
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "General Info"
        '
        'lblTag
        '
        Me.lblTag.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblTag.Location = New System.Drawing.Point(133, 19)
        Me.lblTag.Name = "lblTag"
        Me.lblTag.Size = New System.Drawing.Size(203, 20)
        Me.lblTag.TabIndex = 24
        '
        'chkActive
        '
        Me.chkActive.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.chkActive.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkActive.Location = New System.Drawing.Point(315, 43)
        Me.chkActive.Name = "chkActive"
        Me.chkActive.Size = New System.Drawing.Size(21, 21)
        Me.chkActive.TabIndex = 21
        Me.ToolTipValues.SetToolTip(Me.chkActive, "Ativo/Inativo")
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        Me.lblConnectedTo.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblConnectedTo.AutoSize = True
        Me.lblConnectedTo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblConnectedTo.Location = New System.Drawing.Point(132, 72)
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
        Me.lblStatus.Location = New System.Drawing.Point(132, 47)
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
        Me.Label13.Size = New System.Drawing.Size(51, 13)
        Me.Label13.TabIndex = 17
        Me.Label13.Text = "Linked to"
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
        Me.Label11.Text = "Object"
        '
        'GroupBox4
        '
        Me.GroupBox4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Location = New System.Drawing.Point(6, 705)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(344, 223)
        Me.GroupBox4.TabIndex = 10
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Notes"
        '
        'rtbAnnotations
        '
        Me.rtbAnnotations.Dock = System.Windows.Forms.DockStyle.Fill
        Me.rtbAnnotations.Location = New System.Drawing.Point(3, 16)
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.22621}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.rtbAnnotations.Size = New System.Drawing.Size(338, 204)
        Me.rtbAnnotations.TabIndex = 0
        '
        'GroupBoxParameters
        '
        Me.GroupBoxParameters.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxParameters.Controls.Add(Me.TabControl1)
        Me.GroupBoxParameters.Location = New System.Drawing.Point(7, 221)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.Size = New System.Drawing.Size(344, 482)
        Me.GroupBoxParameters.TabIndex = 13
        Me.GroupBoxParameters.TabStop = False
        Me.GroupBoxParameters.Text = "Calculation Parameters"
        '
        'TabControl1
        '
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Controls.Add(Me.TabPage4)
        Me.TabControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TabControl1.Location = New System.Drawing.Point(3, 16)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        Me.TabControl1.Size = New System.Drawing.Size(338, 463)
        Me.TabControl1.TabIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.Label10)
        Me.TabPage1.Controls.Add(Me.rbUseGlobal)
        Me.TabPage1.Controls.Add(Me.rbUserDef)
        Me.TabPage1.Controls.Add(Me.Label1)
        Me.TabPage1.Controls.Add(Me.tbEff)
        Me.TabPage1.Controls.Add(Me.Label9)
        Me.TabPage1.Controls.Add(Me.Label43)
        Me.TabPage1.Controls.Add(Me.tbOverallUA)
        Me.TabPage1.Controls.Add(Me.Label44)
        Me.TabPage1.Controls.Add(Me.lblMaximumHeatExchange)
        Me.TabPage1.Controls.Add(Me.tbMaxHeatEx)
        Me.TabPage1.Controls.Add(Me.Label42)
        Me.TabPage1.Controls.Add(Me.lblHeatExchanged)
        Me.TabPage1.Controls.Add(Me.tbHeatingChange)
        Me.TabPage1.Controls.Add(Me.Label6)
        Me.TabPage1.Controls.Add(Me.lblOutletAirTemperature)
        Me.TabPage1.Controls.Add(Me.tbOutletAirTemp)
        Me.TabPage1.Controls.Add(Me.Label40)
        Me.TabPage1.Controls.Add(Me.lblInletAirPressure)
        Me.TabPage1.Controls.Add(Me.tbInletAirPre)
        Me.TabPage1.Controls.Add(Me.Label38)
        Me.TabPage1.Controls.Add(Me.lblInletAirTemperature)
        Me.TabPage1.Controls.Add(Me.tbInletAirTemp)
        Me.TabPage1.Controls.Add(Me.Label36)
        Me.TabPage1.Controls.Add(Me.lblOutletTemperature)
        Me.TabPage1.Controls.Add(Me.lblPressureDrop)
        Me.TabPage1.Controls.Add(Me.tbOutletTemperature)
        Me.TabPage1.Controls.Add(Me.Label2)
        Me.TabPage1.Controls.Add(Me.tbPressureDrop)
        Me.TabPage1.Controls.Add(Me.Label3)
        Me.TabPage1.Controls.Add(Me.cbCalcMode)
        Me.TabPage1.Controls.Add(Me.Label8)
        Me.TabPage1.Location = New System.Drawing.Point(4, 22)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage1.Size = New System.Drawing.Size(330, 437)
        Me.TabPage1.TabIndex = 0
        Me.TabPage1.Text = "Main"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label10.Location = New System.Drawing.Point(12, 123)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(94, 13)
        Me.Label10.TabIndex = 73
        Me.Label10.Text = "Inlet Air Conditions"
        '
        'rbUseGlobal
        '
        Me.rbUseGlobal.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.rbUseGlobal.AutoSize = True
        Me.rbUseGlobal.Location = New System.Drawing.Point(246, 121)
        Me.rbUseGlobal.Name = "rbUseGlobal"
        Me.rbUseGlobal.Size = New System.Drawing.Size(77, 17)
        Me.rbUseGlobal.TabIndex = 72
        Me.rbUseGlobal.Text = "Use Global"
        Me.rbUseGlobal.UseVisualStyleBackColor = True
        '
        'rbUserDef
        '
        Me.rbUserDef.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.rbUserDef.AutoSize = True
        Me.rbUserDef.Checked = True
        Me.rbUserDef.Location = New System.Drawing.Point(153, 121)
        Me.rbUserDef.Name = "rbUserDef"
        Me.rbUserDef.Size = New System.Drawing.Size(87, 17)
        Me.rbUserDef.TabIndex = 71
        Me.rbUserDef.TabStop = True
        Me.rbUserDef.Text = "User-Defined"
        Me.rbUserDef.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(265, 346)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(15, 13)
        Me.Label1.TabIndex = 70
        Me.Label1.Text = "%"
        '
        'tbEff
        '
        Me.tbEff.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbEff.Location = New System.Drawing.Point(153, 343)
        Me.tbEff.Name = "tbEff"
        Me.tbEff.ReadOnly = True
        Me.tbEff.Size = New System.Drawing.Size(106, 20)
        Me.tbEff.TabIndex = 69
        Me.tbEff.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label9.Location = New System.Drawing.Point(12, 346)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(107, 13)
        Me.Label9.TabIndex = 68
        Me.Label9.Text = "Exchanger Efficiency"
        '
        'Label43
        '
        Me.Label43.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label43.AutoSize = True
        Me.Label43.Location = New System.Drawing.Point(265, 268)
        Me.Label43.Name = "Label43"
        Me.Label43.Size = New System.Drawing.Size(30, 13)
        Me.Label43.TabIndex = 67
        Me.Label43.Text = "W/K"
        '
        'tbOverallUA
        '
        Me.tbOverallUA.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbOverallUA.Location = New System.Drawing.Point(153, 265)
        Me.tbOverallUA.Name = "tbOverallUA"
        Me.tbOverallUA.Size = New System.Drawing.Size(106, 20)
        Me.tbOverallUA.TabIndex = 66
        Me.tbOverallUA.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label44
        '
        Me.Label44.AutoSize = True
        Me.Label44.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label44.Location = New System.Drawing.Point(12, 268)
        Me.Label44.Name = "Label44"
        Me.Label44.Size = New System.Drawing.Size(58, 13)
        Me.Label44.TabIndex = 65
        Me.Label44.Text = "Overall UA"
        '
        'lblMaximumHeatExchange
        '
        Me.lblMaximumHeatExchange.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblMaximumHeatExchange.AutoSize = True
        Me.lblMaximumHeatExchange.Location = New System.Drawing.Point(265, 320)
        Me.lblMaximumHeatExchange.Name = "lblMaximumHeatExchange"
        Me.lblMaximumHeatExchange.Size = New System.Drawing.Size(45, 13)
        Me.lblMaximumHeatExchange.TabIndex = 64
        Me.lblMaximumHeatExchange.Text = "Label41"
        '
        'tbMaxHeatEx
        '
        Me.tbMaxHeatEx.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbMaxHeatEx.Location = New System.Drawing.Point(153, 317)
        Me.tbMaxHeatEx.Name = "tbMaxHeatEx"
        Me.tbMaxHeatEx.ReadOnly = True
        Me.tbMaxHeatEx.Size = New System.Drawing.Size(106, 20)
        Me.tbMaxHeatEx.TabIndex = 63
        Me.tbMaxHeatEx.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label42
        '
        Me.Label42.AutoSize = True
        Me.Label42.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label42.Location = New System.Drawing.Point(12, 320)
        Me.Label42.Name = "Label42"
        Me.Label42.Size = New System.Drawing.Size(128, 13)
        Me.Label42.TabIndex = 62
        Me.Label42.Text = "Maximum Heat Exchange"
        '
        'lblHeatExchanged
        '
        Me.lblHeatExchanged.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblHeatExchanged.AutoSize = True
        Me.lblHeatExchanged.Location = New System.Drawing.Point(265, 294)
        Me.lblHeatExchanged.Name = "lblHeatExchanged"
        Me.lblHeatExchanged.Size = New System.Drawing.Size(45, 13)
        Me.lblHeatExchanged.TabIndex = 61
        Me.lblHeatExchanged.Text = "Label10"
        '
        'tbHeatingChange
        '
        Me.tbHeatingChange.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbHeatingChange.Location = New System.Drawing.Point(153, 291)
        Me.tbHeatingChange.Name = "tbHeatingChange"
        Me.tbHeatingChange.ReadOnly = True
        Me.tbHeatingChange.Size = New System.Drawing.Size(106, 20)
        Me.tbHeatingChange.TabIndex = 60
        Me.tbHeatingChange.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(12, 294)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(87, 13)
        Me.Label6.TabIndex = 59
        Me.Label6.Text = "Heat Exchanged"
        '
        'lblOutletAirTemperature
        '
        Me.lblOutletAirTemperature.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblOutletAirTemperature.AutoSize = True
        Me.lblOutletAirTemperature.Location = New System.Drawing.Point(265, 221)
        Me.lblOutletAirTemperature.Name = "lblOutletAirTemperature"
        Me.lblOutletAirTemperature.Size = New System.Drawing.Size(45, 13)
        Me.lblOutletAirTemperature.TabIndex = 58
        Me.lblOutletAirTemperature.Text = "Label39"
        '
        'tbOutletAirTemp
        '
        Me.tbOutletAirTemp.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbOutletAirTemp.Location = New System.Drawing.Point(153, 218)
        Me.tbOutletAirTemp.Name = "tbOutletAirTemp"
        Me.tbOutletAirTemp.ReadOnly = True
        Me.tbOutletAirTemp.Size = New System.Drawing.Size(106, 20)
        Me.tbOutletAirTemp.TabIndex = 57
        Me.tbOutletAirTemp.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label40
        '
        Me.Label40.AutoSize = True
        Me.Label40.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label40.Location = New System.Drawing.Point(12, 221)
        Me.Label40.Name = "Label40"
        Me.Label40.Size = New System.Drawing.Size(113, 13)
        Me.Label40.TabIndex = 56
        Me.Label40.Text = "Outlet Air Temperature"
        '
        'lblInletAirPressure
        '
        Me.lblInletAirPressure.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblInletAirPressure.AutoSize = True
        Me.lblInletAirPressure.Location = New System.Drawing.Point(265, 175)
        Me.lblInletAirPressure.Name = "lblInletAirPressure"
        Me.lblInletAirPressure.Size = New System.Drawing.Size(45, 13)
        Me.lblInletAirPressure.TabIndex = 55
        Me.lblInletAirPressure.Text = "Label37"
        '
        'tbInletAirPre
        '
        Me.tbInletAirPre.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbInletAirPre.Location = New System.Drawing.Point(153, 172)
        Me.tbInletAirPre.Name = "tbInletAirPre"
        Me.tbInletAirPre.Size = New System.Drawing.Size(106, 20)
        Me.tbInletAirPre.TabIndex = 54
        Me.tbInletAirPre.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label38
        '
        Me.Label38.AutoSize = True
        Me.Label38.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label38.Location = New System.Drawing.Point(12, 175)
        Me.Label38.Name = "Label38"
        Me.Label38.Size = New System.Drawing.Size(86, 13)
        Me.Label38.TabIndex = 53
        Me.Label38.Text = "Inlet Air Pressure"
        '
        'lblInletAirTemperature
        '
        Me.lblInletAirTemperature.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblInletAirTemperature.AutoSize = True
        Me.lblInletAirTemperature.Location = New System.Drawing.Point(265, 149)
        Me.lblInletAirTemperature.Name = "lblInletAirTemperature"
        Me.lblInletAirTemperature.Size = New System.Drawing.Size(45, 13)
        Me.lblInletAirTemperature.TabIndex = 52
        Me.lblInletAirTemperature.Text = "Label35"
        '
        'tbInletAirTemp
        '
        Me.tbInletAirTemp.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbInletAirTemp.Location = New System.Drawing.Point(153, 146)
        Me.tbInletAirTemp.Name = "tbInletAirTemp"
        Me.tbInletAirTemp.Size = New System.Drawing.Size(106, 20)
        Me.tbInletAirTemp.TabIndex = 51
        Me.tbInletAirTemp.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label36
        '
        Me.Label36.AutoSize = True
        Me.Label36.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label36.Location = New System.Drawing.Point(12, 149)
        Me.Label36.Name = "Label36"
        Me.Label36.Size = New System.Drawing.Size(105, 13)
        Me.Label36.TabIndex = 50
        Me.Label36.Text = "Inlet Air Temperature"
        '
        'lblOutletTemperature
        '
        Me.lblOutletTemperature.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblOutletTemperature.AutoSize = True
        Me.lblOutletTemperature.Location = New System.Drawing.Point(265, 79)
        Me.lblOutletTemperature.Name = "lblOutletTemperature"
        Me.lblOutletTemperature.Size = New System.Drawing.Size(39, 13)
        Me.lblOutletTemperature.TabIndex = 48
        Me.lblOutletTemperature.Text = "Label9"
        '
        'lblPressureDrop
        '
        Me.lblPressureDrop.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblPressureDrop.AutoSize = True
        Me.lblPressureDrop.Location = New System.Drawing.Point(265, 53)
        Me.lblPressureDrop.Name = "lblPressureDrop"
        Me.lblPressureDrop.Size = New System.Drawing.Size(39, 13)
        Me.lblPressureDrop.TabIndex = 47
        Me.lblPressureDrop.Text = "Label1"
        '
        'tbOutletTemperature
        '
        Me.tbOutletTemperature.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbOutletTemperature.Location = New System.Drawing.Point(153, 76)
        Me.tbOutletTemperature.Name = "tbOutletTemperature"
        Me.tbOutletTemperature.Size = New System.Drawing.Size(106, 20)
        Me.tbOutletTemperature.TabIndex = 44
        Me.tbOutletTemperature.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(12, 79)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(123, 13)
        Me.Label2.TabIndex = 43
        Me.Label2.Text = "Outlet Fluid Temperature"
        '
        'tbPressureDrop
        '
        Me.tbPressureDrop.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbPressureDrop.Location = New System.Drawing.Point(153, 50)
        Me.tbPressureDrop.Name = "tbPressureDrop"
        Me.tbPressureDrop.Size = New System.Drawing.Size(106, 20)
        Me.tbPressureDrop.TabIndex = 42
        Me.tbPressureDrop.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(12, 53)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(99, 13)
        Me.Label3.TabIndex = 41
        Me.Label3.Text = "Fluid Pressure Drop"
        '
        'cbCalcMode
        '
        Me.cbCalcMode.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {"Specify Outlet Temperature", "Specify Tube Geometry", "Specify Overall UA"})
        Me.cbCalcMode.Location = New System.Drawing.Point(153, 15)
        Me.cbCalcMode.Name = "cbCalcMode"
        Me.cbCalcMode.Size = New System.Drawing.Size(157, 21)
        Me.cbCalcMode.TabIndex = 40
        '
        'Label8
        '
        Me.Label8.AutoSize = True
        Me.Label8.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label8.Location = New System.Drawing.Point(12, 19)
        Me.Label8.Name = "Label8"
        Me.Label8.Size = New System.Drawing.Size(86, 13)
        Me.Label8.TabIndex = 39
        Me.Label8.Text = "Calculation Type"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.lbuTubeThermalCond)
        Me.TabPage2.Controls.Add(Me.tbTubeThermalCond)
        Me.TabPage2.Controls.Add(Me.tbTubeDi)
        Me.TabPage2.Controls.Add(Me.Label5)
        Me.TabPage2.Controls.Add(Me.Label32)
        Me.TabPage2.Controls.Add(Me.lbTubeRoughness)
        Me.TabPage2.Controls.Add(Me.lbuTubeDi)
        Me.TabPage2.Controls.Add(Me.tbTubeRoughness)
        Me.TabPage2.Controls.Add(Me.Label30)
        Me.TabPage2.Controls.Add(Me.Label4)
        Me.TabPage2.Controls.Add(Me.tbTubeDe)
        Me.TabPage2.Controls.Add(Me.lbuTubePitch)
        Me.TabPage2.Controls.Add(Me.lbTubeDe)
        Me.TabPage2.Controls.Add(Me.tbTubePitch)
        Me.TabPage2.Controls.Add(Me.Label28)
        Me.TabPage2.Controls.Add(Me.Label20)
        Me.TabPage2.Controls.Add(Me.tbTubeLength)
        Me.TabPage2.Controls.Add(Me.tbNumberOfTubesPerShell)
        Me.TabPage2.Controls.Add(Me.lbuTubeLength)
        Me.TabPage2.Controls.Add(Me.Label22)
        Me.TabPage2.Controls.Add(Me.Label26)
        Me.TabPage2.Controls.Add(Me.tbTubePassesPerShell)
        Me.TabPage2.Controls.Add(Me.tbTubeFoulingFactor)
        Me.TabPage2.Controls.Add(Me.Label24)
        Me.TabPage2.Controls.Add(Me.lbuTubeFoulingFactor)
        Me.TabPage2.Location = New System.Drawing.Point(4, 22)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage2.Size = New System.Drawing.Size(330, 437)
        Me.TabPage2.TabIndex = 1
        Me.TabPage2.Text = "Tubes"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'lbuTubeThermalCond
        '
        Me.lbuTubeThermalCond.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbuTubeThermalCond.AutoSize = True
        Me.lbuTubeThermalCond.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbuTubeThermalCond.Location = New System.Drawing.Point(266, 146)
        Me.lbuTubeThermalCond.Name = "lbuTubeThermalCond"
        Me.lbuTubeThermalCond.Size = New System.Drawing.Size(45, 13)
        Me.lbuTubeThermalCond.TabIndex = 60
        Me.lbuTubeThermalCond.Text = "Label31"
        '
        'tbTubeThermalCond
        '
        Me.tbTubeThermalCond.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeThermalCond.Location = New System.Drawing.Point(177, 143)
        Me.tbTubeThermalCond.Name = "tbTubeThermalCond"
        Me.tbTubeThermalCond.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeThermalCond.TabIndex = 59
        Me.tbTubeThermalCond.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbTubeDi
        '
        Me.tbTubeDi.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeDi.Location = New System.Drawing.Point(177, 13)
        Me.tbTubeDi.Name = "tbTubeDi"
        Me.tbTubeDi.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeDi.TabIndex = 26
        Me.tbTubeDi.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(14, 146)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(106, 13)
        Me.Label5.TabIndex = 58
        Me.Label5.Text = "Thermal Conductivity"
        '
        'Label32
        '
        Me.Label32.AutoSize = True
        Me.Label32.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label32.Location = New System.Drawing.Point(13, 16)
        Me.Label32.Name = "Label32"
        Me.Label32.Size = New System.Drawing.Size(87, 13)
        Me.Label32.TabIndex = 25
        Me.Label32.Text = "Internal Diameter"
        '
        'lbTubeRoughness
        '
        Me.lbTubeRoughness.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbTubeRoughness.AutoSize = True
        Me.lbTubeRoughness.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbTubeRoughness.Location = New System.Drawing.Point(267, 120)
        Me.lbTubeRoughness.Name = "lbTubeRoughness"
        Me.lbTubeRoughness.Size = New System.Drawing.Size(45, 13)
        Me.lbTubeRoughness.TabIndex = 54
        Me.lbTubeRoughness.Text = "Label31"
        '
        'lbuTubeDi
        '
        Me.lbuTubeDi.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbuTubeDi.AutoSize = True
        Me.lbuTubeDi.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbuTubeDi.Location = New System.Drawing.Point(266, 16)
        Me.lbuTubeDi.Name = "lbuTubeDi"
        Me.lbuTubeDi.Size = New System.Drawing.Size(45, 13)
        Me.lbuTubeDi.TabIndex = 28
        Me.lbuTubeDi.Text = "Label31"
        '
        'tbTubeRoughness
        '
        Me.tbTubeRoughness.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeRoughness.Location = New System.Drawing.Point(177, 117)
        Me.tbTubeRoughness.Name = "tbTubeRoughness"
        Me.tbTubeRoughness.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeRoughness.TabIndex = 53
        Me.tbTubeRoughness.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label30
        '
        Me.Label30.AutoSize = True
        Me.Label30.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label30.Location = New System.Drawing.Point(13, 42)
        Me.Label30.Name = "Label30"
        Me.Label30.Size = New System.Drawing.Size(90, 13)
        Me.Label30.TabIndex = 29
        Me.Label30.Text = "External Diameter"
        '
        'Label4
        '
        Me.Label4.AutoSize = True
        Me.Label4.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label4.Location = New System.Drawing.Point(13, 120)
        Me.Label4.Name = "Label4"
        Me.Label4.Size = New System.Drawing.Size(61, 13)
        Me.Label4.TabIndex = 52
        Me.Label4.Text = "Roughness"
        '
        'tbTubeDe
        '
        Me.tbTubeDe.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeDe.Location = New System.Drawing.Point(177, 39)
        Me.tbTubeDe.Name = "tbTubeDe"
        Me.tbTubeDe.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeDe.TabIndex = 30
        Me.tbTubeDe.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lbuTubePitch
        '
        Me.lbuTubePitch.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbuTubePitch.AutoSize = True
        Me.lbuTubePitch.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbuTubePitch.Location = New System.Drawing.Point(266, 224)
        Me.lbuTubePitch.Name = "lbuTubePitch"
        Me.lbuTubePitch.Size = New System.Drawing.Size(45, 13)
        Me.lbuTubePitch.TabIndex = 51
        Me.lbuTubePitch.Text = "Label19"
        '
        'lbTubeDe
        '
        Me.lbTubeDe.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbTubeDe.AutoSize = True
        Me.lbTubeDe.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbTubeDe.Location = New System.Drawing.Point(266, 42)
        Me.lbTubeDe.Name = "lbTubeDe"
        Me.lbTubeDe.Size = New System.Drawing.Size(45, 13)
        Me.lbTubeDe.TabIndex = 31
        Me.lbTubeDe.Text = "Label29"
        '
        'tbTubePitch
        '
        Me.tbTubePitch.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubePitch.Location = New System.Drawing.Point(177, 221)
        Me.tbTubePitch.Name = "tbTubePitch"
        Me.tbTubePitch.Size = New System.Drawing.Size(83, 20)
        Me.tbTubePitch.TabIndex = 50
        Me.tbTubePitch.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label28
        '
        Me.Label28.AutoSize = True
        Me.Label28.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label28.Location = New System.Drawing.Point(13, 68)
        Me.Label28.Name = "Label28"
        Me.Label28.Size = New System.Drawing.Size(40, 13)
        Me.Label28.TabIndex = 32
        Me.Label28.Text = "Length"
        '
        'Label20
        '
        Me.Label20.AutoSize = True
        Me.Label20.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label20.Location = New System.Drawing.Point(13, 224)
        Me.Label20.Name = "Label20"
        Me.Label20.Size = New System.Drawing.Size(74, 13)
        Me.Label20.TabIndex = 49
        Me.Label20.Text = "Tube Spacing"
        '
        'tbTubeLength
        '
        Me.tbTubeLength.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeLength.Location = New System.Drawing.Point(177, 65)
        Me.tbTubeLength.Name = "tbTubeLength"
        Me.tbTubeLength.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeLength.TabIndex = 33
        Me.tbTubeLength.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbNumberOfTubesPerShell
        '
        Me.tbNumberOfTubesPerShell.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbNumberOfTubesPerShell.Location = New System.Drawing.Point(177, 195)
        Me.tbNumberOfTubesPerShell.Name = "tbNumberOfTubesPerShell"
        Me.tbNumberOfTubesPerShell.Size = New System.Drawing.Size(83, 20)
        Me.tbNumberOfTubesPerShell.TabIndex = 42
        Me.tbNumberOfTubesPerShell.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lbuTubeLength
        '
        Me.lbuTubeLength.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbuTubeLength.AutoSize = True
        Me.lbuTubeLength.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbuTubeLength.Location = New System.Drawing.Point(266, 68)
        Me.lbuTubeLength.Name = "lbuTubeLength"
        Me.lbuTubeLength.Size = New System.Drawing.Size(45, 13)
        Me.lbuTubeLength.TabIndex = 34
        Me.lbuTubeLength.Text = "Label27"
        '
        'Label22
        '
        Me.Label22.AutoSize = True
        Me.Label22.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label22.Location = New System.Drawing.Point(13, 198)
        Me.Label22.Name = "Label22"
        Me.Label22.Size = New System.Drawing.Size(89, 13)
        Me.Label22.TabIndex = 41
        Me.Label22.Text = "Number of Tubes"
        '
        'Label26
        '
        Me.Label26.AutoSize = True
        Me.Label26.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label26.Location = New System.Drawing.Point(13, 94)
        Me.Label26.Name = "Label26"
        Me.Label26.Size = New System.Drawing.Size(74, 13)
        Me.Label26.TabIndex = 35
        Me.Label26.Text = "Fouling Factor"
        '
        'tbTubePassesPerShell
        '
        Me.tbTubePassesPerShell.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubePassesPerShell.Location = New System.Drawing.Point(177, 169)
        Me.tbTubePassesPerShell.Name = "tbTubePassesPerShell"
        Me.tbTubePassesPerShell.Size = New System.Drawing.Size(83, 20)
        Me.tbTubePassesPerShell.TabIndex = 39
        Me.tbTubePassesPerShell.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'tbTubeFoulingFactor
        '
        Me.tbTubeFoulingFactor.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbTubeFoulingFactor.Location = New System.Drawing.Point(177, 91)
        Me.tbTubeFoulingFactor.Name = "tbTubeFoulingFactor"
        Me.tbTubeFoulingFactor.Size = New System.Drawing.Size(83, 20)
        Me.tbTubeFoulingFactor.TabIndex = 36
        Me.tbTubeFoulingFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label24
        '
        Me.Label24.AutoSize = True
        Me.Label24.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label24.Location = New System.Drawing.Point(13, 172)
        Me.Label24.Name = "Label24"
        Me.Label24.Size = New System.Drawing.Size(93, 13)
        Me.Label24.TabIndex = 38
        Me.Label24.Text = "Number of Passes"
        '
        'lbuTubeFoulingFactor
        '
        Me.lbuTubeFoulingFactor.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbuTubeFoulingFactor.AutoSize = True
        Me.lbuTubeFoulingFactor.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lbuTubeFoulingFactor.Location = New System.Drawing.Point(266, 94)
        Me.lbuTubeFoulingFactor.Name = "lbuTubeFoulingFactor"
        Me.lbuTubeFoulingFactor.Size = New System.Drawing.Size(45, 13)
        Me.lbuTubeFoulingFactor.TabIndex = 37
        Me.lbuTubeFoulingFactor.Text = "Label25"
        '
        'TabPage3
        '
        Me.TabPage3.Controls.Add(Me.lblElectricalLoad)
        Me.TabPage3.Controls.Add(Me.tbElecLoad)
        Me.TabPage3.Controls.Add(Me.Label34)
        Me.TabPage3.Controls.Add(Me.Label29)
        Me.TabPage3.Controls.Add(Me.tbElecConv)
        Me.TabPage3.Controls.Add(Me.Label31)
        Me.TabPage3.Controls.Add(Me.lblActualAirFlow)
        Me.TabPage3.Controls.Add(Me.tbActualAF)
        Me.TabPage3.Controls.Add(Me.Label27)
        Me.TabPage3.Controls.Add(Me.Label21)
        Me.TabPage3.Controls.Add(Me.tbActualR)
        Me.TabPage3.Controls.Add(Me.Label23)
        Me.TabPage3.Controls.Add(Me.lblRefAirFlow)
        Me.TabPage3.Controls.Add(Me.tbRefAF)
        Me.TabPage3.Controls.Add(Me.Label18)
        Me.TabPage3.Controls.Add(Me.Label15)
        Me.TabPage3.Controls.Add(Me.tbRefR)
        Me.TabPage3.Controls.Add(Me.Label16)
        Me.TabPage3.Location = New System.Drawing.Point(4, 22)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.Size = New System.Drawing.Size(330, 437)
        Me.TabPage3.TabIndex = 2
        Me.TabPage3.Text = "Fan"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'lblElectricalLoad
        '
        Me.lblElectricalLoad.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblElectricalLoad.AutoSize = True
        Me.lblElectricalLoad.Location = New System.Drawing.Point(281, 146)
        Me.lblElectricalLoad.Name = "lblElectricalLoad"
        Me.lblElectricalLoad.Size = New System.Drawing.Size(45, 13)
        Me.lblElectricalLoad.TabIndex = 67
        Me.lblElectricalLoad.Text = "Label33"
        '
        'tbElecLoad
        '
        Me.tbElecLoad.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbElecLoad.Location = New System.Drawing.Point(168, 143)
        Me.tbElecLoad.Name = "tbElecLoad"
        Me.tbElecLoad.ReadOnly = True
        Me.tbElecLoad.Size = New System.Drawing.Size(105, 20)
        Me.tbElecLoad.TabIndex = 66
        Me.tbElecLoad.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label34
        '
        Me.Label34.AutoSize = True
        Me.Label34.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label34.Location = New System.Drawing.Point(12, 146)
        Me.Label34.Name = "Label34"
        Me.Label34.Size = New System.Drawing.Size(110, 13)
        Me.Label34.TabIndex = 65
        Me.Label34.Text = "Electrical Power Load"
        '
        'Label29
        '
        Me.Label29.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label29.AutoSize = True
        Me.Label29.Location = New System.Drawing.Point(281, 120)
        Me.Label29.Name = "Label29"
        Me.Label29.Size = New System.Drawing.Size(46, 13)
        Me.Label29.TabIndex = 64
        Me.Label29.Text = "kW/rpm"
        '
        'tbElecConv
        '
        Me.tbElecConv.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbElecConv.Location = New System.Drawing.Point(213, 117)
        Me.tbElecConv.Name = "tbElecConv"
        Me.tbElecConv.Size = New System.Drawing.Size(60, 20)
        Me.tbElecConv.TabIndex = 63
        Me.tbElecConv.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label31
        '
        Me.Label31.AutoSize = True
        Me.Label31.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label31.Location = New System.Drawing.Point(12, 120)
        Me.Label31.Name = "Label31"
        Me.Label31.Size = New System.Drawing.Size(172, 13)
        Me.Label31.TabIndex = 62
        Me.Label31.Text = "Electrical Power Conversion Factor"
        '
        'lblActualAirFlow
        '
        Me.lblActualAirFlow.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblActualAirFlow.AutoSize = True
        Me.lblActualAirFlow.Location = New System.Drawing.Point(281, 94)
        Me.lblActualAirFlow.Name = "lblActualAirFlow"
        Me.lblActualAirFlow.Size = New System.Drawing.Size(45, 13)
        Me.lblActualAirFlow.TabIndex = 61
        Me.lblActualAirFlow.Text = "Label25"
        '
        'tbActualAF
        '
        Me.tbActualAF.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbActualAF.Location = New System.Drawing.Point(168, 91)
        Me.tbActualAF.Name = "tbActualAF"
        Me.tbActualAF.ReadOnly = True
        Me.tbActualAF.Size = New System.Drawing.Size(105, 20)
        Me.tbActualAF.TabIndex = 60
        Me.tbActualAF.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label27
        '
        Me.Label27.AutoSize = True
        Me.Label27.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label27.Location = New System.Drawing.Point(12, 94)
        Me.Label27.Name = "Label27"
        Me.Label27.Size = New System.Drawing.Size(77, 13)
        Me.Label27.TabIndex = 59
        Me.Label27.Text = "Actual Air Flow"
        '
        'Label21
        '
        Me.Label21.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label21.AutoSize = True
        Me.Label21.Location = New System.Drawing.Point(281, 68)
        Me.Label21.Name = "Label21"
        Me.Label21.Size = New System.Drawing.Size(24, 13)
        Me.Label21.TabIndex = 58
        Me.Label21.Text = "rpm"
        '
        'tbActualR
        '
        Me.tbActualR.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbActualR.Location = New System.Drawing.Point(168, 65)
        Me.tbActualR.Name = "tbActualR"
        Me.tbActualR.Size = New System.Drawing.Size(105, 20)
        Me.tbActualR.TabIndex = 57
        Me.tbActualR.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label23
        '
        Me.Label23.AutoSize = True
        Me.Label23.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label23.Location = New System.Drawing.Point(12, 68)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(80, 13)
        Me.Label23.TabIndex = 56
        Me.Label23.Text = "Actual Rotation"
        '
        'lblRefAirFlow
        '
        Me.lblRefAirFlow.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblRefAirFlow.AutoSize = True
        Me.lblRefAirFlow.Location = New System.Drawing.Point(281, 42)
        Me.lblRefAirFlow.Name = "lblRefAirFlow"
        Me.lblRefAirFlow.Size = New System.Drawing.Size(45, 13)
        Me.lblRefAirFlow.TabIndex = 55
        Me.lblRefAirFlow.Text = "Label17"
        '
        'tbRefAF
        '
        Me.tbRefAF.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbRefAF.Location = New System.Drawing.Point(168, 39)
        Me.tbRefAF.Name = "tbRefAF"
        Me.tbRefAF.Size = New System.Drawing.Size(105, 20)
        Me.tbRefAF.TabIndex = 54
        Me.tbRefAF.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label18
        '
        Me.Label18.AutoSize = True
        Me.Label18.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label18.Location = New System.Drawing.Point(12, 42)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(97, 13)
        Me.Label18.TabIndex = 53
        Me.Label18.Text = "Reference Air Flow"
        '
        'Label15
        '
        Me.Label15.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label15.AutoSize = True
        Me.Label15.Location = New System.Drawing.Point(281, 18)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(24, 13)
        Me.Label15.TabIndex = 52
        Me.Label15.Text = "rpm"
        '
        'tbRefR
        '
        Me.tbRefR.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbRefR.Location = New System.Drawing.Point(168, 13)
        Me.tbRefR.Name = "tbRefR"
        Me.tbRefR.Size = New System.Drawing.Size(105, 20)
        Me.tbRefR.TabIndex = 51
        Me.tbRefR.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label16
        '
        Me.Label16.AutoSize = True
        Me.Label16.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label16.Location = New System.Drawing.Point(12, 16)
        Me.Label16.Name = "Label16"
        Me.Label16.Size = New System.Drawing.Size(100, 13)
        Me.Label16.TabIndex = 50
        Me.Label16.Text = "Reference Rotation"
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'TabPage4
        '
        Me.TabPage4.Location = New System.Drawing.Point(4, 22)
        Me.TabPage4.Name = "TabPage4"
        Me.TabPage4.Padding = New System.Windows.Forms.Padding(3)
        Me.TabPage4.Size = New System.Drawing.Size(330, 437)
        Me.TabPage4.TabIndex = 3
        Me.TabPage4.Text = "GHG Emissions"
        Me.TabPage4.UseVisualStyleBackColor = True
        '
        'EditingForm_AirCooler
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(359, 940)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "EditingForm_AirCooler"
        Me.Text = "EditingForm_AirCooler"
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage3.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBoxConnections As Windows.Forms.GroupBox
    Public WithEvents btnCreateAndConnectEnergy As Windows.Forms.Button
    Public WithEvents btnDisconnectEnergy As Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As Windows.Forms.Button
    Public WithEvents Label14 As Windows.Forms.Label
    Public WithEvents btnCreateAndConnectInlet1 As Windows.Forms.Button
    Public WithEvents cbEnergy As Windows.Forms.ComboBox
    Public WithEvents btnDisconnectOutlet1 As Windows.Forms.Button
    Public WithEvents btnDisconnect1 As Windows.Forms.Button
    Public WithEvents Label7 As Windows.Forms.Label
    Public WithEvents cbOutlet1 As Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As Windows.Forms.ComboBox
    Public WithEvents Label19 As Windows.Forms.Label
    Public WithEvents GroupBox5 As Windows.Forms.GroupBox
    Public WithEvents lblTag As Windows.Forms.TextBox
    Public WithEvents chkActive As Windows.Forms.CheckBox
    Public WithEvents lblConnectedTo As Windows.Forms.Label
    Public WithEvents lblStatus As Windows.Forms.Label
    Public WithEvents Label13 As Windows.Forms.Label
    Public WithEvents Label12 As Windows.Forms.Label
    Public WithEvents Label11 As Windows.Forms.Label
    Public WithEvents GroupBox4 As Windows.Forms.GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Friend WithEvents TabControl1 As Windows.Forms.TabControl
    Friend WithEvents TabPage1 As Windows.Forms.TabPage
    Friend WithEvents TabPage2 As Windows.Forms.TabPage
    Friend WithEvents TabPage3 As Windows.Forms.TabPage
    Public WithEvents tbOutletTemperature As Windows.Forms.TextBox
    Public WithEvents Label2 As Windows.Forms.Label
    Public WithEvents tbPressureDrop As Windows.Forms.TextBox
    Public WithEvents Label3 As Windows.Forms.Label
    Public WithEvents cbCalcMode As Windows.Forms.ComboBox
    Public WithEvents Label8 As Windows.Forms.Label
    Public WithEvents lbuTubeThermalCond As Windows.Forms.Label
    Public WithEvents tbTubeThermalCond As Windows.Forms.TextBox
    Public WithEvents tbTubeDi As Windows.Forms.TextBox
    Public WithEvents Label5 As Windows.Forms.Label
    Public WithEvents Label32 As Windows.Forms.Label
    Public WithEvents lbTubeRoughness As Windows.Forms.Label
    Public WithEvents lbuTubeDi As Windows.Forms.Label
    Public WithEvents tbTubeRoughness As Windows.Forms.TextBox
    Public WithEvents Label30 As Windows.Forms.Label
    Public WithEvents Label4 As Windows.Forms.Label
    Public WithEvents tbTubeDe As Windows.Forms.TextBox
    Public WithEvents lbuTubePitch As Windows.Forms.Label
    Public WithEvents lbTubeDe As Windows.Forms.Label
    Public WithEvents tbTubePitch As Windows.Forms.TextBox
    Public WithEvents Label28 As Windows.Forms.Label
    Public WithEvents Label20 As Windows.Forms.Label
    Public WithEvents tbTubeLength As Windows.Forms.TextBox
    Public WithEvents tbNumberOfTubesPerShell As Windows.Forms.TextBox
    Public WithEvents lbuTubeLength As Windows.Forms.Label
    Public WithEvents Label22 As Windows.Forms.Label
    Public WithEvents Label26 As Windows.Forms.Label
    Public WithEvents tbTubePassesPerShell As Windows.Forms.TextBox
    Public WithEvents tbTubeFoulingFactor As Windows.Forms.TextBox
    Public WithEvents Label24 As Windows.Forms.Label
    Public WithEvents lbuTubeFoulingFactor As Windows.Forms.Label
    Friend WithEvents lblOutletTemperature As Windows.Forms.Label
    Friend WithEvents lblPressureDrop As Windows.Forms.Label
    Friend WithEvents lblActualAirFlow As Windows.Forms.Label
    Public WithEvents tbActualAF As Windows.Forms.TextBox
    Public WithEvents Label27 As Windows.Forms.Label
    Friend WithEvents Label21 As Windows.Forms.Label
    Public WithEvents tbActualR As Windows.Forms.TextBox
    Public WithEvents Label23 As Windows.Forms.Label
    Friend WithEvents lblRefAirFlow As Windows.Forms.Label
    Public WithEvents tbRefAF As Windows.Forms.TextBox
    Public WithEvents Label18 As Windows.Forms.Label
    Friend WithEvents Label15 As Windows.Forms.Label
    Public WithEvents tbRefR As Windows.Forms.TextBox
    Public WithEvents Label16 As Windows.Forms.Label
    Friend WithEvents Label29 As Windows.Forms.Label
    Public WithEvents tbElecConv As Windows.Forms.TextBox
    Public WithEvents Label31 As Windows.Forms.Label
    Friend WithEvents lblElectricalLoad As Windows.Forms.Label
    Public WithEvents tbElecLoad As Windows.Forms.TextBox
    Public WithEvents Label34 As Windows.Forms.Label
    Friend WithEvents lblOutletAirTemperature As Windows.Forms.Label
    Public WithEvents tbOutletAirTemp As Windows.Forms.TextBox
    Public WithEvents Label40 As Windows.Forms.Label
    Friend WithEvents lblInletAirPressure As Windows.Forms.Label
    Public WithEvents tbInletAirPre As Windows.Forms.TextBox
    Public WithEvents Label38 As Windows.Forms.Label
    Friend WithEvents lblInletAirTemperature As Windows.Forms.Label
    Public WithEvents tbInletAirTemp As Windows.Forms.TextBox
    Public WithEvents Label36 As Windows.Forms.Label
    Friend WithEvents lblMaximumHeatExchange As Windows.Forms.Label
    Public WithEvents tbMaxHeatEx As Windows.Forms.TextBox
    Public WithEvents Label42 As Windows.Forms.Label
    Friend WithEvents lblHeatExchanged As Windows.Forms.Label
    Public WithEvents tbHeatingChange As Windows.Forms.TextBox
    Public WithEvents Label6 As Windows.Forms.Label
    Friend WithEvents Label43 As Windows.Forms.Label
    Public WithEvents tbOverallUA As Windows.Forms.TextBox
    Public WithEvents Label44 As Windows.Forms.Label
    Friend WithEvents Label1 As Windows.Forms.Label
    Public WithEvents tbEff As Windows.Forms.TextBox
    Public WithEvents Label9 As Windows.Forms.Label
    Friend WithEvents ToolTipChangeTag As Windows.Forms.ToolTip
    Public WithEvents Label10 As Label
    Friend WithEvents rbUseGlobal As RadioButton
    Friend WithEvents rbUserDef As RadioButton
    Public WithEvents GroupBoxParameters As GroupBox
    Friend WithEvents TabPage4 As TabPage
End Class
