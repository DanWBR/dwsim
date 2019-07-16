<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Vessel
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Vessel))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet3 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet6 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet5 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet4 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet3 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy = New System.Windows.Forms.Button()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.cbEnergy = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet3 = New System.Windows.Forms.Button()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbOutlet3 = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet2 = New System.Windows.Forms.Button()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbOutlet2 = New System.Windows.Forms.ComboBox()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect6 = New System.Windows.Forms.Button()
        Me.btnDisconnect5 = New System.Windows.Forms.Button()
        Me.btnDisconnect4 = New System.Windows.Forms.Button()
        Me.btnDisconnect3 = New System.Windows.Forms.Button()
        Me.btnDisconnect2 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet6 = New System.Windows.Forms.ComboBox()
        Me.cbInlet5 = New System.Windows.Forms.ComboBox()
        Me.cbInlet4 = New System.Windows.Forms.ComboBox()
        Me.cbInlet3 = New System.Windows.Forms.ComboBox()
        Me.cbInlet2 = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.cbPressureCalcMode = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.cbPress = New System.Windows.Forms.ComboBox()
        Me.tbPressure = New System.Windows.Forms.TextBox()
        Me.chkOverrideP = New System.Windows.Forms.CheckBox()
        Me.chkOverrideT = New System.Windows.Forms.CheckBox()
        Me.cbTemp = New System.Windows.Forms.ComboBox()
        Me.tbTemperature = New System.Windows.Forms.TextBox()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.btnUtils = New System.Windows.Forms.Button()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.btnConfigureFlashAlg = New System.Windows.Forms.Button()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.UtilitiesCtxMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.AddUtilityTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.sizingtsmi = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.UtilitiesCtxMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet3)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet6)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet5)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet4)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet3)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBox1.Controls.Add(Me.Label16)
        Me.GroupBox1.Controls.Add(Me.cbEnergy)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet3)
        Me.GroupBox1.Controls.Add(Me.Label15)
        Me.GroupBox1.Controls.Add(Me.cbOutlet3)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.Label14)
        Me.GroupBox1.Controls.Add(Me.cbOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect6)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect5)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect4)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect3)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect1)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.cbOutlet1)
        Me.GroupBox1.Controls.Add(Me.cbInlet6)
        Me.GroupBox1.Controls.Add(Me.cbInlet5)
        Me.GroupBox1.Controls.Add(Me.cbInlet4)
        Me.GroupBox1.Controls.Add(Me.cbInlet3)
        Me.GroupBox1.Controls.Add(Me.cbInlet2)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.cbInlet1)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'btnCreateAndConnectEnergy
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy, "btnCreateAndConnectEnergy")
        Me.btnCreateAndConnectEnergy.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy.Name = "btnCreateAndConnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip1"))
        Me.btnCreateAndConnectEnergy.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet3
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet3, "btnCreateAndConnectOutlet3")
        Me.btnCreateAndConnectOutlet3.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet3.Name = "btnCreateAndConnectOutlet3"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet3, resources.GetString("btnCreateAndConnectOutlet3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet3, resources.GetString("btnCreateAndConnectOutlet3.ToolTip1"))
        Me.btnCreateAndConnectOutlet3.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet2, "btnCreateAndConnectOutlet2")
        Me.btnCreateAndConnectOutlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet2.Name = "btnCreateAndConnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip1"))
        Me.btnCreateAndConnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet1, "btnCreateAndConnectOutlet1")
        Me.btnCreateAndConnectOutlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip1"))
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet6
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet6, "btnCreateAndConnectInlet6")
        Me.btnCreateAndConnectInlet6.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet6.Name = "btnCreateAndConnectInlet6"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet6, resources.GetString("btnCreateAndConnectInlet6.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet6, resources.GetString("btnCreateAndConnectInlet6.ToolTip1"))
        Me.btnCreateAndConnectInlet6.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet5
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet5, "btnCreateAndConnectInlet5")
        Me.btnCreateAndConnectInlet5.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet5.Name = "btnCreateAndConnectInlet5"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet5, resources.GetString("btnCreateAndConnectInlet5.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet5, resources.GetString("btnCreateAndConnectInlet5.ToolTip1"))
        Me.btnCreateAndConnectInlet5.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet4
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet4, "btnCreateAndConnectInlet4")
        Me.btnCreateAndConnectInlet4.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet4.Name = "btnCreateAndConnectInlet4"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet4, resources.GetString("btnCreateAndConnectInlet4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet4, resources.GetString("btnCreateAndConnectInlet4.ToolTip1"))
        Me.btnCreateAndConnectInlet4.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet3
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet3, "btnCreateAndConnectInlet3")
        Me.btnCreateAndConnectInlet3.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet3.Name = "btnCreateAndConnectInlet3"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet3, resources.GetString("btnCreateAndConnectInlet3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet3, resources.GetString("btnCreateAndConnectInlet3.ToolTip1"))
        Me.btnCreateAndConnectInlet3.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet2, "btnCreateAndConnectInlet2")
        Me.btnCreateAndConnectInlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet2.Name = "btnCreateAndConnectInlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip1"))
        Me.btnCreateAndConnectInlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet1, "btnCreateAndConnectInlet1")
        Me.btnCreateAndConnectInlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip1"))
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnectEnergy
        '
        resources.ApplyResources(Me.btnDisconnectEnergy, "btnDisconnectEnergy")
        Me.btnDisconnectEnergy.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip1"))
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'cbEnergy
        '
        resources.ApplyResources(Me.cbEnergy, "cbEnergy")
        Me.cbEnergy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy.FormattingEnabled = True
        Me.cbEnergy.Name = "cbEnergy"
        '
        'btnDisconnectOutlet3
        '
        resources.ApplyResources(Me.btnDisconnectOutlet3, "btnDisconnectOutlet3")
        Me.btnDisconnectOutlet3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet3.Name = "btnDisconnectOutlet3"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet3, resources.GetString("btnDisconnectOutlet3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet3, resources.GetString("btnDisconnectOutlet3.ToolTip1"))
        Me.btnDisconnectOutlet3.UseVisualStyleBackColor = True
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'cbOutlet3
        '
        resources.ApplyResources(Me.cbOutlet3, "cbOutlet3")
        Me.cbOutlet3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet3.FormattingEnabled = True
        Me.cbOutlet3.Name = "cbOutlet3"
        '
        'btnDisconnectOutlet2
        '
        resources.ApplyResources(Me.btnDisconnectOutlet2, "btnDisconnectOutlet2")
        Me.btnDisconnectOutlet2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet2.Name = "btnDisconnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip1"))
        Me.btnDisconnectOutlet2.UseVisualStyleBackColor = True
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbOutlet2
        '
        resources.ApplyResources(Me.cbOutlet2, "cbOutlet2")
        Me.cbOutlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet2.FormattingEnabled = True
        Me.cbOutlet2.Name = "cbOutlet2"
        '
        'btnDisconnectOutlet1
        '
        resources.ApplyResources(Me.btnDisconnectOutlet1, "btnDisconnectOutlet1")
        Me.btnDisconnectOutlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip1"))
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect6
        '
        resources.ApplyResources(Me.btnDisconnect6, "btnDisconnect6")
        Me.btnDisconnect6.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect6.Name = "btnDisconnect6"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect6, resources.GetString("btnDisconnect6.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect6, resources.GetString("btnDisconnect6.ToolTip1"))
        Me.btnDisconnect6.UseVisualStyleBackColor = True
        '
        'btnDisconnect5
        '
        resources.ApplyResources(Me.btnDisconnect5, "btnDisconnect5")
        Me.btnDisconnect5.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect5.Name = "btnDisconnect5"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect5, resources.GetString("btnDisconnect5.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect5, resources.GetString("btnDisconnect5.ToolTip1"))
        Me.btnDisconnect5.UseVisualStyleBackColor = True
        '
        'btnDisconnect4
        '
        resources.ApplyResources(Me.btnDisconnect4, "btnDisconnect4")
        Me.btnDisconnect4.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect4.Name = "btnDisconnect4"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect4, resources.GetString("btnDisconnect4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect4, resources.GetString("btnDisconnect4.ToolTip1"))
        Me.btnDisconnect4.UseVisualStyleBackColor = True
        '
        'btnDisconnect3
        '
        resources.ApplyResources(Me.btnDisconnect3, "btnDisconnect3")
        Me.btnDisconnect3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect3.Name = "btnDisconnect3"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect3, resources.GetString("btnDisconnect3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect3, resources.GetString("btnDisconnect3.ToolTip1"))
        Me.btnDisconnect3.UseVisualStyleBackColor = True
        '
        'btnDisconnect2
        '
        resources.ApplyResources(Me.btnDisconnect2, "btnDisconnect2")
        Me.btnDisconnect2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect2.Name = "btnDisconnect2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip1"))
        Me.btnDisconnect2.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        resources.ApplyResources(Me.btnDisconnect1, "btnDisconnect1")
        Me.btnDisconnect1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip1"))
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbOutlet1
        '
        resources.ApplyResources(Me.cbOutlet1, "cbOutlet1")
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Name = "cbOutlet1"
        '
        'cbInlet6
        '
        resources.ApplyResources(Me.cbInlet6, "cbInlet6")
        Me.cbInlet6.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet6.FormattingEnabled = True
        Me.cbInlet6.Name = "cbInlet6"
        '
        'cbInlet5
        '
        resources.ApplyResources(Me.cbInlet5, "cbInlet5")
        Me.cbInlet5.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet5.FormattingEnabled = True
        Me.cbInlet5.Name = "cbInlet5"
        '
        'cbInlet4
        '
        resources.ApplyResources(Me.cbInlet4, "cbInlet4")
        Me.cbInlet4.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet4.FormattingEnabled = True
        Me.cbInlet4.Name = "cbInlet4"
        '
        'cbInlet3
        '
        resources.ApplyResources(Me.cbInlet3, "cbInlet3")
        Me.cbInlet3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet3.FormattingEnabled = True
        Me.cbInlet3.Name = "cbInlet3"
        '
        'cbInlet2
        '
        resources.ApplyResources(Me.cbInlet2, "cbInlet2")
        Me.cbInlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet2.FormattingEnabled = True
        Me.cbInlet2.Name = "cbInlet2"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbInlet1
        '
        resources.ApplyResources(Me.cbInlet1, "cbInlet1")
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Name = "cbInlet1"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.cbPressureCalcMode)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Controls.Add(Me.cbPress)
        Me.GroupBox2.Controls.Add(Me.tbPressure)
        Me.GroupBox2.Controls.Add(Me.chkOverrideP)
        Me.GroupBox2.Controls.Add(Me.chkOverrideT)
        Me.GroupBox2.Controls.Add(Me.cbTemp)
        Me.GroupBox2.Controls.Add(Me.tbTemperature)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'cbPressureCalcMode
        '
        resources.ApplyResources(Me.cbPressureCalcMode, "cbPressureCalcMode")
        Me.cbPressureCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPressureCalcMode.FormattingEnabled = True
        Me.cbPressureCalcMode.Items.AddRange(New Object() {resources.GetString("cbPressureCalcMode.Items"), resources.GetString("cbPressureCalcMode.Items1"), resources.GetString("cbPressureCalcMode.Items2")})
        Me.cbPressureCalcMode.Name = "cbPressureCalcMode"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'cbPress
        '
        resources.ApplyResources(Me.cbPress, "cbPress")
        Me.cbPress.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPress.FormattingEnabled = True
        Me.cbPress.Items.AddRange(New Object() {resources.GetString("cbPress.Items"), resources.GetString("cbPress.Items1"), resources.GetString("cbPress.Items2")})
        Me.cbPress.Name = "cbPress"
        '
        'tbPressure
        '
        resources.ApplyResources(Me.tbPressure, "tbPressure")
        Me.tbPressure.Name = "tbPressure"
        '
        'chkOverrideP
        '
        resources.ApplyResources(Me.chkOverrideP, "chkOverrideP")
        Me.chkOverrideP.Name = "chkOverrideP"
        Me.chkOverrideP.UseVisualStyleBackColor = True
        '
        'chkOverrideT
        '
        resources.ApplyResources(Me.chkOverrideT, "chkOverrideT")
        Me.chkOverrideT.Name = "chkOverrideT"
        Me.chkOverrideT.UseVisualStyleBackColor = True
        '
        'cbTemp
        '
        resources.ApplyResources(Me.cbTemp, "cbTemp")
        Me.cbTemp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTemp.FormattingEnabled = True
        Me.cbTemp.Items.AddRange(New Object() {resources.GetString("cbTemp.Items"), resources.GetString("cbTemp.Items1"), resources.GetString("cbTemp.Items2")})
        Me.cbTemp.Name = "cbTemp"
        '
        'tbTemperature
        '
        resources.ApplyResources(Me.tbTemperature, "tbTemperature")
        Me.tbTemperature.Name = "tbTemperature"
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
        '
        'btnUtils
        '
        resources.ApplyResources(Me.btnUtils, "btnUtils")
        Me.btnUtils.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_sparkle
        Me.btnUtils.Name = "btnUtils"
        Me.ToolTip1.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnUtils, resources.GetString("btnUtils.ToolTip1"))
        Me.btnUtils.UseVisualStyleBackColor = True
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
        Me.ToolTipValues.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip1"))
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
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'rtbAnnotations
        '
        resources.ApplyResources(Me.rtbAnnotations, "rtbAnnotations")
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fnil\fcharset0 Microsoft S" &
    "ans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\viewkind4\uc1\pard\f0\fs17\par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.btnConfigureFlashAlg)
        Me.GroupBox3.Controls.Add(Me.btnConfigurePP)
        Me.GroupBox3.Controls.Add(Me.cbFlashAlg)
        Me.GroupBox3.Controls.Add(Me.Label10)
        Me.GroupBox3.Controls.Add(Me.cbPropPack)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'btnConfigureFlashAlg
        '
        resources.ApplyResources(Me.btnConfigureFlashAlg, "btnConfigureFlashAlg")
        Me.btnConfigureFlashAlg.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigureFlashAlg.Name = "btnConfigureFlashAlg"
        Me.ToolTip1.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnConfigureFlashAlg, resources.GetString("btnConfigureFlashAlg.ToolTip1"))
        Me.btnConfigureFlashAlg.UseVisualStyleBackColor = True
        '
        'btnConfigurePP
        '
        resources.ApplyResources(Me.btnConfigurePP, "btnConfigurePP")
        Me.btnConfigurePP.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.btnConfigurePP.Name = "btnConfigurePP"
        Me.ToolTip1.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.btnConfigurePP, resources.GetString("btnConfigurePP.ToolTip1"))
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
        'UtilitiesCtxMenu
        '
        Me.UtilitiesCtxMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AddUtilityTSMI})
        Me.UtilitiesCtxMenu.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.UtilitiesCtxMenu, "UtilitiesCtxMenu")
        '
        'AddUtilityTSMI
        '
        Me.AddUtilityTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.sizingtsmi})
        Me.AddUtilityTSMI.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        Me.AddUtilityTSMI.Name = "AddUtilityTSMI"
        resources.ApplyResources(Me.AddUtilityTSMI, "AddUtilityTSMI")
        '
        'sizingtsmi
        '
        Me.sizingtsmi.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.cog
        Me.sizingtsmi.Name = "sizingtsmi"
        resources.ApplyResources(Me.sizingtsmi, "sizingtsmi")
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_Vessel
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Name = "EditingForm_Vessel"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.UtilitiesCtxMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet6 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet5 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet4 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet3 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet2 As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect6 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect5 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect4 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect3 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect2 As System.Windows.Forms.Button
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents btnDisconnectEnergy As System.Windows.Forms.Button
    Public WithEvents Label16 As System.Windows.Forms.Label
    Public WithEvents cbEnergy As System.Windows.Forms.ComboBox
    Public WithEvents btnDisconnectOutlet3 As System.Windows.Forms.Button
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents cbOutlet3 As System.Windows.Forms.ComboBox
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents cbPress As System.Windows.Forms.ComboBox
    Public WithEvents tbPressure As System.Windows.Forms.TextBox
    Public WithEvents chkOverrideP As System.Windows.Forms.CheckBox
    Public WithEvents chkOverrideT As System.Windows.Forms.CheckBox
    Public WithEvents cbTemp As System.Windows.Forms.ComboBox
    Public WithEvents tbTemperature As System.Windows.Forms.TextBox
    Public WithEvents cbPressureCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents btnCreateAndConnectEnergy As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet3 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet6 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet5 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet4 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet3 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents btnUtils As System.Windows.Forms.Button
    Public WithEvents UtilitiesCtxMenu As System.Windows.Forms.ContextMenuStrip
    Public WithEvents AddUtilityTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents sizingtsmi As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolTipChangeTag As ToolTip
End Class
