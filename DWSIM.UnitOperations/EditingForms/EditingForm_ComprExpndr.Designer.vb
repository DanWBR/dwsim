<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_ComprExpndr
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_ComprExpndr))
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
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.tbRotSpeed = New System.Windows.Forms.TextBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.btnCurves = New System.Windows.Forms.Button()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.cbPolytropicHead = New System.Windows.Forms.ComboBox()
        Me.cbAdiabaticHead = New System.Windows.Forms.ComboBox()
        Me.tbPolytropicHead = New System.Windows.Forms.TextBox()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.tbAdiabaticHead = New System.Windows.Forms.TextBox()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.tbPolytropicCoeff = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.tbAdiabaticCoeff = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.cbProcessPath = New System.Windows.Forms.ComboBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.tbPolytropicEfficiency = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.cbDeltaT = New System.Windows.Forms.ComboBox()
        Me.tbDeltaT = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.cbTemp = New System.Windows.Forms.ComboBox()
        Me.tbTemp = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbPower = New System.Windows.Forms.ComboBox()
        Me.tbPower = New System.Windows.Forms.TextBox()
        Me.lblPower = New System.Windows.Forms.Label()
        Me.cbPress = New System.Windows.Forms.ComboBox()
        Me.tbOutletPressure = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbEfficiency = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbPressureDropU = New System.Windows.Forms.ComboBox()
        Me.tbPressureDrop = New System.Windows.Forms.TextBox()
        Me.lblDP = New System.Windows.Forms.Label()
        Me.cbCalcMode = New System.Windows.Forms.ComboBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
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
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.tbPRatio = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
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
        Me.ToolTipValues.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip2"))
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
        Me.GroupBoxParameters.Controls.Add(Me.tbPRatio)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.Label25)
        Me.GroupBoxParameters.Controls.Add(Me.tbRotSpeed)
        Me.GroupBoxParameters.Controls.Add(Me.Label26)
        Me.GroupBoxParameters.Controls.Add(Me.btnCurves)
        Me.GroupBoxParameters.Controls.Add(Me.Label24)
        Me.GroupBoxParameters.Controls.Add(Me.Label23)
        Me.GroupBoxParameters.Controls.Add(Me.Label22)
        Me.GroupBoxParameters.Controls.Add(Me.cbPolytropicHead)
        Me.GroupBoxParameters.Controls.Add(Me.cbAdiabaticHead)
        Me.GroupBoxParameters.Controls.Add(Me.tbPolytropicHead)
        Me.GroupBoxParameters.Controls.Add(Me.Label20)
        Me.GroupBoxParameters.Controls.Add(Me.tbAdiabaticHead)
        Me.GroupBoxParameters.Controls.Add(Me.Label21)
        Me.GroupBoxParameters.Controls.Add(Me.tbPolytropicCoeff)
        Me.GroupBoxParameters.Controls.Add(Me.Label18)
        Me.GroupBoxParameters.Controls.Add(Me.tbAdiabaticCoeff)
        Me.GroupBoxParameters.Controls.Add(Me.Label17)
        Me.GroupBoxParameters.Controls.Add(Me.cbProcessPath)
        Me.GroupBoxParameters.Controls.Add(Me.Label16)
        Me.GroupBoxParameters.Controls.Add(Me.tbPolytropicEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.Label15)
        Me.GroupBoxParameters.Controls.Add(Me.cbDeltaT)
        Me.GroupBoxParameters.Controls.Add(Me.tbDeltaT)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.cbTemp)
        Me.GroupBoxParameters.Controls.Add(Me.tbTemp)
        Me.GroupBoxParameters.Controls.Add(Me.Label4)
        Me.GroupBoxParameters.Controls.Add(Me.cbPower)
        Me.GroupBoxParameters.Controls.Add(Me.tbPower)
        Me.GroupBoxParameters.Controls.Add(Me.lblPower)
        Me.GroupBoxParameters.Controls.Add(Me.cbPress)
        Me.GroupBoxParameters.Controls.Add(Me.tbOutletPressure)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.tbEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.cbPressureDropU)
        Me.GroupBoxParameters.Controls.Add(Me.tbPressureDrop)
        Me.GroupBoxParameters.Controls.Add(Me.lblDP)
        Me.GroupBoxParameters.Controls.Add(Me.cbCalcMode)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'Label25
        '
        resources.ApplyResources(Me.Label25, "Label25")
        Me.Label25.Name = "Label25"
        Me.ToolTip1.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label25, resources.GetString("Label25.ToolTip2"))
        '
        'tbRotSpeed
        '
        resources.ApplyResources(Me.tbRotSpeed, "tbRotSpeed")
        Me.tbRotSpeed.Name = "tbRotSpeed"
        Me.ToolTipValues.SetToolTip(Me.tbRotSpeed, resources.GetString("tbRotSpeed.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbRotSpeed, resources.GetString("tbRotSpeed.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbRotSpeed, resources.GetString("tbRotSpeed.ToolTip2"))
        '
        'Label26
        '
        resources.ApplyResources(Me.Label26, "Label26")
        Me.Label26.Name = "Label26"
        Me.ToolTip1.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label26, resources.GetString("Label26.ToolTip2"))
        '
        'btnCurves
        '
        resources.ApplyResources(Me.btnCurves, "btnCurves")
        Me.btnCurves.Name = "btnCurves"
        Me.ToolTip1.SetToolTip(Me.btnCurves, resources.GetString("btnCurves.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCurves, resources.GetString("btnCurves.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCurves, resources.GetString("btnCurves.ToolTip2"))
        Me.btnCurves.UseVisualStyleBackColor = True
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        Me.ToolTip1.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label24, resources.GetString("Label24.ToolTip2"))
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        Me.ToolTip1.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label23, resources.GetString("Label23.ToolTip2"))
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        Me.ToolTip1.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label22, resources.GetString("Label22.ToolTip2"))
        '
        'cbPolytropicHead
        '
        resources.ApplyResources(Me.cbPolytropicHead, "cbPolytropicHead")
        Me.cbPolytropicHead.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPolytropicHead.FormattingEnabled = True
        Me.cbPolytropicHead.Items.AddRange(New Object() {resources.GetString("cbPolytropicHead.Items"), resources.GetString("cbPolytropicHead.Items1"), resources.GetString("cbPolytropicHead.Items2")})
        Me.cbPolytropicHead.Name = "cbPolytropicHead"
        Me.ToolTip1.SetToolTip(Me.cbPolytropicHead, resources.GetString("cbPolytropicHead.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPolytropicHead, resources.GetString("cbPolytropicHead.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPolytropicHead, resources.GetString("cbPolytropicHead.ToolTip2"))
        '
        'cbAdiabaticHead
        '
        resources.ApplyResources(Me.cbAdiabaticHead, "cbAdiabaticHead")
        Me.cbAdiabaticHead.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbAdiabaticHead.FormattingEnabled = True
        Me.cbAdiabaticHead.Items.AddRange(New Object() {resources.GetString("cbAdiabaticHead.Items"), resources.GetString("cbAdiabaticHead.Items1"), resources.GetString("cbAdiabaticHead.Items2")})
        Me.cbAdiabaticHead.Name = "cbAdiabaticHead"
        Me.ToolTip1.SetToolTip(Me.cbAdiabaticHead, resources.GetString("cbAdiabaticHead.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbAdiabaticHead, resources.GetString("cbAdiabaticHead.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbAdiabaticHead, resources.GetString("cbAdiabaticHead.ToolTip2"))
        '
        'tbPolytropicHead
        '
        resources.ApplyResources(Me.tbPolytropicHead, "tbPolytropicHead")
        Me.tbPolytropicHead.Name = "tbPolytropicHead"
        Me.ToolTipValues.SetToolTip(Me.tbPolytropicHead, resources.GetString("tbPolytropicHead.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPolytropicHead, resources.GetString("tbPolytropicHead.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPolytropicHead, resources.GetString("tbPolytropicHead.ToolTip2"))
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        Me.ToolTip1.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label20, resources.GetString("Label20.ToolTip2"))
        '
        'tbAdiabaticHead
        '
        resources.ApplyResources(Me.tbAdiabaticHead, "tbAdiabaticHead")
        Me.tbAdiabaticHead.Name = "tbAdiabaticHead"
        Me.ToolTipValues.SetToolTip(Me.tbAdiabaticHead, resources.GetString("tbAdiabaticHead.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbAdiabaticHead, resources.GetString("tbAdiabaticHead.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbAdiabaticHead, resources.GetString("tbAdiabaticHead.ToolTip2"))
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        Me.ToolTip1.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label21, resources.GetString("Label21.ToolTip2"))
        '
        'tbPolytropicCoeff
        '
        resources.ApplyResources(Me.tbPolytropicCoeff, "tbPolytropicCoeff")
        Me.tbPolytropicCoeff.Name = "tbPolytropicCoeff"
        Me.ToolTipValues.SetToolTip(Me.tbPolytropicCoeff, resources.GetString("tbPolytropicCoeff.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPolytropicCoeff, resources.GetString("tbPolytropicCoeff.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPolytropicCoeff, resources.GetString("tbPolytropicCoeff.ToolTip2"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTip1.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip2"))
        '
        'tbAdiabaticCoeff
        '
        resources.ApplyResources(Me.tbAdiabaticCoeff, "tbAdiabaticCoeff")
        Me.tbAdiabaticCoeff.Name = "tbAdiabaticCoeff"
        Me.ToolTipValues.SetToolTip(Me.tbAdiabaticCoeff, resources.GetString("tbAdiabaticCoeff.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbAdiabaticCoeff, resources.GetString("tbAdiabaticCoeff.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbAdiabaticCoeff, resources.GetString("tbAdiabaticCoeff.ToolTip2"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTip1.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip2"))
        '
        'cbProcessPath
        '
        resources.ApplyResources(Me.cbProcessPath, "cbProcessPath")
        Me.cbProcessPath.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbProcessPath.FormattingEnabled = True
        Me.cbProcessPath.Items.AddRange(New Object() {resources.GetString("cbProcessPath.Items"), resources.GetString("cbProcessPath.Items1")})
        Me.cbProcessPath.Name = "cbProcessPath"
        Me.ToolTip1.SetToolTip(Me.cbProcessPath, resources.GetString("cbProcessPath.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbProcessPath, resources.GetString("cbProcessPath.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbProcessPath, resources.GetString("cbProcessPath.ToolTip2"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTip1.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip2"))
        '
        'tbPolytropicEfficiency
        '
        resources.ApplyResources(Me.tbPolytropicEfficiency, "tbPolytropicEfficiency")
        Me.tbPolytropicEfficiency.Name = "tbPolytropicEfficiency"
        Me.ToolTipValues.SetToolTip(Me.tbPolytropicEfficiency, resources.GetString("tbPolytropicEfficiency.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPolytropicEfficiency, resources.GetString("tbPolytropicEfficiency.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPolytropicEfficiency, resources.GetString("tbPolytropicEfficiency.ToolTip2"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTip1.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip2"))
        '
        'cbDeltaT
        '
        resources.ApplyResources(Me.cbDeltaT, "cbDeltaT")
        Me.cbDeltaT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbDeltaT.FormattingEnabled = True
        Me.cbDeltaT.Items.AddRange(New Object() {resources.GetString("cbDeltaT.Items"), resources.GetString("cbDeltaT.Items1"), resources.GetString("cbDeltaT.Items2")})
        Me.cbDeltaT.Name = "cbDeltaT"
        Me.ToolTip1.SetToolTip(Me.cbDeltaT, resources.GetString("cbDeltaT.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbDeltaT, resources.GetString("cbDeltaT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbDeltaT, resources.GetString("cbDeltaT.ToolTip2"))
        '
        'tbDeltaT
        '
        resources.ApplyResources(Me.tbDeltaT, "tbDeltaT")
        Me.tbDeltaT.Name = "tbDeltaT"
        Me.ToolTipValues.SetToolTip(Me.tbDeltaT, resources.GetString("tbDeltaT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbDeltaT, resources.GetString("tbDeltaT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbDeltaT, resources.GetString("tbDeltaT.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
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
        'tbTemp
        '
        resources.ApplyResources(Me.tbTemp, "tbTemp")
        Me.tbTemp.Name = "tbTemp"
        Me.ToolTipValues.SetToolTip(Me.tbTemp, resources.GetString("tbTemp.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbTemp, resources.GetString("tbTemp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbTemp, resources.GetString("tbTemp.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'cbPower
        '
        resources.ApplyResources(Me.cbPower, "cbPower")
        Me.cbPower.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPower.FormattingEnabled = True
        Me.cbPower.Items.AddRange(New Object() {resources.GetString("cbPower.Items"), resources.GetString("cbPower.Items1"), resources.GetString("cbPower.Items2")})
        Me.cbPower.Name = "cbPower"
        Me.ToolTip1.SetToolTip(Me.cbPower, resources.GetString("cbPower.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbPower, resources.GetString("cbPower.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbPower, resources.GetString("cbPower.ToolTip2"))
        '
        'tbPower
        '
        resources.ApplyResources(Me.tbPower, "tbPower")
        Me.tbPower.Name = "tbPower"
        Me.ToolTipValues.SetToolTip(Me.tbPower, resources.GetString("tbPower.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPower, resources.GetString("tbPower.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPower, resources.GetString("tbPower.ToolTip2"))
        '
        'lblPower
        '
        resources.ApplyResources(Me.lblPower, "lblPower")
        Me.lblPower.Name = "lblPower"
        Me.ToolTip1.SetToolTip(Me.lblPower, resources.GetString("lblPower.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblPower, resources.GetString("lblPower.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblPower, resources.GetString("lblPower.ToolTip2"))
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
        'tbEfficiency
        '
        resources.ApplyResources(Me.tbEfficiency, "tbEfficiency")
        Me.tbEfficiency.Name = "tbEfficiency"
        Me.ToolTipValues.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbEfficiency, resources.GetString("tbEfficiency.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
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
        'lblDP
        '
        resources.ApplyResources(Me.lblDP, "lblDP")
        Me.lblDP.Name = "lblDP"
        Me.ToolTip1.SetToolTip(Me.lblDP, resources.GetString("lblDP.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblDP, resources.GetString("lblDP.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblDP, resources.GetString("lblDP.ToolTip2"))
        '
        'cbCalcMode
        '
        resources.ApplyResources(Me.cbCalcMode, "cbCalcMode")
        Me.cbCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCalcMode.FormattingEnabled = True
        Me.cbCalcMode.Items.AddRange(New Object() {resources.GetString("cbCalcMode.Items"), resources.GetString("cbCalcMode.Items1"), resources.GetString("cbCalcMode.Items2"), resources.GetString("cbCalcMode.Items3"), resources.GetString("cbCalcMode.Items4"), resources.GetString("cbCalcMode.Items5"), resources.GetString("cbCalcMode.Items6")})
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
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'tbPRatio
        '
        resources.ApplyResources(Me.tbPRatio, "tbPRatio")
        Me.tbPRatio.Name = "tbPRatio"
        Me.ToolTipValues.SetToolTip(Me.tbPRatio, resources.GetString("tbPRatio.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPRatio, resources.GetString("tbPRatio.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPRatio, resources.GetString("tbPRatio.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'EditingForm_ComprExpndr
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_ComprExpndr"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
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
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents cbCalcMode As System.Windows.Forms.ComboBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbPressureDropU As System.Windows.Forms.ComboBox
    Public WithEvents tbPressureDrop As System.Windows.Forms.TextBox
    Public WithEvents lblDP As System.Windows.Forms.Label
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents cbPower As System.Windows.Forms.ComboBox
    Public WithEvents tbPower As System.Windows.Forms.TextBox
    Public WithEvents lblPower As System.Windows.Forms.Label
    Public WithEvents cbPress As System.Windows.Forms.ComboBox
    Public WithEvents tbOutletPressure As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents tbEfficiency As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
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
    Public WithEvents cbDeltaT As System.Windows.Forms.ComboBox
    Public WithEvents tbDeltaT As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents cbTemp As System.Windows.Forms.ComboBox
    Public WithEvents tbTemp As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectEnergy As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents cbProcessPath As ComboBox
    Public WithEvents Label16 As Label
    Public WithEvents tbPolytropicEfficiency As TextBox
    Public WithEvents Label15 As Label
    Public WithEvents cbPolytropicHead As ComboBox
    Public WithEvents cbAdiabaticHead As ComboBox
    Public WithEvents tbPolytropicHead As TextBox
    Public WithEvents Label20 As Label
    Public WithEvents tbAdiabaticHead As TextBox
    Public WithEvents Label21 As Label
    Public WithEvents tbPolytropicCoeff As TextBox
    Public WithEvents Label18 As Label
    Public WithEvents tbAdiabaticCoeff As TextBox
    Public WithEvents Label17 As Label
    Friend WithEvents Label23 As Label
    Friend WithEvents Label22 As Label
    Public WithEvents btnCurves As Button
    Public WithEvents Label24 As Label
    Friend WithEvents Label25 As Label
    Public WithEvents tbRotSpeed As TextBox
    Public WithEvents Label26 As Label
    Public WithEvents tbPRatio As TextBox
    Public WithEvents Label3 As Label
End Class
