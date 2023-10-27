<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_OrificePlate
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_OrificePlate))
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
        Me.cbIntPipeDiam = New System.Windows.Forms.ComboBox()
        Me.tbIntPipeDiameter = New System.Windows.Forms.TextBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbOrificePdrop = New System.Windows.Forms.ComboBox()
        Me.tbOrificePDrop = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.cbOverallPdrop = New System.Windows.Forms.ComboBox()
        Me.tbOverallPDrop = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.rbRadius = New System.Windows.Forms.RadioButton()
        Me.rbFlange = New System.Windows.Forms.RadioButton()
        Me.rbCorner = New System.Windows.Forms.RadioButton()
        Me.cbDeltaT = New System.Windows.Forms.ComboBox()
        Me.tbDeltaT = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbCorrF = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbBeta = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbOrifDiam = New System.Windows.Forms.ComboBox()
        Me.tbOrificeDiameter = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
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
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
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
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.19041}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
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
        Me.GroupBoxParameters.Controls.Add(Me.cbIntPipeDiam)
        Me.GroupBoxParameters.Controls.Add(Me.tbIntPipeDiameter)
        Me.GroupBoxParameters.Controls.Add(Me.Label14)
        Me.GroupBoxParameters.Controls.Add(Me.cbOrificePdrop)
        Me.GroupBoxParameters.Controls.Add(Me.tbOrificePDrop)
        Me.GroupBoxParameters.Controls.Add(Me.Label6)
        Me.GroupBoxParameters.Controls.Add(Me.cbOverallPdrop)
        Me.GroupBoxParameters.Controls.Add(Me.tbOverallPDrop)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.rbRadius)
        Me.GroupBoxParameters.Controls.Add(Me.rbFlange)
        Me.GroupBoxParameters.Controls.Add(Me.rbCorner)
        Me.GroupBoxParameters.Controls.Add(Me.cbDeltaT)
        Me.GroupBoxParameters.Controls.Add(Me.tbDeltaT)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.tbCorrF)
        Me.GroupBoxParameters.Controls.Add(Me.Label4)
        Me.GroupBoxParameters.Controls.Add(Me.tbBeta)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.cbOrifDiam)
        Me.GroupBoxParameters.Controls.Add(Me.tbOrificeDiameter)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'cbIntPipeDiam
        '
        resources.ApplyResources(Me.cbIntPipeDiam, "cbIntPipeDiam")
        Me.cbIntPipeDiam.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbIntPipeDiam.FormattingEnabled = True
        Me.cbIntPipeDiam.Items.AddRange(New Object() {resources.GetString("cbIntPipeDiam.Items"), resources.GetString("cbIntPipeDiam.Items1"), resources.GetString("cbIntPipeDiam.Items2")})
        Me.cbIntPipeDiam.Name = "cbIntPipeDiam"
        Me.ToolTip1.SetToolTip(Me.cbIntPipeDiam, resources.GetString("cbIntPipeDiam.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbIntPipeDiam, resources.GetString("cbIntPipeDiam.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbIntPipeDiam, resources.GetString("cbIntPipeDiam.ToolTip2"))
        '
        'tbIntPipeDiameter
        '
        resources.ApplyResources(Me.tbIntPipeDiameter, "tbIntPipeDiameter")
        Me.tbIntPipeDiameter.Name = "tbIntPipeDiameter"
        Me.ToolTipValues.SetToolTip(Me.tbIntPipeDiameter, resources.GetString("tbIntPipeDiameter.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbIntPipeDiameter, resources.GetString("tbIntPipeDiameter.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbIntPipeDiameter, resources.GetString("tbIntPipeDiameter.ToolTip2"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTip1.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip2"))
        '
        'cbOrificePdrop
        '
        resources.ApplyResources(Me.cbOrificePdrop, "cbOrificePdrop")
        Me.cbOrificePdrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrificePdrop.FormattingEnabled = True
        Me.cbOrificePdrop.Items.AddRange(New Object() {resources.GetString("cbOrificePdrop.Items"), resources.GetString("cbOrificePdrop.Items1"), resources.GetString("cbOrificePdrop.Items2")})
        Me.cbOrificePdrop.Name = "cbOrificePdrop"
        Me.ToolTip1.SetToolTip(Me.cbOrificePdrop, resources.GetString("cbOrificePdrop.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOrificePdrop, resources.GetString("cbOrificePdrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOrificePdrop, resources.GetString("cbOrificePdrop.ToolTip2"))
        '
        'tbOrificePDrop
        '
        resources.ApplyResources(Me.tbOrificePDrop, "tbOrificePDrop")
        Me.tbOrificePDrop.Name = "tbOrificePDrop"
        Me.tbOrificePDrop.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbOrificePDrop, resources.GetString("tbOrificePDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOrificePDrop, resources.GetString("tbOrificePDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOrificePDrop, resources.GetString("tbOrificePDrop.ToolTip2"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'cbOverallPdrop
        '
        resources.ApplyResources(Me.cbOverallPdrop, "cbOverallPdrop")
        Me.cbOverallPdrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOverallPdrop.FormattingEnabled = True
        Me.cbOverallPdrop.Items.AddRange(New Object() {resources.GetString("cbOverallPdrop.Items"), resources.GetString("cbOverallPdrop.Items1"), resources.GetString("cbOverallPdrop.Items2")})
        Me.cbOverallPdrop.Name = "cbOverallPdrop"
        Me.ToolTip1.SetToolTip(Me.cbOverallPdrop, resources.GetString("cbOverallPdrop.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOverallPdrop, resources.GetString("cbOverallPdrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOverallPdrop, resources.GetString("cbOverallPdrop.ToolTip2"))
        '
        'tbOverallPDrop
        '
        resources.ApplyResources(Me.tbOverallPDrop, "tbOverallPDrop")
        Me.tbOverallPDrop.Name = "tbOverallPDrop"
        Me.tbOverallPDrop.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbOverallPDrop, resources.GetString("tbOverallPDrop.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOverallPDrop, resources.GetString("tbOverallPDrop.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOverallPDrop, resources.GetString("tbOverallPDrop.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'rbRadius
        '
        resources.ApplyResources(Me.rbRadius, "rbRadius")
        Me.rbRadius.Name = "rbRadius"
        Me.rbRadius.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbRadius, resources.GetString("rbRadius.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbRadius, resources.GetString("rbRadius.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbRadius, resources.GetString("rbRadius.ToolTip2"))
        Me.rbRadius.UseVisualStyleBackColor = True
        '
        'rbFlange
        '
        resources.ApplyResources(Me.rbFlange, "rbFlange")
        Me.rbFlange.Name = "rbFlange"
        Me.rbFlange.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbFlange, resources.GetString("rbFlange.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbFlange, resources.GetString("rbFlange.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbFlange, resources.GetString("rbFlange.ToolTip2"))
        Me.rbFlange.UseVisualStyleBackColor = True
        '
        'rbCorner
        '
        resources.ApplyResources(Me.rbCorner, "rbCorner")
        Me.rbCorner.Name = "rbCorner"
        Me.rbCorner.TabStop = True
        Me.ToolTip1.SetToolTip(Me.rbCorner, resources.GetString("rbCorner.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.rbCorner, resources.GetString("rbCorner.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.rbCorner, resources.GetString("rbCorner.ToolTip2"))
        Me.rbCorner.UseVisualStyleBackColor = True
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
        Me.tbDeltaT.ReadOnly = True
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
        'tbCorrF
        '
        resources.ApplyResources(Me.tbCorrF, "tbCorrF")
        Me.tbCorrF.Name = "tbCorrF"
        Me.ToolTipValues.SetToolTip(Me.tbCorrF, resources.GetString("tbCorrF.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbCorrF, resources.GetString("tbCorrF.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbCorrF, resources.GetString("tbCorrF.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'tbBeta
        '
        resources.ApplyResources(Me.tbBeta, "tbBeta")
        Me.tbBeta.Name = "tbBeta"
        Me.tbBeta.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbBeta, resources.GetString("tbBeta.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbBeta, resources.GetString("tbBeta.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbBeta, resources.GetString("tbBeta.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbOrifDiam
        '
        resources.ApplyResources(Me.cbOrifDiam, "cbOrifDiam")
        Me.cbOrifDiam.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrifDiam.FormattingEnabled = True
        Me.cbOrifDiam.Items.AddRange(New Object() {resources.GetString("cbOrifDiam.Items"), resources.GetString("cbOrifDiam.Items1"), resources.GetString("cbOrifDiam.Items2")})
        Me.cbOrifDiam.Name = "cbOrifDiam"
        Me.ToolTip1.SetToolTip(Me.cbOrifDiam, resources.GetString("cbOrifDiam.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbOrifDiam, resources.GetString("cbOrifDiam.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbOrifDiam, resources.GetString("cbOrifDiam.ToolTip2"))
        '
        'tbOrificeDiameter
        '
        resources.ApplyResources(Me.tbOrificeDiameter, "tbOrificeDiameter")
        Me.tbOrificeDiameter.Name = "tbOrificeDiameter"
        Me.ToolTipValues.SetToolTip(Me.tbOrificeDiameter, resources.GetString("tbOrificeDiameter.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbOrificeDiameter, resources.GetString("tbOrificeDiameter.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbOrificeDiameter, resources.GetString("tbOrificeDiameter.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_OrificePlate
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_OrificePlate"
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
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents cbOrifDiam As System.Windows.Forms.ComboBox
    Public WithEvents tbOrificeDiameter As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents tbBeta As System.Windows.Forms.TextBox
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
    Public WithEvents cbDeltaT As System.Windows.Forms.ComboBox
    Public WithEvents tbDeltaT As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents tbCorrF As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents rbRadius As System.Windows.Forms.RadioButton
    Public WithEvents rbFlange As System.Windows.Forms.RadioButton
    Public WithEvents rbCorner As System.Windows.Forms.RadioButton
    Public WithEvents cbOrificePdrop As System.Windows.Forms.ComboBox
    Public WithEvents tbOrificePDrop As System.Windows.Forms.TextBox
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents cbOverallPdrop As System.Windows.Forms.ComboBox
    Public WithEvents tbOverallPDrop As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents tbIntPipeDiameter As System.Windows.Forms.TextBox
    Public WithEvents Label14 As System.Windows.Forms.Label
    Public WithEvents cbIntPipeDiam As System.Windows.Forms.ComboBox
    Friend WithEvents ToolTipChangeTag As ToolTip
End Class
