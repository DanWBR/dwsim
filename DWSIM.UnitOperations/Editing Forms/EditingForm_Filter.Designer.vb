<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Filter
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Filter))
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
        Me.btnConfigureFlashAlg = New System.Windows.Forms.Button()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.cbPDrop = New System.Windows.Forms.ComboBox()
        Me.tbPressureDrop = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.cbArea = New System.Windows.Forms.ComboBox()
        Me.tbTotalArea = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.tbCakeRelativeHumidity = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.tbSubmergedArea = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.cbCycleTime = New System.Windows.Forms.ComboBox()
        Me.tbCycleTime = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbCakeResistance = New System.Windows.Forms.ComboBox()
        Me.tbCakeResistance = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbFilterResistance = New System.Windows.Forms.ComboBox()
        Me.tbFilterMediumResistance = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.rbEvaluation = New System.Windows.Forms.RadioButton()
        Me.rbSizing = New System.Windows.Forms.RadioButton()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
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
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
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
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fnil\fcharset0 Microsoft S" & _
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
        Me.GroupBox2.Controls.Add(Me.cbPDrop)
        Me.GroupBox2.Controls.Add(Me.tbPressureDrop)
        Me.GroupBox2.Controls.Add(Me.Label16)
        Me.GroupBox2.Controls.Add(Me.cbArea)
        Me.GroupBox2.Controls.Add(Me.tbTotalArea)
        Me.GroupBox2.Controls.Add(Me.Label15)
        Me.GroupBox2.Controls.Add(Me.tbCakeRelativeHumidity)
        Me.GroupBox2.Controls.Add(Me.Label6)
        Me.GroupBox2.Controls.Add(Me.tbSubmergedArea)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Controls.Add(Me.cbCycleTime)
        Me.GroupBox2.Controls.Add(Me.tbCycleTime)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.cbCakeResistance)
        Me.GroupBox2.Controls.Add(Me.tbCakeResistance)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.cbFilterResistance)
        Me.GroupBox2.Controls.Add(Me.tbFilterMediumResistance)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.rbEvaluation)
        Me.GroupBox2.Controls.Add(Me.rbSizing)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'cbPDrop
        '
        resources.ApplyResources(Me.cbPDrop, "cbPDrop")
        Me.cbPDrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPDrop.FormattingEnabled = True
        Me.cbPDrop.Items.AddRange(New Object() {resources.GetString("cbPDrop.Items"), resources.GetString("cbPDrop.Items1"), resources.GetString("cbPDrop.Items2")})
        Me.cbPDrop.Name = "cbPDrop"
        '
        'tbPressureDrop
        '
        resources.ApplyResources(Me.tbPressureDrop, "tbPressureDrop")
        Me.tbPressureDrop.Name = "tbPressureDrop"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'cbArea
        '
        resources.ApplyResources(Me.cbArea, "cbArea")
        Me.cbArea.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbArea.FormattingEnabled = True
        Me.cbArea.Items.AddRange(New Object() {resources.GetString("cbArea.Items"), resources.GetString("cbArea.Items1"), resources.GetString("cbArea.Items2")})
        Me.cbArea.Name = "cbArea"
        '
        'tbTotalArea
        '
        resources.ApplyResources(Me.tbTotalArea, "tbTotalArea")
        Me.tbTotalArea.Name = "tbTotalArea"
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'tbCakeRelativeHumidity
        '
        resources.ApplyResources(Me.tbCakeRelativeHumidity, "tbCakeRelativeHumidity")
        Me.tbCakeRelativeHumidity.Name = "tbCakeRelativeHumidity"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'tbSubmergedArea
        '
        resources.ApplyResources(Me.tbSubmergedArea, "tbSubmergedArea")
        Me.tbSubmergedArea.Name = "tbSubmergedArea"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'cbCycleTime
        '
        resources.ApplyResources(Me.cbCycleTime, "cbCycleTime")
        Me.cbCycleTime.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCycleTime.FormattingEnabled = True
        Me.cbCycleTime.Items.AddRange(New Object() {resources.GetString("cbCycleTime.Items"), resources.GetString("cbCycleTime.Items1"), resources.GetString("cbCycleTime.Items2")})
        Me.cbCycleTime.Name = "cbCycleTime"
        '
        'tbCycleTime
        '
        resources.ApplyResources(Me.tbCycleTime, "tbCycleTime")
        Me.tbCycleTime.Name = "tbCycleTime"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'cbCakeResistance
        '
        resources.ApplyResources(Me.cbCakeResistance, "cbCakeResistance")
        Me.cbCakeResistance.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbCakeResistance.FormattingEnabled = True
        Me.cbCakeResistance.Items.AddRange(New Object() {resources.GetString("cbCakeResistance.Items"), resources.GetString("cbCakeResistance.Items1"), resources.GetString("cbCakeResistance.Items2")})
        Me.cbCakeResistance.Name = "cbCakeResistance"
        '
        'tbCakeResistance
        '
        resources.ApplyResources(Me.tbCakeResistance, "tbCakeResistance")
        Me.tbCakeResistance.Name = "tbCakeResistance"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbFilterResistance
        '
        resources.ApplyResources(Me.cbFilterResistance, "cbFilterResistance")
        Me.cbFilterResistance.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFilterResistance.FormattingEnabled = True
        Me.cbFilterResistance.Items.AddRange(New Object() {resources.GetString("cbFilterResistance.Items"), resources.GetString("cbFilterResistance.Items1"), resources.GetString("cbFilterResistance.Items2")})
        Me.cbFilterResistance.Name = "cbFilterResistance"
        '
        'tbFilterMediumResistance
        '
        resources.ApplyResources(Me.tbFilterMediumResistance, "tbFilterMediumResistance")
        Me.tbFilterMediumResistance.Name = "tbFilterMediumResistance"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'rbEvaluation
        '
        resources.ApplyResources(Me.rbEvaluation, "rbEvaluation")
        Me.rbEvaluation.Name = "rbEvaluation"
        Me.rbEvaluation.TabStop = True
        Me.rbEvaluation.UseVisualStyleBackColor = True
        '
        'rbSizing
        '
        resources.ApplyResources(Me.rbSizing, "rbSizing")
        Me.rbSizing.Name = "rbSizing"
        Me.rbSizing.TabStop = True
        Me.rbSizing.UseVisualStyleBackColor = True
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.cbOutlet2)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBox1.Controls.Add(Me.Label14)
        Me.GroupBox1.Controls.Add(Me.cbEnergy)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect1)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbOutlet1)
        Me.GroupBox1.Controls.Add(Me.cbInlet1)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'btnCreateAndConnectEnergy
        '
        resources.ApplyResources(Me.btnCreateAndConnectEnergy, "btnCreateAndConnectEnergy")
        Me.btnCreateAndConnectEnergy.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectEnergy.Name = "btnCreateAndConnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectEnergy, resources.GetString("btnCreateAndConnectEnergy.ToolTip"))
        Me.btnCreateAndConnectEnergy.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet2, "btnCreateAndConnectOutlet2")
        Me.btnCreateAndConnectOutlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet2.Name = "btnCreateAndConnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet2, resources.GetString("btnCreateAndConnectOutlet2.ToolTip"))
        Me.btnCreateAndConnectOutlet2.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectOutlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectOutlet1, "btnCreateAndConnectOutlet1")
        Me.btnCreateAndConnectOutlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectOutlet1.Name = "btnCreateAndConnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectOutlet1, resources.GetString("btnCreateAndConnectOutlet1.ToolTip"))
        Me.btnCreateAndConnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet1
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet1, "btnCreateAndConnectInlet1")
        Me.btnCreateAndConnectInlet1.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet1.Name = "btnCreateAndConnectInlet1"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet1, resources.GetString("btnCreateAndConnectInlet1.ToolTip"))
        Me.btnCreateAndConnectInlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnectOutlet2
        '
        resources.ApplyResources(Me.btnDisconnectOutlet2, "btnDisconnectOutlet2")
        Me.btnDisconnectOutlet2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet2.Name = "btnDisconnectOutlet2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet2, resources.GetString("btnDisconnectOutlet2.ToolTip"))
        Me.btnDisconnectOutlet2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbOutlet2
        '
        resources.ApplyResources(Me.cbOutlet2, "cbOutlet2")
        Me.cbOutlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet2.FormattingEnabled = True
        Me.cbOutlet2.Name = "cbOutlet2"
        '
        'btnDisconnectEnergy
        '
        resources.ApplyResources(Me.btnDisconnectEnergy, "btnDisconnectEnergy")
        Me.btnDisconnectEnergy.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectEnergy, resources.GetString("btnDisconnectEnergy.ToolTip"))
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbEnergy
        '
        resources.ApplyResources(Me.cbEnergy, "cbEnergy")
        Me.cbEnergy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy.FormattingEnabled = True
        Me.cbEnergy.Name = "cbEnergy"
        '
        'btnDisconnectOutlet1
        '
        resources.ApplyResources(Me.btnDisconnectOutlet1, "btnDisconnectOutlet1")
        Me.btnDisconnectOutlet1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnectOutlet1.Name = "btnDisconnectOutlet1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnectOutlet1, resources.GetString("btnDisconnectOutlet1.ToolTip"))
        Me.btnDisconnectOutlet1.UseVisualStyleBackColor = True
        '
        'btnDisconnect1
        '
        resources.ApplyResources(Me.btnDisconnect1, "btnDisconnect1")
        Me.btnDisconnect1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect1.Name = "btnDisconnect1"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect1, resources.GetString("btnDisconnect1.ToolTip"))
        Me.btnDisconnect1.UseVisualStyleBackColor = True
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'cbOutlet1
        '
        resources.ApplyResources(Me.cbOutlet1, "cbOutlet1")
        Me.cbOutlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOutlet1.FormattingEnabled = True
        Me.cbOutlet1.Name = "cbOutlet1"
        '
        'cbInlet1
        '
        resources.ApplyResources(Me.cbInlet1, "cbInlet1")
        Me.cbInlet1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet1.FormattingEnabled = True
        Me.cbInlet1.Name = "cbInlet1"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'EditingForm_Filter
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox3)
        Me.Name = "EditingForm_Filter"
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
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
    Public WithEvents cbFlashAlg As System.Windows.Forms.ComboBox
    Public WithEvents Label10 As System.Windows.Forms.Label
    Public WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
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
    Public WithEvents rbEvaluation As System.Windows.Forms.RadioButton
    Public WithEvents rbSizing As System.Windows.Forms.RadioButton
    Public WithEvents btnDisconnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents cbOutlet2 As System.Windows.Forms.ComboBox
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectEnergy As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents cbArea As System.Windows.Forms.ComboBox
    Public WithEvents tbTotalArea As System.Windows.Forms.TextBox
    Public WithEvents Label15 As System.Windows.Forms.Label
    Public WithEvents tbCakeRelativeHumidity As System.Windows.Forms.TextBox
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents tbSubmergedArea As System.Windows.Forms.TextBox
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents cbCycleTime As System.Windows.Forms.ComboBox
    Public WithEvents tbCycleTime As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents cbCakeResistance As System.Windows.Forms.ComboBox
    Public WithEvents tbCakeResistance As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbFilterResistance As System.Windows.Forms.ComboBox
    Public WithEvents tbFilterMediumResistance As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbPDrop As System.Windows.Forms.ComboBox
    Public WithEvents tbPressureDrop As System.Windows.Forms.TextBox
    Public WithEvents Label16 As System.Windows.Forms.Label
End Class
