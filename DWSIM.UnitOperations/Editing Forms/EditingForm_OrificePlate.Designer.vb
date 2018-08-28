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
        Me.btnConfigureFlashAlg = New System.Windows.Forms.Button()
        Me.btnConfigurePP = New System.Windows.Forms.Button()
        Me.cbFlashAlg = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
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
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
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
        Me.GroupBox2.Controls.Add(Me.cbIntPipeDiam)
        Me.GroupBox2.Controls.Add(Me.tbIntPipeDiameter)
        Me.GroupBox2.Controls.Add(Me.Label14)
        Me.GroupBox2.Controls.Add(Me.cbOrificePdrop)
        Me.GroupBox2.Controls.Add(Me.tbOrificePDrop)
        Me.GroupBox2.Controls.Add(Me.Label6)
        Me.GroupBox2.Controls.Add(Me.cbOverallPdrop)
        Me.GroupBox2.Controls.Add(Me.tbOverallPDrop)
        Me.GroupBox2.Controls.Add(Me.Label1)
        Me.GroupBox2.Controls.Add(Me.rbRadius)
        Me.GroupBox2.Controls.Add(Me.rbFlange)
        Me.GroupBox2.Controls.Add(Me.rbCorner)
        Me.GroupBox2.Controls.Add(Me.cbDeltaT)
        Me.GroupBox2.Controls.Add(Me.tbDeltaT)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Controls.Add(Me.tbCorrF)
        Me.GroupBox2.Controls.Add(Me.Label4)
        Me.GroupBox2.Controls.Add(Me.tbBeta)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.cbOrifDiam)
        Me.GroupBox2.Controls.Add(Me.tbOrificeDiameter)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'cbIntPipeDiam
        '
        resources.ApplyResources(Me.cbIntPipeDiam, "cbIntPipeDiam")
        Me.cbIntPipeDiam.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbIntPipeDiam.FormattingEnabled = True
        Me.cbIntPipeDiam.Items.AddRange(New Object() {resources.GetString("cbIntPipeDiam.Items"), resources.GetString("cbIntPipeDiam.Items1"), resources.GetString("cbIntPipeDiam.Items2")})
        Me.cbIntPipeDiam.Name = "cbIntPipeDiam"
        '
        'tbIntPipeDiameter
        '
        resources.ApplyResources(Me.tbIntPipeDiameter, "tbIntPipeDiameter")
        Me.tbIntPipeDiameter.Name = "tbIntPipeDiameter"
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbOrificePdrop
        '
        resources.ApplyResources(Me.cbOrificePdrop, "cbOrificePdrop")
        Me.cbOrificePdrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrificePdrop.FormattingEnabled = True
        Me.cbOrificePdrop.Items.AddRange(New Object() {resources.GetString("cbOrificePdrop.Items"), resources.GetString("cbOrificePdrop.Items1"), resources.GetString("cbOrificePdrop.Items2")})
        Me.cbOrificePdrop.Name = "cbOrificePdrop"
        '
        'tbOrificePDrop
        '
        resources.ApplyResources(Me.tbOrificePDrop, "tbOrificePDrop")
        Me.tbOrificePDrop.Name = "tbOrificePDrop"
        Me.tbOrificePDrop.ReadOnly = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'cbOverallPdrop
        '
        resources.ApplyResources(Me.cbOverallPdrop, "cbOverallPdrop")
        Me.cbOverallPdrop.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOverallPdrop.FormattingEnabled = True
        Me.cbOverallPdrop.Items.AddRange(New Object() {resources.GetString("cbOverallPdrop.Items"), resources.GetString("cbOverallPdrop.Items1"), resources.GetString("cbOverallPdrop.Items2")})
        Me.cbOverallPdrop.Name = "cbOverallPdrop"
        '
        'tbOverallPDrop
        '
        resources.ApplyResources(Me.tbOverallPDrop, "tbOverallPDrop")
        Me.tbOverallPDrop.Name = "tbOverallPDrop"
        Me.tbOverallPDrop.ReadOnly = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'rbRadius
        '
        resources.ApplyResources(Me.rbRadius, "rbRadius")
        Me.rbRadius.Name = "rbRadius"
        Me.rbRadius.TabStop = True
        Me.rbRadius.UseVisualStyleBackColor = True
        '
        'rbFlange
        '
        resources.ApplyResources(Me.rbFlange, "rbFlange")
        Me.rbFlange.Name = "rbFlange"
        Me.rbFlange.TabStop = True
        Me.rbFlange.UseVisualStyleBackColor = True
        '
        'rbCorner
        '
        resources.ApplyResources(Me.rbCorner, "rbCorner")
        Me.rbCorner.Name = "rbCorner"
        Me.rbCorner.TabStop = True
        Me.rbCorner.UseVisualStyleBackColor = True
        '
        'cbDeltaT
        '
        resources.ApplyResources(Me.cbDeltaT, "cbDeltaT")
        Me.cbDeltaT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbDeltaT.FormattingEnabled = True
        Me.cbDeltaT.Items.AddRange(New Object() {resources.GetString("cbDeltaT.Items"), resources.GetString("cbDeltaT.Items1"), resources.GetString("cbDeltaT.Items2")})
        Me.cbDeltaT.Name = "cbDeltaT"
        '
        'tbDeltaT
        '
        resources.ApplyResources(Me.tbDeltaT, "tbDeltaT")
        Me.tbDeltaT.Name = "tbDeltaT"
        Me.tbDeltaT.ReadOnly = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'tbCorrF
        '
        resources.ApplyResources(Me.tbCorrF, "tbCorrF")
        Me.tbCorrF.Name = "tbCorrF"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'tbBeta
        '
        resources.ApplyResources(Me.tbBeta, "tbBeta")
        Me.tbBeta.Name = "tbBeta"
        Me.tbBeta.ReadOnly = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbOrifDiam
        '
        resources.ApplyResources(Me.cbOrifDiam, "cbOrifDiam")
        Me.cbOrifDiam.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrifDiam.FormattingEnabled = True
        Me.cbOrifDiam.Items.AddRange(New Object() {resources.GetString("cbOrifDiam.Items"), resources.GetString("cbOrifDiam.Items1"), resources.GetString("cbOrifDiam.Items2")})
        Me.cbOrifDiam.Name = "cbOrifDiam"
        '
        'tbOrificeDiameter
        '
        resources.ApplyResources(Me.tbOrificeDiameter, "tbOrificeDiameter")
        Me.tbOrificeDiameter.Name = "tbOrificeDiameter"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBox1.Controls.Add(Me.btnDisconnect1)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbOutlet1)
        Me.GroupBox1.Controls.Add(Me.cbInlet1)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
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
        'EditingForm_OrificePlate
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Name = "EditingForm_OrificePlate"
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
    Public WithEvents cbOrifDiam As System.Windows.Forms.ComboBox
    Public WithEvents tbOrificeDiameter As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents btnConfigureFlashAlg As System.Windows.Forms.Button
    Public WithEvents btnConfigurePP As System.Windows.Forms.Button
    Public WithEvents tbBeta As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
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
End Class
