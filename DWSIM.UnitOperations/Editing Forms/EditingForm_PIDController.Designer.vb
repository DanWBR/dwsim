<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_PIDController

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_PIDController))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbTargetUnits = New System.Windows.Forms.ComboBox()
        Me.cbTargetType = New System.Windows.Forms.ComboBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.cbSourceUnits = New System.Windows.Forms.ComboBox()
        Me.cbSourceType = New System.Windows.Forms.ComboBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.lblTargetVal = New System.Windows.Forms.Label()
        Me.lblSourceVal = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbTargetProp = New System.Windows.Forms.ComboBox()
        Me.cbTargetObj = New System.Windows.Forms.ComboBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbSourceProp = New System.Windows.Forms.ComboBox()
        Me.cbSourceObj = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.tbOutputMax = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.tbOutputMin = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.chkControllerActive = New System.Windows.Forms.CheckBox()
        Me.tbOffset = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tbKd = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.tbKi = New System.Windows.Forms.TextBox()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.tbKp = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbSetPoint = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.Label14)
        Me.GroupBox1.Controls.Add(Me.cbTargetUnits)
        Me.GroupBox1.Controls.Add(Me.cbTargetType)
        Me.GroupBox1.Controls.Add(Me.Label15)
        Me.GroupBox1.Controls.Add(Me.Label12)
        Me.GroupBox1.Controls.Add(Me.cbSourceUnits)
        Me.GroupBox1.Controls.Add(Me.cbSourceType)
        Me.GroupBox1.Controls.Add(Me.Label13)
        Me.GroupBox1.Controls.Add(Me.lblTargetVal)
        Me.GroupBox1.Controls.Add(Me.lblSourceVal)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.cbTargetProp)
        Me.GroupBox1.Controls.Add(Me.cbTargetObj)
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbSourceProp)
        Me.GroupBox1.Controls.Add(Me.cbSourceObj)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'cbTargetUnits
        '
        resources.ApplyResources(Me.cbTargetUnits, "cbTargetUnits")
        Me.cbTargetUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetUnits.FormattingEnabled = True
        Me.cbTargetUnits.Name = "cbTargetUnits"
        '
        'cbTargetType
        '
        resources.ApplyResources(Me.cbTargetType, "cbTargetType")
        Me.cbTargetType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetType.FormattingEnabled = True
        Me.cbTargetType.Name = "cbTargetType"
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'cbSourceUnits
        '
        resources.ApplyResources(Me.cbSourceUnits, "cbSourceUnits")
        Me.cbSourceUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceUnits.FormattingEnabled = True
        Me.cbSourceUnits.Name = "cbSourceUnits"
        '
        'cbSourceType
        '
        resources.ApplyResources(Me.cbSourceType, "cbSourceType")
        Me.cbSourceType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceType.FormattingEnabled = True
        Me.cbSourceType.Name = "cbSourceType"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        '
        'lblTargetVal
        '
        resources.ApplyResources(Me.lblTargetVal, "lblTargetVal")
        Me.lblTargetVal.Name = "lblTargetVal"
        '
        'lblSourceVal
        '
        resources.ApplyResources(Me.lblSourceVal, "lblSourceVal")
        Me.lblSourceVal.Name = "lblSourceVal"
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
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbTargetProp
        '
        resources.ApplyResources(Me.cbTargetProp, "cbTargetProp")
        Me.cbTargetProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetProp.FormattingEnabled = True
        Me.cbTargetProp.Name = "cbTargetProp"
        '
        'cbTargetObj
        '
        resources.ApplyResources(Me.cbTargetObj, "cbTargetObj")
        Me.cbTargetObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetObj.FormattingEnabled = True
        Me.cbTargetObj.Name = "cbTargetObj"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'cbSourceProp
        '
        resources.ApplyResources(Me.cbSourceProp, "cbSourceProp")
        Me.cbSourceProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceProp.FormattingEnabled = True
        Me.cbSourceProp.Name = "cbSourceProp"
        '
        'cbSourceObj
        '
        resources.ApplyResources(Me.cbSourceObj, "cbSourceObj")
        Me.cbSourceObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceObj.FormattingEnabled = True
        Me.cbSourceObj.Name = "cbSourceObj"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
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
        Me.ToolTipValues.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.chkActive.UseVisualStyleBackColor = True
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
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.18362}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.tbOutputMax)
        Me.GroupBox2.Controls.Add(Me.Label17)
        Me.GroupBox2.Controls.Add(Me.tbOutputMin)
        Me.GroupBox2.Controls.Add(Me.Label16)
        Me.GroupBox2.Controls.Add(Me.chkControllerActive)
        Me.GroupBox2.Controls.Add(Me.tbOffset)
        Me.GroupBox2.Controls.Add(Me.Label10)
        Me.GroupBox2.Controls.Add(Me.tbKd)
        Me.GroupBox2.Controls.Add(Me.Label9)
        Me.GroupBox2.Controls.Add(Me.tbKi)
        Me.GroupBox2.Controls.Add(Me.Label8)
        Me.GroupBox2.Controls.Add(Me.tbKp)
        Me.GroupBox2.Controls.Add(Me.Label1)
        Me.GroupBox2.Controls.Add(Me.tbSetPoint)
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'tbOutputMax
        '
        resources.ApplyResources(Me.tbOutputMax, "tbOutputMax")
        Me.tbOutputMax.Name = "tbOutputMax"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        '
        'tbOutputMin
        '
        resources.ApplyResources(Me.tbOutputMin, "tbOutputMin")
        Me.tbOutputMin.Name = "tbOutputMin"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'chkControllerActive
        '
        resources.ApplyResources(Me.chkControllerActive, "chkControllerActive")
        Me.chkControllerActive.Name = "chkControllerActive"
        Me.chkControllerActive.UseVisualStyleBackColor = True
        '
        'tbOffset
        '
        resources.ApplyResources(Me.tbOffset, "tbOffset")
        Me.tbOffset.Name = "tbOffset"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'tbKd
        '
        resources.ApplyResources(Me.tbKd, "tbKd")
        Me.tbKd.Name = "tbKd"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'tbKi
        '
        resources.ApplyResources(Me.tbKi, "tbKi")
        Me.tbKi.Name = "tbKi"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'tbKp
        '
        resources.ApplyResources(Me.tbKp, "tbKp")
        Me.tbKp.Name = "tbKp"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbSetPoint
        '
        resources.ApplyResources(Me.tbSetPoint, "tbSetPoint")
        Me.tbSetPoint.Name = "tbSetPoint"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_PIDController
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox2)
        Me.Name = "EditingForm_PIDController"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBox1 As GroupBox
    Public WithEvents lblTargetVal As Label
    Public WithEvents lblSourceVal As Label
    Public WithEvents Label6 As Label
    Public WithEvents Label5 As Label
    Public WithEvents Label2 As Label
    Public WithEvents cbTargetProp As ComboBox
    Public WithEvents cbTargetObj As ComboBox
    Public WithEvents Label4 As Label
    Public WithEvents Label7 As Label
    Public WithEvents cbSourceProp As ComboBox
    Public WithEvents cbSourceObj As ComboBox
    Public WithEvents Label19 As Label
    Public WithEvents GroupBox5 As GroupBox
    Public WithEvents lblTag As TextBox
    Public WithEvents chkActive As CheckBox
    Public WithEvents Label11 As Label
    Public WithEvents GroupBox4 As GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents GroupBox2 As GroupBox
    Public WithEvents tbKd As TextBox
    Public WithEvents Label9 As Label
    Public WithEvents tbKi As TextBox
    Public WithEvents Label8 As Label
    Public WithEvents tbKp As TextBox
    Public WithEvents Label1 As Label
    Public WithEvents tbSetPoint As TextBox
    Public WithEvents Label3 As Label
    Public WithEvents tbOffset As TextBox
    Public WithEvents Label10 As Label
    Public WithEvents Label14 As Label
    Public WithEvents cbTargetUnits As ComboBox
    Public WithEvents cbTargetType As ComboBox
    Public WithEvents Label15 As Label
    Public WithEvents Label12 As Label
    Public WithEvents cbSourceUnits As ComboBox
    Public WithEvents cbSourceType As ComboBox
    Public WithEvents Label13 As Label
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents chkControllerActive As CheckBox
    Public WithEvents tbOutputMax As TextBox
    Public WithEvents Label17 As Label
    Public WithEvents tbOutputMin As TextBox
    Public WithEvents Label16 As Label
End Class
