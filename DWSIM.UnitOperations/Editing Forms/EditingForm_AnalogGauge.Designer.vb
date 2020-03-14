<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_AnalogGauge

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_AnalogGauge))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbSourceUnits = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbSourceUnitType = New System.Windows.Forms.ComboBox()
        Me.tbMaxVal = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbMinVal = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lblSourceVal = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbSourceProp = New System.Windows.Forms.ComboBox()
        Me.cbSourceObj = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.tbVeryHigh = New System.Windows.Forms.TextBox()
        Me.chkVeryHigh = New System.Windows.Forms.CheckBox()
        Me.tbHigh = New System.Windows.Forms.TextBox()
        Me.chkHigh = New System.Windows.Forms.CheckBox()
        Me.tbLow = New System.Windows.Forms.TextBox()
        Me.chkLow = New System.Windows.Forms.CheckBox()
        Me.tbVeryLow = New System.Windows.Forms.TextBox()
        Me.chkVeryLow = New System.Windows.Forms.CheckBox()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.cbSourceUnits)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.cbSourceUnitType)
        Me.GroupBox1.Controls.Add(Me.tbMaxVal)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.tbMinVal)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Controls.Add(Me.lblSourceVal)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbSourceProp)
        Me.GroupBox1.Controls.Add(Me.cbSourceObj)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'cbSourceUnits
        '
        resources.ApplyResources(Me.cbSourceUnits, "cbSourceUnits")
        Me.cbSourceUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceUnits.FormattingEnabled = True
        Me.cbSourceUnits.Name = "cbSourceUnits"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbSourceUnitType
        '
        resources.ApplyResources(Me.cbSourceUnitType, "cbSourceUnitType")
        Me.cbSourceUnitType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceUnitType.FormattingEnabled = True
        Me.cbSourceUnitType.Name = "cbSourceUnitType"
        '
        'tbMaxVal
        '
        resources.ApplyResources(Me.tbMaxVal, "tbMaxVal")
        Me.tbMaxVal.Name = "tbMaxVal"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'tbMinVal
        '
        resources.ApplyResources(Me.tbMinVal, "tbMinVal")
        Me.tbMinVal.Name = "tbMinVal"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'lblSourceVal
        '
        resources.ApplyResources(Me.lblSourceVal, "lblSourceVal")
        Me.lblSourceVal.Name = "lblSourceVal"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.tbVeryHigh)
        Me.GroupBox2.Controls.Add(Me.chkVeryHigh)
        Me.GroupBox2.Controls.Add(Me.tbHigh)
        Me.GroupBox2.Controls.Add(Me.chkHigh)
        Me.GroupBox2.Controls.Add(Me.tbLow)
        Me.GroupBox2.Controls.Add(Me.chkLow)
        Me.GroupBox2.Controls.Add(Me.tbVeryLow)
        Me.GroupBox2.Controls.Add(Me.chkVeryLow)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'tbVeryHigh
        '
        resources.ApplyResources(Me.tbVeryHigh, "tbVeryHigh")
        Me.tbVeryHigh.Name = "tbVeryHigh"
        '
        'chkVeryHigh
        '
        resources.ApplyResources(Me.chkVeryHigh, "chkVeryHigh")
        Me.chkVeryHigh.Name = "chkVeryHigh"
        Me.chkVeryHigh.UseVisualStyleBackColor = True
        '
        'tbHigh
        '
        resources.ApplyResources(Me.tbHigh, "tbHigh")
        Me.tbHigh.Name = "tbHigh"
        '
        'chkHigh
        '
        resources.ApplyResources(Me.chkHigh, "chkHigh")
        Me.chkHigh.Name = "chkHigh"
        Me.chkHigh.UseVisualStyleBackColor = True
        '
        'tbLow
        '
        resources.ApplyResources(Me.tbLow, "tbLow")
        Me.tbLow.Name = "tbLow"
        '
        'chkLow
        '
        resources.ApplyResources(Me.chkLow, "chkLow")
        Me.chkLow.Name = "chkLow"
        Me.chkLow.UseVisualStyleBackColor = True
        '
        'tbVeryLow
        '
        resources.ApplyResources(Me.tbVeryLow, "tbVeryLow")
        Me.tbVeryLow.Name = "tbVeryLow"
        '
        'chkVeryLow
        '
        resources.ApplyResources(Me.chkVeryLow, "chkVeryLow")
        Me.chkVeryLow.Name = "chkVeryLow"
        Me.chkVeryLow.UseVisualStyleBackColor = True
        '
        'EditingForm_AnalogGauge
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Name = "EditingForm_AnalogGauge"
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBox1 As GroupBox
    Public WithEvents Label4 As Label
    Public WithEvents cbSourceUnits As ComboBox
    Public WithEvents Label3 As Label
    Public WithEvents cbSourceUnitType As ComboBox
    Public WithEvents tbMaxVal As TextBox
    Public WithEvents Label2 As Label
    Public WithEvents tbMinVal As TextBox
    Public WithEvents Label1 As Label
    Public WithEvents lblSourceVal As Label
    Public WithEvents Label5 As Label
    Public WithEvents Label7 As Label
    Public WithEvents cbSourceProp As ComboBox
    Public WithEvents cbSourceObj As ComboBox
    Public WithEvents Label19 As Label
    Public WithEvents GroupBox5 As GroupBox
    Public WithEvents lblTag As TextBox
    Public WithEvents chkActive As CheckBox
    Public WithEvents Label11 As Label
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents GroupBox2 As GroupBox
    Public WithEvents tbVeryHigh As TextBox
    Friend WithEvents chkVeryHigh As CheckBox
    Public WithEvents tbHigh As TextBox
    Friend WithEvents chkHigh As CheckBox
    Public WithEvents tbLow As TextBox
    Friend WithEvents chkLow As CheckBox
    Public WithEvents tbVeryLow As TextBox
    Friend WithEvents chkVeryLow As CheckBox
End Class
