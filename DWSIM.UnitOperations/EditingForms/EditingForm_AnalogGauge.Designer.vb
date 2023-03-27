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
        Me.GroupBoxMonitoredProperty = New System.Windows.Forms.GroupBox()
        Me.chkDisplayPercent = New System.Windows.Forms.CheckBox()
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
        Me.GroupBoxAlarms = New System.Windows.Forms.GroupBox()
        Me.chkShowAlarms = New System.Windows.Forms.CheckBox()
        Me.tbVeryHigh = New System.Windows.Forms.TextBox()
        Me.chkVeryHigh = New System.Windows.Forms.CheckBox()
        Me.tbHigh = New System.Windows.Forms.TextBox()
        Me.chkHigh = New System.Windows.Forms.CheckBox()
        Me.tbLow = New System.Windows.Forms.TextBox()
        Me.chkLow = New System.Windows.Forms.CheckBox()
        Me.tbVeryLow = New System.Windows.Forms.TextBox()
        Me.chkVeryLow = New System.Windows.Forms.CheckBox()
        Me.GroupBoxMonitoredProperty.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBoxAlarms.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxMonitoredProperty
        '
        resources.ApplyResources(Me.GroupBoxMonitoredProperty, "GroupBoxMonitoredProperty")
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.chkDisplayPercent)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label4)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.cbSourceUnits)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label3)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.cbSourceUnitType)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.tbMaxVal)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label2)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.tbMinVal)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label1)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.lblSourceVal)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label5)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label7)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.cbSourceProp)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.cbSourceObj)
        Me.GroupBoxMonitoredProperty.Controls.Add(Me.Label19)
        Me.GroupBoxMonitoredProperty.Name = "GroupBoxMonitoredProperty"
        Me.GroupBoxMonitoredProperty.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBoxMonitoredProperty, resources.GetString("GroupBoxMonitoredProperty.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxMonitoredProperty, resources.GetString("GroupBoxMonitoredProperty.ToolTip1"))
        '
        'chkDisplayPercent
        '
        resources.ApplyResources(Me.chkDisplayPercent, "chkDisplayPercent")
        Me.chkDisplayPercent.Name = "chkDisplayPercent"
        Me.ToolTipValues.SetToolTip(Me.chkDisplayPercent, resources.GetString("chkDisplayPercent.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkDisplayPercent, resources.GetString("chkDisplayPercent.ToolTip1"))
        Me.chkDisplayPercent.UseVisualStyleBackColor = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        '
        'cbSourceUnits
        '
        resources.ApplyResources(Me.cbSourceUnits, "cbSourceUnits")
        Me.cbSourceUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceUnits.FormattingEnabled = True
        Me.cbSourceUnits.Name = "cbSourceUnits"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceUnits, resources.GetString("cbSourceUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceUnits, resources.GetString("cbSourceUnits.ToolTip1"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        '
        'cbSourceUnitType
        '
        resources.ApplyResources(Me.cbSourceUnitType, "cbSourceUnitType")
        Me.cbSourceUnitType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceUnitType.FormattingEnabled = True
        Me.cbSourceUnitType.Name = "cbSourceUnitType"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceUnitType, resources.GetString("cbSourceUnitType.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceUnitType, resources.GetString("cbSourceUnitType.ToolTip1"))
        '
        'tbMaxVal
        '
        resources.ApplyResources(Me.tbMaxVal, "tbMaxVal")
        Me.tbMaxVal.Name = "tbMaxVal"
        Me.ToolTipChangeTag.SetToolTip(Me.tbMaxVal, resources.GetString("tbMaxVal.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbMaxVal, resources.GetString("tbMaxVal.ToolTip1"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        '
        'tbMinVal
        '
        resources.ApplyResources(Me.tbMinVal, "tbMinVal")
        Me.tbMinVal.Name = "tbMinVal"
        Me.ToolTipChangeTag.SetToolTip(Me.tbMinVal, resources.GetString("tbMinVal.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbMinVal, resources.GetString("tbMinVal.ToolTip1"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        '
        'lblSourceVal
        '
        resources.ApplyResources(Me.lblSourceVal, "lblSourceVal")
        Me.lblSourceVal.Name = "lblSourceVal"
        Me.ToolTipValues.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip1"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        '
        'cbSourceProp
        '
        resources.ApplyResources(Me.cbSourceProp, "cbSourceProp")
        Me.cbSourceProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceProp.FormattingEnabled = True
        Me.cbSourceProp.Name = "cbSourceProp"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceProp, resources.GetString("cbSourceProp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceProp, resources.GetString("cbSourceProp.ToolTip1"))
        '
        'cbSourceObj
        '
        resources.ApplyResources(Me.cbSourceObj, "cbSourceObj")
        Me.cbSourceObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceObj.FormattingEnabled = True
        Me.cbSourceObj.Name = "cbSourceObj"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceObj, resources.GetString("cbSourceObj.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceObj, resources.GetString("cbSourceObj.ToolTip1"))
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTipValues.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip1"))
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox5, resources.GetString("GroupBox5.ToolTip1"))
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
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.Name = "chkActive"
        Me.ToolTipValues.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkActive, resources.GetString("chkActive.ToolTip1"))
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTipValues.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip1"))
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'GroupBoxAlarms
        '
        resources.ApplyResources(Me.GroupBoxAlarms, "GroupBoxAlarms")
        Me.GroupBoxAlarms.Controls.Add(Me.chkShowAlarms)
        Me.GroupBoxAlarms.Controls.Add(Me.tbVeryHigh)
        Me.GroupBoxAlarms.Controls.Add(Me.chkVeryHigh)
        Me.GroupBoxAlarms.Controls.Add(Me.tbHigh)
        Me.GroupBoxAlarms.Controls.Add(Me.chkHigh)
        Me.GroupBoxAlarms.Controls.Add(Me.tbLow)
        Me.GroupBoxAlarms.Controls.Add(Me.chkLow)
        Me.GroupBoxAlarms.Controls.Add(Me.tbVeryLow)
        Me.GroupBoxAlarms.Controls.Add(Me.chkVeryLow)
        Me.GroupBoxAlarms.Name = "GroupBoxAlarms"
        Me.GroupBoxAlarms.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBoxAlarms, resources.GetString("GroupBoxAlarms.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxAlarms, resources.GetString("GroupBoxAlarms.ToolTip1"))
        '
        'chkShowAlarms
        '
        resources.ApplyResources(Me.chkShowAlarms, "chkShowAlarms")
        Me.chkShowAlarms.Name = "chkShowAlarms"
        Me.ToolTipValues.SetToolTip(Me.chkShowAlarms, resources.GetString("chkShowAlarms.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkShowAlarms, resources.GetString("chkShowAlarms.ToolTip1"))
        Me.chkShowAlarms.UseVisualStyleBackColor = True
        '
        'tbVeryHigh
        '
        resources.ApplyResources(Me.tbVeryHigh, "tbVeryHigh")
        Me.tbVeryHigh.Name = "tbVeryHigh"
        Me.ToolTipChangeTag.SetToolTip(Me.tbVeryHigh, resources.GetString("tbVeryHigh.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbVeryHigh, resources.GetString("tbVeryHigh.ToolTip1"))
        '
        'chkVeryHigh
        '
        resources.ApplyResources(Me.chkVeryHigh, "chkVeryHigh")
        Me.chkVeryHigh.Name = "chkVeryHigh"
        Me.ToolTipValues.SetToolTip(Me.chkVeryHigh, resources.GetString("chkVeryHigh.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkVeryHigh, resources.GetString("chkVeryHigh.ToolTip1"))
        Me.chkVeryHigh.UseVisualStyleBackColor = True
        '
        'tbHigh
        '
        resources.ApplyResources(Me.tbHigh, "tbHigh")
        Me.tbHigh.Name = "tbHigh"
        Me.ToolTipChangeTag.SetToolTip(Me.tbHigh, resources.GetString("tbHigh.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbHigh, resources.GetString("tbHigh.ToolTip1"))
        '
        'chkHigh
        '
        resources.ApplyResources(Me.chkHigh, "chkHigh")
        Me.chkHigh.Name = "chkHigh"
        Me.ToolTipValues.SetToolTip(Me.chkHigh, resources.GetString("chkHigh.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkHigh, resources.GetString("chkHigh.ToolTip1"))
        Me.chkHigh.UseVisualStyleBackColor = True
        '
        'tbLow
        '
        resources.ApplyResources(Me.tbLow, "tbLow")
        Me.tbLow.Name = "tbLow"
        Me.ToolTipChangeTag.SetToolTip(Me.tbLow, resources.GetString("tbLow.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbLow, resources.GetString("tbLow.ToolTip1"))
        '
        'chkLow
        '
        resources.ApplyResources(Me.chkLow, "chkLow")
        Me.chkLow.Name = "chkLow"
        Me.ToolTipValues.SetToolTip(Me.chkLow, resources.GetString("chkLow.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkLow, resources.GetString("chkLow.ToolTip1"))
        Me.chkLow.UseVisualStyleBackColor = True
        '
        'tbVeryLow
        '
        resources.ApplyResources(Me.tbVeryLow, "tbVeryLow")
        Me.tbVeryLow.Name = "tbVeryLow"
        Me.ToolTipChangeTag.SetToolTip(Me.tbVeryLow, resources.GetString("tbVeryLow.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbVeryLow, resources.GetString("tbVeryLow.ToolTip1"))
        '
        'chkVeryLow
        '
        resources.ApplyResources(Me.chkVeryLow, "chkVeryLow")
        Me.chkVeryLow.Name = "chkVeryLow"
        Me.ToolTipValues.SetToolTip(Me.chkVeryLow, resources.GetString("chkVeryLow.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkVeryLow, resources.GetString("chkVeryLow.ToolTip1"))
        Me.chkVeryLow.UseVisualStyleBackColor = True
        '
        'EditingForm_AnalogGauge
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxAlarms)
        Me.Controls.Add(Me.GroupBoxMonitoredProperty)
        Me.Controls.Add(Me.GroupBox5)
        Me.Name = "EditingForm_AnalogGauge"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.GroupBoxMonitoredProperty.ResumeLayout(False)
        Me.GroupBoxMonitoredProperty.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBoxAlarms.ResumeLayout(False)
        Me.GroupBoxAlarms.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBoxMonitoredProperty As GroupBox
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
    Friend WithEvents GroupBoxAlarms As GroupBox
    Public WithEvents tbVeryHigh As TextBox
    Friend WithEvents chkVeryHigh As CheckBox
    Public WithEvents tbHigh As TextBox
    Friend WithEvents chkHigh As CheckBox
    Public WithEvents tbLow As TextBox
    Friend WithEvents chkLow As CheckBox
    Public WithEvents tbVeryLow As TextBox
    Friend WithEvents chkVeryLow As CheckBox
    Friend WithEvents chkShowAlarms As CheckBox
    Friend WithEvents chkDisplayPercent As CheckBox
End Class
