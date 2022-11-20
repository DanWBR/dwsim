<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_Switch

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Switch))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tbOnValue = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.cbSourceUnits = New System.Windows.Forms.ComboBox()
        Me.tbOffValue = New System.Windows.Forms.TextBox()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbSourceUnitType = New System.Windows.Forms.ComboBox()
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
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.tbOnValue)
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.Label17)
        Me.GroupBox1.Controls.Add(Me.cbSourceUnits)
        Me.GroupBox1.Controls.Add(Me.tbOffValue)
        Me.GroupBox1.Controls.Add(Me.Label16)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.cbSourceUnitType)
        Me.GroupBox1.Controls.Add(Me.lblSourceVal)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.cbSourceProp)
        Me.GroupBox1.Controls.Add(Me.cbSourceObj)
        Me.GroupBox1.Controls.Add(Me.Label19)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox1, resources.GetString("GroupBox1.ToolTip1"))
        '
        'tbOnValue
        '
        resources.ApplyResources(Me.tbOnValue, "tbOnValue")
        Me.tbOnValue.Name = "tbOnValue"
        Me.ToolTipChangeTag.SetToolTip(Me.tbOnValue, resources.GetString("tbOnValue.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbOnValue, resources.GetString("tbOnValue.ToolTip1"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
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
        'tbOffValue
        '
        resources.ApplyResources(Me.tbOffValue, "tbOffValue")
        Me.tbOffValue.Name = "tbOffValue"
        Me.ToolTipChangeTag.SetToolTip(Me.tbOffValue, resources.GetString("tbOffValue.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbOffValue, resources.GetString("tbOffValue.ToolTip1"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
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
        'EditingForm_Switch
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox5)
        Me.Name = "EditingForm_Switch"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBox1 As GroupBox
    Public WithEvents Label4 As Label
    Public WithEvents cbSourceUnits As ComboBox
    Public WithEvents Label3 As Label
    Public WithEvents cbSourceUnitType As ComboBox
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
    Public WithEvents tbOnValue As TextBox
    Public WithEvents Label17 As Label
    Public WithEvents tbOffValue As TextBox
    Public WithEvents Label16 As Label
End Class
