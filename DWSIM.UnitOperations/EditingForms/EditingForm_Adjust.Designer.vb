<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Adjust

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Adjust))
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.gbControlPanel = New System.Windows.Forms.GroupBox()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.tbTolerance = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.lblSPUnits = New System.Windows.Forms.Label()
        Me.tbSetPoint = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.chkSolveGlobal = New System.Windows.Forms.CheckBox()
        Me.GroupBoxLinkedObjects = New System.Windows.Forms.GroupBox()
        Me.chkUseReferenced = New System.Windows.Forms.CheckBox()
        Me.lblRefVal = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.cbRefProp = New System.Windows.Forms.ComboBox()
        Me.cbRefObj = New System.Windows.Forms.ComboBox()
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
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxLinkedObjects.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
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
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTip1.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip2"))
        '
        'gbControlPanel
        '
        resources.ApplyResources(Me.gbControlPanel, "gbControlPanel")
        Me.gbControlPanel.Name = "gbControlPanel"
        Me.gbControlPanel.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.gbControlPanel, resources.GetString("gbControlPanel.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.gbControlPanel, resources.GetString("gbControlPanel.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.gbControlPanel, resources.GetString("gbControlPanel.ToolTip2"))
        '
        'GroupBoxParameters
        '
        resources.ApplyResources(Me.GroupBoxParameters, "GroupBoxParameters")
        Me.GroupBoxParameters.Controls.Add(Me.tbTolerance)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.lblSPUnits)
        Me.GroupBoxParameters.Controls.Add(Me.tbSetPoint)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.chkSolveGlobal)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip2"))
        '
        'tbTolerance
        '
        resources.ApplyResources(Me.tbTolerance, "tbTolerance")
        Me.tbTolerance.Name = "tbTolerance"
        Me.ToolTipValues.SetToolTip(Me.tbTolerance, resources.GetString("tbTolerance.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbTolerance, resources.GetString("tbTolerance.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbTolerance, resources.GetString("tbTolerance.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'lblSPUnits
        '
        resources.ApplyResources(Me.lblSPUnits, "lblSPUnits")
        Me.lblSPUnits.Name = "lblSPUnits"
        Me.ToolTip1.SetToolTip(Me.lblSPUnits, resources.GetString("lblSPUnits.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblSPUnits, resources.GetString("lblSPUnits.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblSPUnits, resources.GetString("lblSPUnits.ToolTip2"))
        '
        'tbSetPoint
        '
        resources.ApplyResources(Me.tbSetPoint, "tbSetPoint")
        Me.tbSetPoint.Name = "tbSetPoint"
        Me.ToolTipValues.SetToolTip(Me.tbSetPoint, resources.GetString("tbSetPoint.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbSetPoint, resources.GetString("tbSetPoint.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbSetPoint, resources.GetString("tbSetPoint.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'chkSolveGlobal
        '
        resources.ApplyResources(Me.chkSolveGlobal, "chkSolveGlobal")
        Me.chkSolveGlobal.Name = "chkSolveGlobal"
        Me.ToolTip1.SetToolTip(Me.chkSolveGlobal, resources.GetString("chkSolveGlobal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkSolveGlobal, resources.GetString("chkSolveGlobal.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkSolveGlobal, resources.GetString("chkSolveGlobal.ToolTip2"))
        Me.chkSolveGlobal.UseVisualStyleBackColor = True
        '
        'GroupBoxLinkedObjects
        '
        resources.ApplyResources(Me.GroupBoxLinkedObjects, "GroupBoxLinkedObjects")
        Me.GroupBoxLinkedObjects.Controls.Add(Me.chkUseReferenced)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.lblRefVal)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label9)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label10)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbRefProp)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbRefObj)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.lblTargetVal)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.lblSourceVal)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label6)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label5)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label2)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbTargetProp)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbTargetObj)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label4)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label7)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbSourceProp)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbSourceObj)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label19)
        Me.GroupBoxLinkedObjects.Name = "GroupBoxLinkedObjects"
        Me.GroupBoxLinkedObjects.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxLinkedObjects, resources.GetString("GroupBoxLinkedObjects.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxLinkedObjects, resources.GetString("GroupBoxLinkedObjects.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxLinkedObjects, resources.GetString("GroupBoxLinkedObjects.ToolTip2"))
        '
        'chkUseReferenced
        '
        resources.ApplyResources(Me.chkUseReferenced, "chkUseReferenced")
        Me.chkUseReferenced.Name = "chkUseReferenced"
        Me.ToolTip1.SetToolTip(Me.chkUseReferenced, resources.GetString("chkUseReferenced.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkUseReferenced, resources.GetString("chkUseReferenced.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkUseReferenced, resources.GetString("chkUseReferenced.ToolTip2"))
        Me.chkUseReferenced.UseVisualStyleBackColor = True
        '
        'lblRefVal
        '
        resources.ApplyResources(Me.lblRefVal, "lblRefVal")
        Me.lblRefVal.Name = "lblRefVal"
        Me.ToolTip1.SetToolTip(Me.lblRefVal, resources.GetString("lblRefVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblRefVal, resources.GetString("lblRefVal.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblRefVal, resources.GetString("lblRefVal.ToolTip2"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTip1.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip2"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTip1.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip2"))
        '
        'cbRefProp
        '
        resources.ApplyResources(Me.cbRefProp, "cbRefProp")
        Me.cbRefProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRefProp.FormattingEnabled = True
        Me.cbRefProp.Name = "cbRefProp"
        Me.ToolTip1.SetToolTip(Me.cbRefProp, resources.GetString("cbRefProp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRefProp, resources.GetString("cbRefProp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRefProp, resources.GetString("cbRefProp.ToolTip2"))
        '
        'cbRefObj
        '
        resources.ApplyResources(Me.cbRefObj, "cbRefObj")
        Me.cbRefObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbRefObj.FormattingEnabled = True
        Me.cbRefObj.Name = "cbRefObj"
        Me.ToolTip1.SetToolTip(Me.cbRefObj, resources.GetString("cbRefObj.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbRefObj, resources.GetString("cbRefObj.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbRefObj, resources.GetString("cbRefObj.ToolTip2"))
        '
        'lblTargetVal
        '
        resources.ApplyResources(Me.lblTargetVal, "lblTargetVal")
        Me.lblTargetVal.Name = "lblTargetVal"
        Me.ToolTip1.SetToolTip(Me.lblTargetVal, resources.GetString("lblTargetVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblTargetVal, resources.GetString("lblTargetVal.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblTargetVal, resources.GetString("lblTargetVal.ToolTip2"))
        '
        'lblSourceVal
        '
        resources.ApplyResources(Me.lblSourceVal, "lblSourceVal")
        Me.lblSourceVal.Name = "lblSourceVal"
        Me.ToolTip1.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip2"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTip1.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip2"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbTargetProp
        '
        resources.ApplyResources(Me.cbTargetProp, "cbTargetProp")
        Me.cbTargetProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetProp.FormattingEnabled = True
        Me.cbTargetProp.Name = "cbTargetProp"
        Me.ToolTip1.SetToolTip(Me.cbTargetProp, resources.GetString("cbTargetProp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetProp, resources.GetString("cbTargetProp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetProp, resources.GetString("cbTargetProp.ToolTip2"))
        '
        'cbTargetObj
        '
        resources.ApplyResources(Me.cbTargetObj, "cbTargetObj")
        Me.cbTargetObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetObj.FormattingEnabled = True
        Me.cbTargetObj.Name = "cbTargetObj"
        Me.ToolTip1.SetToolTip(Me.cbTargetObj, resources.GetString("cbTargetObj.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetObj, resources.GetString("cbTargetObj.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetObj, resources.GetString("cbTargetObj.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        Me.ToolTip1.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label7, resources.GetString("Label7.ToolTip2"))
        '
        'cbSourceProp
        '
        resources.ApplyResources(Me.cbSourceProp, "cbSourceProp")
        Me.cbSourceProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceProp.FormattingEnabled = True
        Me.cbSourceProp.Name = "cbSourceProp"
        Me.ToolTip1.SetToolTip(Me.cbSourceProp, resources.GetString("cbSourceProp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceProp, resources.GetString("cbSourceProp.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceProp, resources.GetString("cbSourceProp.ToolTip2"))
        '
        'cbSourceObj
        '
        resources.ApplyResources(Me.cbSourceObj, "cbSourceObj")
        Me.cbSourceObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceObj.FormattingEnabled = True
        Me.cbSourceObj.Name = "cbSourceObj"
        Me.ToolTip1.SetToolTip(Me.cbSourceObj, resources.GetString("cbSourceObj.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceObj, resources.GetString("cbSourceObj.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceObj, resources.GetString("cbSourceObj.ToolTip2"))
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
        'EditingForm_Adjust
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxLinkedObjects)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.gbControlPanel)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_Adjust"
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxLinkedObjects.ResumeLayout(False)
        Me.GroupBoxLinkedObjects.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents gbControlPanel As System.Windows.Forms.GroupBox
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents GroupBoxLinkedObjects As System.Windows.Forms.GroupBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbSourceProp As System.Windows.Forms.ComboBox
    Public WithEvents cbSourceObj As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbTargetProp As System.Windows.Forms.ComboBox
    Public WithEvents cbTargetObj As System.Windows.Forms.ComboBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents lblTargetVal As System.Windows.Forms.Label
    Public WithEvents lblSourceVal As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents chkSolveGlobal As System.Windows.Forms.CheckBox
    Public WithEvents tbSetPoint As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents lblSPUnits As System.Windows.Forms.Label
    Public WithEvents tbTolerance As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolTipChangeTag As ToolTip
    Friend WithEvents chkUseReferenced As CheckBox
    Public WithEvents lblRefVal As Label
    Public WithEvents Label9 As Label
    Public WithEvents Label10 As Label
    Public WithEvents cbRefProp As ComboBox
    Public WithEvents cbRefObj As ComboBox
End Class
