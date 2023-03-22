<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Spec

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Spec))
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.lblResult = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbExpression = New System.Windows.Forms.TextBox()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
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
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
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
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.19041}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        '
        'GroupBoxParameters
        '
        resources.ApplyResources(Me.GroupBoxParameters, "GroupBoxParameters")
        Me.GroupBoxParameters.Controls.Add(Me.lblResult)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Controls.Add(Me.tbExpression)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        '
        'lblResult
        '
        resources.ApplyResources(Me.lblResult, "lblResult")
        Me.lblResult.Name = "lblResult"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'tbExpression
        '
        resources.ApplyResources(Me.tbExpression, "tbExpression")
        Me.tbExpression.Name = "tbExpression"
        '
        'GroupBoxConnections
        '
        resources.ApplyResources(Me.GroupBoxConnections, "GroupBoxConnections")
        Me.GroupBoxConnections.Controls.Add(Me.lblTargetVal)
        Me.GroupBoxConnections.Controls.Add(Me.lblSourceVal)
        Me.GroupBoxConnections.Controls.Add(Me.Label6)
        Me.GroupBoxConnections.Controls.Add(Me.Label5)
        Me.GroupBoxConnections.Controls.Add(Me.Label2)
        Me.GroupBoxConnections.Controls.Add(Me.cbTargetProp)
        Me.GroupBoxConnections.Controls.Add(Me.cbTargetObj)
        Me.GroupBoxConnections.Controls.Add(Me.Label4)
        Me.GroupBoxConnections.Controls.Add(Me.Label7)
        Me.GroupBoxConnections.Controls.Add(Me.cbSourceProp)
        Me.GroupBoxConnections.Controls.Add(Me.cbSourceObj)
        Me.GroupBoxConnections.Controls.Add(Me.Label19)
        Me.GroupBoxConnections.Name = "GroupBoxConnections"
        Me.GroupBoxConnections.TabStop = False
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_Spec
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_Spec"
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents GroupBoxParameters As System.Windows.Forms.GroupBox
    Public WithEvents tbExpression As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbSourceProp As System.Windows.Forms.ComboBox
    Public WithEvents cbSourceObj As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbTargetProp As System.Windows.Forms.ComboBox
    Public WithEvents cbTargetObj As System.Windows.Forms.ComboBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents lblTargetVal As System.Windows.Forms.Label
    Public WithEvents lblSourceVal As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents lblResult As System.Windows.Forms.Label
    Friend WithEvents ToolTipChangeTag As ToolTip
End Class
