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
        Me.GroupBoxLinkedObjects = New System.Windows.Forms.GroupBox()
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
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.tbWG = New System.Windows.Forms.TextBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.chkReverse = New System.Windows.Forms.CheckBox()
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
        Me.GroupBoxLinkedObjects.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxLinkedObjects
        '
        resources.ApplyResources(Me.GroupBoxLinkedObjects, "GroupBoxLinkedObjects")
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label14)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbTargetUnits)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbTargetType)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label15)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label12)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbSourceUnits)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.cbSourceType)
        Me.GroupBoxLinkedObjects.Controls.Add(Me.Label13)
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
        Me.ToolTipValues.SetToolTip(Me.GroupBoxLinkedObjects, resources.GetString("GroupBoxLinkedObjects.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxLinkedObjects, resources.GetString("GroupBoxLinkedObjects.ToolTip1"))
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        Me.ToolTipValues.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label14, resources.GetString("Label14.ToolTip1"))
        '
        'cbTargetUnits
        '
        resources.ApplyResources(Me.cbTargetUnits, "cbTargetUnits")
        Me.cbTargetUnits.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetUnits.FormattingEnabled = True
        Me.cbTargetUnits.Name = "cbTargetUnits"
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetUnits, resources.GetString("cbTargetUnits.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetUnits, resources.GetString("cbTargetUnits.ToolTip1"))
        '
        'cbTargetType
        '
        resources.ApplyResources(Me.cbTargetType, "cbTargetType")
        Me.cbTargetType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetType.FormattingEnabled = True
        Me.cbTargetType.Name = "cbTargetType"
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetType, resources.GetString("cbTargetType.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetType, resources.GetString("cbTargetType.ToolTip1"))
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        Me.ToolTipValues.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label15, resources.GetString("Label15.ToolTip1"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTipValues.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip1"))
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
        'cbSourceType
        '
        resources.ApplyResources(Me.cbSourceType, "cbSourceType")
        Me.cbSourceType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSourceType.FormattingEnabled = True
        Me.cbSourceType.Name = "cbSourceType"
        Me.ToolTipChangeTag.SetToolTip(Me.cbSourceType, resources.GetString("cbSourceType.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbSourceType, resources.GetString("cbSourceType.ToolTip1"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTipValues.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip1"))
        '
        'lblTargetVal
        '
        resources.ApplyResources(Me.lblTargetVal, "lblTargetVal")
        Me.lblTargetVal.Name = "lblTargetVal"
        Me.ToolTipValues.SetToolTip(Me.lblTargetVal, resources.GetString("lblTargetVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblTargetVal, resources.GetString("lblTargetVal.ToolTip1"))
        '
        'lblSourceVal
        '
        resources.ApplyResources(Me.lblSourceVal, "lblSourceVal")
        Me.lblSourceVal.Name = "lblSourceVal"
        Me.ToolTipValues.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblSourceVal, resources.GetString("lblSourceVal.ToolTip1"))
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        Me.ToolTipValues.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label6, resources.GetString("Label6.ToolTip1"))
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        '
        'cbTargetProp
        '
        resources.ApplyResources(Me.cbTargetProp, "cbTargetProp")
        Me.cbTargetProp.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetProp.FormattingEnabled = True
        Me.cbTargetProp.Name = "cbTargetProp"
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetProp, resources.GetString("cbTargetProp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetProp, resources.GetString("cbTargetProp.ToolTip1"))
        '
        'cbTargetObj
        '
        resources.ApplyResources(Me.cbTargetObj, "cbTargetObj")
        Me.cbTargetObj.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbTargetObj.FormattingEnabled = True
        Me.cbTargetObj.Name = "cbTargetObj"
        Me.ToolTipChangeTag.SetToolTip(Me.cbTargetObj, resources.GetString("cbTargetObj.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbTargetObj, resources.GetString("cbTargetObj.ToolTip1"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
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
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBox4, resources.GetString("GroupBox4.ToolTip1"))
        '
        'rtbAnnotations
        '
        resources.ApplyResources(Me.rtbAnnotations, "rtbAnnotations")
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.22621}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.ToolTipChangeTag.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip1"))
        '
        'GroupBoxParameters
        '
        resources.ApplyResources(Me.GroupBoxParameters, "GroupBoxParameters")
        Me.GroupBoxParameters.Controls.Add(Me.Button1)
        Me.GroupBoxParameters.Controls.Add(Me.tbWG)
        Me.GroupBoxParameters.Controls.Add(Me.Label18)
        Me.GroupBoxParameters.Controls.Add(Me.chkReverse)
        Me.GroupBoxParameters.Controls.Add(Me.tbOutputMax)
        Me.GroupBoxParameters.Controls.Add(Me.Label17)
        Me.GroupBoxParameters.Controls.Add(Me.tbOutputMin)
        Me.GroupBoxParameters.Controls.Add(Me.Label16)
        Me.GroupBoxParameters.Controls.Add(Me.chkControllerActive)
        Me.GroupBoxParameters.Controls.Add(Me.tbOffset)
        Me.GroupBoxParameters.Controls.Add(Me.Label10)
        Me.GroupBoxParameters.Controls.Add(Me.tbKd)
        Me.GroupBoxParameters.Controls.Add(Me.Label9)
        Me.GroupBoxParameters.Controls.Add(Me.tbKi)
        Me.GroupBoxParameters.Controls.Add(Me.Label8)
        Me.GroupBoxParameters.Controls.Add(Me.tbKp)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.tbSetPoint)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.TabStop = False
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters, resources.GetString("GroupBoxParameters.ToolTip1"))
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.lightning
        Me.Button1.Name = "Button1"
        Me.ToolTipValues.SetToolTip(Me.Button1, resources.GetString("Button1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Button1, resources.GetString("Button1.ToolTip1"))
        Me.Button1.UseVisualStyleBackColor = True
        '
        'tbWG
        '
        resources.ApplyResources(Me.tbWG, "tbWG")
        Me.tbWG.Name = "tbWG"
        Me.ToolTipChangeTag.SetToolTip(Me.tbWG, resources.GetString("tbWG.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbWG, resources.GetString("tbWG.ToolTip1"))
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        Me.ToolTipValues.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label18, resources.GetString("Label18.ToolTip1"))
        '
        'chkReverse
        '
        resources.ApplyResources(Me.chkReverse, "chkReverse")
        Me.chkReverse.Name = "chkReverse"
        Me.ToolTipValues.SetToolTip(Me.chkReverse, resources.GetString("chkReverse.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkReverse, resources.GetString("chkReverse.ToolTip1"))
        Me.chkReverse.UseVisualStyleBackColor = True
        '
        'tbOutputMax
        '
        resources.ApplyResources(Me.tbOutputMax, "tbOutputMax")
        Me.tbOutputMax.Name = "tbOutputMax"
        Me.ToolTipChangeTag.SetToolTip(Me.tbOutputMax, resources.GetString("tbOutputMax.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbOutputMax, resources.GetString("tbOutputMax.ToolTip1"))
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        Me.ToolTipValues.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label17, resources.GetString("Label17.ToolTip1"))
        '
        'tbOutputMin
        '
        resources.ApplyResources(Me.tbOutputMin, "tbOutputMin")
        Me.tbOutputMin.Name = "tbOutputMin"
        Me.ToolTipChangeTag.SetToolTip(Me.tbOutputMin, resources.GetString("tbOutputMin.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbOutputMin, resources.GetString("tbOutputMin.ToolTip1"))
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        Me.ToolTipValues.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label16, resources.GetString("Label16.ToolTip1"))
        '
        'chkControllerActive
        '
        resources.ApplyResources(Me.chkControllerActive, "chkControllerActive")
        Me.chkControllerActive.Name = "chkControllerActive"
        Me.ToolTipValues.SetToolTip(Me.chkControllerActive, resources.GetString("chkControllerActive.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkControllerActive, resources.GetString("chkControllerActive.ToolTip1"))
        Me.chkControllerActive.UseVisualStyleBackColor = True
        '
        'tbOffset
        '
        resources.ApplyResources(Me.tbOffset, "tbOffset")
        Me.tbOffset.Name = "tbOffset"
        Me.ToolTipChangeTag.SetToolTip(Me.tbOffset, resources.GetString("tbOffset.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbOffset, resources.GetString("tbOffset.ToolTip1"))
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        Me.ToolTipValues.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label10, resources.GetString("Label10.ToolTip1"))
        '
        'tbKd
        '
        resources.ApplyResources(Me.tbKd, "tbKd")
        Me.tbKd.Name = "tbKd"
        Me.ToolTipChangeTag.SetToolTip(Me.tbKd, resources.GetString("tbKd.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbKd, resources.GetString("tbKd.ToolTip1"))
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        Me.ToolTipValues.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label9, resources.GetString("Label9.ToolTip1"))
        '
        'tbKi
        '
        resources.ApplyResources(Me.tbKi, "tbKi")
        Me.tbKi.Name = "tbKi"
        Me.ToolTipChangeTag.SetToolTip(Me.tbKi, resources.GetString("tbKi.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbKi, resources.GetString("tbKi.ToolTip1"))
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        Me.ToolTipValues.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label8, resources.GetString("Label8.ToolTip1"))
        '
        'tbKp
        '
        resources.ApplyResources(Me.tbKp, "tbKp")
        Me.tbKp.Name = "tbKp"
        Me.ToolTipChangeTag.SetToolTip(Me.tbKp, resources.GetString("tbKp.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbKp, resources.GetString("tbKp.ToolTip1"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        '
        'tbSetPoint
        '
        resources.ApplyResources(Me.tbSetPoint, "tbSetPoint")
        Me.tbSetPoint.Name = "tbSetPoint"
        Me.ToolTipChangeTag.SetToolTip(Me.tbSetPoint, resources.GetString("tbSetPoint.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.tbSetPoint, resources.GetString("tbSetPoint.ToolTip1"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_PIDController
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxLinkedObjects)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Name = "EditingForm_PIDController"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.GroupBoxLinkedObjects.ResumeLayout(False)
        Me.GroupBoxLinkedObjects.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBoxLinkedObjects As GroupBox
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
    Public WithEvents GroupBoxParameters As GroupBox
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
    Friend WithEvents chkReverse As CheckBox
    Public WithEvents tbWG As TextBox
    Public WithEvents Label18 As Label
    Friend WithEvents Button1 As Button
End Class
