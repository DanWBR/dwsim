<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Emixer
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Emixer))
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet6 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet5 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet4 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet3 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet2 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect6 = New System.Windows.Forms.Button()
        Me.btnDisconnect5 = New System.Windows.Forms.Button()
        Me.btnDisconnect4 = New System.Windows.Forms.Button()
        Me.btnDisconnect3 = New System.Windows.Forms.Button()
        Me.btnDisconnect2 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet6 = New System.Windows.Forms.ComboBox()
        Me.cbInlet5 = New System.Windows.Forms.ComboBox()
        Me.cbInlet4 = New System.Windows.Forms.ComboBox()
        Me.cbInlet3 = New System.Windows.Forms.ComboBox()
        Me.cbInlet2 = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.gbGHG = New System.Windows.Forms.GroupBox()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxConnections
        '
        resources.ApplyResources(Me.GroupBoxConnections, "GroupBoxConnections")
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet6)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet5)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet4)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet3)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet2)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect6)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect5)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect4)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect3)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect2)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect1)
        Me.GroupBoxConnections.Controls.Add(Me.Label7)
        Me.GroupBoxConnections.Controls.Add(Me.Label6)
        Me.GroupBoxConnections.Controls.Add(Me.Label5)
        Me.GroupBoxConnections.Controls.Add(Me.Label4)
        Me.GroupBoxConnections.Controls.Add(Me.Label3)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet6)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet5)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet4)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet3)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet2)
        Me.GroupBoxConnections.Controls.Add(Me.Label2)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.Label1)
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
        'btnCreateAndConnectInlet6
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet6, "btnCreateAndConnectInlet6")
        Me.btnCreateAndConnectInlet6.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet6.Name = "btnCreateAndConnectInlet6"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet6, resources.GetString("btnCreateAndConnectInlet6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet6, resources.GetString("btnCreateAndConnectInlet6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet6, resources.GetString("btnCreateAndConnectInlet6.ToolTip2"))
        Me.btnCreateAndConnectInlet6.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet5
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet5, "btnCreateAndConnectInlet5")
        Me.btnCreateAndConnectInlet5.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet5.Name = "btnCreateAndConnectInlet5"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet5, resources.GetString("btnCreateAndConnectInlet5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet5, resources.GetString("btnCreateAndConnectInlet5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet5, resources.GetString("btnCreateAndConnectInlet5.ToolTip2"))
        Me.btnCreateAndConnectInlet5.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet4
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet4, "btnCreateAndConnectInlet4")
        Me.btnCreateAndConnectInlet4.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet4.Name = "btnCreateAndConnectInlet4"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet4, resources.GetString("btnCreateAndConnectInlet4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet4, resources.GetString("btnCreateAndConnectInlet4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet4, resources.GetString("btnCreateAndConnectInlet4.ToolTip2"))
        Me.btnCreateAndConnectInlet4.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet3
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet3, "btnCreateAndConnectInlet3")
        Me.btnCreateAndConnectInlet3.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet3.Name = "btnCreateAndConnectInlet3"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet3, resources.GetString("btnCreateAndConnectInlet3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet3, resources.GetString("btnCreateAndConnectInlet3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet3, resources.GetString("btnCreateAndConnectInlet3.ToolTip2"))
        Me.btnCreateAndConnectInlet3.UseVisualStyleBackColor = True
        '
        'btnCreateAndConnectInlet2
        '
        resources.ApplyResources(Me.btnCreateAndConnectInlet2, "btnCreateAndConnectInlet2")
        Me.btnCreateAndConnectInlet2.BackgroundImage = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_lightning
        Me.btnCreateAndConnectInlet2.Name = "btnCreateAndConnectInlet2"
        Me.ToolTip1.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnCreateAndConnectInlet2, resources.GetString("btnCreateAndConnectInlet2.ToolTip2"))
        Me.btnCreateAndConnectInlet2.UseVisualStyleBackColor = True
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
        'btnDisconnect6
        '
        resources.ApplyResources(Me.btnDisconnect6, "btnDisconnect6")
        Me.btnDisconnect6.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect6.Name = "btnDisconnect6"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect6, resources.GetString("btnDisconnect6.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect6, resources.GetString("btnDisconnect6.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect6, resources.GetString("btnDisconnect6.ToolTip2"))
        Me.btnDisconnect6.UseVisualStyleBackColor = True
        '
        'btnDisconnect5
        '
        resources.ApplyResources(Me.btnDisconnect5, "btnDisconnect5")
        Me.btnDisconnect5.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect5.Name = "btnDisconnect5"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect5, resources.GetString("btnDisconnect5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect5, resources.GetString("btnDisconnect5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect5, resources.GetString("btnDisconnect5.ToolTip2"))
        Me.btnDisconnect5.UseVisualStyleBackColor = True
        '
        'btnDisconnect4
        '
        resources.ApplyResources(Me.btnDisconnect4, "btnDisconnect4")
        Me.btnDisconnect4.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect4.Name = "btnDisconnect4"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect4, resources.GetString("btnDisconnect4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect4, resources.GetString("btnDisconnect4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect4, resources.GetString("btnDisconnect4.ToolTip2"))
        Me.btnDisconnect4.UseVisualStyleBackColor = True
        '
        'btnDisconnect3
        '
        resources.ApplyResources(Me.btnDisconnect3, "btnDisconnect3")
        Me.btnDisconnect3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect3.Name = "btnDisconnect3"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect3, resources.GetString("btnDisconnect3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect3, resources.GetString("btnDisconnect3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect3, resources.GetString("btnDisconnect3.ToolTip2"))
        Me.btnDisconnect3.UseVisualStyleBackColor = True
        '
        'btnDisconnect2
        '
        resources.ApplyResources(Me.btnDisconnect2, "btnDisconnect2")
        Me.btnDisconnect2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.disconnect
        Me.btnDisconnect2.Name = "btnDisconnect2"
        Me.ToolTip1.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.btnDisconnect2, resources.GetString("btnDisconnect2.ToolTip2"))
        Me.btnDisconnect2.UseVisualStyleBackColor = True
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
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
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
        'cbInlet6
        '
        resources.ApplyResources(Me.cbInlet6, "cbInlet6")
        Me.cbInlet6.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet6.FormattingEnabled = True
        Me.cbInlet6.Name = "cbInlet6"
        Me.ToolTip1.SetToolTip(Me.cbInlet6, resources.GetString("cbInlet6.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet6, resources.GetString("cbInlet6.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet6, resources.GetString("cbInlet6.ToolTip2"))
        '
        'cbInlet5
        '
        resources.ApplyResources(Me.cbInlet5, "cbInlet5")
        Me.cbInlet5.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet5.FormattingEnabled = True
        Me.cbInlet5.Name = "cbInlet5"
        Me.ToolTip1.SetToolTip(Me.cbInlet5, resources.GetString("cbInlet5.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet5, resources.GetString("cbInlet5.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet5, resources.GetString("cbInlet5.ToolTip2"))
        '
        'cbInlet4
        '
        resources.ApplyResources(Me.cbInlet4, "cbInlet4")
        Me.cbInlet4.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet4.FormattingEnabled = True
        Me.cbInlet4.Name = "cbInlet4"
        Me.ToolTip1.SetToolTip(Me.cbInlet4, resources.GetString("cbInlet4.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet4, resources.GetString("cbInlet4.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet4, resources.GetString("cbInlet4.ToolTip2"))
        '
        'cbInlet3
        '
        resources.ApplyResources(Me.cbInlet3, "cbInlet3")
        Me.cbInlet3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet3.FormattingEnabled = True
        Me.cbInlet3.Name = "cbInlet3"
        Me.ToolTip1.SetToolTip(Me.cbInlet3, resources.GetString("cbInlet3.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet3, resources.GetString("cbInlet3.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet3, resources.GetString("cbInlet3.ToolTip2"))
        '
        'cbInlet2
        '
        resources.ApplyResources(Me.cbInlet2, "cbInlet2")
        Me.cbInlet2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbInlet2.FormattingEnabled = True
        Me.cbInlet2.Name = "cbInlet2"
        Me.ToolTip1.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbInlet2, resources.GetString("cbInlet2.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
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
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
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
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.22621}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.ToolTipValues.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.rtbAnnotations, resources.GetString("rtbAnnotations.ToolTip2"))
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
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTip1.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip2"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTip1.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip2"))
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        Me.ToolTip1.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip2"))
        '
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.ToolTip1.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip2"))
        '
        'lblTag
        '
        resources.ApplyResources(Me.lblTag, "lblTag")
        Me.lblTag.Name = "lblTag"
        Me.ToolTipValues.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblTag, resources.GetString("lblTag.ToolTip2"))
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'gbGHG
        '
        resources.ApplyResources(Me.gbGHG, "gbGHG")
        Me.gbGHG.Name = "gbGHG"
        Me.gbGHG.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.gbGHG, resources.GetString("gbGHG.ToolTip2"))
        '
        'EditingForm_Emixer
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.gbGHG)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Name = "EditingForm_Emixer"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet6 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet5 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet4 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet3 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet2 As System.Windows.Forms.ComboBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect6 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect5 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect4 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect3 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect2 As System.Windows.Forms.Button
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet6 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet5 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet4 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet3 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet2 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents Label11 As System.Windows.Forms.Label
    Public WithEvents Label12 As System.Windows.Forms.Label
    Public WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents lblStatus As System.Windows.Forms.Label
    Public WithEvents lblConnectedTo As System.Windows.Forms.Label
    Public WithEvents chkActive As System.Windows.Forms.CheckBox
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents ToolTipChangeTag As Windows.Forms.ToolTip
    Public WithEvents gbGHG As GroupBox
End Class
