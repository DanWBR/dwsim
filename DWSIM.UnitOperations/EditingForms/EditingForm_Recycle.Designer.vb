<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Recycle

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_Recycle))
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
        Me.GroupBoxParameters1 = New System.Windows.Forms.GroupBox()
        Me.chkLegacyMode = New System.Windows.Forms.CheckBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbMaxIts = New System.Windows.Forms.TextBox()
        Me.chkGlobalBroyden = New System.Windows.Forms.CheckBox()
        Me.TrackBar1 = New System.Windows.Forms.TrackBar()
        Me.cbP = New System.Windows.Forms.ComboBox()
        Me.tbPT = New System.Windows.Forms.TextBox()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.cbT = New System.Windows.Forms.ComboBox()
        Me.tbTT = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbW = New System.Windows.Forms.ComboBox()
        Me.tbWT = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnCreateAndConnectInlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnectOutlet1 = New System.Windows.Forms.Button()
        Me.btnDisconnect1 = New System.Windows.Forms.Button()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.cbOutlet1 = New System.Windows.Forms.ComboBox()
        Me.cbInlet1 = New System.Windows.Forms.ComboBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBoxParameters2 = New System.Windows.Forms.GroupBox()
        Me.tbWE = New System.Windows.Forms.TextBox()
        Me.tbPE = New System.Windows.Forms.TextBox()
        Me.tbTE = New System.Windows.Forms.TextBox()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox5.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxParameters1.SuspendLayout()
        CType(Me.TrackBar1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBoxParameters2.SuspendLayout()
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
        'lblConnectedTo
        '
        resources.ApplyResources(Me.lblConnectedTo, "lblConnectedTo")
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.ToolTip1.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblConnectedTo, resources.GetString("lblConnectedTo.ToolTip2"))
        '
        'lblStatus
        '
        resources.ApplyResources(Me.lblStatus, "lblStatus")
        Me.lblStatus.Name = "lblStatus"
        Me.ToolTip1.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.lblStatus, resources.GetString("lblStatus.ToolTip2"))
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        Me.ToolTip1.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label13, resources.GetString("Label13.ToolTip2"))
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        Me.ToolTip1.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label12, resources.GetString("Label12.ToolTip2"))
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        Me.ToolTip1.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label11, resources.GetString("Label11.ToolTip2"))
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
        'GroupBoxParameters1
        '
        resources.ApplyResources(Me.GroupBoxParameters1, "GroupBoxParameters1")
        Me.GroupBoxParameters1.Controls.Add(Me.chkLegacyMode)
        Me.GroupBoxParameters1.Controls.Add(Me.Label5)
        Me.GroupBoxParameters1.Controls.Add(Me.Label1)
        Me.GroupBoxParameters1.Controls.Add(Me.tbMaxIts)
        Me.GroupBoxParameters1.Controls.Add(Me.chkGlobalBroyden)
        Me.GroupBoxParameters1.Controls.Add(Me.TrackBar1)
        Me.GroupBoxParameters1.Name = "GroupBoxParameters1"
        Me.GroupBoxParameters1.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters1, resources.GetString("GroupBoxParameters1.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters1, resources.GetString("GroupBoxParameters1.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters1, resources.GetString("GroupBoxParameters1.ToolTip2"))
        '
        'chkLegacyMode
        '
        resources.ApplyResources(Me.chkLegacyMode, "chkLegacyMode")
        Me.chkLegacyMode.Name = "chkLegacyMode"
        Me.ToolTip1.SetToolTip(Me.chkLegacyMode, resources.GetString("chkLegacyMode.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkLegacyMode, resources.GetString("chkLegacyMode.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkLegacyMode, resources.GetString("chkLegacyMode.ToolTip2"))
        Me.chkLegacyMode.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        Me.ToolTip1.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label5, resources.GetString("Label5.ToolTip2"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip2"))
        '
        'tbMaxIts
        '
        resources.ApplyResources(Me.tbMaxIts, "tbMaxIts")
        Me.tbMaxIts.Name = "tbMaxIts"
        Me.ToolTipValues.SetToolTip(Me.tbMaxIts, resources.GetString("tbMaxIts.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbMaxIts, resources.GetString("tbMaxIts.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbMaxIts, resources.GetString("tbMaxIts.ToolTip2"))
        '
        'chkGlobalBroyden
        '
        resources.ApplyResources(Me.chkGlobalBroyden, "chkGlobalBroyden")
        Me.chkGlobalBroyden.Name = "chkGlobalBroyden"
        Me.ToolTip1.SetToolTip(Me.chkGlobalBroyden, resources.GetString("chkGlobalBroyden.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.chkGlobalBroyden, resources.GetString("chkGlobalBroyden.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.chkGlobalBroyden, resources.GetString("chkGlobalBroyden.ToolTip2"))
        Me.chkGlobalBroyden.UseVisualStyleBackColor = True
        '
        'TrackBar1
        '
        resources.ApplyResources(Me.TrackBar1, "TrackBar1")
        Me.TrackBar1.Minimum = 1
        Me.TrackBar1.Name = "TrackBar1"
        Me.ToolTip1.SetToolTip(Me.TrackBar1, resources.GetString("TrackBar1.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.TrackBar1, resources.GetString("TrackBar1.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.TrackBar1, resources.GetString("TrackBar1.ToolTip2"))
        Me.TrackBar1.Value = 10
        '
        'cbP
        '
        resources.ApplyResources(Me.cbP, "cbP")
        Me.cbP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbP.FormattingEnabled = True
        Me.cbP.Items.AddRange(New Object() {resources.GetString("cbP.Items"), resources.GetString("cbP.Items1"), resources.GetString("cbP.Items2")})
        Me.cbP.Name = "cbP"
        Me.ToolTip1.SetToolTip(Me.cbP, resources.GetString("cbP.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbP, resources.GetString("cbP.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbP, resources.GetString("cbP.ToolTip2"))
        '
        'tbPT
        '
        resources.ApplyResources(Me.tbPT, "tbPT")
        Me.tbPT.Name = "tbPT"
        Me.ToolTipValues.SetToolTip(Me.tbPT, resources.GetString("tbPT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPT, resources.GetString("tbPT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPT, resources.GetString("tbPT.ToolTip2"))
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        Me.ToolTip1.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label4, resources.GetString("Label4.ToolTip2"))
        '
        'cbT
        '
        resources.ApplyResources(Me.cbT, "cbT")
        Me.cbT.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbT.FormattingEnabled = True
        Me.cbT.Items.AddRange(New Object() {resources.GetString("cbT.Items"), resources.GetString("cbT.Items1"), resources.GetString("cbT.Items2")})
        Me.cbT.Name = "cbT"
        Me.ToolTip1.SetToolTip(Me.cbT, resources.GetString("cbT.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbT, resources.GetString("cbT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbT, resources.GetString("cbT.ToolTip2"))
        '
        'tbTT
        '
        resources.ApplyResources(Me.tbTT, "tbTT")
        Me.tbTT.Name = "tbTT"
        Me.ToolTipValues.SetToolTip(Me.tbTT, resources.GetString("tbTT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbTT, resources.GetString("tbTT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbTT, resources.GetString("tbTT.ToolTip2"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip2"))
        '
        'cbW
        '
        resources.ApplyResources(Me.cbW, "cbW")
        Me.cbW.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbW.FormattingEnabled = True
        Me.cbW.Items.AddRange(New Object() {resources.GetString("cbW.Items"), resources.GetString("cbW.Items1"), resources.GetString("cbW.Items2")})
        Me.cbW.Name = "cbW"
        Me.ToolTip1.SetToolTip(Me.cbW, resources.GetString("cbW.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.cbW, resources.GetString("cbW.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.cbW, resources.GetString("cbW.ToolTip2"))
        '
        'tbWT
        '
        resources.ApplyResources(Me.tbWT, "tbWT")
        Me.tbWT.Name = "tbWT"
        Me.ToolTipValues.SetToolTip(Me.tbWT, resources.GetString("tbWT.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbWT, resources.GetString("tbWT.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbWT, resources.GetString("tbWT.ToolTip2"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip2"))
        '
        'GroupBoxConnections
        '
        resources.ApplyResources(Me.GroupBoxConnections, "GroupBoxConnections")
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnect1)
        Me.GroupBoxConnections.Controls.Add(Me.Label7)
        Me.GroupBoxConnections.Controls.Add(Me.cbOutlet1)
        Me.GroupBoxConnections.Controls.Add(Me.cbInlet1)
        Me.GroupBoxConnections.Controls.Add(Me.Label19)
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
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        Me.ToolTip1.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip"))
        Me.ToolTipChangeTag.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip1"))
        Me.ToolTipValues.SetToolTip(Me.Label19, resources.GetString("Label19.ToolTip2"))
        '
        'GroupBoxParameters2
        '
        resources.ApplyResources(Me.GroupBoxParameters2, "GroupBoxParameters2")
        Me.GroupBoxParameters2.Controls.Add(Me.tbWE)
        Me.GroupBoxParameters2.Controls.Add(Me.tbPE)
        Me.GroupBoxParameters2.Controls.Add(Me.tbTE)
        Me.GroupBoxParameters2.Controls.Add(Me.Label3)
        Me.GroupBoxParameters2.Controls.Add(Me.cbP)
        Me.GroupBoxParameters2.Controls.Add(Me.tbWT)
        Me.GroupBoxParameters2.Controls.Add(Me.tbPT)
        Me.GroupBoxParameters2.Controls.Add(Me.cbW)
        Me.GroupBoxParameters2.Controls.Add(Me.Label4)
        Me.GroupBoxParameters2.Controls.Add(Me.Label2)
        Me.GroupBoxParameters2.Controls.Add(Me.cbT)
        Me.GroupBoxParameters2.Controls.Add(Me.tbTT)
        Me.GroupBoxParameters2.Name = "GroupBoxParameters2"
        Me.GroupBoxParameters2.TabStop = False
        Me.ToolTipChangeTag.SetToolTip(Me.GroupBoxParameters2, resources.GetString("GroupBoxParameters2.ToolTip"))
        Me.ToolTipValues.SetToolTip(Me.GroupBoxParameters2, resources.GetString("GroupBoxParameters2.ToolTip1"))
        Me.ToolTip1.SetToolTip(Me.GroupBoxParameters2, resources.GetString("GroupBoxParameters2.ToolTip2"))
        '
        'tbWE
        '
        resources.ApplyResources(Me.tbWE, "tbWE")
        Me.tbWE.Name = "tbWE"
        Me.tbWE.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbWE, resources.GetString("tbWE.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbWE, resources.GetString("tbWE.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbWE, resources.GetString("tbWE.ToolTip2"))
        '
        'tbPE
        '
        resources.ApplyResources(Me.tbPE, "tbPE")
        Me.tbPE.Name = "tbPE"
        Me.tbPE.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbPE, resources.GetString("tbPE.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbPE, resources.GetString("tbPE.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbPE, resources.GetString("tbPE.ToolTip2"))
        '
        'tbTE
        '
        resources.ApplyResources(Me.tbTE, "tbTE")
        Me.tbTE.Name = "tbTE"
        Me.tbTE.ReadOnly = True
        Me.ToolTipValues.SetToolTip(Me.tbTE, resources.GetString("tbTE.ToolTip"))
        Me.ToolTip1.SetToolTip(Me.tbTE, resources.GetString("tbTE.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me.tbTE, resources.GetString("tbTE.ToolTip2"))
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_Recycle
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBoxParameters1)
        Me.Controls.Add(Me.GroupBoxParameters2)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Controls.Add(Me.GroupBox4)
        Me.Name = "EditingForm_Recycle"
        Me.ToolTipValues.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip1"))
        Me.ToolTipChangeTag.SetToolTip(Me, resources.GetString("$this.ToolTip2"))
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxParameters1.ResumeLayout(False)
        Me.GroupBoxParameters1.PerformLayout()
        CType(Me.TrackBar1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBoxParameters2.ResumeLayout(False)
        Me.GroupBoxParameters2.PerformLayout()
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
    Public WithEvents GroupBoxParameters1 As System.Windows.Forms.GroupBox
    Public WithEvents cbW As System.Windows.Forms.ComboBox
    Public WithEvents tbWT As System.Windows.Forms.TextBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents cbT As System.Windows.Forms.ComboBox
    Public WithEvents tbTT As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents GroupBoxConnections As System.Windows.Forms.GroupBox
    Public WithEvents btnDisconnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnDisconnect1 As System.Windows.Forms.Button
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents cbOutlet1 As System.Windows.Forms.ComboBox
    Public WithEvents cbInlet1 As System.Windows.Forms.ComboBox
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents lblTag As System.Windows.Forms.TextBox
    Public WithEvents cbP As System.Windows.Forms.ComboBox
    Public WithEvents tbPT As System.Windows.Forms.TextBox
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents btnCreateAndConnectOutlet1 As System.Windows.Forms.Button
    Public WithEvents btnCreateAndConnectInlet1 As System.Windows.Forms.Button
    Public WithEvents chkGlobalBroyden As System.Windows.Forms.CheckBox
    Public WithEvents GroupBoxParameters2 As System.Windows.Forms.GroupBox
    Public WithEvents tbWE As System.Windows.Forms.TextBox
    Public WithEvents tbPE As System.Windows.Forms.TextBox
    Public WithEvents tbTE As System.Windows.Forms.TextBox
    Friend WithEvents ToolTipChangeTag As ToolTip
    Public WithEvents Label1 As Label
    Public WithEvents tbMaxIts As TextBox
    Friend WithEvents TrackBar1 As TrackBar
    Public WithEvents Label5 As Label
    Friend WithEvents chkLegacyMode As CheckBox
End Class
