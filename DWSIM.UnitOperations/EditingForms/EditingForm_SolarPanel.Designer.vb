<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_SolarPanel

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_SolarPanel))
        Me.GroupBoxConnections = New System.Windows.Forms.GroupBox()
        Me.btnCreateAndConnectEnergy = New System.Windows.Forms.Button()
        Me.btnDisconnectEnergy = New System.Windows.Forms.Button()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.cbEnergy = New System.Windows.Forms.ComboBox()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.lblTag = New System.Windows.Forms.TextBox()
        Me.chkActive = New System.Windows.Forms.CheckBox()
        Me.lblConnectedTo = New System.Windows.Forms.Label()
        Me.lblStatus = New System.Windows.Forms.Label()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.tbNumberOfPanels = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.tbPanelEfficiency = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.lblAreaUnits = New System.Windows.Forms.Label()
        Me.tbPanelArea = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.chkUseGlobalIrr = New System.Windows.Forms.CheckBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbSolarIrr = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.GroupBoxResults = New System.Windows.Forms.GroupBox()
        Me.lblPowerUnits = New System.Windows.Forms.Label()
        Me.tbGenPower = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxResults.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBoxConnections
        '
        Me.GroupBoxConnections.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.Label14)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy)
        Me.GroupBoxConnections.Location = New System.Drawing.Point(7, 107)
        Me.GroupBoxConnections.Name = "GroupBoxConnections"
        Me.GroupBoxConnections.Size = New System.Drawing.Size(354, 65)
        Me.GroupBoxConnections.TabIndex = 16
        Me.GroupBoxConnections.TabStop = False
        Me.GroupBoxConnections.Text = "Connections"
        '
        'btnCreateAndConnectEnergy
        '
        Me.btnCreateAndConnectEnergy.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCreateAndConnectEnergy.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.btnCreateAndConnectEnergy.Image = CType(resources.GetObject("btnCreateAndConnectEnergy.Image"), System.Drawing.Image)
        Me.btnCreateAndConnectEnergy.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnCreateAndConnectEnergy.Location = New System.Drawing.Point(298, 26)
        Me.btnCreateAndConnectEnergy.Name = "btnCreateAndConnectEnergy"
        Me.btnCreateAndConnectEnergy.Size = New System.Drawing.Size(21, 21)
        Me.btnCreateAndConnectEnergy.TabIndex = 42
        Me.btnCreateAndConnectEnergy.UseVisualStyleBackColor = True
        '
        'btnDisconnectEnergy
        '
        Me.btnDisconnectEnergy.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnDisconnectEnergy.Image = CType(resources.GetObject("btnDisconnectEnergy.Image"), System.Drawing.Image)
        Me.btnDisconnectEnergy.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnDisconnectEnergy.Location = New System.Drawing.Point(325, 26)
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.btnDisconnectEnergy.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectEnergy.TabIndex = 23
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label14.Location = New System.Drawing.Point(9, 29)
        Me.Label14.Name = "Label14"
        Me.Label14.Size = New System.Drawing.Size(76, 13)
        Me.Label14.TabIndex = 22
        Me.Label14.Text = "Energy Stream"
        '
        'cbEnergy
        '
        Me.cbEnergy.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbEnergy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbEnergy.FormattingEnabled = True
        Me.cbEnergy.Location = New System.Drawing.Point(157, 26)
        Me.cbEnergy.Name = "cbEnergy"
        Me.cbEnergy.Size = New System.Drawing.Size(135, 21)
        Me.cbEnergy.TabIndex = 21
        '
        'GroupBox5
        '
        Me.GroupBox5.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox5.Controls.Add(Me.lblTag)
        Me.GroupBox5.Controls.Add(Me.chkActive)
        Me.GroupBox5.Controls.Add(Me.lblConnectedTo)
        Me.GroupBox5.Controls.Add(Me.lblStatus)
        Me.GroupBox5.Controls.Add(Me.Label13)
        Me.GroupBox5.Controls.Add(Me.Label12)
        Me.GroupBox5.Controls.Add(Me.Label11)
        Me.GroupBox5.Location = New System.Drawing.Point(7, 7)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(354, 98)
        Me.GroupBox5.TabIndex = 15
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "General Info"
        '
        'lblTag
        '
        Me.lblTag.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblTag.Location = New System.Drawing.Point(133, 19)
        Me.lblTag.Name = "lblTag"
        Me.lblTag.Size = New System.Drawing.Size(213, 20)
        Me.lblTag.TabIndex = 24
        '
        'chkActive
        '
        Me.chkActive.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.chkActive.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkActive.Location = New System.Drawing.Point(325, 43)
        Me.chkActive.Name = "chkActive"
        Me.chkActive.Size = New System.Drawing.Size(21, 21)
        Me.chkActive.TabIndex = 21
        Me.chkActive.UseVisualStyleBackColor = True
        '
        'lblConnectedTo
        '
        Me.lblConnectedTo.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblConnectedTo.AutoSize = True
        Me.lblConnectedTo.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblConnectedTo.Location = New System.Drawing.Point(132, 72)
        Me.lblConnectedTo.Name = "lblConnectedTo"
        Me.lblConnectedTo.Size = New System.Drawing.Size(38, 13)
        Me.lblConnectedTo.TabIndex = 20
        Me.lblConnectedTo.Text = "Objeto"
        '
        'lblStatus
        '
        Me.lblStatus.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblStatus.AutoSize = True
        Me.lblStatus.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.lblStatus.Location = New System.Drawing.Point(132, 47)
        Me.lblStatus.Name = "lblStatus"
        Me.lblStatus.Size = New System.Drawing.Size(38, 13)
        Me.lblStatus.TabIndex = 19
        Me.lblStatus.Text = "Objeto"
        '
        'Label13
        '
        Me.Label13.AutoSize = True
        Me.Label13.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label13.Location = New System.Drawing.Point(9, 72)
        Me.Label13.Name = "Label13"
        Me.Label13.Size = New System.Drawing.Size(51, 13)
        Me.Label13.TabIndex = 17
        Me.Label13.Text = "Linked to"
        '
        'Label12
        '
        Me.Label12.AutoSize = True
        Me.Label12.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label12.Location = New System.Drawing.Point(9, 47)
        Me.Label12.Name = "Label12"
        Me.Label12.Size = New System.Drawing.Size(37, 13)
        Me.Label12.TabIndex = 16
        Me.Label12.Text = "Status"
        '
        'Label11
        '
        Me.Label11.AutoSize = True
        Me.Label11.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label11.Location = New System.Drawing.Point(9, 22)
        Me.Label11.Name = "Label11"
        Me.Label11.Size = New System.Drawing.Size(38, 13)
        Me.Label11.TabIndex = 14
        Me.Label11.Text = "Object"
        '
        'GroupBoxParameters
        '
        Me.GroupBoxParameters.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxParameters.Controls.Add(Me.tbNumberOfPanels)
        Me.GroupBoxParameters.Controls.Add(Me.Label6)
        Me.GroupBoxParameters.Controls.Add(Me.tbPanelEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.lblAreaUnits)
        Me.GroupBoxParameters.Controls.Add(Me.tbPanelArea)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.chkUseGlobalIrr)
        Me.GroupBoxParameters.Controls.Add(Me.Label1)
        Me.GroupBoxParameters.Controls.Add(Me.tbSolarIrr)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Location = New System.Drawing.Point(7, 178)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.Size = New System.Drawing.Size(354, 180)
        Me.GroupBoxParameters.TabIndex = 17
        Me.GroupBoxParameters.TabStop = False
        Me.GroupBoxParameters.Text = "Calculation Parameters"
        '
        'tbNumberOfPanels
        '
        Me.tbNumberOfPanels.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbNumberOfPanels.Location = New System.Drawing.Point(201, 141)
        Me.tbNumberOfPanels.Name = "tbNumberOfPanels"
        Me.tbNumberOfPanels.Size = New System.Drawing.Size(91, 20)
        Me.tbNumberOfPanels.TabIndex = 36
        Me.tbNumberOfPanels.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(8, 144)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(91, 13)
        Me.Label6.TabIndex = 35
        Me.Label6.Text = "Number of Panels"
        '
        'tbPanelEfficiency
        '
        Me.tbPanelEfficiency.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbPanelEfficiency.Location = New System.Drawing.Point(201, 115)
        Me.tbPanelEfficiency.Name = "tbPanelEfficiency"
        Me.tbPanelEfficiency.Size = New System.Drawing.Size(91, 20)
        Me.tbPanelEfficiency.TabIndex = 34
        Me.tbPanelEfficiency.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(8, 118)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(127, 13)
        Me.Label5.TabIndex = 33
        Me.Label5.Text = "Panel Efficiency (0-100%)"
        '
        'lblAreaUnits
        '
        Me.lblAreaUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblAreaUnits.AutoSize = True
        Me.lblAreaUnits.Location = New System.Drawing.Point(298, 92)
        Me.lblAreaUnits.Name = "lblAreaUnits"
        Me.lblAreaUnits.Size = New System.Drawing.Size(43, 13)
        Me.lblAreaUnits.TabIndex = 32
        Me.lblAreaUnits.Text = "kW/m2"
        '
        'tbPanelArea
        '
        Me.tbPanelArea.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbPanelArea.Location = New System.Drawing.Point(201, 89)
        Me.tbPanelArea.Name = "tbPanelArea"
        Me.tbPanelArea.Size = New System.Drawing.Size(91, 20)
        Me.tbPanelArea.TabIndex = 31
        Me.tbPanelArea.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(8, 92)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(59, 13)
        Me.Label2.TabIndex = 30
        Me.Label2.Text = "Panel Area"
        '
        'chkUseGlobalIrr
        '
        Me.chkUseGlobalIrr.AutoSize = True
        Me.chkUseGlobalIrr.Location = New System.Drawing.Point(11, 26)
        Me.chkUseGlobalIrr.Name = "chkUseGlobalIrr"
        Me.chkUseGlobalIrr.Size = New System.Drawing.Size(174, 17)
        Me.chkUseGlobalIrr.TabIndex = 29
        Me.chkUseGlobalIrr.Text = "Use Global Weather Conditions"
        Me.chkUseGlobalIrr.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(298, 53)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(43, 13)
        Me.Label1.TabIndex = 28
        Me.Label1.Text = "kW/m2"
        '
        'tbSolarIrr
        '
        Me.tbSolarIrr.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbSolarIrr.Location = New System.Drawing.Point(201, 49)
        Me.tbSolarIrr.Name = "tbSolarIrr"
        Me.tbSolarIrr.Size = New System.Drawing.Size(91, 20)
        Me.tbSolarIrr.TabIndex = 27
        Me.tbSolarIrr.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(8, 52)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(80, 13)
        Me.Label3.TabIndex = 26
        Me.Label3.Text = "Solar Irradiation"
        '
        'GroupBoxResults
        '
        Me.GroupBoxResults.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxResults.Controls.Add(Me.lblPowerUnits)
        Me.GroupBoxResults.Controls.Add(Me.tbGenPower)
        Me.GroupBoxResults.Controls.Add(Me.Label15)
        Me.GroupBoxResults.Location = New System.Drawing.Point(7, 362)
        Me.GroupBoxResults.Name = "GroupBoxResults"
        Me.GroupBoxResults.Size = New System.Drawing.Size(354, 69)
        Me.GroupBoxResults.TabIndex = 18
        Me.GroupBoxResults.TabStop = False
        Me.GroupBoxResults.Text = "Results"
        '
        'lblPowerUnits
        '
        Me.lblPowerUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblPowerUnits.AutoSize = True
        Me.lblPowerUnits.Location = New System.Drawing.Point(298, 34)
        Me.lblPowerUnits.Name = "lblPowerUnits"
        Me.lblPowerUnits.Size = New System.Drawing.Size(24, 13)
        Me.lblPowerUnits.TabIndex = 28
        Me.lblPowerUnits.Text = "kW"
        '
        'tbGenPower
        '
        Me.tbGenPower.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbGenPower.Location = New System.Drawing.Point(201, 30)
        Me.tbGenPower.Name = "tbGenPower"
        Me.tbGenPower.ReadOnly = True
        Me.tbGenPower.Size = New System.Drawing.Size(91, 20)
        Me.tbGenPower.TabIndex = 27
        Me.tbGenPower.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label15
        '
        Me.Label15.AutoSize = True
        Me.Label15.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label15.Location = New System.Drawing.Point(8, 33)
        Me.Label15.Name = "Label15"
        Me.Label15.Size = New System.Drawing.Size(90, 13)
        Me.Label15.TabIndex = 26
        Me.Label15.Text = "Generated Power"
        '
        'GroupBox4
        '
        Me.GroupBox4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Location = New System.Drawing.Point(7, 437)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(354, 223)
        Me.GroupBox4.TabIndex = 19
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Notes"
        '
        'rtbAnnotations
        '
        Me.rtbAnnotations.Dock = System.Windows.Forms.DockStyle.Fill
        Me.rtbAnnotations.Location = New System.Drawing.Point(3, 16)
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil Microsoft " &
    "Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.19041}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\pard\f0\fs17\" &
    "par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.rtbAnnotations.Size = New System.Drawing.Size(348, 204)
        Me.rtbAnnotations.TabIndex = 0
        '
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_SolarPanel
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.AutoScroll = True
        Me.ClientSize = New System.Drawing.Size(373, 672)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBoxResults)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "EditingForm_SolarPanel"
        Me.Text = "EditingForm_SolarPanel"
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxResults.ResumeLayout(False)
        Me.GroupBoxResults.PerformLayout()
        Me.GroupBox4.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBoxConnections As GroupBox
    Public WithEvents btnCreateAndConnectEnergy As Button
    Public WithEvents btnDisconnectEnergy As Button
    Public WithEvents Label14 As Label
    Public WithEvents cbEnergy As ComboBox
    Public WithEvents GroupBox5 As GroupBox
    Public WithEvents lblTag As TextBox
    Public WithEvents chkActive As CheckBox
    Public WithEvents lblConnectedTo As Label
    Public WithEvents lblStatus As Label
    Public WithEvents Label13 As Label
    Public WithEvents Label12 As Label
    Public WithEvents Label11 As Label
    Public WithEvents GroupBoxParameters As GroupBox
    Public WithEvents tbNumberOfPanels As TextBox
    Public WithEvents Label6 As Label
    Public WithEvents tbPanelEfficiency As TextBox
    Public WithEvents Label5 As Label
    Friend WithEvents lblAreaUnits As Label
    Public WithEvents tbPanelArea As TextBox
    Public WithEvents Label2 As Label
    Friend WithEvents chkUseGlobalIrr As CheckBox
    Friend WithEvents Label1 As Label
    Public WithEvents tbSolarIrr As TextBox
    Public WithEvents Label3 As Label
    Public WithEvents GroupBoxResults As GroupBox
    Friend WithEvents lblPowerUnits As Label
    Public WithEvents tbGenPower As TextBox
    Public WithEvents Label15 As Label
    Public WithEvents GroupBox4 As GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Friend WithEvents ToolTipChangeTag As ToolTip
End Class
