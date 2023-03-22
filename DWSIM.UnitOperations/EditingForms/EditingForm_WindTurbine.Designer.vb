<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_WindTurbine


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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_WindTurbine))
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.rtbAnnotations = New Extended.Windows.Forms.RichTextBoxExtended()
        Me.GroupBoxResults = New System.Windows.Forms.GroupBox()
        Me.lblAirDensityUnits = New System.Windows.Forms.Label()
        Me.tbAirDensity = New System.Windows.Forms.TextBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.lblMaxPowerUnits = New System.Windows.Forms.Label()
        Me.tbMaxPower = New System.Windows.Forms.TextBox()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.lblPowerUnits = New System.Windows.Forms.Label()
        Me.tbGenPower = New System.Windows.Forms.TextBox()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.GroupBoxParameters = New System.Windows.Forms.GroupBox()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.tbRelHum = New System.Windows.Forms.TextBox()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.lblAtmTemp = New System.Windows.Forms.Label()
        Me.tbAtmTemp = New System.Windows.Forms.TextBox()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.lblAtmPres = New System.Windows.Forms.Label()
        Me.tbAtmPres = New System.Windows.Forms.TextBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.tbNumberOfPanels = New System.Windows.Forms.TextBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.tbPanelEfficiency = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.lblDistanceUnits = New System.Windows.Forms.Label()
        Me.tbRotorDiameter = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.chkUseGlobalIrr = New System.Windows.Forms.CheckBox()
        Me.lblWindSpeed = New System.Windows.Forms.Label()
        Me.tbWindSpeed = New System.Windows.Forms.TextBox()
        Me.Label3 = New System.Windows.Forms.Label()
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
        Me.ToolTipChangeTag = New System.Windows.Forms.ToolTip(Me.components)
        Me.GroupBox4.SuspendLayout()
        Me.GroupBoxResults.SuspendLayout()
        Me.GroupBoxParameters.SuspendLayout()
        Me.GroupBoxConnections.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox4
        '
        Me.GroupBox4.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox4.Controls.Add(Me.rtbAnnotations)
        Me.GroupBox4.Location = New System.Drawing.Point(12, 568)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.Size = New System.Drawing.Size(388, 223)
        Me.GroupBox4.TabIndex = 24
        Me.GroupBox4.TabStop = False
        Me.GroupBox4.Text = "Notes"
        '
        'rtbAnnotations
        '
        Me.rtbAnnotations.Dock = System.Windows.Forms.DockStyle.Fill
        Me.rtbAnnotations.Location = New System.Drawing.Point(3, 16)
        Me.rtbAnnotations.Name = "rtbAnnotations"
        Me.rtbAnnotations.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\nouicompat\deflang1046{\fonttbl{\f0\fnil\fcharset0 " &
    "Microsoft Sans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "{\*\generator Riched20 10.0.19041}\viewkind4\uc1 " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\par" &
    "d\f0\fs17\par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.rtbAnnotations.ShowRedo = False
        Me.rtbAnnotations.ShowUndo = False
        Me.rtbAnnotations.Size = New System.Drawing.Size(382, 204)
        Me.rtbAnnotations.TabIndex = 0
        '
        'GroupBoxResults
        '
        Me.GroupBoxResults.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxResults.Controls.Add(Me.lblAirDensityUnits)
        Me.GroupBoxResults.Controls.Add(Me.tbAirDensity)
        Me.GroupBoxResults.Controls.Add(Me.Label9)
        Me.GroupBoxResults.Controls.Add(Me.lblMaxPowerUnits)
        Me.GroupBoxResults.Controls.Add(Me.tbMaxPower)
        Me.GroupBoxResults.Controls.Add(Me.Label7)
        Me.GroupBoxResults.Controls.Add(Me.lblPowerUnits)
        Me.GroupBoxResults.Controls.Add(Me.tbGenPower)
        Me.GroupBoxResults.Controls.Add(Me.Label15)
        Me.GroupBoxResults.Location = New System.Drawing.Point(12, 443)
        Me.GroupBoxResults.Name = "GroupBoxResults"
        Me.GroupBoxResults.Size = New System.Drawing.Size(388, 119)
        Me.GroupBoxResults.TabIndex = 23
        Me.GroupBoxResults.TabStop = False
        Me.GroupBoxResults.Text = "Results"
        '
        'lblAirDensityUnits
        '
        Me.lblAirDensityUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblAirDensityUnits.AutoSize = True
        Me.lblAirDensityUnits.Location = New System.Drawing.Point(332, 86)
        Me.lblAirDensityUnits.Name = "lblAirDensityUnits"
        Me.lblAirDensityUnits.Size = New System.Drawing.Size(24, 13)
        Me.lblAirDensityUnits.TabIndex = 34
        Me.lblAirDensityUnits.Text = "kW"
        '
        'tbAirDensity
        '
        Me.tbAirDensity.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbAirDensity.Location = New System.Drawing.Point(235, 82)
        Me.tbAirDensity.Name = "tbAirDensity"
        Me.tbAirDensity.ReadOnly = True
        Me.tbAirDensity.Size = New System.Drawing.Size(91, 20)
        Me.tbAirDensity.TabIndex = 33
        Me.tbAirDensity.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label9
        '
        Me.Label9.AutoSize = True
        Me.Label9.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label9.Location = New System.Drawing.Point(8, 85)
        Me.Label9.Name = "Label9"
        Me.Label9.Size = New System.Drawing.Size(110, 13)
        Me.Label9.TabIndex = 32
        Me.Label9.Text = "Calculated Air Density"
        '
        'lblMaxPowerUnits
        '
        Me.lblMaxPowerUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblMaxPowerUnits.AutoSize = True
        Me.lblMaxPowerUnits.Location = New System.Drawing.Point(332, 60)
        Me.lblMaxPowerUnits.Name = "lblMaxPowerUnits"
        Me.lblMaxPowerUnits.Size = New System.Drawing.Size(24, 13)
        Me.lblMaxPowerUnits.TabIndex = 31
        Me.lblMaxPowerUnits.Text = "kW"
        '
        'tbMaxPower
        '
        Me.tbMaxPower.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbMaxPower.Location = New System.Drawing.Point(235, 56)
        Me.tbMaxPower.Name = "tbMaxPower"
        Me.tbMaxPower.ReadOnly = True
        Me.tbMaxPower.Size = New System.Drawing.Size(91, 20)
        Me.tbMaxPower.TabIndex = 30
        Me.tbMaxPower.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label7.Location = New System.Drawing.Point(8, 59)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(140, 13)
        Me.Label7.TabIndex = 29
        Me.Label7.Text = "Maximum Theoretical Power"
        '
        'lblPowerUnits
        '
        Me.lblPowerUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblPowerUnits.AutoSize = True
        Me.lblPowerUnits.Location = New System.Drawing.Point(332, 34)
        Me.lblPowerUnits.Name = "lblPowerUnits"
        Me.lblPowerUnits.Size = New System.Drawing.Size(24, 13)
        Me.lblPowerUnits.TabIndex = 28
        Me.lblPowerUnits.Text = "kW"
        '
        'tbGenPower
        '
        Me.tbGenPower.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbGenPower.Location = New System.Drawing.Point(235, 30)
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
        'GroupBoxParameters
        '
        Me.GroupBoxParameters.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxParameters.Controls.Add(Me.Label18)
        Me.GroupBoxParameters.Controls.Add(Me.tbRelHum)
        Me.GroupBoxParameters.Controls.Add(Me.Label19)
        Me.GroupBoxParameters.Controls.Add(Me.lblAtmTemp)
        Me.GroupBoxParameters.Controls.Add(Me.tbAtmTemp)
        Me.GroupBoxParameters.Controls.Add(Me.Label17)
        Me.GroupBoxParameters.Controls.Add(Me.lblAtmPres)
        Me.GroupBoxParameters.Controls.Add(Me.tbAtmPres)
        Me.GroupBoxParameters.Controls.Add(Me.Label10)
        Me.GroupBoxParameters.Controls.Add(Me.tbNumberOfPanels)
        Me.GroupBoxParameters.Controls.Add(Me.Label6)
        Me.GroupBoxParameters.Controls.Add(Me.tbPanelEfficiency)
        Me.GroupBoxParameters.Controls.Add(Me.Label5)
        Me.GroupBoxParameters.Controls.Add(Me.lblDistanceUnits)
        Me.GroupBoxParameters.Controls.Add(Me.tbRotorDiameter)
        Me.GroupBoxParameters.Controls.Add(Me.Label2)
        Me.GroupBoxParameters.Controls.Add(Me.chkUseGlobalIrr)
        Me.GroupBoxParameters.Controls.Add(Me.lblWindSpeed)
        Me.GroupBoxParameters.Controls.Add(Me.tbWindSpeed)
        Me.GroupBoxParameters.Controls.Add(Me.Label3)
        Me.GroupBoxParameters.Location = New System.Drawing.Point(12, 183)
        Me.GroupBoxParameters.Name = "GroupBoxParameters"
        Me.GroupBoxParameters.Size = New System.Drawing.Size(388, 254)
        Me.GroupBoxParameters.TabIndex = 22
        Me.GroupBoxParameters.TabStop = False
        Me.GroupBoxParameters.Text = "Calculation Parameters"
        '
        'Label18
        '
        Me.Label18.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label18.AutoSize = True
        Me.Label18.Location = New System.Drawing.Point(332, 127)
        Me.Label18.Name = "Label18"
        Me.Label18.Size = New System.Drawing.Size(15, 13)
        Me.Label18.TabIndex = 45
        Me.Label18.Text = "%"
        '
        'tbRelHum
        '
        Me.tbRelHum.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbRelHum.Location = New System.Drawing.Point(235, 123)
        Me.tbRelHum.Name = "tbRelHum"
        Me.tbRelHum.Size = New System.Drawing.Size(91, 20)
        Me.tbRelHum.TabIndex = 44
        Me.tbRelHum.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label19
        '
        Me.Label19.AutoSize = True
        Me.Label19.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label19.Location = New System.Drawing.Point(9, 129)
        Me.Label19.Name = "Label19"
        Me.Label19.Size = New System.Drawing.Size(89, 13)
        Me.Label19.TabIndex = 43
        Me.Label19.Text = "Relative Humidity"
        '
        'lblAtmTemp
        '
        Me.lblAtmTemp.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblAtmTemp.AutoSize = True
        Me.lblAtmTemp.Location = New System.Drawing.Point(332, 101)
        Me.lblAtmTemp.Name = "lblAtmTemp"
        Me.lblAtmTemp.Size = New System.Drawing.Size(43, 13)
        Me.lblAtmTemp.TabIndex = 42
        Me.lblAtmTemp.Text = "kW/m2"
        '
        'tbAtmTemp
        '
        Me.tbAtmTemp.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbAtmTemp.Location = New System.Drawing.Point(235, 97)
        Me.tbAtmTemp.Name = "tbAtmTemp"
        Me.tbAtmTemp.Size = New System.Drawing.Size(91, 20)
        Me.tbAtmTemp.TabIndex = 41
        Me.tbAtmTemp.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label17
        '
        Me.Label17.AutoSize = True
        Me.Label17.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label17.Location = New System.Drawing.Point(9, 103)
        Me.Label17.Name = "Label17"
        Me.Label17.Size = New System.Drawing.Size(128, 13)
        Me.Label17.TabIndex = 40
        Me.Label17.Text = "Atmospheric Temperature"
        '
        'lblAtmPres
        '
        Me.lblAtmPres.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblAtmPres.AutoSize = True
        Me.lblAtmPres.Location = New System.Drawing.Point(332, 75)
        Me.lblAtmPres.Name = "lblAtmPres"
        Me.lblAtmPres.Size = New System.Drawing.Size(43, 13)
        Me.lblAtmPres.TabIndex = 39
        Me.lblAtmPres.Text = "kW/m2"
        '
        'tbAtmPres
        '
        Me.tbAtmPres.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbAtmPres.Location = New System.Drawing.Point(235, 71)
        Me.tbAtmPres.Name = "tbAtmPres"
        Me.tbAtmPres.Size = New System.Drawing.Size(91, 20)
        Me.tbAtmPres.TabIndex = 38
        Me.tbAtmPres.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label10
        '
        Me.Label10.AutoSize = True
        Me.Label10.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label10.Location = New System.Drawing.Point(9, 77)
        Me.Label10.Name = "Label10"
        Me.Label10.Size = New System.Drawing.Size(109, 13)
        Me.Label10.TabIndex = 37
        Me.Label10.Text = "Atmospheric Pressure"
        '
        'tbNumberOfPanels
        '
        Me.tbNumberOfPanels.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbNumberOfPanels.Location = New System.Drawing.Point(235, 216)
        Me.tbNumberOfPanels.Name = "tbNumberOfPanels"
        Me.tbNumberOfPanels.Size = New System.Drawing.Size(91, 20)
        Me.tbNumberOfPanels.TabIndex = 36
        Me.tbNumberOfPanels.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label6
        '
        Me.Label6.AutoSize = True
        Me.Label6.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label6.Location = New System.Drawing.Point(9, 218)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(83, 13)
        Me.Label6.TabIndex = 35
        Me.Label6.Text = "Number of Units"
        '
        'tbPanelEfficiency
        '
        Me.tbPanelEfficiency.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbPanelEfficiency.Location = New System.Drawing.Point(235, 190)
        Me.tbPanelEfficiency.Name = "tbPanelEfficiency"
        Me.tbPanelEfficiency.Size = New System.Drawing.Size(91, 20)
        Me.tbPanelEfficiency.TabIndex = 34
        Me.tbPanelEfficiency.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label5
        '
        Me.Label5.AutoSize = True
        Me.Label5.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label5.Location = New System.Drawing.Point(9, 192)
        Me.Label5.Name = "Label5"
        Me.Label5.Size = New System.Drawing.Size(136, 13)
        Me.Label5.TabIndex = 33
        Me.Label5.Text = "Turbine Efficiency (0-100%)"
        '
        'lblDistanceUnits
        '
        Me.lblDistanceUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblDistanceUnits.AutoSize = True
        Me.lblDistanceUnits.Location = New System.Drawing.Point(333, 167)
        Me.lblDistanceUnits.Name = "lblDistanceUnits"
        Me.lblDistanceUnits.Size = New System.Drawing.Size(43, 13)
        Me.lblDistanceUnits.TabIndex = 32
        Me.lblDistanceUnits.Text = "kW/m2"
        '
        'tbRotorDiameter
        '
        Me.tbRotorDiameter.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbRotorDiameter.Location = New System.Drawing.Point(235, 164)
        Me.tbRotorDiameter.Name = "tbRotorDiameter"
        Me.tbRotorDiameter.Size = New System.Drawing.Size(91, 20)
        Me.tbRotorDiameter.TabIndex = 31
        Me.tbRotorDiameter.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label2.Location = New System.Drawing.Point(9, 166)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(78, 13)
        Me.Label2.TabIndex = 30
        Me.Label2.Text = "Rotor Diameter"
        '
        'chkUseGlobalIrr
        '
        Me.chkUseGlobalIrr.AutoSize = True
        Me.chkUseGlobalIrr.Location = New System.Drawing.Point(11, 24)
        Me.chkUseGlobalIrr.Name = "chkUseGlobalIrr"
        Me.chkUseGlobalIrr.Size = New System.Drawing.Size(174, 17)
        Me.chkUseGlobalIrr.TabIndex = 29
        Me.chkUseGlobalIrr.Text = "Use Global Weather Conditions"
        Me.chkUseGlobalIrr.UseVisualStyleBackColor = True
        '
        'lblWindSpeed
        '
        Me.lblWindSpeed.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblWindSpeed.AutoSize = True
        Me.lblWindSpeed.Location = New System.Drawing.Point(333, 49)
        Me.lblWindSpeed.Name = "lblWindSpeed"
        Me.lblWindSpeed.Size = New System.Drawing.Size(43, 13)
        Me.lblWindSpeed.TabIndex = 28
        Me.lblWindSpeed.Text = "kW/m2"
        '
        'tbWindSpeed
        '
        Me.tbWindSpeed.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbWindSpeed.Location = New System.Drawing.Point(235, 45)
        Me.tbWindSpeed.Name = "tbWindSpeed"
        Me.tbWindSpeed.Size = New System.Drawing.Size(91, 20)
        Me.tbWindSpeed.TabIndex = 27
        Me.tbWindSpeed.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label3
        '
        Me.Label3.AutoSize = True
        Me.Label3.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label3.Location = New System.Drawing.Point(9, 51)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(66, 13)
        Me.Label3.TabIndex = 26
        Me.Label3.Text = "Wind Speed"
        '
        'GroupBoxConnections
        '
        Me.GroupBoxConnections.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBoxConnections.Controls.Add(Me.btnCreateAndConnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.btnDisconnectEnergy)
        Me.GroupBoxConnections.Controls.Add(Me.Label14)
        Me.GroupBoxConnections.Controls.Add(Me.cbEnergy)
        Me.GroupBoxConnections.Location = New System.Drawing.Point(12, 112)
        Me.GroupBoxConnections.Name = "GroupBoxConnections"
        Me.GroupBoxConnections.Size = New System.Drawing.Size(388, 65)
        Me.GroupBoxConnections.TabIndex = 21
        Me.GroupBoxConnections.TabStop = False
        Me.GroupBoxConnections.Text = "Connections"
        '
        'btnCreateAndConnectEnergy
        '
        Me.btnCreateAndConnectEnergy.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.btnCreateAndConnectEnergy.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.btnCreateAndConnectEnergy.Image = CType(resources.GetObject("btnCreateAndConnectEnergy.Image"), System.Drawing.Image)
        Me.btnCreateAndConnectEnergy.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.btnCreateAndConnectEnergy.Location = New System.Drawing.Point(332, 26)
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
        Me.btnDisconnectEnergy.Location = New System.Drawing.Point(359, 26)
        Me.btnDisconnectEnergy.Name = "btnDisconnectEnergy"
        Me.btnDisconnectEnergy.Size = New System.Drawing.Size(21, 21)
        Me.btnDisconnectEnergy.TabIndex = 23
        Me.btnDisconnectEnergy.UseVisualStyleBackColor = True
        '
        'Label14
        '
        Me.Label14.AutoSize = True
        Me.Label14.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.Label14.Location = New System.Drawing.Point(9, 30)
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
        Me.cbEnergy.Location = New System.Drawing.Point(133, 26)
        Me.cbEnergy.Name = "cbEnergy"
        Me.cbEnergy.Size = New System.Drawing.Size(193, 21)
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
        Me.GroupBox5.Location = New System.Drawing.Point(12, 12)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.Size = New System.Drawing.Size(388, 98)
        Me.GroupBox5.TabIndex = 20
        Me.GroupBox5.TabStop = False
        Me.GroupBox5.Text = "General Info"
        '
        'lblTag
        '
        Me.lblTag.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblTag.Location = New System.Drawing.Point(133, 19)
        Me.lblTag.Name = "lblTag"
        Me.lblTag.Size = New System.Drawing.Size(247, 20)
        Me.lblTag.TabIndex = 24
        '
        'chkActive
        '
        Me.chkActive.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.chkActive.Appearance = System.Windows.Forms.Appearance.Button
        Me.chkActive.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.bullet_tick
        Me.chkActive.ImeMode = System.Windows.Forms.ImeMode.NoControl
        Me.chkActive.Location = New System.Drawing.Point(359, 43)
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
        'ToolTipChangeTag
        '
        Me.ToolTipChangeTag.ToolTipIcon = System.Windows.Forms.ToolTipIcon.Info
        Me.ToolTipChangeTag.ToolTipTitle = "Info"
        '
        'EditingForm_WindTurbine
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.AutoScroll = True
        Me.ClientSize = New System.Drawing.Size(412, 821)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.GroupBoxResults)
        Me.Controls.Add(Me.GroupBoxParameters)
        Me.Controls.Add(Me.GroupBoxConnections)
        Me.Controls.Add(Me.GroupBox5)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "EditingForm_WindTurbine"
        Me.Text = "EditingForm_WindTurbine"
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBoxResults.ResumeLayout(False)
        Me.GroupBoxResults.PerformLayout()
        Me.GroupBoxParameters.ResumeLayout(False)
        Me.GroupBoxParameters.PerformLayout()
        Me.GroupBoxConnections.ResumeLayout(False)
        Me.GroupBoxConnections.PerformLayout()
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Public WithEvents GroupBox4 As GroupBox
    Public WithEvents rtbAnnotations As Extended.Windows.Forms.RichTextBoxExtended
    Public WithEvents GroupBoxResults As GroupBox
    Friend WithEvents lblPowerUnits As Label
    Public WithEvents tbGenPower As TextBox
    Public WithEvents Label15 As Label
    Public WithEvents GroupBoxParameters As GroupBox
    Public WithEvents tbNumberOfPanels As TextBox
    Public WithEvents Label6 As Label
    Public WithEvents tbPanelEfficiency As TextBox
    Public WithEvents Label5 As Label
    Friend WithEvents lblDistanceUnits As Label
    Public WithEvents tbRotorDiameter As TextBox
    Public WithEvents Label2 As Label
    Friend WithEvents chkUseGlobalIrr As CheckBox
    Friend WithEvents lblWindSpeed As Label
    Public WithEvents tbWindSpeed As TextBox
    Public WithEvents Label3 As Label
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
    Friend WithEvents lblAirDensityUnits As Label
    Public WithEvents tbAirDensity As TextBox
    Public WithEvents Label9 As Label
    Friend WithEvents lblMaxPowerUnits As Label
    Public WithEvents tbMaxPower As TextBox
    Public WithEvents Label7 As Label
    Friend WithEvents Label18 As Label
    Public WithEvents tbRelHum As TextBox
    Public WithEvents Label19 As Label
    Friend WithEvents lblAtmTemp As Label
    Public WithEvents tbAtmTemp As TextBox
    Public WithEvents Label17 As Label
    Friend WithEvents lblAtmPres As Label
    Public WithEvents tbAtmPres As TextBox
    Public WithEvents Label10 As Label
    Friend WithEvents ToolTipChangeTag As ToolTip
End Class
