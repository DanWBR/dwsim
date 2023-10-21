<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class GHGEmitterEditor
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
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
        Me.chkEmissionActive = New System.Windows.Forms.CheckBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbEmissionFactor = New System.Windows.Forms.TextBox()
        Me.cbGasComposition = New System.Windows.Forms.ComboBox()
        Me.lblEFUnits = New System.Windows.Forms.Label()
        Me.chkOverridePower = New System.Windows.Forms.CheckBox()
        Me.tbOverridePower = New System.Windows.Forms.TextBox()
        Me.lbECUnits = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.chkCO2eq = New System.Windows.Forms.CheckBox()
        Me.PanelUOParameters = New System.Windows.Forms.Panel()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.cms1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.PanelUOParameters.SuspendLayout()
        Me.SuspendLayout()
        '
        'chkEmissionActive
        '
        Me.chkEmissionActive.AutoSize = True
        Me.chkEmissionActive.Location = New System.Drawing.Point(13, 10)
        Me.chkEmissionActive.Name = "chkEmissionActive"
        Me.chkEmissionActive.Size = New System.Drawing.Size(100, 17)
        Me.chkEmissionActive.TabIndex = 0
        Me.chkEmissionActive.Text = "Emission Active"
        Me.ToolTip1.SetToolTip(Me.chkEmissionActive, "This Emission is Active and will be taken into account when calculating global pr" &
        "ocess emissions.")
        Me.chkEmissionActive.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(3, 43)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(81, 13)
        Me.Label1.TabIndex = 1
        Me.Label1.Text = "Emission Factor"
        '
        'tbEmissionFactor
        '
        Me.tbEmissionFactor.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbEmissionFactor.Location = New System.Drawing.Point(283, 40)
        Me.tbEmissionFactor.Name = "tbEmissionFactor"
        Me.tbEmissionFactor.Size = New System.Drawing.Size(92, 20)
        Me.tbEmissionFactor.TabIndex = 2
        Me.tbEmissionFactor.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'cbGasComposition
        '
        Me.cbGasComposition.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.cbGasComposition.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbGasComposition.FormattingEnabled = True
        Me.cbGasComposition.Location = New System.Drawing.Point(283, 6)
        Me.cbGasComposition.Name = "cbGasComposition"
        Me.cbGasComposition.Size = New System.Drawing.Size(92, 21)
        Me.cbGasComposition.TabIndex = 3
        '
        'lblEFUnits
        '
        Me.lblEFUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblEFUnits.AutoSize = True
        Me.lblEFUnits.Location = New System.Drawing.Point(381, 43)
        Me.lblEFUnits.Name = "lblEFUnits"
        Me.lblEFUnits.Size = New System.Drawing.Size(39, 13)
        Me.lblEFUnits.TabIndex = 4
        Me.lblEFUnits.Text = "Label2"
        '
        'chkOverridePower
        '
        Me.chkOverridePower.AutoSize = True
        Me.chkOverridePower.Location = New System.Drawing.Point(6, 89)
        Me.chkOverridePower.Name = "chkOverridePower"
        Me.chkOverridePower.Size = New System.Drawing.Size(152, 17)
        Me.chkOverridePower.TabIndex = 8
        Me.chkOverridePower.Text = "Override Ref. Power Value"
        Me.ToolTip1.SetToolTip(Me.chkOverridePower, "Overrides the Reference Value for Power/Energy Consumption/Generation used to cal" &
        "culate the amount of emmited GHG.")
        Me.chkOverridePower.UseVisualStyleBackColor = True
        '
        'tbOverridePower
        '
        Me.tbOverridePower.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbOverridePower.Enabled = False
        Me.tbOverridePower.Location = New System.Drawing.Point(283, 87)
        Me.tbOverridePower.Name = "tbOverridePower"
        Me.tbOverridePower.Size = New System.Drawing.Size(92, 20)
        Me.tbOverridePower.TabIndex = 9
        Me.tbOverridePower.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'lbECUnits
        '
        Me.lbECUnits.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lbECUnits.AutoSize = True
        Me.lbECUnits.Location = New System.Drawing.Point(381, 90)
        Me.lbECUnits.Name = "lbECUnits"
        Me.lbECUnits.Size = New System.Drawing.Size(39, 13)
        Me.lbECUnits.TabIndex = 10
        Me.lbECUnits.Text = "Label6"
        '
        'Label7
        '
        Me.Label7.AutoSize = True
        Me.Label7.Location = New System.Drawing.Point(3, 9)
        Me.Label7.Name = "Label7"
        Me.Label7.Size = New System.Drawing.Size(129, 13)
        Me.Label7.TabIndex = 11
        Me.Label7.Text = "Emmited Gas Composition"
        '
        'chkCO2eq
        '
        Me.chkCO2eq.AutoSize = True
        Me.chkCO2eq.Location = New System.Drawing.Point(92, 42)
        Me.chkCO2eq.Name = "chkCO2eq"
        Me.chkCO2eq.Size = New System.Drawing.Size(59, 17)
        Me.chkCO2eq.TabIndex = 12
        Me.chkCO2eq.Text = "CO2eq"
        Me.ToolTip1.SetToolTip(Me.chkCO2eq, "The given Emission Factor is in CO2-equivalent.")
        Me.chkCO2eq.UseVisualStyleBackColor = True
        '
        'PanelUOParameters
        '
        Me.PanelUOParameters.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.PanelUOParameters.Controls.Add(Me.Button1)
        Me.PanelUOParameters.Controls.Add(Me.Label7)
        Me.PanelUOParameters.Controls.Add(Me.chkCO2eq)
        Me.PanelUOParameters.Controls.Add(Me.Label1)
        Me.PanelUOParameters.Controls.Add(Me.tbEmissionFactor)
        Me.PanelUOParameters.Controls.Add(Me.lbECUnits)
        Me.PanelUOParameters.Controls.Add(Me.cbGasComposition)
        Me.PanelUOParameters.Controls.Add(Me.tbOverridePower)
        Me.PanelUOParameters.Controls.Add(Me.lblEFUnits)
        Me.PanelUOParameters.Controls.Add(Me.chkOverridePower)
        Me.PanelUOParameters.Location = New System.Drawing.Point(7, 34)
        Me.PanelUOParameters.Name = "PanelUOParameters"
        Me.PanelUOParameters.Size = New System.Drawing.Size(431, 150)
        Me.PanelUOParameters.TabIndex = 13
        '
        'Button1
        '
        Me.Button1.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Button1.BackgroundImage = Global.DWSIM.SharedClasses.My.Resources.Resources.read_64px
        Me.Button1.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Zoom
        Me.Button1.Location = New System.Drawing.Point(253, 38)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(24, 24)
        Me.Button1.TabIndex = 13
        Me.ToolTip1.SetToolTip(Me.Button1, "Reference Emission Factors taken from " & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "https://www.epa.gov/climateleadership/ghg" &
        "-emission-factors-hub, 2023 Edition.")
        Me.Button1.UseVisualStyleBackColor = True
        '
        'cms1
        '
        Me.cms1.Name = "cms1"
        Me.cms1.ShowImageMargin = False
        Me.cms1.Size = New System.Drawing.Size(36, 4)
        '
        'GHGEmitterEditor
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.PanelUOParameters)
        Me.Controls.Add(Me.chkEmissionActive)
        Me.DoubleBuffered = True
        Me.Name = "GHGEmitterEditor"
        Me.Size = New System.Drawing.Size(441, 201)
        Me.PanelUOParameters.ResumeLayout(False)
        Me.PanelUOParameters.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents chkEmissionActive As CheckBox
    Friend WithEvents Label1 As Label
    Friend WithEvents tbEmissionFactor As TextBox
    Friend WithEvents cbGasComposition As ComboBox
    Friend WithEvents lblEFUnits As Label
    Friend WithEvents chkOverridePower As CheckBox
    Friend WithEvents tbOverridePower As TextBox
    Friend WithEvents lbECUnits As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents chkCO2eq As CheckBox
    Public WithEvents PanelUOParameters As Panel
    Friend WithEvents Button1 As Button
    Friend WithEvents cms1 As ContextMenuStrip
    Friend WithEvents ToolTip1 As ToolTip
End Class
