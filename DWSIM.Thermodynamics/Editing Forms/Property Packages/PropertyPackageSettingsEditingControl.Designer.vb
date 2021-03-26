<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class PropertyPackageSettingsEditingControl
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(PropertyPackageSettingsEditingControl))
        Me.cbLiqDens = New System.Windows.Forms.ComboBox()
        Me.chkLiqDensPCorr = New System.Windows.Forms.CheckBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.chkLiqViscPCorr = New System.Windows.Forms.CheckBox()
        Me.chkLiqDensPeneloux = New System.Windows.Forms.CheckBox()
        Me.cbLiqVisc = New System.Windows.Forms.ComboBox()
        Me.cbHSCpCalcMode = New System.Windows.Forms.ComboBox()
        Me.cbLiqVIscMixRule = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.chkLiqFugPoynt = New System.Windows.Forms.CheckBox()
        Me.chkVapFugIdeal = New System.Windows.Forms.CheckBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.chkIgnoreSalLim = New System.Windows.Forms.CheckBox()
        Me.chkIgnoreVapFracLim = New System.Windows.Forms.CheckBox()
        Me.chkIgnoreIPs = New System.Windows.Forms.CheckBox()
        Me.chkUseSolidCp = New System.Windows.Forms.CheckBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.chkCalcAdditionalProps = New System.Windows.Forms.CheckBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'cbLiqDens
        '
        resources.ApplyResources(Me.cbLiqDens, "cbLiqDens")
        Me.cbLiqDens.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbLiqDens.FormattingEnabled = True
        Me.cbLiqDens.Items.AddRange(New Object() {resources.GetString("cbLiqDens.Items"), resources.GetString("cbLiqDens.Items1"), resources.GetString("cbLiqDens.Items2")})
        Me.cbLiqDens.Name = "cbLiqDens"
        '
        'chkLiqDensPCorr
        '
        resources.ApplyResources(Me.chkLiqDensPCorr, "chkLiqDensPCorr")
        Me.chkLiqDensPCorr.Name = "chkLiqDensPCorr"
        Me.chkLiqDensPCorr.UseVisualStyleBackColor = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'chkLiqViscPCorr
        '
        resources.ApplyResources(Me.chkLiqViscPCorr, "chkLiqViscPCorr")
        Me.chkLiqViscPCorr.Name = "chkLiqViscPCorr"
        Me.chkLiqViscPCorr.UseVisualStyleBackColor = True
        '
        'chkLiqDensPeneloux
        '
        resources.ApplyResources(Me.chkLiqDensPeneloux, "chkLiqDensPeneloux")
        Me.chkLiqDensPeneloux.Name = "chkLiqDensPeneloux"
        Me.chkLiqDensPeneloux.UseVisualStyleBackColor = True
        '
        'cbLiqVisc
        '
        resources.ApplyResources(Me.cbLiqVisc, "cbLiqVisc")
        Me.cbLiqVisc.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbLiqVisc.FormattingEnabled = True
        Me.cbLiqVisc.Items.AddRange(New Object() {resources.GetString("cbLiqVisc.Items"), resources.GetString("cbLiqVisc.Items1")})
        Me.cbLiqVisc.Name = "cbLiqVisc"
        '
        'cbHSCpCalcMode
        '
        resources.ApplyResources(Me.cbHSCpCalcMode, "cbHSCpCalcMode")
        Me.cbHSCpCalcMode.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbHSCpCalcMode.FormattingEnabled = True
        Me.cbHSCpCalcMode.Items.AddRange(New Object() {resources.GetString("cbHSCpCalcMode.Items"), resources.GetString("cbHSCpCalcMode.Items1"), resources.GetString("cbHSCpCalcMode.Items2"), resources.GetString("cbHSCpCalcMode.Items3")})
        Me.cbHSCpCalcMode.Name = "cbHSCpCalcMode"
        '
        'cbLiqVIscMixRule
        '
        resources.ApplyResources(Me.cbLiqVIscMixRule, "cbLiqVIscMixRule")
        Me.cbLiqVIscMixRule.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbLiqVIscMixRule.FormattingEnabled = True
        Me.cbLiqVIscMixRule.Items.AddRange(New Object() {resources.GetString("cbLiqVIscMixRule.Items"), resources.GetString("cbLiqVIscMixRule.Items1"), resources.GetString("cbLiqVIscMixRule.Items2"), resources.GetString("cbLiqVIscMixRule.Items3")})
        Me.cbLiqVIscMixRule.Name = "cbLiqVIscMixRule"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'chkLiqFugPoynt
        '
        resources.ApplyResources(Me.chkLiqFugPoynt, "chkLiqFugPoynt")
        Me.chkLiqFugPoynt.Name = "chkLiqFugPoynt"
        Me.chkLiqFugPoynt.UseVisualStyleBackColor = True
        '
        'chkVapFugIdeal
        '
        resources.ApplyResources(Me.chkVapFugIdeal, "chkVapFugIdeal")
        Me.chkVapFugIdeal.Name = "chkVapFugIdeal"
        Me.chkVapFugIdeal.UseVisualStyleBackColor = True
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'chkIgnoreSalLim
        '
        resources.ApplyResources(Me.chkIgnoreSalLim, "chkIgnoreSalLim")
        Me.chkIgnoreSalLim.Name = "chkIgnoreSalLim"
        Me.chkIgnoreSalLim.UseVisualStyleBackColor = True
        '
        'chkIgnoreVapFracLim
        '
        resources.ApplyResources(Me.chkIgnoreVapFracLim, "chkIgnoreVapFracLim")
        Me.chkIgnoreVapFracLim.Name = "chkIgnoreVapFracLim"
        Me.chkIgnoreVapFracLim.UseVisualStyleBackColor = True
        '
        'chkIgnoreIPs
        '
        resources.ApplyResources(Me.chkIgnoreIPs, "chkIgnoreIPs")
        Me.chkIgnoreIPs.Name = "chkIgnoreIPs"
        Me.chkIgnoreIPs.UseVisualStyleBackColor = True
        '
        'chkUseSolidCp
        '
        resources.ApplyResources(Me.chkUseSolidCp, "chkUseSolidCp")
        Me.chkUseSolidCp.Name = "chkUseSolidCp"
        Me.chkUseSolidCp.UseVisualStyleBackColor = True
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'chkCalcAdditionalProps
        '
        resources.ApplyResources(Me.chkCalcAdditionalProps, "chkCalcAdditionalProps")
        Me.chkCalcAdditionalProps.Name = "chkCalcAdditionalProps"
        Me.chkCalcAdditionalProps.UseVisualStyleBackColor = True
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'PropertyPackageSettingsEditingControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.SystemColors.Window
        Me.Controls.Add(Me.chkCalcAdditionalProps)
        Me.Controls.Add(Me.Label11)
        Me.Controls.Add(Me.Label10)
        Me.Controls.Add(Me.chkUseSolidCp)
        Me.Controls.Add(Me.chkIgnoreIPs)
        Me.Controls.Add(Me.chkIgnoreVapFracLim)
        Me.Controls.Add(Me.chkIgnoreSalLim)
        Me.Controls.Add(Me.Label9)
        Me.Controls.Add(Me.chkVapFugIdeal)
        Me.Controls.Add(Me.chkLiqFugPoynt)
        Me.Controls.Add(Me.cbLiqVIscMixRule)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.cbHSCpCalcMode)
        Me.Controls.Add(Me.cbLiqVisc)
        Me.Controls.Add(Me.chkLiqDensPeneloux)
        Me.Controls.Add(Me.chkLiqViscPCorr)
        Me.Controls.Add(Me.Label8)
        Me.Controls.Add(Me.Label7)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.chkLiqDensPCorr)
        Me.Controls.Add(Me.cbLiqDens)
        Me.Name = "PropertyPackageSettingsEditingControl"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents cbLiqDens As ComboBox
    Friend WithEvents chkLiqDensPCorr As CheckBox
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents Label4 As Label
    Friend WithEvents Label5 As Label
    Friend WithEvents Label6 As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents Label8 As Label
    Friend WithEvents chkLiqViscPCorr As CheckBox
    Friend WithEvents chkLiqDensPeneloux As CheckBox
    Friend WithEvents cbLiqVisc As ComboBox
    Friend WithEvents cbHSCpCalcMode As ComboBox
    Friend WithEvents cbLiqVIscMixRule As ComboBox
    Friend WithEvents Label1 As Label
    Friend WithEvents chkLiqFugPoynt As CheckBox
    Friend WithEvents chkVapFugIdeal As CheckBox
    Friend WithEvents Label9 As Label
    Friend WithEvents chkIgnoreSalLim As CheckBox
    Friend WithEvents chkIgnoreVapFracLim As CheckBox
    Friend WithEvents chkIgnoreIPs As CheckBox
    Friend WithEvents chkUseSolidCp As CheckBox
    Friend WithEvents Label10 As Label
    Friend WithEvents chkCalcAdditionalProps As CheckBox
    Friend WithEvents Label11 As Label
End Class
