<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FlashAlgorithmConfig
    Inherits System.Windows.Forms.Form

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FlashAlgorithmConfig))
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPageGeneral = New System.Windows.Forms.TabPage()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.cbFSMethod = New System.Windows.Forms.ComboBox()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.btnConfigExtSolver = New System.Windows.Forms.Button()
        Me.cbExternalSolver = New System.Windows.Forms.ComboBox()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.gpNL = New System.Windows.Forms.GroupBox()
        Me.chkIdealPVFlash = New System.Windows.Forms.CheckBox()
        Me.CheckBoxInterpTempPHFlash = New System.Windows.Forms.CheckBox()
        Me.chkHandleSolids = New System.Windows.Forms.CheckBox()
        Me.chkImmiscible = New System.Windows.Forms.CheckBox()
        Me.chkFastModeNL = New System.Windows.Forms.CheckBox()
        Me.cbFlashApproach = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.chkDoPhaseId = New System.Windows.Forms.CheckBox()
        Me.chkCalcBubbleDew = New System.Windows.Forms.CheckBox()
        Me.cbFlashType = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.tbPT_DampingFactor = New System.Windows.Forms.TextBox()
        Me.Label30 = New System.Windows.Forms.Label()
        Me.tbPH_MaxDT = New System.Windows.Forms.TextBox()
        Me.Label29 = New System.Windows.Forms.Label()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.tbPV_EpsilonT = New System.Windows.Forms.TextBox()
        Me.tbPV_DampingFactor = New System.Windows.Forms.TextBox()
        Me.chkValidateEqCalc = New System.Windows.Forms.CheckBox()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.tbPV_MaxDT = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.TabPageConvPars = New System.Windows.Forms.TabPage()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tbPHExtMaxIt = New System.Windows.Forms.TextBox()
        Me.tbPHIntMaxIt = New System.Windows.Forms.TextBox()
        Me.tbPHExtMaxTol = New System.Windows.Forms.TextBox()
        Me.tbPHintMaxTol = New System.Windows.Forms.TextBox()
        Me.tbPTExtMaxIt = New System.Windows.Forms.TextBox()
        Me.tbPTintMaxIt = New System.Windows.Forms.TextBox()
        Me.tbPTExtTol = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbPTIntTol = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.chkDisplayCompWarning = New System.Windows.Forms.CheckBox()
        Me.TabControl1.SuspendLayout()
        Me.TabPageGeneral.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.gpNL.SuspendLayout()
        Me.TabPageConvPars.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPageGeneral)
        Me.TabControl1.Controls.Add(Me.TabPageConvPars)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPageGeneral
        '
        resources.ApplyResources(Me.TabPageGeneral, "TabPageGeneral")
        Me.TabPageGeneral.Controls.Add(Me.GroupBox4)
        Me.TabPageGeneral.Name = "TabPageGeneral"
        Me.TabPageGeneral.UseVisualStyleBackColor = True
        '
        'GroupBox4
        '
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Controls.Add(Me.chkDisplayCompWarning)
        Me.GroupBox4.Controls.Add(Me.cbFSMethod)
        Me.GroupBox4.Controls.Add(Me.Label12)
        Me.GroupBox4.Controls.Add(Me.btnConfigExtSolver)
        Me.GroupBox4.Controls.Add(Me.cbExternalSolver)
        Me.GroupBox4.Controls.Add(Me.Label11)
        Me.GroupBox4.Controls.Add(Me.gpNL)
        Me.GroupBox4.Controls.Add(Me.cbFlashApproach)
        Me.GroupBox4.Controls.Add(Me.Label10)
        Me.GroupBox4.Controls.Add(Me.chkDoPhaseId)
        Me.GroupBox4.Controls.Add(Me.chkCalcBubbleDew)
        Me.GroupBox4.Controls.Add(Me.cbFlashType)
        Me.GroupBox4.Controls.Add(Me.Label9)
        Me.GroupBox4.Controls.Add(Me.tbPT_DampingFactor)
        Me.GroupBox4.Controls.Add(Me.Label30)
        Me.GroupBox4.Controls.Add(Me.tbPH_MaxDT)
        Me.GroupBox4.Controls.Add(Me.Label29)
        Me.GroupBox4.Controls.Add(Me.Label26)
        Me.GroupBox4.Controls.Add(Me.tbPV_EpsilonT)
        Me.GroupBox4.Controls.Add(Me.tbPV_DampingFactor)
        Me.GroupBox4.Controls.Add(Me.chkValidateEqCalc)
        Me.GroupBox4.Controls.Add(Me.Label25)
        Me.GroupBox4.Controls.Add(Me.tbPV_MaxDT)
        Me.GroupBox4.Controls.Add(Me.Label24)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'cbFSMethod
        '
        resources.ApplyResources(Me.cbFSMethod, "cbFSMethod")
        Me.cbFSMethod.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFSMethod.FormattingEnabled = True
        Me.cbFSMethod.Items.AddRange(New Object() {resources.GetString("cbFSMethod.Items"), resources.GetString("cbFSMethod.Items1"), resources.GetString("cbFSMethod.Items2"), resources.GetString("cbFSMethod.Items3")})
        Me.cbFSMethod.Name = "cbFSMethod"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'btnConfigExtSolver
        '
        resources.ApplyResources(Me.btnConfigExtSolver, "btnConfigExtSolver")
        Me.btnConfigExtSolver.BackgroundImage = Global.DWSIM.Thermodynamics.My.Resources.Resources.cog1
        Me.btnConfigExtSolver.Name = "btnConfigExtSolver"
        Me.btnConfigExtSolver.UseVisualStyleBackColor = True
        '
        'cbExternalSolver
        '
        resources.ApplyResources(Me.cbExternalSolver, "cbExternalSolver")
        Me.cbExternalSolver.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbExternalSolver.FormattingEnabled = True
        Me.cbExternalSolver.Name = "cbExternalSolver"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'gpNL
        '
        resources.ApplyResources(Me.gpNL, "gpNL")
        Me.gpNL.Controls.Add(Me.chkIdealPVFlash)
        Me.gpNL.Controls.Add(Me.CheckBoxInterpTempPHFlash)
        Me.gpNL.Controls.Add(Me.chkHandleSolids)
        Me.gpNL.Controls.Add(Me.chkImmiscible)
        Me.gpNL.Controls.Add(Me.chkFastModeNL)
        Me.gpNL.Name = "gpNL"
        Me.gpNL.TabStop = False
        '
        'chkIdealPVFlash
        '
        resources.ApplyResources(Me.chkIdealPVFlash, "chkIdealPVFlash")
        Me.chkIdealPVFlash.Name = "chkIdealPVFlash"
        Me.chkIdealPVFlash.UseVisualStyleBackColor = True
        '
        'CheckBoxInterpTempPHFlash
        '
        resources.ApplyResources(Me.CheckBoxInterpTempPHFlash, "CheckBoxInterpTempPHFlash")
        Me.CheckBoxInterpTempPHFlash.Name = "CheckBoxInterpTempPHFlash"
        Me.CheckBoxInterpTempPHFlash.UseVisualStyleBackColor = True
        '
        'chkHandleSolids
        '
        resources.ApplyResources(Me.chkHandleSolids, "chkHandleSolids")
        Me.chkHandleSolids.Name = "chkHandleSolids"
        '
        'chkImmiscible
        '
        resources.ApplyResources(Me.chkImmiscible, "chkImmiscible")
        Me.chkImmiscible.Name = "chkImmiscible"
        '
        'chkFastModeNL
        '
        resources.ApplyResources(Me.chkFastModeNL, "chkFastModeNL")
        Me.chkFastModeNL.Name = "chkFastModeNL"
        Me.chkFastModeNL.UseVisualStyleBackColor = True
        '
        'cbFlashApproach
        '
        resources.ApplyResources(Me.cbFlashApproach, "cbFlashApproach")
        Me.cbFlashApproach.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlashApproach.FormattingEnabled = True
        Me.cbFlashApproach.Items.AddRange(New Object() {resources.GetString("cbFlashApproach.Items"), resources.GetString("cbFlashApproach.Items1"), resources.GetString("cbFlashApproach.Items2")})
        Me.cbFlashApproach.Name = "cbFlashApproach"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'chkDoPhaseId
        '
        resources.ApplyResources(Me.chkDoPhaseId, "chkDoPhaseId")
        Me.chkDoPhaseId.Name = "chkDoPhaseId"
        '
        'chkCalcBubbleDew
        '
        resources.ApplyResources(Me.chkCalcBubbleDew, "chkCalcBubbleDew")
        Me.chkCalcBubbleDew.Name = "chkCalcBubbleDew"
        '
        'cbFlashType
        '
        resources.ApplyResources(Me.cbFlashType, "cbFlashType")
        Me.cbFlashType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFlashType.FormattingEnabled = True
        Me.cbFlashType.Items.AddRange(New Object() {resources.GetString("cbFlashType.Items"), resources.GetString("cbFlashType.Items1"), resources.GetString("cbFlashType.Items2"), resources.GetString("cbFlashType.Items3"), resources.GetString("cbFlashType.Items4"), resources.GetString("cbFlashType.Items5")})
        Me.cbFlashType.Name = "cbFlashType"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'tbPT_DampingFactor
        '
        resources.ApplyResources(Me.tbPT_DampingFactor, "tbPT_DampingFactor")
        Me.tbPT_DampingFactor.Name = "tbPT_DampingFactor"
        '
        'Label30
        '
        resources.ApplyResources(Me.Label30, "Label30")
        Me.Label30.Name = "Label30"
        '
        'tbPH_MaxDT
        '
        resources.ApplyResources(Me.tbPH_MaxDT, "tbPH_MaxDT")
        Me.tbPH_MaxDT.Name = "tbPH_MaxDT"
        '
        'Label29
        '
        resources.ApplyResources(Me.Label29, "Label29")
        Me.Label29.Name = "Label29"
        '
        'Label26
        '
        resources.ApplyResources(Me.Label26, "Label26")
        Me.Label26.Name = "Label26"
        '
        'tbPV_EpsilonT
        '
        resources.ApplyResources(Me.tbPV_EpsilonT, "tbPV_EpsilonT")
        Me.tbPV_EpsilonT.Name = "tbPV_EpsilonT"
        '
        'tbPV_DampingFactor
        '
        resources.ApplyResources(Me.tbPV_DampingFactor, "tbPV_DampingFactor")
        Me.tbPV_DampingFactor.Name = "tbPV_DampingFactor"
        '
        'chkValidateEqCalc
        '
        resources.ApplyResources(Me.chkValidateEqCalc, "chkValidateEqCalc")
        Me.chkValidateEqCalc.Name = "chkValidateEqCalc"
        '
        'Label25
        '
        resources.ApplyResources(Me.Label25, "Label25")
        Me.Label25.Name = "Label25"
        '
        'tbPV_MaxDT
        '
        resources.ApplyResources(Me.tbPV_MaxDT, "tbPV_MaxDT")
        Me.tbPV_MaxDT.Name = "tbPV_MaxDT"
        '
        'Label24
        '
        resources.ApplyResources(Me.Label24, "Label24")
        Me.Label24.Name = "Label24"
        '
        'TabPageConvPars
        '
        resources.ApplyResources(Me.TabPageConvPars, "TabPageConvPars")
        Me.TabPageConvPars.Controls.Add(Me.GroupBox1)
        Me.TabPageConvPars.Name = "TabPageConvPars"
        Me.TabPageConvPars.UseVisualStyleBackColor = True
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.tbPHExtMaxIt)
        Me.GroupBox1.Controls.Add(Me.tbPHIntMaxIt)
        Me.GroupBox1.Controls.Add(Me.tbPHExtMaxTol)
        Me.GroupBox1.Controls.Add(Me.tbPHintMaxTol)
        Me.GroupBox1.Controls.Add(Me.tbPTExtMaxIt)
        Me.GroupBox1.Controls.Add(Me.tbPTintMaxIt)
        Me.GroupBox1.Controls.Add(Me.tbPTExtTol)
        Me.GroupBox1.Controls.Add(Me.Label5)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Controls.Add(Me.Label7)
        Me.GroupBox1.Controls.Add(Me.Label8)
        Me.GroupBox1.Controls.Add(Me.Label4)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.tbPTIntTol)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'tbPHExtMaxIt
        '
        resources.ApplyResources(Me.tbPHExtMaxIt, "tbPHExtMaxIt")
        Me.tbPHExtMaxIt.Name = "tbPHExtMaxIt"
        '
        'tbPHIntMaxIt
        '
        resources.ApplyResources(Me.tbPHIntMaxIt, "tbPHIntMaxIt")
        Me.tbPHIntMaxIt.Name = "tbPHIntMaxIt"
        '
        'tbPHExtMaxTol
        '
        resources.ApplyResources(Me.tbPHExtMaxTol, "tbPHExtMaxTol")
        Me.tbPHExtMaxTol.Name = "tbPHExtMaxTol"
        '
        'tbPHintMaxTol
        '
        resources.ApplyResources(Me.tbPHintMaxTol, "tbPHintMaxTol")
        Me.tbPHintMaxTol.Name = "tbPHintMaxTol"
        '
        'tbPTExtMaxIt
        '
        resources.ApplyResources(Me.tbPTExtMaxIt, "tbPTExtMaxIt")
        Me.tbPTExtMaxIt.Name = "tbPTExtMaxIt"
        '
        'tbPTintMaxIt
        '
        resources.ApplyResources(Me.tbPTintMaxIt, "tbPTintMaxIt")
        Me.tbPTintMaxIt.Name = "tbPTintMaxIt"
        '
        'tbPTExtTol
        '
        resources.ApplyResources(Me.tbPTExtTol, "tbPTExtTol")
        Me.tbPTExtTol.Name = "tbPTExtTol"
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
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'tbPTIntTol
        '
        resources.ApplyResources(Me.tbPTIntTol, "tbPTIntTol")
        Me.tbPTIntTol.Name = "tbPTIntTol"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'chkDisplayCompWarning
        '
        resources.ApplyResources(Me.chkDisplayCompWarning, "chkDisplayCompWarning")
        Me.chkDisplayCompWarning.Name = "chkDisplayCompWarning"
        '
        'FlashAlgorithmConfig
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FlashAlgorithmConfig"
        Me.ShowIcon = False
        Me.TabControl1.ResumeLayout(False)
        Me.TabPageGeneral.ResumeLayout(False)
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.gpNL.ResumeLayout(False)
        Me.gpNL.PerformLayout()
        Me.TabPageConvPars.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPageConvPars As System.Windows.Forms.TabPage
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents tbPHExtMaxIt As System.Windows.Forms.TextBox
    Friend WithEvents tbPHIntMaxIt As System.Windows.Forms.TextBox
    Friend WithEvents tbPHExtMaxTol As System.Windows.Forms.TextBox
    Friend WithEvents tbPHintMaxTol As System.Windows.Forms.TextBox
    Friend WithEvents tbPTExtMaxIt As System.Windows.Forms.TextBox
    Friend WithEvents tbPTintMaxIt As System.Windows.Forms.TextBox
    Friend WithEvents tbPTExtTol As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbPTIntTol As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents chkFastModeNL As System.Windows.Forms.CheckBox
    Friend WithEvents TabPageGeneral As System.Windows.Forms.TabPage
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents chkValidateEqCalc As System.Windows.Forms.CheckBox
    Friend WithEvents tbPV_DampingFactor As TextBox
    Friend WithEvents tbPV_MaxDT As TextBox
    Friend WithEvents Label24 As Label
    Friend WithEvents Label25 As Label
    Friend WithEvents tbPV_EpsilonT As TextBox
    Friend WithEvents Label26 As Label
    Friend WithEvents tbPH_MaxDT As TextBox
    Friend WithEvents Label29 As Label
    Friend WithEvents tbPT_DampingFactor As TextBox
    Friend WithEvents Label30 As Label
    Friend WithEvents cbFlashType As ComboBox
    Friend WithEvents Label9 As Label
    Public WithEvents chkImmiscible As CheckBox
    Public WithEvents chkHandleSolids As CheckBox
    Public WithEvents chkDoPhaseId As CheckBox
    Public WithEvents chkCalcBubbleDew As CheckBox
    Friend WithEvents cbFlashApproach As ComboBox
    Friend WithEvents Label10 As Label
    Friend WithEvents gpNL As GroupBox
    Friend WithEvents cbExternalSolver As ComboBox
    Friend WithEvents Label11 As Label
    Public WithEvents btnConfigExtSolver As Button
    Friend WithEvents CheckBoxInterpTempPHFlash As CheckBox
    Friend WithEvents chkIdealPVFlash As CheckBox
    Friend WithEvents cbFSMethod As ComboBox
    Friend WithEvents Label12 As Label
    Public WithEvents chkDisplayCompWarning As CheckBox
End Class
