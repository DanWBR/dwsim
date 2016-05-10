<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FlashAlgorithmConfig
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

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
        Me.chkReplaceFlashPT = New System.Windows.Forms.CheckBox()
        Me.Label13 = New System.Windows.Forms.Label()
        Me.chkDoPhaseId = New System.Windows.Forms.CheckBox()
        Me.tbFlashValidationTolerance = New System.Windows.Forms.TextBox()
        Me.chkCalcBubbleDew = New System.Windows.Forms.CheckBox()
        Me.chkValidateEqCalc = New System.Windows.Forms.CheckBox()
        Me.Label12 = New System.Windows.Forms.Label()
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
        Me.TabPageNL = New System.Windows.Forms.TabPage()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.chkFastModeNL = New System.Windows.Forms.CheckBox()
        Me.TabPageIO = New System.Windows.Forms.TabPage()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.chkUseBroydenIO = New System.Windows.Forms.CheckBox()
        Me.TabPageGM = New System.Windows.Forms.TabPage()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.cbMinMethodGM = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.TabPageVLLE = New System.Windows.Forms.TabPage()
        Me.GroupBox11 = New System.Windows.Forms.GroupBox()
        Me.rbHigh = New System.Windows.Forms.RadioButton()
        Me.rbMedium = New System.Windows.Forms.RadioButton()
        Me.rbLow = New System.Windows.Forms.RadioButton()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.lvKeyComp = New System.Windows.Forms.ListView()
        Me.TabControl1.SuspendLayout()
        Me.TabPageGeneral.SuspendLayout()
        Me.GroupBox4.SuspendLayout()
        Me.TabPageConvPars.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.TabPageNL.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.TabPageIO.SuspendLayout()
        Me.GroupBox5.SuspendLayout()
        Me.TabPageGM.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.TabPageVLLE.SuspendLayout()
        Me.GroupBox11.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPageGeneral)
        Me.TabControl1.Controls.Add(Me.TabPageConvPars)
        Me.TabControl1.Controls.Add(Me.TabPageNL)
        Me.TabControl1.Controls.Add(Me.TabPageIO)
        Me.TabControl1.Controls.Add(Me.TabPageGM)
        Me.TabControl1.Controls.Add(Me.TabPageVLLE)
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
        Me.GroupBox4.Controls.Add(Me.chkReplaceFlashPT)
        Me.GroupBox4.Controls.Add(Me.Label13)
        Me.GroupBox4.Controls.Add(Me.chkDoPhaseId)
        Me.GroupBox4.Controls.Add(Me.tbFlashValidationTolerance)
        Me.GroupBox4.Controls.Add(Me.chkCalcBubbleDew)
        Me.GroupBox4.Controls.Add(Me.chkValidateEqCalc)
        Me.GroupBox4.Controls.Add(Me.Label12)
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'chkReplaceFlashPT
        '
        resources.ApplyResources(Me.chkReplaceFlashPT, "chkReplaceFlashPT")
        Me.chkReplaceFlashPT.Name = "chkReplaceFlashPT"
        '
        'Label13
        '
        resources.ApplyResources(Me.Label13, "Label13")
        Me.Label13.Name = "Label13"
        '
        'chkDoPhaseId
        '
        resources.ApplyResources(Me.chkDoPhaseId, "chkDoPhaseId")
        Me.chkDoPhaseId.Name = "chkDoPhaseId"
        '
        'tbFlashValidationTolerance
        '
        resources.ApplyResources(Me.tbFlashValidationTolerance, "tbFlashValidationTolerance")
        Me.tbFlashValidationTolerance.Name = "tbFlashValidationTolerance"
        '
        'chkCalcBubbleDew
        '
        resources.ApplyResources(Me.chkCalcBubbleDew, "chkCalcBubbleDew")
        Me.chkCalcBubbleDew.Name = "chkCalcBubbleDew"
        '
        'chkValidateEqCalc
        '
        resources.ApplyResources(Me.chkValidateEqCalc, "chkValidateEqCalc")
        Me.chkValidateEqCalc.Name = "chkValidateEqCalc"
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
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
        'TabPageNL
        '
        resources.ApplyResources(Me.TabPageNL, "TabPageNL")
        Me.TabPageNL.Controls.Add(Me.GroupBox2)
        Me.TabPageNL.Name = "TabPageNL"
        Me.TabPageNL.UseVisualStyleBackColor = True
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.chkFastModeNL)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'chkFastModeNL
        '
        resources.ApplyResources(Me.chkFastModeNL, "chkFastModeNL")
        Me.chkFastModeNL.Name = "chkFastModeNL"
        Me.chkFastModeNL.UseVisualStyleBackColor = True
        '
        'TabPageIO
        '
        resources.ApplyResources(Me.TabPageIO, "TabPageIO")
        Me.TabPageIO.Controls.Add(Me.GroupBox5)
        Me.TabPageIO.Name = "TabPageIO"
        Me.TabPageIO.UseVisualStyleBackColor = True
        '
        'GroupBox5
        '
        resources.ApplyResources(Me.GroupBox5, "GroupBox5")
        Me.GroupBox5.Controls.Add(Me.chkUseBroydenIO)
        Me.GroupBox5.Name = "GroupBox5"
        Me.GroupBox5.TabStop = False
        '
        'chkUseBroydenIO
        '
        resources.ApplyResources(Me.chkUseBroydenIO, "chkUseBroydenIO")
        Me.chkUseBroydenIO.Name = "chkUseBroydenIO"
        Me.chkUseBroydenIO.UseVisualStyleBackColor = True
        '
        'TabPageGM
        '
        resources.ApplyResources(Me.TabPageGM, "TabPageGM")
        Me.TabPageGM.Controls.Add(Me.GroupBox3)
        Me.TabPageGM.Name = "TabPageGM"
        Me.TabPageGM.UseVisualStyleBackColor = True
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.cbMinMethodGM)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'cbMinMethodGM
        '
        resources.ApplyResources(Me.cbMinMethodGM, "cbMinMethodGM")
        Me.cbMinMethodGM.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbMinMethodGM.FormattingEnabled = True
        Me.cbMinMethodGM.Items.AddRange(New Object() {resources.GetString("cbMinMethodGM.Items"), resources.GetString("cbMinMethodGM.Items1"), resources.GetString("cbMinMethodGM.Items2"), resources.GetString("cbMinMethodGM.Items3"), resources.GetString("cbMinMethodGM.Items4"), resources.GetString("cbMinMethodGM.Items5"), resources.GetString("cbMinMethodGM.Items6"), resources.GetString("cbMinMethodGM.Items7"), resources.GetString("cbMinMethodGM.Items8"), resources.GetString("cbMinMethodGM.Items9"), resources.GetString("cbMinMethodGM.Items10")})
        Me.cbMinMethodGM.Name = "cbMinMethodGM"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'TabPageVLLE
        '
        resources.ApplyResources(Me.TabPageVLLE, "TabPageVLLE")
        Me.TabPageVLLE.Controls.Add(Me.GroupBox11)
        Me.TabPageVLLE.Name = "TabPageVLLE"
        Me.TabPageVLLE.UseVisualStyleBackColor = True
        '
        'GroupBox11
        '
        resources.ApplyResources(Me.GroupBox11, "GroupBox11")
        Me.GroupBox11.Controls.Add(Me.rbHigh)
        Me.GroupBox11.Controls.Add(Me.rbMedium)
        Me.GroupBox11.Controls.Add(Me.rbLow)
        Me.GroupBox11.Controls.Add(Me.Label10)
        Me.GroupBox11.Controls.Add(Me.Label11)
        Me.GroupBox11.Controls.Add(Me.lvKeyComp)
        Me.GroupBox11.Name = "GroupBox11"
        Me.GroupBox11.TabStop = False
        '
        'rbHigh
        '
        resources.ApplyResources(Me.rbHigh, "rbHigh")
        Me.rbHigh.Name = "rbHigh"
        Me.rbHigh.TabStop = True
        Me.rbHigh.UseVisualStyleBackColor = True
        '
        'rbMedium
        '
        resources.ApplyResources(Me.rbMedium, "rbMedium")
        Me.rbMedium.Name = "rbMedium"
        Me.rbMedium.TabStop = True
        Me.rbMedium.UseVisualStyleBackColor = True
        '
        'rbLow
        '
        resources.ApplyResources(Me.rbLow, "rbLow")
        Me.rbLow.Name = "rbLow"
        Me.rbLow.TabStop = True
        Me.rbLow.UseVisualStyleBackColor = True
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'lvKeyComp
        '
        resources.ApplyResources(Me.lvKeyComp, "lvKeyComp")
        Me.lvKeyComp.CheckBoxes = True
        Me.lvKeyComp.Name = "lvKeyComp"
        Me.lvKeyComp.UseCompatibleStateImageBehavior = False
        Me.lvKeyComp.View = System.Windows.Forms.View.List
        '
        'FlashAlgorithmConfig
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FlashAlgorithmConfig"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float
        Me.ShowIcon = False
        Me.TabControl1.ResumeLayout(False)
        Me.TabPageGeneral.ResumeLayout(False)
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.TabPageConvPars.ResumeLayout(False)
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.TabPageNL.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.TabPageIO.ResumeLayout(False)
        Me.GroupBox5.ResumeLayout(False)
        Me.GroupBox5.PerformLayout()
        Me.TabPageGM.ResumeLayout(False)
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.TabPageVLLE.ResumeLayout(False)
        Me.GroupBox11.ResumeLayout(False)
        Me.GroupBox11.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPageConvPars As System.Windows.Forms.TabPage
    Friend WithEvents TabPageNL As System.Windows.Forms.TabPage
    Friend WithEvents TabPageIO As System.Windows.Forms.TabPage
    Friend WithEvents TabPageGM As System.Windows.Forms.TabPage
    Friend WithEvents TabPageVLLE As System.Windows.Forms.TabPage
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
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Friend WithEvents cbMinMethodGM As System.Windows.Forms.ComboBox
    Friend WithEvents Label9 As System.Windows.Forms.Label
    Friend WithEvents GroupBox11 As System.Windows.Forms.GroupBox
    Friend WithEvents rbHigh As System.Windows.Forms.RadioButton
    Friend WithEvents rbMedium As System.Windows.Forms.RadioButton
    Friend WithEvents rbLow As System.Windows.Forms.RadioButton
    Friend WithEvents Label10 As System.Windows.Forms.Label
    Friend WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents lvKeyComp As System.Windows.Forms.ListView
    Friend WithEvents TabPageGeneral As System.Windows.Forms.TabPage
    Friend WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents chkReplaceFlashPT As System.Windows.Forms.CheckBox
    Friend WithEvents Label13 As System.Windows.Forms.Label
    Public WithEvents chkDoPhaseId As System.Windows.Forms.CheckBox
    Friend WithEvents tbFlashValidationTolerance As System.Windows.Forms.TextBox
    Public WithEvents chkCalcBubbleDew As System.Windows.Forms.CheckBox
    Public WithEvents chkValidateEqCalc As System.Windows.Forms.CheckBox
    Friend WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents GroupBox5 As System.Windows.Forms.GroupBox
    Friend WithEvents chkUseBroydenIO As System.Windows.Forms.CheckBox
End Class
