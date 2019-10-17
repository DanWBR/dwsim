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
        Me.tbPV_DampingFactor = New System.Windows.Forms.TextBox()
        Me.tbPV_MaxDT = New System.Windows.Forms.TextBox()
        Me.Label24 = New System.Windows.Forms.Label()
        Me.Label25 = New System.Windows.Forms.Label()
        Me.tbPV_EpsilonT = New System.Windows.Forms.TextBox()
        Me.Label26 = New System.Windows.Forms.Label()
        Me.chkFastModeNL = New System.Windows.Forms.CheckBox()
        Me.TabPageIO = New System.Windows.Forms.TabPage()
        Me.GroupBox5 = New System.Windows.Forms.GroupBox()
        Me.chkUseBroydenIO = New System.Windows.Forms.CheckBox()
        Me.TabPageGM = New System.Windows.Forms.TabPage()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.cbMinMethodGM = New System.Windows.Forms.ComboBox()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.TabPageST = New System.Windows.Forms.TabPage()
        Me.GroupBox10 = New System.Windows.Forms.GroupBox()
        Me.NumericUpDown1 = New System.Windows.Forms.NumericUpDown()
        Me.Label27 = New System.Windows.Forms.Label()
        Me.GroupBox9 = New System.Windows.Forms.GroupBox()
        Me.Label28 = New System.Windows.Forms.Label()
        Me.chkForcePT3P = New System.Windows.Forms.CheckBox()
        Me.TabPageCOES = New System.Windows.Forms.TabPage()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.tbSelectedES = New System.Windows.Forms.TextBox()
        Me.btnSearch = New System.Windows.Forms.Button()
        Me.lblAbout2 = New System.Windows.Forms.Label()
        Me.lblDesc2 = New System.Windows.Forms.Label()
        Me.lblAuthorURL2 = New System.Windows.Forms.LinkLabel()
        Me.lblVersion2 = New System.Windows.Forms.Label()
        Me.lblName2 = New System.Windows.Forms.Label()
        Me.Label15 = New System.Windows.Forms.Label()
        Me.Label16 = New System.Windows.Forms.Label()
        Me.Label17 = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.btnEditThermoServer = New System.Windows.Forms.Button()
        Me.TabPageIM = New System.Windows.Forms.TabPage()
        Me.GroupBox7 = New System.Windows.Forms.GroupBox()
        Me.cbImmiscible = New System.Windows.Forms.ComboBox()
        Me.Label10 = New System.Windows.Forms.Label()
        Me.TabPageUD = New System.Windows.Forms.TabPage()
        Me.GroupBox8 = New System.Windows.Forms.GroupBox()
        Me.btnTestTVFFlash = New System.Windows.Forms.Button()
        Me.btnTestPVFFlash = New System.Windows.Forms.Button()
        Me.btnTestPSFlash = New System.Windows.Forms.Button()
        Me.btnTestPHFlash = New System.Windows.Forms.Button()
        Me.btnTestPTFlash = New System.Windows.Forms.Button()
        Me.lblTVFFlash = New System.Windows.Forms.Label()
        Me.PBTVFFlash = New System.Windows.Forms.PictureBox()
        Me.lblPVFFlash = New System.Windows.Forms.Label()
        Me.PBPVFFlash = New System.Windows.Forms.PictureBox()
        Me.lblPSFlash = New System.Windows.Forms.Label()
        Me.PBPSFlash = New System.Windows.Forms.PictureBox()
        Me.lblPHFlash = New System.Windows.Forms.Label()
        Me.PBPHFlash = New System.Windows.Forms.PictureBox()
        Me.lblPTFlash = New System.Windows.Forms.Label()
        Me.PBPTFlash = New System.Windows.Forms.PictureBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.Label14 = New System.Windows.Forms.Label()
        Me.Label11 = New System.Windows.Forms.Label()
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
        Me.TabPageST.SuspendLayout()
        Me.GroupBox10.SuspendLayout()
        CType(Me.NumericUpDown1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox9.SuspendLayout()
        Me.TabPageCOES.SuspendLayout()
        Me.GroupBox6.SuspendLayout()
        Me.TabPageIM.SuspendLayout()
        Me.GroupBox7.SuspendLayout()
        Me.TabPageUD.SuspendLayout()
        Me.GroupBox8.SuspendLayout()
        CType(Me.PBTVFFlash, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PBPVFFlash, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PBPSFlash, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PBPHFlash, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PBPTFlash, System.ComponentModel.ISupportInitialize).BeginInit()
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
        Me.TabControl1.Controls.Add(Me.TabPageST)
        Me.TabControl1.Controls.Add(Me.TabPageCOES)
        Me.TabControl1.Controls.Add(Me.TabPageIM)
        Me.TabControl1.Controls.Add(Me.TabPageUD)
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
        Me.GroupBox2.Controls.Add(Me.tbPV_DampingFactor)
        Me.GroupBox2.Controls.Add(Me.tbPV_MaxDT)
        Me.GroupBox2.Controls.Add(Me.Label24)
        Me.GroupBox2.Controls.Add(Me.Label25)
        Me.GroupBox2.Controls.Add(Me.tbPV_EpsilonT)
        Me.GroupBox2.Controls.Add(Me.Label26)
        Me.GroupBox2.Controls.Add(Me.chkFastModeNL)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'tbPV_DampingFactor
        '
        resources.ApplyResources(Me.tbPV_DampingFactor, "tbPV_DampingFactor")
        Me.tbPV_DampingFactor.Name = "tbPV_DampingFactor"
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
        'Label25
        '
        resources.ApplyResources(Me.Label25, "Label25")
        Me.Label25.Name = "Label25"
        '
        'tbPV_EpsilonT
        '
        resources.ApplyResources(Me.tbPV_EpsilonT, "tbPV_EpsilonT")
        Me.tbPV_EpsilonT.Name = "tbPV_EpsilonT"
        '
        'Label26
        '
        resources.ApplyResources(Me.Label26, "Label26")
        Me.Label26.Name = "Label26"
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
        'TabPageST
        '
        resources.ApplyResources(Me.TabPageST, "TabPageST")
        Me.TabPageST.Controls.Add(Me.GroupBox10)
        Me.TabPageST.Controls.Add(Me.GroupBox9)
        Me.TabPageST.Name = "TabPageST"
        Me.TabPageST.UseVisualStyleBackColor = True
        '
        'GroupBox10
        '
        resources.ApplyResources(Me.GroupBox10, "GroupBox10")
        Me.GroupBox10.Controls.Add(Me.NumericUpDown1)
        Me.GroupBox10.Controls.Add(Me.Label27)
        Me.GroupBox10.Name = "GroupBox10"
        Me.GroupBox10.TabStop = False
        '
        'NumericUpDown1
        '
        resources.ApplyResources(Me.NumericUpDown1, "NumericUpDown1")
        Me.NumericUpDown1.Minimum = New Decimal(New Integer() {5, 0, 0, 0})
        Me.NumericUpDown1.Name = "NumericUpDown1"
        Me.NumericUpDown1.Value = New Decimal(New Integer() {5, 0, 0, 0})
        '
        'Label27
        '
        resources.ApplyResources(Me.Label27, "Label27")
        Me.Label27.Name = "Label27"
        '
        'GroupBox9
        '
        resources.ApplyResources(Me.GroupBox9, "GroupBox9")
        Me.GroupBox9.Controls.Add(Me.Label28)
        Me.GroupBox9.Controls.Add(Me.chkForcePT3P)
        Me.GroupBox9.Name = "GroupBox9"
        Me.GroupBox9.TabStop = False
        '
        'Label28
        '
        resources.ApplyResources(Me.Label28, "Label28")
        Me.Label28.Name = "Label28"
        '
        'chkForcePT3P
        '
        resources.ApplyResources(Me.chkForcePT3P, "chkForcePT3P")
        Me.chkForcePT3P.Name = "chkForcePT3P"
        Me.chkForcePT3P.UseVisualStyleBackColor = True
        '
        'TabPageCOES
        '
        resources.ApplyResources(Me.TabPageCOES, "TabPageCOES")
        Me.TabPageCOES.Controls.Add(Me.GroupBox6)
        Me.TabPageCOES.Name = "TabPageCOES"
        Me.TabPageCOES.UseVisualStyleBackColor = True
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.Button1)
        Me.GroupBox6.Controls.Add(Me.tbSelectedES)
        Me.GroupBox6.Controls.Add(Me.btnSearch)
        Me.GroupBox6.Controls.Add(Me.lblAbout2)
        Me.GroupBox6.Controls.Add(Me.lblDesc2)
        Me.GroupBox6.Controls.Add(Me.lblAuthorURL2)
        Me.GroupBox6.Controls.Add(Me.lblVersion2)
        Me.GroupBox6.Controls.Add(Me.lblName2)
        Me.GroupBox6.Controls.Add(Me.Label15)
        Me.GroupBox6.Controls.Add(Me.Label16)
        Me.GroupBox6.Controls.Add(Me.Label17)
        Me.GroupBox6.Controls.Add(Me.Label18)
        Me.GroupBox6.Controls.Add(Me.Label19)
        Me.GroupBox6.Controls.Add(Me.Label20)
        Me.GroupBox6.Controls.Add(Me.btnEditThermoServer)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'tbSelectedES
        '
        resources.ApplyResources(Me.tbSelectedES, "tbSelectedES")
        Me.tbSelectedES.Name = "tbSelectedES"
        Me.tbSelectedES.ReadOnly = True
        '
        'btnSearch
        '
        resources.ApplyResources(Me.btnSearch, "btnSearch")
        Me.btnSearch.Name = "btnSearch"
        Me.btnSearch.UseVisualStyleBackColor = True
        '
        'lblAbout2
        '
        resources.ApplyResources(Me.lblAbout2, "lblAbout2")
        Me.lblAbout2.Name = "lblAbout2"
        '
        'lblDesc2
        '
        resources.ApplyResources(Me.lblDesc2, "lblDesc2")
        Me.lblDesc2.Name = "lblDesc2"
        '
        'lblAuthorURL2
        '
        resources.ApplyResources(Me.lblAuthorURL2, "lblAuthorURL2")
        Me.lblAuthorURL2.Name = "lblAuthorURL2"
        '
        'lblVersion2
        '
        resources.ApplyResources(Me.lblVersion2, "lblVersion2")
        Me.lblVersion2.Name = "lblVersion2"
        '
        'lblName2
        '
        resources.ApplyResources(Me.lblName2, "lblName2")
        Me.lblName2.Name = "lblName2"
        '
        'Label15
        '
        resources.ApplyResources(Me.Label15, "Label15")
        Me.Label15.Name = "Label15"
        '
        'Label16
        '
        resources.ApplyResources(Me.Label16, "Label16")
        Me.Label16.Name = "Label16"
        '
        'Label17
        '
        resources.ApplyResources(Me.Label17, "Label17")
        Me.Label17.Name = "Label17"
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        '
        'btnEditThermoServer
        '
        resources.ApplyResources(Me.btnEditThermoServer, "btnEditThermoServer")
        Me.btnEditThermoServer.Name = "btnEditThermoServer"
        Me.btnEditThermoServer.UseVisualStyleBackColor = True
        '
        'TabPageIM
        '
        resources.ApplyResources(Me.TabPageIM, "TabPageIM")
        Me.TabPageIM.Controls.Add(Me.GroupBox7)
        Me.TabPageIM.Name = "TabPageIM"
        Me.TabPageIM.UseVisualStyleBackColor = True
        '
        'GroupBox7
        '
        resources.ApplyResources(Me.GroupBox7, "GroupBox7")
        Me.GroupBox7.Controls.Add(Me.cbImmiscible)
        Me.GroupBox7.Controls.Add(Me.Label10)
        Me.GroupBox7.Name = "GroupBox7"
        Me.GroupBox7.TabStop = False
        '
        'cbImmiscible
        '
        resources.ApplyResources(Me.cbImmiscible, "cbImmiscible")
        Me.cbImmiscible.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbImmiscible.FormattingEnabled = True
        Me.cbImmiscible.Items.AddRange(New Object() {resources.GetString("cbImmiscible.Items"), resources.GetString("cbImmiscible.Items1"), resources.GetString("cbImmiscible.Items2"), resources.GetString("cbImmiscible.Items3"), resources.GetString("cbImmiscible.Items4"), resources.GetString("cbImmiscible.Items5"), resources.GetString("cbImmiscible.Items6"), resources.GetString("cbImmiscible.Items7"), resources.GetString("cbImmiscible.Items8"), resources.GetString("cbImmiscible.Items9"), resources.GetString("cbImmiscible.Items10")})
        Me.cbImmiscible.Name = "cbImmiscible"
        '
        'Label10
        '
        resources.ApplyResources(Me.Label10, "Label10")
        Me.Label10.Name = "Label10"
        '
        'TabPageUD
        '
        resources.ApplyResources(Me.TabPageUD, "TabPageUD")
        Me.TabPageUD.Controls.Add(Me.GroupBox8)
        Me.TabPageUD.Name = "TabPageUD"
        Me.TabPageUD.UseVisualStyleBackColor = True
        '
        'GroupBox8
        '
        resources.ApplyResources(Me.GroupBox8, "GroupBox8")
        Me.GroupBox8.Controls.Add(Me.btnTestTVFFlash)
        Me.GroupBox8.Controls.Add(Me.btnTestPVFFlash)
        Me.GroupBox8.Controls.Add(Me.btnTestPSFlash)
        Me.GroupBox8.Controls.Add(Me.btnTestPHFlash)
        Me.GroupBox8.Controls.Add(Me.btnTestPTFlash)
        Me.GroupBox8.Controls.Add(Me.lblTVFFlash)
        Me.GroupBox8.Controls.Add(Me.PBTVFFlash)
        Me.GroupBox8.Controls.Add(Me.lblPVFFlash)
        Me.GroupBox8.Controls.Add(Me.PBPVFFlash)
        Me.GroupBox8.Controls.Add(Me.lblPSFlash)
        Me.GroupBox8.Controls.Add(Me.PBPSFlash)
        Me.GroupBox8.Controls.Add(Me.lblPHFlash)
        Me.GroupBox8.Controls.Add(Me.PBPHFlash)
        Me.GroupBox8.Controls.Add(Me.lblPTFlash)
        Me.GroupBox8.Controls.Add(Me.PBPTFlash)
        Me.GroupBox8.Controls.Add(Me.Label23)
        Me.GroupBox8.Controls.Add(Me.Label22)
        Me.GroupBox8.Controls.Add(Me.Label21)
        Me.GroupBox8.Controls.Add(Me.Label14)
        Me.GroupBox8.Controls.Add(Me.Label11)
        Me.GroupBox8.Name = "GroupBox8"
        Me.GroupBox8.TabStop = False
        '
        'btnTestTVFFlash
        '
        resources.ApplyResources(Me.btnTestTVFFlash, "btnTestTVFFlash")
        Me.btnTestTVFFlash.Name = "btnTestTVFFlash"
        Me.btnTestTVFFlash.UseVisualStyleBackColor = True
        '
        'btnTestPVFFlash
        '
        resources.ApplyResources(Me.btnTestPVFFlash, "btnTestPVFFlash")
        Me.btnTestPVFFlash.Name = "btnTestPVFFlash"
        Me.btnTestPVFFlash.UseVisualStyleBackColor = True
        '
        'btnTestPSFlash
        '
        resources.ApplyResources(Me.btnTestPSFlash, "btnTestPSFlash")
        Me.btnTestPSFlash.Name = "btnTestPSFlash"
        Me.btnTestPSFlash.UseVisualStyleBackColor = True
        '
        'btnTestPHFlash
        '
        resources.ApplyResources(Me.btnTestPHFlash, "btnTestPHFlash")
        Me.btnTestPHFlash.Name = "btnTestPHFlash"
        Me.btnTestPHFlash.UseVisualStyleBackColor = True
        '
        'btnTestPTFlash
        '
        resources.ApplyResources(Me.btnTestPTFlash, "btnTestPTFlash")
        Me.btnTestPTFlash.Name = "btnTestPTFlash"
        Me.btnTestPTFlash.UseVisualStyleBackColor = True
        '
        'lblTVFFlash
        '
        resources.ApplyResources(Me.lblTVFFlash, "lblTVFFlash")
        Me.lblTVFFlash.Name = "lblTVFFlash"
        '
        'PBTVFFlash
        '
        resources.ApplyResources(Me.PBTVFFlash, "PBTVFFlash")
        Me.PBTVFFlash.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cross
        Me.PBTVFFlash.Name = "PBTVFFlash"
        Me.PBTVFFlash.TabStop = False
        '
        'lblPVFFlash
        '
        resources.ApplyResources(Me.lblPVFFlash, "lblPVFFlash")
        Me.lblPVFFlash.Name = "lblPVFFlash"
        '
        'PBPVFFlash
        '
        resources.ApplyResources(Me.PBPVFFlash, "PBPVFFlash")
        Me.PBPVFFlash.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cross
        Me.PBPVFFlash.Name = "PBPVFFlash"
        Me.PBPVFFlash.TabStop = False
        '
        'lblPSFlash
        '
        resources.ApplyResources(Me.lblPSFlash, "lblPSFlash")
        Me.lblPSFlash.Name = "lblPSFlash"
        '
        'PBPSFlash
        '
        resources.ApplyResources(Me.PBPSFlash, "PBPSFlash")
        Me.PBPSFlash.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cross
        Me.PBPSFlash.Name = "PBPSFlash"
        Me.PBPSFlash.TabStop = False
        '
        'lblPHFlash
        '
        resources.ApplyResources(Me.lblPHFlash, "lblPHFlash")
        Me.lblPHFlash.Name = "lblPHFlash"
        '
        'PBPHFlash
        '
        resources.ApplyResources(Me.PBPHFlash, "PBPHFlash")
        Me.PBPHFlash.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cross
        Me.PBPHFlash.Name = "PBPHFlash"
        Me.PBPHFlash.TabStop = False
        '
        'lblPTFlash
        '
        resources.ApplyResources(Me.lblPTFlash, "lblPTFlash")
        Me.lblPTFlash.Name = "lblPTFlash"
        '
        'PBPTFlash
        '
        resources.ApplyResources(Me.PBPTFlash, "PBPTFlash")
        Me.PBPTFlash.Image = Global.DWSIM.Thermodynamics.My.Resources.Resources.cross
        Me.PBPTFlash.Name = "PBPTFlash"
        Me.PBPTFlash.TabStop = False
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        '
        'Label22
        '
        resources.ApplyResources(Me.Label22, "Label22")
        Me.Label22.Name = "Label22"
        '
        'Label21
        '
        resources.ApplyResources(Me.Label21, "Label21")
        Me.Label21.Name = "Label21"
        '
        'Label14
        '
        resources.ApplyResources(Me.Label14, "Label14")
        Me.Label14.Name = "Label14"
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
        '
        'FlashAlgorithmConfig
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FlashAlgorithmConfig"
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
        Me.TabPageST.ResumeLayout(False)
        Me.GroupBox10.ResumeLayout(False)
        Me.GroupBox10.PerformLayout()
        CType(Me.NumericUpDown1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox9.ResumeLayout(False)
        Me.GroupBox9.PerformLayout()
        Me.TabPageCOES.ResumeLayout(False)
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        Me.TabPageIM.ResumeLayout(False)
        Me.GroupBox7.ResumeLayout(False)
        Me.GroupBox7.PerformLayout()
        Me.TabPageUD.ResumeLayout(False)
        Me.GroupBox8.ResumeLayout(False)
        Me.GroupBox8.PerformLayout()
        CType(Me.PBTVFFlash, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PBPVFFlash, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PBPSFlash, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PBPHFlash, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PBPTFlash, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPageConvPars As System.Windows.Forms.TabPage
    Friend WithEvents TabPageNL As System.Windows.Forms.TabPage
    Friend WithEvents TabPageIO As System.Windows.Forms.TabPage
    Friend WithEvents TabPageGM As System.Windows.Forms.TabPage
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
    Friend WithEvents TabPageCOES As System.Windows.Forms.TabPage
    Friend WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Friend WithEvents lblAbout2 As System.Windows.Forms.Label
    Friend WithEvents lblDesc2 As System.Windows.Forms.Label
    Friend WithEvents lblAuthorURL2 As System.Windows.Forms.LinkLabel
    Friend WithEvents lblVersion2 As System.Windows.Forms.Label
    Friend WithEvents lblName2 As System.Windows.Forms.Label
    Friend WithEvents Label15 As System.Windows.Forms.Label
    Friend WithEvents Label16 As System.Windows.Forms.Label
    Friend WithEvents Label17 As System.Windows.Forms.Label
    Friend WithEvents Label18 As System.Windows.Forms.Label
    Friend WithEvents Label19 As System.Windows.Forms.Label
    Friend WithEvents Label20 As System.Windows.Forms.Label
    Friend WithEvents btnEditThermoServer As System.Windows.Forms.Button
    Friend WithEvents tbSelectedES As System.Windows.Forms.TextBox
    Friend WithEvents btnSearch As System.Windows.Forms.Button
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents TabPageIM As TabPage
    Friend WithEvents GroupBox7 As GroupBox
    Friend WithEvents cbImmiscible As ComboBox
    Friend WithEvents Label10 As Label
    Friend WithEvents TabPageUD As TabPage
    Friend WithEvents GroupBox8 As GroupBox
    Friend WithEvents lblTVFFlash As Label
    Friend WithEvents PBTVFFlash As PictureBox
    Friend WithEvents lblPVFFlash As Label
    Friend WithEvents PBPVFFlash As PictureBox
    Friend WithEvents lblPSFlash As Label
    Friend WithEvents PBPSFlash As PictureBox
    Friend WithEvents lblPHFlash As Label
    Friend WithEvents PBPHFlash As PictureBox
    Friend WithEvents lblPTFlash As Label
    Friend WithEvents PBPTFlash As PictureBox
    Friend WithEvents Label23 As Label
    Friend WithEvents Label22 As Label
    Friend WithEvents Label21 As Label
    Friend WithEvents Label14 As Label
    Friend WithEvents Label11 As Label
    Friend WithEvents btnTestTVFFlash As Button
    Friend WithEvents btnTestPVFFlash As Button
    Friend WithEvents btnTestPSFlash As Button
    Friend WithEvents btnTestPHFlash As Button
    Friend WithEvents btnTestPTFlash As Button
    Friend WithEvents tbPV_DampingFactor As TextBox
    Friend WithEvents tbPV_MaxDT As TextBox
    Friend WithEvents Label24 As Label
    Friend WithEvents Label25 As Label
    Friend WithEvents tbPV_EpsilonT As TextBox
    Friend WithEvents Label26 As Label
    Friend WithEvents TabPageST As TabPage
    Friend WithEvents NumericUpDown1 As NumericUpDown
    Friend WithEvents Label27 As Label
    Friend WithEvents GroupBox10 As GroupBox
    Friend WithEvents GroupBox9 As GroupBox
    Friend WithEvents chkForcePT3P As CheckBox
    Friend WithEvents Label28 As Label
End Class
