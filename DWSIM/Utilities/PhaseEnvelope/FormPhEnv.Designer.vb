<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormPhEnv

    Inherits System.Windows.Forms.UserControl

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormPhEnv))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.chkImmiscibleWater = New System.Windows.Forms.CheckBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.chkQualityLine = New System.Windows.Forms.CheckBox()
        Me.chkStabCurve = New System.Windows.Forms.CheckBox()
        Me.chkHydVapOnly = New System.Windows.Forms.CheckBox()
        Me.chkOp = New System.Windows.Forms.CheckBox()
        Me.cbhydmodel = New System.Windows.Forms.ComboBox()
        Me.tbQuality = New System.Windows.Forms.NumericUpDown()
        Me.chkpip = New System.Windows.Forms.CheckBox()
        Me.chkhyd = New System.Windows.Forms.CheckBox()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.PanelBub = New System.Windows.Forms.Panel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.lblBubTmax = New System.Windows.Forms.Label()
        Me.chkBubLiqInstability = New System.Windows.Forms.CheckBox()
        Me.tbBubTmax = New System.Windows.Forms.TextBox()
        Me.rbBubPVF = New System.Windows.Forms.RadioButton()
        Me.Label12 = New System.Windows.Forms.Label()
        Me.rbBubTVF = New System.Windows.Forms.RadioButton()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.lbBubDT = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.lbBubDP = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.lbBubT0 = New System.Windows.Forms.Label()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.lbBubP0 = New System.Windows.Forms.Label()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.tbBubDT = New System.Windows.Forms.TextBox()
        Me.tbBubP0 = New System.Windows.Forms.TextBox()
        Me.tbBubDP = New System.Windows.Forms.TextBox()
        Me.tbBubT0 = New System.Windows.Forms.TextBox()
        Me.tbBubMaxPoints = New System.Windows.Forms.TextBox()
        Me.chkControlBubInit = New System.Windows.Forms.CheckBox()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.PanelDew = New System.Windows.Forms.Panel()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.lblDewTmax = New System.Windows.Forms.Label()
        Me.rbDewPVF = New System.Windows.Forms.RadioButton()
        Me.tbDewTmax = New System.Windows.Forms.TextBox()
        Me.rbDewTVF = New System.Windows.Forms.RadioButton()
        Me.Label11 = New System.Windows.Forms.Label()
        Me.Label22 = New System.Windows.Forms.Label()
        Me.Label21 = New System.Windows.Forms.Label()
        Me.lbDewDT = New System.Windows.Forms.Label()
        Me.Label20 = New System.Windows.Forms.Label()
        Me.lbDewDP = New System.Windows.Forms.Label()
        Me.Label19 = New System.Windows.Forms.Label()
        Me.lbDewT0 = New System.Windows.Forms.Label()
        Me.Label18 = New System.Windows.Forms.Label()
        Me.lbDewP0 = New System.Windows.Forms.Label()
        Me.tbDewP0 = New System.Windows.Forms.TextBox()
        Me.tbDewDT = New System.Windows.Forms.TextBox()
        Me.tbDewT0 = New System.Windows.Forms.TextBox()
        Me.tbDewDP = New System.Windows.Forms.TextBox()
        Me.tbDewMaxPoints = New System.Windows.Forms.TextBox()
        Me.chkControlDewInit = New System.Windows.Forms.CheckBox()
        Me.PanelCalc = New System.Windows.Forms.Panel()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.LabelStatus = New System.Windows.Forms.Label()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.ComboBox1 = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.GraphControl = New ZedGraph.ZedGraphControl()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.Grid1 = New System.Windows.Forms.DataGridView()
        Me.BackgroundWorker1 = New System.ComponentModel.BackgroundWorker()
        Me.dckMenu = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.FloatToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockLeftToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockRightToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockTopToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockBottomToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockLeftAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockRightAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockTopAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DockBottomAutoHideToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DocumentToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GroupBox6.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        CType(Me.tbQuality, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.PanelBub.SuspendLayout()
        Me.TabPage3.SuspendLayout()
        Me.PanelDew.SuspendLayout()
        Me.PanelCalc.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem1.SuspendLayout()
        Me.FaTabStripItem2.SuspendLayout()
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.TabControl1)
        Me.GroupBox6.Controls.Add(Me.PanelCalc)
        Me.GroupBox6.Controls.Add(Me.Button1)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Multiline = True
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.chkImmiscibleWater)
        Me.TabPage1.Controls.Add(Me.Label3)
        Me.TabPage1.Controls.Add(Me.Label2)
        Me.TabPage1.Controls.Add(Me.chkQualityLine)
        Me.TabPage1.Controls.Add(Me.chkStabCurve)
        Me.TabPage1.Controls.Add(Me.chkHydVapOnly)
        Me.TabPage1.Controls.Add(Me.chkOp)
        Me.TabPage1.Controls.Add(Me.cbhydmodel)
        Me.TabPage1.Controls.Add(Me.tbQuality)
        Me.TabPage1.Controls.Add(Me.chkpip)
        Me.TabPage1.Controls.Add(Me.chkhyd)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'chkImmiscibleWater
        '
        resources.ApplyResources(Me.chkImmiscibleWater, "chkImmiscibleWater")
        Me.chkImmiscibleWater.Name = "chkImmiscibleWater"
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
        'chkQualityLine
        '
        resources.ApplyResources(Me.chkQualityLine, "chkQualityLine")
        Me.chkQualityLine.Name = "chkQualityLine"
        '
        'chkStabCurve
        '
        resources.ApplyResources(Me.chkStabCurve, "chkStabCurve")
        Me.chkStabCurve.Name = "chkStabCurve"
        '
        'chkHydVapOnly
        '
        resources.ApplyResources(Me.chkHydVapOnly, "chkHydVapOnly")
        Me.chkHydVapOnly.Name = "chkHydVapOnly"
        '
        'chkOp
        '
        resources.ApplyResources(Me.chkOp, "chkOp")
        Me.chkOp.Checked = True
        Me.chkOp.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkOp.Name = "chkOp"
        '
        'cbhydmodel
        '
        resources.ApplyResources(Me.cbhydmodel, "cbhydmodel")
        Me.cbhydmodel.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbhydmodel.DropDownWidth = 260
        Me.cbhydmodel.Items.AddRange(New Object() {resources.GetString("cbhydmodel.Items"), resources.GetString("cbhydmodel.Items1"), resources.GetString("cbhydmodel.Items2"), resources.GetString("cbhydmodel.Items3")})
        Me.cbhydmodel.Name = "cbhydmodel"
        '
        'tbQuality
        '
        resources.ApplyResources(Me.tbQuality, "tbQuality")
        Me.tbQuality.DecimalPlaces = 2
        Me.tbQuality.Increment = New Decimal(New Integer() {1, 0, 0, 131072})
        Me.tbQuality.Maximum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.tbQuality.Name = "tbQuality"
        Me.tbQuality.Value = New Decimal(New Integer() {5, 0, 0, 65536})
        '
        'chkpip
        '
        resources.ApplyResources(Me.chkpip, "chkpip")
        Me.chkpip.Name = "chkpip"
        '
        'chkhyd
        '
        resources.ApplyResources(Me.chkhyd, "chkhyd")
        Me.chkhyd.Name = "chkhyd"
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.PanelBub)
        Me.TabPage2.Controls.Add(Me.chkControlBubInit)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'PanelBub
        '
        resources.ApplyResources(Me.PanelBub, "PanelBub")
        Me.PanelBub.Controls.Add(Me.Label4)
        Me.PanelBub.Controls.Add(Me.lblBubTmax)
        Me.PanelBub.Controls.Add(Me.chkBubLiqInstability)
        Me.PanelBub.Controls.Add(Me.tbBubTmax)
        Me.PanelBub.Controls.Add(Me.rbBubPVF)
        Me.PanelBub.Controls.Add(Me.Label12)
        Me.PanelBub.Controls.Add(Me.rbBubTVF)
        Me.PanelBub.Controls.Add(Me.Label5)
        Me.PanelBub.Controls.Add(Me.lbBubDT)
        Me.PanelBub.Controls.Add(Me.Label6)
        Me.PanelBub.Controls.Add(Me.lbBubDP)
        Me.PanelBub.Controls.Add(Me.Label7)
        Me.PanelBub.Controls.Add(Me.lbBubT0)
        Me.PanelBub.Controls.Add(Me.Label8)
        Me.PanelBub.Controls.Add(Me.lbBubP0)
        Me.PanelBub.Controls.Add(Me.Label9)
        Me.PanelBub.Controls.Add(Me.tbBubDT)
        Me.PanelBub.Controls.Add(Me.tbBubP0)
        Me.PanelBub.Controls.Add(Me.tbBubDP)
        Me.PanelBub.Controls.Add(Me.tbBubT0)
        Me.PanelBub.Controls.Add(Me.tbBubMaxPoints)
        Me.PanelBub.Name = "PanelBub"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'lblBubTmax
        '
        resources.ApplyResources(Me.lblBubTmax, "lblBubTmax")
        Me.lblBubTmax.Name = "lblBubTmax"
        '
        'chkBubLiqInstability
        '
        resources.ApplyResources(Me.chkBubLiqInstability, "chkBubLiqInstability")
        Me.chkBubLiqInstability.Name = "chkBubLiqInstability"
        '
        'tbBubTmax
        '
        resources.ApplyResources(Me.tbBubTmax, "tbBubTmax")
        Me.tbBubTmax.Name = "tbBubTmax"
        '
        'rbBubPVF
        '
        resources.ApplyResources(Me.rbBubPVF, "rbBubPVF")
        Me.rbBubPVF.Name = "rbBubPVF"
        Me.rbBubPVF.TabStop = True
        Me.rbBubPVF.UseVisualStyleBackColor = True
        '
        'Label12
        '
        resources.ApplyResources(Me.Label12, "Label12")
        Me.Label12.Name = "Label12"
        '
        'rbBubTVF
        '
        resources.ApplyResources(Me.rbBubTVF, "rbBubTVF")
        Me.rbBubTVF.Name = "rbBubTVF"
        Me.rbBubTVF.TabStop = True
        Me.rbBubTVF.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'lbBubDT
        '
        resources.ApplyResources(Me.lbBubDT, "lbBubDT")
        Me.lbBubDT.Name = "lbBubDT"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'lbBubDP
        '
        resources.ApplyResources(Me.lbBubDP, "lbBubDP")
        Me.lbBubDP.Name = "lbBubDP"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'lbBubT0
        '
        resources.ApplyResources(Me.lbBubT0, "lbBubT0")
        Me.lbBubT0.Name = "lbBubT0"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'lbBubP0
        '
        resources.ApplyResources(Me.lbBubP0, "lbBubP0")
        Me.lbBubP0.Name = "lbBubP0"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'tbBubDT
        '
        resources.ApplyResources(Me.tbBubDT, "tbBubDT")
        Me.tbBubDT.Name = "tbBubDT"
        '
        'tbBubP0
        '
        resources.ApplyResources(Me.tbBubP0, "tbBubP0")
        Me.tbBubP0.Name = "tbBubP0"
        '
        'tbBubDP
        '
        resources.ApplyResources(Me.tbBubDP, "tbBubDP")
        Me.tbBubDP.Name = "tbBubDP"
        '
        'tbBubT0
        '
        resources.ApplyResources(Me.tbBubT0, "tbBubT0")
        Me.tbBubT0.Name = "tbBubT0"
        '
        'tbBubMaxPoints
        '
        resources.ApplyResources(Me.tbBubMaxPoints, "tbBubMaxPoints")
        Me.tbBubMaxPoints.Name = "tbBubMaxPoints"
        '
        'chkControlBubInit
        '
        resources.ApplyResources(Me.chkControlBubInit, "chkControlBubInit")
        Me.chkControlBubInit.Name = "chkControlBubInit"
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Controls.Add(Me.PanelDew)
        Me.TabPage3.Controls.Add(Me.chkControlDewInit)
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'PanelDew
        '
        resources.ApplyResources(Me.PanelDew, "PanelDew")
        Me.PanelDew.Controls.Add(Me.Label23)
        Me.PanelDew.Controls.Add(Me.lblDewTmax)
        Me.PanelDew.Controls.Add(Me.rbDewPVF)
        Me.PanelDew.Controls.Add(Me.tbDewTmax)
        Me.PanelDew.Controls.Add(Me.rbDewTVF)
        Me.PanelDew.Controls.Add(Me.Label11)
        Me.PanelDew.Controls.Add(Me.Label22)
        Me.PanelDew.Controls.Add(Me.Label21)
        Me.PanelDew.Controls.Add(Me.lbDewDT)
        Me.PanelDew.Controls.Add(Me.Label20)
        Me.PanelDew.Controls.Add(Me.lbDewDP)
        Me.PanelDew.Controls.Add(Me.Label19)
        Me.PanelDew.Controls.Add(Me.lbDewT0)
        Me.PanelDew.Controls.Add(Me.Label18)
        Me.PanelDew.Controls.Add(Me.lbDewP0)
        Me.PanelDew.Controls.Add(Me.tbDewP0)
        Me.PanelDew.Controls.Add(Me.tbDewDT)
        Me.PanelDew.Controls.Add(Me.tbDewT0)
        Me.PanelDew.Controls.Add(Me.tbDewDP)
        Me.PanelDew.Controls.Add(Me.tbDewMaxPoints)
        Me.PanelDew.Name = "PanelDew"
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        '
        'lblDewTmax
        '
        resources.ApplyResources(Me.lblDewTmax, "lblDewTmax")
        Me.lblDewTmax.Name = "lblDewTmax"
        '
        'rbDewPVF
        '
        resources.ApplyResources(Me.rbDewPVF, "rbDewPVF")
        Me.rbDewPVF.Name = "rbDewPVF"
        Me.rbDewPVF.TabStop = True
        Me.rbDewPVF.UseVisualStyleBackColor = True
        '
        'tbDewTmax
        '
        resources.ApplyResources(Me.tbDewTmax, "tbDewTmax")
        Me.tbDewTmax.Name = "tbDewTmax"
        '
        'rbDewTVF
        '
        resources.ApplyResources(Me.rbDewTVF, "rbDewTVF")
        Me.rbDewTVF.Name = "rbDewTVF"
        Me.rbDewTVF.TabStop = True
        Me.rbDewTVF.UseVisualStyleBackColor = True
        '
        'Label11
        '
        resources.ApplyResources(Me.Label11, "Label11")
        Me.Label11.Name = "Label11"
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
        'lbDewDT
        '
        resources.ApplyResources(Me.lbDewDT, "lbDewDT")
        Me.lbDewDT.Name = "lbDewDT"
        '
        'Label20
        '
        resources.ApplyResources(Me.Label20, "Label20")
        Me.Label20.Name = "Label20"
        '
        'lbDewDP
        '
        resources.ApplyResources(Me.lbDewDP, "lbDewDP")
        Me.lbDewDP.Name = "lbDewDP"
        '
        'Label19
        '
        resources.ApplyResources(Me.Label19, "Label19")
        Me.Label19.Name = "Label19"
        '
        'lbDewT0
        '
        resources.ApplyResources(Me.lbDewT0, "lbDewT0")
        Me.lbDewT0.Name = "lbDewT0"
        '
        'Label18
        '
        resources.ApplyResources(Me.Label18, "Label18")
        Me.Label18.Name = "Label18"
        '
        'lbDewP0
        '
        resources.ApplyResources(Me.lbDewP0, "lbDewP0")
        Me.lbDewP0.Name = "lbDewP0"
        '
        'tbDewP0
        '
        resources.ApplyResources(Me.tbDewP0, "tbDewP0")
        Me.tbDewP0.Name = "tbDewP0"
        '
        'tbDewDT
        '
        resources.ApplyResources(Me.tbDewDT, "tbDewDT")
        Me.tbDewDT.Name = "tbDewDT"
        '
        'tbDewT0
        '
        resources.ApplyResources(Me.tbDewT0, "tbDewT0")
        Me.tbDewT0.Name = "tbDewT0"
        '
        'tbDewDP
        '
        resources.ApplyResources(Me.tbDewDP, "tbDewDP")
        Me.tbDewDP.Name = "tbDewDP"
        '
        'tbDewMaxPoints
        '
        resources.ApplyResources(Me.tbDewMaxPoints, "tbDewMaxPoints")
        Me.tbDewMaxPoints.Name = "tbDewMaxPoints"
        '
        'chkControlDewInit
        '
        resources.ApplyResources(Me.chkControlDewInit, "chkControlDewInit")
        Me.chkControlDewInit.Name = "chkControlDewInit"
        '
        'PanelCalc
        '
        resources.ApplyResources(Me.PanelCalc, "PanelCalc")
        Me.PanelCalc.Controls.Add(Me.Button2)
        Me.PanelCalc.Controls.Add(Me.LabelStatus)
        Me.PanelCalc.Name = "PanelCalc"
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        '
        'LabelStatus
        '
        resources.ApplyResources(Me.LabelStatus, "LabelStatus")
        Me.LabelStatus.Name = "LabelStatus"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.ComboBox1)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'ComboBox1
        '
        resources.ApplyResources(Me.ComboBox1, "ComboBox1")
        Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox1.DropDownWidth = 250
        Me.ComboBox1.Items.AddRange(New Object() {resources.GetString("ComboBox1.Items"), resources.GetString("ComboBox1.Items1"), resources.GetString("ComboBox1.Items2"), resources.GetString("ComboBox1.Items3"), resources.GetString("ComboBox1.Items4"), resources.GetString("ComboBox1.Items5"), resources.GetString("ComboBox1.Items6"), resources.GetString("ComboBox1.Items7"), resources.GetString("ComboBox1.Items8"), resources.GetString("ComboBox1.Items9"), resources.GetString("ComboBox1.Items10"), resources.GetString("ComboBox1.Items11")})
        Me.ComboBox1.Name = "ComboBox1"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.FaTabStrip1)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'FaTabStrip1
        '
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem1, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem1
        '
        'FaTabStripItem1
        '
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.Controls.Add(Me.GraphControl)
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Selected = True
        '
        'GraphControl
        '
        resources.ApplyResources(Me.GraphControl, "GraphControl")
        Me.GraphControl.Cursor = System.Windows.Forms.Cursors.Default
        Me.GraphControl.IsAntiAlias = True
        Me.GraphControl.IsAutoScrollRange = True
        Me.GraphControl.IsShowCopyMessage = False
        Me.GraphControl.Name = "GraphControl"
        Me.GraphControl.ScrollGrace = 0R
        Me.GraphControl.ScrollMaxX = 0R
        Me.GraphControl.ScrollMaxY = 0R
        Me.GraphControl.ScrollMaxY2 = 0R
        Me.GraphControl.ScrollMinX = 0R
        Me.GraphControl.ScrollMinY = 0R
        Me.GraphControl.ScrollMinY2 = 0R
        '
        'FaTabStripItem2
        '
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.Grid1)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        '
        'Grid1
        '
        resources.ApplyResources(Me.Grid1, "Grid1")
        Me.Grid1.AllowUserToAddRows = False
        Me.Grid1.AllowUserToDeleteRows = False
        Me.Grid1.AllowUserToOrderColumns = True
        Me.Grid1.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.Grid1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.Grid1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.AllCells
        Me.Grid1.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        Me.Grid1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.Grid1.Name = "Grid1"
        Me.Grid1.ReadOnly = True
        Me.Grid1.RowHeadersVisible = False
        Me.Grid1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'BackgroundWorker1
        '
        Me.BackgroundWorker1.WorkerReportsProgress = True
        Me.BackgroundWorker1.WorkerSupportsCancellation = True
        '
        'dckMenu
        '
        resources.ApplyResources(Me.dckMenu, "dckMenu")
        Me.dckMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FloatToolStripMenuItem, Me.DockLeftToolStripMenuItem, Me.DockRightToolStripMenuItem, Me.DockTopToolStripMenuItem, Me.DockBottomToolStripMenuItem, Me.DockLeftAutoHideToolStripMenuItem, Me.DockRightAutoHideToolStripMenuItem, Me.DockTopAutoHideToolStripMenuItem, Me.DockBottomAutoHideToolStripMenuItem, Me.DocumentToolStripMenuItem})
        Me.dckMenu.Name = "dckMenu"
        '
        'FloatToolStripMenuItem
        '
        resources.ApplyResources(Me.FloatToolStripMenuItem, "FloatToolStripMenuItem")
        Me.FloatToolStripMenuItem.Name = "FloatToolStripMenuItem"
        '
        'DockLeftToolStripMenuItem
        '
        resources.ApplyResources(Me.DockLeftToolStripMenuItem, "DockLeftToolStripMenuItem")
        Me.DockLeftToolStripMenuItem.Name = "DockLeftToolStripMenuItem"
        '
        'DockRightToolStripMenuItem
        '
        resources.ApplyResources(Me.DockRightToolStripMenuItem, "DockRightToolStripMenuItem")
        Me.DockRightToolStripMenuItem.Name = "DockRightToolStripMenuItem"
        '
        'DockTopToolStripMenuItem
        '
        resources.ApplyResources(Me.DockTopToolStripMenuItem, "DockTopToolStripMenuItem")
        Me.DockTopToolStripMenuItem.Name = "DockTopToolStripMenuItem"
        '
        'DockBottomToolStripMenuItem
        '
        resources.ApplyResources(Me.DockBottomToolStripMenuItem, "DockBottomToolStripMenuItem")
        Me.DockBottomToolStripMenuItem.Name = "DockBottomToolStripMenuItem"
        '
        'DockLeftAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockLeftAutoHideToolStripMenuItem, "DockLeftAutoHideToolStripMenuItem")
        Me.DockLeftAutoHideToolStripMenuItem.Name = "DockLeftAutoHideToolStripMenuItem"
        '
        'DockRightAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockRightAutoHideToolStripMenuItem, "DockRightAutoHideToolStripMenuItem")
        Me.DockRightAutoHideToolStripMenuItem.Name = "DockRightAutoHideToolStripMenuItem"
        '
        'DockTopAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockTopAutoHideToolStripMenuItem, "DockTopAutoHideToolStripMenuItem")
        Me.DockTopAutoHideToolStripMenuItem.Name = "DockTopAutoHideToolStripMenuItem"
        '
        'DockBottomAutoHideToolStripMenuItem
        '
        resources.ApplyResources(Me.DockBottomAutoHideToolStripMenuItem, "DockBottomAutoHideToolStripMenuItem")
        Me.DockBottomAutoHideToolStripMenuItem.Name = "DockBottomAutoHideToolStripMenuItem"
        '
        'DocumentToolStripMenuItem
        '
        resources.ApplyResources(Me.DocumentToolStripMenuItem, "DocumentToolStripMenuItem")
        Me.DocumentToolStripMenuItem.Name = "DocumentToolStripMenuItem"
        '
        'FormPhEnv
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox6)
        Me.Name = "FormPhEnv"
        Me.GroupBox6.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        CType(Me.tbQuality, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.TabPage2.PerformLayout()
        Me.PanelBub.ResumeLayout(False)
        Me.PanelBub.PerformLayout()
        Me.TabPage3.ResumeLayout(False)
        Me.TabPage3.PerformLayout()
        Me.PanelDew.ResumeLayout(False)
        Me.PanelDew.PerformLayout()
        Me.PanelCalc.ResumeLayout(False)
        Me.PanelCalc.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem1.ResumeLayout(False)
        Me.FaTabStripItem2.ResumeLayout(False)
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Public WithEvents Button1 As System.Windows.Forms.Button
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents ComboBox1 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents Grid1 As System.Windows.Forms.DataGridView
    Public WithEvents GraphControl As ZedGraph.ZedGraphControl
    Public WithEvents BackgroundWorker1 As System.ComponentModel.BackgroundWorker
    Public WithEvents chkQualityLine As System.Windows.Forms.CheckBox
    Public WithEvents chkOp As System.Windows.Forms.CheckBox
    Public WithEvents chkStabCurve As System.Windows.Forms.CheckBox
    Public WithEvents tbQuality As System.Windows.Forms.NumericUpDown
    Public WithEvents chkpip As System.Windows.Forms.CheckBox
    Public WithEvents chkhyd As System.Windows.Forms.CheckBox
    Public WithEvents cbhydmodel As System.Windows.Forms.ComboBox
    Public WithEvents chkHydVapOnly As System.Windows.Forms.CheckBox
    Friend WithEvents dckMenu As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents FloatToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockLeftToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockRightToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockTopToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockBottomToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockLeftAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockRightAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockTopAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DockBottomAutoHideToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DocumentToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PanelCalc As System.Windows.Forms.Panel
    Public WithEvents Button2 As System.Windows.Forms.Button
    Public WithEvents LabelStatus As System.Windows.Forms.Label
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents TabPage3 As System.Windows.Forms.TabPage
    Friend WithEvents rbBubTVF As System.Windows.Forms.RadioButton
    Friend WithEvents rbBubPVF As System.Windows.Forms.RadioButton
    Public WithEvents Label4 As System.Windows.Forms.Label
    Public WithEvents chkBubLiqInstability As System.Windows.Forms.CheckBox
    Friend WithEvents lbBubDT As System.Windows.Forms.Label
    Friend WithEvents lbBubDP As System.Windows.Forms.Label
    Friend WithEvents lbBubT0 As System.Windows.Forms.Label
    Friend WithEvents lbBubP0 As System.Windows.Forms.Label
    Friend WithEvents tbBubDT As System.Windows.Forms.TextBox
    Friend WithEvents tbBubDP As System.Windows.Forms.TextBox
    Friend WithEvents tbBubMaxPoints As System.Windows.Forms.TextBox
    Friend WithEvents tbBubT0 As System.Windows.Forms.TextBox
    Friend WithEvents tbBubP0 As System.Windows.Forms.TextBox
    Public WithEvents Label9 As System.Windows.Forms.Label
    Public WithEvents Label8 As System.Windows.Forms.Label
    Public WithEvents Label7 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents lbDewDT As System.Windows.Forms.Label
    Friend WithEvents lbDewDP As System.Windows.Forms.Label
    Friend WithEvents lbDewT0 As System.Windows.Forms.Label
    Friend WithEvents lbDewP0 As System.Windows.Forms.Label
    Friend WithEvents tbDewDT As System.Windows.Forms.TextBox
    Friend WithEvents tbDewDP As System.Windows.Forms.TextBox
    Friend WithEvents tbDewMaxPoints As System.Windows.Forms.TextBox
    Friend WithEvents tbDewT0 As System.Windows.Forms.TextBox
    Friend WithEvents tbDewP0 As System.Windows.Forms.TextBox
    Public WithEvents Label18 As System.Windows.Forms.Label
    Public WithEvents Label19 As System.Windows.Forms.Label
    Public WithEvents Label20 As System.Windows.Forms.Label
    Public WithEvents Label21 As System.Windows.Forms.Label
    Public WithEvents Label22 As System.Windows.Forms.Label
    Friend WithEvents rbDewTVF As System.Windows.Forms.RadioButton
    Friend WithEvents rbDewPVF As System.Windows.Forms.RadioButton
    Public WithEvents Label23 As System.Windows.Forms.Label
    Public WithEvents chkControlBubInit As System.Windows.Forms.CheckBox
    Public WithEvents chkControlDewInit As System.Windows.Forms.CheckBox
    Friend WithEvents lblBubTmax As System.Windows.Forms.Label
    Friend WithEvents tbBubTmax As System.Windows.Forms.TextBox
    Public WithEvents Label12 As System.Windows.Forms.Label
    Friend WithEvents lblDewTmax As System.Windows.Forms.Label
    Friend WithEvents tbDewTmax As System.Windows.Forms.TextBox
    Public WithEvents Label11 As System.Windows.Forms.Label
    Friend WithEvents PanelBub As System.Windows.Forms.Panel
    Friend WithEvents PanelDew As System.Windows.Forms.Panel
    Public WithEvents chkImmiscibleWater As CheckBox
End Class
