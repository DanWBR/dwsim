Imports DWSIM.DrawingTools

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FlowsheetSurface_SkiaSharp
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
    Protected Overrides Sub Dispose(ByVal disposing As Boolean)
        If disposing AndAlso components IsNot Nothing Then
            components.Dispose()
        End If
        MyBase.Dispose(disposing)
    End Sub

    'Required by the Windows Form Designer
    Private components As System.ComponentModel.IContainer

    'NOTE: The following procedure is required by the Windows Form Designer
    'It can be modified using the Windows Form Designer.  
    'Do not modify it using the code editor.
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FlowsheetSurface_SkiaSharp))
        Me.ToolStripContainer1 = New System.Windows.Forms.ToolStripContainer()
        Me.SplitContainerVertical = New System.Windows.Forms.SplitContainer()
        Me.SplitContainerHorizontal = New System.Windows.Forms.SplitContainer()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.PanelFlowsheetControl = New System.Windows.Forms.Panel()
        Me.btnLeft = New System.Windows.Forms.Button()
        Me.btnUp = New System.Windows.Forms.Button()
        Me.btnDown = New System.Windows.Forms.Button()
        Me.btnRight = New System.Windows.Forms.Button()
        Me.PanelWeather = New System.Windows.Forms.Panel()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.btnGetWeather = New System.Windows.Forms.Button()
        Me.tbCurrentLocation = New System.Windows.Forms.TextBox()
        Me.btnGetLocation = New System.Windows.Forms.Button()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.tbAtmPress = New System.Windows.Forms.TextBox()
        Me.PictureBox6 = New System.Windows.Forms.PictureBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.tbAmbientTemperature = New System.Windows.Forms.TextBox()
        Me.PictureBox5 = New System.Windows.Forms.PictureBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbSolarIrradiation = New System.Windows.Forms.TextBox()
        Me.PictureBox4 = New System.Windows.Forms.PictureBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbHumidity = New System.Windows.Forms.TextBox()
        Me.PictureBox3 = New System.Windows.Forms.PictureBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbWindSpeed = New System.Windows.Forms.TextBox()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.ToolStripFlowsheet = New System.Windows.Forms.ToolStrip()
        Me.tsbControlPanelMode = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator17 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.tstbSearch = New System.Windows.Forms.ToolStripTextBox()
        Me.ToolStripSeparator15 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbCutObj = New System.Windows.Forms.ToolStripButton()
        Me.tsbCopyObj = New System.Windows.Forms.ToolStripButton()
        Me.tsbPasteObj = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator12 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton12 = New System.Windows.Forms.ToolStripButton()
        Me.TSBtabela = New System.Windows.Forms.ToolStripButton()
        Me.TSBTexto = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton6 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton19 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator10 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.TSTBZoom = New System.Windows.Forms.ToolStripTextBox()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton20 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator9 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbResizeMode = New System.Windows.Forms.ToolStripButton()
        Me.tsbResizeModeKeepAR = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator13 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbDisplayGrid = New System.Windows.Forms.ToolStripButton()
        Me.tsbSnapObjectsToGrid = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton17 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator14 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbConfigPage = New System.Windows.Forms.ToolStripButton()
        Me.tsbConfigPrinter = New System.Windows.Forms.ToolStripButton()
        Me.tsbPrint = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator19 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbMultiSelectMode = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignLefts = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignCenters = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignRights = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignTops = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignMiddles = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignBottoms = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator16 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbAlignVertical = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignHorizontal = New System.Windows.Forms.ToolStripButton()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.tsmiHeatMap = New System.Windows.Forms.ToolStripButton()
        Me.tss1 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsmiLiveFlow = New System.Windows.Forms.ToolStripButton()
        Me.tss2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel2 = New System.Windows.Forms.ToolStripLabel()
        Me.tbFontSize = New System.Windows.Forms.ToolStripTextBox()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator20 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel3 = New System.Windows.Forms.ToolStripLabel()
        Me.tsbColorTheme = New System.Windows.Forms.ToolStripComboBox()
        Me.ToolStripSeparator21 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripLabel4 = New System.Windows.Forms.ToolStripLabel()
        Me.tscbAddObjectsWithStreams = New System.Windows.Forms.ToolStripComboBox()
        Me.CMS_NoSel = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ExibirTudoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ZoomPadrao100ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CentralizarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyAsImageToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopiarComoImagem200ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopiarComoImagem300ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExportarParaPDFToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExportarParaSVGToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator22 = New System.Windows.Forms.ToolStripSeparator()
        Me.PasteObjectTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator18 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsmiNaturalLayout = New System.Windows.Forms.ToolStripMenuItem()
        Me.LayoutAutomaticoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RestaurarLayoutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tssepPro2 = New System.Windows.Forms.ToolStripSeparator()
        Me.FindTearStreamsAutomaticallyToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMS_Sel = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.TSMI_Label = New System.Windows.Forms.ToolStripMenuItem()
        Me.AtivadoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiToggleDrawLabel = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.RecalcularToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyFromTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.DepurarObjetoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator6 = New System.Windows.Forms.ToolStripSeparator()
        Me.ConectarAToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DesconectarDeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.TSMI_Girar = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem6 = New System.Windows.Forms.ToolStripMenuItem()
        Me.BToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem7 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem11 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem12 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem13 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem14 = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiInvertVertically = New System.Windows.Forms.ToolStripMenuItem()
        Me.HorizontalmenteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.CutTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopyTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ClonarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExcluirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator()
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator7 = New System.Windows.Forms.ToolStripSeparator()
        Me.EditAppearanceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiCopyObjID = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator8 = New System.Windows.Forms.ToolStripSeparator()
        Me.SplitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SplitAndInsertValveTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.SplitAndInsertRecycleMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MergeStreamsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PreviewDialog = New System.Windows.Forms.PrintPreviewDialog()
        Me.designSurfacePrintDocument = New System.Drawing.Printing.PrintDocument()
        Me.CMS_ItemsToConnect = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CMS_ItemsToDisconnect = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.pageSetup = New System.Windows.Forms.PageSetupDialog()
        Me.setupPrint = New System.Windows.Forms.PrintDialog()
        Me.Timer1 = New System.Windows.Forms.Timer(Me.components)
        Me.Timer2 = New System.Windows.Forms.Timer(Me.components)
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
        Me.ToolStripSeparator11 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.ToolStripContainer1.ContentPanel.SuspendLayout()
        Me.ToolStripContainer1.TopToolStripPanel.SuspendLayout()
        Me.ToolStripContainer1.SuspendLayout()
        CType(Me.SplitContainerVertical, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainerVertical.Panel1.SuspendLayout()
        Me.SplitContainerVertical.SuspendLayout()
        CType(Me.SplitContainerHorizontal, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainerHorizontal.Panel1.SuspendLayout()
        Me.SplitContainerHorizontal.SuspendLayout()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.PanelWeather.SuspendLayout()
        Me.Panel1.SuspendLayout()
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox5, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStripFlowsheet.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.CMS_NoSel.SuspendLayout()
        Me.CMS_Sel.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolStripContainer1
        '
        '
        'ToolStripContainer1.ContentPanel
        '
        Me.ToolStripContainer1.ContentPanel.Controls.Add(Me.SplitContainerVertical)
        resources.ApplyResources(Me.ToolStripContainer1.ContentPanel, "ToolStripContainer1.ContentPanel")
        resources.ApplyResources(Me.ToolStripContainer1, "ToolStripContainer1")
        Me.ToolStripContainer1.Name = "ToolStripContainer1"
        '
        'ToolStripContainer1.TopToolStripPanel
        '
        Me.ToolStripContainer1.TopToolStripPanel.Controls.Add(Me.ToolStripFlowsheet)
        Me.ToolStripContainer1.TopToolStripPanel.Controls.Add(Me.ToolStrip1)
        '
        'SplitContainerVertical
        '
        Me.SplitContainerVertical.BackColor = System.Drawing.Color.White
        resources.ApplyResources(Me.SplitContainerVertical, "SplitContainerVertical")
        Me.SplitContainerVertical.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
        Me.SplitContainerVertical.Name = "SplitContainerVertical"
        '
        'SplitContainerVertical.Panel1
        '
        Me.SplitContainerVertical.Panel1.Controls.Add(Me.SplitContainerHorizontal)
        '
        'SplitContainerHorizontal
        '
        resources.ApplyResources(Me.SplitContainerHorizontal, "SplitContainerHorizontal")
        Me.SplitContainerHorizontal.FixedPanel = System.Windows.Forms.FixedPanel.Panel2
        Me.SplitContainerHorizontal.Name = "SplitContainerHorizontal"
        '
        'SplitContainerHorizontal.Panel1
        '
        Me.SplitContainerHorizontal.Panel1.Controls.Add(Me.TableLayoutPanel1)
        Me.SplitContainerHorizontal.Panel1.Controls.Add(Me.PanelWeather)
        '
        'SplitContainerHorizontal.Panel2
        '
        Me.SplitContainerHorizontal.Panel2.BackColor = System.Drawing.Color.White
        Me.SplitContainerHorizontal.Panel2Collapsed = True
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.BackColor = System.Drawing.Color.White
        resources.ApplyResources(Me.TableLayoutPanel1, "TableLayoutPanel1")
        Me.TableLayoutPanel1.Controls.Add(Me.PanelFlowsheetControl, 1, 1)
        Me.TableLayoutPanel1.Controls.Add(Me.btnLeft, 0, 1)
        Me.TableLayoutPanel1.Controls.Add(Me.btnUp, 1, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.btnDown, 1, 2)
        Me.TableLayoutPanel1.Controls.Add(Me.btnRight, 2, 1)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        '
        'PanelFlowsheetControl
        '
        resources.ApplyResources(Me.PanelFlowsheetControl, "PanelFlowsheetControl")
        Me.PanelFlowsheetControl.Name = "PanelFlowsheetControl"
        '
        'btnLeft
        '
        resources.ApplyResources(Me.btnLeft, "btnLeft")
        Me.btnLeft.Name = "btnLeft"
        Me.btnLeft.UseVisualStyleBackColor = True
        '
        'btnUp
        '
        resources.ApplyResources(Me.btnUp, "btnUp")
        Me.btnUp.Name = "btnUp"
        Me.btnUp.UseVisualStyleBackColor = True
        '
        'btnDown
        '
        resources.ApplyResources(Me.btnDown, "btnDown")
        Me.btnDown.Name = "btnDown"
        Me.btnDown.UseVisualStyleBackColor = True
        '
        'btnRight
        '
        resources.ApplyResources(Me.btnRight, "btnRight")
        Me.btnRight.Name = "btnRight"
        Me.btnRight.UseVisualStyleBackColor = True
        '
        'PanelWeather
        '
        Me.PanelWeather.BackColor = System.Drawing.Color.White
        Me.PanelWeather.Controls.Add(Me.Panel1)
        Me.PanelWeather.Controls.Add(Me.Label6)
        Me.PanelWeather.Controls.Add(Me.tbAtmPress)
        Me.PanelWeather.Controls.Add(Me.PictureBox6)
        Me.PanelWeather.Controls.Add(Me.Label5)
        Me.PanelWeather.Controls.Add(Me.tbAmbientTemperature)
        Me.PanelWeather.Controls.Add(Me.PictureBox5)
        Me.PanelWeather.Controls.Add(Me.Label3)
        Me.PanelWeather.Controls.Add(Me.tbSolarIrradiation)
        Me.PanelWeather.Controls.Add(Me.PictureBox4)
        Me.PanelWeather.Controls.Add(Me.Label2)
        Me.PanelWeather.Controls.Add(Me.tbHumidity)
        Me.PanelWeather.Controls.Add(Me.PictureBox3)
        Me.PanelWeather.Controls.Add(Me.Label1)
        Me.PanelWeather.Controls.Add(Me.tbWindSpeed)
        Me.PanelWeather.Controls.Add(Me.PictureBox2)
        resources.ApplyResources(Me.PanelWeather, "PanelWeather")
        Me.PanelWeather.Name = "PanelWeather"
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.Button1)
        Me.Panel1.Controls.Add(Me.Label4)
        Me.Panel1.Controls.Add(Me.btnGetWeather)
        Me.Panel1.Controls.Add(Me.tbCurrentLocation)
        Me.Panel1.Controls.Add(Me.btnGetLocation)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'Button1
        '
        Me.Button1.BackgroundImage = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'btnGetWeather
        '
        resources.ApplyResources(Me.btnGetWeather, "btnGetWeather")
        Me.btnGetWeather.Name = "btnGetWeather"
        Me.btnGetWeather.UseVisualStyleBackColor = True
        '
        'tbCurrentLocation
        '
        resources.ApplyResources(Me.tbCurrentLocation, "tbCurrentLocation")
        Me.tbCurrentLocation.Name = "tbCurrentLocation"
        '
        'btnGetLocation
        '
        Me.btnGetLocation.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_location
        resources.ApplyResources(Me.btnGetLocation, "btnGetLocation")
        Me.btnGetLocation.Name = "btnGetLocation"
        Me.btnGetLocation.UseVisualStyleBackColor = True
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'tbAtmPress
        '
        resources.ApplyResources(Me.tbAtmPress, "tbAtmPress")
        Me.tbAtmPress.Name = "tbAtmPress"
        '
        'PictureBox6
        '
        Me.PictureBox6.Image = Global.DWSIM.My.Resources.Resources.icons8_pressure_gauge
        resources.ApplyResources(Me.PictureBox6, "PictureBox6")
        Me.PictureBox6.Name = "PictureBox6"
        Me.PictureBox6.TabStop = False
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'tbAmbientTemperature
        '
        resources.ApplyResources(Me.tbAmbientTemperature, "tbAmbientTemperature")
        Me.tbAmbientTemperature.Name = "tbAmbientTemperature"
        '
        'PictureBox5
        '
        Me.PictureBox5.Image = Global.DWSIM.My.Resources.Resources.icons8_temperature
        resources.ApplyResources(Me.PictureBox5, "PictureBox5")
        Me.PictureBox5.Name = "PictureBox5"
        Me.PictureBox5.TabStop = False
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'tbSolarIrradiation
        '
        resources.ApplyResources(Me.tbSolarIrradiation, "tbSolarIrradiation")
        Me.tbSolarIrradiation.Name = "tbSolarIrradiation"
        '
        'PictureBox4
        '
        Me.PictureBox4.Image = Global.DWSIM.My.Resources.Resources.icons8_solar_irradiation
        resources.ApplyResources(Me.PictureBox4, "PictureBox4")
        Me.PictureBox4.Name = "PictureBox4"
        Me.PictureBox4.TabStop = False
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'tbHumidity
        '
        resources.ApplyResources(Me.tbHumidity, "tbHumidity")
        Me.tbHumidity.Name = "tbHumidity"
        '
        'PictureBox3
        '
        Me.PictureBox3.Image = Global.DWSIM.My.Resources.Resources.icons8_humidity
        resources.ApplyResources(Me.PictureBox3, "PictureBox3")
        Me.PictureBox3.Name = "PictureBox3"
        Me.PictureBox3.TabStop = False
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbWindSpeed
        '
        resources.ApplyResources(Me.tbWindSpeed, "tbWindSpeed")
        Me.tbWindSpeed.Name = "tbWindSpeed"
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.DWSIM.My.Resources.Resources.icons8_wind
        resources.ApplyResources(Me.PictureBox2, "PictureBox2")
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.TabStop = False
        '
        'ToolStripFlowsheet
        '
        Me.ToolStripFlowsheet.CanOverflow = False
        resources.ApplyResources(Me.ToolStripFlowsheet, "ToolStripFlowsheet")
        Me.ToolStripFlowsheet.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbControlPanelMode, Me.ToolStripSeparator17, Me.ToolStripLabel1, Me.tstbSearch, Me.ToolStripSeparator15, Me.tsbCutObj, Me.tsbCopyObj, Me.tsbPasteObj, Me.ToolStripSeparator12, Me.ToolStripButton12, Me.TSBtabela, Me.TSBTexto, Me.ToolStripButton4, Me.ToolStripButton6, Me.ToolStripButton19, Me.ToolStripSeparator10, Me.ToolStripButton1, Me.TSTBZoom, Me.ToolStripButton2, Me.ToolStripButton20, Me.ToolStripButton3, Me.ToolStripSeparator9, Me.tsbResizeMode, Me.tsbResizeModeKeepAR, Me.ToolStripSeparator13, Me.tsbDisplayGrid, Me.tsbSnapObjectsToGrid, Me.ToolStripButton17, Me.ToolStripSeparator14, Me.tsbConfigPage, Me.tsbConfigPrinter, Me.tsbPrint, Me.ToolStripSeparator19, Me.tsbMultiSelectMode, Me.tsbAlignLefts, Me.tsbAlignCenters, Me.tsbAlignRights, Me.tsbAlignTops, Me.tsbAlignMiddles, Me.tsbAlignBottoms, Me.ToolStripSeparator16, Me.tsbAlignVertical, Me.tsbAlignHorizontal})
        Me.ToolStripFlowsheet.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.Flow
        Me.ToolStripFlowsheet.Name = "ToolStripFlowsheet"
        Me.ToolStripFlowsheet.Stretch = True
        '
        'tsbControlPanelMode
        '
        Me.tsbControlPanelMode.CheckOnClick = True
        Me.tsbControlPanelMode.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.tsbControlPanelMode, "tsbControlPanelMode")
        Me.tsbControlPanelMode.Name = "tsbControlPanelMode"
        '
        'ToolStripSeparator17
        '
        Me.ToolStripSeparator17.Name = "ToolStripSeparator17"
        resources.ApplyResources(Me.ToolStripSeparator17, "ToolStripSeparator17")
        '
        'ToolStripLabel1
        '
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        '
        'tstbSearch
        '
        Me.tstbSearch.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.tstbSearch.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        resources.ApplyResources(Me.tstbSearch, "tstbSearch")
        Me.tstbSearch.Name = "tstbSearch"
        '
        'ToolStripSeparator15
        '
        Me.ToolStripSeparator15.Name = "ToolStripSeparator15"
        resources.ApplyResources(Me.ToolStripSeparator15, "ToolStripSeparator15")
        '
        'tsbCutObj
        '
        Me.tsbCutObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbCutObj.Image = Global.DWSIM.My.Resources.Resources.cut
        resources.ApplyResources(Me.tsbCutObj, "tsbCutObj")
        Me.tsbCutObj.Name = "tsbCutObj"
        '
        'tsbCopyObj
        '
        Me.tsbCopyObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbCopyObj.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.tsbCopyObj, "tsbCopyObj")
        Me.tsbCopyObj.Name = "tsbCopyObj"
        '
        'tsbPasteObj
        '
        Me.tsbPasteObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbPasteObj.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        resources.ApplyResources(Me.tsbPasteObj, "tsbPasteObj")
        Me.tsbPasteObj.Name = "tsbPasteObj"
        '
        'ToolStripSeparator12
        '
        Me.ToolStripSeparator12.Name = "ToolStripSeparator12"
        resources.ApplyResources(Me.ToolStripSeparator12, "ToolStripSeparator12")
        '
        'ToolStripButton12
        '
        Me.ToolStripButton12.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton12.Image = Global.DWSIM.My.Resources.Resources.shape_square
        resources.ApplyResources(Me.ToolStripButton12, "ToolStripButton12")
        Me.ToolStripButton12.Name = "ToolStripButton12"
        '
        'TSBtabela
        '
        Me.TSBtabela.CheckOnClick = True
        Me.TSBtabela.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtabela.Image = Global.DWSIM.My.Resources.Resources.picture
        resources.ApplyResources(Me.TSBtabela, "TSBtabela")
        Me.TSBtabela.Name = "TSBtabela"
        '
        'TSBTexto
        '
        Me.TSBTexto.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBTexto.Image = Global.DWSIM.My.Resources.Resources.font
        resources.ApplyResources(Me.TSBTexto, "TSBTexto")
        Me.TSBTexto.Name = "TSBTexto"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.My.Resources.Resources.table_relationship
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripButton6
        '
        Me.ToolStripButton6.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton6.Image = Global.DWSIM.My.Resources.Resources.table_lightning
        resources.ApplyResources(Me.ToolStripButton6, "ToolStripButton6")
        Me.ToolStripButton6.Name = "ToolStripButton6"
        '
        'ToolStripButton19
        '
        Me.ToolStripButton19.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton19.Image = Global.DWSIM.My.Resources.Resources.table
        resources.ApplyResources(Me.ToolStripButton19, "ToolStripButton19")
        Me.ToolStripButton19.Name = "ToolStripButton19"
        '
        'ToolStripSeparator10
        '
        Me.ToolStripSeparator10.Name = "ToolStripSeparator10"
        resources.ApplyResources(Me.ToolStripSeparator10, "ToolStripSeparator10")
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.zoom_out
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'TSTBZoom
        '
        Me.TSTBZoom.BackColor = System.Drawing.SystemColors.Control
        resources.ApplyResources(Me.TSTBZoom, "TSTBZoom")
        Me.TSTBZoom.Name = "TSTBZoom"
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.zoom_in
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton20
        '
        Me.ToolStripButton20.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton20.Image = Global.DWSIM.My.Resources.Resources.zoom_extend
        resources.ApplyResources(Me.ToolStripButton20, "ToolStripButton20")
        Me.ToolStripButton20.Name = "ToolStripButton20"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.My.Resources.Resources.zoom_refresh
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripSeparator9
        '
        Me.ToolStripSeparator9.Name = "ToolStripSeparator9"
        resources.ApplyResources(Me.ToolStripSeparator9, "ToolStripSeparator9")
        '
        'tsbResizeMode
        '
        Me.tsbResizeMode.CheckOnClick = True
        Me.tsbResizeMode.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbResizeMode.Image = Global.DWSIM.My.Resources.Resources.shape_handles
        resources.ApplyResources(Me.tsbResizeMode, "tsbResizeMode")
        Me.tsbResizeMode.Name = "tsbResizeMode"
        '
        'tsbResizeModeKeepAR
        '
        Me.tsbResizeModeKeepAR.Checked = True
        Me.tsbResizeModeKeepAR.CheckOnClick = True
        Me.tsbResizeModeKeepAR.CheckState = System.Windows.Forms.CheckState.Checked
        Me.tsbResizeModeKeepAR.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbResizeModeKeepAR.Image = Global.DWSIM.My.Resources.Resources.arrow_nw_se
        resources.ApplyResources(Me.tsbResizeModeKeepAR, "tsbResizeModeKeepAR")
        Me.tsbResizeModeKeepAR.Name = "tsbResizeModeKeepAR"
        '
        'ToolStripSeparator13
        '
        Me.ToolStripSeparator13.Name = "ToolStripSeparator13"
        resources.ApplyResources(Me.ToolStripSeparator13, "ToolStripSeparator13")
        '
        'tsbDisplayGrid
        '
        Me.tsbDisplayGrid.BackColor = System.Drawing.Color.LightSkyBlue
        Me.tsbDisplayGrid.CheckOnClick = True
        Me.tsbDisplayGrid.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDisplayGrid.Image = Global.DWSIM.My.Resources.Resources.shading
        Me.tsbDisplayGrid.Name = "tsbDisplayGrid"
        resources.ApplyResources(Me.tsbDisplayGrid, "tsbDisplayGrid")
        '
        'tsbSnapObjectsToGrid
        '
        Me.tsbSnapObjectsToGrid.CheckOnClick = True
        Me.tsbSnapObjectsToGrid.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbSnapObjectsToGrid.Image = Global.DWSIM.My.Resources.Resources.shading
        Me.tsbSnapObjectsToGrid.Name = "tsbSnapObjectsToGrid"
        resources.ApplyResources(Me.tsbSnapObjectsToGrid, "tsbSnapObjectsToGrid")
        '
        'ToolStripButton17
        '
        Me.ToolStripButton17.CheckOnClick = True
        Me.ToolStripButton17.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton17.Image = Global.DWSIM.My.Resources.Resources.connect
        resources.ApplyResources(Me.ToolStripButton17, "ToolStripButton17")
        Me.ToolStripButton17.Name = "ToolStripButton17"
        '
        'ToolStripSeparator14
        '
        Me.ToolStripSeparator14.Name = "ToolStripSeparator14"
        resources.ApplyResources(Me.ToolStripSeparator14, "ToolStripSeparator14")
        '
        'tsbConfigPage
        '
        Me.tsbConfigPage.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbConfigPage.Image = Global.DWSIM.My.Resources.Resources.page_gear
        resources.ApplyResources(Me.tsbConfigPage, "tsbConfigPage")
        Me.tsbConfigPage.Name = "tsbConfigPage"
        '
        'tsbConfigPrinter
        '
        Me.tsbConfigPrinter.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbConfigPrinter.Image = Global.DWSIM.My.Resources.Resources.printer
        resources.ApplyResources(Me.tsbConfigPrinter, "tsbConfigPrinter")
        Me.tsbConfigPrinter.Name = "tsbConfigPrinter"
        '
        'tsbPrint
        '
        Me.tsbPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbPrint.Image = Global.DWSIM.My.Resources.Resources.printer_add
        resources.ApplyResources(Me.tsbPrint, "tsbPrint")
        Me.tsbPrint.Name = "tsbPrint"
        '
        'ToolStripSeparator19
        '
        Me.ToolStripSeparator19.Name = "ToolStripSeparator19"
        resources.ApplyResources(Me.ToolStripSeparator19, "ToolStripSeparator19")
        '
        'tsbMultiSelectMode
        '
        Me.tsbMultiSelectMode.CheckOnClick = True
        Me.tsbMultiSelectMode.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbMultiSelectMode.Image = Global.DWSIM.My.Resources.Resources.shape_group
        resources.ApplyResources(Me.tsbMultiSelectMode, "tsbMultiSelectMode")
        Me.tsbMultiSelectMode.Name = "tsbMultiSelectMode"
        '
        'tsbAlignLefts
        '
        Me.tsbAlignLefts.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignLefts.Image = Global.DWSIM.My.Resources.Resources.shape_align_left
        resources.ApplyResources(Me.tsbAlignLefts, "tsbAlignLefts")
        Me.tsbAlignLefts.Name = "tsbAlignLefts"
        '
        'tsbAlignCenters
        '
        Me.tsbAlignCenters.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignCenters.Image = Global.DWSIM.My.Resources.Resources.shape_align_center
        resources.ApplyResources(Me.tsbAlignCenters, "tsbAlignCenters")
        Me.tsbAlignCenters.Name = "tsbAlignCenters"
        '
        'tsbAlignRights
        '
        Me.tsbAlignRights.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignRights.Image = Global.DWSIM.My.Resources.Resources.shape_align_right
        resources.ApplyResources(Me.tsbAlignRights, "tsbAlignRights")
        Me.tsbAlignRights.Name = "tsbAlignRights"
        '
        'tsbAlignTops
        '
        Me.tsbAlignTops.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignTops.Image = Global.DWSIM.My.Resources.Resources.shape_align_top
        resources.ApplyResources(Me.tsbAlignTops, "tsbAlignTops")
        Me.tsbAlignTops.Name = "tsbAlignTops"
        '
        'tsbAlignMiddles
        '
        Me.tsbAlignMiddles.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignMiddles.Image = Global.DWSIM.My.Resources.Resources.shape_align_middle
        resources.ApplyResources(Me.tsbAlignMiddles, "tsbAlignMiddles")
        Me.tsbAlignMiddles.Name = "tsbAlignMiddles"
        '
        'tsbAlignBottoms
        '
        Me.tsbAlignBottoms.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignBottoms.Image = Global.DWSIM.My.Resources.Resources.shape_align_bottom
        resources.ApplyResources(Me.tsbAlignBottoms, "tsbAlignBottoms")
        Me.tsbAlignBottoms.Name = "tsbAlignBottoms"
        '
        'ToolStripSeparator16
        '
        Me.ToolStripSeparator16.Name = "ToolStripSeparator16"
        resources.ApplyResources(Me.ToolStripSeparator16, "ToolStripSeparator16")
        '
        'tsbAlignVertical
        '
        Me.tsbAlignVertical.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignVertical.Image = Global.DWSIM.My.Resources.Resources.shape_align_center1
        resources.ApplyResources(Me.tsbAlignVertical, "tsbAlignVertical")
        Me.tsbAlignVertical.Name = "tsbAlignVertical"
        '
        'tsbAlignHorizontal
        '
        Me.tsbAlignHorizontal.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignHorizontal.Image = Global.DWSIM.My.Resources.Resources.shape_align_middle1
        resources.ApplyResources(Me.tsbAlignHorizontal, "tsbAlignHorizontal")
        Me.tsbAlignHorizontal.Name = "tsbAlignHorizontal"
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsmiHeatMap, Me.tss1, Me.tsmiLiveFlow, Me.tss2, Me.ToolStripLabel2, Me.tbFontSize, Me.ToolStripButton5, Me.ToolStripSeparator20, Me.ToolStripLabel3, Me.tsbColorTheme, Me.ToolStripSeparator21, Me.ToolStripLabel4, Me.tscbAddObjectsWithStreams})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'tsmiHeatMap
        '
        Me.tsmiHeatMap.CheckOnClick = True
        resources.ApplyResources(Me.tsmiHeatMap, "tsmiHeatMap")
        Me.tsmiHeatMap.Name = "tsmiHeatMap"
        '
        'tss1
        '
        Me.tss1.Name = "tss1"
        resources.ApplyResources(Me.tss1, "tss1")
        '
        'tsmiLiveFlow
        '
        resources.ApplyResources(Me.tsmiLiveFlow, "tsmiLiveFlow")
        Me.tsmiLiveFlow.Name = "tsmiLiveFlow"
        '
        'tss2
        '
        Me.tss2.Name = "tss2"
        resources.ApplyResources(Me.tss2, "tss2")
        '
        'ToolStripLabel2
        '
        Me.ToolStripLabel2.AutoToolTip = True
        Me.ToolStripLabel2.Image = Global.DWSIM.My.Resources.Resources.fontsize
        Me.ToolStripLabel2.Name = "ToolStripLabel2"
        resources.ApplyResources(Me.ToolStripLabel2, "ToolStripLabel2")
        '
        'tbFontSize
        '
        resources.ApplyResources(Me.tbFontSize, "tbFontSize")
        Me.tbFontSize.Margin = New System.Windows.Forms.Padding(1, 0, 10, 0)
        Me.tbFontSize.Name = "tbFontSize"
        '
        'ToolStripButton5
        '
        Me.ToolStripButton5.Image = Global.DWSIM.My.Resources.Resources.font
        resources.ApplyResources(Me.ToolStripButton5, "ToolStripButton5")
        Me.ToolStripButton5.Name = "ToolStripButton5"
        '
        'ToolStripSeparator20
        '
        Me.ToolStripSeparator20.Name = "ToolStripSeparator20"
        resources.ApplyResources(Me.ToolStripSeparator20, "ToolStripSeparator20")
        '
        'ToolStripLabel3
        '
        Me.ToolStripLabel3.AutoToolTip = True
        Me.ToolStripLabel3.Image = Global.DWSIM.My.Resources.Resources.color_wheel
        Me.ToolStripLabel3.Name = "ToolStripLabel3"
        resources.ApplyResources(Me.ToolStripLabel3, "ToolStripLabel3")
        '
        'tsbColorTheme
        '
        Me.tsbColorTheme.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tsbColorTheme.Items.AddRange(New Object() {resources.GetString("tsbColorTheme.Items"), resources.GetString("tsbColorTheme.Items1"), resources.GetString("tsbColorTheme.Items2")})
        Me.tsbColorTheme.Name = "tsbColorTheme"
        resources.ApplyResources(Me.tsbColorTheme, "tsbColorTheme")
        '
        'ToolStripSeparator21
        '
        Me.ToolStripSeparator21.Name = "ToolStripSeparator21"
        resources.ApplyResources(Me.ToolStripSeparator21, "ToolStripSeparator21")
        '
        'ToolStripLabel4
        '
        Me.ToolStripLabel4.Name = "ToolStripLabel4"
        resources.ApplyResources(Me.ToolStripLabel4, "ToolStripLabel4")
        '
        'tscbAddObjectsWithStreams
        '
        Me.tscbAddObjectsWithStreams.AutoCompleteCustomSource.AddRange(New String() {resources.GetString("tscbAddObjectsWithStreams.AutoCompleteCustomSource"), resources.GetString("tscbAddObjectsWithStreams.AutoCompleteCustomSource1"), resources.GetString("tscbAddObjectsWithStreams.AutoCompleteCustomSource2")})
        Me.tscbAddObjectsWithStreams.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tscbAddObjectsWithStreams.Items.AddRange(New Object() {resources.GetString("tscbAddObjectsWithStreams.Items"), resources.GetString("tscbAddObjectsWithStreams.Items1"), resources.GetString("tscbAddObjectsWithStreams.Items2")})
        Me.tscbAddObjectsWithStreams.Name = "tscbAddObjectsWithStreams"
        resources.ApplyResources(Me.tscbAddObjectsWithStreams, "tscbAddObjectsWithStreams")
        '
        'CMS_NoSel
        '
        Me.CMS_NoSel.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem3, Me.ToolStripSeparator1, Me.ExibirTudoToolStripMenuItem, Me.ZoomPadrao100ToolStripMenuItem, Me.CentralizarToolStripMenuItem, Me.CopyAsImageToolStripMenuItem, Me.CopiarComoImagem200ToolStripMenuItem, Me.CopiarComoImagem300ToolStripMenuItem, Me.ExportarParaPDFToolStripMenuItem, Me.ExportarParaSVGToolStripMenuItem, Me.ToolStripSeparator22, Me.PasteObjectTSMI, Me.ToolStripSeparator18, Me.tsmiNaturalLayout, Me.LayoutAutomaticoToolStripMenuItem, Me.RestaurarLayoutToolStripMenuItem, Me.tssepPro2, Me.FindTearStreamsAutomaticallyToolStripMenuItem})
        Me.CMS_NoSel.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.CMS_NoSel, "CMS_NoSel")
        '
        'ToolStripMenuItem3
        '
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        '
        'ExibirTudoToolStripMenuItem
        '
        Me.ExibirTudoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom_extend
        Me.ExibirTudoToolStripMenuItem.Name = "ExibirTudoToolStripMenuItem"
        resources.ApplyResources(Me.ExibirTudoToolStripMenuItem, "ExibirTudoToolStripMenuItem")
        '
        'ZoomPadrao100ToolStripMenuItem
        '
        Me.ZoomPadrao100ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom_refresh
        Me.ZoomPadrao100ToolStripMenuItem.Name = "ZoomPadrao100ToolStripMenuItem"
        resources.ApplyResources(Me.ZoomPadrao100ToolStripMenuItem, "ZoomPadrao100ToolStripMenuItem")
        '
        'CentralizarToolStripMenuItem
        '
        Me.CentralizarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.CentralizarToolStripMenuItem.Name = "CentralizarToolStripMenuItem"
        resources.ApplyResources(Me.CentralizarToolStripMenuItem, "CentralizarToolStripMenuItem")
        '
        'CopyAsImageToolStripMenuItem
        '
        Me.CopyAsImageToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.CopyAsImageToolStripMenuItem.Name = "CopyAsImageToolStripMenuItem"
        resources.ApplyResources(Me.CopyAsImageToolStripMenuItem, "CopyAsImageToolStripMenuItem")
        '
        'CopiarComoImagem200ToolStripMenuItem
        '
        Me.CopiarComoImagem200ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.CopiarComoImagem200ToolStripMenuItem.Name = "CopiarComoImagem200ToolStripMenuItem"
        resources.ApplyResources(Me.CopiarComoImagem200ToolStripMenuItem, "CopiarComoImagem200ToolStripMenuItem")
        '
        'CopiarComoImagem300ToolStripMenuItem
        '
        Me.CopiarComoImagem300ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.CopiarComoImagem300ToolStripMenuItem.Name = "CopiarComoImagem300ToolStripMenuItem"
        resources.ApplyResources(Me.CopiarComoImagem300ToolStripMenuItem, "CopiarComoImagem300ToolStripMenuItem")
        '
        'ExportarParaPDFToolStripMenuItem
        '
        Me.ExportarParaPDFToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_pdf
        Me.ExportarParaPDFToolStripMenuItem.Name = "ExportarParaPDFToolStripMenuItem"
        resources.ApplyResources(Me.ExportarParaPDFToolStripMenuItem, "ExportarParaPDFToolStripMenuItem")
        '
        'ExportarParaSVGToolStripMenuItem
        '
        Me.ExportarParaSVGToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_vector
        Me.ExportarParaSVGToolStripMenuItem.Name = "ExportarParaSVGToolStripMenuItem"
        resources.ApplyResources(Me.ExportarParaSVGToolStripMenuItem, "ExportarParaSVGToolStripMenuItem")
        '
        'ToolStripSeparator22
        '
        Me.ToolStripSeparator22.Name = "ToolStripSeparator22"
        resources.ApplyResources(Me.ToolStripSeparator22, "ToolStripSeparator22")
        '
        'PasteObjectTSMI
        '
        Me.PasteObjectTSMI.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        Me.PasteObjectTSMI.Name = "PasteObjectTSMI"
        resources.ApplyResources(Me.PasteObjectTSMI, "PasteObjectTSMI")
        '
        'ToolStripSeparator18
        '
        Me.ToolStripSeparator18.Name = "ToolStripSeparator18"
        resources.ApplyResources(Me.ToolStripSeparator18, "ToolStripSeparator18")
        '
        'tsmiNaturalLayout
        '
        Me.tsmiNaturalLayout.Image = Global.DWSIM.My.Resources.Resources.arrows_4_way
        Me.tsmiNaturalLayout.Name = "tsmiNaturalLayout"
        resources.ApplyResources(Me.tsmiNaturalLayout, "tsmiNaturalLayout")
        '
        'LayoutAutomaticoToolStripMenuItem
        '
        Me.LayoutAutomaticoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrows_4_way
        Me.LayoutAutomaticoToolStripMenuItem.Name = "LayoutAutomaticoToolStripMenuItem"
        resources.ApplyResources(Me.LayoutAutomaticoToolStripMenuItem, "LayoutAutomaticoToolStripMenuItem")
        '
        'RestaurarLayoutToolStripMenuItem
        '
        resources.ApplyResources(Me.RestaurarLayoutToolStripMenuItem, "RestaurarLayoutToolStripMenuItem")
        Me.RestaurarLayoutToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrows_4_way
        Me.RestaurarLayoutToolStripMenuItem.Name = "RestaurarLayoutToolStripMenuItem"
        '
        'tssepPro2
        '
        Me.tssepPro2.Name = "tssepPro2"
        resources.ApplyResources(Me.tssepPro2, "tssepPro2")
        '
        'FindTearStreamsAutomaticallyToolStripMenuItem
        '
        Me.FindTearStreamsAutomaticallyToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.lightning
        Me.FindTearStreamsAutomaticallyToolStripMenuItem.Name = "FindTearStreamsAutomaticallyToolStripMenuItem"
        resources.ApplyResources(Me.FindTearStreamsAutomaticallyToolStripMenuItem, "FindTearStreamsAutomaticallyToolStripMenuItem")
        '
        'CMS_Sel
        '
        Me.CMS_Sel.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSMI_Label, Me.AtivadoToolStripMenuItem, Me.tsmiToggleDrawLabel, Me.ToolStripSeparator3, Me.RecalcularToolStripMenuItem, Me.CopyFromTSMI, Me.DepurarObjetoToolStripMenuItem, Me.ToolStripSeparator6, Me.ConectarAToolStripMenuItem, Me.DesconectarDeToolStripMenuItem, Me.ToolStripSeparator4, Me.TSMI_Girar, Me.tsmiInvertVertically, Me.HorizontalmenteToolStripMenuItem, Me.ToolStripSeparator2, Me.CutTSMI, Me.CopyTSMI, Me.ClonarToolStripMenuItem, Me.ExcluirToolStripMenuItem, Me.ToolStripSeparator5, Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem, Me.ToolStripSeparator7, Me.EditAppearanceToolStripMenuItem, Me.tsmiCopyObjID, Me.ToolStripSeparator8, Me.SplitToolStripMenuItem, Me.SplitAndInsertValveTSMI, Me.SplitAndInsertRecycleMenuItem, Me.MergeStreamsToolStripMenuItem})
        Me.CMS_Sel.Name = "CMS_Sel"
        resources.ApplyResources(Me.CMS_Sel, "CMS_Sel")
        '
        'TSMI_Label
        '
        resources.ApplyResources(Me.TSMI_Label, "TSMI_Label")
        Me.TSMI_Label.Name = "TSMI_Label"
        '
        'AtivadoToolStripMenuItem
        '
        Me.AtivadoToolStripMenuItem.CheckOnClick = True
        Me.AtivadoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.accept
        Me.AtivadoToolStripMenuItem.Name = "AtivadoToolStripMenuItem"
        resources.ApplyResources(Me.AtivadoToolStripMenuItem, "AtivadoToolStripMenuItem")
        '
        'tsmiToggleDrawLabel
        '
        Me.tsmiToggleDrawLabel.Image = Global.DWSIM.My.Resources.Resources.textfield_rename
        Me.tsmiToggleDrawLabel.Name = "tsmiToggleDrawLabel"
        resources.ApplyResources(Me.tsmiToggleDrawLabel, "tsmiToggleDrawLabel")
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        '
        'RecalcularToolStripMenuItem
        '
        Me.RecalcularToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        Me.RecalcularToolStripMenuItem.Name = "RecalcularToolStripMenuItem"
        resources.ApplyResources(Me.RecalcularToolStripMenuItem, "RecalcularToolStripMenuItem")
        '
        'CopyFromTSMI
        '
        Me.CopyFromTSMI.Image = Global.DWSIM.My.Resources.Resources.table_row_insert1
        Me.CopyFromTSMI.Name = "CopyFromTSMI"
        resources.ApplyResources(Me.CopyFromTSMI, "CopyFromTSMI")
        '
        'DepurarObjetoToolStripMenuItem
        '
        Me.DepurarObjetoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_osx_terminal
        Me.DepurarObjetoToolStripMenuItem.Name = "DepurarObjetoToolStripMenuItem"
        resources.ApplyResources(Me.DepurarObjetoToolStripMenuItem, "DepurarObjetoToolStripMenuItem")
        '
        'ToolStripSeparator6
        '
        Me.ToolStripSeparator6.Name = "ToolStripSeparator6"
        resources.ApplyResources(Me.ToolStripSeparator6, "ToolStripSeparator6")
        '
        'ConectarAToolStripMenuItem
        '
        Me.ConectarAToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.connect
        Me.ConectarAToolStripMenuItem.Name = "ConectarAToolStripMenuItem"
        resources.ApplyResources(Me.ConectarAToolStripMenuItem, "ConectarAToolStripMenuItem")
        '
        'DesconectarDeToolStripMenuItem
        '
        Me.DesconectarDeToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.disconnect
        Me.DesconectarDeToolStripMenuItem.Name = "DesconectarDeToolStripMenuItem"
        resources.ApplyResources(Me.DesconectarDeToolStripMenuItem, "DesconectarDeToolStripMenuItem")
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        '
        'TSMI_Girar
        '
        Me.TSMI_Girar.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem6, Me.BToolStripMenuItem, Me.ToolStripMenuItem7, Me.ToolStripMenuItem11, Me.ToolStripMenuItem12, Me.ToolStripMenuItem13, Me.ToolStripMenuItem14})
        Me.TSMI_Girar.Image = Global.DWSIM.My.Resources.Resources.arrow_rotate_clockwise
        Me.TSMI_Girar.Name = "TSMI_Girar"
        resources.ApplyResources(Me.TSMI_Girar, "TSMI_Girar")
        '
        'ToolStripMenuItem6
        '
        Me.ToolStripMenuItem6.Image = Global.DWSIM.My.Resources.Resources.arrow_rotate_clockwise
        Me.ToolStripMenuItem6.Name = "ToolStripMenuItem6"
        resources.ApplyResources(Me.ToolStripMenuItem6, "ToolStripMenuItem6")
        '
        'BToolStripMenuItem
        '
        resources.ApplyResources(Me.BToolStripMenuItem, "BToolStripMenuItem")
        Me.BToolStripMenuItem.Name = "BToolStripMenuItem"
        '
        'ToolStripMenuItem7
        '
        resources.ApplyResources(Me.ToolStripMenuItem7, "ToolStripMenuItem7")
        Me.ToolStripMenuItem7.Name = "ToolStripMenuItem7"
        '
        'ToolStripMenuItem11
        '
        resources.ApplyResources(Me.ToolStripMenuItem11, "ToolStripMenuItem11")
        Me.ToolStripMenuItem11.Name = "ToolStripMenuItem11"
        '
        'ToolStripMenuItem12
        '
        resources.ApplyResources(Me.ToolStripMenuItem12, "ToolStripMenuItem12")
        Me.ToolStripMenuItem12.Name = "ToolStripMenuItem12"
        '
        'ToolStripMenuItem13
        '
        resources.ApplyResources(Me.ToolStripMenuItem13, "ToolStripMenuItem13")
        Me.ToolStripMenuItem13.Name = "ToolStripMenuItem13"
        '
        'ToolStripMenuItem14
        '
        resources.ApplyResources(Me.ToolStripMenuItem14, "ToolStripMenuItem14")
        Me.ToolStripMenuItem14.Name = "ToolStripMenuItem14"
        '
        'tsmiInvertVertically
        '
        Me.tsmiInvertVertically.CheckOnClick = True
        Me.tsmiInvertVertically.Image = Global.DWSIM.My.Resources.Resources.shape_flip_vertical
        Me.tsmiInvertVertically.Name = "tsmiInvertVertically"
        resources.ApplyResources(Me.tsmiInvertVertically, "tsmiInvertVertically")
        '
        'HorizontalmenteToolStripMenuItem
        '
        Me.HorizontalmenteToolStripMenuItem.CheckOnClick = True
        Me.HorizontalmenteToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_flip_horizontal
        Me.HorizontalmenteToolStripMenuItem.Name = "HorizontalmenteToolStripMenuItem"
        resources.ApplyResources(Me.HorizontalmenteToolStripMenuItem, "HorizontalmenteToolStripMenuItem")
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        '
        'CutTSMI
        '
        Me.CutTSMI.Image = Global.DWSIM.My.Resources.Resources.cut
        Me.CutTSMI.Name = "CutTSMI"
        resources.ApplyResources(Me.CutTSMI, "CutTSMI")
        '
        'CopyTSMI
        '
        Me.CopyTSMI.Image = Global.DWSIM.My.Resources.Resources.copy
        Me.CopyTSMI.Name = "CopyTSMI"
        resources.ApplyResources(Me.CopyTSMI, "CopyTSMI")
        '
        'ClonarToolStripMenuItem
        '
        Me.ClonarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.sheep
        resources.ApplyResources(Me.ClonarToolStripMenuItem, "ClonarToolStripMenuItem")
        Me.ClonarToolStripMenuItem.Name = "ClonarToolStripMenuItem"
        '
        'ExcluirToolStripMenuItem
        '
        Me.ExcluirToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.ExcluirToolStripMenuItem.Name = "ExcluirToolStripMenuItem"
        resources.ApplyResources(Me.ExcluirToolStripMenuItem, "ExcluirToolStripMenuItem")
        '
        'ToolStripSeparator5
        '
        Me.ToolStripSeparator5.Name = "ToolStripSeparator5"
        resources.ApplyResources(Me.ToolStripSeparator5, "ToolStripSeparator5")
        '
        'CopiarDadosParaareaDeTransferenciaToolStripMenuItem
        '
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.clipboard_sign
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Name = "CopiarDadosParaareaDeTransferenciaToolStripMenuItem"
        resources.ApplyResources(Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem, "CopiarDadosParaareaDeTransferenciaToolStripMenuItem")
        '
        'ToolStripSeparator7
        '
        Me.ToolStripSeparator7.Name = "ToolStripSeparator7"
        resources.ApplyResources(Me.ToolStripSeparator7, "ToolStripSeparator7")
        '
        'EditAppearanceToolStripMenuItem
        '
        Me.EditAppearanceToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_square_edit
        Me.EditAppearanceToolStripMenuItem.Name = "EditAppearanceToolStripMenuItem"
        resources.ApplyResources(Me.EditAppearanceToolStripMenuItem, "EditAppearanceToolStripMenuItem")
        '
        'tsmiCopyObjID
        '
        Me.tsmiCopyObjID.Image = Global.DWSIM.My.Resources.Resources.Touch_ID_64px
        Me.tsmiCopyObjID.Name = "tsmiCopyObjID"
        resources.ApplyResources(Me.tsmiCopyObjID, "tsmiCopyObjID")
        '
        'ToolStripSeparator8
        '
        Me.ToolStripSeparator8.Name = "ToolStripSeparator8"
        resources.ApplyResources(Me.ToolStripSeparator8, "ToolStripSeparator8")
        '
        'SplitToolStripMenuItem
        '
        Me.SplitToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_divide
        Me.SplitToolStripMenuItem.Name = "SplitToolStripMenuItem"
        resources.ApplyResources(Me.SplitToolStripMenuItem, "SplitToolStripMenuItem")
        '
        'SplitAndInsertValveTSMI
        '
        Me.SplitAndInsertValveTSMI.Image = Global.DWSIM.My.Resources.Resources.uo_valve_16
        Me.SplitAndInsertValveTSMI.Name = "SplitAndInsertValveTSMI"
        resources.ApplyResources(Me.SplitAndInsertValveTSMI, "SplitAndInsertValveTSMI")
        '
        'SplitAndInsertRecycleMenuItem
        '
        Me.SplitAndInsertRecycleMenuItem.Image = Global.DWSIM.My.Resources.Resources.lo_recy_16
        Me.SplitAndInsertRecycleMenuItem.Name = "SplitAndInsertRecycleMenuItem"
        resources.ApplyResources(Me.SplitAndInsertRecycleMenuItem, "SplitAndInsertRecycleMenuItem")
        '
        'MergeStreamsToolStripMenuItem
        '
        Me.MergeStreamsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_join
        Me.MergeStreamsToolStripMenuItem.Name = "MergeStreamsToolStripMenuItem"
        resources.ApplyResources(Me.MergeStreamsToolStripMenuItem, "MergeStreamsToolStripMenuItem")
        '
        'PreviewDialog
        '
        resources.ApplyResources(Me.PreviewDialog, "PreviewDialog")
        Me.PreviewDialog.Document = Me.designSurfacePrintDocument
        Me.PreviewDialog.Name = "PrintPreviewDialog1"
        Me.PreviewDialog.UseAntiAlias = True
        '
        'designSurfacePrintDocument
        '
        Me.designSurfacePrintDocument.DocumentName = "Document"
        '
        'CMS_ItemsToConnect
        '
        Me.CMS_ItemsToConnect.Name = "CMS_ItemsToConnect"
        resources.ApplyResources(Me.CMS_ItemsToConnect, "CMS_ItemsToConnect")
        '
        'CMS_ItemsToDisconnect
        '
        Me.CMS_ItemsToDisconnect.Name = "CMS_ItemsToConnect"
        resources.ApplyResources(Me.CMS_ItemsToDisconnect, "CMS_ItemsToDisconnect")
        '
        'pageSetup
        '
        Me.pageSetup.AllowPrinter = False
        Me.pageSetup.Document = Me.designSurfacePrintDocument
        Me.pageSetup.EnableMetric = True
        Me.pageSetup.ShowHelp = True
        Me.pageSetup.ShowNetwork = False
        '
        'setupPrint
        '
        Me.setupPrint.AllowCurrentPage = True
        Me.setupPrint.AllowPrintToFile = False
        Me.setupPrint.AllowSomePages = True
        Me.setupPrint.Document = Me.designSurfacePrintDocument
        Me.setupPrint.ShowNetwork = False
        Me.setupPrint.UseEXDialog = True
        '
        'Timer1
        '
        Me.Timer1.Enabled = True
        '
        'dckMenu
        '
        Me.dckMenu.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FloatToolStripMenuItem, Me.DockLeftToolStripMenuItem, Me.DockRightToolStripMenuItem, Me.DockTopToolStripMenuItem, Me.DockBottomToolStripMenuItem, Me.DockLeftAutoHideToolStripMenuItem, Me.DockRightAutoHideToolStripMenuItem, Me.DockTopAutoHideToolStripMenuItem, Me.DockBottomAutoHideToolStripMenuItem, Me.DocumentToolStripMenuItem})
        Me.dckMenu.Name = "dckMenu"
        resources.ApplyResources(Me.dckMenu, "dckMenu")
        '
        'FloatToolStripMenuItem
        '
        Me.FloatToolStripMenuItem.Name = "FloatToolStripMenuItem"
        resources.ApplyResources(Me.FloatToolStripMenuItem, "FloatToolStripMenuItem")
        '
        'DockLeftToolStripMenuItem
        '
        Me.DockLeftToolStripMenuItem.Name = "DockLeftToolStripMenuItem"
        resources.ApplyResources(Me.DockLeftToolStripMenuItem, "DockLeftToolStripMenuItem")
        '
        'DockRightToolStripMenuItem
        '
        Me.DockRightToolStripMenuItem.Name = "DockRightToolStripMenuItem"
        resources.ApplyResources(Me.DockRightToolStripMenuItem, "DockRightToolStripMenuItem")
        '
        'DockTopToolStripMenuItem
        '
        Me.DockTopToolStripMenuItem.Name = "DockTopToolStripMenuItem"
        resources.ApplyResources(Me.DockTopToolStripMenuItem, "DockTopToolStripMenuItem")
        '
        'DockBottomToolStripMenuItem
        '
        Me.DockBottomToolStripMenuItem.Name = "DockBottomToolStripMenuItem"
        resources.ApplyResources(Me.DockBottomToolStripMenuItem, "DockBottomToolStripMenuItem")
        '
        'DockLeftAutoHideToolStripMenuItem
        '
        Me.DockLeftAutoHideToolStripMenuItem.Name = "DockLeftAutoHideToolStripMenuItem"
        resources.ApplyResources(Me.DockLeftAutoHideToolStripMenuItem, "DockLeftAutoHideToolStripMenuItem")
        '
        'DockRightAutoHideToolStripMenuItem
        '
        Me.DockRightAutoHideToolStripMenuItem.Name = "DockRightAutoHideToolStripMenuItem"
        resources.ApplyResources(Me.DockRightAutoHideToolStripMenuItem, "DockRightAutoHideToolStripMenuItem")
        '
        'DockTopAutoHideToolStripMenuItem
        '
        Me.DockTopAutoHideToolStripMenuItem.Name = "DockTopAutoHideToolStripMenuItem"
        resources.ApplyResources(Me.DockTopAutoHideToolStripMenuItem, "DockTopAutoHideToolStripMenuItem")
        '
        'DockBottomAutoHideToolStripMenuItem
        '
        Me.DockBottomAutoHideToolStripMenuItem.Name = "DockBottomAutoHideToolStripMenuItem"
        resources.ApplyResources(Me.DockBottomAutoHideToolStripMenuItem, "DockBottomAutoHideToolStripMenuItem")
        '
        'DocumentToolStripMenuItem
        '
        Me.DocumentToolStripMenuItem.Name = "DocumentToolStripMenuItem"
        resources.ApplyResources(Me.DocumentToolStripMenuItem, "DocumentToolStripMenuItem")
        '
        'ToolStripSeparator11
        '
        Me.ToolStripSeparator11.Name = "ToolStripSeparator11"
        resources.ApplyResources(Me.ToolStripSeparator11, "ToolStripSeparator11")
        '
        'FlowsheetSurface_SkiaSharp
        '
        Me.AllowDrop = True
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.ToolStripContainer1)
        Me.DoubleBuffered = True
        Me.Name = "FlowsheetSurface_SkiaSharp"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.TabText = Me.Text
        Me.ToolStripContainer1.ContentPanel.ResumeLayout(False)
        Me.ToolStripContainer1.TopToolStripPanel.ResumeLayout(False)
        Me.ToolStripContainer1.TopToolStripPanel.PerformLayout()
        Me.ToolStripContainer1.ResumeLayout(False)
        Me.ToolStripContainer1.PerformLayout()
        Me.SplitContainerVertical.Panel1.ResumeLayout(False)
        CType(Me.SplitContainerVertical, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainerVertical.ResumeLayout(False)
        Me.SplitContainerHorizontal.Panel1.ResumeLayout(False)
        CType(Me.SplitContainerHorizontal, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainerHorizontal.ResumeLayout(False)
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.PanelWeather.ResumeLayout(False)
        Me.PanelWeather.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        CType(Me.PictureBox6, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox5, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox4, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox3, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStripFlowsheet.ResumeLayout(False)
        Me.ToolStripFlowsheet.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.CMS_NoSel.ResumeLayout(False)
        Me.CMS_Sel.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents CMS_NoSel As System.Windows.Forms.ContextMenuStrip
    Public WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CMS_Sel As System.Windows.Forms.ContextMenuStrip
    Public WithEvents TSMI_Girar As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem6 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents BToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem7 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents TSMI_Label As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents PreviewDialog As System.Windows.Forms.PrintPreviewDialog
    Public WithEvents designSurfacePrintDocument As System.Drawing.Printing.PrintDocument
    Public WithEvents pageSetup As System.Windows.Forms.PageSetupDialog
    Public WithEvents setupPrint As System.Windows.Forms.PrintDialog
    Public WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents Timer1 As System.Windows.Forms.Timer
    Public WithEvents ClonarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents HorizontalmenteToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ConectarAToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents DesconectarDeToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents CMS_ItemsToConnect As System.Windows.Forms.ContextMenuStrip
    Public WithEvents CMS_ItemsToDisconnect As System.Windows.Forms.ContextMenuStrip
    Public WithEvents ExcluirToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents Timer2 As System.Windows.Forms.Timer
    Public WithEvents RecalcularToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator6 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents CopiarDadosParaareaDeTransferenciaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CopyFromTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExibirTudoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ZoomPadrao100ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CentralizarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
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
    Friend WithEvents ToolStripMenuItem11 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem12 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem13 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem14 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DepurarObjetoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator7 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents AtivadoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SplitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MergeStreamsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator8 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripFlowsheet As System.Windows.Forms.ToolStrip
    Friend WithEvents tsbCutObj As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbCopyObj As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbPasteObj As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator12 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton12 As System.Windows.Forms.ToolStripButton
    Public WithEvents TSBtabela As System.Windows.Forms.ToolStripButton
    Public WithEvents TSBTexto As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton6 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton19 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator10 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents TSTBZoom As System.Windows.Forms.ToolStripTextBox
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton20 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator11 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbResizeMode As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbResizeModeKeepAR As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator13 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbSnapObjectsToGrid As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton17 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator14 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents tsbConfigPage As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbConfigPrinter As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbPrint As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignLefts As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignCenters As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignRights As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignTops As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignMiddles As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignBottoms As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator16 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbAlignVertical As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignHorizontal As System.Windows.Forms.ToolStripButton
    Friend WithEvents SplitContainerHorizontal As SplitContainer
    Friend WithEvents EditAppearanceToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents tsbDisplayGrid As ToolStripButton
    Friend WithEvents tsbMultiSelectMode As ToolStripButton
    Public WithEvents ToolStripSeparator9 As ToolStripSeparator
    Friend WithEvents SplitAndInsertRecycleMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripLabel1 As ToolStripLabel
    Friend WithEvents tstbSearch As ToolStripTextBox
    Friend WithEvents ToolStripSeparator15 As ToolStripSeparator
    Friend WithEvents SplitAndInsertValveTSMI As ToolStripMenuItem
    Friend WithEvents btnLeft As Button
    Friend WithEvents btnUp As Button
    Friend WithEvents btnDown As Button
    Friend WithEvents btnRight As Button
    Friend WithEvents tsbControlPanelMode As ToolStripButton
    Friend WithEvents ToolStripSeparator17 As ToolStripSeparator
    Friend WithEvents ToolStripSeparator18 As ToolStripSeparator
    Friend WithEvents LayoutAutomaticoToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents RestaurarLayoutToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents SplitContainerVertical As SplitContainer
    Friend WithEvents ToolStripSeparator19 As ToolStripSeparator
    Friend WithEvents ToolStripLabel2 As ToolStripLabel
    Friend WithEvents tbFontSize As ToolStripTextBox
    Friend WithEvents ToolStripLabel3 As ToolStripLabel
    Friend WithEvents tsbColorTheme As ToolStripComboBox
    Friend WithEvents ToolStripContainer1 As ToolStripContainer
    Friend WithEvents ToolStripButton5 As ToolStripButton
    Friend WithEvents ToolStripSeparator20 As ToolStripSeparator
    Public WithEvents tsmiInvertVertically As ToolStripMenuItem
    Friend WithEvents tssepPro2 As ToolStripSeparator
    Friend WithEvents FindTearStreamsAutomaticallyToolStripMenuItem As ToolStripMenuItem
    Public WithEvents TableLayoutPanel1 As TableLayoutPanel
    Public WithEvents PanelFlowsheetControl As Panel
    Friend WithEvents ToolStripSeparator21 As ToolStripSeparator
    Friend WithEvents ToolStripLabel4 As ToolStripLabel
    Friend WithEvents tscbAddObjectsWithStreams As ToolStripComboBox
    Friend WithEvents ExportarParaPDFToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ExportarParaSVGToolStripMenuItem As ToolStripMenuItem
    Public WithEvents ToolStrip1 As ToolStrip
    Friend WithEvents tsmiHeatMap As ToolStripButton
    Friend WithEvents tss2 As ToolStripSeparator
    Friend WithEvents tsmiLiveFlow As ToolStripButton
    Friend WithEvents tss1 As ToolStripSeparator
    Friend WithEvents Label2 As Label
    Friend WithEvents tbHumidity As TextBox
    Friend WithEvents PictureBox3 As PictureBox
    Friend WithEvents Label1 As Label
    Friend WithEvents tbWindSpeed As TextBox
    Friend WithEvents PictureBox2 As PictureBox
    Friend WithEvents Label3 As Label
    Friend WithEvents tbSolarIrradiation As TextBox
    Friend WithEvents PictureBox4 As PictureBox
    Friend WithEvents btnGetWeather As Button
    Friend WithEvents btnGetLocation As Button
    Friend WithEvents Label4 As Label
    Friend WithEvents tbCurrentLocation As TextBox
    Friend WithEvents Label5 As Label
    Friend WithEvents tbAmbientTemperature As TextBox
    Friend WithEvents PictureBox5 As PictureBox
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents Label6 As Label
    Friend WithEvents tbAtmPress As TextBox
    Friend WithEvents PictureBox6 As PictureBox
    Public WithEvents PanelWeather As Panel
    Friend WithEvents Panel1 As Panel
    Friend WithEvents Button1 As Button
    Friend WithEvents tsmiNaturalLayout As ToolStripMenuItem
    Public WithEvents CopyAsImageToolStripMenuItem As ToolStripMenuItem
    Public WithEvents CopiarComoImagem200ToolStripMenuItem As ToolStripMenuItem
    Public WithEvents CopiarComoImagem300ToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator22 As ToolStripSeparator
    Public WithEvents PasteObjectTSMI As ToolStripMenuItem
    Public WithEvents CutTSMI As ToolStripMenuItem
    Public WithEvents CopyTSMI As ToolStripMenuItem
    Friend WithEvents tsmiCopyObjID As ToolStripMenuItem
    Public WithEvents tsmiToggleDrawLabel As ToolStripMenuItem
End Class
