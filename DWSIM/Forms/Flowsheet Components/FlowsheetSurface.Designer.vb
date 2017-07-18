<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FlowsheetSurface
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()> _
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
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FlowsheetSurface))
        Me.FlowsheetDesignSurface = New Global.DWSIM.DrawingTools.GraphicsSurface()
        Me.CMS_NoSel = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.AddNewTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator9 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem5 = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopiarParaAareaDeTransferenciaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem4 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem8 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem10 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExibirTudoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ZoomPadrao100ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CentralizarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CMS_Sel = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.TSMI_Label = New System.Windows.Forms.ToolStripMenuItem()
        Me.AtivadoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
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
        Me.HorizontalmenteToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.ClonarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExcluirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator()
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator7 = New System.Windows.Forms.ToolStripSeparator()
        Me.EditAppearanceToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator8 = New System.Windows.Forms.ToolStripSeparator()
        Me.SplitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
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
        Me.PanelSearch = New System.Windows.Forms.Panel()
        Me.tbSearch = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.FlowLayoutPanel1 = New System.Windows.Forms.FlowLayoutPanel()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.ToolStripFlowsheet = New System.Windows.Forms.ToolStrip()
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
        Me.ToolStripSeparator11 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbResizeMode = New System.Windows.Forms.ToolStripButton()
        Me.tsbResizeModeKeepAR = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator13 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton16 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton17 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator14 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton18 = New System.Windows.Forms.ToolStripButton()
        Me.tsbConfigPage = New System.Windows.Forms.ToolStripButton()
        Me.tsbConfigPrinter = New System.Windows.Forms.ToolStripButton()
        Me.tsbPrint = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator15 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbAlignLefts = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignCenters = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignRights = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignTops = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignMiddles = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignBottoms = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator16 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbAlignVertical = New System.Windows.Forms.ToolStripButton()
        Me.tsbAlignHorizontal = New System.Windows.Forms.ToolStripButton()
        Me.CMS_NoSel.SuspendLayout()
        Me.CMS_Sel.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.PanelSearch.SuspendLayout()
        Me.FlowLayoutPanel1.SuspendLayout()
        Me.ToolStripFlowsheet.SuspendLayout()
        Me.SuspendLayout()
        '
        'FlowsheetDesignSurface
        '
        resources.ApplyResources(Me.FlowsheetDesignSurface, "FlowsheetDesignSurface")
        Me.FlowsheetDesignSurface.AllowDrop = True
        Me.FlowsheetDesignSurface.BackColor = System.Drawing.Color.White
        Me.FlowsheetDesignSurface.Editor = Nothing
        Me.FlowsheetDesignSurface.GridColor = System.Drawing.Color.GhostWhite
        Me.FlowsheetDesignSurface.GridLineWidth = 1
        Me.FlowsheetDesignSurface.GridSize = 25.0!
        Me.FlowsheetDesignSurface.MarginColor = System.Drawing.SystemColors.Control
        Me.FlowsheetDesignSurface.MarginLineWidth = 1
        Me.FlowsheetDesignSurface.MouseHoverSelect = False
        Me.FlowsheetDesignSurface.Name = "FlowsheetDesignSurface"
        Me.FlowsheetDesignSurface.NonPrintingAreaColor = System.Drawing.Color.LightGray
        Me.FlowsheetDesignSurface.QuickConnect = False
        Me.FlowsheetDesignSurface.ResizingMode = False
        Me.FlowsheetDesignSurface.ResizingMode_KeepAR = True
        Me.FlowsheetDesignSurface.SelectedObject = Nothing
        Me.FlowsheetDesignSurface.SelectRectangle = True
        Me.FlowsheetDesignSurface.ShowGrid = False
        Me.FlowsheetDesignSurface.SnapToGrid = False
        Me.FlowsheetDesignSurface.SurfaceBounds = New System.Drawing.Rectangle(0, 0, 10000, 7000)
        Me.FlowsheetDesignSurface.SurfaceMargins = New System.Drawing.Rectangle(0, 0, 10000, 7000)
        Me.FlowsheetDesignSurface.Zoom = 1.0!
        '
        'CMS_NoSel
        '
        resources.ApplyResources(Me.CMS_NoSel, "CMS_NoSel")
        Me.CMS_NoSel.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem3, Me.ToolStripSeparator1, Me.AddNewTSMI, Me.ToolStripSeparator9, Me.ToolStripMenuItem2, Me.ToolStripMenuItem5, Me.CopiarParaAareaDeTransferenciaToolStripMenuItem, Me.ExibirTudoToolStripMenuItem, Me.ZoomPadrao100ToolStripMenuItem, Me.CentralizarToolStripMenuItem})
        Me.CMS_NoSel.Name = "ContextMenuStrip1"
        '
        'ToolStripMenuItem3
        '
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        '
        'ToolStripSeparator1
        '
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        '
        'AddNewTSMI
        '
        resources.ApplyResources(Me.AddNewTSMI, "AddNewTSMI")
        Me.AddNewTSMI.Image = Global.DWSIM.My.Resources.Resources.add
        Me.AddNewTSMI.Name = "AddNewTSMI"
        '
        'ToolStripSeparator9
        '
        resources.ApplyResources(Me.ToolStripSeparator9, "ToolStripSeparator9")
        Me.ToolStripSeparator9.Name = "ToolStripSeparator9"
        '
        'ToolStripMenuItem2
        '
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
        Me.ToolStripMenuItem2.Image = Global.DWSIM.My.Resources.Resources.page_white_paint
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        '
        'ToolStripMenuItem5
        '
        resources.ApplyResources(Me.ToolStripMenuItem5, "ToolStripMenuItem5")
        Me.ToolStripMenuItem5.Image = Global.DWSIM.My.Resources.Resources.printer
        Me.ToolStripMenuItem5.Name = "ToolStripMenuItem5"
        '
        'CopiarParaAareaDeTransferenciaToolStripMenuItem
        '
        resources.ApplyResources(Me.CopiarParaAareaDeTransferenciaToolStripMenuItem, "CopiarParaAareaDeTransferenciaToolStripMenuItem")
        Me.CopiarParaAareaDeTransferenciaToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1, Me.ToolStripMenuItem4, Me.ToolStripMenuItem8, Me.ToolStripMenuItem10})
        Me.CopiarParaAareaDeTransferenciaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.images
        Me.CopiarParaAareaDeTransferenciaToolStripMenuItem.Name = "CopiarParaAareaDeTransferenciaToolStripMenuItem"
        '
        'ToolStripMenuItem1
        '
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        Me.ToolStripMenuItem1.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        '
        'ToolStripMenuItem4
        '
        resources.ApplyResources(Me.ToolStripMenuItem4, "ToolStripMenuItem4")
        Me.ToolStripMenuItem4.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.ToolStripMenuItem4.Name = "ToolStripMenuItem4"
        '
        'ToolStripMenuItem8
        '
        resources.ApplyResources(Me.ToolStripMenuItem8, "ToolStripMenuItem8")
        Me.ToolStripMenuItem8.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.ToolStripMenuItem8.Name = "ToolStripMenuItem8"
        '
        'ToolStripMenuItem10
        '
        resources.ApplyResources(Me.ToolStripMenuItem10, "ToolStripMenuItem10")
        Me.ToolStripMenuItem10.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.ToolStripMenuItem10.Name = "ToolStripMenuItem10"
        '
        'ExibirTudoToolStripMenuItem
        '
        resources.ApplyResources(Me.ExibirTudoToolStripMenuItem, "ExibirTudoToolStripMenuItem")
        Me.ExibirTudoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom_extend
        Me.ExibirTudoToolStripMenuItem.Name = "ExibirTudoToolStripMenuItem"
        '
        'ZoomPadrao100ToolStripMenuItem
        '
        resources.ApplyResources(Me.ZoomPadrao100ToolStripMenuItem, "ZoomPadrao100ToolStripMenuItem")
        Me.ZoomPadrao100ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom_refresh
        Me.ZoomPadrao100ToolStripMenuItem.Name = "ZoomPadrao100ToolStripMenuItem"
        '
        'CentralizarToolStripMenuItem
        '
        resources.ApplyResources(Me.CentralizarToolStripMenuItem, "CentralizarToolStripMenuItem")
        Me.CentralizarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.zoom
        Me.CentralizarToolStripMenuItem.Name = "CentralizarToolStripMenuItem"
        '
        'CMS_Sel
        '
        resources.ApplyResources(Me.CMS_Sel, "CMS_Sel")
        Me.CMS_Sel.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSMI_Label, Me.AtivadoToolStripMenuItem, Me.ToolStripSeparator3, Me.RecalcularToolStripMenuItem, Me.CopyFromTSMI, Me.DepurarObjetoToolStripMenuItem, Me.ToolStripSeparator6, Me.ConectarAToolStripMenuItem, Me.DesconectarDeToolStripMenuItem, Me.ToolStripSeparator4, Me.TSMI_Girar, Me.HorizontalmenteToolStripMenuItem, Me.ToolStripSeparator2, Me.ClonarToolStripMenuItem, Me.ExcluirToolStripMenuItem, Me.ToolStripSeparator5, Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem, Me.ToolStripSeparator7, Me.EditAppearanceToolStripMenuItem, Me.ToolStripSeparator8, Me.SplitToolStripMenuItem, Me.MergeStreamsToolStripMenuItem})
        Me.CMS_Sel.Name = "CMS_Sel"
        '
        'TSMI_Label
        '
        resources.ApplyResources(Me.TSMI_Label, "TSMI_Label")
        Me.TSMI_Label.Name = "TSMI_Label"
        '
        'AtivadoToolStripMenuItem
        '
        resources.ApplyResources(Me.AtivadoToolStripMenuItem, "AtivadoToolStripMenuItem")
        Me.AtivadoToolStripMenuItem.CheckOnClick = True
        Me.AtivadoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.accept
        Me.AtivadoToolStripMenuItem.Name = "AtivadoToolStripMenuItem"
        '
        'ToolStripSeparator3
        '
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        '
        'RecalcularToolStripMenuItem
        '
        resources.ApplyResources(Me.RecalcularToolStripMenuItem, "RecalcularToolStripMenuItem")
        Me.RecalcularToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        Me.RecalcularToolStripMenuItem.Name = "RecalcularToolStripMenuItem"
        '
        'CopyFromTSMI
        '
        resources.ApplyResources(Me.CopyFromTSMI, "CopyFromTSMI")
        Me.CopyFromTSMI.Image = Global.DWSIM.My.Resources.Resources.table_row_insert1
        Me.CopyFromTSMI.Name = "CopyFromTSMI"
        '
        'DepurarObjetoToolStripMenuItem
        '
        resources.ApplyResources(Me.DepurarObjetoToolStripMenuItem, "DepurarObjetoToolStripMenuItem")
        Me.DepurarObjetoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_osx_terminal
        Me.DepurarObjetoToolStripMenuItem.Name = "DepurarObjetoToolStripMenuItem"
        '
        'ToolStripSeparator6
        '
        resources.ApplyResources(Me.ToolStripSeparator6, "ToolStripSeparator6")
        Me.ToolStripSeparator6.Name = "ToolStripSeparator6"
        '
        'ConectarAToolStripMenuItem
        '
        resources.ApplyResources(Me.ConectarAToolStripMenuItem, "ConectarAToolStripMenuItem")
        Me.ConectarAToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.connect
        Me.ConectarAToolStripMenuItem.Name = "ConectarAToolStripMenuItem"
        '
        'DesconectarDeToolStripMenuItem
        '
        resources.ApplyResources(Me.DesconectarDeToolStripMenuItem, "DesconectarDeToolStripMenuItem")
        Me.DesconectarDeToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.disconnect
        Me.DesconectarDeToolStripMenuItem.Name = "DesconectarDeToolStripMenuItem"
        '
        'ToolStripSeparator4
        '
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        '
        'TSMI_Girar
        '
        resources.ApplyResources(Me.TSMI_Girar, "TSMI_Girar")
        Me.TSMI_Girar.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem6, Me.BToolStripMenuItem, Me.ToolStripMenuItem7, Me.ToolStripMenuItem11, Me.ToolStripMenuItem12, Me.ToolStripMenuItem13, Me.ToolStripMenuItem14})
        Me.TSMI_Girar.Image = Global.DWSIM.My.Resources.Resources.arrow_rotate_clockwise
        Me.TSMI_Girar.Name = "TSMI_Girar"
        '
        'ToolStripMenuItem6
        '
        resources.ApplyResources(Me.ToolStripMenuItem6, "ToolStripMenuItem6")
        Me.ToolStripMenuItem6.Image = Global.DWSIM.My.Resources.Resources.arrow_rotate_clockwise
        Me.ToolStripMenuItem6.Name = "ToolStripMenuItem6"
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
        'HorizontalmenteToolStripMenuItem
        '
        resources.ApplyResources(Me.HorizontalmenteToolStripMenuItem, "HorizontalmenteToolStripMenuItem")
        Me.HorizontalmenteToolStripMenuItem.CheckOnClick = True
        Me.HorizontalmenteToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_flip_horizontal
        Me.HorizontalmenteToolStripMenuItem.Name = "HorizontalmenteToolStripMenuItem"
        '
        'ToolStripSeparator2
        '
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        '
        'ClonarToolStripMenuItem
        '
        resources.ApplyResources(Me.ClonarToolStripMenuItem, "ClonarToolStripMenuItem")
        Me.ClonarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.sheep
        Me.ClonarToolStripMenuItem.Name = "ClonarToolStripMenuItem"
        '
        'ExcluirToolStripMenuItem
        '
        resources.ApplyResources(Me.ExcluirToolStripMenuItem, "ExcluirToolStripMenuItem")
        Me.ExcluirToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.ExcluirToolStripMenuItem.Name = "ExcluirToolStripMenuItem"
        '
        'ToolStripSeparator5
        '
        resources.ApplyResources(Me.ToolStripSeparator5, "ToolStripSeparator5")
        Me.ToolStripSeparator5.Name = "ToolStripSeparator5"
        '
        'CopiarDadosParaareaDeTransferenciaToolStripMenuItem
        '
        resources.ApplyResources(Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem, "CopiarDadosParaareaDeTransferenciaToolStripMenuItem")
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.clipboard_sign
        Me.CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Name = "CopiarDadosParaareaDeTransferenciaToolStripMenuItem"
        '
        'ToolStripSeparator7
        '
        resources.ApplyResources(Me.ToolStripSeparator7, "ToolStripSeparator7")
        Me.ToolStripSeparator7.Name = "ToolStripSeparator7"
        '
        'EditAppearanceToolStripMenuItem
        '
        resources.ApplyResources(Me.EditAppearanceToolStripMenuItem, "EditAppearanceToolStripMenuItem")
        Me.EditAppearanceToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_square_edit
        Me.EditAppearanceToolStripMenuItem.Name = "EditAppearanceToolStripMenuItem"
        '
        'ToolStripSeparator8
        '
        resources.ApplyResources(Me.ToolStripSeparator8, "ToolStripSeparator8")
        Me.ToolStripSeparator8.Name = "ToolStripSeparator8"
        '
        'SplitToolStripMenuItem
        '
        resources.ApplyResources(Me.SplitToolStripMenuItem, "SplitToolStripMenuItem")
        Me.SplitToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_divide
        Me.SplitToolStripMenuItem.Name = "SplitToolStripMenuItem"
        '
        'MergeStreamsToolStripMenuItem
        '
        resources.ApplyResources(Me.MergeStreamsToolStripMenuItem, "MergeStreamsToolStripMenuItem")
        Me.MergeStreamsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_join
        Me.MergeStreamsToolStripMenuItem.Name = "MergeStreamsToolStripMenuItem"
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
        Me.designSurfacePrintDocument.DocumentName = "documento"
        '
        'CMS_ItemsToConnect
        '
        resources.ApplyResources(Me.CMS_ItemsToConnect, "CMS_ItemsToConnect")
        Me.CMS_ItemsToConnect.Name = "CMS_ItemsToConnect"
        '
        'CMS_ItemsToDisconnect
        '
        resources.ApplyResources(Me.CMS_ItemsToDisconnect, "CMS_ItemsToDisconnect")
        Me.CMS_ItemsToDisconnect.Name = "CMS_ItemsToConnect"
        '
        'pageSetup
        '
        Me.pageSetup.Document = Me.designSurfacePrintDocument
        '
        'setupPrint
        '
        Me.setupPrint.AllowCurrentPage = True
        Me.setupPrint.Document = Me.designSurfacePrintDocument
        Me.setupPrint.UseEXDialog = True
        '
        'Timer1
        '
        Me.Timer1.Enabled = True
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
        'PanelSearch
        '
        resources.ApplyResources(Me.PanelSearch, "PanelSearch")
        Me.PanelSearch.BackColor = System.Drawing.Color.SteelBlue
        Me.PanelSearch.Controls.Add(Me.tbSearch)
        Me.PanelSearch.Controls.Add(Me.Label1)
        Me.PanelSearch.Controls.Add(Me.FlowLayoutPanel1)
        Me.PanelSearch.Name = "PanelSearch"
        '
        'tbSearch
        '
        resources.ApplyResources(Me.tbSearch, "tbSearch")
        Me.tbSearch.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.tbSearch.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        Me.tbSearch.Name = "tbSearch"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Name = "Label1"
        '
        'FlowLayoutPanel1
        '
        resources.ApplyResources(Me.FlowLayoutPanel1, "FlowLayoutPanel1")
        Me.FlowLayoutPanel1.Controls.Add(Me.Button1)
        Me.FlowLayoutPanel1.Name = "FlowLayoutPanel1"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.BackgroundImage = Global.DWSIM.My.Resources.Resources.Search
        Me.Button1.ForeColor = System.Drawing.Color.SteelBlue
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'ToolStripFlowsheet
        '
        resources.ApplyResources(Me.ToolStripFlowsheet, "ToolStripFlowsheet")
        Me.ToolStripFlowsheet.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbCutObj, Me.tsbCopyObj, Me.tsbPasteObj, Me.ToolStripSeparator12, Me.ToolStripButton12, Me.TSBtabela, Me.TSBTexto, Me.ToolStripButton4, Me.ToolStripButton6, Me.ToolStripButton19, Me.ToolStripSeparator10, Me.ToolStripButton1, Me.TSTBZoom, Me.ToolStripButton2, Me.ToolStripButton20, Me.ToolStripButton3, Me.ToolStripSeparator11, Me.tsbResizeMode, Me.tsbResizeModeKeepAR, Me.ToolStripSeparator13, Me.ToolStripButton16, Me.ToolStripButton17, Me.ToolStripSeparator14, Me.ToolStripButton18, Me.tsbConfigPage, Me.tsbConfigPrinter, Me.tsbPrint, Me.ToolStripSeparator15, Me.tsbAlignLefts, Me.tsbAlignCenters, Me.tsbAlignRights, Me.tsbAlignTops, Me.tsbAlignMiddles, Me.tsbAlignBottoms, Me.ToolStripSeparator16, Me.tsbAlignVertical, Me.tsbAlignHorizontal})
        Me.ToolStripFlowsheet.Name = "ToolStripFlowsheet"
        '
        'tsbCutObj
        '
        resources.ApplyResources(Me.tsbCutObj, "tsbCutObj")
        Me.tsbCutObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbCutObj.Image = Global.DWSIM.My.Resources.Resources.cut
        Me.tsbCutObj.Name = "tsbCutObj"
        '
        'tsbCopyObj
        '
        resources.ApplyResources(Me.tsbCopyObj, "tsbCopyObj")
        Me.tsbCopyObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbCopyObj.Image = Global.DWSIM.My.Resources.Resources.copy
        Me.tsbCopyObj.Name = "tsbCopyObj"
        '
        'tsbPasteObj
        '
        resources.ApplyResources(Me.tsbPasteObj, "tsbPasteObj")
        Me.tsbPasteObj.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbPasteObj.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        Me.tsbPasteObj.Name = "tsbPasteObj"
        '
        'ToolStripSeparator12
        '
        resources.ApplyResources(Me.ToolStripSeparator12, "ToolStripSeparator12")
        Me.ToolStripSeparator12.Name = "ToolStripSeparator12"
        '
        'ToolStripButton12
        '
        resources.ApplyResources(Me.ToolStripButton12, "ToolStripButton12")
        Me.ToolStripButton12.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton12.Image = Global.DWSIM.My.Resources.Resources.shape_square
        Me.ToolStripButton12.Name = "ToolStripButton12"
        '
        'TSBtabela
        '
        resources.ApplyResources(Me.TSBtabela, "TSBtabela")
        Me.TSBtabela.CheckOnClick = True
        Me.TSBtabela.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBtabela.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.TSBtabela.Name = "TSBtabela"
        '
        'TSBTexto
        '
        resources.ApplyResources(Me.TSBTexto, "TSBTexto")
        Me.TSBTexto.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSBTexto.Image = Global.DWSIM.My.Resources.Resources.font
        Me.TSBTexto.Name = "TSBTexto"
        '
        'ToolStripButton4
        '
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.My.Resources.Resources.table_relationship
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripButton6
        '
        resources.ApplyResources(Me.ToolStripButton6, "ToolStripButton6")
        Me.ToolStripButton6.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton6.Image = Global.DWSIM.My.Resources.Resources.table_lightning
        Me.ToolStripButton6.Name = "ToolStripButton6"
        '
        'ToolStripButton19
        '
        resources.ApplyResources(Me.ToolStripButton19, "ToolStripButton19")
        Me.ToolStripButton19.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton19.Image = Global.DWSIM.My.Resources.Resources.table
        Me.ToolStripButton19.Name = "ToolStripButton19"
        '
        'ToolStripSeparator10
        '
        resources.ApplyResources(Me.ToolStripSeparator10, "ToolStripSeparator10")
        Me.ToolStripSeparator10.Name = "ToolStripSeparator10"
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.zoom_out
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'TSTBZoom
        '
        resources.ApplyResources(Me.TSTBZoom, "TSTBZoom")
        Me.TSTBZoom.BackColor = System.Drawing.SystemColors.Control
        Me.TSTBZoom.Name = "TSTBZoom"
        '
        'ToolStripButton2
        '
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.zoom_in
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton20
        '
        resources.ApplyResources(Me.ToolStripButton20, "ToolStripButton20")
        Me.ToolStripButton20.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton20.Image = Global.DWSIM.My.Resources.Resources.zoom_extend
        Me.ToolStripButton20.Name = "ToolStripButton20"
        '
        'ToolStripButton3
        '
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.My.Resources.Resources.zoom_refresh
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripSeparator11
        '
        resources.ApplyResources(Me.ToolStripSeparator11, "ToolStripSeparator11")
        Me.ToolStripSeparator11.Name = "ToolStripSeparator11"
        '
        'tsbResizeMode
        '
        resources.ApplyResources(Me.tsbResizeMode, "tsbResizeMode")
        Me.tsbResizeMode.CheckOnClick = True
        Me.tsbResizeMode.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbResizeMode.Image = Global.DWSIM.My.Resources.Resources.shape_handles
        Me.tsbResizeMode.Name = "tsbResizeMode"
        '
        'tsbResizeModeKeepAR
        '
        resources.ApplyResources(Me.tsbResizeModeKeepAR, "tsbResizeModeKeepAR")
        Me.tsbResizeModeKeepAR.Checked = True
        Me.tsbResizeModeKeepAR.CheckOnClick = True
        Me.tsbResizeModeKeepAR.CheckState = System.Windows.Forms.CheckState.Checked
        Me.tsbResizeModeKeepAR.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbResizeModeKeepAR.Image = Global.DWSIM.My.Resources.Resources.arrow_nw_se
        Me.tsbResizeModeKeepAR.Name = "tsbResizeModeKeepAR"
        '
        'ToolStripSeparator13
        '
        resources.ApplyResources(Me.ToolStripSeparator13, "ToolStripSeparator13")
        Me.ToolStripSeparator13.Name = "ToolStripSeparator13"
        '
        'ToolStripButton16
        '
        resources.ApplyResources(Me.ToolStripButton16, "ToolStripButton16")
        Me.ToolStripButton16.CheckOnClick = True
        Me.ToolStripButton16.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton16.Image = Global.DWSIM.My.Resources.Resources.shading
        Me.ToolStripButton16.Name = "ToolStripButton16"
        '
        'ToolStripButton17
        '
        resources.ApplyResources(Me.ToolStripButton17, "ToolStripButton17")
        Me.ToolStripButton17.CheckOnClick = True
        Me.ToolStripButton17.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton17.Image = Global.DWSIM.My.Resources.Resources.connect
        Me.ToolStripButton17.Name = "ToolStripButton17"
        '
        'ToolStripSeparator14
        '
        resources.ApplyResources(Me.ToolStripSeparator14, "ToolStripSeparator14")
        Me.ToolStripSeparator14.Name = "ToolStripSeparator14"
        '
        'ToolStripButton18
        '
        resources.ApplyResources(Me.ToolStripButton18, "ToolStripButton18")
        Me.ToolStripButton18.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton18.Image = Global.DWSIM.My.Resources.Resources.images
        Me.ToolStripButton18.Name = "ToolStripButton18"
        '
        'tsbConfigPage
        '
        resources.ApplyResources(Me.tsbConfigPage, "tsbConfigPage")
        Me.tsbConfigPage.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbConfigPage.Image = Global.DWSIM.My.Resources.Resources.page_gear
        Me.tsbConfigPage.Name = "tsbConfigPage"
        '
        'tsbConfigPrinter
        '
        resources.ApplyResources(Me.tsbConfigPrinter, "tsbConfigPrinter")
        Me.tsbConfigPrinter.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbConfigPrinter.Image = Global.DWSIM.My.Resources.Resources.printer
        Me.tsbConfigPrinter.Name = "tsbConfigPrinter"
        '
        'tsbPrint
        '
        resources.ApplyResources(Me.tsbPrint, "tsbPrint")
        Me.tsbPrint.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbPrint.Image = Global.DWSIM.My.Resources.Resources.printer_add
        Me.tsbPrint.Name = "tsbPrint"
        '
        'ToolStripSeparator15
        '
        resources.ApplyResources(Me.ToolStripSeparator15, "ToolStripSeparator15")
        Me.ToolStripSeparator15.Name = "ToolStripSeparator15"
        '
        'tsbAlignLefts
        '
        resources.ApplyResources(Me.tsbAlignLefts, "tsbAlignLefts")
        Me.tsbAlignLefts.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignLefts.Image = Global.DWSIM.My.Resources.Resources.shape_align_left
        Me.tsbAlignLefts.Name = "tsbAlignLefts"
        '
        'tsbAlignCenters
        '
        resources.ApplyResources(Me.tsbAlignCenters, "tsbAlignCenters")
        Me.tsbAlignCenters.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignCenters.Image = Global.DWSIM.My.Resources.Resources.shape_align_center
        Me.tsbAlignCenters.Name = "tsbAlignCenters"
        '
        'tsbAlignRights
        '
        resources.ApplyResources(Me.tsbAlignRights, "tsbAlignRights")
        Me.tsbAlignRights.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignRights.Image = Global.DWSIM.My.Resources.Resources.shape_align_right
        Me.tsbAlignRights.Name = "tsbAlignRights"
        '
        'tsbAlignTops
        '
        resources.ApplyResources(Me.tsbAlignTops, "tsbAlignTops")
        Me.tsbAlignTops.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignTops.Image = Global.DWSIM.My.Resources.Resources.shape_align_top
        Me.tsbAlignTops.Name = "tsbAlignTops"
        '
        'tsbAlignMiddles
        '
        resources.ApplyResources(Me.tsbAlignMiddles, "tsbAlignMiddles")
        Me.tsbAlignMiddles.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignMiddles.Image = Global.DWSIM.My.Resources.Resources.shape_align_middle
        Me.tsbAlignMiddles.Name = "tsbAlignMiddles"
        '
        'tsbAlignBottoms
        '
        resources.ApplyResources(Me.tsbAlignBottoms, "tsbAlignBottoms")
        Me.tsbAlignBottoms.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignBottoms.Image = Global.DWSIM.My.Resources.Resources.shape_align_bottom
        Me.tsbAlignBottoms.Name = "tsbAlignBottoms"
        '
        'ToolStripSeparator16
        '
        resources.ApplyResources(Me.ToolStripSeparator16, "ToolStripSeparator16")
        Me.ToolStripSeparator16.Name = "ToolStripSeparator16"
        '
        'tsbAlignVertical
        '
        resources.ApplyResources(Me.tsbAlignVertical, "tsbAlignVertical")
        Me.tsbAlignVertical.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignVertical.Image = Global.DWSIM.My.Resources.Resources.shape_align_center1
        Me.tsbAlignVertical.Name = "tsbAlignVertical"
        '
        'tsbAlignHorizontal
        '
        resources.ApplyResources(Me.tsbAlignHorizontal, "tsbAlignHorizontal")
        Me.tsbAlignHorizontal.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAlignHorizontal.Image = Global.DWSIM.My.Resources.Resources.shape_align_middle1
        Me.tsbAlignHorizontal.Name = "tsbAlignHorizontal"
        '
        'FlowsheetSurface
        '
        resources.ApplyResources(Me, "$this")
        Me.AllowDrop = True
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.ToolStripFlowsheet)
        Me.Controls.Add(Me.PanelSearch)
        Me.Controls.Add(Me.FlowsheetDesignSurface)
        Me.DoubleBuffered = True
        Me.Name = "FlowsheetSurface"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.TabText = Me.Text
        Me.CMS_NoSel.ResumeLayout(False)
        Me.CMS_Sel.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.PanelSearch.ResumeLayout(False)
        Me.PanelSearch.PerformLayout()
        Me.FlowLayoutPanel1.ResumeLayout(False)
        Me.ToolStripFlowsheet.ResumeLayout(False)
        Me.ToolStripFlowsheet.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents FlowsheetDesignSurface As DrawingTools.GraphicsSurface
    Public WithEvents CMS_NoSel As System.Windows.Forms.ContextMenuStrip
    Public WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem5 As System.Windows.Forms.ToolStripMenuItem
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
    Friend WithEvents CopiarParaAareaDeTransferenciaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem4 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem8 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripMenuItem10 As System.Windows.Forms.ToolStripMenuItem
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
    Friend WithEvents EditAppearanceToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PanelSearch As System.Windows.Forms.Panel
    Friend WithEvents FlowLayoutPanel1 As System.Windows.Forms.FlowLayoutPanel
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents tbSearch As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents AtivadoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SplitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents MergeStreamsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator8 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents AddNewTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator9 As System.Windows.Forms.ToolStripSeparator
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
    Friend WithEvents ToolStripButton16 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripButton17 As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator14 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ToolStripButton18 As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbConfigPage As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbConfigPrinter As System.Windows.Forms.ToolStripButton
    Public WithEvents tsbPrint As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator15 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbAlignLefts As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignCenters As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignRights As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignTops As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignMiddles As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignBottoms As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator16 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbAlignVertical As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbAlignHorizontal As System.Windows.Forms.ToolStripButton
End Class
