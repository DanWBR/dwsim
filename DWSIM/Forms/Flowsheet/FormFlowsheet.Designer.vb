<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormFlowsheet
    Inherits System.Windows.Forms.Form

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormFlowsheet))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.TSTable = New FarsiLibrary.Win.FATabStripItem()
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.ArquivoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CloseToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiUndo = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiRedo = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator14 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsmiCut = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiCopy = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiPaste = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiRemoveSelected = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiCloneSelected = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiRecalc = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiExportData = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator18 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsmiConfigSimulation = New System.Windows.Forms.ToolStripMenuItem()
        Me.InserirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.BlocoDeSimulacaoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TabelaDePropriedadesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TabelaDePropriedatesMestraToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TabelaDePropriedadesPlanilhaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FiguraToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TextoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RectangleToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GraficoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FerramentasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CompoundCreatorWizardTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.PropriedadesDasSubstânciasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.InspectorTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator15 = New System.Windows.Forms.ToolStripSeparator()
        Me.UtilitiesTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.TSMIAddUtility = New System.Windows.Forms.ToolStripMenuItem()
        Me.OtimizaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AnaliseDeSensibilidadeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.MultivariateOptimizerToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ScriptsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.IronRubyToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ResultadosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GerarRelatorioToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PluginsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExibirToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExibirListaDeItensACalcularToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.varpaneltsmi = New System.Windows.Forms.ToolStripMenuItem()
        Me.COObjTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ConsoleOutputTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator10 = New System.Windows.Forms.ToolStripSeparator()
        Me.showflowsheettoolstripmenuitem = New System.Windows.Forms.ToolStripMenuItem()
        Me.showunitstoolstripmenuitem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator11 = New System.Windows.Forms.ToolStripSeparator()
        Me.RestoreLayoutTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiCloseOpenedEditors = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem3 = New System.Windows.Forms.ToolStripMenuItem()
        Me.BGLoadComp = New System.ComponentModel.BackgroundWorker()
        Me.bgCalc = New System.ComponentModel.BackgroundWorker()
        Me.dckPanel = New WeifenLuo.WinFormsUI.Docking.DockPanel()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.OpenFileName = New System.Windows.Forms.OpenFileDialog()
        Me.TimerScripts1 = New System.Windows.Forms.Timer(Me.components)
        Me.TimerScripts5 = New System.Windows.Forms.Timer(Me.components)
        Me.TimerScripts15 = New System.Windows.Forms.Timer(Me.components)
        Me.TimerScripts30 = New System.Windows.Forms.Timer(Me.components)
        Me.TimerScripts60 = New System.Windows.Forms.Timer(Me.components)
        Me.QuestionBox_Panel = New System.Windows.Forms.Panel()
        Me.QuestionBox_Button2 = New System.Windows.Forms.Button()
        Me.QuestionBox_Button1 = New System.Windows.Forms.Button()
        Me.QuestionBox_PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.QuestionBox_Label1 = New System.Windows.Forms.Label()
        Me.PanelMobileCompatMode = New System.Windows.Forms.Panel()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbAtivar = New System.Windows.Forms.ToolStripButton()
        Me.tsbCalcF = New System.Windows.Forms.ToolStripButton()
        Me.tsbCalc = New System.Windows.Forms.ToolStripButton()
        Me.tsbAbortCalc = New System.Windows.Forms.ToolStripButton()
        Me.tsbSimultAdjustSolver = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbUndo = New System.Windows.Forms.ToolStripDropDownButton()
        Me.tsbRedo = New System.Windows.Forms.ToolStripDropDownButton()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TSTable.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.QuestionBox_Panel.SuspendLayout()
        CType(Me.QuestionBox_PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.PanelMobileCompatMode.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'DataGridView1
        '
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridView1.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Window
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.ControlText
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        Me.DataGridView1.DefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridView1.Name = "DataGridView1"
        DataGridViewCellStyle3.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle3.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle3.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle3.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle3.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle3.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle3.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridView1.RowHeadersDefaultCellStyle = DataGridViewCellStyle3
        '
        'TSTable
        '
        resources.ApplyResources(Me.TSTable, "TSTable")
        Me.TSTable.CanClose = False
        Me.TSTable.Controls.Add(Me.DataGridView1)
        Me.TSTable.IsDrawn = True
        Me.TSTable.Name = "TSTable"
        '
        'MenuStrip1
        '
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.AllowItemReorder = True
        Me.MenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ArquivoToolStripMenuItem, Me.EditarToolStripMenuItem, Me.InserirToolStripMenuItem, Me.FerramentasToolStripMenuItem, Me.UtilitiesTSMI, Me.OtimizaToolStripMenuItem, Me.ScriptsToolStripMenuItem, Me.ResultadosToolStripMenuItem, Me.PluginsToolStripMenuItem, Me.ExibirToolStripMenuItem})
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.ShowItemToolTips = True
        '
        'ArquivoToolStripMenuItem
        '
        resources.ApplyResources(Me.ArquivoToolStripMenuItem, "ArquivoToolStripMenuItem")
        Me.ArquivoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CloseToolStripMenuItem})
        Me.ArquivoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.ArquivoToolStripMenuItem.MergeIndex = 0
        Me.ArquivoToolStripMenuItem.Name = "ArquivoToolStripMenuItem"
        '
        'CloseToolStripMenuItem
        '
        resources.ApplyResources(Me.CloseToolStripMenuItem, "CloseToolStripMenuItem")
        Me.CloseToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.CloseToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CloseToolStripMenuItem.MergeIndex = 11
        Me.CloseToolStripMenuItem.Name = "CloseToolStripMenuItem"
        Me.CloseToolStripMenuItem.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        '
        'EditarToolStripMenuItem
        '
        resources.ApplyResources(Me.EditarToolStripMenuItem, "EditarToolStripMenuItem")
        Me.EditarToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsmiUndo, Me.tsmiRedo, Me.ToolStripSeparator14, Me.tsmiCut, Me.tsmiCopy, Me.tsmiPaste, Me.tsmiRemoveSelected, Me.tsmiCloneSelected, Me.tsmiRecalc, Me.tsmiExportData, Me.ToolStripSeparator18, Me.tsmiConfigSimulation})
        Me.EditarToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.EditarToolStripMenuItem.MergeIndex = 1
        Me.EditarToolStripMenuItem.Name = "EditarToolStripMenuItem"
        '
        'tsmiUndo
        '
        resources.ApplyResources(Me.tsmiUndo, "tsmiUndo")
        Me.tsmiUndo.Image = Global.DWSIM.My.Resources.Resources.undo_161
        Me.tsmiUndo.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiUndo.MergeIndex = 0
        Me.tsmiUndo.Name = "tsmiUndo"
        '
        'tsmiRedo
        '
        resources.ApplyResources(Me.tsmiRedo, "tsmiRedo")
        Me.tsmiRedo.Image = Global.DWSIM.My.Resources.Resources.redo_16
        Me.tsmiRedo.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiRedo.MergeIndex = 1
        Me.tsmiRedo.Name = "tsmiRedo"
        '
        'ToolStripSeparator14
        '
        resources.ApplyResources(Me.ToolStripSeparator14, "ToolStripSeparator14")
        Me.ToolStripSeparator14.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator14.MergeIndex = 2
        Me.ToolStripSeparator14.Name = "ToolStripSeparator14"
        Me.ToolStripSeparator14.Overflow = System.Windows.Forms.ToolStripItemOverflow.Never
        '
        'tsmiCut
        '
        resources.ApplyResources(Me.tsmiCut, "tsmiCut")
        Me.tsmiCut.Image = Global.DWSIM.My.Resources.Resources.cut
        Me.tsmiCut.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCut.MergeIndex = 3
        Me.tsmiCut.Name = "tsmiCut"
        '
        'tsmiCopy
        '
        resources.ApplyResources(Me.tsmiCopy, "tsmiCopy")
        Me.tsmiCopy.Image = Global.DWSIM.My.Resources.Resources.page_copy
        Me.tsmiCopy.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCopy.MergeIndex = 4
        Me.tsmiCopy.Name = "tsmiCopy"
        '
        'tsmiPaste
        '
        resources.ApplyResources(Me.tsmiPaste, "tsmiPaste")
        Me.tsmiPaste.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        Me.tsmiPaste.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiPaste.MergeIndex = 5
        Me.tsmiPaste.Name = "tsmiPaste"
        '
        'tsmiRemoveSelected
        '
        resources.ApplyResources(Me.tsmiRemoveSelected, "tsmiRemoveSelected")
        Me.tsmiRemoveSelected.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.tsmiRemoveSelected.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiRemoveSelected.MergeIndex = 6
        Me.tsmiRemoveSelected.Name = "tsmiRemoveSelected"
        '
        'tsmiCloneSelected
        '
        resources.ApplyResources(Me.tsmiCloneSelected, "tsmiCloneSelected")
        Me.tsmiCloneSelected.Image = Global.DWSIM.My.Resources.Resources.sheep
        Me.tsmiCloneSelected.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCloneSelected.MergeIndex = 7
        Me.tsmiCloneSelected.Name = "tsmiCloneSelected"
        '
        'tsmiRecalc
        '
        resources.ApplyResources(Me.tsmiRecalc, "tsmiRecalc")
        Me.tsmiRecalc.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        Me.tsmiRecalc.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiRecalc.MergeIndex = 8
        Me.tsmiRecalc.Name = "tsmiRecalc"
        '
        'tsmiExportData
        '
        resources.ApplyResources(Me.tsmiExportData, "tsmiExportData")
        Me.tsmiExportData.Image = Global.DWSIM.My.Resources.Resources.clipboard_sign
        Me.tsmiExportData.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiExportData.MergeIndex = 9
        Me.tsmiExportData.Name = "tsmiExportData"
        '
        'ToolStripSeparator18
        '
        resources.ApplyResources(Me.ToolStripSeparator18, "ToolStripSeparator18")
        Me.ToolStripSeparator18.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator18.MergeIndex = 10
        Me.ToolStripSeparator18.Name = "ToolStripSeparator18"
        '
        'tsmiConfigSimulation
        '
        resources.ApplyResources(Me.tsmiConfigSimulation, "tsmiConfigSimulation")
        Me.tsmiConfigSimulation.Image = Global.DWSIM.My.Resources.Resources.brick
        Me.tsmiConfigSimulation.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiConfigSimulation.MergeIndex = 11
        Me.tsmiConfigSimulation.Name = "tsmiConfigSimulation"
        '
        'InserirToolStripMenuItem
        '
        resources.ApplyResources(Me.InserirToolStripMenuItem, "InserirToolStripMenuItem")
        Me.InserirToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BlocoDeSimulacaoToolStripMenuItem, Me.TabelaDePropriedadesToolStripMenuItem, Me.TabelaDePropriedatesMestraToolStripMenuItem, Me.TabelaDePropriedadesPlanilhaToolStripMenuItem, Me.FiguraToolStripMenuItem, Me.TextoToolStripMenuItem, Me.RectangleToolStripMenuItem, Me.GraficoToolStripMenuItem})
        Me.InserirToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.InserirToolStripMenuItem.MergeIndex = 2
        Me.InserirToolStripMenuItem.Name = "InserirToolStripMenuItem"
        '
        'BlocoDeSimulacaoToolStripMenuItem
        '
        resources.ApplyResources(Me.BlocoDeSimulacaoToolStripMenuItem, "BlocoDeSimulacaoToolStripMenuItem")
        Me.BlocoDeSimulacaoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.uo_custom_16
        Me.BlocoDeSimulacaoToolStripMenuItem.Name = "BlocoDeSimulacaoToolStripMenuItem"
        '
        'TabelaDePropriedadesToolStripMenuItem
        '
        resources.ApplyResources(Me.TabelaDePropriedadesToolStripMenuItem, "TabelaDePropriedadesToolStripMenuItem")
        Me.TabelaDePropriedadesToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table_lightning
        Me.TabelaDePropriedadesToolStripMenuItem.Name = "TabelaDePropriedadesToolStripMenuItem"
        '
        'TabelaDePropriedatesMestraToolStripMenuItem
        '
        resources.ApplyResources(Me.TabelaDePropriedatesMestraToolStripMenuItem, "TabelaDePropriedatesMestraToolStripMenuItem")
        Me.TabelaDePropriedatesMestraToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table
        Me.TabelaDePropriedatesMestraToolStripMenuItem.Name = "TabelaDePropriedatesMestraToolStripMenuItem"
        '
        'TabelaDePropriedadesPlanilhaToolStripMenuItem
        '
        resources.ApplyResources(Me.TabelaDePropriedadesPlanilhaToolStripMenuItem, "TabelaDePropriedadesPlanilhaToolStripMenuItem")
        Me.TabelaDePropriedadesPlanilhaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table_relationship
        Me.TabelaDePropriedadesPlanilhaToolStripMenuItem.Name = "TabelaDePropriedadesPlanilhaToolStripMenuItem"
        '
        'FiguraToolStripMenuItem
        '
        resources.ApplyResources(Me.FiguraToolStripMenuItem, "FiguraToolStripMenuItem")
        Me.FiguraToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.FiguraToolStripMenuItem.Name = "FiguraToolStripMenuItem"
        '
        'TextoToolStripMenuItem
        '
        resources.ApplyResources(Me.TextoToolStripMenuItem, "TextoToolStripMenuItem")
        Me.TextoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.font
        Me.TextoToolStripMenuItem.Name = "TextoToolStripMenuItem"
        '
        'RectangleToolStripMenuItem
        '
        resources.ApplyResources(Me.RectangleToolStripMenuItem, "RectangleToolStripMenuItem")
        Me.RectangleToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_square
        Me.RectangleToolStripMenuItem.Name = "RectangleToolStripMenuItem"
        '
        'GraficoToolStripMenuItem
        '
        resources.ApplyResources(Me.GraficoToolStripMenuItem, "GraficoToolStripMenuItem")
        Me.GraficoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.chart_curve
        Me.GraficoToolStripMenuItem.Name = "GraficoToolStripMenuItem"
        '
        'FerramentasToolStripMenuItem
        '
        resources.ApplyResources(Me.FerramentasToolStripMenuItem, "FerramentasToolStripMenuItem")
        Me.FerramentasToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CompoundCreatorWizardTSMI, Me.PropriedadesDasSubstânciasToolStripMenuItem, Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem, Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem, Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem, Me.InspectorTSMI, Me.ToolStripSeparator15})
        Me.FerramentasToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.FerramentasToolStripMenuItem.MergeIndex = 3
        Me.FerramentasToolStripMenuItem.Name = "FerramentasToolStripMenuItem"
        '
        'CompoundCreatorWizardTSMI
        '
        resources.ApplyResources(Me.CompoundCreatorWizardTSMI, "CompoundCreatorWizardTSMI")
        Me.CompoundCreatorWizardTSMI.Image = Global.DWSIM.My.Resources.Resources.wand
        Me.CompoundCreatorWizardTSMI.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CompoundCreatorWizardTSMI.MergeIndex = 6
        Me.CompoundCreatorWizardTSMI.Name = "CompoundCreatorWizardTSMI"
        '
        'PropriedadesDasSubstânciasToolStripMenuItem
        '
        resources.ApplyResources(Me.PropriedadesDasSubstânciasToolStripMenuItem, "PropriedadesDasSubstânciasToolStripMenuItem")
        Me.PropriedadesDasSubstânciasToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.wi0124_16
        Me.PropriedadesDasSubstânciasToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.PropriedadesDasSubstânciasToolStripMenuItem.MergeIndex = 5
        Me.PropriedadesDasSubstânciasToolStripMenuItem.Name = "PropriedadesDasSubstânciasToolStripMenuItem"
        '
        'CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem
        '
        resources.ApplyResources(Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem, "CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem")
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.MergeIndex = 1
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.Name = "CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem"
        '
        'CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem
        '
        resources.ApplyResources(Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem, "CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem")
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.MergeIndex = 2
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.Name = "CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem"
        '
        'GerenciadorDeAmostrasDePetroleoToolStripMenuItem
        '
        resources.ApplyResources(Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem, "GerenciadorDeAmostrasDePetroleoToolStripMenuItem")
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.MergeIndex = 3
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.Name = "GerenciadorDeAmostrasDePetroleoToolStripMenuItem"
        '
        'InspectorTSMI
        '
        resources.ApplyResources(Me.InspectorTSMI, "InspectorTSMI")
        Me.InspectorTSMI.Image = Global.DWSIM.My.Resources.Resources.icons8_spy_filled
        Me.InspectorTSMI.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.InspectorTSMI.MergeIndex = 0
        Me.InspectorTSMI.Name = "InspectorTSMI"
        '
        'ToolStripSeparator15
        '
        resources.ApplyResources(Me.ToolStripSeparator15, "ToolStripSeparator15")
        Me.ToolStripSeparator15.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator15.MergeIndex = 7
        Me.ToolStripSeparator15.Name = "ToolStripSeparator15"
        '
        'UtilitiesTSMI
        '
        resources.ApplyResources(Me.UtilitiesTSMI, "UtilitiesTSMI")
        Me.UtilitiesTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSMIAddUtility})
        Me.UtilitiesTSMI.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.UtilitiesTSMI.MergeIndex = 4
        Me.UtilitiesTSMI.Name = "UtilitiesTSMI"
        '
        'TSMIAddUtility
        '
        resources.ApplyResources(Me.TSMIAddUtility, "TSMIAddUtility")
        Me.TSMIAddUtility.Image = Global.DWSIM.My.Resources.Resources.add
        Me.TSMIAddUtility.Name = "TSMIAddUtility"
        '
        'OtimizaToolStripMenuItem
        '
        resources.ApplyResources(Me.OtimizaToolStripMenuItem, "OtimizaToolStripMenuItem")
        Me.OtimizaToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AnaliseDeSensibilidadeToolStripMenuItem, Me.MultivariateOptimizerToolStripMenuItem})
        Me.OtimizaToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.OtimizaToolStripMenuItem.MergeIndex = 5
        Me.OtimizaToolStripMenuItem.Name = "OtimizaToolStripMenuItem"
        '
        'AnaliseDeSensibilidadeToolStripMenuItem
        '
        resources.ApplyResources(Me.AnaliseDeSensibilidadeToolStripMenuItem, "AnaliseDeSensibilidadeToolStripMenuItem")
        Me.AnaliseDeSensibilidadeToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.chart_curve
        Me.AnaliseDeSensibilidadeToolStripMenuItem.Name = "AnaliseDeSensibilidadeToolStripMenuItem"
        '
        'MultivariateOptimizerToolStripMenuItem
        '
        resources.ApplyResources(Me.MultivariateOptimizerToolStripMenuItem, "MultivariateOptimizerToolStripMenuItem")
        Me.MultivariateOptimizerToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_switch
        Me.MultivariateOptimizerToolStripMenuItem.Name = "MultivariateOptimizerToolStripMenuItem"
        '
        'ScriptsToolStripMenuItem
        '
        resources.ApplyResources(Me.ScriptsToolStripMenuItem, "ScriptsToolStripMenuItem")
        Me.ScriptsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.IronRubyToolStripMenuItem})
        Me.ScriptsToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ScriptsToolStripMenuItem.MergeIndex = 6
        Me.ScriptsToolStripMenuItem.Name = "ScriptsToolStripMenuItem"
        '
        'IronRubyToolStripMenuItem
        '
        resources.ApplyResources(Me.IronRubyToolStripMenuItem, "IronRubyToolStripMenuItem")
        Me.IronRubyToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources._280px_python_logo_2
        Me.IronRubyToolStripMenuItem.Name = "IronRubyToolStripMenuItem"
        '
        'ResultadosToolStripMenuItem
        '
        resources.ApplyResources(Me.ResultadosToolStripMenuItem, "ResultadosToolStripMenuItem")
        Me.ResultadosToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.GerarRelatorioToolStripMenuItem})
        Me.ResultadosToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ResultadosToolStripMenuItem.MergeIndex = 7
        Me.ResultadosToolStripMenuItem.Name = "ResultadosToolStripMenuItem"
        '
        'GerarRelatorioToolStripMenuItem
        '
        resources.ApplyResources(Me.GerarRelatorioToolStripMenuItem, "GerarRelatorioToolStripMenuItem")
        Me.GerarRelatorioToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.report
        Me.GerarRelatorioToolStripMenuItem.Name = "GerarRelatorioToolStripMenuItem"
        '
        'PluginsToolStripMenuItem
        '
        resources.ApplyResources(Me.PluginsToolStripMenuItem, "PluginsToolStripMenuItem")
        Me.PluginsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem})
        Me.PluginsToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.PluginsToolStripMenuItem.MergeIndex = 8
        Me.PluginsToolStripMenuItem.Name = "PluginsToolStripMenuItem"
        '
        'CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem
        '
        resources.ApplyResources(Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem, "CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem")
        Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.colan2
        Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Name = "CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem"
        '
        'ExibirToolStripMenuItem
        '
        resources.ApplyResources(Me.ExibirToolStripMenuItem, "ExibirToolStripMenuItem")
        Me.ExibirToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ExibirListaDeItensACalcularToolStripMenuItem, Me.varpaneltsmi, Me.COObjTSMI, Me.ConsoleOutputTSMI, Me.ToolStripSeparator10, Me.showflowsheettoolstripmenuitem, Me.showunitstoolstripmenuitem, Me.ToolStripSeparator11, Me.RestoreLayoutTSMI, Me.tsmiCloseOpenedEditors})
        Me.ExibirToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.ExibirToolStripMenuItem.MergeIndex = 9
        Me.ExibirToolStripMenuItem.Name = "ExibirToolStripMenuItem"
        '
        'ExibirListaDeItensACalcularToolStripMenuItem
        '
        resources.ApplyResources(Me.ExibirListaDeItensACalcularToolStripMenuItem, "ExibirListaDeItensACalcularToolStripMenuItem")
        Me.ExibirListaDeItensACalcularToolStripMenuItem.CheckOnClick = True
        Me.ExibirListaDeItensACalcularToolStripMenuItem.Name = "ExibirListaDeItensACalcularToolStripMenuItem"
        '
        'varpaneltsmi
        '
        resources.ApplyResources(Me.varpaneltsmi, "varpaneltsmi")
        Me.varpaneltsmi.CheckOnClick = True
        Me.varpaneltsmi.Name = "varpaneltsmi"
        '
        'COObjTSMI
        '
        resources.ApplyResources(Me.COObjTSMI, "COObjTSMI")
        Me.COObjTSMI.CheckOnClick = True
        Me.COObjTSMI.Name = "COObjTSMI"
        '
        'ConsoleOutputTSMI
        '
        resources.ApplyResources(Me.ConsoleOutputTSMI, "ConsoleOutputTSMI")
        Me.ConsoleOutputTSMI.Name = "ConsoleOutputTSMI"
        '
        'ToolStripSeparator10
        '
        resources.ApplyResources(Me.ToolStripSeparator10, "ToolStripSeparator10")
        Me.ToolStripSeparator10.Name = "ToolStripSeparator10"
        '
        'showflowsheettoolstripmenuitem
        '
        resources.ApplyResources(Me.showflowsheettoolstripmenuitem, "showflowsheettoolstripmenuitem")
        Me.showflowsheettoolstripmenuitem.CheckOnClick = True
        Me.showflowsheettoolstripmenuitem.Name = "showflowsheettoolstripmenuitem"
        '
        'showunitstoolstripmenuitem
        '
        resources.ApplyResources(Me.showunitstoolstripmenuitem, "showunitstoolstripmenuitem")
        Me.showunitstoolstripmenuitem.CheckOnClick = True
        Me.showunitstoolstripmenuitem.Name = "showunitstoolstripmenuitem"
        '
        'ToolStripSeparator11
        '
        resources.ApplyResources(Me.ToolStripSeparator11, "ToolStripSeparator11")
        Me.ToolStripSeparator11.Name = "ToolStripSeparator11"
        '
        'RestoreLayoutTSMI
        '
        resources.ApplyResources(Me.RestoreLayoutTSMI, "RestoreLayoutTSMI")
        Me.RestoreLayoutTSMI.Name = "RestoreLayoutTSMI"
        '
        'tsmiCloseOpenedEditors
        '
        resources.ApplyResources(Me.tsmiCloseOpenedEditors, "tsmiCloseOpenedEditors")
        Me.tsmiCloseOpenedEditors.Name = "tsmiCloseOpenedEditors"
        '
        'ToolStripMenuItem1
        '
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        '
        'ToolStripMenuItem2
        '
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        '
        'ToolStripMenuItem3
        '
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        '
        'BGLoadComp
        '
        Me.BGLoadComp.WorkerReportsProgress = True
        '
        'bgCalc
        '
        Me.bgCalc.WorkerReportsProgress = True
        Me.bgCalc.WorkerSupportsCancellation = True
        '
        'dckPanel
        '
        resources.ApplyResources(Me.dckPanel, "dckPanel")
        Me.dckPanel.DefaultFloatWindowSize = New System.Drawing.Size(900, 600)
        Me.dckPanel.DockBackColor = System.Drawing.SystemColors.Control
        Me.dckPanel.DocumentStyle = WeifenLuo.WinFormsUI.Docking.DocumentStyle.DockingWindow
        Me.dckPanel.Name = "dckPanel"
        '
        'SaveFileDialog1
        '
        Me.SaveFileDialog1.DefaultExt = "png"
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'OpenFileName
        '
        resources.ApplyResources(Me.OpenFileName, "OpenFileName")
        '
        'TimerScripts1
        '
        Me.TimerScripts1.Enabled = True
        Me.TimerScripts1.Interval = 60000
        '
        'TimerScripts5
        '
        Me.TimerScripts5.Enabled = True
        Me.TimerScripts5.Interval = 300000
        '
        'TimerScripts15
        '
        Me.TimerScripts15.Enabled = True
        Me.TimerScripts15.Interval = 900000
        '
        'TimerScripts30
        '
        Me.TimerScripts30.Enabled = True
        Me.TimerScripts30.Interval = 1800000
        '
        'TimerScripts60
        '
        Me.TimerScripts60.Enabled = True
        Me.TimerScripts60.Interval = 3600000
        '
        'QuestionBox_Panel
        '
        resources.ApplyResources(Me.QuestionBox_Panel, "QuestionBox_Panel")
        Me.QuestionBox_Panel.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Button2)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Button1)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_PictureBox1)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Label1)
        Me.QuestionBox_Panel.Name = "QuestionBox_Panel"
        '
        'QuestionBox_Button2
        '
        resources.ApplyResources(Me.QuestionBox_Button2, "QuestionBox_Button2")
        Me.QuestionBox_Button2.DialogResult = System.Windows.Forms.DialogResult.No
        Me.QuestionBox_Button2.Name = "QuestionBox_Button2"
        Me.QuestionBox_Button2.UseVisualStyleBackColor = True
        '
        'QuestionBox_Button1
        '
        resources.ApplyResources(Me.QuestionBox_Button1, "QuestionBox_Button1")
        Me.QuestionBox_Button1.DialogResult = System.Windows.Forms.DialogResult.Yes
        Me.QuestionBox_Button1.Name = "QuestionBox_Button1"
        Me.QuestionBox_Button1.UseVisualStyleBackColor = True
        '
        'QuestionBox_PictureBox1
        '
        resources.ApplyResources(Me.QuestionBox_PictureBox1, "QuestionBox_PictureBox1")
        Me.QuestionBox_PictureBox1.Name = "QuestionBox_PictureBox1"
        Me.QuestionBox_PictureBox1.TabStop = False
        '
        'QuestionBox_Label1
        '
        resources.ApplyResources(Me.QuestionBox_Label1, "QuestionBox_Label1")
        Me.QuestionBox_Label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.QuestionBox_Label1.Name = "QuestionBox_Label1"
        '
        'PanelMobileCompatMode
        '
        resources.ApplyResources(Me.PanelMobileCompatMode, "PanelMobileCompatMode")
        Me.PanelMobileCompatMode.BackColor = System.Drawing.Color.White
        Me.PanelMobileCompatMode.Controls.Add(Me.Button2)
        Me.PanelMobileCompatMode.Controls.Add(Me.PictureBox1)
        Me.PanelMobileCompatMode.Controls.Add(Me.Label1)
        Me.PanelMobileCompatMode.Name = "PanelMobileCompatMode"
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.DialogResult = System.Windows.Forms.DialogResult.Yes
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.emblem_important
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.Label1.Name = "Label1"
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripSeparator1, Me.tsbAtivar, Me.tsbCalcF, Me.tsbCalc, Me.tsbAbortCalc, Me.tsbSimultAdjustSolver, Me.ToolStripSeparator2, Me.tsbUndo, Me.tsbRedo})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.cog
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripSeparator1
        '
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        '
        'tsbAtivar
        '
        resources.ApplyResources(Me.tsbAtivar, "tsbAtivar")
        Me.tsbAtivar.CheckOnClick = True
        Me.tsbAtivar.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbAtivar.Image = Global.DWSIM.My.Resources.Resources.control_power_blue
        Me.tsbAtivar.Name = "tsbAtivar"
        '
        'tsbCalcF
        '
        resources.ApplyResources(Me.tsbCalcF, "tsbCalcF")
        Me.tsbCalcF.BackColor = System.Drawing.Color.LightSteelBlue
        Me.tsbCalcF.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbCalcF.Image = Global.DWSIM.My.Resources.Resources.control_play
        Me.tsbCalcF.Name = "tsbCalcF"
        '
        'tsbCalc
        '
        resources.ApplyResources(Me.tsbCalc, "tsbCalc")
        Me.tsbCalc.Image = Global.DWSIM.My.Resources.Resources.control_play_blue
        Me.tsbCalc.Name = "tsbCalc"
        '
        'tsbAbortCalc
        '
        resources.ApplyResources(Me.tsbAbortCalc, "tsbAbortCalc")
        Me.tsbAbortCalc.Image = Global.DWSIM.My.Resources.Resources.control_stop_blue
        Me.tsbAbortCalc.Name = "tsbAbortCalc"
        '
        'tsbSimultAdjustSolver
        '
        resources.ApplyResources(Me.tsbSimultAdjustSolver, "tsbSimultAdjustSolver")
        Me.tsbSimultAdjustSolver.CheckOnClick = True
        Me.tsbSimultAdjustSolver.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbSimultAdjustSolver.Image = Global.DWSIM.My.Resources.Resources.control_power
        Me.tsbSimultAdjustSolver.Name = "tsbSimultAdjustSolver"
        '
        'ToolStripSeparator2
        '
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        '
        'tsbUndo
        '
        resources.ApplyResources(Me.tsbUndo, "tsbUndo")
        Me.tsbUndo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbUndo.Image = Global.DWSIM.My.Resources.Resources.undo_161
        Me.tsbUndo.Name = "tsbUndo"
        '
        'tsbRedo
        '
        resources.ApplyResources(Me.tsbRedo, "tsbRedo")
        Me.tsbRedo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbRedo.Image = Global.DWSIM.My.Resources.Resources.redo_16
        Me.tsbRedo.Name = "tsbRedo"
        '
        'FormFlowsheet
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.PanelMobileCompatMode)
        Me.Controls.Add(Me.dckPanel)
        Me.Controls.Add(Me.QuestionBox_Panel)
        Me.Controls.Add(Me.MenuStrip1)
        Me.DoubleBuffered = True
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "FormFlowsheet"
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TSTable.ResumeLayout(False)
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.QuestionBox_Panel.ResumeLayout(False)
        Me.QuestionBox_Panel.PerformLayout()
        CType(Me.QuestionBox_PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.PanelMobileCompatMode.ResumeLayout(False)
        Me.PanelMobileCompatMode.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem3 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Public WithEvents TSTable As FarsiLibrary.Win.FATabStripItem
    Public WithEvents BGLoadComp As System.ComponentModel.BackgroundWorker
    Public WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Public WithEvents ResultadosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents GerarRelatorioToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents bgCalc As System.ComponentModel.BackgroundWorker
    Public WithEvents OtimizaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents AnaliseDeSensibilidadeToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ArquivoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CloseToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents MultivariateOptimizerToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents dckPanel As WeifenLuo.WinFormsUI.Docking.DockPanel
    Public WithEvents FerramentasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents PluginsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ScriptsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents IronRubyToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ExibirListaDeItensACalcularToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents COObjTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ExibirToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents varpaneltsmi As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Friend WithEvents GerenciadorDeAmostrasDePetroleoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents OpenFileName As System.Windows.Forms.OpenFileDialog
    Friend WithEvents TimerScripts1 As System.Windows.Forms.Timer
    Friend WithEvents TimerScripts5 As System.Windows.Forms.Timer
    Friend WithEvents TimerScripts15 As System.Windows.Forms.Timer
    Friend WithEvents TimerScripts30 As System.Windows.Forms.Timer
    Friend WithEvents TimerScripts60 As System.Windows.Forms.Timer
    Friend WithEvents QuestionBox_Panel As System.Windows.Forms.Panel
    Friend WithEvents QuestionBox_Button2 As System.Windows.Forms.Button
    Friend WithEvents QuestionBox_Button1 As System.Windows.Forms.Button
    Friend WithEvents QuestionBox_PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents QuestionBox_Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolStripSeparator10 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents showflowsheettoolstripmenuitem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents showunitstoolstripmenuitem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator11 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents RestoreLayoutTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents EditarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiUndo As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiRedo As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator14 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsmiCut As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiCopy As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiPaste As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiRemoveSelected As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiCloneSelected As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiRecalc As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiExportData As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator18 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents tsmiConfigSimulation As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator15 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents InserirToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents BlocoDeSimulacaoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TabelaDePropriedadesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TabelaDePropriedatesMestraToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TabelaDePropriedadesPlanilhaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FiguraToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TextoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents RectangleToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsmiCloseOpenedEditors As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents UtilitiesTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents TSMIAddUtility As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PropriedadesDasSubstânciasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents PanelMobileCompatMode As System.Windows.Forms.Panel
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents GraficoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CompoundCreatorWizardTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ConsoleOutputTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents InspectorTSMI As ToolStripMenuItem
    Friend WithEvents ToolStrip1 As ToolStrip
    Friend WithEvents ToolStripButton1 As ToolStripButton
    Friend WithEvents tsbCalc As ToolStripButton
    Friend WithEvents tsbAbortCalc As ToolStripButton
    Friend WithEvents tsbAtivar As ToolStripButton
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
    Friend WithEvents tsbSimultAdjustSolver As ToolStripButton
    Friend WithEvents ToolStripSeparator2 As ToolStripSeparator
    Friend WithEvents tsbUndo As ToolStripDropDownButton
    Friend WithEvents tsbRedo As ToolStripDropDownButton
    Friend WithEvents tsbCalcF As ToolStripButton
End Class
