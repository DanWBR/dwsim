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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormFlowsheet))
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
        Me.FerramentasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GerenciadorDeReacoesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PropriedadesDasSubstânciasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
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
        Me.consoletsmi = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExibirListaDeItensACalcularToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.varpaneltsmi = New System.Windows.Forms.ToolStripMenuItem()
        Me.COObjTSMI = New System.Windows.Forms.ToolStripMenuItem()
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
        Me.StatusStrip0 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripDropDownButton1 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.tsbmiCompounds = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbmiModels = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbmiReactions = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbmiUnits = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripComboBoxUnitSystem = New System.Windows.Forms.ToolStripComboBox()
        Me.tsbEditUnits = New System.Windows.Forms.ToolStripMenuItem()
        Me.AdicionarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FormataçãoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NúmerosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripComboBoxNumberFormatting = New System.Windows.Forms.ToolStripComboBox()
        Me.FraçõesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripComboBoxNumberFractionFormatting = New System.Windows.Forms.ToolStripComboBox()
        Me.AjudaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripDropDownButton2 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.tsbAtivar = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbCalc = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbAbortCalc = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbSimultAdjustSolver = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsbUndo = New System.Windows.Forms.ToolStripSplitButton()
        Me.tsbRedo = New System.Windows.Forms.ToolStripSplitButton()
        Me.tsbLogMessage = New System.Windows.Forms.ToolStripDropDownButton()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TSTable.SuspendLayout()
        Me.MenuStrip1.SuspendLayout()
        Me.QuestionBox_Panel.SuspendLayout()
        CType(Me.QuestionBox_PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.PanelMobileCompatMode.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.StatusStrip0.SuspendLayout()
        Me.SuspendLayout()
        '
        'DataGridView1
        '
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
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
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
        Me.TSTable.CanClose = False
        Me.TSTable.Controls.Add(Me.DataGridView1)
        Me.TSTable.IsDrawn = True
        Me.TSTable.Name = "TSTable"
        resources.ApplyResources(Me.TSTable, "TSTable")
        '
        'MenuStrip1
        '
        Me.MenuStrip1.AllowItemReorder = True
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ArquivoToolStripMenuItem, Me.EditarToolStripMenuItem, Me.InserirToolStripMenuItem, Me.FerramentasToolStripMenuItem, Me.UtilitiesTSMI, Me.OtimizaToolStripMenuItem, Me.ScriptsToolStripMenuItem, Me.ResultadosToolStripMenuItem, Me.PluginsToolStripMenuItem, Me.ExibirToolStripMenuItem})
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.ShowItemToolTips = True
        '
        'ArquivoToolStripMenuItem
        '
        Me.ArquivoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CloseToolStripMenuItem})
        Me.ArquivoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.ArquivoToolStripMenuItem.MergeIndex = 0
        Me.ArquivoToolStripMenuItem.Name = "ArquivoToolStripMenuItem"
        resources.ApplyResources(Me.ArquivoToolStripMenuItem, "ArquivoToolStripMenuItem")
        '
        'CloseToolStripMenuItem
        '
        Me.CloseToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.CloseToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CloseToolStripMenuItem.MergeIndex = 11
        Me.CloseToolStripMenuItem.Name = "CloseToolStripMenuItem"
        Me.CloseToolStripMenuItem.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        resources.ApplyResources(Me.CloseToolStripMenuItem, "CloseToolStripMenuItem")
        '
        'EditarToolStripMenuItem
        '
        Me.EditarToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsmiUndo, Me.tsmiRedo, Me.ToolStripSeparator14, Me.tsmiCut, Me.tsmiCopy, Me.tsmiPaste, Me.tsmiRemoveSelected, Me.tsmiCloneSelected, Me.tsmiRecalc, Me.tsmiExportData, Me.ToolStripSeparator18, Me.tsmiConfigSimulation})
        Me.EditarToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.EditarToolStripMenuItem.MergeIndex = 1
        Me.EditarToolStripMenuItem.Name = "EditarToolStripMenuItem"
        resources.ApplyResources(Me.EditarToolStripMenuItem, "EditarToolStripMenuItem")
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
        Me.ToolStripSeparator14.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator14.MergeIndex = 2
        Me.ToolStripSeparator14.Name = "ToolStripSeparator14"
        Me.ToolStripSeparator14.Overflow = System.Windows.Forms.ToolStripItemOverflow.Never
        resources.ApplyResources(Me.ToolStripSeparator14, "ToolStripSeparator14")
        '
        'tsmiCut
        '
        Me.tsmiCut.Image = Global.DWSIM.My.Resources.Resources.cut
        Me.tsmiCut.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCut.MergeIndex = 3
        Me.tsmiCut.Name = "tsmiCut"
        resources.ApplyResources(Me.tsmiCut, "tsmiCut")
        '
        'tsmiCopy
        '
        Me.tsmiCopy.Image = Global.DWSIM.My.Resources.Resources.page_copy
        Me.tsmiCopy.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCopy.MergeIndex = 4
        Me.tsmiCopy.Name = "tsmiCopy"
        resources.ApplyResources(Me.tsmiCopy, "tsmiCopy")
        '
        'tsmiPaste
        '
        Me.tsmiPaste.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        Me.tsmiPaste.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiPaste.MergeIndex = 5
        Me.tsmiPaste.Name = "tsmiPaste"
        resources.ApplyResources(Me.tsmiPaste, "tsmiPaste")
        '
        'tsmiRemoveSelected
        '
        Me.tsmiRemoveSelected.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.tsmiRemoveSelected.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiRemoveSelected.MergeIndex = 6
        Me.tsmiRemoveSelected.Name = "tsmiRemoveSelected"
        resources.ApplyResources(Me.tsmiRemoveSelected, "tsmiRemoveSelected")
        '
        'tsmiCloneSelected
        '
        Me.tsmiCloneSelected.Image = Global.DWSIM.My.Resources.Resources.sheep
        Me.tsmiCloneSelected.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiCloneSelected.MergeIndex = 7
        Me.tsmiCloneSelected.Name = "tsmiCloneSelected"
        resources.ApplyResources(Me.tsmiCloneSelected, "tsmiCloneSelected")
        '
        'tsmiRecalc
        '
        Me.tsmiRecalc.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        Me.tsmiRecalc.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiRecalc.MergeIndex = 8
        Me.tsmiRecalc.Name = "tsmiRecalc"
        resources.ApplyResources(Me.tsmiRecalc, "tsmiRecalc")
        '
        'tsmiExportData
        '
        Me.tsmiExportData.Image = Global.DWSIM.My.Resources.Resources.clipboard_sign
        Me.tsmiExportData.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiExportData.MergeIndex = 9
        Me.tsmiExportData.Name = "tsmiExportData"
        resources.ApplyResources(Me.tsmiExportData, "tsmiExportData")
        '
        'ToolStripSeparator18
        '
        Me.ToolStripSeparator18.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator18.MergeIndex = 10
        Me.ToolStripSeparator18.Name = "ToolStripSeparator18"
        resources.ApplyResources(Me.ToolStripSeparator18, "ToolStripSeparator18")
        '
        'tsmiConfigSimulation
        '
        Me.tsmiConfigSimulation.Image = Global.DWSIM.My.Resources.Resources.brick
        Me.tsmiConfigSimulation.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.tsmiConfigSimulation.MergeIndex = 11
        Me.tsmiConfigSimulation.Name = "tsmiConfigSimulation"
        resources.ApplyResources(Me.tsmiConfigSimulation, "tsmiConfigSimulation")
        '
        'InserirToolStripMenuItem
        '
        Me.InserirToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.BlocoDeSimulacaoToolStripMenuItem, Me.TabelaDePropriedadesToolStripMenuItem, Me.TabelaDePropriedatesMestraToolStripMenuItem, Me.TabelaDePropriedadesPlanilhaToolStripMenuItem, Me.FiguraToolStripMenuItem, Me.TextoToolStripMenuItem, Me.RectangleToolStripMenuItem})
        Me.InserirToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.InserirToolStripMenuItem.MergeIndex = 2
        Me.InserirToolStripMenuItem.Name = "InserirToolStripMenuItem"
        resources.ApplyResources(Me.InserirToolStripMenuItem, "InserirToolStripMenuItem")
        '
        'BlocoDeSimulacaoToolStripMenuItem
        '
        Me.BlocoDeSimulacaoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.uo_custom_16
        Me.BlocoDeSimulacaoToolStripMenuItem.Name = "BlocoDeSimulacaoToolStripMenuItem"
        resources.ApplyResources(Me.BlocoDeSimulacaoToolStripMenuItem, "BlocoDeSimulacaoToolStripMenuItem")
        '
        'TabelaDePropriedadesToolStripMenuItem
        '
        Me.TabelaDePropriedadesToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table_lightning
        Me.TabelaDePropriedadesToolStripMenuItem.Name = "TabelaDePropriedadesToolStripMenuItem"
        resources.ApplyResources(Me.TabelaDePropriedadesToolStripMenuItem, "TabelaDePropriedadesToolStripMenuItem")
        '
        'TabelaDePropriedatesMestraToolStripMenuItem
        '
        Me.TabelaDePropriedatesMestraToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table
        Me.TabelaDePropriedatesMestraToolStripMenuItem.Name = "TabelaDePropriedatesMestraToolStripMenuItem"
        resources.ApplyResources(Me.TabelaDePropriedatesMestraToolStripMenuItem, "TabelaDePropriedatesMestraToolStripMenuItem")
        '
        'TabelaDePropriedadesPlanilhaToolStripMenuItem
        '
        Me.TabelaDePropriedadesPlanilhaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.table_relationship
        Me.TabelaDePropriedadesPlanilhaToolStripMenuItem.Name = "TabelaDePropriedadesPlanilhaToolStripMenuItem"
        resources.ApplyResources(Me.TabelaDePropriedadesPlanilhaToolStripMenuItem, "TabelaDePropriedadesPlanilhaToolStripMenuItem")
        '
        'FiguraToolStripMenuItem
        '
        Me.FiguraToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.picture
        Me.FiguraToolStripMenuItem.Name = "FiguraToolStripMenuItem"
        resources.ApplyResources(Me.FiguraToolStripMenuItem, "FiguraToolStripMenuItem")
        '
        'TextoToolStripMenuItem
        '
        Me.TextoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.font
        Me.TextoToolStripMenuItem.Name = "TextoToolStripMenuItem"
        resources.ApplyResources(Me.TextoToolStripMenuItem, "TextoToolStripMenuItem")
        '
        'RectangleToolStripMenuItem
        '
        Me.RectangleToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.shape_square
        Me.RectangleToolStripMenuItem.Name = "RectangleToolStripMenuItem"
        resources.ApplyResources(Me.RectangleToolStripMenuItem, "RectangleToolStripMenuItem")
        '
        'FerramentasToolStripMenuItem
        '
        Me.FerramentasToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem, Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem, Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem, Me.GerenciadorDeReacoesToolStripMenuItem, Me.PropriedadesDasSubstânciasToolStripMenuItem, Me.ToolStripSeparator15})
        Me.FerramentasToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.FerramentasToolStripMenuItem.MergeIndex = 3
        Me.FerramentasToolStripMenuItem.Name = "FerramentasToolStripMenuItem"
        resources.ApplyResources(Me.FerramentasToolStripMenuItem, "FerramentasToolStripMenuItem")
        '
        'CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem
        '
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.MergeIndex = 0
        Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.Name = "CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem"
        resources.ApplyResources(Me.CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem, "CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem")
        '
        'CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem
        '
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.MergeIndex = 1
        Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.Name = "CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem"
        resources.ApplyResources(Me.CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem, "CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem")
        '
        'GerenciadorDeAmostrasDePetroleoToolStripMenuItem
        '
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.Experiments_Badge
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.MergeIndex = 2
        Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem.Name = "GerenciadorDeAmostrasDePetroleoToolStripMenuItem"
        resources.ApplyResources(Me.GerenciadorDeAmostrasDePetroleoToolStripMenuItem, "GerenciadorDeAmostrasDePetroleoToolStripMenuItem")
        '
        'GerenciadorDeReacoesToolStripMenuItem
        '
        Me.GerenciadorDeReacoesToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.weather_lightning
        Me.GerenciadorDeReacoesToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.GerenciadorDeReacoesToolStripMenuItem.MergeIndex = 3
        Me.GerenciadorDeReacoesToolStripMenuItem.Name = "GerenciadorDeReacoesToolStripMenuItem"
        resources.ApplyResources(Me.GerenciadorDeReacoesToolStripMenuItem, "GerenciadorDeReacoesToolStripMenuItem")
        '
        'PropriedadesDasSubstânciasToolStripMenuItem
        '
        Me.PropriedadesDasSubstânciasToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.wi0124_16
        Me.PropriedadesDasSubstânciasToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.PropriedadesDasSubstânciasToolStripMenuItem.MergeIndex = 4
        Me.PropriedadesDasSubstânciasToolStripMenuItem.Name = "PropriedadesDasSubstânciasToolStripMenuItem"
        resources.ApplyResources(Me.PropriedadesDasSubstânciasToolStripMenuItem, "PropriedadesDasSubstânciasToolStripMenuItem")
        '
        'ToolStripSeparator15
        '
        Me.ToolStripSeparator15.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ToolStripSeparator15.MergeIndex = 5
        Me.ToolStripSeparator15.Name = "ToolStripSeparator15"
        resources.ApplyResources(Me.ToolStripSeparator15, "ToolStripSeparator15")
        '
        'UtilitiesTSMI
        '
        Me.UtilitiesTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSMIAddUtility})
        Me.UtilitiesTSMI.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.UtilitiesTSMI.MergeIndex = 4
        Me.UtilitiesTSMI.Name = "UtilitiesTSMI"
        resources.ApplyResources(Me.UtilitiesTSMI, "UtilitiesTSMI")
        '
        'TSMIAddUtility
        '
        Me.TSMIAddUtility.Image = Global.DWSIM.My.Resources.Resources.add
        Me.TSMIAddUtility.Name = "TSMIAddUtility"
        resources.ApplyResources(Me.TSMIAddUtility, "TSMIAddUtility")
        '
        'OtimizaToolStripMenuItem
        '
        Me.OtimizaToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.AnaliseDeSensibilidadeToolStripMenuItem, Me.MultivariateOptimizerToolStripMenuItem})
        Me.OtimizaToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.OtimizaToolStripMenuItem.MergeIndex = 5
        Me.OtimizaToolStripMenuItem.Name = "OtimizaToolStripMenuItem"
        resources.ApplyResources(Me.OtimizaToolStripMenuItem, "OtimizaToolStripMenuItem")
        '
        'AnaliseDeSensibilidadeToolStripMenuItem
        '
        Me.AnaliseDeSensibilidadeToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.chart_curve
        Me.AnaliseDeSensibilidadeToolStripMenuItem.Name = "AnaliseDeSensibilidadeToolStripMenuItem"
        resources.ApplyResources(Me.AnaliseDeSensibilidadeToolStripMenuItem, "AnaliseDeSensibilidadeToolStripMenuItem")
        '
        'MultivariateOptimizerToolStripMenuItem
        '
        Me.MultivariateOptimizerToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_switch
        Me.MultivariateOptimizerToolStripMenuItem.Name = "MultivariateOptimizerToolStripMenuItem"
        resources.ApplyResources(Me.MultivariateOptimizerToolStripMenuItem, "MultivariateOptimizerToolStripMenuItem")
        '
        'ScriptsToolStripMenuItem
        '
        Me.ScriptsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.IronRubyToolStripMenuItem})
        Me.ScriptsToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ScriptsToolStripMenuItem.MergeIndex = 6
        Me.ScriptsToolStripMenuItem.Name = "ScriptsToolStripMenuItem"
        resources.ApplyResources(Me.ScriptsToolStripMenuItem, "ScriptsToolStripMenuItem")
        '
        'IronRubyToolStripMenuItem
        '
        Me.IronRubyToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources._280px_python_logo_2
        resources.ApplyResources(Me.IronRubyToolStripMenuItem, "IronRubyToolStripMenuItem")
        Me.IronRubyToolStripMenuItem.Name = "IronRubyToolStripMenuItem"
        '
        'ResultadosToolStripMenuItem
        '
        Me.ResultadosToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.GerarRelatorioToolStripMenuItem})
        Me.ResultadosToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.ResultadosToolStripMenuItem.MergeIndex = 7
        Me.ResultadosToolStripMenuItem.Name = "ResultadosToolStripMenuItem"
        resources.ApplyResources(Me.ResultadosToolStripMenuItem, "ResultadosToolStripMenuItem")
        '
        'GerarRelatorioToolStripMenuItem
        '
        Me.GerarRelatorioToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.report
        Me.GerarRelatorioToolStripMenuItem.Name = "GerarRelatorioToolStripMenuItem"
        resources.ApplyResources(Me.GerarRelatorioToolStripMenuItem, "GerarRelatorioToolStripMenuItem")
        '
        'PluginsToolStripMenuItem
        '
        Me.PluginsToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem})
        Me.PluginsToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.PluginsToolStripMenuItem.MergeIndex = 8
        Me.PluginsToolStripMenuItem.Name = "PluginsToolStripMenuItem"
        resources.ApplyResources(Me.PluginsToolStripMenuItem, "PluginsToolStripMenuItem")
        '
        'CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem
        '
        Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.colan2
        Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Name = "CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem"
        resources.ApplyResources(Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem, "CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem")
        '
        'ExibirToolStripMenuItem
        '
        Me.ExibirToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.consoletsmi, Me.ExibirListaDeItensACalcularToolStripMenuItem, Me.varpaneltsmi, Me.COObjTSMI, Me.ToolStripSeparator10, Me.showflowsheettoolstripmenuitem, Me.showunitstoolstripmenuitem, Me.ToolStripSeparator11, Me.RestoreLayoutTSMI, Me.tsmiCloseOpenedEditors})
        Me.ExibirToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.MatchOnly
        Me.ExibirToolStripMenuItem.MergeIndex = 9
        Me.ExibirToolStripMenuItem.Name = "ExibirToolStripMenuItem"
        resources.ApplyResources(Me.ExibirToolStripMenuItem, "ExibirToolStripMenuItem")
        '
        'consoletsmi
        '
        Me.consoletsmi.CheckOnClick = True
        Me.consoletsmi.Name = "consoletsmi"
        resources.ApplyResources(Me.consoletsmi, "consoletsmi")
        '
        'ExibirListaDeItensACalcularToolStripMenuItem
        '
        Me.ExibirListaDeItensACalcularToolStripMenuItem.CheckOnClick = True
        Me.ExibirListaDeItensACalcularToolStripMenuItem.Name = "ExibirListaDeItensACalcularToolStripMenuItem"
        resources.ApplyResources(Me.ExibirListaDeItensACalcularToolStripMenuItem, "ExibirListaDeItensACalcularToolStripMenuItem")
        '
        'varpaneltsmi
        '
        Me.varpaneltsmi.CheckOnClick = True
        Me.varpaneltsmi.Name = "varpaneltsmi"
        resources.ApplyResources(Me.varpaneltsmi, "varpaneltsmi")
        '
        'COObjTSMI
        '
        Me.COObjTSMI.CheckOnClick = True
        Me.COObjTSMI.Name = "COObjTSMI"
        resources.ApplyResources(Me.COObjTSMI, "COObjTSMI")
        '
        'ToolStripSeparator10
        '
        Me.ToolStripSeparator10.Name = "ToolStripSeparator10"
        resources.ApplyResources(Me.ToolStripSeparator10, "ToolStripSeparator10")
        '
        'showflowsheettoolstripmenuitem
        '
        Me.showflowsheettoolstripmenuitem.CheckOnClick = True
        Me.showflowsheettoolstripmenuitem.Name = "showflowsheettoolstripmenuitem"
        resources.ApplyResources(Me.showflowsheettoolstripmenuitem, "showflowsheettoolstripmenuitem")
        '
        'showunitstoolstripmenuitem
        '
        Me.showunitstoolstripmenuitem.CheckOnClick = True
        Me.showunitstoolstripmenuitem.Name = "showunitstoolstripmenuitem"
        resources.ApplyResources(Me.showunitstoolstripmenuitem, "showunitstoolstripmenuitem")
        '
        'ToolStripSeparator11
        '
        Me.ToolStripSeparator11.Name = "ToolStripSeparator11"
        resources.ApplyResources(Me.ToolStripSeparator11, "ToolStripSeparator11")
        '
        'RestoreLayoutTSMI
        '
        Me.RestoreLayoutTSMI.Name = "RestoreLayoutTSMI"
        resources.ApplyResources(Me.RestoreLayoutTSMI, "RestoreLayoutTSMI")
        '
        'tsmiCloseOpenedEditors
        '
        Me.tsmiCloseOpenedEditors.Name = "tsmiCloseOpenedEditors"
        resources.ApplyResources(Me.tsmiCloseOpenedEditors, "tsmiCloseOpenedEditors")
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
        '
        'ToolStripMenuItem3
        '
        Me.ToolStripMenuItem3.Name = "ToolStripMenuItem3"
        resources.ApplyResources(Me.ToolStripMenuItem3, "ToolStripMenuItem3")
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
        Me.dckPanel.DefaultFloatWindowSize = New System.Drawing.Size(900, 600)
        resources.ApplyResources(Me.dckPanel, "dckPanel")
        Me.dckPanel.DockBackColor = System.Drawing.SystemColors.Control
        Me.dckPanel.DocumentStyle = WeifenLuo.WinFormsUI.Docking.DocumentStyle.DockingWindow
        Me.dckPanel.Name = "dckPanel"
        '
        'SaveFileDialog1
        '
        Me.SaveFileDialog1.DefaultExt = "png"
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
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
        Me.QuestionBox_Panel.BackColor = System.Drawing.SystemColors.ActiveCaption
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Button2)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Button1)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_PictureBox1)
        Me.QuestionBox_Panel.Controls.Add(Me.QuestionBox_Label1)
        resources.ApplyResources(Me.QuestionBox_Panel, "QuestionBox_Panel")
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
        Me.PanelMobileCompatMode.BackColor = System.Drawing.Color.White
        Me.PanelMobileCompatMode.Controls.Add(Me.Button2)
        Me.PanelMobileCompatMode.Controls.Add(Me.PictureBox1)
        Me.PanelMobileCompatMode.Controls.Add(Me.Label1)
        resources.ApplyResources(Me.PanelMobileCompatMode, "PanelMobileCompatMode")
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
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.emblem_important
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.SystemColors.ActiveCaptionText
        Me.Label1.Name = "Label1"
        '
        'StatusStrip0
        '
        resources.ApplyResources(Me.StatusStrip0, "StatusStrip0")
        Me.StatusStrip0.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripDropDownButton1, Me.ToolStripDropDownButton2, Me.tsbUndo, Me.tsbRedo, Me.tsbLogMessage})
        Me.StatusStrip0.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow
        Me.StatusStrip0.Name = "StatusStrip0"
        Me.StatusStrip0.ShowItemToolTips = True
        Me.StatusStrip0.SizingGrip = False
        '
        'ToolStripDropDownButton1
        '
        Me.ToolStripDropDownButton1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbmiCompounds, Me.tsbmiModels, Me.tsbmiReactions, Me.tsbmiUnits, Me.FormataçãoToolStripMenuItem})
        Me.ToolStripDropDownButton1.Image = Global.DWSIM.My.Resources.Resources.cog
        resources.ApplyResources(Me.ToolStripDropDownButton1, "ToolStripDropDownButton1")
        Me.ToolStripDropDownButton1.Name = "ToolStripDropDownButton1"
        '
        'tsbmiCompounds
        '
        Me.tsbmiCompounds.Image = Global.DWSIM.My.Resources.Resources.wi0124_16
        Me.tsbmiCompounds.Name = "tsbmiCompounds"
        resources.ApplyResources(Me.tsbmiCompounds, "tsbmiCompounds")
        '
        'tsbmiModels
        '
        Me.tsbmiModels.Image = Global.DWSIM.My.Resources.Resources.book_open
        Me.tsbmiModels.Name = "tsbmiModels"
        resources.ApplyResources(Me.tsbmiModels, "tsbmiModels")
        '
        'tsbmiReactions
        '
        Me.tsbmiReactions.Image = Global.DWSIM.My.Resources.Resources.lightning
        Me.tsbmiReactions.Name = "tsbmiReactions"
        resources.ApplyResources(Me.tsbmiReactions, "tsbmiReactions")
        '
        'tsbmiUnits
        '
        Me.tsbmiUnits.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripComboBoxUnitSystem, Me.tsbEditUnits, Me.AdicionarToolStripMenuItem})
        Me.tsbmiUnits.Image = Global.DWSIM.My.Resources.Resources.text_letter_omega
        Me.tsbmiUnits.Name = "tsbmiUnits"
        resources.ApplyResources(Me.tsbmiUnits, "tsbmiUnits")
        '
        'ToolStripComboBoxUnitSystem
        '
        Me.ToolStripComboBoxUnitSystem.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ToolStripComboBoxUnitSystem.Name = "ToolStripComboBoxUnitSystem"
        resources.ApplyResources(Me.ToolStripComboBoxUnitSystem, "ToolStripComboBoxUnitSystem")
        '
        'tsbEditUnits
        '
        Me.tsbEditUnits.Image = Global.DWSIM.My.Resources.Resources.table_gear
        Me.tsbEditUnits.Name = "tsbEditUnits"
        resources.ApplyResources(Me.tsbEditUnits, "tsbEditUnits")
        '
        'AdicionarToolStripMenuItem
        '
        Me.AdicionarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.add
        Me.AdicionarToolStripMenuItem.Name = "AdicionarToolStripMenuItem"
        resources.ApplyResources(Me.AdicionarToolStripMenuItem, "AdicionarToolStripMenuItem")
        '
        'FormataçãoToolStripMenuItem
        '
        Me.FormataçãoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NúmerosToolStripMenuItem, Me.FraçõesToolStripMenuItem, Me.AjudaToolStripMenuItem})
        Me.FormataçãoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.textfield_rename
        Me.FormataçãoToolStripMenuItem.Name = "FormataçãoToolStripMenuItem"
        resources.ApplyResources(Me.FormataçãoToolStripMenuItem, "FormataçãoToolStripMenuItem")
        '
        'NúmerosToolStripMenuItem
        '
        Me.NúmerosToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripComboBoxNumberFormatting})
        Me.NúmerosToolStripMenuItem.Name = "NúmerosToolStripMenuItem"
        resources.ApplyResources(Me.NúmerosToolStripMenuItem, "NúmerosToolStripMenuItem")
        '
        'ToolStripComboBoxNumberFormatting
        '
        Me.ToolStripComboBoxNumberFormatting.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ToolStripComboBoxNumberFormatting.Items.AddRange(New Object() {resources.GetString("ToolStripComboBoxNumberFormatting.Items"), resources.GetString("ToolStripComboBoxNumberFormatting.Items1"), resources.GetString("ToolStripComboBoxNumberFormatting.Items2"), resources.GetString("ToolStripComboBoxNumberFormatting.Items3"), resources.GetString("ToolStripComboBoxNumberFormatting.Items4"), resources.GetString("ToolStripComboBoxNumberFormatting.Items5"), resources.GetString("ToolStripComboBoxNumberFormatting.Items6"), resources.GetString("ToolStripComboBoxNumberFormatting.Items7"), resources.GetString("ToolStripComboBoxNumberFormatting.Items8"), resources.GetString("ToolStripComboBoxNumberFormatting.Items9"), resources.GetString("ToolStripComboBoxNumberFormatting.Items10"), resources.GetString("ToolStripComboBoxNumberFormatting.Items11"), resources.GetString("ToolStripComboBoxNumberFormatting.Items12"), resources.GetString("ToolStripComboBoxNumberFormatting.Items13"), resources.GetString("ToolStripComboBoxNumberFormatting.Items14"), resources.GetString("ToolStripComboBoxNumberFormatting.Items15"), resources.GetString("ToolStripComboBoxNumberFormatting.Items16"), resources.GetString("ToolStripComboBoxNumberFormatting.Items17")})
        Me.ToolStripComboBoxNumberFormatting.Name = "ToolStripComboBoxNumberFormatting"
        resources.ApplyResources(Me.ToolStripComboBoxNumberFormatting, "ToolStripComboBoxNumberFormatting")
        '
        'FraçõesToolStripMenuItem
        '
        Me.FraçõesToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripComboBoxNumberFractionFormatting})
        Me.FraçõesToolStripMenuItem.Name = "FraçõesToolStripMenuItem"
        resources.ApplyResources(Me.FraçõesToolStripMenuItem, "FraçõesToolStripMenuItem")
        '
        'ToolStripComboBoxNumberFractionFormatting
        '
        Me.ToolStripComboBoxNumberFractionFormatting.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ToolStripComboBoxNumberFractionFormatting.Items.AddRange(New Object() {resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items1"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items2"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items3"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items4"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items5"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items6"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items7"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items8"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items9"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items10"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items11"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items12"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items13"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items14"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items15"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items16"), resources.GetString("ToolStripComboBoxNumberFractionFormatting.Items17")})
        Me.ToolStripComboBoxNumberFractionFormatting.Name = "ToolStripComboBoxNumberFractionFormatting"
        resources.ApplyResources(Me.ToolStripComboBoxNumberFractionFormatting, "ToolStripComboBoxNumberFractionFormatting")
        '
        'AjudaToolStripMenuItem
        '
        Me.AjudaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.help
        Me.AjudaToolStripMenuItem.Name = "AjudaToolStripMenuItem"
        resources.ApplyResources(Me.AjudaToolStripMenuItem, "AjudaToolStripMenuItem")
        '
        'ToolStripDropDownButton2
        '
        Me.ToolStripDropDownButton2.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsbAtivar, Me.tsbCalc, Me.tsbAbortCalc, Me.tsbSimultAdjustSolver})
        Me.ToolStripDropDownButton2.Image = Global.DWSIM.My.Resources.Resources.calculator
        resources.ApplyResources(Me.ToolStripDropDownButton2, "ToolStripDropDownButton2")
        Me.ToolStripDropDownButton2.Name = "ToolStripDropDownButton2"
        '
        'tsbAtivar
        '
        Me.tsbAtivar.Checked = True
        Me.tsbAtivar.CheckOnClick = True
        Me.tsbAtivar.CheckState = System.Windows.Forms.CheckState.Checked
        Me.tsbAtivar.Name = "tsbAtivar"
        resources.ApplyResources(Me.tsbAtivar, "tsbAtivar")
        '
        'tsbCalc
        '
        Me.tsbCalc.Image = Global.DWSIM.My.Resources.Resources.control_play_blue
        Me.tsbCalc.Name = "tsbCalc"
        resources.ApplyResources(Me.tsbCalc, "tsbCalc")
        '
        'tsbAbortCalc
        '
        Me.tsbAbortCalc.Image = Global.DWSIM.My.Resources.Resources.control_stop_blue
        Me.tsbAbortCalc.Name = "tsbAbortCalc"
        resources.ApplyResources(Me.tsbAbortCalc, "tsbAbortCalc")
        '
        'tsbSimultAdjustSolver
        '
        Me.tsbSimultAdjustSolver.Checked = True
        Me.tsbSimultAdjustSolver.CheckOnClick = True
        Me.tsbSimultAdjustSolver.CheckState = System.Windows.Forms.CheckState.Checked
        Me.tsbSimultAdjustSolver.Name = "tsbSimultAdjustSolver"
        resources.ApplyResources(Me.tsbSimultAdjustSolver, "tsbSimultAdjustSolver")
        '
        'tsbUndo
        '
        Me.tsbUndo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.tsbUndo, "tsbUndo")
        Me.tsbUndo.Image = Global.DWSIM.My.Resources.Resources.undo_161
        Me.tsbUndo.Name = "tsbUndo"
        '
        'tsbRedo
        '
        Me.tsbRedo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.tsbRedo, "tsbRedo")
        Me.tsbRedo.Image = Global.DWSIM.My.Resources.Resources.redo_16
        Me.tsbRedo.Name = "tsbRedo"
        '
        'tsbLogMessage
        '
        Me.tsbLogMessage.AutoToolTip = False
        resources.ApplyResources(Me.tsbLogMessage, "tsbLogMessage")
        Me.tsbLogMessage.Name = "tsbLogMessage"
        Me.tsbLogMessage.ShowDropDownArrow = False
        '
        'FormFlowsheet
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.PanelMobileCompatMode)
        Me.Controls.Add(Me.dckPanel)
        Me.Controls.Add(Me.QuestionBox_Panel)
        Me.Controls.Add(Me.MenuStrip1)
        Me.Controls.Add(Me.StatusStrip0)
        Me.DoubleBuffered = True
        Me.Name = "FormFlowsheet"
        Me.ShowIcon = False
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
        Me.StatusStrip0.ResumeLayout(False)
        Me.StatusStrip0.PerformLayout()
        Me.ResumeLayout(False)

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
    Public WithEvents GerenciadorDeReacoesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents PluginsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ScriptsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents IronRubyToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents consoletsmi As System.Windows.Forms.ToolStripMenuItem
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
    Friend WithEvents StatusStrip0 As System.Windows.Forms.StatusStrip
    Friend WithEvents tsbLogMessage As System.Windows.Forms.ToolStripDropDownButton
    Friend WithEvents ToolStripDropDownButton1 As System.Windows.Forms.ToolStripDropDownButton
    Friend WithEvents tsbmiCompounds As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbmiModels As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbmiReactions As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbmiUnits As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripDropDownButton2 As System.Windows.Forms.ToolStripDropDownButton
    Friend WithEvents tsbAtivar As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbCalc As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbAbortCalc As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbSimultAdjustSolver As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbUndo As System.Windows.Forms.ToolStripSplitButton
    Friend WithEvents tsbRedo As System.Windows.Forms.ToolStripSplitButton
    Friend WithEvents FormataçãoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NúmerosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripComboBoxNumberFormatting As System.Windows.Forms.ToolStripComboBox
    Friend WithEvents FraçõesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripComboBoxNumberFractionFormatting As System.Windows.Forms.ToolStripComboBox
    Friend WithEvents AjudaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripComboBoxUnitSystem As System.Windows.Forms.ToolStripComboBox
    Friend WithEvents tsbEditUnits As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents AdicionarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
End Class
