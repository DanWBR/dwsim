Imports System.IO
Imports System.Runtime.Serialization.Formatters
Imports Infralution.Localization
Imports System.Globalization
Imports System.Linq
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Configuration

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormMain
    Inherits System.Windows.Forms.Form

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.NewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoRegressaoUNIFACIPs = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AbrirDoDashboardToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiFOSSEE = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiSamples = New System.Windows.Forms.ToolStripMenuItem()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.SaveToDashboardTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.CloseAllToolstripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.toolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsFileSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.tsFolderSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.EditTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.PreferenciasDoDWSIMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolsTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.DatabaseManagerToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RegistroCAPEOPENToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DownloadSupplementarySoftwareToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NNUOToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PNUOToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CapitalCostToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.OPCPluginToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DTLToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PsycrometrySimulationTemplateToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WindowsMenu = New System.Windows.Forms.ToolStripMenuItem()
        Me.CascadeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TileVerticalToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TileHorizontalToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ViewTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.MostrarBarraDeFerramentasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PainelDeBoasvindasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.DocumentacaoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GuiaDoUsuarioToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DWSIMNaInternetToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WikiToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ForumToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RastreamentoDeBugsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DIscordChannelToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WhatsNewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiPrivateSupport = New System.Windows.Forms.ToolStripMenuItem()
        Me.tsmiFreeProTrial = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.NewToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.OpenToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.OpenFileS365 = New System.Windows.Forms.ToolStripButton()
        Me.SaveToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.SaveFileS365 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.tsbInspector = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator6 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbRegCO = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator8 = New System.Windows.Forms.ToolStripSeparator()
        Me.LoginButton = New System.Windows.Forms.ToolStripButton()
        Me.LogoutDropdown = New System.Windows.Forms.ToolStripDropDownButton()
        Me.DashboardToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.LoggedInS365Button = New System.Windows.Forms.ToolStripMenuItem()
        Me.LogoutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.BgLoadComp = New System.ComponentModel.BackgroundWorker()
        Me.bgLoadFile = New System.ComponentModel.BackgroundWorker()
        Me.bgSaveFile = New System.ComponentModel.BackgroundWorker()
        Me.bgLoadNews = New System.ComponentModel.BackgroundWorker()
        Me.TimerBackup = New System.Windows.Forms.Timer(Me.components)
        Me.bgSaveBackup = New System.ComponentModel.BackgroundWorker()
        Me.CultureManager1 = New Infralution.Localization.CultureManager(Me.components)
        Me.SettingsPanel = New System.Windows.Forms.Panel()
        Me.ButtonClose = New System.Windows.Forms.Button()
        Me.WelcomePanel = New System.Windows.Forms.Panel()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.tsblDonate = New System.Windows.Forms.ToolStripStatusLabel()
        Me.tsbdonate1 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.tsbdonate2 = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ToolStripStatusLabel2 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.MenuStrip1.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SettingsPanel.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.AllowItemReorder = True
        Me.MenuStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileTSMI, Me.EditTSMI, Me.ToolsTSMI, Me.WindowsMenu, Me.ViewTSMI, Me.HelpTSMI, Me.tsmiPrivateSupport, Me.tsmiFreeProTrial})
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.MdiWindowListItem = Me.WindowsMenu
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        Me.MenuStrip1.ShowItemToolTips = True
        '
        'FileTSMI
        '
        Me.FileTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripMenuItem, Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem, Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem, Me.NovoRegressaoUNIFACIPs, Me.OpenToolStripMenuItem, Me.AbrirDoDashboardToolStripMenuItem, Me.tsmiFOSSEE, Me.tsmiSamples, Me.toolStripSeparator, Me.SaveToDashboardTSMI, Me.SaveToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.ToolStripSeparator2, Me.CloseAllToolstripMenuItem, Me.toolStripSeparator1, Me.tsFileSeparator, Me.tsFolderSeparator, Me.ExitToolStripMenuItem})
        Me.FileTSMI.Name = "FileTSMI"
        Me.FileTSMI.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        resources.ApplyResources(Me.FileTSMI, "FileTSMI")
        '
        'NewToolStripMenuItem
        '
        Me.NewToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_new_file
        resources.ApplyResources(Me.NewToolStripMenuItem, "NewToolStripMenuItem")
        Me.NewToolStripMenuItem.Name = "NewToolStripMenuItem"
        '
        'NovoEstudoDoCriadorDeComponentesToolStripMenuItem
        '
        Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.wi0124_16
        Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem.Name = "NovoEstudoDoCriadorDeComponentesToolStripMenuItem"
        resources.ApplyResources(Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem, "NovoEstudoDoCriadorDeComponentesToolStripMenuItem")
        '
        'NovoEstudoDeRegressaoDeDadosToolStripMenuItem
        '
        Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.chart_line
        Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem.Name = "NovoEstudoDeRegressaoDeDadosToolStripMenuItem"
        resources.ApplyResources(Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem, "NovoEstudoDeRegressaoDeDadosToolStripMenuItem")
        '
        'NovoRegressaoUNIFACIPs
        '
        Me.NovoRegressaoUNIFACIPs.Image = Global.DWSIM.My.Resources.Resources.chart_line1
        Me.NovoRegressaoUNIFACIPs.Name = "NovoRegressaoUNIFACIPs"
        resources.ApplyResources(Me.NovoRegressaoUNIFACIPs, "NovoRegressaoUNIFACIPs")
        '
        'OpenToolStripMenuItem
        '
        Me.OpenToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_live_folder
        resources.ApplyResources(Me.OpenToolStripMenuItem, "OpenToolStripMenuItem")
        Me.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem"
        '
        'AbrirDoDashboardToolStripMenuItem
        '
        Me.AbrirDoDashboardToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.s365_logo_login
        Me.AbrirDoDashboardToolStripMenuItem.Name = "AbrirDoDashboardToolStripMenuItem"
        resources.ApplyResources(Me.AbrirDoDashboardToolStripMenuItem, "AbrirDoDashboardToolStripMenuItem")
        '
        'tsmiFOSSEE
        '
        Me.tsmiFOSSEE.Image = Global.DWSIM.My.Resources.Resources.icons8_live_folder
        Me.tsmiFOSSEE.Name = "tsmiFOSSEE"
        resources.ApplyResources(Me.tsmiFOSSEE, "tsmiFOSSEE")
        '
        'tsmiSamples
        '
        Me.tsmiSamples.Image = Global.DWSIM.My.Resources.Resources.icons8_live_folder
        resources.ApplyResources(Me.tsmiSamples, "tsmiSamples")
        Me.tsmiSamples.Name = "tsmiSamples"
        '
        'toolStripSeparator
        '
        Me.toolStripSeparator.Name = "toolStripSeparator"
        resources.ApplyResources(Me.toolStripSeparator, "toolStripSeparator")
        '
        'SaveToDashboardTSMI
        '
        Me.SaveToDashboardTSMI.Image = Global.DWSIM.My.Resources.Resources.s365_logo_login
        Me.SaveToDashboardTSMI.Name = "SaveToDashboardTSMI"
        resources.ApplyResources(Me.SaveToDashboardTSMI, "SaveToDashboardTSMI")
        '
        'SaveToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveToolStripMenuItem, "SaveToolStripMenuItem")
        Me.SaveToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_save
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        '
        'SaveAsToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveAsToolStripMenuItem, "SaveAsToolStripMenuItem")
        Me.SaveAsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_save_as
        Me.SaveAsToolStripMenuItem.Name = "SaveAsToolStripMenuItem"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        '
        'CloseAllToolstripMenuItem
        '
        resources.ApplyResources(Me.CloseAllToolstripMenuItem, "CloseAllToolstripMenuItem")
        Me.CloseAllToolstripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.CloseAllToolstripMenuItem.Name = "CloseAllToolstripMenuItem"
        '
        'toolStripSeparator1
        '
        Me.toolStripSeparator1.Name = "toolStripSeparator1"
        resources.ApplyResources(Me.toolStripSeparator1, "toolStripSeparator1")
        '
        'tsFileSeparator
        '
        Me.tsFileSeparator.Name = "tsFileSeparator"
        resources.ApplyResources(Me.tsFileSeparator, "tsFileSeparator")
        '
        'tsFolderSeparator
        '
        Me.tsFolderSeparator.Name = "tsFolderSeparator"
        resources.ApplyResources(Me.tsFolderSeparator, "tsFolderSeparator")
        '
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.undo_16
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        resources.ApplyResources(Me.ExitToolStripMenuItem, "ExitToolStripMenuItem")
        '
        'EditTSMI
        '
        Me.EditTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.PreferenciasDoDWSIMToolStripMenuItem})
        Me.EditTSMI.Name = "EditTSMI"
        Me.EditTSMI.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        resources.ApplyResources(Me.EditTSMI, "EditTSMI")
        '
        'PreferenciasDoDWSIMToolStripMenuItem
        '
        Me.PreferenciasDoDWSIMToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_edit
        Me.PreferenciasDoDWSIMToolStripMenuItem.Name = "PreferenciasDoDWSIMToolStripMenuItem"
        resources.ApplyResources(Me.PreferenciasDoDWSIMToolStripMenuItem, "PreferenciasDoDWSIMToolStripMenuItem")
        '
        'ToolsTSMI
        '
        Me.ToolsTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DatabaseManagerToolStripMenuItem, Me.RegistroCAPEOPENToolStripMenuItem, Me.DownloadSupplementarySoftwareToolStripMenuItem})
        Me.ToolsTSMI.Name = "ToolsTSMI"
        resources.ApplyResources(Me.ToolsTSMI, "ToolsTSMI")
        '
        'DatabaseManagerToolStripMenuItem
        '
        Me.DatabaseManagerToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_form_edit
        Me.DatabaseManagerToolStripMenuItem.Name = "DatabaseManagerToolStripMenuItem"
        resources.ApplyResources(Me.DatabaseManagerToolStripMenuItem, "DatabaseManagerToolStripMenuItem")
        '
        'RegistroCAPEOPENToolStripMenuItem
        '
        Me.RegistroCAPEOPENToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.colan2
        Me.RegistroCAPEOPENToolStripMenuItem.Name = "RegistroCAPEOPENToolStripMenuItem"
        resources.ApplyResources(Me.RegistroCAPEOPENToolStripMenuItem, "RegistroCAPEOPENToolStripMenuItem")
        '
        'DownloadSupplementarySoftwareToolStripMenuItem
        '
        Me.DownloadSupplementarySoftwareToolStripMenuItem.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.DownloadSupplementarySoftwareToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NNUOToolStripMenuItem, Me.PNUOToolStripMenuItem, Me.CapitalCostToolStripMenuItem, Me.OPCPluginToolStripMenuItem, Me.DTLToolStripMenuItem, Me.PsycrometrySimulationTemplateToolStripMenuItem})
        Me.DownloadSupplementarySoftwareToolStripMenuItem.Name = "DownloadSupplementarySoftwareToolStripMenuItem"
        resources.ApplyResources(Me.DownloadSupplementarySoftwareToolStripMenuItem, "DownloadSupplementarySoftwareToolStripMenuItem")
        '
        'NNUOToolStripMenuItem
        '
        Me.NNUOToolStripMenuItem.Name = "NNUOToolStripMenuItem"
        resources.ApplyResources(Me.NNUOToolStripMenuItem, "NNUOToolStripMenuItem")
        '
        'PNUOToolStripMenuItem
        '
        Me.PNUOToolStripMenuItem.Name = "PNUOToolStripMenuItem"
        resources.ApplyResources(Me.PNUOToolStripMenuItem, "PNUOToolStripMenuItem")
        '
        'CapitalCostToolStripMenuItem
        '
        Me.CapitalCostToolStripMenuItem.Name = "CapitalCostToolStripMenuItem"
        resources.ApplyResources(Me.CapitalCostToolStripMenuItem, "CapitalCostToolStripMenuItem")
        '
        'OPCPluginToolStripMenuItem
        '
        Me.OPCPluginToolStripMenuItem.Name = "OPCPluginToolStripMenuItem"
        resources.ApplyResources(Me.OPCPluginToolStripMenuItem, "OPCPluginToolStripMenuItem")
        '
        'DTLToolStripMenuItem
        '
        Me.DTLToolStripMenuItem.Name = "DTLToolStripMenuItem"
        resources.ApplyResources(Me.DTLToolStripMenuItem, "DTLToolStripMenuItem")
        '
        'PsycrometrySimulationTemplateToolStripMenuItem
        '
        Me.PsycrometrySimulationTemplateToolStripMenuItem.Name = "PsycrometrySimulationTemplateToolStripMenuItem"
        resources.ApplyResources(Me.PsycrometrySimulationTemplateToolStripMenuItem, "PsycrometrySimulationTemplateToolStripMenuItem")
        '
        'WindowsMenu
        '
        Me.WindowsMenu.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CascadeToolStripMenuItem, Me.TileVerticalToolStripMenuItem, Me.TileHorizontalToolStripMenuItem})
        Me.WindowsMenu.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.WindowsMenu.MergeIndex = 101
        Me.WindowsMenu.Name = "WindowsMenu"
        resources.ApplyResources(Me.WindowsMenu, "WindowsMenu")
        '
        'CascadeToolStripMenuItem
        '
        Me.CascadeToolStripMenuItem.AutoToolTip = True
        Me.CascadeToolStripMenuItem.CheckOnClick = True
        Me.CascadeToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_cascade
        Me.CascadeToolStripMenuItem.Name = "CascadeToolStripMenuItem"
        resources.ApplyResources(Me.CascadeToolStripMenuItem, "CascadeToolStripMenuItem")
        '
        'TileVerticalToolStripMenuItem
        '
        Me.TileVerticalToolStripMenuItem.AutoToolTip = True
        Me.TileVerticalToolStripMenuItem.CheckOnClick = True
        Me.TileVerticalToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_tile_horizontal
        Me.TileVerticalToolStripMenuItem.Name = "TileVerticalToolStripMenuItem"
        resources.ApplyResources(Me.TileVerticalToolStripMenuItem, "TileVerticalToolStripMenuItem")
        '
        'TileHorizontalToolStripMenuItem
        '
        Me.TileHorizontalToolStripMenuItem.AutoToolTip = True
        Me.TileHorizontalToolStripMenuItem.CheckOnClick = True
        Me.TileHorizontalToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_tile_vertical
        Me.TileHorizontalToolStripMenuItem.Name = "TileHorizontalToolStripMenuItem"
        resources.ApplyResources(Me.TileHorizontalToolStripMenuItem, "TileHorizontalToolStripMenuItem")
        '
        'ViewTSMI
        '
        Me.ViewTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MostrarBarraDeFerramentasToolStripMenuItem, Me.PainelDeBoasvindasToolStripMenuItem})
        Me.ViewTSMI.Name = "ViewTSMI"
        resources.ApplyResources(Me.ViewTSMI, "ViewTSMI")
        '
        'MostrarBarraDeFerramentasToolStripMenuItem
        '
        Me.MostrarBarraDeFerramentasToolStripMenuItem.Checked = True
        Me.MostrarBarraDeFerramentasToolStripMenuItem.CheckOnClick = True
        Me.MostrarBarraDeFerramentasToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked
        Me.MostrarBarraDeFerramentasToolStripMenuItem.Name = "MostrarBarraDeFerramentasToolStripMenuItem"
        resources.ApplyResources(Me.MostrarBarraDeFerramentasToolStripMenuItem, "MostrarBarraDeFerramentasToolStripMenuItem")
        '
        'PainelDeBoasvindasToolStripMenuItem
        '
        Me.PainelDeBoasvindasToolStripMenuItem.Checked = True
        Me.PainelDeBoasvindasToolStripMenuItem.CheckOnClick = True
        Me.PainelDeBoasvindasToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked
        Me.PainelDeBoasvindasToolStripMenuItem.Name = "PainelDeBoasvindasToolStripMenuItem"
        resources.ApplyResources(Me.PainelDeBoasvindasToolStripMenuItem, "PainelDeBoasvindasToolStripMenuItem")
        '
        'HelpTSMI
        '
        Me.HelpTSMI.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DocumentacaoToolStripMenuItem, Me.DWSIMNaInternetToolStripMenuItem, Me.WhatsNewToolStripMenuItem, Me.AboutToolStripMenuItem})
        Me.HelpTSMI.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.HelpTSMI.MergeIndex = 102
        Me.HelpTSMI.Name = "HelpTSMI"
        resources.ApplyResources(Me.HelpTSMI, "HelpTSMI")
        '
        'DocumentacaoToolStripMenuItem
        '
        Me.DocumentacaoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.GuiaDoUsuarioToolStripMenuItem})
        Me.DocumentacaoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.book_open
        Me.DocumentacaoToolStripMenuItem.Name = "DocumentacaoToolStripMenuItem"
        resources.ApplyResources(Me.DocumentacaoToolStripMenuItem, "DocumentacaoToolStripMenuItem")
        '
        'GuiaDoUsuarioToolStripMenuItem
        '
        Me.GuiaDoUsuarioToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_white_acrobat
        Me.GuiaDoUsuarioToolStripMenuItem.Name = "GuiaDoUsuarioToolStripMenuItem"
        resources.ApplyResources(Me.GuiaDoUsuarioToolStripMenuItem, "GuiaDoUsuarioToolStripMenuItem")
        '
        'DWSIMNaInternetToolStripMenuItem
        '
        Me.DWSIMNaInternetToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.WikiToolStripMenuItem, Me.ForumToolStripMenuItem, Me.RastreamentoDeBugsToolStripMenuItem, Me.DIscordChannelToolStripMenuItem})
        Me.DWSIMNaInternetToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.information
        Me.DWSIMNaInternetToolStripMenuItem.Name = "DWSIMNaInternetToolStripMenuItem"
        resources.ApplyResources(Me.DWSIMNaInternetToolStripMenuItem, "DWSIMNaInternetToolStripMenuItem")
        '
        'WikiToolStripMenuItem
        '
        resources.ApplyResources(Me.WikiToolStripMenuItem, "WikiToolStripMenuItem")
        Me.WikiToolStripMenuItem.Name = "WikiToolStripMenuItem"
        '
        'ForumToolStripMenuItem
        '
        Me.ForumToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.world_go
        Me.ForumToolStripMenuItem.Name = "ForumToolStripMenuItem"
        resources.ApplyResources(Me.ForumToolStripMenuItem, "ForumToolStripMenuItem")
        '
        'RastreamentoDeBugsToolStripMenuItem
        '
        Me.RastreamentoDeBugsToolStripMenuItem.Name = "RastreamentoDeBugsToolStripMenuItem"
        resources.ApplyResources(Me.RastreamentoDeBugsToolStripMenuItem, "RastreamentoDeBugsToolStripMenuItem")
        '
        'DIscordChannelToolStripMenuItem
        '
        Me.DIscordChannelToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icons8_discord
        Me.DIscordChannelToolStripMenuItem.Name = "DIscordChannelToolStripMenuItem"
        resources.ApplyResources(Me.DIscordChannelToolStripMenuItem, "DIscordChannelToolStripMenuItem")
        '
        'WhatsNewToolStripMenuItem
        '
        Me.WhatsNewToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.icon_info
        Me.WhatsNewToolStripMenuItem.Name = "WhatsNewToolStripMenuItem"
        resources.ApplyResources(Me.WhatsNewToolStripMenuItem, "WhatsNewToolStripMenuItem")
        '
        'AboutToolStripMenuItem
        '
        Me.AboutToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.DWSIM_ico_64
        Me.AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        resources.ApplyResources(Me.AboutToolStripMenuItem, "AboutToolStripMenuItem")
        '
        'tsmiPrivateSupport
        '
        Me.tsmiPrivateSupport.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsmiPrivateSupport.Image = Global.DWSIM.My.Resources.Resources.icons8_technical_support
        Me.tsmiPrivateSupport.Name = "tsmiPrivateSupport"
        resources.ApplyResources(Me.tsmiPrivateSupport, "tsmiPrivateSupport")
        '
        'tsmiFreeProTrial
        '
        Me.tsmiFreeProTrial.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsmiFreeProTrial.Image = Global.DWSIM.My.Resources.Resources.Icon1281
        Me.tsmiFreeProTrial.Name = "tsmiFreeProTrial"
        resources.ApplyResources(Me.tsmiFreeProTrial, "tsmiFreeProTrial")
        '
        'ToolStrip1
        '
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.ImageScalingSize = New System.Drawing.Size(20, 20)
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripButton, Me.OpenToolStripButton, Me.OpenFileS365, Me.SaveToolStripButton, Me.SaveFileS365, Me.ToolStripButton1, Me.ToolStripSeparator3, Me.ToolStripButton2, Me.tsbInspector, Me.ToolStripSeparator4, Me.ToolStripButton3, Me.ToolStripButton5, Me.ToolStripButton4, Me.ToolStripSeparator6, Me.tsbRegCO, Me.ToolStripSeparator8, Me.LoginButton, Me.LogoutDropdown})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        '
        'NewToolStripButton
        '
        Me.NewToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.NewToolStripButton.Image = Global.DWSIM.My.Resources.Resources.icons8_new_file
        resources.ApplyResources(Me.NewToolStripButton, "NewToolStripButton")
        Me.NewToolStripButton.Name = "NewToolStripButton"
        '
        'OpenToolStripButton
        '
        Me.OpenToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.OpenToolStripButton.Image = Global.DWSIM.My.Resources.Resources.icons8_live_folder
        resources.ApplyResources(Me.OpenToolStripButton, "OpenToolStripButton")
        Me.OpenToolStripButton.Name = "OpenToolStripButton"
        '
        'OpenFileS365
        '
        Me.OpenFileS365.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.OpenFileS365.Image = Global.DWSIM.My.Resources.Resources.open_file_s365
        resources.ApplyResources(Me.OpenFileS365, "OpenFileS365")
        Me.OpenFileS365.Name = "OpenFileS365"
        '
        'SaveToolStripButton
        '
        Me.SaveToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveToolStripButton, "SaveToolStripButton")
        Me.SaveToolStripButton.Image = Global.DWSIM.My.Resources.Resources.icons8_save
        Me.SaveToolStripButton.Name = "SaveToolStripButton"
        '
        'SaveFileS365
        '
        Me.SaveFileS365.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveFileS365, "SaveFileS365")
        Me.SaveFileS365.Image = Global.DWSIM.My.Resources.Resources.save_file_s365
        Me.SaveFileS365.Name = "SaveFileS365"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.icons8_save_as
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.application_edit
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'tsbInspector
        '
        Me.tsbInspector.CheckOnClick = True
        Me.tsbInspector.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbInspector.Image = Global.DWSIM.My.Resources.Resources.icons8_spy_male
        resources.ApplyResources(Me.tsbInspector, "tsbInspector")
        Me.tsbInspector.Name = "tsbInspector"
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.My.Resources.Resources.application_cascade
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripButton5
        '
        Me.ToolStripButton5.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton5.Image = Global.DWSIM.My.Resources.Resources.application_tile_horizontal
        resources.ApplyResources(Me.ToolStripButton5, "ToolStripButton5")
        Me.ToolStripButton5.Name = "ToolStripButton5"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.My.Resources.Resources.application_tile_vertical
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripSeparator6
        '
        Me.ToolStripSeparator6.Name = "ToolStripSeparator6"
        resources.ApplyResources(Me.ToolStripSeparator6, "ToolStripSeparator6")
        '
        'tsbRegCO
        '
        Me.tsbRegCO.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbRegCO.Image = Global.DWSIM.My.Resources.Resources.colan2
        resources.ApplyResources(Me.tsbRegCO, "tsbRegCO")
        Me.tsbRegCO.Name = "tsbRegCO"
        '
        'ToolStripSeparator8
        '
        Me.ToolStripSeparator8.Name = "ToolStripSeparator8"
        resources.ApplyResources(Me.ToolStripSeparator8, "ToolStripSeparator8")
        '
        'LoginButton
        '
        Me.LoginButton.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.LoginButton.Image = Global.DWSIM.My.Resources.Resources.s365_logo_login
        resources.ApplyResources(Me.LoginButton, "LoginButton")
        Me.LoginButton.Name = "LoginButton"
        '
        'LogoutDropdown
        '
        Me.LogoutDropdown.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.LogoutDropdown.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DashboardToolStripMenuItem, Me.LoggedInS365Button, Me.LogoutToolStripMenuItem})
        resources.ApplyResources(Me.LogoutDropdown, "LogoutDropdown")
        Me.LogoutDropdown.Name = "LogoutDropdown"
        '
        'DashboardToolStripMenuItem
        '
        Me.DashboardToolStripMenuItem.Name = "DashboardToolStripMenuItem"
        resources.ApplyResources(Me.DashboardToolStripMenuItem, "DashboardToolStripMenuItem")
        '
        'LoggedInS365Button
        '
        Me.LoggedInS365Button.Name = "LoggedInS365Button"
        resources.ApplyResources(Me.LoggedInS365Button, "LoggedInS365Button")
        '
        'LogoutToolStripMenuItem
        '
        Me.LogoutToolStripMenuItem.Name = "LogoutToolStripMenuItem"
        resources.ApplyResources(Me.LogoutToolStripMenuItem, "LogoutToolStripMenuItem")
        '
        'BgLoadComp
        '
        Me.BgLoadComp.WorkerReportsProgress = True
        '
        'bgLoadFile
        '
        Me.bgLoadFile.WorkerReportsProgress = True
        '
        'bgSaveFile
        '
        Me.bgSaveFile.WorkerReportsProgress = True
        '
        'TimerBackup
        '
        Me.TimerBackup.Enabled = True
        '
        'bgSaveBackup
        '
        Me.bgSaveBackup.WorkerReportsProgress = True
        '
        'CultureManager1
        '
        Me.CultureManager1.ManagedControl = Me
        '
        'SettingsPanel
        '
        Me.SettingsPanel.Controls.Add(Me.ButtonClose)
        resources.ApplyResources(Me.SettingsPanel, "SettingsPanel")
        Me.SettingsPanel.Name = "SettingsPanel"
        '
        'ButtonClose
        '
        resources.ApplyResources(Me.ButtonClose, "ButtonClose")
        Me.ButtonClose.Name = "ButtonClose"
        Me.ButtonClose.UseVisualStyleBackColor = True
        '
        'WelcomePanel
        '
        resources.ApplyResources(Me.WelcomePanel, "WelcomePanel")
        Me.WelcomePanel.Name = "WelcomePanel"
        '
        'StatusStrip1
        '
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.tsblDonate, Me.tsbdonate1, Me.tsbdonate2, Me.ToolStripStatusLabel2})
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        Me.StatusStrip1.SizingGrip = False
        '
        'tsblDonate
        '
        Me.tsblDonate.Image = Global.DWSIM.My.Resources.Resources.heart
        Me.tsblDonate.Name = "tsblDonate"
        resources.ApplyResources(Me.tsblDonate, "tsblDonate")
        '
        'tsbdonate1
        '
        Me.tsbdonate1.Image = Global.DWSIM.My.Resources.Resources.coffee
        Me.tsbdonate1.Name = "tsbdonate1"
        Me.tsbdonate1.ShowDropDownArrow = False
        resources.ApplyResources(Me.tsbdonate1, "tsbdonate1")
        '
        'tsbdonate2
        '
        Me.tsbdonate2.Image = Global.DWSIM.My.Resources.Resources.icons8_patreon
        Me.tsbdonate2.Name = "tsbdonate2"
        Me.tsbdonate2.ShowDropDownArrow = False
        resources.ApplyResources(Me.tsbdonate2, "tsbdonate2")
        '
        'ToolStripStatusLabel2
        '
        Me.ToolStripStatusLabel2.Name = "ToolStripStatusLabel2"
        resources.ApplyResources(Me.ToolStripStatusLabel2, "ToolStripStatusLabel2")
        Me.ToolStripStatusLabel2.Spring = True
        '
        'FormMain
        '
        Me.AllowDrop = True
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.StatusStrip1)
        Me.Controls.Add(Me.WelcomePanel)
        Me.Controls.Add(Me.SettingsPanel)
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.MenuStrip1)
        Me.DoubleBuffered = True
        Me.IsMdiContainer = True
        Me.MainMenuStrip = Me.MenuStrip1
        Me.Name = "FormMain"
        Me.WindowState = System.Windows.Forms.FormWindowState.Maximized
        Me.MenuStrip1.ResumeLayout(False)
        Me.MenuStrip1.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.SettingsPanel.ResumeLayout(False)
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Public WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Public WithEvents FileTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents OpenToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents toolStripSeparator As System.Windows.Forms.ToolStripSeparator
    Public WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents toolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents HelpTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents AboutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents NewToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents OpenToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents SaveToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents BgLoadComp As System.ComponentModel.BackgroundWorker
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents bgLoadFile As System.ComponentModel.BackgroundWorker
    Public WithEvents bgSaveFile As System.ComponentModel.BackgroundWorker
    Public WithEvents CloseAllToolstripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents WindowsMenu As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CascadeToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TileVerticalToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TileHorizontalToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents EditTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents bgLoadNews As System.ComponentModel.BackgroundWorker

    Public WithEvents PreferenciasDoDWSIMToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TimerBackup As System.Windows.Forms.Timer
    Public WithEvents bgSaveBackup As System.ComponentModel.BackgroundWorker


    Private WithEvents CultureManager1 As Infralution.Localization.CultureManager
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator

    Public Sub New()

        My.Application.MainWindowForm = Me

        If DWSIM.App.IsRunningOnMono Or GlobalSettings.Settings.AutomationMode Then

            If DWSIM.App.IsRunningOnMono Then

                'handler for unhandled exceptions (!)

                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException)
                AddHandler Application.ThreadException, AddressOf MyApplication_UnhandledException
                AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf MyApplication_UnhandledException2
                My.Application.UtilityPlugins = New Dictionary(Of String, Interfaces.IUtilityPlugin)

                'remove user.config file
                Try
                    Dim config = System.Configuration.ConfigurationManager.OpenExeConfiguration(System.Configuration.ConfigurationUserLevel.PerUserRoaming)
                    If File.Exists(config.FilePath) Then File.Delete(config.FilePath)
                Catch ex As Exception

                End Try

            End If

            If GlobalSettings.Settings.OldUI Then

                'settings workaround for Mono
                'load settings from INI file

                DWSIM.App.LoadSettings()

                DWSIM.App.InitializeSettings()

                'loads the current language

                If My.Settings.CultureInfo = "de" Or My.Settings.CultureInfo = "es" Or My.Settings.CultureInfo = "en-US" Then
                    My.Settings.CultureInfo = "en"
                End If

                My.Application.ChangeUICulture(My.Settings.CultureInfo)

            Else

                My.Application.ChangeUICulture("en")

            End If

        End If

        ' This call is required by the Windows Form Designer.

        If Not My.Application.CommandLineMode And Not Settings.CAPEOPENMode Then

            InitializeComponent()

            'If Not GlobalSettings.Settings.AutomationMode Then InitializeChromium()

        End If

        If GlobalSettings.Settings.OldUI Then

            ' Add any initialization after the InitializeComponent() call.

            If My.Settings.BackupFiles Is Nothing Then My.Settings.BackupFiles = New System.Collections.Specialized.StringCollection
            If My.Settings.GeneralSettings Is Nothing Then My.Settings.GeneralSettings = New System.Collections.Specialized.StringCollection
            If My.Settings.UserDatabases Is Nothing Then My.Settings.UserDatabases = New System.Collections.Specialized.StringCollection
            If My.Settings.UserInteractionsDatabases Is Nothing Then My.Settings.UserInteractionsDatabases = New System.Collections.Specialized.StringCollection

            'load user unit systems

            If My.Application.UserUnitSystems Is Nothing Then My.Application.UserUnitSystems = New Dictionary(Of String, SystemsOfUnits.Units)
            If My.Application.UtilityPlugins Is Nothing Then My.Application.UtilityPlugins = New Dictionary(Of String, Interfaces.IUtilityPlugin)

            Dim xdoc As New XDocument()
            Dim xel As XElement

            If My.Settings.UserUnits <> "" Then

                Dim myarraylist As New ArrayList

                Try
                    xdoc = XDocument.Load(New StringReader(My.Settings.UserUnits))
                Catch ex As Exception

                End Try

                If xdoc.Root Is Nothing Then

                    Dim formatter As New BinaryFormatter()
                    Dim bytearray() As Byte
                    bytearray = System.Text.Encoding.ASCII.GetBytes(My.Settings.UserUnits)
                    formatter = New BinaryFormatter()
                    Dim stream As New IO.MemoryStream(bytearray)

                    Try
                        myarraylist = CType(formatter.Deserialize(stream), ArrayList)
                    Catch ex As Exception
                    Finally
                        stream.Close()
                    End Try

                Else

                    Dim data As List(Of XElement) = xdoc.Element("Units").Elements.ToList

                    For Each xel In data
                        Try
                            Dim su As New SystemsOfUnits.SI()
                            su.LoadData(xel.Elements.ToList)
                            myarraylist.Add(su)
                        Catch ex As Exception

                        End Try
                    Next

                End If

                For Each su As SystemsOfUnits.Units In myarraylist
                    If Not My.Application.UserUnitSystems.ContainsKey(su.Name) Then My.Application.UserUnitSystems.Add(su.Name, su)
                Next

            End If

        End If

        pathsep = Path.DirectorySeparatorChar

        If Not Settings.CAPEOPENMode Then
            AddPropPacks()
            AddExternalUOs()
            GetComponents()
        End If

        With Me.AvailableUnitSystems

            .Add("SI", New SystemsOfUnits.SI)
            .Add("SI (Engineering)", New SystemsOfUnits.SI_ENG)
            .Add("CGS", New SystemsOfUnits.CGS)
            .Add("ENG", New SystemsOfUnits.English)
            .Add("C1", New SystemsOfUnits.SIUnits_Custom1)
            .Add("C2", New SystemsOfUnits.SIUnits_Custom2)
            .Add("C3", New SystemsOfUnits.SIUnits_Custom3)
            .Add("C4", New SystemsOfUnits.SIUnits_Custom4)
            .Add("C5", New SystemsOfUnits.SIUnits_Custom5)

            If Not My.Application.UserUnitSystems Is Nothing Then
                If My.Application.UserUnitSystems.Count > 0 Then
                    Dim su As New SystemsOfUnits.Units
                    For Each su In My.Application.UserUnitSystems.Values
                        If Not .ContainsKey(su.Name) Then .Add(su.Name, su)
                    Next
                End If
            End If

        End With

        LoadExtenders()

        UpdateIcon()

        'process command line arguments
        If My.Application.CommandLineMode Then
            Dim bp As New CommandLineProcessor
            bp.ProcessCommandLineArgs(Me)
        End If

        If DWSIM.App.IsRunningOnMono And GlobalSettings.Settings.OldUI And Not GlobalSettings.Settings.AutomationMode Then
            Using spsh As New SplashScreen
                spsh.Show()
                Application.DoEvents()
                Threading.Thread.Sleep(3000)
            End Using
        End If

    End Sub

    Public WithEvents DWSIMNaInternetToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents WikiToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ForumToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents RastreamentoDeBugsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator6 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents NovoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents NewToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoEstudoDeRegressaoDeDadosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoEstudoDoCriadorDeComponentesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents tsmiSamples As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoRegressaoUNIFACIPs As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents GuiaDoUsuarioToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SettingsPanel As System.Windows.Forms.Panel
    Friend WithEvents ButtonClose As System.Windows.Forms.Button
    Friend WithEvents ViewTSMI As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents MostrarBarraDeFerramentasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolsTSMI As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DatabaseManagerToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents RegistroCAPEOPENToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents tsbRegCO As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator8 As ToolStripSeparator
    Friend WithEvents tsmiFOSSEE As ToolStripMenuItem
    Public WithEvents WelcomePanel As Panel
    Friend WithEvents tsbInspector As ToolStripButton
    Friend WithEvents tsFileSeparator As ToolStripSeparator
    Friend WithEvents tsFolderSeparator As ToolStripSeparator
    Friend WithEvents NNUOToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents PNUOToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents CapitalCostToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents OPCPluginToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DTLToolStripMenuItem As ToolStripMenuItem
    Public WithEvents PainelDeBoasvindasToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents PsycrometrySimulationTemplateToolStripMenuItem As ToolStripMenuItem
    Public WithEvents DownloadSupplementarySoftwareToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents LoginButton As ToolStripButton
    Friend WithEvents LogoutDropdown As ToolStripDropDownButton
    Friend WithEvents LogoutToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents LoggedInS365Button As ToolStripMenuItem
    Public WithEvents DocumentacaoToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents tsmiFreeProTrial As ToolStripMenuItem
    Public WithEvents AbrirDoDashboardToolStripMenuItem As ToolStripMenuItem
    Public WithEvents SaveToDashboardTSMI As ToolStripMenuItem
    Public WithEvents OpenFileS365 As ToolStripButton
    Public WithEvents SaveFileS365 As ToolStripButton
    Friend WithEvents DashboardToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents DIscordChannelToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents tsblDonate As ToolStripStatusLabel
    Friend WithEvents tsbdonate1 As ToolStripDropDownButton
    Friend WithEvents tsbdonate2 As ToolStripDropDownButton
    Friend WithEvents ToolStripStatusLabel2 As ToolStripStatusLabel
    Public WithEvents StatusStrip1 As StatusStrip
    Friend WithEvents WhatsNewToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents tsmiPrivateSupport As ToolStripMenuItem
End Class
