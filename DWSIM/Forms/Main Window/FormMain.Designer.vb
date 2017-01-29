Imports System.IO
Imports System.Runtime.Serialization.Formatters
Imports Infralution.Localization
Imports System.Globalization
Imports System.Linq
Imports System.Runtime.Serialization.Formatters.Binary

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormMain
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormMain))
        Me.MenuStrip1 = New System.Windows.Forms.MenuStrip()
        Me.FileToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NewToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.NovoRegressaoUNIFACIPs = New System.Windows.Forms.ToolStripMenuItem()
        Me.OpenToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.SaveToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAllToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.SaveAsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.CloseAllToolstripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.toolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ExitToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.VerToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.PreferenciasDoDWSIMToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.FerramentasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DatabaseManagerToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RegistroCAPEOPENToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WindowsMenu = New System.Windows.Forms.ToolStripMenuItem()
        Me.CascadeToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TileVerticalToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.TileHorizontalToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.VerToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.MostrarBarraDeFerramentasToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.HelpToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ContentsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DocumentacaoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ManualTecnicoToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GuiaDoUsuarioToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.GuiaDoUsuarioHTMLToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.toolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator()
        Me.DWSIMNaInternetToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DownloadsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.WikiToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ForumToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.RastreamentoDeBugsToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.DonateToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.AboutToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.NewToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.OpenToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.SaveToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.SaveAllToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton5 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator6 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton6 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton7 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton8 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator7 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbRegCO = New System.Windows.Forms.ToolStripButton()
        Me.BgLoadComp = New System.ComponentModel.BackgroundWorker()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.bgLoadFile = New System.ComponentModel.BackgroundWorker()
        Me.bgSaveFile = New System.ComponentModel.BackgroundWorker()
        Me.bgLoadNews = New System.ComponentModel.BackgroundWorker()
        Me.StatusStrip1 = New System.Windows.Forms.StatusStrip()
        Me.ToolStripStatusLabel1 = New System.Windows.Forms.ToolStripStatusLabel()
        Me.tslupd = New System.Windows.Forms.ToolStripStatusLabel()
        Me.TimerBackup = New System.Windows.Forms.Timer(Me.components)
        Me.bgSaveBackup = New System.ComponentModel.BackgroundWorker()
        Me.CultureManager1 = New Infralution.Localization.CultureManager(Me.components)
        Me.bgUpdater = New System.ComponentModel.BackgroundWorker()
        Me.sfdUpdater = New System.Windows.Forms.SaveFileDialog()
        Me.SaveStudyDlg = New System.Windows.Forms.SaveFileDialog()
        Me.SaveRegStudyDlg = New System.Windows.Forms.SaveFileDialog()
        Me.SaveUnifacIPRegrDlg = New System.Windows.Forms.SaveFileDialog()
        Me.SettingsPanel = New System.Windows.Forms.Panel()
        Me.ButtonClose = New System.Windows.Forms.Button()
        Me.UpdateBox_Panel = New System.Windows.Forms.Panel()
        Me.UpdateBox_ProgressBar1 = New System.Windows.Forms.ProgressBar()
        Me.UpdateBox_Button2 = New System.Windows.Forms.Button()
        Me.UpdateBox_Button1 = New System.Windows.Forms.Button()
        Me.UpdateBox_PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.UpdateBox_Label1 = New System.Windows.Forms.Label()
        Me.ErrorBox_Panel = New System.Windows.Forms.Panel()
        Me.ErrorBox_Button1 = New System.Windows.Forms.Button()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.ErrorBox_Label1 = New System.Windows.Forms.Label()
        Me.MenuStrip1.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.StatusStrip1.SuspendLayout()
        Me.SettingsPanel.SuspendLayout()
        Me.UpdateBox_Panel.SuspendLayout()
        CType(Me.UpdateBox_PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ErrorBox_Panel.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'MenuStrip1
        '
        Me.MenuStrip1.AllowItemReorder = True
        resources.ApplyResources(Me.MenuStrip1, "MenuStrip1")
        Me.MenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.FileToolStripMenuItem, Me.VerToolStripMenuItem, Me.FerramentasToolStripMenuItem, Me.WindowsMenu, Me.VerToolStripMenuItem1, Me.HelpToolStripMenuItem})
        Me.MenuStrip1.MdiWindowListItem = Me.WindowsMenu
        Me.MenuStrip1.Name = "MenuStrip1"
        Me.MenuStrip1.ShowItemToolTips = True
        '
        'FileToolStripMenuItem
        '
        Me.FileToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripMenuItem, Me.NovoEstudoDoCriadorDeComponentesToolStripMenuItem, Me.NovoEstudoDeRegressaoDeDadosToolStripMenuItem, Me.NovoRegressaoUNIFACIPs, Me.OpenToolStripMenuItem, Me.ToolStripMenuItem1, Me.toolStripSeparator, Me.SaveToolStripMenuItem, Me.SaveAllToolStripMenuItem, Me.SaveAsToolStripMenuItem, Me.ToolStripSeparator2, Me.CloseAllToolstripMenuItem, Me.toolStripSeparator1, Me.ExitToolStripMenuItem})
        Me.FileToolStripMenuItem.Name = "FileToolStripMenuItem"
        Me.FileToolStripMenuItem.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        resources.ApplyResources(Me.FileToolStripMenuItem, "FileToolStripMenuItem")
        '
        'NewToolStripMenuItem
        '
        Me.NewToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_white
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
        Me.OpenToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.folder_page_white
        resources.ApplyResources(Me.OpenToolStripMenuItem, "OpenToolStripMenuItem")
        Me.OpenToolStripMenuItem.Name = "OpenToolStripMenuItem"
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Image = Global.DWSIM.My.Resources.Resources.folder_page_white
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        '
        'toolStripSeparator
        '
        Me.toolStripSeparator.Name = "toolStripSeparator"
        resources.ApplyResources(Me.toolStripSeparator, "toolStripSeparator")
        '
        'SaveToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveToolStripMenuItem, "SaveToolStripMenuItem")
        Me.SaveToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_save
        Me.SaveToolStripMenuItem.Name = "SaveToolStripMenuItem"
        '
        'SaveAllToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveAllToolStripMenuItem, "SaveAllToolStripMenuItem")
        Me.SaveAllToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.disk_multiple
        Me.SaveAllToolStripMenuItem.Name = "SaveAllToolStripMenuItem"
        '
        'SaveAsToolStripMenuItem
        '
        resources.ApplyResources(Me.SaveAsToolStripMenuItem, "SaveAsToolStripMenuItem")
        Me.SaveAsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.disk
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
        'ExitToolStripMenuItem
        '
        Me.ExitToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.undo_16
        Me.ExitToolStripMenuItem.Name = "ExitToolStripMenuItem"
        resources.ApplyResources(Me.ExitToolStripMenuItem, "ExitToolStripMenuItem")
        '
        'VerToolStripMenuItem
        '
        Me.VerToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.PreferenciasDoDWSIMToolStripMenuItem})
        Me.VerToolStripMenuItem.Name = "VerToolStripMenuItem"
        Me.VerToolStripMenuItem.Overflow = System.Windows.Forms.ToolStripItemOverflow.AsNeeded
        resources.ApplyResources(Me.VerToolStripMenuItem, "VerToolStripMenuItem")
        '
        'PreferenciasDoDWSIMToolStripMenuItem
        '
        Me.PreferenciasDoDWSIMToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.application_edit
        Me.PreferenciasDoDWSIMToolStripMenuItem.MergeIndex = 12
        Me.PreferenciasDoDWSIMToolStripMenuItem.Name = "PreferenciasDoDWSIMToolStripMenuItem"
        resources.ApplyResources(Me.PreferenciasDoDWSIMToolStripMenuItem, "PreferenciasDoDWSIMToolStripMenuItem")
        '
        'FerramentasToolStripMenuItem
        '
        Me.FerramentasToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DatabaseManagerToolStripMenuItem, Me.RegistroCAPEOPENToolStripMenuItem})
        Me.FerramentasToolStripMenuItem.Name = "FerramentasToolStripMenuItem"
        resources.ApplyResources(Me.FerramentasToolStripMenuItem, "FerramentasToolStripMenuItem")
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
        'VerToolStripMenuItem1
        '
        Me.VerToolStripMenuItem1.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.MostrarBarraDeFerramentasToolStripMenuItem})
        Me.VerToolStripMenuItem1.Name = "VerToolStripMenuItem1"
        resources.ApplyResources(Me.VerToolStripMenuItem1, "VerToolStripMenuItem1")
        '
        'MostrarBarraDeFerramentasToolStripMenuItem
        '
        Me.MostrarBarraDeFerramentasToolStripMenuItem.Checked = True
        Me.MostrarBarraDeFerramentasToolStripMenuItem.CheckOnClick = True
        Me.MostrarBarraDeFerramentasToolStripMenuItem.CheckState = System.Windows.Forms.CheckState.Checked
        Me.MostrarBarraDeFerramentasToolStripMenuItem.Name = "MostrarBarraDeFerramentasToolStripMenuItem"
        resources.ApplyResources(Me.MostrarBarraDeFerramentasToolStripMenuItem, "MostrarBarraDeFerramentasToolStripMenuItem")
        '
        'HelpToolStripMenuItem
        '
        Me.HelpToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ContentsToolStripMenuItem, Me.DocumentacaoToolStripMenuItem, Me.toolStripSeparator5, Me.DWSIMNaInternetToolStripMenuItem, Me.DonateToolStripMenuItem, Me.AboutToolStripMenuItem})
        Me.HelpToolStripMenuItem.MergeAction = System.Windows.Forms.MergeAction.Insert
        Me.HelpToolStripMenuItem.MergeIndex = 102
        Me.HelpToolStripMenuItem.Name = "HelpToolStripMenuItem"
        resources.ApplyResources(Me.HelpToolStripMenuItem, "HelpToolStripMenuItem")
        '
        'ContentsToolStripMenuItem
        '
        Me.ContentsToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.help
        Me.ContentsToolStripMenuItem.Name = "ContentsToolStripMenuItem"
        resources.ApplyResources(Me.ContentsToolStripMenuItem, "ContentsToolStripMenuItem")
        '
        'DocumentacaoToolStripMenuItem
        '
        Me.DocumentacaoToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ManualTecnicoToolStripMenuItem, Me.GuiaDoUsuarioToolStripMenuItem, Me.GuiaDoUsuarioHTMLToolStripMenuItem})
        Me.DocumentacaoToolStripMenuItem.Name = "DocumentacaoToolStripMenuItem"
        resources.ApplyResources(Me.DocumentacaoToolStripMenuItem, "DocumentacaoToolStripMenuItem")
        '
        'ManualTecnicoToolStripMenuItem
        '
        Me.ManualTecnicoToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_white_acrobat
        Me.ManualTecnicoToolStripMenuItem.Name = "ManualTecnicoToolStripMenuItem"
        resources.ApplyResources(Me.ManualTecnicoToolStripMenuItem, "ManualTecnicoToolStripMenuItem")
        '
        'GuiaDoUsuarioToolStripMenuItem
        '
        Me.GuiaDoUsuarioToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_white_acrobat
        Me.GuiaDoUsuarioToolStripMenuItem.Name = "GuiaDoUsuarioToolStripMenuItem"
        resources.ApplyResources(Me.GuiaDoUsuarioToolStripMenuItem, "GuiaDoUsuarioToolStripMenuItem")
        '
        'GuiaDoUsuarioHTMLToolStripMenuItem
        '
        Me.GuiaDoUsuarioHTMLToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_world
        Me.GuiaDoUsuarioHTMLToolStripMenuItem.Name = "GuiaDoUsuarioHTMLToolStripMenuItem"
        resources.ApplyResources(Me.GuiaDoUsuarioHTMLToolStripMenuItem, "GuiaDoUsuarioHTMLToolStripMenuItem")
        '
        'toolStripSeparator5
        '
        Me.toolStripSeparator5.Name = "toolStripSeparator5"
        resources.ApplyResources(Me.toolStripSeparator5, "toolStripSeparator5")
        '
        'DWSIMNaInternetToolStripMenuItem
        '
        Me.DWSIMNaInternetToolStripMenuItem.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.DownloadsToolStripMenuItem, Me.WikiToolStripMenuItem, Me.ForumToolStripMenuItem, Me.RastreamentoDeBugsToolStripMenuItem})
        Me.DWSIMNaInternetToolStripMenuItem.Name = "DWSIMNaInternetToolStripMenuItem"
        resources.ApplyResources(Me.DWSIMNaInternetToolStripMenuItem, "DWSIMNaInternetToolStripMenuItem")
        '
        'DownloadsToolStripMenuItem
        '
        resources.ApplyResources(Me.DownloadsToolStripMenuItem, "DownloadsToolStripMenuItem")
        Me.DownloadsToolStripMenuItem.Name = "DownloadsToolStripMenuItem"
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
        resources.ApplyResources(Me.RastreamentoDeBugsToolStripMenuItem, "RastreamentoDeBugsToolStripMenuItem")
        Me.RastreamentoDeBugsToolStripMenuItem.Name = "RastreamentoDeBugsToolStripMenuItem"
        '
        'DonateToolStripMenuItem
        '
        Me.DonateToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.money_add
        Me.DonateToolStripMenuItem.Name = "DonateToolStripMenuItem"
        resources.ApplyResources(Me.DonateToolStripMenuItem, "DonateToolStripMenuItem")
        '
        'AboutToolStripMenuItem
        '
        Me.AboutToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.information
        Me.AboutToolStripMenuItem.Name = "AboutToolStripMenuItem"
        resources.ApplyResources(Me.AboutToolStripMenuItem, "AboutToolStripMenuItem")
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripButton, Me.OpenToolStripButton, Me.SaveToolStripButton, Me.ToolStripButton1, Me.SaveAllToolStripButton, Me.ToolStripSeparator3, Me.ToolStripButton2, Me.ToolStripSeparator4, Me.ToolStripButton3, Me.ToolStripButton5, Me.ToolStripButton4, Me.ToolStripSeparator6, Me.ToolStripButton6, Me.ToolStripButton7, Me.ToolStripButton8, Me.ToolStripSeparator7, Me.tsbRegCO})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'NewToolStripButton
        '
        Me.NewToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.NewToolStripButton.Image = Global.DWSIM.My.Resources.Resources.page_white
        resources.ApplyResources(Me.NewToolStripButton, "NewToolStripButton")
        Me.NewToolStripButton.Name = "NewToolStripButton"
        '
        'OpenToolStripButton
        '
        Me.OpenToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.OpenToolStripButton.Image = Global.DWSIM.My.Resources.Resources.folder_page_white
        resources.ApplyResources(Me.OpenToolStripButton, "OpenToolStripButton")
        Me.OpenToolStripButton.Name = "OpenToolStripButton"
        '
        'SaveToolStripButton
        '
        Me.SaveToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveToolStripButton, "SaveToolStripButton")
        Me.SaveToolStripButton.Image = Global.DWSIM.My.Resources.Resources.page_save
        Me.SaveToolStripButton.Name = "SaveToolStripButton"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.disk
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'SaveAllToolStripButton
        '
        Me.SaveAllToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveAllToolStripButton, "SaveAllToolStripButton")
        Me.SaveAllToolStripButton.Image = Global.DWSIM.My.Resources.Resources.disk_multiple
        Me.SaveAllToolStripButton.Name = "SaveAllToolStripButton"
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
        'ToolStripButton6
        '
        Me.ToolStripButton6.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton6.Image = Global.DWSIM.My.Resources.Resources.help
        resources.ApplyResources(Me.ToolStripButton6, "ToolStripButton6")
        Me.ToolStripButton6.Name = "ToolStripButton6"
        '
        'ToolStripButton7
        '
        Me.ToolStripButton7.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton7.Image = Global.DWSIM.My.Resources.Resources.money_add
        resources.ApplyResources(Me.ToolStripButton7, "ToolStripButton7")
        Me.ToolStripButton7.Name = "ToolStripButton7"
        '
        'ToolStripButton8
        '
        Me.ToolStripButton8.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton8.Image = Global.DWSIM.My.Resources.Resources.information
        resources.ApplyResources(Me.ToolStripButton8, "ToolStripButton8")
        Me.ToolStripButton8.Name = "ToolStripButton8"
        '
        'ToolStripSeparator7
        '
        Me.ToolStripSeparator7.Name = "ToolStripSeparator7"
        resources.ApplyResources(Me.ToolStripSeparator7, "ToolStripSeparator7")
        '
        'tsbRegCO
        '
        Me.tsbRegCO.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbRegCO.Image = Global.DWSIM.My.Resources.Resources.colan2
        resources.ApplyResources(Me.tsbRegCO, "tsbRegCO")
        Me.tsbRegCO.Name = "tsbRegCO"
        '
        'BgLoadComp
        '
        Me.BgLoadComp.WorkerReportsProgress = True
        '
        'OpenFileDialog1
        '
        Me.OpenFileDialog1.DefaultExt = "dwsim"
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        Me.OpenFileDialog1.FilterIndex = 7
        Me.OpenFileDialog1.RestoreDirectory = True
        '
        'SaveFileDialog1
        '
        Me.SaveFileDialog1.DefaultExt = "dwsim"
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        Me.SaveFileDialog1.FilterIndex = 2
        Me.SaveFileDialog1.RestoreDirectory = True
        Me.SaveFileDialog1.SupportMultiDottedExtensions = True
        '
        'bgLoadFile
        '
        Me.bgLoadFile.WorkerReportsProgress = True
        '
        'bgSaveFile
        '
        Me.bgSaveFile.WorkerReportsProgress = True
        '
        'StatusStrip1
        '
        Me.StatusStrip1.GripMargin = New System.Windows.Forms.Padding(0)
        Me.StatusStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Visible
        Me.StatusStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripStatusLabel1, Me.tslupd})
        Me.StatusStrip1.LayoutStyle = System.Windows.Forms.ToolStripLayoutStyle.HorizontalStackWithOverflow
        resources.ApplyResources(Me.StatusStrip1, "StatusStrip1")
        Me.StatusStrip1.Name = "StatusStrip1"
        Me.StatusStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        '
        'ToolStripStatusLabel1
        '
        Me.ToolStripStatusLabel1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        Me.ToolStripStatusLabel1.Margin = New System.Windows.Forms.Padding(0)
        Me.ToolStripStatusLabel1.Name = "ToolStripStatusLabel1"
        resources.ApplyResources(Me.ToolStripStatusLabel1, "ToolStripStatusLabel1")
        Me.ToolStripStatusLabel1.Spring = True
        '
        'tslupd
        '
        Me.tslupd.Image = Global.DWSIM.My.Resources.Resources.information
        resources.ApplyResources(Me.tslupd, "tslupd")
        Me.tslupd.IsLink = True
        Me.tslupd.LinkBehavior = System.Windows.Forms.LinkBehavior.HoverUnderline
        Me.tslupd.Name = "tslupd"
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
        'bgUpdater
        '
        '
        'sfdUpdater
        '
        resources.ApplyResources(Me.sfdUpdater, "sfdUpdater")
        '
        'SaveStudyDlg
        '
        resources.ApplyResources(Me.SaveStudyDlg, "SaveStudyDlg")
        '
        'SaveRegStudyDlg
        '
        resources.ApplyResources(Me.SaveRegStudyDlg, "SaveRegStudyDlg")
        '
        'SaveUnifacIPRegrDlg
        '
        resources.ApplyResources(Me.SaveUnifacIPRegrDlg, "SaveUnifacIPRegrDlg")
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
        'UpdateBox_Panel
        '
        Me.UpdateBox_Panel.BackColor = System.Drawing.Color.SteelBlue
        Me.UpdateBox_Panel.Controls.Add(Me.UpdateBox_ProgressBar1)
        Me.UpdateBox_Panel.Controls.Add(Me.UpdateBox_Button2)
        Me.UpdateBox_Panel.Controls.Add(Me.UpdateBox_Button1)
        Me.UpdateBox_Panel.Controls.Add(Me.UpdateBox_PictureBox1)
        Me.UpdateBox_Panel.Controls.Add(Me.UpdateBox_Label1)
        resources.ApplyResources(Me.UpdateBox_Panel, "UpdateBox_Panel")
        Me.UpdateBox_Panel.Name = "UpdateBox_Panel"
        '
        'UpdateBox_ProgressBar1
        '
        resources.ApplyResources(Me.UpdateBox_ProgressBar1, "UpdateBox_ProgressBar1")
        Me.UpdateBox_ProgressBar1.Name = "UpdateBox_ProgressBar1"
        Me.UpdateBox_ProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous
        '
        'UpdateBox_Button2
        '
        resources.ApplyResources(Me.UpdateBox_Button2, "UpdateBox_Button2")
        Me.UpdateBox_Button2.DialogResult = System.Windows.Forms.DialogResult.No
        Me.UpdateBox_Button2.Name = "UpdateBox_Button2"
        Me.UpdateBox_Button2.UseVisualStyleBackColor = True
        '
        'UpdateBox_Button1
        '
        resources.ApplyResources(Me.UpdateBox_Button1, "UpdateBox_Button1")
        Me.UpdateBox_Button1.DialogResult = System.Windows.Forms.DialogResult.Yes
        Me.UpdateBox_Button1.Name = "UpdateBox_Button1"
        Me.UpdateBox_Button1.UseVisualStyleBackColor = True
        '
        'UpdateBox_PictureBox1
        '
        Me.UpdateBox_PictureBox1.Image = Global.DWSIM.My.Resources.Resources.world_connect
        resources.ApplyResources(Me.UpdateBox_PictureBox1, "UpdateBox_PictureBox1")
        Me.UpdateBox_PictureBox1.Name = "UpdateBox_PictureBox1"
        Me.UpdateBox_PictureBox1.TabStop = False
        '
        'UpdateBox_Label1
        '
        resources.ApplyResources(Me.UpdateBox_Label1, "UpdateBox_Label1")
        Me.UpdateBox_Label1.ForeColor = System.Drawing.Color.White
        Me.UpdateBox_Label1.Name = "UpdateBox_Label1"
        '
        'ErrorBox_Panel
        '
        Me.ErrorBox_Panel.BackColor = System.Drawing.Color.White
        Me.ErrorBox_Panel.Controls.Add(Me.ErrorBox_Button1)
        Me.ErrorBox_Panel.Controls.Add(Me.PictureBox1)
        Me.ErrorBox_Panel.Controls.Add(Me.ErrorBox_Label1)
        resources.ApplyResources(Me.ErrorBox_Panel, "ErrorBox_Panel")
        Me.ErrorBox_Panel.Name = "ErrorBox_Panel"
        '
        'ErrorBox_Button1
        '
        resources.ApplyResources(Me.ErrorBox_Button1, "ErrorBox_Button1")
        Me.ErrorBox_Button1.DialogResult = System.Windows.Forms.DialogResult.No
        Me.ErrorBox_Button1.Name = "ErrorBox_Button1"
        Me.ErrorBox_Button1.UseVisualStyleBackColor = True
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources._error
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'ErrorBox_Label1
        '
        resources.ApplyResources(Me.ErrorBox_Label1, "ErrorBox_Label1")
        Me.ErrorBox_Label1.ForeColor = System.Drawing.Color.DarkRed
        Me.ErrorBox_Label1.Name = "ErrorBox_Label1"
        '
        'FormMain
        '
        Me.AllowDrop = True
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.ErrorBox_Panel)
        Me.Controls.Add(Me.UpdateBox_Panel)
        Me.Controls.Add(Me.SettingsPanel)
        Me.Controls.Add(Me.StatusStrip1)
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
        Me.StatusStrip1.ResumeLayout(False)
        Me.StatusStrip1.PerformLayout()
        Me.SettingsPanel.ResumeLayout(False)
        Me.UpdateBox_Panel.ResumeLayout(False)
        Me.UpdateBox_Panel.PerformLayout()
        CType(Me.UpdateBox_PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ErrorBox_Panel.ResumeLayout(False)
        Me.ErrorBox_Panel.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Public WithEvents MenuStrip1 As System.Windows.Forms.MenuStrip
    Public WithEvents FileToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents OpenToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents toolStripSeparator As System.Windows.Forms.ToolStripSeparator
    Public WithEvents SaveToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents SaveAsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents toolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ExitToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents HelpToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ContentsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents toolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents AboutToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents NewToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents OpenToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents SaveToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents BgLoadComp As System.ComponentModel.BackgroundWorker
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents OpenFileDialog1 As System.Windows.Forms.OpenFileDialog
    Public WithEvents SaveFileDialog1 As System.Windows.Forms.SaveFileDialog
    Public WithEvents bgLoadFile As System.ComponentModel.BackgroundWorker
    Public WithEvents bgSaveFile As System.ComponentModel.BackgroundWorker
    Public WithEvents SaveAllToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents SaveAllToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CloseAllToolstripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents WindowsMenu As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CascadeToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TileVerticalToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TileHorizontalToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents VerToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents bgLoadNews As System.ComponentModel.BackgroundWorker
    Public WithEvents StatusStrip1 As System.Windows.Forms.StatusStrip
    Public WithEvents ToolStripStatusLabel1 As System.Windows.Forms.ToolStripStatusLabel

    Public WithEvents PreferenciasDoDWSIMToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents TimerBackup As System.Windows.Forms.Timer
    Public WithEvents bgSaveBackup As System.ComponentModel.BackgroundWorker


    Private WithEvents CultureManager1 As Infralution.Localization.CultureManager
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator

    Public Sub New()

        If File.Exists(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "update.run") Then
            'launch updater
            If DWSIM.App.RunningPlatform = DWSIM.App.Platform.Linux Then
                Dim startInfo = New ProcessStartInfo("mono", My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "DWSIM.Updater.exe")
                startInfo.UseShellExecute = True
                Process.Start(startInfo)
            Else
                Dim startInfo = New ProcessStartInfo(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "DWSIM.Updater.exe")
                startInfo.UseShellExecute = True
                Process.Start(startInfo)
            End If
            Process.GetCurrentProcess.Kill()
        End If

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

            'settings workaround for Mono
            'load settings from INI file
            DWSIM.App.LoadSettings()

                DWSIM.App.InitializeSettings()

                'loads the current language
                My.Application.ChangeUICulture(My.Settings.CultureInfo)

            End If

            ' This call is required by the Windows Form Designer.

            If Not My.Application.CommandLineMode Or Not Settings.CAPEOPENMode Then
            InitializeComponent()
        End If

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

        pathsep = Path.DirectorySeparatorChar

        If Not Settings.CAPEOPENMode AndAlso Not Settings.AutomationMode Then
            AddPropPacks()
            AddFlashAlgorithms()
            GetComponents()
        End If

        With Me.AvailableUnitSystems

            .Add("SI", New SystemsOfUnits.SI)
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

        If DWSIM.App.IsRunningOnMono() Then
            Using spsh As New SplashScreen
                spsh.Show()
                Application.DoEvents()
                Threading.Thread.Sleep(3000)
            End Using
        End If

    End Sub
    Public WithEvents DWSIMNaInternetToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents DownloadsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents WikiToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ForumToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents RastreamentoDeBugsToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents DonateToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton5 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator6 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton6 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton7 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton8 As System.Windows.Forms.ToolStripButton
    Friend WithEvents bgUpdater As System.ComponentModel.BackgroundWorker
    Friend WithEvents sfdUpdater As System.Windows.Forms.SaveFileDialog
    Friend WithEvents tslupd As System.Windows.Forms.ToolStripStatusLabel
    Friend WithEvents NovoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents NewToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoEstudoDeRegressaoDeDadosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoEstudoDoCriadorDeComponentesToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveStudyDlg As System.Windows.Forms.SaveFileDialog
    Friend WithEvents SaveRegStudyDlg As System.Windows.Forms.SaveFileDialog
    Public WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents NovoRegressaoUNIFACIPs As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SaveUnifacIPRegrDlg As System.Windows.Forms.SaveFileDialog
    Friend WithEvents DocumentacaoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ManualTecnicoToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents GuiaDoUsuarioToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents SettingsPanel As System.Windows.Forms.Panel
    Friend WithEvents ButtonClose As System.Windows.Forms.Button
    Friend WithEvents VerToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents MostrarBarraDeFerramentasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents FerramentasToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents DatabaseManagerToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents UpdateBox_Panel As System.Windows.Forms.Panel
    Friend WithEvents UpdateBox_Button2 As System.Windows.Forms.Button
    Friend WithEvents UpdateBox_Button1 As System.Windows.Forms.Button
    Friend WithEvents UpdateBox_PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents UpdateBox_Label1 As System.Windows.Forms.Label
    Friend WithEvents UpdateBox_ProgressBar1 As System.Windows.Forms.ProgressBar
    Friend WithEvents RegistroCAPEOPENToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents ToolStripSeparator7 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents tsbRegCO As System.Windows.Forms.ToolStripButton
    Friend WithEvents ErrorBox_Panel As System.Windows.Forms.Panel
    Friend WithEvents ErrorBox_Button1 As System.Windows.Forms.Button
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents ErrorBox_Label1 As System.Windows.Forms.Label
    Friend WithEvents GuiaDoUsuarioHTMLToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem

End Class
