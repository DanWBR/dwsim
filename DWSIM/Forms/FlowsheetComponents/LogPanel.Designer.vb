<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class LogPanel
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(LogPanel))
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.Grid1 = New System.Windows.Forms.DataGridView()
        Me.Imagem = New System.Windows.Forms.DataGridViewImageColumn()
        Me.Indice = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Data = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Tipo = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Mensagem = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Info = New System.Windows.Forms.DataGridViewButtonColumn()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CopiarInformaçõesToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.LimparListaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
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
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'Grid1
        '
        resources.ApplyResources(Me.Grid1, "Grid1")
        Me.Grid1.AllowUserToAddRows = False
        Me.Grid1.AllowUserToDeleteRows = False
        Me.Grid1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.Grid1.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.Grid1.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.SingleHorizontal
        Me.Grid1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Imagem, Me.Indice, Me.Data, Me.Tipo, Me.Mensagem, Me.Info})
        Me.Grid1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.Grid1.Name = "Grid1"
        Me.Grid1.ReadOnly = True
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle2.Padding = New System.Windows.Forms.Padding(3)
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Grid1.RowHeadersDefaultCellStyle = DataGridViewCellStyle2
        Me.Grid1.RowHeadersVisible = False
        DataGridViewCellStyle3.Padding = New System.Windows.Forms.Padding(3)
        Me.Grid1.RowsDefaultCellStyle = DataGridViewCellStyle3
        Me.Grid1.RowTemplate.DefaultCellStyle.Padding = New System.Windows.Forms.Padding(2)
        Me.Grid1.RowTemplate.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Grid1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'Imagem
        '
        Me.Imagem.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCellsExceptHeader
        Me.Imagem.Description = " "
        Me.Imagem.FillWeight = 84.94611!
        resources.ApplyResources(Me.Imagem, "Imagem")
        Me.Imagem.Name = "Imagem"
        Me.Imagem.ReadOnly = True
        Me.Imagem.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Imagem.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        '
        'Indice
        '
        Me.Indice.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.DisplayedCellsExceptHeader
        Me.Indice.FillWeight = 75.12691!
        resources.ApplyResources(Me.Indice, "Indice")
        Me.Indice.Name = "Indice"
        Me.Indice.ReadOnly = True
        Me.Indice.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        '
        'Data
        '
        Me.Data.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.Data.FillWeight = 49.39458!
        resources.ApplyResources(Me.Data, "Data")
        Me.Data.Name = "Data"
        Me.Data.ReadOnly = True
        Me.Data.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        '
        'Tipo
        '
        Me.Tipo.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        Me.Tipo.FillWeight = 37.04594!
        resources.ApplyResources(Me.Tipo, "Tipo")
        Me.Tipo.Name = "Tipo"
        Me.Tipo.ReadOnly = True
        Me.Tipo.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Tipo.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        '
        'Mensagem
        '
        Me.Mensagem.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Mensagem.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.Mensagem, "Mensagem")
        Me.Mensagem.Name = "Mensagem"
        Me.Mensagem.ReadOnly = True
        Me.Mensagem.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        '
        'Info
        '
        Me.Info.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.AllCells
        resources.ApplyResources(Me.Info, "Info")
        Me.Info.Name = "Info"
        Me.Info.ReadOnly = True
        Me.Info.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
        Me.Info.Text = "+ Info"
        Me.Info.UseColumnTextForButtonValue = True
        '
        'ContextMenuStrip1
        '
        resources.ApplyResources(Me.ContextMenuStrip1, "ContextMenuStrip1")
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CopiarInformaçõesToolStripMenuItem, Me.LimparListaToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        '
        'CopiarInformaçõesToolStripMenuItem
        '
        resources.ApplyResources(Me.CopiarInformaçõesToolStripMenuItem, "CopiarInformaçõesToolStripMenuItem")
        Me.CopiarInformaçõesToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.copy
        Me.CopiarInformaçõesToolStripMenuItem.Name = "CopiarInformaçõesToolStripMenuItem"
        '
        'LimparListaToolStripMenuItem
        '
        resources.ApplyResources(Me.LimparListaToolStripMenuItem, "LimparListaToolStripMenuItem")
        Me.LimparListaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.LimparListaToolStripMenuItem.Name = "LimparListaToolStripMenuItem"
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
        'LogPanel
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.Grid1)
        Me.HideOnClose = True
        Me.Name = "LogPanel"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
        Me.TabPageContextMenuStrip = Me.dckMenu
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents Grid1 As System.Windows.Forms.DataGridView
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
    Friend WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Friend WithEvents LimparListaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Friend WithEvents Imagem As DataGridViewImageColumn
    Friend WithEvents Indice As DataGridViewTextBoxColumn
    Friend WithEvents Data As DataGridViewTextBoxColumn
    Friend WithEvents Tipo As DataGridViewTextBoxColumn
    Friend WithEvents Mensagem As DataGridViewTextBoxColumn
    Friend WithEvents Info As DataGridViewButtonColumn
    Friend WithEvents CopiarInformaçõesToolStripMenuItem As ToolStripMenuItem
End Class
