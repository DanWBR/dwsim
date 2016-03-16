<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmObjListView
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmObjListView))
        Me.ImageList = New System.Windows.Forms.ImageList(Me.components)
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.col0 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.col1 = New System.Windows.Forms.DataGridViewImageColumn()
        Me.col2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
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
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'ImageList
        '
        Me.ImageList.ImageStream = CType(resources.GetObject("ImageList.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList.TransparentColor = System.Drawing.Color.White
        Me.ImageList.Images.SetKeyName(0, "stream_mat_32.png")
        Me.ImageList.Images.SetKeyName(1, "stream_en_32.png")
        Me.ImageList.Images.SetKeyName(2, "uo_mixer_32.png")
        Me.ImageList.Images.SetKeyName(3, "uo_split_32.png")
        Me.ImageList.Images.SetKeyName(4, "uo_cooler_32.png")
        Me.ImageList.Images.SetKeyName(5, "uo_heater_32.png")
        Me.ImageList.Images.SetKeyName(6, "uo_pipe_32.png")
        Me.ImageList.Images.SetKeyName(7, "uo_valve_32.png")
        Me.ImageList.Images.SetKeyName(8, "uo_pump_32.png")
        Me.ImageList.Images.SetKeyName(9, "uo_compr_32.png")
        Me.ImageList.Images.SetKeyName(10, "uo_expan_32.png")
        Me.ImageList.Images.SetKeyName(11, "uo_hx_32.png")
        Me.ImageList.Images.SetKeyName(12, "col_sc_32.png")
        Me.ImageList.Images.SetKeyName(13, "col_dc_32.png")
        Me.ImageList.Images.SetKeyName(14, "col_abs_32.png")
        Me.ImageList.Images.SetKeyName(15, "col_rebabs_32.png")
        Me.ImageList.Images.SetKeyName(16, "col_rflabs_32.png")
        Me.ImageList.Images.SetKeyName(17, "uo_compsep_32.png")
        Me.ImageList.Images.SetKeyName(18, "uo_tank_32.png")
        Me.ImageList.Images.SetKeyName(19, "uo_flash_32.png")
        Me.ImageList.Images.SetKeyName(20, "re_conv_32.png")
        Me.ImageList.Images.SetKeyName(21, "re_equi_32.png")
        Me.ImageList.Images.SetKeyName(22, "re_gibbs_32.png")
        Me.ImageList.Images.SetKeyName(23, "re_cstr_32.png")
        Me.ImageList.Images.SetKeyName(24, "re_pfr_32.png")
        Me.ImageList.Images.SetKeyName(25, "uo_orifice_32.png")
        Me.ImageList.Images.SetKeyName(26, "lo_adjust_32.png")
        Me.ImageList.Images.SetKeyName(27, "lo_spec_32.png")
        Me.ImageList.Images.SetKeyName(28, "lo_recy_32.png")
        Me.ImageList.Images.SetKeyName(29, "lo_enrecy_32.png")
        Me.ImageList.Images.SetKeyName(30, "uo_custom_32.png")
        Me.ImageList.Images.SetKeyName(31, "uo_excel_32.png")
        Me.ImageList.Images.SetKeyName(32, "uo_fs_32.png")
        Me.ImageList.Images.SetKeyName(33, "uo_co_32.png")
        Me.ImageList.Images.SetKeyName(34, "uo_solidsep_32.png")
        Me.ImageList.Images.SetKeyName(35, "uo_filter_32.png")
        '
        'DataGridView1
        '
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        Me.DataGridView1.AllowUserToOrderColumns = True
        Me.DataGridView1.AllowUserToResizeColumns = False
        Me.DataGridView1.AllowUserToResizeRows = False
        Me.DataGridView1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.DataGridView1.BackgroundColor = System.Drawing.Color.White
        Me.DataGridView1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.DataGridView1.CellBorderStyle = System.Windows.Forms.DataGridViewCellBorderStyle.None
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.ColumnHeadersVisible = False
        Me.DataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.col0, Me.col1, Me.col2})
        Me.DataGridView1.MultiSelect = False
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.ReadOnly = True
        Me.DataGridView1.RowHeadersVisible = False
        Me.DataGridView1.RowTemplate.Height = 34
        Me.DataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.FullRowSelect
        '
        'col0
        '
        resources.ApplyResources(Me.col0, "col0")
        Me.col0.Name = "col0"
        Me.col0.ReadOnly = True
        '
        'col1
        '
        Me.col1.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.None
        resources.ApplyResources(Me.col1, "col1")
        Me.col1.Name = "col1"
        Me.col1.ReadOnly = True
        '
        'col2
        '
        Me.col2.AutoSizeMode = System.Windows.Forms.DataGridViewAutoSizeColumnMode.Fill
        resources.ApplyResources(Me.col2, "col2")
        Me.col2.Name = "col2"
        Me.col2.ReadOnly = True
        Me.col2.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Programmatic
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
        'frmObjListView
        '
        resources.ApplyResources(Me, "$this")
        Me.AllowDrop = True
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.DataGridView1)
        Me.DockAreas = CType(((((WeifenLuo.WinFormsUI.Docking.DockAreas.Float Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockLeft) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockRight) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockTop) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockBottom), WeifenLuo.WinFormsUI.Docking.DockAreas)
        Me.DoubleBuffered = True
        Me.HideOnClose = True
        Me.Name = "frmObjListView"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        Me.ShowInTaskbar = False
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.TabText = Me.Text
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents ImageList As System.Windows.Forms.ImageList
    Friend WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Friend WithEvents col0 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents col1 As System.Windows.Forms.DataGridViewImageColumn
    Friend WithEvents col2 As System.Windows.Forms.DataGridViewTextBoxColumn
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
End Class
