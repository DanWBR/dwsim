<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class frmObjList
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(frmObjList))
        Me.ImageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.TreeViewObj = New System.Windows.Forms.TreeView()
        Me.TableLayoutPanel4 = New System.Windows.Forms.TableLayoutPanel()
        Me.TableLayoutPanel3 = New System.Windows.Forms.TableLayoutPanel()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.TBSearch = New System.Windows.Forms.TextBox()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ToolStripMenuItem1 = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripMenuItem2 = New System.Windows.Forms.ToolStripMenuItem()
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
        Me.TableLayoutPanel4.SuspendLayout()
        Me.TableLayoutPanel3.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'ImageList1
        '
        Me.ImageList1.ImageStream = CType(resources.GetObject("ImageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.ImageList1.TransparentColor = System.Drawing.Color.Transparent
        Me.ImageList1.Images.SetKeyName(0, "tag_blue.png")
        Me.ImageList1.Images.SetKeyName(1, "stream_mat_16.png")
        Me.ImageList1.Images.SetKeyName(2, "stream_en_16.png")
        Me.ImageList1.Images.SetKeyName(3, "uo_mixer_16.png")
        Me.ImageList1.Images.SetKeyName(4, "uo_split_16.png")
        Me.ImageList1.Images.SetKeyName(5, "uo_pipe_16.png")
        Me.ImageList1.Images.SetKeyName(6, "uo_valve_16.png")
        Me.ImageList1.Images.SetKeyName(7, "uo_pump_16.png")
        Me.ImageList1.Images.SetKeyName(8, "uo_tank_16.png")
        Me.ImageList1.Images.SetKeyName(9, "uo_flash_16.png")
        Me.ImageList1.Images.SetKeyName(10, "uo_compr_16.png")
        Me.ImageList1.Images.SetKeyName(11, "uo_heater_16.png")
        Me.ImageList1.Images.SetKeyName(12, "uo_cooler_16.png")
        Me.ImageList1.Images.SetKeyName(13, "uo_expan_16.png")
        Me.ImageList1.Images.SetKeyName(14, "re_conv_16.png")
        Me.ImageList1.Images.SetKeyName(15, "re_equi_16.png")
        Me.ImageList1.Images.SetKeyName(16, "re_gibbs_16.png")
        Me.ImageList1.Images.SetKeyName(17, "re_cstr_16.png")
        Me.ImageList1.Images.SetKeyName(18, "re_pfr_16.png")
        Me.ImageList1.Images.SetKeyName(19, "uo_hx_16.png")
        Me.ImageList1.Images.SetKeyName(20, "col_sc_16.png")
        Me.ImageList1.Images.SetKeyName(21, "col_dc_16.png")
        Me.ImageList1.Images.SetKeyName(22, "col_abs_16.png")
        Me.ImageList1.Images.SetKeyName(23, "col_rebabs_16.png")
        Me.ImageList1.Images.SetKeyName(24, "col_rflabs_16.png")
        Me.ImageList1.Images.SetKeyName(25, "uo_compsep_16.png")
        Me.ImageList1.Images.SetKeyName(26, "uo_orifice_16.png")
        Me.ImageList1.Images.SetKeyName(27, "uo_custom_16.png")
        Me.ImageList1.Images.SetKeyName(28, "uo_co_16.png")
        Me.ImageList1.Images.SetKeyName(29, "uo_solidsep_16.png")
        Me.ImageList1.Images.SetKeyName(30, "uo_filter_16.png")
        Me.ImageList1.Images.SetKeyName(31, "uo_excel_16.png")
        Me.ImageList1.Images.SetKeyName(32, "uo_fs_16.png")
        '
        'TreeViewObj
        '
        resources.ApplyResources(Me.TreeViewObj, "TreeViewObj")
        Me.TreeViewObj.FullRowSelect = True
        Me.TreeViewObj.HideSelection = False
        Me.TreeViewObj.ImageList = Me.ImageList1
        Me.TreeViewObj.Name = "TreeViewObj"
        Me.TreeViewObj.Nodes.AddRange(New System.Windows.Forms.TreeNode() {CType(resources.GetObject("TreeViewObj.Nodes"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes1"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes2"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes3"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes4"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes5"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes6"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes7"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes8"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes9"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes10"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes11"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes12"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes13"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes14"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes15"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes16"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes17"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes18"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes19"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes20"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes21"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes22"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes23"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes24"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes25"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes26"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes27"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes28"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes29"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes30"), System.Windows.Forms.TreeNode), CType(resources.GetObject("TreeViewObj.Nodes31"), System.Windows.Forms.TreeNode)})
        '
        'TableLayoutPanel4
        '
        resources.ApplyResources(Me.TableLayoutPanel4, "TableLayoutPanel4")
        Me.TableLayoutPanel4.Controls.Add(Me.TreeViewObj, 0, 1)
        Me.TableLayoutPanel4.Controls.Add(Me.TableLayoutPanel3, 0, 0)
        Me.TableLayoutPanel4.Name = "TableLayoutPanel4"
        '
        'TableLayoutPanel3
        '
        resources.ApplyResources(Me.TableLayoutPanel3, "TableLayoutPanel3")
        Me.TableLayoutPanel3.Controls.Add(Me.Label2, 0, 0)
        Me.TableLayoutPanel3.Controls.Add(Me.TBSearch, 1, 0)
        Me.TableLayoutPanel3.Name = "TableLayoutPanel3"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'TBSearch
        '
        resources.ApplyResources(Me.TBSearch, "TBSearch")
        Me.TBSearch.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.SuggestAppend
        Me.TBSearch.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.CustomSource
        Me.TBSearch.Name = "TBSearch"
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripMenuItem1, Me.ToolStripMenuItem2})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.ContextMenuStrip1, "ContextMenuStrip1")
        '
        'ToolStripMenuItem1
        '
        Me.ToolStripMenuItem1.Image = Global.DWSIM.My.Resources.Resources.shading
        Me.ToolStripMenuItem1.Name = "ToolStripMenuItem1"
        resources.ApplyResources(Me.ToolStripMenuItem1, "ToolStripMenuItem1")
        '
        'ToolStripMenuItem2
        '
        Me.ToolStripMenuItem2.Image = Global.DWSIM.My.Resources.Resources.shape_move_front
        Me.ToolStripMenuItem2.Name = "ToolStripMenuItem2"
        resources.ApplyResources(Me.ToolStripMenuItem2, "ToolStripMenuItem2")
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
        'frmObjList
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.TableLayoutPanel4)
        Me.DoubleBuffered = True
        Me.Name = "frmObjList"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.TabText = Me.Text
        Me.TableLayoutPanel4.ResumeLayout(False)
        Me.TableLayoutPanel3.ResumeLayout(False)
        Me.TableLayoutPanel3.PerformLayout()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents ImageList1 As System.Windows.Forms.ImageList
    Public WithEvents TreeViewObj As System.Windows.Forms.TreeView
    Public WithEvents TableLayoutPanel4 As System.Windows.Forms.TableLayoutPanel
    Public WithEvents TableLayoutPanel3 As System.Windows.Forms.TableLayoutPanel
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents TBSearch As System.Windows.Forms.TextBox
    Public WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Public WithEvents ToolStripMenuItem1 As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripMenuItem2 As System.Windows.Forms.ToolStripMenuItem
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
