<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class SpreadsheetForm
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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(SpreadsheetForm))
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.A = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.B = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.C = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.D = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.E = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.F = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.G = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.H = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.I = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.J = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.K = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.L = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.M = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.N = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.O = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.P = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Q = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.R = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.S = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.T = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.U = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.V = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.W = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.X = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Y = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Z = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.CelulaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.AvaliarFormulaToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ImportarDadosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ExportarDadosToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.LimparToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.CopiarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ColarToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.tbCell = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbValue = New System.Windows.Forms.TextBox()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.chkUpdate = New System.Windows.Forms.CheckBox()
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
        Me.Button4 = New System.Windows.Forms.Button()
        Me.chkUseRegionalSeparator = New System.Windows.Forms.CheckBox()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'DataGridView1
        '
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.DataGridView1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!)
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridView1.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.A, Me.B, Me.C, Me.D, Me.E, Me.F, Me.G, Me.H, Me.I, Me.J, Me.K, Me.L, Me.M, Me.N, Me.O, Me.P, Me.Q, Me.R, Me.S, Me.T, Me.U, Me.V, Me.W, Me.X, Me.Y, Me.Z})
        Me.DataGridView1.ContextMenuStrip = Me.ContextMenuStrip1
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        Me.DataGridView1.RowTemplate.DefaultCellStyle.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleRight
        Me.DataGridView1.RowTemplate.Height = 20
        Me.DataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.DataGridView1.ShowCellErrors = False
        Me.DataGridView1.ShowEditingIcon = False
        Me.DataGridView1.ShowRowErrors = False
        '
        'A
        '
        resources.ApplyResources(Me.A, "A")
        Me.A.Name = "A"
        Me.A.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'B
        '
        resources.ApplyResources(Me.B, "B")
        Me.B.Name = "B"
        Me.B.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'C
        '
        resources.ApplyResources(Me.C, "C")
        Me.C.Name = "C"
        Me.C.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'D
        '
        resources.ApplyResources(Me.D, "D")
        Me.D.Name = "D"
        Me.D.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'E
        '
        resources.ApplyResources(Me.E, "E")
        Me.E.Name = "E"
        Me.E.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'F
        '
        resources.ApplyResources(Me.F, "F")
        Me.F.Name = "F"
        Me.F.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'G
        '
        resources.ApplyResources(Me.G, "G")
        Me.G.Name = "G"
        Me.G.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'H
        '
        resources.ApplyResources(Me.H, "H")
        Me.H.Name = "H"
        Me.H.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'I
        '
        resources.ApplyResources(Me.I, "I")
        Me.I.Name = "I"
        Me.I.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'J
        '
        resources.ApplyResources(Me.J, "J")
        Me.J.Name = "J"
        Me.J.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'K
        '
        resources.ApplyResources(Me.K, "K")
        Me.K.Name = "K"
        Me.K.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'L
        '
        resources.ApplyResources(Me.L, "L")
        Me.L.Name = "L"
        Me.L.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'M
        '
        resources.ApplyResources(Me.M, "M")
        Me.M.Name = "M"
        Me.M.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'N
        '
        resources.ApplyResources(Me.N, "N")
        Me.N.Name = "N"
        Me.N.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'O
        '
        resources.ApplyResources(Me.O, "O")
        Me.O.Name = "O"
        Me.O.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'P
        '
        resources.ApplyResources(Me.P, "P")
        Me.P.Name = "P"
        Me.P.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Q
        '
        resources.ApplyResources(Me.Q, "Q")
        Me.Q.Name = "Q"
        Me.Q.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'R
        '
        resources.ApplyResources(Me.R, "R")
        Me.R.Name = "R"
        Me.R.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'S
        '
        resources.ApplyResources(Me.S, "S")
        Me.S.Name = "S"
        Me.S.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'T
        '
        resources.ApplyResources(Me.T, "T")
        Me.T.Name = "T"
        Me.T.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'U
        '
        resources.ApplyResources(Me.U, "U")
        Me.U.Name = "U"
        Me.U.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'V
        '
        resources.ApplyResources(Me.V, "V")
        Me.V.Name = "V"
        Me.V.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'W
        '
        resources.ApplyResources(Me.W, "W")
        Me.W.Name = "W"
        Me.W.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'X
        '
        resources.ApplyResources(Me.X, "X")
        Me.X.Name = "X"
        Me.X.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Y
        '
        resources.ApplyResources(Me.Y, "Y")
        Me.Y.Name = "Y"
        Me.Y.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'Z
        '
        resources.ApplyResources(Me.Z, "Z")
        Me.Z.Name = "Z"
        Me.Z.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.CelulaToolStripMenuItem, Me.ToolStripSeparator2, Me.AvaliarFormulaToolStripMenuItem, Me.ImportarDadosToolStripMenuItem, Me.ExportarDadosToolStripMenuItem, Me.ToolStripSeparator1, Me.LimparToolStripMenuItem, Me.CopiarToolStripMenuItem, Me.ColarToolStripMenuItem})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.ContextMenuStrip1, "ContextMenuStrip1")
        '
        'CelulaToolStripMenuItem
        '
        resources.ApplyResources(Me.CelulaToolStripMenuItem, "CelulaToolStripMenuItem")
        Me.CelulaToolStripMenuItem.Name = "CelulaToolStripMenuItem"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        '
        'AvaliarFormulaToolStripMenuItem
        '
        Me.AvaliarFormulaToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.calculator
        Me.AvaliarFormulaToolStripMenuItem.Name = "AvaliarFormulaToolStripMenuItem"
        resources.ApplyResources(Me.AvaliarFormulaToolStripMenuItem, "AvaliarFormulaToolStripMenuItem")
        '
        'ImportarDadosToolStripMenuItem
        '
        Me.ImportarDadosToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_in1
        Me.ImportarDadosToolStripMenuItem.Name = "ImportarDadosToolStripMenuItem"
        resources.ApplyResources(Me.ImportarDadosToolStripMenuItem, "ImportarDadosToolStripMenuItem")
        '
        'ExportarDadosToolStripMenuItem
        '
        Me.ExportarDadosToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.arrow_out
        Me.ExportarDadosToolStripMenuItem.Name = "ExportarDadosToolStripMenuItem"
        resources.ApplyResources(Me.ExportarDadosToolStripMenuItem, "ExportarDadosToolStripMenuItem")
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        '
        'LimparToolStripMenuItem
        '
        Me.LimparToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.LimparToolStripMenuItem.Name = "LimparToolStripMenuItem"
        resources.ApplyResources(Me.LimparToolStripMenuItem, "LimparToolStripMenuItem")
        '
        'CopiarToolStripMenuItem
        '
        Me.CopiarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.page_copy
        Me.CopiarToolStripMenuItem.Name = "CopiarToolStripMenuItem"
        resources.ApplyResources(Me.CopiarToolStripMenuItem, "CopiarToolStripMenuItem")
        '
        'ColarToolStripMenuItem
        '
        Me.ColarToolStripMenuItem.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        Me.ColarToolStripMenuItem.Name = "ColarToolStripMenuItem"
        resources.ApplyResources(Me.ColarToolStripMenuItem, "ColarToolStripMenuItem")
        '
        'tbCell
        '
        resources.ApplyResources(Me.tbCell, "tbCell")
        Me.tbCell.Name = "tbCell"
        Me.tbCell.ReadOnly = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbValue
        '
        resources.ApplyResources(Me.tbValue, "tbValue")
        Me.tbValue.Name = "tbValue"
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.Image = Global.DWSIM.My.Resources.Resources.tick
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Image = Global.DWSIM.My.Resources.Resources.arrow_refresh
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'chkUpdate
        '
        resources.ApplyResources(Me.chkUpdate, "chkUpdate")
        Me.chkUpdate.Checked = True
        Me.chkUpdate.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkUpdate.Name = "chkUpdate"
        Me.chkUpdate.UseVisualStyleBackColor = True
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
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.Image = Global.DWSIM.My.Resources.Resources.page_copy
        Me.Button4.Name = "Button4"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'chkUseRegionalSeparator
        '
        resources.ApplyResources(Me.chkUseRegionalSeparator, "chkUseRegionalSeparator")
        Me.chkUseRegionalSeparator.Checked = True
        Me.chkUseRegionalSeparator.CheckState = System.Windows.Forms.CheckState.Checked
        Me.chkUseRegionalSeparator.Name = "chkUseRegionalSeparator"
        Me.chkUseRegionalSeparator.UseVisualStyleBackColor = True
        '
        'SpreadsheetForm
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.chkUseRegionalSeparator)
        Me.Controls.Add(Me.Button4)
        Me.Controls.Add(Me.DataGridView1)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.chkUpdate)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.tbValue)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.tbCell)
        Me.Controls.Add(Me.Button2)
        Me.DoubleBuffered = True
        Me.HideOnClose = True
        Me.Name = "SpreadsheetForm"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabPageContextMenuStrip = Me.dckMenu
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Public WithEvents tbCell As System.Windows.Forms.TextBox
    Public WithEvents Button1 As System.Windows.Forms.Button
    Public WithEvents Button2 As System.Windows.Forms.Button
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents tbValue As System.Windows.Forms.TextBox
    Public WithEvents Button3 As System.Windows.Forms.Button
    Public WithEvents ContextMenuStrip1 As System.Windows.Forms.ContextMenuStrip
    Public WithEvents ImportarDadosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents LimparToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CopiarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ColarToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents CelulaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents AvaliarFormulaToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents ExportarDadosToolStripMenuItem As System.Windows.Forms.ToolStripMenuItem
    Public WithEvents chkUpdate As System.Windows.Forms.CheckBox
    Public WithEvents A As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents B As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents C As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents D As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents E As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents F As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents G As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents H As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents I As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents J As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents K As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents L As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents M As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents N As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents O As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents P As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Q As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents R As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents S As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents T As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents U As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents V As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents W As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents X As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Y As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Z As System.Windows.Forms.DataGridViewTextBoxColumn
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
    Public WithEvents Button4 As System.Windows.Forms.Button
    Public WithEvents chkUseRegionalSeparator As System.Windows.Forms.CheckBox
End Class
