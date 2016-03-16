<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormPhEnv
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormPhEnv))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.GroupBox6 = New System.Windows.Forms.GroupBox()
        Me.PanelCalc = New System.Windows.Forms.Panel()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.LabelStatus = New System.Windows.Forms.Label()
        Me.CheckBoxHYDVAP = New System.Windows.Forms.CheckBox()
        Me.ComboBox2 = New System.Windows.Forms.ComboBox()
        Me.chkpip = New System.Windows.Forms.CheckBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.chkhyd = New System.Windows.Forms.CheckBox()
        Me.TextBox1 = New System.Windows.Forms.NumericUpDown()
        Me.CheckBox2 = New System.Windows.Forms.CheckBox()
        Me.CheckBox3 = New System.Windows.Forms.CheckBox()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.ComboBox3 = New System.Windows.Forms.ComboBox()
        Me.Label23 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.ComboBox1 = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        Me.GraphControl = New ZedGraph.ZedGraphControl()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.Grid1 = New System.Windows.Forms.DataGridView()
        Me.BackgroundWorker1 = New System.ComponentModel.BackgroundWorker()
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
        Me.GroupBox6.SuspendLayout()
        Me.PanelCalc.SuspendLayout()
        CType(Me.TextBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem1.SuspendLayout()
        Me.FaTabStripItem2.SuspendLayout()
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox6
        '
        resources.ApplyResources(Me.GroupBox6, "GroupBox6")
        Me.GroupBox6.Controls.Add(Me.PanelCalc)
        Me.GroupBox6.Controls.Add(Me.CheckBoxHYDVAP)
        Me.GroupBox6.Controls.Add(Me.ComboBox2)
        Me.GroupBox6.Controls.Add(Me.chkpip)
        Me.GroupBox6.Controls.Add(Me.Button1)
        Me.GroupBox6.Controls.Add(Me.chkhyd)
        Me.GroupBox6.Controls.Add(Me.TextBox1)
        Me.GroupBox6.Controls.Add(Me.CheckBox2)
        Me.GroupBox6.Controls.Add(Me.CheckBox3)
        Me.GroupBox6.Controls.Add(Me.CheckBox1)
        Me.GroupBox6.Controls.Add(Me.ComboBox3)
        Me.GroupBox6.Controls.Add(Me.Label23)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.TabStop = False
        '
        'PanelCalc
        '
        resources.ApplyResources(Me.PanelCalc, "PanelCalc")
        Me.PanelCalc.Controls.Add(Me.Button2)
        Me.PanelCalc.Controls.Add(Me.LabelStatus)
        Me.PanelCalc.Name = "PanelCalc"
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        '
        'LabelStatus
        '
        resources.ApplyResources(Me.LabelStatus, "LabelStatus")
        Me.LabelStatus.Name = "LabelStatus"
        '
        'CheckBoxHYDVAP
        '
        resources.ApplyResources(Me.CheckBoxHYDVAP, "CheckBoxHYDVAP")
        Me.CheckBoxHYDVAP.Name = "CheckBoxHYDVAP"
        '
        'ComboBox2
        '
        resources.ApplyResources(Me.ComboBox2, "ComboBox2")
        Me.ComboBox2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox2.DropDownWidth = 260
        Me.ComboBox2.Items.AddRange(New Object() {resources.GetString("ComboBox2.Items"), resources.GetString("ComboBox2.Items1"), resources.GetString("ComboBox2.Items2"), resources.GetString("ComboBox2.Items3")})
        Me.ComboBox2.Name = "ComboBox2"
        '
        'chkpip
        '
        resources.ApplyResources(Me.chkpip, "chkpip")
        Me.chkpip.Name = "chkpip"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        '
        'chkhyd
        '
        resources.ApplyResources(Me.chkhyd, "chkhyd")
        Me.chkhyd.Name = "chkhyd"
        '
        'TextBox1
        '
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.DecimalPlaces = 2
        Me.TextBox1.Increment = New Decimal(New Integer() {1, 0, 0, 131072})
        Me.TextBox1.Maximum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Value = New Decimal(New Integer() {5, 0, 0, 65536})
        '
        'CheckBox2
        '
        resources.ApplyResources(Me.CheckBox2, "CheckBox2")
        Me.CheckBox2.Checked = True
        Me.CheckBox2.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckBox2.Name = "CheckBox2"
        '
        'CheckBox3
        '
        resources.ApplyResources(Me.CheckBox3, "CheckBox3")
        Me.CheckBox3.Name = "CheckBox3"
        '
        'CheckBox1
        '
        resources.ApplyResources(Me.CheckBox1, "CheckBox1")
        Me.CheckBox1.Name = "CheckBox1"
        '
        'ComboBox3
        '
        resources.ApplyResources(Me.ComboBox3, "ComboBox3")
        Me.ComboBox3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox3.DropDownWidth = 139
        Me.ComboBox3.Name = "ComboBox3"
        '
        'Label23
        '
        resources.ApplyResources(Me.Label23, "Label23")
        Me.Label23.Name = "Label23"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.ComboBox1)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'ComboBox1
        '
        resources.ApplyResources(Me.ComboBox1, "ComboBox1")
        Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox1.DropDownWidth = 250
        Me.ComboBox1.Items.AddRange(New Object() {resources.GetString("ComboBox1.Items"), resources.GetString("ComboBox1.Items1"), resources.GetString("ComboBox1.Items2"), resources.GetString("ComboBox1.Items3"), resources.GetString("ComboBox1.Items4"), resources.GetString("ComboBox1.Items5"), resources.GetString("ComboBox1.Items6"), resources.GetString("ComboBox1.Items7"), resources.GetString("ComboBox1.Items8"), resources.GetString("ComboBox1.Items9"), resources.GetString("ComboBox1.Items10"), resources.GetString("ComboBox1.Items11")})
        Me.ComboBox1.Name = "ComboBox1"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.FaTabStrip1)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'FaTabStrip1
        '
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem1, Me.FaTabStripItem2})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem1
        '
        'FaTabStripItem1
        '
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.Controls.Add(Me.GraphControl)
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        Me.FaTabStripItem1.Selected = True
        '
        'GraphControl
        '
        resources.ApplyResources(Me.GraphControl, "GraphControl")
        Me.GraphControl.Cursor = System.Windows.Forms.Cursors.Default
        Me.GraphControl.IsAntiAlias = True
        Me.GraphControl.IsAutoScrollRange = True
        Me.GraphControl.Name = "GraphControl"
        Me.GraphControl.ScrollGrace = 0.0R
        Me.GraphControl.ScrollMaxX = 0.0R
        Me.GraphControl.ScrollMaxY = 0.0R
        Me.GraphControl.ScrollMaxY2 = 0.0R
        Me.GraphControl.ScrollMinX = 0.0R
        Me.GraphControl.ScrollMinY = 0.0R
        Me.GraphControl.ScrollMinY2 = 0.0R
        '
        'FaTabStripItem2
        '
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.Grid1)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        '
        'Grid1
        '
        resources.ApplyResources(Me.Grid1, "Grid1")
        Me.Grid1.AllowUserToAddRows = False
        Me.Grid1.AllowUserToDeleteRows = False
        Me.Grid1.AllowUserToOrderColumns = True
        Me.Grid1.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.Grid1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.Grid1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.DisplayedCells
        Me.Grid1.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        Me.Grid1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.Grid1.Name = "Grid1"
        Me.Grid1.ReadOnly = True
        Me.Grid1.RowHeadersVisible = False
        Me.Grid1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'BackgroundWorker1
        '
        Me.BackgroundWorker1.WorkerReportsProgress = True
        Me.BackgroundWorker1.WorkerSupportsCancellation = True
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
        'FormPhEnv
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.Controls.Add(Me.GroupBox6)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormPhEnv"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        Me.PanelCalc.ResumeLayout(False)
        Me.PanelCalc.PerformLayout()
        CType(Me.TextBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem1.ResumeLayout(False)
        Me.FaTabStripItem2.ResumeLayout(False)
        CType(Me.Grid1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Public WithEvents Button1 As System.Windows.Forms.Button
    Public WithEvents ComboBox3 As System.Windows.Forms.ComboBox
    Public WithEvents Label23 As System.Windows.Forms.Label
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents ComboBox1 As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents Grid1 As System.Windows.Forms.DataGridView
    Public WithEvents GraphControl As ZedGraph.ZedGraphControl
    Public WithEvents BackgroundWorker1 As System.ComponentModel.BackgroundWorker
    Public WithEvents CheckBox1 As System.Windows.Forms.CheckBox
    Public WithEvents CheckBox2 As System.Windows.Forms.CheckBox
    Public WithEvents CheckBox3 As System.Windows.Forms.CheckBox
    Public WithEvents TextBox1 As System.Windows.Forms.NumericUpDown
    Public WithEvents chkpip As System.Windows.Forms.CheckBox
    Public WithEvents chkhyd As System.Windows.Forms.CheckBox
    Public WithEvents ComboBox2 As System.Windows.Forms.ComboBox
    Public WithEvents CheckBoxHYDVAP As System.Windows.Forms.CheckBox
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
    Friend WithEvents PanelCalc As System.Windows.Forms.Panel
    Public WithEvents Button2 As System.Windows.Forms.Button
    Public WithEvents LabelStatus As System.Windows.Forms.Label
End Class
