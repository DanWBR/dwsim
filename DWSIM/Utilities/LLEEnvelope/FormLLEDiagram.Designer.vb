<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormLLEDiagram
    Inherits System.Windows.Forms.UserControl

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
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

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormLLEDiagram))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle4 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.PrintDocument1 = New System.Drawing.Printing.PrintDocument()
        Me.PrintDialog1 = New System.Windows.Forms.PrintDialog()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.PanelDiag = New System.Windows.Forms.Panel()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.DataGridView1 = New System.Windows.Forms.DataGridView()
        Me.X11 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.X12 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.X21 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.X22 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.PanelMenu = New System.Windows.Forms.Panel()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.lblP = New System.Windows.Forms.Label()
        Me.tbP = New System.Windows.Forms.TextBox()
        Me.cbPropPack = New System.Windows.Forms.ComboBox()
        Me.btnCalcDiagram = New System.Windows.Forms.Button()
        Me.lblT = New System.Windows.Forms.Label()
        Me.tbT = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbComp3 = New System.Windows.Forms.ComboBox()
        Me.cbComp2 = New System.Windows.Forms.ComboBox()
        Me.cbComp1 = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.TSB_Print = New System.Windows.Forms.ToolStripButton()
        Me.TSB_PrinterSetup = New System.Windows.Forms.ToolStripButton()
        Me.TSB_CalcDiagr = New System.Windows.Forms.ToolStripButton()
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
        Me.Panel1.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPage2.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.PanelMenu.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'PrintDocument1
        '
        '
        'PrintDialog1
        '
        Me.PrintDialog1.Document = Me.PrintDocument1
        Me.PrintDialog1.UseEXDialog = True
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Panel1.Controls.Add(Me.TabControl1)
        Me.Panel1.Controls.Add(Me.PanelMenu)
        Me.Panel1.Name = "Panel1"
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        Me.TabPage1.Controls.Add(Me.PanelDiag)
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'PanelDiag
        '
        Me.PanelDiag.BackColor = System.Drawing.Color.WhiteSmoke
        resources.ApplyResources(Me.PanelDiag, "PanelDiag")
        Me.PanelDiag.Name = "PanelDiag"
        '
        'TabPage2
        '
        Me.TabPage2.Controls.Add(Me.DataGridView1)
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'DataGridView1
        '
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        Me.DataGridView1.AllowUserToResizeColumns = False
        Me.DataGridView1.AllowUserToResizeRows = False
        Me.DataGridView1.ClipboardCopyMode = System.Windows.Forms.DataGridViewClipboardCopyMode.EnableAlwaysIncludeHeaderText
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.X11, Me.X12, Me.X21, Me.X22})
        resources.ApplyResources(Me.DataGridView1, "DataGridView1")
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.ReadOnly = True
        '
        'X11
        '
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        DataGridViewCellStyle1.Format = "N6"
        DataGridViewCellStyle1.NullValue = Nothing
        Me.X11.DefaultCellStyle = DataGridViewCellStyle1
        Me.X11.Frozen = True
        resources.ApplyResources(Me.X11, "X11")
        Me.X11.Name = "X11"
        Me.X11.ReadOnly = True
        Me.X11.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.X11.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'X12
        '
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(192, Byte), Integer))
        DataGridViewCellStyle2.Format = "N6"
        DataGridViewCellStyle2.NullValue = Nothing
        Me.X12.DefaultCellStyle = DataGridViewCellStyle2
        Me.X12.DividerWidth = 5
        Me.X12.Frozen = True
        resources.ApplyResources(Me.X12, "X12")
        Me.X12.Name = "X12"
        Me.X12.ReadOnly = True
        Me.X12.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.X12.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'X21
        '
        DataGridViewCellStyle3.BackColor = System.Drawing.Color.PaleTurquoise
        DataGridViewCellStyle3.Format = "N6"
        DataGridViewCellStyle3.NullValue = Nothing
        Me.X21.DefaultCellStyle = DataGridViewCellStyle3
        resources.ApplyResources(Me.X21, "X21")
        Me.X21.Name = "X21"
        Me.X21.ReadOnly = True
        Me.X21.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.X21.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'X22
        '
        DataGridViewCellStyle4.BackColor = System.Drawing.Color.PaleTurquoise
        DataGridViewCellStyle4.Format = "N6"
        DataGridViewCellStyle4.NullValue = Nothing
        Me.X22.DefaultCellStyle = DataGridViewCellStyle4
        resources.ApplyResources(Me.X22, "X22")
        Me.X22.Name = "X22"
        Me.X22.ReadOnly = True
        Me.X22.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        Me.X22.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        '
        'PanelMenu
        '
        resources.ApplyResources(Me.PanelMenu, "PanelMenu")
        Me.PanelMenu.BackColor = System.Drawing.SystemColors.Control
        Me.PanelMenu.Controls.Add(Me.Label8)
        Me.PanelMenu.Controls.Add(Me.Label7)
        Me.PanelMenu.Controls.Add(Me.Label6)
        Me.PanelMenu.Controls.Add(Me.lblP)
        Me.PanelMenu.Controls.Add(Me.tbP)
        Me.PanelMenu.Controls.Add(Me.cbPropPack)
        Me.PanelMenu.Controls.Add(Me.btnCalcDiagram)
        Me.PanelMenu.Controls.Add(Me.lblT)
        Me.PanelMenu.Controls.Add(Me.tbT)
        Me.PanelMenu.Controls.Add(Me.Label5)
        Me.PanelMenu.Controls.Add(Me.Label4)
        Me.PanelMenu.Controls.Add(Me.Label3)
        Me.PanelMenu.Controls.Add(Me.Label2)
        Me.PanelMenu.Controls.Add(Me.cbComp3)
        Me.PanelMenu.Controls.Add(Me.cbComp2)
        Me.PanelMenu.Controls.Add(Me.cbComp1)
        Me.PanelMenu.Controls.Add(Me.Label1)
        Me.PanelMenu.Name = "PanelMenu"
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'lblP
        '
        resources.ApplyResources(Me.lblP, "lblP")
        Me.lblP.Name = "lblP"
        '
        'tbP
        '
        resources.ApplyResources(Me.tbP, "tbP")
        Me.tbP.Name = "tbP"
        '
        'cbPropPack
        '
        Me.cbPropPack.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPropPack.FormattingEnabled = True
        resources.ApplyResources(Me.cbPropPack, "cbPropPack")
        Me.cbPropPack.Name = "cbPropPack"
        '
        'btnCalcDiagram
        '
        Me.btnCalcDiagram.Image = Global.DWSIM.My.Resources.Resources.chart_LLE
        resources.ApplyResources(Me.btnCalcDiagram, "btnCalcDiagram")
        Me.btnCalcDiagram.Name = "btnCalcDiagram"
        Me.btnCalcDiagram.UseVisualStyleBackColor = True
        '
        'lblT
        '
        resources.ApplyResources(Me.lblT, "lblT")
        Me.lblT.Name = "lblT"
        '
        'tbT
        '
        resources.ApplyResources(Me.tbT, "tbT")
        Me.tbT.Name = "tbT"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbComp3
        '
        Me.cbComp3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbComp3.FormattingEnabled = True
        resources.ApplyResources(Me.cbComp3, "cbComp3")
        Me.cbComp3.Name = "cbComp3"
        '
        'cbComp2
        '
        Me.cbComp2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbComp2.FormattingEnabled = True
        resources.ApplyResources(Me.cbComp2, "cbComp2")
        Me.cbComp2.Name = "cbComp2"
        '
        'cbComp1
        '
        Me.cbComp1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbComp1.FormattingEnabled = True
        resources.ApplyResources(Me.cbComp1, "cbComp1")
        Me.cbComp1.Name = "cbComp1"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.TSB_Print, Me.TSB_PrinterSetup, Me.TSB_CalcDiagr})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        '
        'TSB_Print
        '
        Me.TSB_Print.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSB_Print.Image = Global.DWSIM.My.Resources.Resources.printer
        resources.ApplyResources(Me.TSB_Print, "TSB_Print")
        Me.TSB_Print.Name = "TSB_Print"
        '
        'TSB_PrinterSetup
        '
        Me.TSB_PrinterSetup.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSB_PrinterSetup.Image = Global.DWSIM.My.Resources.Resources.printer_add
        resources.ApplyResources(Me.TSB_PrinterSetup, "TSB_PrinterSetup")
        Me.TSB_PrinterSetup.Name = "TSB_PrinterSetup"
        '
        'TSB_CalcDiagr
        '
        Me.TSB_CalcDiagr.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.TSB_CalcDiagr.Image = Global.DWSIM.My.Resources.Resources.chart_LLE
        resources.ApplyResources(Me.TSB_CalcDiagr, "TSB_CalcDiagr")
        Me.TSB_CalcDiagr.Name = "TSB_CalcDiagr"
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
        'FormLLEDiagram
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.ToolStrip1)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "FormLLEDiagram"
        Me.Panel1.ResumeLayout(False)
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage2.ResumeLayout(False)
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.PanelMenu.ResumeLayout(False)
        Me.PanelMenu.PerformLayout()
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents PrintDocument1 As System.Drawing.Printing.PrintDocument
    Friend WithEvents PrintDialog1 As System.Windows.Forms.PrintDialog
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents PanelMenu As System.Windows.Forms.Panel
    Friend WithEvents Label8 As System.Windows.Forms.Label
    Friend WithEvents Label7 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
    Friend WithEvents lblP As System.Windows.Forms.Label
    Friend WithEvents tbP As System.Windows.Forms.TextBox
    Friend WithEvents cbPropPack As System.Windows.Forms.ComboBox
    Friend WithEvents btnCalcDiagram As System.Windows.Forms.Button
    Friend WithEvents lblT As System.Windows.Forms.Label
    Friend WithEvents tbT As System.Windows.Forms.TextBox
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents cbComp3 As System.Windows.Forms.ComboBox
    Friend WithEvents cbComp2 As System.Windows.Forms.ComboBox
    Friend WithEvents cbComp1 As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents TSB_Print As System.Windows.Forms.ToolStripButton
    Friend WithEvents TSB_PrinterSetup As System.Windows.Forms.ToolStripButton
    Friend WithEvents TSB_CalcDiagr As System.Windows.Forms.ToolStripButton
    Friend WithEvents TabControl1 As System.Windows.Forms.TabControl
    Friend WithEvents TabPage1 As System.Windows.Forms.TabPage
    Friend WithEvents PanelDiag As System.Windows.Forms.Panel
    Friend WithEvents TabPage2 As System.Windows.Forms.TabPage
    Friend WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Friend WithEvents X11 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents X12 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents X21 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents X22 As System.Windows.Forms.DataGridViewTextBoxColumn
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
