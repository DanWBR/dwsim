<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> Partial Class FormScript
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormScript))
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.NewToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.PrintToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.CutToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.CopyToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.PasteToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.btnUndo = New System.Windows.Forms.ToolStripButton()
        Me.btnRedo = New System.Windows.Forms.ToolStripButton()
        Me.btnComment = New System.Windows.Forms.ToolStripButton()
        Me.btnUncomment = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.btnIdent = New System.Windows.Forms.ToolStripButton()
        Me.btnIdentRemove = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator6 = New System.Windows.Forms.ToolStripSeparator()
        Me.tscb1 = New System.Windows.Forms.ToolStripComboBox()
        Me.tscb2 = New System.Windows.Forms.ToolStripComboBox()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbInsertSnippet = New System.Windows.Forms.ToolStripDropDownButton()
        Me.GetPropertyTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.SetPropertyTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator7 = New System.Windows.Forms.ToolStripSeparator()
        Me.btnRun = New System.Windows.Forms.ToolStripButton()
        Me.btnRunAsync = New System.Windows.Forms.ToolStripButton()
        Me.btnRunDebug = New System.Windows.Forms.ToolStripButton()
        Me.btnStopDebug = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator()
        Me.APIHelptsbutton = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.TabStripScripts = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem3 = New FarsiLibrary.Win.FATabStripItem()
        Me.imageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.FaTabStripItem5 = New FarsiLibrary.Win.FATabStripItem()
        Me.treeViewItems = New System.Windows.Forms.TreeView()
        Me.pd1 = New System.Windows.Forms.PrintDialog()
        Me.ListBox1 = New System.Windows.Forms.ListBox()
        Me.FaTabStripItem4 = New FarsiLibrary.Win.FATabStripItem()
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
        Me.ToolStrip1.SuspendLayout()
        CType(Me.TabStripScripts, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStripItem5.SuspendLayout()
        Me.FaTabStripItem4.SuspendLayout()
        Me.dckMenu.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.NewToolStripButton, Me.ToolStripButton2, Me.ToolStripButton1, Me.PrintToolStripButton, Me.toolStripSeparator, Me.CutToolStripButton, Me.CopyToolStripButton, Me.PasteToolStripButton, Me.toolStripSeparator1, Me.btnUndo, Me.btnRedo, Me.btnComment, Me.btnUncomment, Me.ToolStripSeparator4, Me.btnIdent, Me.btnIdentRemove, Me.ToolStripSeparator6, Me.tscb1, Me.tscb2, Me.ToolStripSeparator2, Me.tsbInsertSnippet, Me.ToolStripSeparator7, Me.btnRun, Me.btnRunAsync, Me.btnRunDebug, Me.btnStopDebug, Me.toolStripSeparator5, Me.APIHelptsbutton, Me.ToolStripSeparator3})
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'NewToolStripButton
        '
        resources.ApplyResources(Me.NewToolStripButton, "NewToolStripButton")
        Me.NewToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.NewToolStripButton.Name = "NewToolStripButton"
        '
        'ToolStripButton2
        '
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.arrow_down
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'PrintToolStripButton
        '
        resources.ApplyResources(Me.PrintToolStripButton, "PrintToolStripButton")
        Me.PrintToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.PrintToolStripButton.Name = "PrintToolStripButton"
        '
        'toolStripSeparator
        '
        resources.ApplyResources(Me.toolStripSeparator, "toolStripSeparator")
        Me.toolStripSeparator.Name = "toolStripSeparator"
        '
        'CutToolStripButton
        '
        resources.ApplyResources(Me.CutToolStripButton, "CutToolStripButton")
        Me.CutToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.CutToolStripButton.Name = "CutToolStripButton"
        '
        'CopyToolStripButton
        '
        resources.ApplyResources(Me.CopyToolStripButton, "CopyToolStripButton")
        Me.CopyToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.CopyToolStripButton.Name = "CopyToolStripButton"
        '
        'PasteToolStripButton
        '
        resources.ApplyResources(Me.PasteToolStripButton, "PasteToolStripButton")
        Me.PasteToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.PasteToolStripButton.Name = "PasteToolStripButton"
        '
        'toolStripSeparator1
        '
        resources.ApplyResources(Me.toolStripSeparator1, "toolStripSeparator1")
        Me.toolStripSeparator1.Name = "toolStripSeparator1"
        '
        'btnUndo
        '
        resources.ApplyResources(Me.btnUndo, "btnUndo")
        Me.btnUndo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnUndo.Image = Global.DWSIM.My.Resources.Resources.turn_left
        Me.btnUndo.Name = "btnUndo"
        '
        'btnRedo
        '
        resources.ApplyResources(Me.btnRedo, "btnRedo")
        Me.btnRedo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRedo.Image = Global.DWSIM.My.Resources.Resources.turn_right
        Me.btnRedo.Name = "btnRedo"
        '
        'btnComment
        '
        resources.ApplyResources(Me.btnComment, "btnComment")
        Me.btnComment.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnComment.Image = Global.DWSIM.My.Resources.Resources.text_padding_left
        Me.btnComment.Name = "btnComment"
        '
        'btnUncomment
        '
        resources.ApplyResources(Me.btnUncomment, "btnUncomment")
        Me.btnUncomment.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnUncomment.Image = Global.DWSIM.My.Resources.Resources.text_padding_left1
        Me.btnUncomment.Name = "btnUncomment"
        '
        'ToolStripSeparator4
        '
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        '
        'btnIdent
        '
        resources.ApplyResources(Me.btnIdent, "btnIdent")
        Me.btnIdent.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnIdent.Image = Global.DWSIM.My.Resources.Resources.text_indent
        Me.btnIdent.Name = "btnIdent"
        '
        'btnIdentRemove
        '
        resources.ApplyResources(Me.btnIdentRemove, "btnIdentRemove")
        Me.btnIdentRemove.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnIdentRemove.Image = Global.DWSIM.My.Resources.Resources.text_indent_remove
        Me.btnIdentRemove.Name = "btnIdentRemove"
        '
        'ToolStripSeparator6
        '
        resources.ApplyResources(Me.ToolStripSeparator6, "ToolStripSeparator6")
        Me.ToolStripSeparator6.Name = "ToolStripSeparator6"
        '
        'tscb1
        '
        resources.ApplyResources(Me.tscb1, "tscb1")
        Me.tscb1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tscb1.Name = "tscb1"
        Me.tscb1.Sorted = True
        '
        'tscb2
        '
        resources.ApplyResources(Me.tscb2, "tscb2")
        Me.tscb2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tscb2.DropDownWidth = 35
        Me.tscb2.Name = "tscb2"
        '
        'ToolStripSeparator2
        '
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        '
        'tsbInsertSnippet
        '
        resources.ApplyResources(Me.tsbInsertSnippet, "tsbInsertSnippet")
        Me.tsbInsertSnippet.DropDownItems.AddRange(New System.Windows.Forms.ToolStripItem() {Me.GetPropertyTSMI, Me.SetPropertyTSMI})
        Me.tsbInsertSnippet.Image = Global.DWSIM.My.Resources.Resources.script_code
        Me.tsbInsertSnippet.Name = "tsbInsertSnippet"
        '
        'GetPropertyTSMI
        '
        resources.ApplyResources(Me.GetPropertyTSMI, "GetPropertyTSMI")
        Me.GetPropertyTSMI.Name = "GetPropertyTSMI"
        '
        'SetPropertyTSMI
        '
        resources.ApplyResources(Me.SetPropertyTSMI, "SetPropertyTSMI")
        Me.SetPropertyTSMI.Name = "SetPropertyTSMI"
        '
        'ToolStripSeparator7
        '
        resources.ApplyResources(Me.ToolStripSeparator7, "ToolStripSeparator7")
        Me.ToolStripSeparator7.Name = "ToolStripSeparator7"
        '
        'btnRun
        '
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRun.Image = Global.DWSIM.My.Resources.Resources.control_play
        Me.btnRun.Name = "btnRun"
        '
        'btnRunAsync
        '
        resources.ApplyResources(Me.btnRunAsync, "btnRunAsync")
        Me.btnRunAsync.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRunAsync.Image = Global.DWSIM.My.Resources.Resources.control_play_blue
        Me.btnRunAsync.Name = "btnRunAsync"
        '
        'btnRunDebug
        '
        resources.ApplyResources(Me.btnRunDebug, "btnRunDebug")
        Me.btnRunDebug.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRunDebug.Image = Global.DWSIM.My.Resources.Resources.play_blue
        Me.btnRunDebug.Name = "btnRunDebug"
        '
        'btnStopDebug
        '
        resources.ApplyResources(Me.btnStopDebug, "btnStopDebug")
        Me.btnStopDebug.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnStopDebug.Image = Global.DWSIM.My.Resources.Resources._stop
        Me.btnStopDebug.Name = "btnStopDebug"
        '
        'toolStripSeparator5
        '
        resources.ApplyResources(Me.toolStripSeparator5, "toolStripSeparator5")
        Me.toolStripSeparator5.Name = "toolStripSeparator5"
        '
        'APIHelptsbutton
        '
        resources.ApplyResources(Me.APIHelptsbutton, "APIHelptsbutton")
        Me.APIHelptsbutton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.APIHelptsbutton.Image = Global.DWSIM.My.Resources.Resources.help_browser1
        Me.APIHelptsbutton.Name = "APIHelptsbutton"
        '
        'ToolStripSeparator3
        '
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        '
        'TabStripScripts
        '
        resources.ApplyResources(Me.TabStripScripts, "TabStripScripts")
        Me.TabStripScripts.Name = "TabStripScripts"
        Me.TabStripScripts.SelectedItem = Me.FaTabStripItem3
        '
        'FaTabStripItem3
        '
        resources.ApplyResources(Me.FaTabStripItem3, "FaTabStripItem3")
        Me.FaTabStripItem3.CanClose = False
        Me.FaTabStripItem3.IsDrawn = True
        Me.FaTabStripItem3.Name = "FaTabStripItem3"
        Me.FaTabStripItem3.Selected = True
        '
        'imageList1
        '
        Me.imageList1.ImageStream = CType(resources.GetObject("imageList1.ImageStream"), System.Windows.Forms.ImageListStreamer)
        Me.imageList1.TransparentColor = System.Drawing.Color.Lime
        Me.imageList1.Images.SetKeyName(0, "")
        Me.imageList1.Images.SetKeyName(1, "")
        Me.imageList1.Images.SetKeyName(2, "")
        Me.imageList1.Images.SetKeyName(3, "")
        Me.imageList1.Images.SetKeyName(4, "")
        '
        'FaTabStripItem5
        '
        resources.ApplyResources(Me.FaTabStripItem5, "FaTabStripItem5")
        Me.FaTabStripItem5.Controls.Add(Me.treeViewItems)
        Me.FaTabStripItem5.IsDrawn = True
        Me.FaTabStripItem5.Name = "FaTabStripItem5"
        Me.FaTabStripItem5.Selected = True
        '
        'treeViewItems
        '
        resources.ApplyResources(Me.treeViewItems, "treeViewItems")
        Me.treeViewItems.LineColor = System.Drawing.Color.Empty
        Me.treeViewItems.Name = "treeViewItems"
        Me.treeViewItems.PathSeparator = "."
        '
        'pd1
        '
        Me.pd1.UseEXDialog = True
        '
        'ListBox1
        '
        resources.ApplyResources(Me.ListBox1, "ListBox1")
        Me.ListBox1.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.ListBox1.FormattingEnabled = True
        Me.ListBox1.Name = "ListBox1"
        Me.ListBox1.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
        '
        'FaTabStripItem4
        '
        resources.ApplyResources(Me.FaTabStripItem4, "FaTabStripItem4")
        Me.FaTabStripItem4.CanClose = False
        Me.FaTabStripItem4.Controls.Add(Me.ListBox1)
        Me.FaTabStripItem4.IsDrawn = True
        Me.FaTabStripItem4.Name = "FaTabStripItem4"
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
        'FormScript
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.CloseButton = False
        Me.CloseButtonVisible = False
        Me.Controls.Add(Me.TabStripScripts)
        Me.Controls.Add(Me.ToolStrip1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormScript"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.TabPageContextMenuStrip = Me.dckMenu
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        CType(Me.TabStripScripts, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStripItem5.ResumeLayout(False)
        Me.FaTabStripItem4.ResumeLayout(False)
        Me.dckMenu.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents PrintToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents toolStripSeparator As System.Windows.Forms.ToolStripSeparator
    Public WithEvents CutToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents CopyToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents PasteToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents toolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents btnRun As System.Windows.Forms.ToolStripButton
    Public WithEvents TabStripScripts As FarsiLibrary.Win.FATabStrip
    Public WithEvents pd1 As System.Windows.Forms.PrintDialog
    Public WithEvents tscb1 As System.Windows.Forms.ToolStripComboBox
    Public WithEvents tscb2 As System.Windows.Forms.ToolStripComboBox
    Public WithEvents imageList1 As System.Windows.Forms.ImageList
    Public WithEvents FaTabStripItem5 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents treeViewItems As System.Windows.Forms.TreeView
    Public WithEvents FaTabStripItem3 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents ListBox1 As System.Windows.Forms.ListBox
    Public WithEvents FaTabStripItem4 As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents NewToolStripButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents toolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
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
    Friend WithEvents APIHelptsbutton As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnUndo As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnRedo As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator6 As System.Windows.Forms.ToolStripSeparator
    Friend WithEvents btnComment As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnUncomment As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnIdent As System.Windows.Forms.ToolStripButton
    Friend WithEvents btnIdentRemove As System.Windows.Forms.ToolStripButton
    Friend WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents btnRunAsync As System.Windows.Forms.ToolStripButton
    Public WithEvents btnRunDebug As ToolStripButton
    Public WithEvents btnStopDebug As ToolStripButton
    Friend WithEvents tsbInsertSnippet As ToolStripDropDownButton
    Friend WithEvents ToolStripSeparator7 As ToolStripSeparator
    Friend WithEvents GetPropertyTSMI As ToolStripMenuItem
    Friend WithEvents SetPropertyTSMI As ToolStripMenuItem
    Friend WithEvents ToolStripButton1 As ToolStripButton
    Friend WithEvents ToolStripButton2 As ToolStripButton
End Class
