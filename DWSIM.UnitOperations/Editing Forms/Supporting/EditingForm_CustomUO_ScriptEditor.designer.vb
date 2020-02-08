<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_CustomUO_ScriptEditor
    Inherits SharedClasses.ObjectEditorForm

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_CustomUO_ScriptEditor))
        Me.FaTabStrip2 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem3 = New FarsiLibrary.Win.FATabStripItem()
        Me.txtScript = New ScintillaNET.Scintilla()
        Me.FaTabStripItem4 = New FarsiLibrary.Win.FATabStripItem()
        Me.ListBox1 = New System.Windows.Forms.ListBox()
        Me.imageList1 = New System.Windows.Forms.ImageList(Me.components)
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.OpenToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.SaveToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.PrintToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator = New System.Windows.Forms.ToolStripSeparator()
        Me.CutToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.CopyToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.PasteToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.toolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.tscb1 = New System.Windows.Forms.ToolStripComboBox()
        Me.tscb2 = New System.Windows.Forms.ToolStripComboBox()
        Me.btnHighlightSpaces = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator2 = New System.Windows.Forms.ToolStripSeparator()
        Me.btnUndo = New System.Windows.Forms.ToolStripButton()
        Me.btnRedo = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator6 = New System.Windows.Forms.ToolStripSeparator()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator3 = New System.Windows.Forms.ToolStripSeparator()
        Me.tsbInsertSnippet = New System.Windows.Forms.ToolStripDropDownButton()
        Me.ToolStripSeparator7 = New System.Windows.Forms.ToolStripSeparator()
        Me.btnDebug = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator4 = New System.Windows.Forms.ToolStripSeparator()
        Me.HelpToolStripButton = New System.Windows.Forms.ToolStripButton()
        Me.APIHelptsbutton = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripSeparator5 = New System.Windows.Forms.ToolStripSeparator()
        Me.ofd1 = New System.Windows.Forms.OpenFileDialog()
        Me.sfd1 = New System.Windows.Forms.SaveFileDialog()
        Me.ofd2 = New System.Windows.Forms.OpenFileDialog()
        Me.pd1 = New System.Windows.Forms.PrintDialog()
        Me.treeViewItems = New System.Windows.Forms.TreeView()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        CType(Me.FaTabStrip2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip2.SuspendLayout()
        Me.FaTabStripItem3.SuspendLayout()
        Me.FaTabStripItem4.SuspendLayout()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'FaTabStrip2
        '
        Me.FaTabStrip2.AlwaysShowClose = False
        Me.FaTabStrip2.AlwaysShowMenuGlyph = False
        resources.ApplyResources(Me.FaTabStrip2, "FaTabStrip2")
        Me.FaTabStrip2.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem3, Me.FaTabStripItem4})
        Me.FaTabStrip2.Name = "FaTabStrip2"
        Me.FaTabStrip2.SelectedItem = Me.FaTabStripItem3
        '
        'FaTabStripItem3
        '
        Me.FaTabStripItem3.CanClose = False
        Me.FaTabStripItem3.Controls.Add(Me.txtScript)
        Me.FaTabStripItem3.IsDrawn = True
        Me.FaTabStripItem3.Name = "FaTabStripItem3"
        Me.FaTabStripItem3.Selected = True
        resources.ApplyResources(Me.FaTabStripItem3, "FaTabStripItem3")
        '
        'txtScript
        '
        Me.txtScript.AnnotationVisible = ScintillaNET.Annotation.Standard
        Me.txtScript.AutoCChooseSingle = True
        Me.txtScript.AutoCMaxHeight = 10
        Me.txtScript.AutoCOrder = ScintillaNET.Order.PerformSort
        Me.txtScript.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        resources.ApplyResources(Me.txtScript, "txtScript")
        Me.txtScript.Lexer = ScintillaNET.Lexer.Python
        Me.txtScript.Name = "txtScript"
        Me.txtScript.UseTabs = False
        '
        'FaTabStripItem4
        '
        Me.FaTabStripItem4.CanClose = False
        Me.FaTabStripItem4.Controls.Add(Me.ListBox1)
        Me.FaTabStripItem4.IsDrawn = True
        Me.FaTabStripItem4.Name = "FaTabStripItem4"
        resources.ApplyResources(Me.FaTabStripItem4, "FaTabStripItem4")
        '
        'ListBox1
        '
        Me.ListBox1.BorderStyle = System.Windows.Forms.BorderStyle.None
        resources.ApplyResources(Me.ListBox1, "ListBox1")
        Me.ListBox1.FormattingEnabled = True
        Me.ListBox1.Name = "ListBox1"
        Me.ListBox1.SelectionMode = System.Windows.Forms.SelectionMode.MultiExtended
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
        'ToolStrip1
        '
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.OpenToolStripButton, Me.SaveToolStripButton, Me.PrintToolStripButton, Me.toolStripSeparator, Me.CutToolStripButton, Me.CopyToolStripButton, Me.PasteToolStripButton, Me.toolStripSeparator1, Me.tscb1, Me.tscb2, Me.btnHighlightSpaces, Me.ToolStripSeparator2, Me.btnUndo, Me.btnRedo, Me.ToolStripSeparator6, Me.ToolStripButton2, Me.ToolStripButton3, Me.ToolStripSeparator3, Me.tsbInsertSnippet, Me.ToolStripSeparator7, Me.btnDebug, Me.ToolStripSeparator4, Me.HelpToolStripButton, Me.APIHelptsbutton, Me.ToolStripSeparator5, Me.ToolStripButton1})
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Name = "ToolStrip1"
        '
        'OpenToolStripButton
        '
        Me.OpenToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.OpenToolStripButton, "OpenToolStripButton")
        Me.OpenToolStripButton.Name = "OpenToolStripButton"
        '
        'SaveToolStripButton
        '
        Me.SaveToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.SaveToolStripButton, "SaveToolStripButton")
        Me.SaveToolStripButton.Name = "SaveToolStripButton"
        '
        'PrintToolStripButton
        '
        Me.PrintToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.PrintToolStripButton, "PrintToolStripButton")
        Me.PrintToolStripButton.Name = "PrintToolStripButton"
        '
        'toolStripSeparator
        '
        Me.toolStripSeparator.Name = "toolStripSeparator"
        resources.ApplyResources(Me.toolStripSeparator, "toolStripSeparator")
        '
        'CutToolStripButton
        '
        Me.CutToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.CutToolStripButton, "CutToolStripButton")
        Me.CutToolStripButton.Name = "CutToolStripButton"
        '
        'CopyToolStripButton
        '
        Me.CopyToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.CopyToolStripButton, "CopyToolStripButton")
        Me.CopyToolStripButton.Name = "CopyToolStripButton"
        '
        'PasteToolStripButton
        '
        Me.PasteToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.PasteToolStripButton, "PasteToolStripButton")
        Me.PasteToolStripButton.Name = "PasteToolStripButton"
        '
        'toolStripSeparator1
        '
        Me.toolStripSeparator1.Name = "toolStripSeparator1"
        resources.ApplyResources(Me.toolStripSeparator1, "toolStripSeparator1")
        '
        'tscb1
        '
        Me.tscb1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tscb1.Name = "tscb1"
        resources.ApplyResources(Me.tscb1, "tscb1")
        Me.tscb1.Sorted = True
        '
        'tscb2
        '
        resources.ApplyResources(Me.tscb2, "tscb2")
        Me.tscb2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tscb2.DropDownWidth = 35
        Me.tscb2.Name = "tscb2"
        '
        'btnHighlightSpaces
        '
        Me.btnHighlightSpaces.Checked = True
        Me.btnHighlightSpaces.CheckOnClick = True
        Me.btnHighlightSpaces.CheckState = System.Windows.Forms.CheckState.Checked
        Me.btnHighlightSpaces.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.btnHighlightSpaces, "btnHighlightSpaces")
        Me.btnHighlightSpaces.Name = "btnHighlightSpaces"
        '
        'ToolStripSeparator2
        '
        Me.ToolStripSeparator2.Name = "ToolStripSeparator2"
        resources.ApplyResources(Me.ToolStripSeparator2, "ToolStripSeparator2")
        '
        'btnUndo
        '
        Me.btnUndo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnUndo.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.turn_left
        resources.ApplyResources(Me.btnUndo, "btnUndo")
        Me.btnUndo.Name = "btnUndo"
        '
        'btnRedo
        '
        Me.btnRedo.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnRedo.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.turn_right
        resources.ApplyResources(Me.btnRedo, "btnRedo")
        Me.btnRedo.Name = "btnRedo"
        '
        'ToolStripSeparator6
        '
        Me.ToolStripSeparator6.Name = "ToolStripSeparator6"
        resources.ApplyResources(Me.ToolStripSeparator6, "ToolStripSeparator6")
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripSeparator3
        '
        Me.ToolStripSeparator3.Name = "ToolStripSeparator3"
        resources.ApplyResources(Me.ToolStripSeparator3, "ToolStripSeparator3")
        '
        'tsbInsertSnippet
        '
        resources.ApplyResources(Me.tsbInsertSnippet, "tsbInsertSnippet")
        Me.tsbInsertSnippet.Name = "tsbInsertSnippet"
        '
        'ToolStripSeparator7
        '
        Me.ToolStripSeparator7.Name = "ToolStripSeparator7"
        resources.ApplyResources(Me.ToolStripSeparator7, "ToolStripSeparator7")
        '
        'btnDebug
        '
        Me.btnDebug.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.btnDebug.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.control_play
        resources.ApplyResources(Me.btnDebug, "btnDebug")
        Me.btnDebug.Name = "btnDebug"
        '
        'ToolStripSeparator4
        '
        Me.ToolStripSeparator4.Name = "ToolStripSeparator4"
        resources.ApplyResources(Me.ToolStripSeparator4, "ToolStripSeparator4")
        '
        'HelpToolStripButton
        '
        Me.HelpToolStripButton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        resources.ApplyResources(Me.HelpToolStripButton, "HelpToolStripButton")
        Me.HelpToolStripButton.Name = "HelpToolStripButton"
        '
        'APIHelptsbutton
        '
        Me.APIHelptsbutton.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.APIHelptsbutton.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.script_code
        resources.ApplyResources(Me.APIHelptsbutton, "APIHelptsbutton")
        Me.APIHelptsbutton.Name = "APIHelptsbutton"
        '
        'ToolStripSeparator5
        '
        Me.ToolStripSeparator5.Name = "ToolStripSeparator5"
        resources.ApplyResources(Me.ToolStripSeparator5, "ToolStripSeparator5")
        '
        'ofd1
        '
        resources.ApplyResources(Me.ofd1, "ofd1")
        Me.ofd1.Multiselect = True
        Me.ofd1.SupportMultiDottedExtensions = True
        '
        'sfd1
        '
        resources.ApplyResources(Me.sfd1, "sfd1")
        Me.sfd1.SupportMultiDottedExtensions = True
        '
        'ofd2
        '
        resources.ApplyResources(Me.ofd2, "ofd2")
        Me.ofd2.Multiselect = True
        Me.ofd2.SupportMultiDottedExtensions = True
        '
        'pd1
        '
        Me.pd1.UseEXDialog = True
        '
        'treeViewItems
        '
        Me.treeViewItems.LineColor = System.Drawing.Color.Empty
        resources.ApplyResources(Me.treeViewItems, "treeViewItems")
        Me.treeViewItems.Name = "treeViewItems"
        Me.treeViewItems.PathSeparator = "."
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'EditingForm_CustomUO_ScriptEditor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.FaTabStrip2)
        Me.Controls.Add(Me.ToolStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_CustomUO_ScriptEditor"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        CType(Me.FaTabStrip2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip2.ResumeLayout(False)
        Me.FaTabStripItem3.ResumeLayout(False)
        Me.FaTabStripItem4.ResumeLayout(False)
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents FaTabStrip2 As FarsiLibrary.Win.FATabStrip
    Public WithEvents FaTabStripItem3 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents FaTabStripItem4 As FarsiLibrary.Win.FATabStripItem
    Public WithEvents ListBox1 As System.Windows.Forms.ListBox
    Public WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Public WithEvents OpenToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents SaveToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents toolStripSeparator As System.Windows.Forms.ToolStripSeparator
    Public WithEvents CutToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents CopyToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents PasteToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents toolStripSeparator1 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents tscb1 As System.Windows.Forms.ToolStripComboBox
    Public WithEvents tscb2 As System.Windows.Forms.ToolStripComboBox
    Public WithEvents ToolStripSeparator2 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ofd1 As System.Windows.Forms.OpenFileDialog
    Public WithEvents sfd1 As System.Windows.Forms.SaveFileDialog
    Public WithEvents ofd2 As System.Windows.Forms.OpenFileDialog
    Public WithEvents pd1 As System.Windows.Forms.PrintDialog
    Public WithEvents treeViewItems As System.Windows.Forms.TreeView
    Public WithEvents imageList1 As System.Windows.Forms.ImageList
    Public WithEvents ToolStripSeparator3 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents APIHelptsbutton As System.Windows.Forms.ToolStripButton
    Public WithEvents HelpToolStripButton As System.Windows.Forms.ToolStripButton
    Public WithEvents btnDebug As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator4 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents txtScript As ScintillaNET.Scintilla
    Public WithEvents btnHighlightSpaces As System.Windows.Forms.ToolStripButton
    Public WithEvents btnUndo As System.Windows.Forms.ToolStripButton
    Public WithEvents btnRedo As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripSeparator6 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents ToolStripSeparator5 As System.Windows.Forms.ToolStripSeparator
    Public WithEvents PrintToolStripButton As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbInsertSnippet As ToolStripDropDownButton
    Public WithEvents ToolStripSeparator7 As ToolStripSeparator
    Friend WithEvents ToolStripButton1 As ToolStripButton
End Class
