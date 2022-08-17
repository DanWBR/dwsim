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
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.pd1 = New System.Windows.Forms.PrintDialog()
        Me.treeViewItems = New System.Windows.Forms.TreeView()
        Me.ToolStrip2 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.ToolStripComboBox1 = New System.Windows.Forms.ToolStripComboBox()
        Me.ToolStripLabel2 = New System.Windows.Forms.ToolStripLabel()
        Me.ToolStripTextBox1 = New System.Windows.Forms.ToolStripTextBox()
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton()
        Me.FolderBrowserDialog1 = New System.Windows.Forms.FolderBrowserDialog()
        Me.txtScript = New ScintillaNET.Scintilla()
        Me.ToolStrip1.SuspendLayout()
        Me.ToolStrip2.SuspendLayout()
        Me.SuspendLayout()
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
        'ToolStripButton1
        '
        Me.ToolStripButton1.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
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
        'ToolStrip2
        '
        Me.ToolStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripLabel1, Me.ToolStripComboBox1, Me.ToolStripLabel2, Me.ToolStripTextBox1, Me.ToolStripButton4})
        resources.ApplyResources(Me.ToolStrip2, "ToolStrip2")
        Me.ToolStrip2.Name = "ToolStrip2"
        '
        'ToolStripLabel1
        '
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        '
        'ToolStripComboBox1
        '
        Me.ToolStripComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ToolStripComboBox1.Items.AddRange(New Object() {resources.GetString("ToolStripComboBox1.Items"), resources.GetString("ToolStripComboBox1.Items1")})
        Me.ToolStripComboBox1.Name = "ToolStripComboBox1"
        resources.ApplyResources(Me.ToolStripComboBox1, "ToolStripComboBox1")
        '
        'ToolStripLabel2
        '
        Me.ToolStripLabel2.Name = "ToolStripLabel2"
        resources.ApplyResources(Me.ToolStripLabel2, "ToolStripLabel2")
        '
        'ToolStripTextBox1
        '
        resources.ApplyResources(Me.ToolStripTextBox1, "ToolStripTextBox1")
        Me.ToolStripTextBox1.Name = "ToolStripTextBox1"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Text
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'txtScript
        '
        resources.ApplyResources(Me.txtScript, "txtScript")
        Me.txtScript.Dock = DockStyle.Fill
        Me.txtScript.AnnotationVisible = ScintillaNET.Annotation.Standard
        Me.txtScript.AutoCChooseSingle = True
        Me.txtScript.AutoCMaxHeight = 10
        Me.txtScript.AutoCOrder = ScintillaNET.Order.PerformSort
        Me.txtScript.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
        Me.txtScript.Lexer = ScintillaNET.Lexer.Python
        Me.txtScript.Name = "txtScript"
        Me.ToolTipValues.SetToolTip(Me.txtScript, resources.GetString("txtScript.ToolTip"))
        Me.txtScript.UseTabs = False
        '
        'FolderBrowserDialog1
        '
        resources.ApplyResources(Me.FolderBrowserDialog1, "FolderBrowserDialog1")
        '
        'EditingForm_CustomUO_ScriptEditor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.txtScript)
        Me.Controls.Add(Me.ToolStrip2)
        Me.Controls.Add(Me.ToolStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_CustomUO_ScriptEditor"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ToolStrip2.ResumeLayout(False)
        Me.ToolStrip2.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
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
    Friend WithEvents ToolStrip2 As ToolStrip
    Friend WithEvents ToolStripLabel1 As ToolStripLabel
    Friend WithEvents ToolStripComboBox1 As ToolStripComboBox
    Friend WithEvents ToolStripLabel2 As ToolStripLabel
    Friend WithEvents ToolStripTextBox1 As ToolStripTextBox
    Friend WithEvents ToolStripButton4 As ToolStripButton
    Public WithEvents FolderBrowserDialog1 As FolderBrowserDialog
End Class
