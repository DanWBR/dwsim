<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class ScriptEditorControl
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ScriptEditorControl))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.txtScript = New ScintillaNET.Scintilla()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tvVariables = New Aga.Controls.Tree.TreeViewAdv()
        Me.tc1 = New Aga.Controls.Tree.TreeColumn()
        Me.tc2 = New Aga.Controls.Tree.TreeColumn()
        Me.tc3 = New Aga.Controls.Tree.TreeColumn()
        Me.ContextMenuStrip1 = New System.Windows.Forms.ContextMenuStrip(Me.components)
        Me.ItemToolStripMenuItem = New System.Windows.Forms.ToolStripMenuItem()
        Me.ToolStripSeparator1 = New System.Windows.Forms.ToolStripSeparator()
        Me.ViewAPIHelpTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.ViewSourceCodeTSMI = New System.Windows.Forms.ToolStripMenuItem()
        Me.NodeIcon1 = New Aga.Controls.Tree.NodeControls.NodeIcon()
        Me.NodeTextBox1 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.NodeTextBox2 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.NodeTextBox3 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.cbPythonEngine = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbLinkedEvent = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbLinkedObject = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.chkLink = New System.Windows.Forms.CheckBox()
        Me.treeViewItems = New System.Windows.Forms.TreeView()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.Panel2.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.ContextMenuStrip1.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'SplitContainer1
        '
        resources.ApplyResources(Me.SplitContainer1, "SplitContainer1")
        Me.SplitContainer1.Name = "SplitContainer1"
        '
        'SplitContainer1.Panel1
        '
        Me.SplitContainer1.Panel1.Controls.Add(Me.txtScript)
        '
        'SplitContainer1.Panel2
        '
        Me.SplitContainer1.Panel2.Controls.Add(Me.TableLayoutPanel1)
        Me.SplitContainer1.Panel2Collapsed = True
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
        '
        'TableLayoutPanel1
        '
        resources.ApplyResources(Me.TableLayoutPanel1, "TableLayoutPanel1")
        Me.TableLayoutPanel1.Controls.Add(Me.Label4, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.tvVariables, 0, 1)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        '
        'Label4
        '
        Me.Label4.BackColor = System.Drawing.Color.SteelBlue
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.ForeColor = System.Drawing.Color.White
        Me.Label4.Name = "Label4"
        '
        'tvVariables
        '
        Me.tvVariables.AsyncExpanding = True
        Me.tvVariables.AutoRowHeight = True
        Me.tvVariables.BackColor = System.Drawing.SystemColors.Window
        Me.tvVariables.Columns.Add(Me.tc1)
        Me.tvVariables.Columns.Add(Me.tc2)
        Me.tvVariables.Columns.Add(Me.tc3)
        Me.tvVariables.ContextMenuStrip = Me.ContextMenuStrip1
        Me.tvVariables.DefaultToolTipProvider = Nothing
        Me.tvVariables.DragDropMarkColor = System.Drawing.Color.Black
        Me.tvVariables.FullRowSelect = True
        Me.tvVariables.GridLineStyle = CType((Aga.Controls.Tree.GridLineStyle.Horizontal Or Aga.Controls.Tree.GridLineStyle.Vertical), Aga.Controls.Tree.GridLineStyle)
        Me.tvVariables.LineColor = System.Drawing.SystemColors.ControlDark
        Me.tvVariables.LoadOnDemand = True
        resources.ApplyResources(Me.tvVariables, "tvVariables")
        Me.tvVariables.Model = Nothing
        Me.tvVariables.Name = "tvVariables"
        Me.tvVariables.NodeControls.Add(Me.NodeIcon1)
        Me.tvVariables.NodeControls.Add(Me.NodeTextBox1)
        Me.tvVariables.NodeControls.Add(Me.NodeTextBox2)
        Me.tvVariables.NodeControls.Add(Me.NodeTextBox3)
        Me.tvVariables.SelectedNode = Nothing
        Me.tvVariables.UseColumns = True
        '
        'tc1
        '
        resources.ApplyResources(Me.tc1, "tc1")
        Me.tc1.SortOrder = System.Windows.Forms.SortOrder.None
        '
        'tc2
        '
        resources.ApplyResources(Me.tc2, "tc2")
        Me.tc2.SortOrder = System.Windows.Forms.SortOrder.None
        '
        'tc3
        '
        resources.ApplyResources(Me.tc3, "tc3")
        Me.tc3.SortOrder = System.Windows.Forms.SortOrder.None
        '
        'ContextMenuStrip1
        '
        Me.ContextMenuStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ItemToolStripMenuItem, Me.ToolStripSeparator1, Me.ViewAPIHelpTSMI, Me.ViewSourceCodeTSMI})
        Me.ContextMenuStrip1.Name = "ContextMenuStrip1"
        resources.ApplyResources(Me.ContextMenuStrip1, "ContextMenuStrip1")
        '
        'ItemToolStripMenuItem
        '
        resources.ApplyResources(Me.ItemToolStripMenuItem, "ItemToolStripMenuItem")
        Me.ItemToolStripMenuItem.Name = "ItemToolStripMenuItem"
        '
        'ToolStripSeparator1
        '
        Me.ToolStripSeparator1.Name = "ToolStripSeparator1"
        resources.ApplyResources(Me.ToolStripSeparator1, "ToolStripSeparator1")
        '
        'ViewAPIHelpTSMI
        '
        Me.ViewAPIHelpTSMI.Image = Global.DWSIM.My.Resources.Resources.help1
        Me.ViewAPIHelpTSMI.Name = "ViewAPIHelpTSMI"
        resources.ApplyResources(Me.ViewAPIHelpTSMI, "ViewAPIHelpTSMI")
        '
        'ViewSourceCodeTSMI
        '
        Me.ViewSourceCodeTSMI.Image = Global.DWSIM.My.Resources.Resources.tag
        Me.ViewSourceCodeTSMI.Name = "ViewSourceCodeTSMI"
        resources.ApplyResources(Me.ViewSourceCodeTSMI, "ViewSourceCodeTSMI")
        '
        'NodeIcon1
        '
        Me.NodeIcon1.DataPropertyName = "ItemIcon"
        Me.NodeIcon1.LeftMargin = 1
        Me.NodeIcon1.ParentColumn = Me.tc1
        Me.NodeIcon1.ScaleMode = Aga.Controls.Tree.ImageScaleMode.Fit
        '
        'NodeTextBox1
        '
        Me.NodeTextBox1.DataPropertyName = "ItemName"
        Me.NodeTextBox1.IncrementalSearchEnabled = True
        Me.NodeTextBox1.LeftMargin = 3
        Me.NodeTextBox1.ParentColumn = Me.tc1
        Me.NodeTextBox1.Trimming = System.Drawing.StringTrimming.EllipsisCharacter
        '
        'NodeTextBox2
        '
        Me.NodeTextBox2.DataPropertyName = "ItemType"
        Me.NodeTextBox2.IncrementalSearchEnabled = True
        Me.NodeTextBox2.LeftMargin = 3
        Me.NodeTextBox2.ParentColumn = Me.tc2
        Me.NodeTextBox2.Trimming = System.Drawing.StringTrimming.EllipsisCharacter
        '
        'NodeTextBox3
        '
        Me.NodeTextBox3.DataPropertyName = "ItemValue"
        Me.NodeTextBox3.IncrementalSearchEnabled = True
        Me.NodeTextBox3.LeftMargin = 3
        Me.NodeTextBox3.ParentColumn = Me.tc3
        Me.NodeTextBox3.Trimming = System.Drawing.StringTrimming.EllipsisCharacter
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.cbPythonEngine)
        Me.Panel1.Controls.Add(Me.Label3)
        Me.Panel1.Controls.Add(Me.cbLinkedEvent)
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Controls.Add(Me.cbLinkedObject)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.chkLink)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'cbPythonEngine
        '
        resources.ApplyResources(Me.cbPythonEngine, "cbPythonEngine")
        Me.cbPythonEngine.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbPythonEngine.FormattingEnabled = True
        Me.cbPythonEngine.Items.AddRange(New Object() {resources.GetString("cbPythonEngine.Items"), resources.GetString("cbPythonEngine.Items1")})
        Me.cbPythonEngine.Name = "cbPythonEngine"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbLinkedEvent
        '
        Me.cbLinkedEvent.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        resources.ApplyResources(Me.cbLinkedEvent, "cbLinkedEvent")
        Me.cbLinkedEvent.FormattingEnabled = True
        Me.cbLinkedEvent.Name = "cbLinkedEvent"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbLinkedObject
        '
        Me.cbLinkedObject.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        resources.ApplyResources(Me.cbLinkedObject, "cbLinkedObject")
        Me.cbLinkedObject.FormattingEnabled = True
        Me.cbLinkedObject.Name = "cbLinkedObject"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'chkLink
        '
        resources.ApplyResources(Me.chkLink, "chkLink")
        Me.chkLink.Name = "chkLink"
        Me.chkLink.UseVisualStyleBackColor = True
        '
        'treeViewItems
        '
        resources.ApplyResources(Me.treeViewItems, "treeViewItems")
        Me.treeViewItems.Name = "treeViewItems"
        Me.treeViewItems.PathSeparator = "."
        '
        'ScriptEditorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.SplitContainer1)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "ScriptEditorControl"
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        Me.SplitContainer1.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.ContextMenuStrip1.ResumeLayout(False)
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents cbLinkedEvent As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents cbLinkedObject As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents chkLink As System.Windows.Forms.CheckBox
    Public WithEvents treeViewItems As System.Windows.Forms.TreeView
    Friend WithEvents txtScript As ScintillaNET.Scintilla
    Friend WithEvents cbPythonEngine As System.Windows.Forms.ComboBox
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents SplitContainer1 As SplitContainer
    Friend WithEvents TableLayoutPanel1 As TableLayoutPanel
    Friend WithEvents Label4 As Label
    Friend WithEvents tvVariables As Aga.Controls.Tree.TreeViewAdv
    Friend WithEvents tc1 As Aga.Controls.Tree.TreeColumn
    Friend WithEvents tc2 As Aga.Controls.Tree.TreeColumn
    Friend WithEvents NodeTextBox1 As Aga.Controls.Tree.NodeControls.NodeTextBox
    Friend WithEvents NodeTextBox2 As Aga.Controls.Tree.NodeControls.NodeTextBox
    Friend WithEvents tc3 As Aga.Controls.Tree.TreeColumn
    Friend WithEvents NodeTextBox3 As Aga.Controls.Tree.NodeControls.NodeTextBox
    Friend WithEvents Panel1 As Panel
    Friend WithEvents NodeIcon1 As Aga.Controls.Tree.NodeControls.NodeIcon
    Friend WithEvents ContextMenuStrip1 As ContextMenuStrip
    Friend WithEvents ViewAPIHelpTSMI As ToolStripMenuItem
    Friend WithEvents ViewSourceCodeTSMI As ToolStripMenuItem
    Friend WithEvents ItemToolStripMenuItem As ToolStripMenuItem
    Friend WithEvents ToolStripSeparator1 As ToolStripSeparator
End Class
