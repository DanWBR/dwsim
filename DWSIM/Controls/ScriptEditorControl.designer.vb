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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ScriptEditorControl))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.txtScript = New ScintillaNET.Scintilla()
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tvVariables = New Aga.Controls.Tree.TreeViewAdv()
        Me.tc1 = New Aga.Controls.Tree.TreeColumn()
        Me.tc2 = New Aga.Controls.Tree.TreeColumn()
        Me.tc3 = New Aga.Controls.Tree.TreeColumn()
        Me.NodeIcon1 = New Aga.Controls.Tree.NodeControls.NodeIcon()
        Me.NodeTextBox1 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.NodeTextBox2 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.NodeTextBox3 = New Aga.Controls.Tree.NodeControls.NodeTextBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.tbName = New System.Windows.Forms.TextBox()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.btnDelete = New System.Windows.Forms.Button()
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
        resources.ApplyResources(Me.SplitContainer1.Panel1, "SplitContainer1.Panel1")
        Me.SplitContainer1.Panel1.Controls.Add(Me.txtScript)
        '
        'SplitContainer1.Panel2
        '
        resources.ApplyResources(Me.SplitContainer1.Panel2, "SplitContainer1.Panel2")
        Me.SplitContainer1.Panel2.Controls.Add(Me.TableLayoutPanel1)
        Me.SplitContainer1.Panel2Collapsed = True
        '
        'txtScript
        '
        resources.ApplyResources(Me.txtScript, "txtScript")
        Me.txtScript.AnnotationVisible = ScintillaNET.Annotation.Standard
        Me.txtScript.AutoCChooseSingle = True
        Me.txtScript.AutoCMaxHeight = 10
        Me.txtScript.AutoCOrder = ScintillaNET.Order.PerformSort
        Me.txtScript.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle
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
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.BackColor = System.Drawing.Color.SteelBlue
        Me.Label4.ForeColor = System.Drawing.Color.White
        Me.Label4.Name = "Label4"
        '
        'tvVariables
        '
        resources.ApplyResources(Me.tvVariables, "tvVariables")
        Me.tvVariables.AsyncExpanding = True
        Me.tvVariables.AutoRowHeight = True
        Me.tvVariables.BackColor = System.Drawing.SystemColors.Window
        Me.tvVariables.Columns.Add(Me.tc1)
        Me.tvVariables.Columns.Add(Me.tc2)
        Me.tvVariables.Columns.Add(Me.tc3)
        Me.tvVariables.DefaultToolTipProvider = Nothing
        Me.tvVariables.DragDropMarkColor = System.Drawing.Color.Black
        Me.tvVariables.FullRowSelect = True
        Me.tvVariables.GridLineStyle = CType((Aga.Controls.Tree.GridLineStyle.Horizontal Or Aga.Controls.Tree.GridLineStyle.Vertical), Aga.Controls.Tree.GridLineStyle)
        Me.tvVariables.LineColor = System.Drawing.SystemColors.ControlDark
        Me.tvVariables.LoadOnDemand = True
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
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Controls.Add(Me.tbName)
        Me.Panel1.Controls.Add(Me.Label5)
        Me.Panel1.Controls.Add(Me.btnDelete)
        Me.Panel1.Controls.Add(Me.cbPythonEngine)
        Me.Panel1.Controls.Add(Me.Label3)
        Me.Panel1.Controls.Add(Me.cbLinkedEvent)
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Controls.Add(Me.cbLinkedObject)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.chkLink)
        Me.Panel1.Name = "Panel1"
        '
        'tbName
        '
        resources.ApplyResources(Me.tbName, "tbName")
        Me.tbName.Name = "tbName"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'btnDelete
        '
        resources.ApplyResources(Me.btnDelete, "btnDelete")
        Me.btnDelete.Name = "btnDelete"
        Me.btnDelete.UseVisualStyleBackColor = True
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
        resources.ApplyResources(Me.cbLinkedEvent, "cbLinkedEvent")
        Me.cbLinkedEvent.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
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
        resources.ApplyResources(Me.cbLinkedObject, "cbLinkedObject")
        Me.cbLinkedObject.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
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
        Me.treeViewItems.LineColor = System.Drawing.Color.Empty
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
    Friend WithEvents Label5 As Label
    Public WithEvents btnDelete As Button
    Public WithEvents tbName As TextBox
End Class
