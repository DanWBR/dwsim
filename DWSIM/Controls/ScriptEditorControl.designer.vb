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
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.cbPythonEngine = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbLinkedEvent = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbLinkedObject = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.chkLink = New System.Windows.Forms.CheckBox()
        Me.treeViewItems = New System.Windows.Forms.TreeView()
        Me.txtScript = New ScintillaNET.Scintilla()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
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
        Me.treeViewItems.LineColor = System.Drawing.Color.Empty
        resources.ApplyResources(Me.treeViewItems, "treeViewItems")
        Me.treeViewItems.Name = "treeViewItems"
        Me.treeViewItems.PathSeparator = "."
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
        'ScriptEditorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.txtScript)
        Me.Controls.Add(Me.Panel1)
        Me.Name = "ScriptEditorControl"
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents cbLinkedEvent As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents cbLinkedObject As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents chkLink As System.Windows.Forms.CheckBox
    Public WithEvents treeViewItems As System.Windows.Forms.TreeView
    Friend WithEvents txtScript As ScintillaNET.Scintilla
    Friend WithEvents cbPythonEngine As System.Windows.Forms.ComboBox
    Friend WithEvents Label3 As System.Windows.Forms.Label

End Class
