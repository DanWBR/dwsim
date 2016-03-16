<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class SelectorForm
    Inherits System.Windows.Forms.Form

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(SelectorForm))
        Me.KryptonButton2 = New System.Windows.Forms.Button()
        Me.KryptonButton1 = New System.Windows.Forms.Button()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.TreeView3 = New System.Windows.Forms.TreeView()
        Me.TreeView2 = New System.Windows.Forms.TreeView()
        Me.TreeView1 = New System.Windows.Forms.TreeView()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.GroupBox1.SuspendLayout()
        Me.SuspendLayout()
        '
        'KryptonButton2
        '
        resources.ApplyResources(Me.KryptonButton2, "KryptonButton2")
        Me.KryptonButton2.Name = "KryptonButton2"
        '
        'KryptonButton1
        '
        resources.ApplyResources(Me.KryptonButton1, "KryptonButton1")
        Me.KryptonButton1.Name = "KryptonButton1"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.TreeView3)
        Me.GroupBox1.Controls.Add(Me.TreeView2)
        Me.GroupBox1.Controls.Add(Me.TreeView1)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'TreeView3
        '
        resources.ApplyResources(Me.TreeView3, "TreeView3")
        Me.TreeView3.FullRowSelect = True
        Me.TreeView3.HideSelection = False
        Me.TreeView3.Name = "TreeView3"
        Me.TreeView3.ShowLines = False
        Me.TreeView3.ShowPlusMinus = False
        Me.TreeView3.ShowRootLines = False
        '
        'TreeView2
        '
        resources.ApplyResources(Me.TreeView2, "TreeView2")
        Me.TreeView2.FullRowSelect = True
        Me.TreeView2.HideSelection = False
        Me.TreeView2.Name = "TreeView2"
        Me.TreeView2.ShowLines = False
        Me.TreeView2.ShowPlusMinus = False
        Me.TreeView2.ShowRootLines = False
        '
        'TreeView1
        '
        resources.ApplyResources(Me.TreeView1, "TreeView1")
        Me.TreeView1.FullRowSelect = True
        Me.TreeView1.HideSelection = False
        Me.TreeView1.Name = "TreeView1"
        Me.TreeView1.ShowLines = False
        Me.TreeView1.ShowPlusMinus = False
        Me.TreeView1.ShowRootLines = False
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
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'SelectorForm
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.KryptonButton2)
        Me.Controls.Add(Me.KryptonButton1)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "SelectorForm"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents KryptonButton2 As System.Windows.Forms.Button
    Public WithEvents KryptonButton1 As System.Windows.Forms.Button
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents Label3 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents TreeView3 As System.Windows.Forms.TreeView
    Public WithEvents TreeView2 As System.Windows.Forms.TreeView
    Public WithEvents TreeView1 As System.Windows.Forms.TreeView
End Class
