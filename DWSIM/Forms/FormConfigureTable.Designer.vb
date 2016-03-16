<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormConfigureTable
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigureTable))
        Me.TreeView1 = New System.Windows.Forms.TreeView()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.KryptonButton1 = New System.Windows.Forms.Button()
        Me.SuspendLayout()
        '
        'TreeView1
        '
        resources.ApplyResources(Me.TreeView1, "TreeView1")
        Me.TreeView1.CheckBoxes = True
        Me.TreeView1.Name = "TreeView1"
        Me.TreeView1.Nodes.AddRange(New System.Windows.Forms.TreeNode() {CType(resources.GetObject("TreeView1.Nodes"), System.Windows.Forms.TreeNode)})
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'KryptonButton1
        '
        resources.ApplyResources(Me.KryptonButton1, "KryptonButton1")
        Me.KryptonButton1.Name = "KryptonButton1"
        '
        'FormConfigureTable
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ControlBox = False
        Me.Controls.Add(Me.KryptonButton1)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.TreeView1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormConfigureTable"
        Me.ShowIcon = False
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents TreeView1 As System.Windows.Forms.TreeView
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents KryptonButton1 As System.Windows.Forms.Button
End Class
