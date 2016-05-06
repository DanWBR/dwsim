<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormSelectProperties
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormSelectProperties))
        Me.lvProps = New System.Windows.Forms.ListView()
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.lvObjects = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'lvProps
        '
        Me.lvProps.CheckBoxes = True
        Me.lvProps.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvProps.FullRowSelect = True
        resources.ApplyResources(Me.lvProps, "lvProps")
        Me.lvProps.Name = "lvProps"
        Me.lvProps.UseCompatibleStateImageBehavior = False
        Me.lvProps.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader2
        '
        resources.ApplyResources(Me.ColumnHeader2, "ColumnHeader2")
        '
        'lvObjects
        '
        Me.lvObjects.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvObjects.FullRowSelect = True
        resources.ApplyResources(Me.lvObjects, "lvObjects")
        Me.lvObjects.MultiSelect = False
        Me.lvObjects.Name = "lvObjects"
        Me.lvObjects.ShowGroups = False
        Me.lvObjects.Sorting = System.Windows.Forms.SortOrder.Ascending
        Me.lvObjects.UseCompatibleStateImageBehavior = False
        Me.lvObjects.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        resources.ApplyResources(Me.ColumnHeader1, "ColumnHeader1")
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'TextBox1
        '
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.Name = "TextBox1"
        '
        'FormSelectProperties
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.lvObjects)
        Me.Controls.Add(Me.lvProps)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormSelectProperties"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents lvProps As System.Windows.Forms.ListView
    Friend WithEvents lvObjects As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
End Class
