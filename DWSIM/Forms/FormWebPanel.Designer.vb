<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormWebPanel

    Inherits UserControl

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormWebPanel))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TabPageD = New System.Windows.Forms.TabPage()
        Me.TabPageC = New System.Windows.Forms.TabPage()
        Me.TabPageB = New System.Windows.Forms.TabPage()
        Me.TabPageA = New System.Windows.Forms.TabPage()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPageE = New System.Windows.Forms.TabPage()
        Me.TabControl1.SuspendLayout()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.BackColor = System.Drawing.Color.Transparent
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Name = "Label1"
        '
        'TabPageD
        '
        resources.ApplyResources(Me.TabPageD, "TabPageD")
        Me.TabPageD.Name = "TabPageD"
        Me.TabPageD.UseVisualStyleBackColor = True
        '
        'TabPageC
        '
        resources.ApplyResources(Me.TabPageC, "TabPageC")
        Me.TabPageC.Name = "TabPageC"
        Me.TabPageC.UseVisualStyleBackColor = True
        '
        'TabPageB
        '
        resources.ApplyResources(Me.TabPageB, "TabPageB")
        Me.TabPageB.Name = "TabPageB"
        Me.TabPageB.UseVisualStyleBackColor = True
        '
        'TabPageA
        '
        resources.ApplyResources(Me.TabPageA, "TabPageA")
        Me.TabPageA.Name = "TabPageA"
        Me.TabPageA.UseVisualStyleBackColor = True
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPageA)
        Me.TabControl1.Controls.Add(Me.TabPageB)
        Me.TabControl1.Controls.Add(Me.TabPageC)
        Me.TabControl1.Controls.Add(Me.TabPageD)
        Me.TabControl1.Controls.Add(Me.TabPageE)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPageE
        '
        resources.ApplyResources(Me.TabPageE, "TabPageE")
        Me.TabPageE.Name = "TabPageE"
        Me.TabPageE.UseVisualStyleBackColor = True
        '
        'FormWebPanel
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.BackColor = System.Drawing.Color.SteelBlue
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.Label1)
        Me.Name = "FormWebPanel"
        Me.TabControl1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents TabPageD As TabPage
    Friend WithEvents TabPageC As TabPage
    Friend WithEvents TabPageB As TabPage
    Friend WithEvents TabPageA As TabPage
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPageE As TabPage
End Class
