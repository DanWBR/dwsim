<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormGraphPFR
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormGraphPFR))
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.ZedGraphControl1 = New ZedGraph.ZedGraphControl()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.ZedGraphControl1)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'ZedGraphControl1
        '
        resources.ApplyResources(Me.ZedGraphControl1, "ZedGraphControl1")
        Me.ZedGraphControl1.IsAntiAlias = True
        Me.ZedGraphControl1.IsAutoScrollRange = True
        Me.ZedGraphControl1.IsShowPointValues = True
        Me.ZedGraphControl1.Name = "ZedGraphControl1"
        Me.ZedGraphControl1.ScrollGrace = 0.0R
        Me.ZedGraphControl1.ScrollMaxX = 0.0R
        Me.ZedGraphControl1.ScrollMaxY = 0.0R
        Me.ZedGraphControl1.ScrollMaxY2 = 0.0R
        Me.ZedGraphControl1.ScrollMinX = 0.0R
        Me.ZedGraphControl1.ScrollMinY = 0.0R
        Me.ZedGraphControl1.ScrollMinY2 = 0.0R
        '
        'FormGraphPFR
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox2)
        Me.MinimizeBox = False
        Me.Name = "FormGraphPFR"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.GroupBox2.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents ZedGraphControl1 As ZedGraph.ZedGraphControl
End Class
