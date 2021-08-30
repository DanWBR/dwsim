<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class EditingForm_CompressorExpander_Curves
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
    <System.Diagnostics.DebuggerNonUserCode()>
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
    <System.Diagnostics.DebuggerStepThrough()>
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_CompressorExpander_Curves))
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton()
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton()
        Me.tsbExport = New System.Windows.Forms.ToolStripButton()
        Me.tsbImport = New System.Windows.Forms.ToolStripButton()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage3 = New System.Windows.Forms.TabPage()
        Me.OpenFileDialog1 = New System.Windows.Forms.OpenFileDialog()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
        Me.ToolStrip1.SuspendLayout()
        Me.TabControl1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2, Me.tsbExport, Me.tsbImport})
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.Professional
        '
        'ToolStripButton1
        '
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.add
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.delete
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'tsbExport
        '
        resources.ApplyResources(Me.tsbExport, "tsbExport")
        Me.tsbExport.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsbExport.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.arrow_up
        Me.tsbExport.Name = "tsbExport"
        '
        'tsbImport
        '
        resources.ApplyResources(Me.tsbImport, "tsbImport")
        Me.tsbImport.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsbImport.Image = Global.DWSIM.UnitOperations.My.Resources.Resources.arrow_down
        Me.tsbImport.Name = "tsbImport"
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage3)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage3
        '
        resources.ApplyResources(Me.TabPage3, "TabPage3")
        Me.TabPage3.Name = "TabPage3"
        Me.TabPage3.UseVisualStyleBackColor = True
        '
        'OpenFileDialog1
        '
        resources.ApplyResources(Me.OpenFileDialog1, "OpenFileDialog1")
        Me.OpenFileDialog1.Multiselect = True
        '
        'SaveFileDialog1
        '
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        Me.SaveFileDialog1.RestoreDirectory = True
        '
        'EditingForm_CompressorExpander_Curves
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TabControl1)
        Me.Controls.Add(Me.ToolStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_CompressorExpander_Curves"
        Me.ShowInTaskbar = False
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.TabControl1.ResumeLayout(False)
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents ToolStrip1 As ToolStrip
    Friend WithEvents ToolStripButton1 As ToolStripButton
    Friend WithEvents ToolStripButton2 As ToolStripButton
    Friend WithEvents tsbExport As ToolStripButton
    Friend WithEvents tsbImport As ToolStripButton
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage3 As TabPage
    Friend WithEvents OpenFileDialog1 As OpenFileDialog
    Friend WithEvents SaveFileDialog1 As SaveFileDialog
End Class
