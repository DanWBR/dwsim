<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormConfigPropertyPackage
    Inherits FormConfigPropertyPackageBase

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigPropertyPackage))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.gridKij = New unvell.ReoGrid.ReoGridControl()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.IPGrid = New System.Windows.Forms.DataGridView()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.lblModel = New System.Windows.Forms.Label()
        Me.TabPageU = New System.Windows.Forms.TabPage()
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.TabPageU.SuspendLayout()
        Me.SuspendLayout()
        '
        'gridKij
        '
        resources.ApplyResources(Me.gridKij, "gridKij")
        Me.gridKij.BackColor = System.Drawing.Color.White
        Me.gridKij.ColumnHeaderContextMenuStrip = Nothing
        Me.gridKij.LeadHeaderContextMenuStrip = Nothing
        Me.gridKij.Name = "gridKij"
        Me.gridKij.RowHeaderContextMenuStrip = Nothing
        Me.gridKij.Script = Nothing
        Me.gridKij.SheetTabContextMenuStrip = Nothing
        Me.gridKij.SheetTabNewButtonVisible = False
        Me.gridKij.SheetTabVisible = False
        Me.gridKij.SheetTabWidth = 60
        Me.gridKij.ShowScrollEndSpacing = False
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'IPGrid
        '
        resources.ApplyResources(Me.IPGrid, "IPGrid")
        Me.IPGrid.AllowUserToAddRows = False
        Me.IPGrid.AllowUserToDeleteRows = False
        Me.IPGrid.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.IPGrid.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Segoe UI", 8.25!)
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.IPGrid.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle1
        Me.IPGrid.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column6})
        Me.IPGrid.MultiSelect = False
        Me.IPGrid.Name = "IPGrid"
        Me.IPGrid.ReadOnly = True
        '
        'Column6
        '
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        Me.Column6.ReadOnly = True
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPageU)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.lblModel)
        Me.TabPage1.Controls.Add(Me.gridKij)
        Me.TabPage1.Controls.Add(Me.Button3)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'lblModel
        '
        resources.ApplyResources(Me.lblModel, "lblModel")
        Me.lblModel.Name = "lblModel"
        '
        'TabPageU
        '
        resources.ApplyResources(Me.TabPageU, "TabPageU")
        Me.TabPageU.Controls.Add(Me.IPGrid)
        Me.TabPageU.Name = "TabPageU"
        Me.TabPageU.UseVisualStyleBackColor = True
        '
        'FormConfigPropertyPackage
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.TabControl1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormConfigPropertyPackage"
        Me.TopMost = True
        CType(Me.IPGrid, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.TabPageU.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents Button3 As System.Windows.Forms.Button
    Friend WithEvents IPGrid As System.Windows.Forms.DataGridView
    Friend WithEvents Column6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPageU As TabPage
    Friend WithEvents gridKij As unvell.ReoGrid.ReoGridControl
    Friend WithEvents lblModel As Label
End Class
