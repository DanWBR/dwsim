<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class UIReboiledSideStripperEditorForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(UIReboiledSideStripperEditorForm))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.ToolStrip2 = New System.Windows.Forms.ToolStrip
        Me.ToolStripButton1 = New System.Windows.Forms.ToolStripButton
        Me.ToolStripButton2 = New System.Windows.Forms.ToolStripButton
        Me.ToolStripButton3 = New System.Windows.Forms.ToolStripButton
        Me.ToolStripButton4 = New System.Windows.Forms.ToolStripButton
        Me.ToolStripButton8 = New System.Windows.Forms.ToolStripButton
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Column3 = New System.Windows.Forms.DataGridViewComboBoxColumn
        Me.Column4 = New System.Windows.Forms.DataGridViewComboBoxColumn
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Column8 = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.Column5 = New System.Windows.Forms.DataGridViewComboBoxColumn
        Me.Column9 = New System.Windows.Forms.DataGridViewTextBoxColumn
        dgv1 = New System.Windows.Forms.DataGridView
        Me.TableLayoutPanel1.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(dgv1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.ToolStrip2.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        resources.ApplyResources(Me.TableLayoutPanel1, "TableLayoutPanel1")
        Me.TableLayoutPanel1.Controls.Add(Me.PictureBox1, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(dgv1, 0, 0)
        Me.TableLayoutPanel1.Controls.Add(Me.ToolStrip2, 0, 0)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        '
        'PictureBox1
        '
        Me.PictureBox1.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.reboiledsidestripper
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'dgv1
        '
        dgv1.AllowUserToAddRows = False
        dgv1.AllowUserToDeleteRows = False
        dgv1.AllowUserToOrderColumns = True
        dgv1.AllowUserToResizeRows = False
        dgv1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.DisplayedCells
        dgv1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        dgv1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column1, Me.Column2, Me.Column6, Me.Column3, Me.Column4, Me.Column7, Me.Column8, Me.Column5, Me.Column9})
        DataGridViewCellStyle1.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleLeft
        DataGridViewCellStyle1.BackColor = System.Drawing.SystemColors.Window
        DataGridViewCellStyle1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle1.ForeColor = System.Drawing.SystemColors.ControlText
        DataGridViewCellStyle1.NullValue = "<empty>"
        DataGridViewCellStyle1.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle1.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle1.WrapMode = System.Windows.Forms.DataGridViewTriState.[False]
        dgv1.DefaultCellStyle = DataGridViewCellStyle1
        resources.ApplyResources(dgv1, "dgv1")
        dgv1.MultiSelect = False
        dgv1.Name = "dgv1"
        dgv1.RowHeadersVisible = False
        dgv1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        AddHandler dgv1.CellValueChanged, AddressOf Me.dgv1_CellValueChanged
        AddHandler dgv1.DataError, AddressOf Me.dgv1_DataError
        '
        'ToolStrip2
        '
        resources.ApplyResources(Me.ToolStrip2, "ToolStrip2")
        Me.ToolStrip2.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripButton1, Me.ToolStripButton2, Me.ToolStripButton3, Me.ToolStripButton4, Me.ToolStripButton8})
        Me.ToolStrip2.Name = "ToolStrip2"
        '
        'ToolStripButton1
        '
        Me.ToolStripButton1.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton1.Image = Global.DWSIM.My.Resources.Resources.add
        resources.ApplyResources(Me.ToolStripButton1, "ToolStripButton1")
        Me.ToolStripButton1.Name = "ToolStripButton1"
        '
        'ToolStripButton2
        '
        Me.ToolStripButton2.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton2.Image = Global.DWSIM.My.Resources.Resources.delete1
        resources.ApplyResources(Me.ToolStripButton2, "ToolStripButton2")
        Me.ToolStripButton2.Name = "ToolStripButton2"
        '
        'ToolStripButton3
        '
        Me.ToolStripButton3.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton3.Image = Global.DWSIM.My.Resources.Resources.copy
        resources.ApplyResources(Me.ToolStripButton3, "ToolStripButton3")
        Me.ToolStripButton3.Name = "ToolStripButton3"
        '
        'ToolStripButton4
        '
        Me.ToolStripButton4.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton4.Image = Global.DWSIM.My.Resources.Resources.paste_plain
        resources.ApplyResources(Me.ToolStripButton4, "ToolStripButton4")
        Me.ToolStripButton4.Name = "ToolStripButton4"
        '
        'ToolStripButton8
        '
        Me.ToolStripButton8.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.ToolStripButton8.Image = Global.DWSIM.My.Resources.Resources.cross
        resources.ApplyResources(Me.ToolStripButton8, "ToolStripButton8")
        Me.ToolStripButton8.Name = "ToolStripButton8"
        '
        'Column1
        '
        Me.Column1.FillWeight = 10.0!
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'Column2
        '
        Me.Column2.FillWeight = 20.0!
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        '
        'Column6
        '
        Me.Column6.FillWeight = 20.0!
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        '
        'Column3
        '
        Me.Column3.FillWeight = 25.0!
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        Me.Column3.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Column3.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'Column4
        '
        Me.Column4.FillWeight = 25.0!
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        Me.Column4.Resizable = System.Windows.Forms.DataGridViewTriState.[True]
        Me.Column4.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.Automatic
        '
        'Column7
        '
        Me.Column7.FillWeight = 20.0!
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        '
        'Column8
        '
        Me.Column8.FillWeight = 20.0!
        resources.ApplyResources(Me.Column8, "Column8")
        Me.Column8.Name = "Column8"
        '
        'Column5
        '
        Me.Column5.FillWeight = 20.0!
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        '
        'Column9
        '
        resources.ApplyResources(Me.Column9, "Column9")
        Me.Column9.Name = "Column9"
        '
        'UIReboiledSideStripperEditorForm
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "UIReboiledSideStripperEditorForm"
        Me.ShowInTaskbar = False
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.TableLayoutPanel1.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(dgv1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ToolStrip2.ResumeLayout(False)
        Me.ToolStrip2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents dgv1 As System.Windows.Forms.DataGridView
    Public WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Public WithEvents ToolStrip2 As System.Windows.Forms.ToolStrip
    Public WithEvents ToolStripButton1 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton2 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton3 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton4 As System.Windows.Forms.ToolStripButton
    Public WithEvents ToolStripButton8 As System.Windows.Forms.ToolStripButton
    Public WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Public WithEvents Column1 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column2 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column6 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column3 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents Column4 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents Column7 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column8 As System.Windows.Forms.DataGridViewTextBoxColumn
    Public WithEvents Column5 As System.Windows.Forms.DataGridViewComboBoxColumn
    Public WithEvents Column9 As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
