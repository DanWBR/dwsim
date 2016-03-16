<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class UISpreadsheetEditorForm
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
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel
        Me.DataGridView1 = New System.Windows.Forms.DataGridView
        Me.A = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.B = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.C = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.D = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.E = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.F = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.G = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.H = New System.Windows.Forms.DataGridViewTextBoxColumn
        Me.TableLayoutPanel2 = New System.Windows.Forms.TableLayoutPanel
        Me.tbCell = New System.Windows.Forms.TextBox
        Me.Button1 = New System.Windows.Forms.Button
        Me.Button2 = New System.Windows.Forms.Button
        Me.Label1 = New System.Windows.Forms.Label
        Me.tbValue = New System.Windows.Forms.TextBox
        Me.TableLayoutPanel1.SuspendLayout()
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TableLayoutPanel2.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.ColumnCount = 1
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.DataGridView1, 0, 1)
        Me.TableLayoutPanel1.Controls.Add(Me.TableLayoutPanel2, 0, 0)
        Me.TableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 2
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 33.0!))
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(541, 379)
        Me.TableLayoutPanel1.TabIndex = 2
        '
        'DataGridView1
        '
        Me.DataGridView1.AllowUserToAddRows = False
        Me.DataGridView1.AllowUserToDeleteRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.DataGridView1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.DataGridView1.BorderStyle = System.Windows.Forms.BorderStyle.None
        DataGridViewCellStyle2.Alignment = System.Windows.Forms.DataGridViewContentAlignment.MiddleCenter
        DataGridViewCellStyle2.BackColor = System.Drawing.SystemColors.Control
        DataGridViewCellStyle2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        DataGridViewCellStyle2.ForeColor = System.Drawing.SystemColors.WindowText
        DataGridViewCellStyle2.SelectionBackColor = System.Drawing.SystemColors.Highlight
        DataGridViewCellStyle2.SelectionForeColor = System.Drawing.SystemColors.HighlightText
        DataGridViewCellStyle2.WrapMode = System.Windows.Forms.DataGridViewTriState.[True]
        Me.DataGridView1.ColumnHeadersDefaultCellStyle = DataGridViewCellStyle2
        Me.DataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.A, Me.B, Me.C, Me.D, Me.E, Me.F, Me.G, Me.H})
        Me.DataGridView1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.DataGridView1.Location = New System.Drawing.Point(3, 36)
        Me.DataGridView1.Name = "DataGridView1"
        Me.DataGridView1.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.AutoSizeToAllHeaders
        Me.DataGridView1.RowTemplate.Height = 20
        Me.DataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        Me.DataGridView1.ShowCellErrors = False
        Me.DataGridView1.ShowEditingIcon = False
        Me.DataGridView1.ShowRowErrors = False
        Me.DataGridView1.Size = New System.Drawing.Size(535, 340)
        Me.DataGridView1.TabIndex = 0
        '
        'A
        '
        Me.A.HeaderText = "A"
        Me.A.Name = "A"
        Me.A.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.A.Width = 64
        '
        'B
        '
        Me.B.HeaderText = "B"
        Me.B.Name = "B"
        Me.B.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.B.Width = 65
        '
        'C
        '
        Me.C.HeaderText = "C"
        Me.C.Name = "C"
        Me.C.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.C.Width = 64
        '
        'D
        '
        Me.D.HeaderText = "D"
        Me.D.Name = "D"
        Me.D.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.D.Width = 64
        '
        'E
        '
        Me.E.HeaderText = "E"
        Me.E.Name = "E"
        Me.E.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.E.Width = 64
        '
        'F
        '
        Me.F.HeaderText = "F"
        Me.F.Name = "F"
        Me.F.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.F.Width = 65
        '
        'G
        '
        Me.G.HeaderText = "G"
        Me.G.Name = "G"
        Me.G.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.G.Width = 64
        '
        'H
        '
        Me.H.HeaderText = "H"
        Me.H.Name = "H"
        Me.H.SortMode = System.Windows.Forms.DataGridViewColumnSortMode.NotSortable
        Me.H.Width = 64
        '
        'TableLayoutPanel2
        '
        Me.TableLayoutPanel2.ColumnCount = 5
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 50.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 28.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 28.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Absolute, 37.0!))
        Me.TableLayoutPanel2.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel2.Controls.Add(Me.tbCell, 0, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.Button1, 1, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.Button2, 2, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.Label1, 3, 0)
        Me.TableLayoutPanel2.Controls.Add(Me.tbValue, 4, 0)
        Me.TableLayoutPanel2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel2.Location = New System.Drawing.Point(2, 2)
        Me.TableLayoutPanel2.Margin = New System.Windows.Forms.Padding(2)
        Me.TableLayoutPanel2.Name = "TableLayoutPanel2"
        Me.TableLayoutPanel2.RowCount = 1
        Me.TableLayoutPanel2.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel2.Size = New System.Drawing.Size(537, 29)
        Me.TableLayoutPanel2.TabIndex = 1
        '
        'tbCell
        '
        Me.tbCell.Anchor = System.Windows.Forms.AnchorStyles.Left
        Me.tbCell.Location = New System.Drawing.Point(3, 4)
        Me.tbCell.Name = "tbCell"
        Me.tbCell.Size = New System.Drawing.Size(44, 20)
        Me.tbCell.TabIndex = 0
        '
        'Button1
        '
        Me.Button1.Image = Global.DWSIM.My.Resources.Resources.cross
        Me.Button1.Location = New System.Drawing.Point(53, 3)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(22, 22)
        Me.Button1.TabIndex = 1
        Me.Button1.TextImageRelation = System.Windows.Forms.TextImageRelation.ImageAboveText
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        Me.Button2.Image = Global.DWSIM.My.Resources.Resources.tick
        Me.Button2.Location = New System.Drawing.Point(81, 3)
        Me.Button2.Name = "Button2"
        Me.Button2.Size = New System.Drawing.Size(22, 22)
        Me.Button2.TabIndex = 2
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Label1
        '
        Me.Label1.Anchor = System.Windows.Forms.AnchorStyles.Left
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(109, 8)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(31, 13)
        Me.Label1.TabIndex = 3
        Me.Label1.Text = "Valor"
        '
        'tbValue
        '
        Me.tbValue.Anchor = CType((System.Windows.Forms.AnchorStyles.Left Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.tbValue.Location = New System.Drawing.Point(146, 4)
        Me.tbValue.Name = "tbValue"
        Me.tbValue.Size = New System.Drawing.Size(388, 20)
        Me.tbValue.TabIndex = 4
        '
        'UISpreadsheetEditorForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font

        Me.ClientSize = New System.Drawing.Size(541, 379)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.Name = "UISpreadsheetEditorForm"
        Me.Text = "UISpreadsheetEditorForm"
        Me.TableLayoutPanel1.ResumeLayout(False)
        CType(Me.DataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TableLayoutPanel2.ResumeLayout(False)
        Me.TableLayoutPanel2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents DataGridView1 As System.Windows.Forms.DataGridView
    Friend WithEvents A As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents B As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents C As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents D As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents E As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents F As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents G As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents H As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents TableLayoutPanel2 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents tbCell As System.Windows.Forms.TextBox
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Button2 As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents tbValue As System.Windows.Forms.TextBox
End Class
