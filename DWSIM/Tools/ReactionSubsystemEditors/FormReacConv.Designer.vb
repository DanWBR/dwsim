<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormReacConv
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormReacConv))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.KryptonDataGridView1 = New System.Windows.Forms.DataGridView()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.CheckID = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.tbPhase = New System.Windows.Forms.ComboBox()
        Me.KryptonLabel1 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tbEquation = New System.Windows.Forms.TextBox()
        Me.KryptonLabel3 = New System.Windows.Forms.Label()
        Me.KryptonButton2 = New System.Windows.Forms.Button()
        Me.tbReacHeat = New System.Windows.Forms.TextBox()
        Me.KryptonLabel2 = New System.Windows.Forms.Label()
        Me.tbStoich = New System.Windows.Forms.TextBox()
        Me.tbCompBase = New System.Windows.Forms.TextBox()
        Me.KryptonLabel4 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.KryptonLabel7 = New System.Windows.Forms.Label()
        Me.tbExp = New System.Windows.Forms.TextBox()
        Me.KryptonLabel6 = New System.Windows.Forms.Label()
        Me.KryptonLabel5 = New System.Windows.Forms.Label()
        Me.KryptonButton3 = New System.Windows.Forms.Button()
        Me.KryptonButton4 = New System.Windows.Forms.Button()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.KryptonLabel8 = New System.Windows.Forms.Label()
        Me.KryptonLabel9 = New System.Windows.Forms.Label()
        Me.tbName = New System.Windows.Forms.TextBox()
        Me.tbDesc = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        CType(Me.KryptonDataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.GroupBox3.SuspendLayout()
        Me.SuspendLayout()
        '
        'KryptonDataGridView1
        '
        Me.KryptonDataGridView1.AllowUserToAddRows = False
        Me.KryptonDataGridView1.AllowUserToDeleteRows = False
        Me.KryptonDataGridView1.AllowUserToResizeRows = False
        Me.KryptonDataGridView1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.KryptonDataGridView1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.KryptonDataGridView1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.Column2, Me.Column3, Me.Column7, Me.Column4, Me.Column5, Me.Column6, Me.Column1, Me.CheckID})
        resources.ApplyResources(Me.KryptonDataGridView1, "KryptonDataGridView1")
        Me.KryptonDataGridView1.MultiSelect = False
        Me.KryptonDataGridView1.Name = "KryptonDataGridView1"
        Me.KryptonDataGridView1.RowHeadersVisible = False
        Me.KryptonDataGridView1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'Column2
        '
        Me.Column2.FillWeight = 40.0!
        resources.ApplyResources(Me.Column2, "Column2")
        Me.Column2.Name = "Column2"
        Me.Column2.ReadOnly = True
        '
        'Column3
        '
        Me.Column3.FillWeight = 20.0!
        resources.ApplyResources(Me.Column3, "Column3")
        Me.Column3.Name = "Column3"
        Me.Column3.ReadOnly = True
        '
        'Column7
        '
        Me.Column7.FillWeight = 20.0!
        resources.ApplyResources(Me.Column7, "Column7")
        Me.Column7.Name = "Column7"
        Me.Column7.ReadOnly = True
        '
        'Column4
        '
        Me.Column4.FillWeight = 10.0!
        resources.ApplyResources(Me.Column4, "Column4")
        Me.Column4.Name = "Column4"
        '
        'Column5
        '
        Me.Column5.FillWeight = 10.0!
        resources.ApplyResources(Me.Column5, "Column5")
        Me.Column5.Name = "Column5"
        Me.Column5.Resizable = System.Windows.Forms.DataGridViewTriState.[False]
        '
        'Column6
        '
        DataGridViewCellStyle1.NullValue = "0"
        Me.Column6.DefaultCellStyle = DataGridViewCellStyle1
        Me.Column6.FillWeight = 20.0!
        resources.ApplyResources(Me.Column6, "Column6")
        Me.Column6.Name = "Column6"
        '
        'Column1
        '
        resources.ApplyResources(Me.Column1, "Column1")
        Me.Column1.Name = "Column1"
        Me.Column1.ReadOnly = True
        '
        'CheckID
        '
        resources.ApplyResources(Me.CheckID, "CheckID")
        Me.CheckID.Name = "CheckID"
        '
        'tbPhase
        '
        Me.tbPhase.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tbPhase.DropDownWidth = 121
        Me.tbPhase.Items.AddRange(New Object() {resources.GetString("tbPhase.Items"), resources.GetString("tbPhase.Items1"), resources.GetString("tbPhase.Items2")})
        resources.ApplyResources(Me.tbPhase, "tbPhase")
        Me.tbPhase.Name = "tbPhase"
        '
        'KryptonLabel1
        '
        resources.ApplyResources(Me.KryptonLabel1, "KryptonLabel1")
        Me.KryptonLabel1.Name = "KryptonLabel1"
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.tbEquation)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel3)
        Me.GroupBox1.Controls.Add(Me.KryptonButton2)
        Me.GroupBox1.Controls.Add(Me.tbReacHeat)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel2)
        Me.GroupBox1.Controls.Add(Me.tbStoich)
        Me.GroupBox1.Controls.Add(Me.KryptonDataGridView1)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel1)
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'tbEquation
        '
        resources.ApplyResources(Me.tbEquation, "tbEquation")
        Me.tbEquation.Name = "tbEquation"
        Me.tbEquation.ReadOnly = True
        '
        'KryptonLabel3
        '
        resources.ApplyResources(Me.KryptonLabel3, "KryptonLabel3")
        Me.KryptonLabel3.Name = "KryptonLabel3"
        '
        'KryptonButton2
        '
        resources.ApplyResources(Me.KryptonButton2, "KryptonButton2")
        Me.KryptonButton2.Name = "KryptonButton2"
        '
        'tbReacHeat
        '
        resources.ApplyResources(Me.tbReacHeat, "tbReacHeat")
        Me.tbReacHeat.Name = "tbReacHeat"
        Me.tbReacHeat.ReadOnly = True
        '
        'KryptonLabel2
        '
        resources.ApplyResources(Me.KryptonLabel2, "KryptonLabel2")
        Me.KryptonLabel2.Name = "KryptonLabel2"
        '
        'tbStoich
        '
        resources.ApplyResources(Me.tbStoich, "tbStoich")
        Me.tbStoich.Name = "tbStoich"
        Me.tbStoich.ReadOnly = True
        '
        'tbCompBase
        '
        resources.ApplyResources(Me.tbCompBase, "tbCompBase")
        Me.tbCompBase.Name = "tbCompBase"
        Me.tbCompBase.ReadOnly = True
        '
        'KryptonLabel4
        '
        resources.ApplyResources(Me.KryptonLabel4, "KryptonLabel4")
        Me.KryptonLabel4.Name = "KryptonLabel4"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.KryptonLabel7)
        Me.GroupBox2.Controls.Add(Me.tbExp)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel6)
        Me.GroupBox2.Controls.Add(Me.tbCompBase)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel5)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel4)
        Me.GroupBox2.Controls.Add(Me.tbPhase)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'KryptonLabel7
        '
        resources.ApplyResources(Me.KryptonLabel7, "KryptonLabel7")
        Me.KryptonLabel7.Name = "KryptonLabel7"
        '
        'tbExp
        '
        resources.ApplyResources(Me.tbExp, "tbExp")
        Me.tbExp.Name = "tbExp"
        '
        'KryptonLabel6
        '
        resources.ApplyResources(Me.KryptonLabel6, "KryptonLabel6")
        Me.KryptonLabel6.Name = "KryptonLabel6"
        '
        'KryptonLabel5
        '
        resources.ApplyResources(Me.KryptonLabel5, "KryptonLabel5")
        Me.KryptonLabel5.Name = "KryptonLabel5"
        '
        'KryptonButton3
        '
        Me.KryptonButton3.DialogResult = System.Windows.Forms.DialogResult.Cancel
        resources.ApplyResources(Me.KryptonButton3, "KryptonButton3")
        Me.KryptonButton3.Name = "KryptonButton3"
        '
        'KryptonButton4
        '
        resources.ApplyResources(Me.KryptonButton4, "KryptonButton4")
        Me.KryptonButton4.Name = "KryptonButton4"
        '
        'GroupBox3
        '
        Me.GroupBox3.Controls.Add(Me.KryptonLabel8)
        Me.GroupBox3.Controls.Add(Me.KryptonLabel9)
        Me.GroupBox3.Controls.Add(Me.tbName)
        Me.GroupBox3.Controls.Add(Me.tbDesc)
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'KryptonLabel8
        '
        resources.ApplyResources(Me.KryptonLabel8, "KryptonLabel8")
        Me.KryptonLabel8.Name = "KryptonLabel8"
        '
        'KryptonLabel9
        '
        resources.ApplyResources(Me.KryptonLabel9, "KryptonLabel9")
        Me.KryptonLabel9.Name = "KryptonLabel9"
        '
        'tbName
        '
        resources.ApplyResources(Me.tbName, "tbName")
        Me.tbName.Name = "tbName"
        '
        'tbDesc
        '
        resources.ApplyResources(Me.tbDesc, "tbDesc")
        Me.tbDesc.Name = "tbDesc"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'FormReacConv
        '
        Me.AcceptButton = Me.KryptonButton4
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.CancelButton = Me.KryptonButton3
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.GroupBox3)
        Me.Controls.Add(Me.KryptonButton4)
        Me.Controls.Add(Me.KryptonButton3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormReacConv"
        CType(Me.KryptonDataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents KryptonDataGridView1 As System.Windows.Forms.DataGridView
    Public WithEvents tbPhase As System.Windows.Forms.ComboBox
    Public WithEvents KryptonLabel1 As System.Windows.Forms.Label
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents tbReacHeat As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel2 As System.Windows.Forms.Label
    Public WithEvents tbStoich As System.Windows.Forms.TextBox
    Public WithEvents tbEquation As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel3 As System.Windows.Forms.Label
    Public WithEvents KryptonButton2 As System.Windows.Forms.Button
    Public WithEvents tbCompBase As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel4 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents KryptonLabel5 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel6 As System.Windows.Forms.Label
    Public WithEvents tbExp As System.Windows.Forms.TextBox

    Public WithEvents KryptonButton3 As System.Windows.Forms.Button
    Public WithEvents KryptonButton4 As System.Windows.Forms.Button
    Public WithEvents KryptonLabel7 As System.Windows.Forms.Label
    Public WithEvents GroupBox3 As System.Windows.Forms.GroupBox
    Public WithEvents KryptonLabel8 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel9 As System.Windows.Forms.Label
    Public WithEvents tbName As System.Windows.Forms.TextBox
    Public WithEvents tbDesc As System.Windows.Forms.TextBox
    Public WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Column2 As DataGridViewTextBoxColumn
    Friend WithEvents Column3 As DataGridViewTextBoxColumn
    Friend WithEvents Column7 As DataGridViewTextBoxColumn
    Friend WithEvents Column4 As DataGridViewCheckBoxColumn
    Friend WithEvents Column5 As DataGridViewCheckBoxColumn
    Friend WithEvents Column6 As DataGridViewTextBoxColumn
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents CheckID As DataGridViewTextBoxColumn
End Class
