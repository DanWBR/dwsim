<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class ComprExprCurveSet
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(ComprExprCurveSet))
        Dim DataGridViewCellStyle1 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle2 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Dim DataGridViewCellStyle3 As System.Windows.Forms.DataGridViewCellStyle = New System.Windows.Forms.DataGridViewCellStyle()
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPage1 = New System.Windows.Forms.TabPage()
        Me.Label8 = New System.Windows.Forms.Label()
        Me.Label7 = New System.Windows.Forms.Label()
        Me.tbRotation = New System.Windows.Forms.TextBox()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.ComboBox1 = New System.Windows.Forms.ComboBox()
        Me.ComboBox2 = New System.Windows.Forms.ComboBox()
        Me.CheckBox1 = New System.Windows.Forms.CheckBox()
        Me.dgv4 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.GroupBox3 = New System.Windows.Forms.GroupBox()
        Me.cb8 = New System.Windows.Forms.ComboBox()
        Me.cb3 = New System.Windows.Forms.ComboBox()
        Me.ch3 = New System.Windows.Forms.CheckBox()
        Me.dgv3 = New System.Windows.Forms.DataGridView()
        Me.DataGridViewTextBoxColumn5 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.DataGridViewTextBoxColumn6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Label9 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.cb6 = New System.Windows.Forms.ComboBox()
        Me.dgv1 = New System.Windows.Forms.DataGridView()
        Me.vap = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.temp = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.ch1 = New System.Windows.Forms.CheckBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cb1 = New System.Windows.Forms.ComboBox()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.TabPage2 = New System.Windows.Forms.TabPage()
        Me.chart1 = New ZedGraph.ZedGraphControl()
        Me.TabControl1.SuspendLayout()
        Me.TabPage1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.dgv4, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox3.SuspendLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox1.SuspendLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.TabPage2.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPage1)
        Me.TabControl1.Controls.Add(Me.TabPage2)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPage1
        '
        resources.ApplyResources(Me.TabPage1, "TabPage1")
        Me.TabPage1.Controls.Add(Me.Label8)
        Me.TabPage1.Controls.Add(Me.Label7)
        Me.TabPage1.Controls.Add(Me.tbRotation)
        Me.TabPage1.Controls.Add(Me.GroupBox2)
        Me.TabPage1.Controls.Add(Me.GroupBox3)
        Me.TabPage1.Controls.Add(Me.GroupBox1)
        Me.TabPage1.Name = "TabPage1"
        Me.TabPage1.UseVisualStyleBackColor = True
        '
        'Label8
        '
        resources.ApplyResources(Me.Label8, "Label8")
        Me.Label8.Name = "Label8"
        '
        'Label7
        '
        resources.ApplyResources(Me.Label7, "Label7")
        Me.Label7.Name = "Label7"
        '
        'tbRotation
        '
        resources.ApplyResources(Me.tbRotation, "tbRotation")
        Me.tbRotation.Name = "tbRotation"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.ComboBox1)
        Me.GroupBox2.Controls.Add(Me.ComboBox2)
        Me.GroupBox2.Controls.Add(Me.CheckBox1)
        Me.GroupBox2.Controls.Add(Me.dgv4)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.Label5)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'ComboBox1
        '
        resources.ApplyResources(Me.ComboBox1, "ComboBox1")
        Me.ComboBox1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox1.FormattingEnabled = True
        Me.ComboBox1.Items.AddRange(New Object() {resources.GetString("ComboBox1.Items"), resources.GetString("ComboBox1.Items1"), resources.GetString("ComboBox1.Items2"), resources.GetString("ComboBox1.Items3"), resources.GetString("ComboBox1.Items4"), resources.GetString("ComboBox1.Items5"), resources.GetString("ComboBox1.Items6"), resources.GetString("ComboBox1.Items7"), resources.GetString("ComboBox1.Items8"), resources.GetString("ComboBox1.Items9")})
        Me.ComboBox1.Name = "ComboBox1"
        '
        'ComboBox2
        '
        resources.ApplyResources(Me.ComboBox2, "ComboBox2")
        Me.ComboBox2.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.ComboBox2.FormattingEnabled = True
        Me.ComboBox2.Items.AddRange(New Object() {resources.GetString("ComboBox2.Items"), resources.GetString("ComboBox2.Items1"), resources.GetString("ComboBox2.Items2"), resources.GetString("ComboBox2.Items3"), resources.GetString("ComboBox2.Items4"), resources.GetString("ComboBox2.Items5"), resources.GetString("ComboBox2.Items6"), resources.GetString("ComboBox2.Items7"), resources.GetString("ComboBox2.Items8"), resources.GetString("ComboBox2.Items9"), resources.GetString("ComboBox2.Items10"), resources.GetString("ComboBox2.Items11"), resources.GetString("ComboBox2.Items12"), resources.GetString("ComboBox2.Items13"), resources.GetString("ComboBox2.Items14"), resources.GetString("ComboBox2.Items15"), resources.GetString("ComboBox2.Items16"), resources.GetString("ComboBox2.Items17"), resources.GetString("ComboBox2.Items18"), resources.GetString("ComboBox2.Items19")})
        Me.ComboBox2.Name = "ComboBox2"
        '
        'CheckBox1
        '
        resources.ApplyResources(Me.CheckBox1, "CheckBox1")
        Me.CheckBox1.Name = "CheckBox1"
        Me.CheckBox1.UseVisualStyleBackColor = True
        '
        'dgv4
        '
        resources.ApplyResources(Me.dgv4, "dgv4")
        Me.dgv4.AllowDrop = True
        Me.dgv4.AllowUserToResizeRows = False
        DataGridViewCellStyle1.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv4.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle1
        Me.dgv4.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv4.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv4.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv4.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn1, Me.DataGridViewTextBoxColumn2})
        Me.dgv4.Name = "dgv4"
        Me.dgv4.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv4.RowHeadersVisible = False
        Me.dgv4.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv4.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'DataGridViewTextBoxColumn1
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn1, "DataGridViewTextBoxColumn1")
        Me.DataGridViewTextBoxColumn1.Name = "DataGridViewTextBoxColumn1"
        '
        'DataGridViewTextBoxColumn2
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn2, "DataGridViewTextBoxColumn2")
        Me.DataGridViewTextBoxColumn2.Name = "DataGridViewTextBoxColumn2"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'GroupBox3
        '
        resources.ApplyResources(Me.GroupBox3, "GroupBox3")
        Me.GroupBox3.Controls.Add(Me.cb8)
        Me.GroupBox3.Controls.Add(Me.cb3)
        Me.GroupBox3.Controls.Add(Me.ch3)
        Me.GroupBox3.Controls.Add(Me.dgv3)
        Me.GroupBox3.Controls.Add(Me.Label9)
        Me.GroupBox3.Controls.Add(Me.Label4)
        Me.GroupBox3.Name = "GroupBox3"
        Me.GroupBox3.TabStop = False
        '
        'cb8
        '
        resources.ApplyResources(Me.cb8, "cb8")
        Me.cb8.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb8.FormattingEnabled = True
        Me.cb8.Items.AddRange(New Object() {resources.GetString("cb8.Items"), resources.GetString("cb8.Items1")})
        Me.cb8.Name = "cb8"
        '
        'cb3
        '
        resources.ApplyResources(Me.cb3, "cb3")
        Me.cb3.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb3.FormattingEnabled = True
        Me.cb3.Items.AddRange(New Object() {resources.GetString("cb3.Items"), resources.GetString("cb3.Items1"), resources.GetString("cb3.Items2"), resources.GetString("cb3.Items3"), resources.GetString("cb3.Items4"), resources.GetString("cb3.Items5"), resources.GetString("cb3.Items6"), resources.GetString("cb3.Items7"), resources.GetString("cb3.Items8"), resources.GetString("cb3.Items9"), resources.GetString("cb3.Items10"), resources.GetString("cb3.Items11"), resources.GetString("cb3.Items12"), resources.GetString("cb3.Items13"), resources.GetString("cb3.Items14"), resources.GetString("cb3.Items15"), resources.GetString("cb3.Items16"), resources.GetString("cb3.Items17"), resources.GetString("cb3.Items18"), resources.GetString("cb3.Items19")})
        Me.cb3.Name = "cb3"
        '
        'ch3
        '
        resources.ApplyResources(Me.ch3, "ch3")
        Me.ch3.Name = "ch3"
        Me.ch3.UseVisualStyleBackColor = True
        '
        'dgv3
        '
        resources.ApplyResources(Me.dgv3, "dgv3")
        Me.dgv3.AllowDrop = True
        Me.dgv3.AllowUserToResizeRows = False
        DataGridViewCellStyle2.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv3.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle2
        Me.dgv3.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv3.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv3.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv3.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.DataGridViewTextBoxColumn5, Me.DataGridViewTextBoxColumn6})
        Me.dgv3.Name = "dgv3"
        Me.dgv3.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv3.RowHeadersVisible = False
        Me.dgv3.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv3.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'DataGridViewTextBoxColumn5
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn5, "DataGridViewTextBoxColumn5")
        Me.DataGridViewTextBoxColumn5.Name = "DataGridViewTextBoxColumn5"
        '
        'DataGridViewTextBoxColumn6
        '
        resources.ApplyResources(Me.DataGridViewTextBoxColumn6, "DataGridViewTextBoxColumn6")
        Me.DataGridViewTextBoxColumn6.Name = "DataGridViewTextBoxColumn6"
        '
        'Label9
        '
        resources.ApplyResources(Me.Label9, "Label9")
        Me.Label9.Name = "Label9"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.cb6)
        Me.GroupBox1.Controls.Add(Me.dgv1)
        Me.GroupBox1.Controls.Add(Me.ch1)
        Me.GroupBox1.Controls.Add(Me.Label3)
        Me.GroupBox1.Controls.Add(Me.cb1)
        Me.GroupBox1.Controls.Add(Me.Label6)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'cb6
        '
        resources.ApplyResources(Me.cb6, "cb6")
        Me.cb6.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb6.FormattingEnabled = True
        Me.cb6.Items.AddRange(New Object() {resources.GetString("cb6.Items"), resources.GetString("cb6.Items1"), resources.GetString("cb6.Items2"), resources.GetString("cb6.Items3"), resources.GetString("cb6.Items4")})
        Me.cb6.Name = "cb6"
        '
        'dgv1
        '
        resources.ApplyResources(Me.dgv1, "dgv1")
        Me.dgv1.AllowDrop = True
        Me.dgv1.AllowUserToResizeRows = False
        DataGridViewCellStyle3.BackColor = System.Drawing.Color.WhiteSmoke
        Me.dgv1.AlternatingRowsDefaultCellStyle = DataGridViewCellStyle3
        Me.dgv1.AutoSizeColumnsMode = System.Windows.Forms.DataGridViewAutoSizeColumnsMode.Fill
        Me.dgv1.AutoSizeRowsMode = System.Windows.Forms.DataGridViewAutoSizeRowsMode.AllCells
        Me.dgv1.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.dgv1.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.vap, Me.temp})
        Me.dgv1.Name = "dgv1"
        Me.dgv1.RowHeadersBorderStyle = System.Windows.Forms.DataGridViewHeaderBorderStyle.[Single]
        Me.dgv1.RowHeadersVisible = False
        Me.dgv1.RowHeadersWidthSizeMode = System.Windows.Forms.DataGridViewRowHeadersWidthSizeMode.DisableResizing
        Me.dgv1.SelectionMode = System.Windows.Forms.DataGridViewSelectionMode.CellSelect
        '
        'vap
        '
        resources.ApplyResources(Me.vap, "vap")
        Me.vap.Name = "vap"
        '
        'temp
        '
        resources.ApplyResources(Me.temp, "temp")
        Me.temp.Name = "temp"
        '
        'ch1
        '
        resources.ApplyResources(Me.ch1, "ch1")
        Me.ch1.Name = "ch1"
        Me.ch1.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cb1
        '
        resources.ApplyResources(Me.cb1, "cb1")
        Me.cb1.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cb1.FormattingEnabled = True
        Me.cb1.Items.AddRange(New Object() {resources.GetString("cb1.Items"), resources.GetString("cb1.Items1"), resources.GetString("cb1.Items2"), resources.GetString("cb1.Items3"), resources.GetString("cb1.Items4"), resources.GetString("cb1.Items5"), resources.GetString("cb1.Items6"), resources.GetString("cb1.Items7"), resources.GetString("cb1.Items8"), resources.GetString("cb1.Items9"), resources.GetString("cb1.Items10"), resources.GetString("cb1.Items11"), resources.GetString("cb1.Items12"), resources.GetString("cb1.Items13"), resources.GetString("cb1.Items14"), resources.GetString("cb1.Items15"), resources.GetString("cb1.Items16"), resources.GetString("cb1.Items17"), resources.GetString("cb1.Items18"), resources.GetString("cb1.Items19")})
        Me.cb1.Name = "cb1"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'TabPage2
        '
        resources.ApplyResources(Me.TabPage2, "TabPage2")
        Me.TabPage2.Controls.Add(Me.chart1)
        Me.TabPage2.Name = "TabPage2"
        Me.TabPage2.UseVisualStyleBackColor = True
        '
        'chart1
        '
        resources.ApplyResources(Me.chart1, "chart1")
        Me.chart1.IsAntiAlias = True
        Me.chart1.IsAutoScrollRange = True
        Me.chart1.IsShowCopyMessage = False
        Me.chart1.Name = "chart1"
        Me.chart1.ScrollGrace = 0R
        Me.chart1.ScrollMaxX = 0R
        Me.chart1.ScrollMaxY = 0R
        Me.chart1.ScrollMaxY2 = 0R
        Me.chart1.ScrollMinX = 0R
        Me.chart1.ScrollMinY = 0R
        Me.chart1.ScrollMinY2 = 0R
        '
        'ComprExprCurveSet
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.TabControl1)
        Me.Name = "ComprExprCurveSet"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPage1.ResumeLayout(False)
        Me.TabPage1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        CType(Me.dgv4, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox3.ResumeLayout(False)
        Me.GroupBox3.PerformLayout()
        CType(Me.dgv3, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.dgv1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.TabPage2.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents TabControl1 As TabControl
    Friend WithEvents TabPage1 As TabPage
    Friend WithEvents TabPage2 As TabPage
    Public WithEvents GroupBox2 As GroupBox
    Public WithEvents ComboBox1 As ComboBox
    Public WithEvents ComboBox2 As ComboBox
    Public WithEvents CheckBox1 As CheckBox
    Public WithEvents dgv4 As DataGridView
    Public WithEvents Label2 As Label
    Public WithEvents Label5 As Label
    Public WithEvents GroupBox3 As GroupBox
    Public WithEvents cb8 As ComboBox
    Public WithEvents cb3 As ComboBox
    Public WithEvents ch3 As CheckBox
    Public WithEvents dgv3 As DataGridView
    Public WithEvents Label9 As Label
    Public WithEvents Label4 As Label
    Public WithEvents GroupBox1 As GroupBox
    Public WithEvents cb6 As ComboBox
    Public WithEvents dgv1 As DataGridView
    Public WithEvents ch1 As CheckBox
    Public WithEvents Label3 As Label
    Public WithEvents cb1 As ComboBox
    Public WithEvents Label6 As Label
    Public WithEvents chart1 As ZedGraph.ZedGraphControl
    Friend WithEvents tbRotation As TextBox
    Friend WithEvents Label8 As Label
    Friend WithEvents Label7 As Label
    Friend WithEvents DataGridViewTextBoxColumn1 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn2 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn5 As DataGridViewTextBoxColumn
    Friend WithEvents DataGridViewTextBoxColumn6 As DataGridViewTextBoxColumn
    Friend WithEvents vap As DataGridViewTextBoxColumn
    Friend WithEvents temp As DataGridViewTextBoxColumn
End Class
