<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormReacHeterog
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormReacHeterog))
        Me.KryptonButton4 = New System.Windows.Forms.Button()
        Me.KryptonButton3 = New System.Windows.Forms.Button()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.btnScriptHelp = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbScripts = New System.Windows.Forms.ComboBox()
        Me.rbAdvKin = New System.Windows.Forms.RadioButton()
        Me.rbBasicKin = New System.Windows.Forms.RadioButton()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.gbExpression = New System.Windows.Forms.GroupBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbDenominator = New System.Windows.Forms.TextBox()
        Me.tbNumerator = New System.Windows.Forms.TextBox()
        Me.KryptonLabel10 = New System.Windows.Forms.Label()
        Me.KryptonLabel5 = New System.Windows.Forms.Label()
        Me.tbTmax = New System.Windows.Forms.TextBox()
        Me.KryptonLabel11 = New System.Windows.Forms.Label()
        Me.cbConcUnit = New System.Windows.Forms.ComboBox()
        Me.tbTmin = New System.Windows.Forms.TextBox()
        Me.KryptonLabel12 = New System.Windows.Forms.Label()
        Me.KryptonLabel8 = New System.Windows.Forms.Label()
        Me.cbVelUnit = New System.Windows.Forms.ComboBox()
        Me.KryptonLabel7 = New System.Windows.Forms.Label()
        Me.KryptonLabel4 = New System.Windows.Forms.Label()
        Me.cbBase = New System.Windows.Forms.ComboBox()
        Me.KryptonLabel6 = New System.Windows.Forms.Label()
        Me.tbPhase = New System.Windows.Forms.ComboBox()
        Me.tbCompBase = New System.Windows.Forms.TextBox()
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tbEquation = New System.Windows.Forms.TextBox()
        Me.KryptonLabel3 = New System.Windows.Forms.Label()
        Me.KryptonLabel9 = New System.Windows.Forms.Label()
        Me.KryptonButton2 = New System.Windows.Forms.Button()
        Me.tbReacHeat = New System.Windows.Forms.TextBox()
        Me.KryptonLabel2 = New System.Windows.Forms.Label()
        Me.tbStoich = New System.Windows.Forms.TextBox()
        Me.KryptonDataGridView1 = New System.Windows.Forms.DataGridView()
        Me.Column2 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column3 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column7 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column4 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.Column5 = New System.Windows.Forms.DataGridViewCheckBoxColumn()
        Me.Column6 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.Column1 = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.CheckID = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.KryptonLabel1 = New System.Windows.Forms.Label()
        Me.GroupBox4 = New System.Windows.Forms.GroupBox()
        Me.KryptonLabel19 = New System.Windows.Forms.Label()
        Me.KryptonLabel20 = New System.Windows.Forms.Label()
        Me.tbName = New System.Windows.Forms.TextBox()
        Me.tbDesc = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.GroupBox2.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.gbExpression.SuspendLayout()
        Me.GroupBox1.SuspendLayout()
        CType(Me.KryptonDataGridView1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.GroupBox4.SuspendLayout()
        Me.SuspendLayout()
        '
        'KryptonButton4
        '
        Me.KryptonButton4.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        resources.ApplyResources(Me.KryptonButton4, "KryptonButton4")
        Me.KryptonButton4.Name = "KryptonButton4"
        '
        'KryptonButton3
        '
        Me.KryptonButton3.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        resources.ApplyResources(Me.KryptonButton3, "KryptonButton3")
        Me.KryptonButton3.Name = "KryptonButton3"
        '
        'GroupBox2
        '
        Me.GroupBox2.Controls.Add(Me.Panel1)
        Me.GroupBox2.Controls.Add(Me.gbExpression)
        Me.GroupBox2.Controls.Add(Me.tbTmax)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel11)
        Me.GroupBox2.Controls.Add(Me.cbConcUnit)
        Me.GroupBox2.Controls.Add(Me.tbTmin)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel12)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel8)
        Me.GroupBox2.Controls.Add(Me.cbVelUnit)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel7)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel4)
        Me.GroupBox2.Controls.Add(Me.cbBase)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel6)
        Me.GroupBox2.Controls.Add(Me.tbPhase)
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'Panel1
        '
        Me.Panel1.BackColor = System.Drawing.Color.White
        Me.Panel1.Controls.Add(Me.btnScriptHelp)
        Me.Panel1.Controls.Add(Me.Label3)
        Me.Panel1.Controls.Add(Me.cbScripts)
        Me.Panel1.Controls.Add(Me.rbAdvKin)
        Me.Panel1.Controls.Add(Me.rbBasicKin)
        Me.Panel1.Controls.Add(Me.Label4)
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.Name = "Panel1"
        '
        'btnScriptHelp
        '
        Me.btnScriptHelp.BackColor = System.Drawing.SystemColors.ButtonFace
        Me.btnScriptHelp.Image = Global.DWSIM.My.Resources.Resources.help
        resources.ApplyResources(Me.btnScriptHelp, "btnScriptHelp")
        Me.btnScriptHelp.Name = "btnScriptHelp"
        Me.btnScriptHelp.UseVisualStyleBackColor = False
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbScripts
        '
        Me.cbScripts.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScripts.DropDownWidth = 121
        Me.cbScripts.Items.AddRange(New Object() {resources.GetString("cbScripts.Items"), resources.GetString("cbScripts.Items1"), resources.GetString("cbScripts.Items2")})
        resources.ApplyResources(Me.cbScripts, "cbScripts")
        Me.cbScripts.Name = "cbScripts"
        '
        'rbAdvKin
        '
        resources.ApplyResources(Me.rbAdvKin, "rbAdvKin")
        Me.rbAdvKin.Name = "rbAdvKin"
        Me.rbAdvKin.TabStop = True
        Me.rbAdvKin.UseVisualStyleBackColor = True
        '
        'rbBasicKin
        '
        resources.ApplyResources(Me.rbBasicKin, "rbBasicKin")
        Me.rbBasicKin.Name = "rbBasicKin"
        Me.rbBasicKin.TabStop = True
        Me.rbBasicKin.UseVisualStyleBackColor = True
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'gbExpression
        '
        Me.gbExpression.Controls.Add(Me.Label1)
        Me.gbExpression.Controls.Add(Me.tbDenominator)
        Me.gbExpression.Controls.Add(Me.tbNumerator)
        Me.gbExpression.Controls.Add(Me.KryptonLabel10)
        Me.gbExpression.Controls.Add(Me.KryptonLabel5)
        resources.ApplyResources(Me.gbExpression, "gbExpression")
        Me.gbExpression.Name = "gbExpression"
        Me.gbExpression.TabStop = False
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbDenominator
        '
        resources.ApplyResources(Me.tbDenominator, "tbDenominator")
        Me.tbDenominator.Name = "tbDenominator"
        '
        'tbNumerator
        '
        resources.ApplyResources(Me.tbNumerator, "tbNumerator")
        Me.tbNumerator.Name = "tbNumerator"
        '
        'KryptonLabel10
        '
        resources.ApplyResources(Me.KryptonLabel10, "KryptonLabel10")
        Me.KryptonLabel10.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel10.Name = "KryptonLabel10"
        '
        'KryptonLabel5
        '
        resources.ApplyResources(Me.KryptonLabel5, "KryptonLabel5")
        Me.KryptonLabel5.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel5.Name = "KryptonLabel5"
        '
        'tbTmax
        '
        resources.ApplyResources(Me.tbTmax, "tbTmax")
        Me.tbTmax.Name = "tbTmax"
        '
        'KryptonLabel11
        '
        resources.ApplyResources(Me.KryptonLabel11, "KryptonLabel11")
        Me.KryptonLabel11.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel11.Name = "KryptonLabel11"
        '
        'cbConcUnit
        '
        Me.cbConcUnit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbConcUnit.DropDownWidth = 121
        resources.ApplyResources(Me.cbConcUnit, "cbConcUnit")
        Me.cbConcUnit.Name = "cbConcUnit"
        '
        'tbTmin
        '
        resources.ApplyResources(Me.tbTmin, "tbTmin")
        Me.tbTmin.Name = "tbTmin"
        '
        'KryptonLabel12
        '
        resources.ApplyResources(Me.KryptonLabel12, "KryptonLabel12")
        Me.KryptonLabel12.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel12.Name = "KryptonLabel12"
        '
        'KryptonLabel8
        '
        resources.ApplyResources(Me.KryptonLabel8, "KryptonLabel8")
        Me.KryptonLabel8.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel8.Name = "KryptonLabel8"
        '
        'cbVelUnit
        '
        Me.cbVelUnit.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbVelUnit.DropDownWidth = 121
        resources.ApplyResources(Me.cbVelUnit, "cbVelUnit")
        Me.cbVelUnit.Name = "cbVelUnit"
        '
        'KryptonLabel7
        '
        resources.ApplyResources(Me.KryptonLabel7, "KryptonLabel7")
        Me.KryptonLabel7.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel7.Name = "KryptonLabel7"
        '
        'KryptonLabel4
        '
        resources.ApplyResources(Me.KryptonLabel4, "KryptonLabel4")
        Me.KryptonLabel4.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel4.Name = "KryptonLabel4"
        '
        'cbBase
        '
        Me.cbBase.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbBase.DropDownWidth = 121
        resources.ApplyResources(Me.cbBase, "cbBase")
        Me.cbBase.Items.AddRange(New Object() {resources.GetString("cbBase.Items"), resources.GetString("cbBase.Items1"), resources.GetString("cbBase.Items2"), resources.GetString("cbBase.Items3"), resources.GetString("cbBase.Items4"), resources.GetString("cbBase.Items5"), resources.GetString("cbBase.Items6")})
        Me.cbBase.Name = "cbBase"
        '
        'KryptonLabel6
        '
        resources.ApplyResources(Me.KryptonLabel6, "KryptonLabel6")
        Me.KryptonLabel6.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel6.Name = "KryptonLabel6"
        '
        'tbPhase
        '
        Me.tbPhase.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.tbPhase.DropDownWidth = 121
        Me.tbPhase.Items.AddRange(New Object() {resources.GetString("tbPhase.Items"), resources.GetString("tbPhase.Items1"), resources.GetString("tbPhase.Items2"), resources.GetString("tbPhase.Items3")})
        resources.ApplyResources(Me.tbPhase, "tbPhase")
        Me.tbPhase.Name = "tbPhase"
        '
        'tbCompBase
        '
        resources.ApplyResources(Me.tbCompBase, "tbCompBase")
        Me.tbCompBase.Name = "tbCompBase"
        Me.tbCompBase.ReadOnly = True
        '
        'GroupBox1
        '
        Me.GroupBox1.Controls.Add(Me.tbEquation)
        Me.GroupBox1.Controls.Add(Me.tbCompBase)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel3)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel9)
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
        Me.KryptonLabel3.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel3.Name = "KryptonLabel3"
        '
        'KryptonLabel9
        '
        resources.ApplyResources(Me.KryptonLabel9, "KryptonLabel9")
        Me.KryptonLabel9.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel9.Name = "KryptonLabel9"
        '
        'KryptonButton2
        '
        Me.KryptonButton2.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
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
        Me.KryptonLabel2.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel2.Name = "KryptonLabel2"
        '
        'tbStoich
        '
        resources.ApplyResources(Me.tbStoich, "tbStoich")
        Me.tbStoich.Name = "tbStoich"
        Me.tbStoich.ReadOnly = True
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
        Me.Column2.ToolTipText = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
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
        '
        'Column6
        '
        Me.Column6.FillWeight = 15.0!
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
        'KryptonLabel1
        '
        resources.ApplyResources(Me.KryptonLabel1, "KryptonLabel1")
        Me.KryptonLabel1.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        Me.KryptonLabel1.Name = "KryptonLabel1"
        '
        'GroupBox4
        '
        Me.GroupBox4.Controls.Add(Me.KryptonLabel19)
        Me.GroupBox4.Controls.Add(Me.KryptonLabel20)
        Me.GroupBox4.Controls.Add(Me.tbName)
        Me.GroupBox4.Controls.Add(Me.tbDesc)
        resources.ApplyResources(Me.GroupBox4, "GroupBox4")
        Me.GroupBox4.Name = "GroupBox4"
        Me.GroupBox4.TabStop = False
        '
        'KryptonLabel19
        '
        Me.KryptonLabel19.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        resources.ApplyResources(Me.KryptonLabel19, "KryptonLabel19")
        Me.KryptonLabel19.Name = "KryptonLabel19"
        '
        'KryptonLabel20
        '
        Me.KryptonLabel20.ImageKey = Global.DWSIM.My.Resources.DWSIM.NewVersionAvailable
        resources.ApplyResources(Me.KryptonLabel20, "KryptonLabel20")
        Me.KryptonLabel20.Name = "KryptonLabel20"
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
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'FormReacHeterog
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.GroupBox4)
        Me.Controls.Add(Me.KryptonButton4)
        Me.Controls.Add(Me.KryptonButton3)
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormReacHeterog"
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.gbExpression.ResumeLayout(False)
        Me.gbExpression.PerformLayout()
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        CType(Me.KryptonDataGridView1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.GroupBox4.ResumeLayout(False)
        Me.GroupBox4.PerformLayout()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents KryptonButton4 As System.Windows.Forms.Button
    Public WithEvents KryptonButton3 As System.Windows.Forms.Button
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents gbExpression As System.Windows.Forms.GroupBox
    Public WithEvents tbCompBase As System.Windows.Forms.TextBox
    Public WithEvents tbTmax As System.Windows.Forms.TextBox
    Public WithEvents tbTmin As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel8 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel7 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel4 As System.Windows.Forms.Label
    Public WithEvents cbBase As System.Windows.Forms.ComboBox
    Public WithEvents KryptonLabel6 As System.Windows.Forms.Label
    Public WithEvents tbPhase As System.Windows.Forms.ComboBox
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents tbEquation As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel3 As System.Windows.Forms.Label
    Public WithEvents KryptonButton2 As System.Windows.Forms.Button
    Public WithEvents tbReacHeat As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel2 As System.Windows.Forms.Label
    Public WithEvents tbStoich As System.Windows.Forms.TextBox
    Public WithEvents KryptonDataGridView1 As System.Windows.Forms.DataGridView
    Public WithEvents KryptonLabel1 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel11 As System.Windows.Forms.Label
    Public WithEvents cbConcUnit As System.Windows.Forms.ComboBox
    Public WithEvents KryptonLabel12 As System.Windows.Forms.Label
    Public WithEvents cbVelUnit As System.Windows.Forms.ComboBox
    Public WithEvents KryptonLabel10 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel5 As System.Windows.Forms.Label
    Public WithEvents tbDenominator As System.Windows.Forms.TextBox
    Public WithEvents tbNumerator As System.Windows.Forms.TextBox
    Public WithEvents GroupBox4 As System.Windows.Forms.GroupBox
    Public WithEvents KryptonLabel19 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel20 As System.Windows.Forms.Label
    Public WithEvents tbName As System.Windows.Forms.TextBox
    Public WithEvents tbDesc As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel9 As System.Windows.Forms.Label
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Panel1 As Panel
    Public WithEvents btnScriptHelp As Button
    Public WithEvents Label3 As Label
    Public WithEvents cbScripts As ComboBox
    Friend WithEvents rbAdvKin As RadioButton
    Friend WithEvents rbBasicKin As RadioButton
    Public WithEvents Label4 As Label
    Friend WithEvents Column2 As DataGridViewTextBoxColumn
    Friend WithEvents Column3 As DataGridViewTextBoxColumn
    Friend WithEvents Column7 As DataGridViewTextBoxColumn
    Friend WithEvents Column4 As DataGridViewCheckBoxColumn
    Friend WithEvents Column5 As DataGridViewCheckBoxColumn
    Friend WithEvents Column6 As DataGridViewTextBoxColumn
    Friend WithEvents Column1 As DataGridViewTextBoxColumn
    Friend WithEvents CheckID As DataGridViewTextBoxColumn
End Class
