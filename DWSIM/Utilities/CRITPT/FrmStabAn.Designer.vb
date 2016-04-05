<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FrmStabAn
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
        Me.components = New System.ComponentModel.Container
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FrmStabAn))
        Me.GroupBox6 = New System.Windows.Forms.GroupBox
        Me.TextBox2 = New System.Windows.Forms.TextBox
        Me.Label2 = New System.Windows.Forms.Label
        Me.Label1 = New System.Windows.Forms.Label
        Me.TextBox1 = New System.Windows.Forms.TextBox
        Me.LblSelected = New System.Windows.Forms.Label
        Me.Button1 = New System.Windows.Forms.Button
        Me.ComboBox3 = New System.Windows.Forms.ComboBox
        Me.Label23 = New System.Windows.Forms.Label
        Me.GroupBox2 = New System.Windows.Forms.GroupBox
        Me.GraphPvap = New ZedGraph.ZedGraphControl
        Me.Label6 = New System.Windows.Forms.Label
        Me.PictureBox1 = New System.Windows.Forms.PictureBox
        Me.LabelWithDivider1 = New System.Windows.Forms.LabelWithDivider
        Me.GroupBox6.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupBox6
        '
        Me.GroupBox6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox6.Controls.Add(Me.TextBox2)
        Me.GroupBox6.Controls.Add(Me.Label2)
        Me.GroupBox6.Controls.Add(Me.Label1)
        Me.GroupBox6.Controls.Add(Me.TextBox1)
        Me.GroupBox6.Controls.Add(Me.LblSelected)
        Me.GroupBox6.Controls.Add(Me.Button1)
        Me.GroupBox6.Controls.Add(Me.ComboBox3)
        Me.GroupBox6.Controls.Add(Me.Label23)
        Me.GroupBox6.Location = New System.Drawing.Point(4, 3)
        Me.GroupBox6.Name = "GroupBox6"
        Me.GroupBox6.Size = New System.Drawing.Size(476, 71)
        Me.GroupBox6.TabIndex = 9
        Me.GroupBox6.TabStop = False
        Me.GroupBox6.Text = "Configurar"
        '
        'TextBox2
        '
        Me.TextBox2.Location = New System.Drawing.Point(216, 42)
        Me.TextBox2.Name = "TextBox2"
        Me.TextBox2.Size = New System.Drawing.Size(35, 20)
        Me.TextBox2.TabIndex = 7
        Me.TextBox2.Text = "200"
        Me.TextBox2.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'Label2
        '
        Me.Label2.AutoSize = True
        Me.Label2.Location = New System.Drawing.Point(170, 45)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(40, 13)
        Me.Label2.TabIndex = 6
        Me.Label2.Text = "Pontos"
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(6, 45)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(117, 13)
        Me.Label1.TabIndex = 5
        Me.Label1.Text = "Multipl. Volume Maximo"
        '
        'TextBox1
        '
        Me.TextBox1.Location = New System.Drawing.Point(129, 42)
        Me.TextBox1.Name = "TextBox1"
        Me.TextBox1.Size = New System.Drawing.Size(35, 20)
        Me.TextBox1.TabIndex = 4
        Me.TextBox1.Text = "15"
        Me.TextBox1.TextAlign = System.Windows.Forms.HorizontalAlignment.Right
        '
        'LblSelected
        '
        Me.LblSelected.AutoSize = True
        Me.LblSelected.Location = New System.Drawing.Point(325, 22)
        Me.LblSelected.Name = "LblSelected"
        Me.LblSelected.Size = New System.Drawing.Size(0, 13)
        Me.LblSelected.TabIndex = 3
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(259, 15)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(87, 23)
        Me.Button1.TabIndex = 2
        Me.Button1.Text = "Calcular"
        '
        'ComboBox3
        '
        Me.ComboBox3.DropDownStyle = ComboBoxStyle.DropDownList
        Me.ComboBox3.FormattingEnabled = True
        Me.ComboBox3.Location = New System.Drawing.Point(112, 16)
        Me.ComboBox3.Name = "ComboBox3"
        Me.ComboBox3.Size = New System.Drawing.Size(139, 21)
        Me.ComboBox3.TabIndex = 1
        '
        'Label23
        '
        Me.Label23.AutoSize = True
        Me.Label23.Location = New System.Drawing.Point(6, 22)
        Me.Label23.Name = "Label23"
        Me.Label23.Size = New System.Drawing.Size(100, 13)
        Me.Label23.TabIndex = 0
        Me.Label23.Text = "Corrente de Materia"
        '
        'GroupBox2
        '
        Me.GroupBox2.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
                    Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.GroupBox2.Controls.Add(Me.GraphPvap)
        Me.GroupBox2.Location = New System.Drawing.Point(4, 76)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.Size = New System.Drawing.Size(476, 323)
        Me.GroupBox2.TabIndex = 8
        Me.GroupBox2.TabStop = False
        Me.GroupBox2.Text = "Grafico"
        '
        'GraphPvap
        '
        Me.GraphPvap.BorderStyle = System.Windows.Forms.BorderStyle.Fixed3D
        Me.GraphPvap.Dock = System.Windows.Forms.DockStyle.Fill
        Me.GraphPvap.Font = New System.Drawing.Font("Arial", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.GraphPvap.IsAntiAlias = True
        Me.GraphPvap.IsAutoScrollRange = True
        Me.GraphPvap.Location = New System.Drawing.Point(3, 16)
        Me.GraphPvap.Name = "GraphPvap"
        Me.GraphPvap.ScrollGrace = 0
        Me.GraphPvap.ScrollMaxX = 0
        Me.GraphPvap.ScrollMaxY = 0
        Me.GraphPvap.ScrollMaxY2 = 0
        Me.GraphPvap.ScrollMinX = 0
        Me.GraphPvap.ScrollMinY = 0
        Me.GraphPvap.ScrollMinY2 = 0
        Me.GraphPvap.Size = New System.Drawing.Size(470, 304)
        Me.GraphPvap.TabIndex = 9
        '
        'Label6
        '
        Me.Label6.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Label6.Location = New System.Drawing.Point(1, 426)
        Me.Label6.Name = "Label6"
        Me.Label6.Size = New System.Drawing.Size(470, 59)
        Me.Label6.TabIndex = 13
        Me.Label6.Text = resources.GetString("Label6.Text")
        '
        'PictureBox1
        '
        Me.PictureBox1.Anchor = CType((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left), System.Windows.Forms.AnchorStyles)
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.exclamation
        Me.PictureBox1.Location = New System.Drawing.Point(4, 407)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(16, 16)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.AutoSize
        Me.PictureBox1.TabIndex = 12
        Me.PictureBox1.TabStop = False
        '
        'LabelWithDivider1
        '
        Me.LabelWithDivider1.Anchor = CType(((System.Windows.Forms.AnchorStyles.Bottom Or System.Windows.Forms.AnchorStyles.Left) _
                    Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.LabelWithDivider1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.LabelWithDivider1.Gap = 10
        Me.LabelWithDivider1.ImageAlign = System.Drawing.ContentAlignment.MiddleLeft
        Me.LabelWithDivider1.Location = New System.Drawing.Point(26, 407)
        Me.LabelWithDivider1.Name = "LabelWithDivider1"
        Me.LabelWithDivider1.Size = New System.Drawing.Size(445, 16)
        Me.LabelWithDivider1.TabIndex = 11
        Me.LabelWithDivider1.Text = "Aviso"
        Me.LabelWithDivider1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'FrmStabAn
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font

        Me.ClientSize = New System.Drawing.Size(483, 485)
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.LabelWithDivider1)
        Me.Controls.Add(Me.GroupBox6)
        Me.Controls.Add(Me.GroupBox2)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FrmStabAn"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.Text = "FrmStabAn"
        Me.GroupBox6.ResumeLayout(False)
        Me.GroupBox6.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Public WithEvents GroupBox6 As System.Windows.Forms.GroupBox
    Public WithEvents LblSelected As System.Windows.Forms.Label
    Public WithEvents Button1 As System.Windows.Forms.Button
    Public WithEvents ComboBox3 As System.Windows.Forms.ComboBox
    Public WithEvents Label23 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents GraphPvap As ZedGraph.ZedGraphControl
    Public WithEvents Label1 As System.Windows.Forms.Label
    Public WithEvents TextBox1 As System.Windows.Forms.TextBox
    Public WithEvents TextBox2 As System.Windows.Forms.TextBox
    Public WithEvents Label2 As System.Windows.Forms.Label
    Public WithEvents Label6 As System.Windows.Forms.Label
    Public WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Public WithEvents LabelWithDivider1 As System.Windows.Forms.LabelWithDivider
End Class
