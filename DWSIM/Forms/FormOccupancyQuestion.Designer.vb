<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormOccupancyQuestion
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
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.PictureBox2 = New System.Windows.Forms.PictureBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.rbConsultant = New System.Windows.Forms.RadioButton()
        Me.rbStudent = New System.Windows.Forms.RadioButton()
        Me.rbHobbyist = New System.Windows.Forms.RadioButton()
        Me.rbEmployee = New System.Windows.Forms.RadioButton()
        Me.rbOther = New System.Windows.Forms.RadioButton()
        Me.tbOther = New System.Windows.Forms.TextBox()
        Me.rbTeacher = New System.Windows.Forms.RadioButton()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        Me.Label1.AutoSize = True
        Me.Label1.Location = New System.Drawing.Point(53, 21)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(275, 13)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "As a valuable DWSIM user, which role best defines you?"
        '
        'PictureBox1
        '
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.help_browser
        Me.PictureBox1.Location = New System.Drawing.Point(15, 12)
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.Size = New System.Drawing.Size(32, 32)
        Me.PictureBox1.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox1.TabIndex = 1
        Me.PictureBox1.TabStop = False
        '
        'PictureBox2
        '
        Me.PictureBox2.Image = Global.DWSIM.My.Resources.Resources.icons8_ok
        Me.PictureBox2.Location = New System.Drawing.Point(15, 224)
        Me.PictureBox2.Name = "PictureBox2"
        Me.PictureBox2.Size = New System.Drawing.Size(32, 32)
        Me.PictureBox2.SizeMode = System.Windows.Forms.PictureBoxSizeMode.Zoom
        Me.PictureBox2.TabIndex = 2
        Me.PictureBox2.TabStop = False
        '
        'Button1
        '
        Me.Button1.Location = New System.Drawing.Point(378, 230)
        Me.Button1.Name = "Button1"
        Me.Button1.Size = New System.Drawing.Size(75, 23)
        Me.Button1.TabIndex = 3
        Me.Button1.Text = "Send"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label2
        '
        Me.Label2.Location = New System.Drawing.Point(53, 224)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(304, 32)
        Me.Label2.TabIndex = 4
        Me.Label2.Text = "This information will be sent isolatedly, anonymously and cannot be used to ident" &
    "ify you."
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'rbConsultant
        '
        Me.rbConsultant.AutoSize = True
        Me.rbConsultant.Location = New System.Drawing.Point(56, 58)
        Me.rbConsultant.Name = "rbConsultant"
        Me.rbConsultant.Size = New System.Drawing.Size(167, 17)
        Me.rbConsultant.TabIndex = 5
        Me.rbConsultant.TabStop = True
        Me.rbConsultant.Text = "I'm an independent consultant"
        Me.rbConsultant.UseVisualStyleBackColor = True
        '
        'rbStudent
        '
        Me.rbStudent.AutoSize = True
        Me.rbStudent.Location = New System.Drawing.Point(56, 130)
        Me.rbStudent.Name = "rbStudent"
        Me.rbStudent.Size = New System.Drawing.Size(85, 17)
        Me.rbStudent.TabIndex = 6
        Me.rbStudent.TabStop = True
        Me.rbStudent.Text = "I'm a student"
        Me.rbStudent.UseVisualStyleBackColor = True
        '
        'rbHobbyist
        '
        Me.rbHobbyist.AutoSize = True
        Me.rbHobbyist.Location = New System.Drawing.Point(56, 154)
        Me.rbHobbyist.Name = "rbHobbyist"
        Me.rbHobbyist.Size = New System.Drawing.Size(142, 17)
        Me.rbHobbyist.TabIndex = 7
        Me.rbHobbyist.TabStop = True
        Me.rbHobbyist.Text = "I'm a hobbyist/enthusiast"
        Me.rbHobbyist.UseVisualStyleBackColor = True
        '
        'rbEmployee
        '
        Me.rbEmployee.AutoSize = True
        Me.rbEmployee.Location = New System.Drawing.Point(56, 82)
        Me.rbEmployee.Name = "rbEmployee"
        Me.rbEmployee.Size = New System.Drawing.Size(141, 17)
        Me.rbEmployee.TabIndex = 8
        Me.rbEmployee.TabStop = True
        Me.rbEmployee.Text = "I'm a company employee"
        Me.rbEmployee.UseVisualStyleBackColor = True
        '
        'rbOther
        '
        Me.rbOther.AutoSize = True
        Me.rbOther.Location = New System.Drawing.Point(56, 178)
        Me.rbOther.Name = "rbOther"
        Me.rbOther.Size = New System.Drawing.Size(130, 17)
        Me.rbOther.TabIndex = 9
        Me.rbOther.TabStop = True
        Me.rbOther.Text = "Other (please specify):"
        Me.rbOther.UseVisualStyleBackColor = True
        '
        'tbOther
        '
        Me.tbOther.Location = New System.Drawing.Point(192, 177)
        Me.tbOther.Name = "tbOther"
        Me.tbOther.Size = New System.Drawing.Size(165, 20)
        Me.tbOther.TabIndex = 10
        '
        'rbTeacher
        '
        Me.rbTeacher.AutoSize = True
        Me.rbTeacher.Location = New System.Drawing.Point(56, 106)
        Me.rbTeacher.Name = "rbTeacher"
        Me.rbTeacher.Size = New System.Drawing.Size(86, 17)
        Me.rbTeacher.TabIndex = 11
        Me.rbTeacher.TabStop = True
        Me.rbTeacher.Text = "I'm a teacher"
        Me.rbTeacher.UseVisualStyleBackColor = True
        '
        'FormOccupancyQuestion
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(473, 272)
        Me.Controls.Add(Me.rbTeacher)
        Me.Controls.Add(Me.tbOther)
        Me.Controls.Add(Me.rbOther)
        Me.Controls.Add(Me.rbEmployee)
        Me.Controls.Add(Me.rbHobbyist)
        Me.Controls.Add(Me.rbStudent)
        Me.Controls.Add(Me.rbConsultant)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.PictureBox2)
        Me.Controls.Add(Me.PictureBox1)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormOccupancyQuestion"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Quick Question"
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        CType(Me.PictureBox2, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents PictureBox1 As PictureBox
    Friend WithEvents PictureBox2 As PictureBox
    Friend WithEvents Button1 As Button
    Friend WithEvents Label2 As Label
    Friend WithEvents rbConsultant As RadioButton
    Friend WithEvents rbStudent As RadioButton
    Friend WithEvents rbHobbyist As RadioButton
    Friend WithEvents rbEmployee As RadioButton
    Friend WithEvents rbOther As RadioButton
    Friend WithEvents tbOther As TextBox
    Friend WithEvents rbTeacher As RadioButton
End Class
