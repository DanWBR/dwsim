<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormCORegistration
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormCORegistration))
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PictureBox1 = New System.Windows.Forms.PictureBox()
        Me.btnUnregisterUser = New System.Windows.Forms.Button()
        Me.btnRegisterUser = New System.Windows.Forms.Button()
        Me.btnUnregisterSystem = New System.Windows.Forms.Button()
        Me.btnRegisterSystem = New System.Windows.Forms.Button()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.Label6 = New System.Windows.Forms.Label()
        Me.Panel1.SuspendLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BackColor = System.Drawing.Color.White
        Me.Panel1.Controls.Add(Me.Label2)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Controls.Add(Me.PictureBox1)
        Me.Panel1.Name = "Panel1"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'PictureBox1
        '
        resources.ApplyResources(Me.PictureBox1, "PictureBox1")
        Me.PictureBox1.Image = Global.DWSIM.My.Resources.Resources.colan
        Me.PictureBox1.Name = "PictureBox1"
        Me.PictureBox1.TabStop = False
        '
        'btnUnregisterUser
        '
        resources.ApplyResources(Me.btnUnregisterUser, "btnUnregisterUser")
        Me.btnUnregisterUser.Image = Global.DWSIM.My.Resources.Resources.user_ninja
        Me.btnUnregisterUser.Name = "btnUnregisterUser"
        Me.btnUnregisterUser.UseVisualStyleBackColor = True
        '
        'btnRegisterUser
        '
        resources.ApplyResources(Me.btnRegisterUser, "btnRegisterUser")
        Me.btnRegisterUser.Image = Global.DWSIM.My.Resources.Resources.user_ninja
        Me.btnRegisterUser.Name = "btnRegisterUser"
        Me.btnRegisterUser.UseVisualStyleBackColor = True
        '
        'btnUnregisterSystem
        '
        resources.ApplyResources(Me.btnUnregisterSystem, "btnUnregisterSystem")
        Me.btnUnregisterSystem.Image = Global.DWSIM.My.Resources.Resources.set_security_question
        Me.btnUnregisterSystem.Name = "btnUnregisterSystem"
        Me.btnUnregisterSystem.UseVisualStyleBackColor = True
        '
        'btnRegisterSystem
        '
        resources.ApplyResources(Me.btnRegisterSystem, "btnRegisterSystem")
        Me.btnRegisterSystem.Image = Global.DWSIM.My.Resources.Resources.set_security_question
        Me.btnRegisterSystem.Name = "btnRegisterSystem"
        Me.btnRegisterSystem.UseVisualStyleBackColor = True
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'Label6
        '
        resources.ApplyResources(Me.Label6, "Label6")
        Me.Label6.Name = "Label6"
        '
        'FormCORegistration
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.Label6)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.btnUnregisterUser)
        Me.Controls.Add(Me.btnRegisterUser)
        Me.Controls.Add(Me.btnUnregisterSystem)
        Me.Controls.Add(Me.btnRegisterSystem)
        Me.Controls.Add(Me.Panel1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormCORegistration"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        CType(Me.PictureBox1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents PictureBox1 As System.Windows.Forms.PictureBox
    Friend WithEvents btnRegisterSystem As System.Windows.Forms.Button
    Friend WithEvents btnUnregisterSystem As System.Windows.Forms.Button
    Friend WithEvents btnRegisterUser As System.Windows.Forms.Button
    Friend WithEvents btnUnregisterUser As System.Windows.Forms.Button
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents Label5 As System.Windows.Forms.Label
    Friend WithEvents Label6 As System.Windows.Forms.Label
End Class
