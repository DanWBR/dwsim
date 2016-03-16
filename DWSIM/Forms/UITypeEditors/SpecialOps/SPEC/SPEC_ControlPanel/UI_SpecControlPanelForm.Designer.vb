<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class UI_SpecControlPanelForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(UI_SpecControlPanelForm))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.tbTVVal = New System.Windows.Forms.TextBox()
        Me.tbSVVal = New System.Windows.Forms.TextBox()
        Me.tbSVID = New System.Windows.Forms.TextBox()
        Me.tbTVID = New System.Windows.Forms.TextBox()
        Me.KryptonLabel3 = New System.Windows.Forms.Label()
        Me.KryptonLabel4 = New System.Windows.Forms.Label()
        Me.KryptonLabel2 = New System.Windows.Forms.Label()
        Me.KryptonLabel1 = New System.Windows.Forms.Label()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.lblExpRes = New System.Windows.Forms.Label()
        Me.KryptonLabel7 = New System.Windows.Forms.Label()
        Me.KryptonButton1 = New System.Windows.Forms.Button()
        Me.KryptonLabel6 = New System.Windows.Forms.Label()
        Me.KryptonLabel5 = New System.Windows.Forms.Label()
        Me.tbExp = New System.Windows.Forms.TextBox()
        Me.KryptonLabel8 = New System.Windows.Forms.Label()
        Me.GroupBox1.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.tbTVVal)
        Me.GroupBox1.Controls.Add(Me.tbSVVal)
        Me.GroupBox1.Controls.Add(Me.tbSVID)
        Me.GroupBox1.Controls.Add(Me.tbTVID)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel3)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel4)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel2)
        Me.GroupBox1.Controls.Add(Me.KryptonLabel1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'tbTVVal
        '
        resources.ApplyResources(Me.tbTVVal, "tbTVVal")
        Me.tbTVVal.Name = "tbTVVal"
        Me.tbTVVal.ReadOnly = True
        '
        'tbSVVal
        '
        resources.ApplyResources(Me.tbSVVal, "tbSVVal")
        Me.tbSVVal.Name = "tbSVVal"
        Me.tbSVVal.ReadOnly = True
        '
        'tbSVID
        '
        resources.ApplyResources(Me.tbSVID, "tbSVID")
        Me.tbSVID.Name = "tbSVID"
        Me.tbSVID.ReadOnly = True
        '
        'tbTVID
        '
        resources.ApplyResources(Me.tbTVID, "tbTVID")
        Me.tbTVID.Name = "tbTVID"
        Me.tbTVID.ReadOnly = True
        '
        'KryptonLabel3
        '
        resources.ApplyResources(Me.KryptonLabel3, "KryptonLabel3")
        Me.KryptonLabel3.Name = "KryptonLabel3"
        '
        'KryptonLabel4
        '
        resources.ApplyResources(Me.KryptonLabel4, "KryptonLabel4")
        Me.KryptonLabel4.Name = "KryptonLabel4"
        '
        'KryptonLabel2
        '
        resources.ApplyResources(Me.KryptonLabel2, "KryptonLabel2")
        Me.KryptonLabel2.Name = "KryptonLabel2"
        '
        'KryptonLabel1
        '
        resources.ApplyResources(Me.KryptonLabel1, "KryptonLabel1")
        Me.KryptonLabel1.Name = "KryptonLabel1"
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.lblExpRes)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel7)
        Me.GroupBox2.Controls.Add(Me.KryptonButton1)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel6)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel5)
        Me.GroupBox2.Controls.Add(Me.tbExp)
        Me.GroupBox2.Controls.Add(Me.KryptonLabel8)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'lblExpRes
        '
        resources.ApplyResources(Me.lblExpRes, "lblExpRes")
        Me.lblExpRes.Name = "lblExpRes"
        '
        'KryptonLabel7
        '
        resources.ApplyResources(Me.KryptonLabel7, "KryptonLabel7")
        Me.KryptonLabel7.Name = "KryptonLabel7"
        '
        'KryptonButton1
        '
        resources.ApplyResources(Me.KryptonButton1, "KryptonButton1")
        Me.KryptonButton1.Name = "KryptonButton1"
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
        'tbExp
        '
        resources.ApplyResources(Me.tbExp, "tbExp")
        Me.tbExp.Name = "tbExp"
        '
        'KryptonLabel8
        '
        resources.ApplyResources(Me.KryptonLabel8, "KryptonLabel8")
        Me.KryptonLabel8.Name = "KryptonLabel8"
        '
        'UI_SpecControlPanelForm
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.GroupBox2)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "UI_SpecControlPanelForm"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Public WithEvents KryptonLabel2 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel1 As System.Windows.Forms.Label
    Public WithEvents tbTVVal As System.Windows.Forms.TextBox
    Public WithEvents tbSVVal As System.Windows.Forms.TextBox
    Public WithEvents tbSVID As System.Windows.Forms.TextBox
    Public WithEvents tbTVID As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel3 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel4 As System.Windows.Forms.Label
    Public WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Public WithEvents tbExp As System.Windows.Forms.TextBox
    Public WithEvents KryptonLabel8 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel7 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel6 As System.Windows.Forms.Label
    Public WithEvents KryptonLabel5 As System.Windows.Forms.Label
    Public WithEvents KryptonButton1 As System.Windows.Forms.Button
    Public WithEvents lblExpRes As System.Windows.Forms.Label
End Class
