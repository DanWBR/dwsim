<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormEventDescription
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormEventDescription))
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.tbEventType = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Label4 = New System.Windows.Forms.Label()
        Me.tbEventDescription = New System.Windows.Forms.TextBox()
        Me.tbEventLocation = New System.Windows.Forms.TextBox()
        Me.tbEventActions = New System.Windows.Forms.TextBox()
        Me.SuspendLayout()
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'tbEventType
        '
        resources.ApplyResources(Me.tbEventType, "tbEventType")
        Me.tbEventType.Name = "tbEventType"
        Me.tbEventType.ReadOnly = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
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
        'tbEventDescription
        '
        resources.ApplyResources(Me.tbEventDescription, "tbEventDescription")
        Me.tbEventDescription.Name = "tbEventDescription"
        Me.tbEventDescription.ReadOnly = True
        '
        'tbEventLocation
        '
        resources.ApplyResources(Me.tbEventLocation, "tbEventLocation")
        Me.tbEventLocation.Name = "tbEventLocation"
        Me.tbEventLocation.ReadOnly = True
        '
        'tbEventActions
        '
        resources.ApplyResources(Me.tbEventActions, "tbEventActions")
        Me.tbEventActions.Name = "tbEventActions"
        Me.tbEventActions.ReadOnly = True
        '
        'FormEventDescription
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.Controls.Add(Me.tbEventActions)
        Me.Controls.Add(Me.tbEventLocation)
        Me.Controls.Add(Me.tbEventDescription)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.tbEventType)
        Me.Controls.Add(Me.Label1)
        Me.Controls.Add(Me.Button1)
        Me.Name = "FormEventDescription"
        Me.ShowIcon = False
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Button1 As System.Windows.Forms.Button
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents tbEventType As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents tbEventDescription As System.Windows.Forms.TextBox
    Friend WithEvents tbEventLocation As System.Windows.Forms.TextBox
    Friend WithEvents tbEventActions As System.Windows.Forms.TextBox
End Class
