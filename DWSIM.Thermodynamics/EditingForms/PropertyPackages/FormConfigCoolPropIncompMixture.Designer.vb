<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormConfigCoolPropIncompMixture
    Inherits System.Windows.Forms.Form

    'Form overrides dispose to clean up the component list.
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigCoolPropIncompMixture))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbSolvent = New System.Windows.Forms.ComboBox()
        Me.cbSoluteDWSIM = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.cbSoluteCP = New System.Windows.Forms.ComboBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbSolvent
        '
        resources.ApplyResources(Me.cbSolvent, "cbSolvent")
        Me.cbSolvent.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSolvent.FormattingEnabled = True
        Me.cbSolvent.Name = "cbSolvent"
        '
        'cbSoluteDWSIM
        '
        resources.ApplyResources(Me.cbSoluteDWSIM, "cbSoluteDWSIM")
        Me.cbSoluteDWSIM.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSoluteDWSIM.FormattingEnabled = True
        Me.cbSoluteDWSIM.Name = "cbSoluteDWSIM"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'cbSoluteCP
        '
        resources.ApplyResources(Me.cbSoluteCP, "cbSoluteCP")
        Me.cbSoluteCP.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbSoluteCP.FormattingEnabled = True
        Me.cbSoluteCP.Name = "cbSoluteCP"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'FormConfigCoolPropIncompMixture
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.Controls.Add(Me.cbSoluteCP)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.cbSoluteDWSIM)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.cbSolvent)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormConfigCoolPropIncompMixture"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents cbSolvent As ComboBox
    Friend WithEvents cbSoluteDWSIM As ComboBox
    Friend WithEvents Label2 As Label
    Friend WithEvents cbSoluteCP As ComboBox
    Friend WithEvents Label3 As Label
End Class
