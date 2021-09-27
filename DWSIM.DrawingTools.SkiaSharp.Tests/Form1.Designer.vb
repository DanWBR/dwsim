<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1

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
        Me.SkControl1 = New SkiaSharp.Views.Desktop.SKControl()
        Me.SuspendLayout()
        '
        'SkControl1
        '
        Me.SkControl1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.SkControl1.Location = New System.Drawing.Point(0, 0)
        Me.SkControl1.Name = "SkControl1"
        Me.SkControl1.Size = New System.Drawing.Size(800, 450)
        Me.SkControl1.TabIndex = 0
        Me.SkControl1.Text = "SkControl1"
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.ClientSize = New System.Drawing.Size(800, 450)
        Me.Controls.Add(Me.SkControl1)
        Me.Name = "Form1"
        Me.Text = "Form1"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents SkControl1 As Global.SkiaSharp.Views.Desktop.SKControl
End Class
