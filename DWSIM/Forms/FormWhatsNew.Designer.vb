Imports Eto.Forms
Imports ICSharpCode.NRefactory.TypeSystem.ReflectionHelper

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormWhatsNew
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
        Me.Viewer = New Microsoft.Web.WebView2.WinForms.WebView2()
        CType(Me.Viewer, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Viewer
        '
        Me.Viewer.AllowExternalDrop = True
        Me.Viewer.CreationProperties = Nothing
        Me.Viewer.DefaultBackgroundColor = System.Drawing.Color.White
        Me.Viewer.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Viewer.Location = New System.Drawing.Point(0, 0)
        Me.Viewer.Name = "Viewer"
        Me.Viewer.Size = New System.Drawing.Size(784, 561)
        Me.Viewer.TabIndex = 1
        Me.Viewer.ZoomFactor = 1.0R
        '
        'FormWhatsNew
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(784, 561)
        Me.Controls.Add(Me.Viewer)
        Me.Icon = Global.DWSIM.My.Resources.Resources.DWSIM_Icon_41
        Me.Name = "FormWhatsNew"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "What's New in DWSIM"
        Me.TopMost = True
        CType(Me.Viewer, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents Viewer As Microsoft.Web.WebView2.WinForms.WebView2
End Class
