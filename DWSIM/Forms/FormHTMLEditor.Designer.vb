<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormHTMLEditor
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormHTMLEditor))
        Me.Editor1 = New LiveSwitch.TextControl.Editor()
        Me.SuspendLayout()
        '
        'Editor1
        '
        Me.Editor1.BodyBackgroundColor = System.Drawing.Color.White
        Me.Editor1.BodyHtml = Nothing
        Me.Editor1.BodyText = Nothing
        Me.Editor1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Editor1.DocumentText = resources.GetString("Editor1.DocumentText")
        Me.Editor1.EditorBackColor = System.Drawing.Color.FromArgb(CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer), CType(CType(255, Byte), Integer))
        Me.Editor1.EditorForeColor = System.Drawing.Color.FromArgb(CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer), CType(CType(0, Byte), Integer))
        Me.Editor1.FontSize = LiveSwitch.TextControl.FontSize.Three
        Me.Editor1.Html = Nothing
        Me.Editor1.Location = New System.Drawing.Point(0, 0)
        Me.Editor1.Name = "Editor1"
        Me.Editor1.Size = New System.Drawing.Size(635, 368)
        Me.Editor1.TabIndex = 0
        '
        'FormHTMLEditor
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(635, 368)
        Me.Controls.Add(Me.Editor1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormHTMLEditor"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "HTML Editor"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents Editor1 As LiveSwitch.TextControl.Editor
End Class
