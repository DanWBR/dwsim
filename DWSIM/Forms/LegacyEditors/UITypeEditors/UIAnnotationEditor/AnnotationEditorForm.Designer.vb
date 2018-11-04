<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class AnnotationEditorForm
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
        Me.RichTextBoxExtended1 = New Extended.Windows.Forms.RichTextBoxExtended
        Me.SuspendLayout()
        '
        'RichTextBoxExtended1
        '
        Me.RichTextBoxExtended1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.RichTextBoxExtended1.Location = New System.Drawing.Point(0, 0)
        Me.RichTextBoxExtended1.Name = "RichTextBoxExtended1"
        Me.RichTextBoxExtended1.Rtf = "{\rtf1\ansi\ansicpg1252\deff0\deflang1046{\fonttbl{\f0\fnil\fcharset0 Microsoft S" & _
            "ans Serif;}}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "\viewkind4\uc1\pard\f0\fs17\par" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10) & "}" & Global.Microsoft.VisualBasic.ChrW(13) & Global.Microsoft.VisualBasic.ChrW(10)
        Me.RichTextBoxExtended1.ShowOpen = False
        Me.RichTextBoxExtended1.ShowSave = False
        Me.RichTextBoxExtended1.Size = New System.Drawing.Size(419, 211)
        Me.RichTextBoxExtended1.TabIndex = 0
        '
        'AnnotationEditorForm
        '

        Me.ClientSize = New System.Drawing.Size(419, 211)
        Me.Controls.Add(Me.RichTextBoxExtended1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "AnnotationEditorForm"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterParent
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents RichTextBoxExtended1 As Extended.Windows.Forms.RichTextBoxExtended
End Class
