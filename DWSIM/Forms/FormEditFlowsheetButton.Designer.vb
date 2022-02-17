<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormEditFlowsheetButton
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormEditFlowsheetButton))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbFontSize = New System.Windows.Forms.ComboBox()
        Me.tbDisplayText = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbScriptToRun = New System.Windows.Forms.ComboBox()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbFontSize
        '
        Me.cbFontSize.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbFontSize.FormattingEnabled = True
        Me.cbFontSize.Items.AddRange(New Object() {resources.GetString("cbFontSize.Items"), resources.GetString("cbFontSize.Items1"), resources.GetString("cbFontSize.Items2"), resources.GetString("cbFontSize.Items3"), resources.GetString("cbFontSize.Items4"), resources.GetString("cbFontSize.Items5"), resources.GetString("cbFontSize.Items6"), resources.GetString("cbFontSize.Items7"), resources.GetString("cbFontSize.Items8"), resources.GetString("cbFontSize.Items9"), resources.GetString("cbFontSize.Items10"), resources.GetString("cbFontSize.Items11"), resources.GetString("cbFontSize.Items12"), resources.GetString("cbFontSize.Items13"), resources.GetString("cbFontSize.Items14"), resources.GetString("cbFontSize.Items15"), resources.GetString("cbFontSize.Items16")})
        resources.ApplyResources(Me.cbFontSize, "cbFontSize")
        Me.cbFontSize.Name = "cbFontSize"
        '
        'tbDisplayText
        '
        resources.ApplyResources(Me.tbDisplayText, "tbDisplayText")
        Me.tbDisplayText.Name = "tbDisplayText"
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
        'cbScriptToRun
        '
        Me.cbScriptToRun.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScriptToRun.FormattingEnabled = True
        resources.ApplyResources(Me.cbScriptToRun, "cbScriptToRun")
        Me.cbScriptToRun.Name = "cbScriptToRun"
        '
        'FormEditFlowsheetButton
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.cbScriptToRun)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.tbDisplayText)
        Me.Controls.Add(Me.cbFontSize)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormEditFlowsheetButton"
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents cbFontSize As ComboBox
    Friend WithEvents tbDisplayText As TextBox
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents cbScriptToRun As ComboBox
End Class
