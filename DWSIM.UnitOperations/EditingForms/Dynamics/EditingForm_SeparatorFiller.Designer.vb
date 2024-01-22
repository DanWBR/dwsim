<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_SeparatorFiller
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(EditingForm_SeparatorFiller))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbStreams = New System.Windows.Forms.ComboBox()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.tbPressure = New System.Windows.Forms.TextBox()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.tbResults = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.lblVessel = New System.Windows.Forms.Label()
        Me.lblPressure = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbStreams
        '
        resources.ApplyResources(Me.cbStreams, "cbStreams")
        Me.cbStreams.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbStreams.FormattingEnabled = True
        Me.cbStreams.Name = "cbStreams"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'tbPressure
        '
        resources.ApplyResources(Me.tbPressure, "tbPressure")
        Me.tbPressure.Name = "tbPressure"
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'tbResults
        '
        resources.ApplyResources(Me.tbResults, "tbResults")
        Me.tbResults.Name = "tbResults"
        Me.tbResults.ReadOnly = True
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
        'lblVessel
        '
        resources.ApplyResources(Me.lblVessel, "lblVessel")
        Me.lblVessel.Name = "lblVessel"
        '
        'lblPressure
        '
        resources.ApplyResources(Me.lblPressure, "lblPressure")
        Me.lblPressure.Name = "lblPressure"
        '
        'EditingForm_SeparatorFiller
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.lblPressure)
        Me.Controls.Add(Me.lblVessel)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.tbResults)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.tbPressure)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.cbStreams)
        Me.Controls.Add(Me.Label1)
        Me.DoubleBuffered = True
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "EditingForm_SeparatorFiller"
        Me.ShowIcon = False
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents Label1 As Label
    Friend WithEvents cbStreams As ComboBox
    Friend WithEvents Button1 As Button
    Friend WithEvents tbPressure As TextBox
    Friend WithEvents Button2 As Button
    Friend WithEvents tbResults As TextBox
    Friend WithEvents Label2 As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents lblVessel As Label
    Friend WithEvents lblPressure As Label
End Class
