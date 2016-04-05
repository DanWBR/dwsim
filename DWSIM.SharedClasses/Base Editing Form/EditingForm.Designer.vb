<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

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
        Me.components = New System.ComponentModel.Container()
        Me.ResizeButton = New System.Windows.Forms.Button()
        Me.Contents = New System.Windows.Forms.FlowLayoutPanel()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.CheckSticky = New System.Windows.Forms.CheckBox()
        Me.SuspendLayout()
        '
        'ResizeButton
        '
        Me.ResizeButton.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.ResizeButton.BackgroundImage = Global.DWSIM.SharedClasses.My.Resources.Resources.canvas_size
        Me.ResizeButton.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
        Me.ResizeButton.Location = New System.Drawing.Point(532, 5)
        Me.ResizeButton.Name = "ResizeButton"
        Me.ResizeButton.Size = New System.Drawing.Size(30, 30)
        Me.ResizeButton.TabIndex = 0
        Me.ToolTip1.SetToolTip(Me.ResizeButton, "Resize")
        Me.ResizeButton.UseVisualStyleBackColor = True
        '
        'Contents
        '
        Me.Contents.Anchor = CType((((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Bottom) _
            Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.Contents.Location = New System.Drawing.Point(4, 5)
        Me.Contents.Name = "Contents"
        Me.Contents.Size = New System.Drawing.Size(522, 392)
        Me.Contents.TabIndex = 1
        '
        'CheckSticky
        '
        Me.CheckSticky.Anchor = CType((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.CheckSticky.Appearance = System.Windows.Forms.Appearance.Button
        Me.CheckSticky.BackgroundImage = Global.DWSIM.SharedClasses.My.Resources.Resources.location_pin
        Me.CheckSticky.BackgroundImageLayout = System.Windows.Forms.ImageLayout.Stretch
        Me.CheckSticky.Checked = True
        Me.CheckSticky.CheckState = System.Windows.Forms.CheckState.Checked
        Me.CheckSticky.Location = New System.Drawing.Point(532, 41)
        Me.CheckSticky.Name = "CheckSticky"
        Me.CheckSticky.Size = New System.Drawing.Size(30, 30)
        Me.CheckSticky.TabIndex = 2
        Me.ToolTip1.SetToolTip(Me.CheckSticky, "Pin (keep window open)")
        Me.CheckSticky.UseVisualStyleBackColor = True
        '
        'EditingForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(568, 400)
        Me.Controls.Add(Me.CheckSticky)
        Me.Controls.Add(Me.Contents)
        Me.Controls.Add(Me.ResizeButton)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "EditingForm"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
        Me.ShowIcon = False
        Me.Text = "EditingForm"
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents ResizeButton As System.Windows.Forms.Button
    Public WithEvents Contents As System.Windows.Forms.FlowLayoutPanel
    Friend WithEvents ToolTip1 As System.Windows.Forms.ToolTip
    Public WithEvents CheckSticky As System.Windows.Forms.CheckBox
End Class
