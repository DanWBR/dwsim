<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class WaitForm
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(WaitForm))
        Me.pbSolver = New CircularProgress.SpinningProgress.SpinningProgress()
        Me.btnCancel = New System.Windows.Forms.Button()
        Me.lblMessage = New System.Windows.Forms.Label()
        Me.SuspendLayout()
        '
        'pbSolver
        '
        Me.pbSolver.ActiveSegmentColour = System.Drawing.Color.SteelBlue
        Me.pbSolver.AutoIncrementFrequency = 50.0R
        Me.pbSolver.BehindTransistionSegmentIsActive = False
        Me.pbSolver.Location = New System.Drawing.Point(79, 16)
        Me.pbSolver.Name = "pbSolver"
        Me.pbSolver.Size = New System.Drawing.Size(60, 60)
        Me.pbSolver.TabIndex = 4
        Me.pbSolver.TransistionSegment = 3
        Me.pbSolver.TransistionSegmentColour = System.Drawing.Color.LightSkyBlue
        '
        'btnCancel
        '
        Me.btnCancel.Location = New System.Drawing.Point(64, 118)
        Me.btnCancel.Name = "btnCancel"
        Me.btnCancel.Size = New System.Drawing.Size(88, 23)
        Me.btnCancel.TabIndex = 5
        Me.btnCancel.Text = "Cancel"
        Me.btnCancel.UseVisualStyleBackColor = True
        '
        'lblMessage
        '
        Me.lblMessage.Anchor = CType(((System.Windows.Forms.AnchorStyles.Top Or System.Windows.Forms.AnchorStyles.Left) _
            Or System.Windows.Forms.AnchorStyles.Right), System.Windows.Forms.AnchorStyles)
        Me.lblMessage.Location = New System.Drawing.Point(12, 85)
        Me.lblMessage.Name = "lblMessage"
        Me.lblMessage.Size = New System.Drawing.Size(197, 30)
        Me.lblMessage.TabIndex = 6
        Me.lblMessage.Text = "working..."
        Me.lblMessage.TextAlign = System.Drawing.ContentAlignment.TopCenter
        '
        'WaitForm
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.BackColor = System.Drawing.Color.White
        Me.ClientSize = New System.Drawing.Size(221, 153)
        Me.Controls.Add(Me.lblMessage)
        Me.Controls.Add(Me.btnCancel)
        Me.Controls.Add(Me.pbSolver)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "WaitForm"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "Please wait..."
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents pbSolver As CircularProgress.SpinningProgress.SpinningProgress
    Public WithEvents btnCancel As Button
    Public WithEvents lblMessage As Label
End Class
