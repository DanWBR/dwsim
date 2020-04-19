<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormDynamicsIntegratorControl

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormDynamicsIntegratorControl))
        Me.lblStart = New System.Windows.Forms.Label()
        Me.lblFinish = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbScenario = New System.Windows.Forms.ComboBox()
        Me.btnStop = New System.Windows.Forms.Button()
        Me.TrackBar1 = New System.Windows.Forms.TrackBar()
        Me.lblCurrent = New System.Windows.Forms.Label()
        Me.btnRun = New System.Windows.Forms.Button()
        Me.btnViewResults = New System.Windows.Forms.Button()
        CType(Me.TrackBar1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'lblStart
        '
        resources.ApplyResources(Me.lblStart, "lblStart")
        Me.lblStart.Name = "lblStart"
        '
        'lblFinish
        '
        resources.ApplyResources(Me.lblFinish, "lblFinish")
        Me.lblFinish.Name = "lblFinish"
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbScenario
        '
        resources.ApplyResources(Me.cbScenario, "cbScenario")
        Me.cbScenario.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScenario.FormattingEnabled = True
        Me.cbScenario.Name = "cbScenario"
        '
        'btnStop
        '
        resources.ApplyResources(Me.btnStop, "btnStop")
        Me.btnStop.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_stop
        Me.btnStop.Name = "btnStop"
        Me.btnStop.UseVisualStyleBackColor = True
        '
        'TrackBar1
        '
        resources.ApplyResources(Me.TrackBar1, "TrackBar1")
        Me.TrackBar1.Maximum = 100
        Me.TrackBar1.Name = "TrackBar1"
        Me.TrackBar1.TickStyle = System.Windows.Forms.TickStyle.Both
        '
        'lblCurrent
        '
        resources.ApplyResources(Me.lblCurrent, "lblCurrent")
        Me.lblCurrent.Name = "lblCurrent"
        '
        'btnRun
        '
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_play
        Me.btnRun.Name = "btnRun"
        Me.btnRun.UseVisualStyleBackColor = True
        '
        'btnViewResults
        '
        resources.ApplyResources(Me.btnViewResults, "btnViewResults")
        Me.btnViewResults.Name = "btnViewResults"
        Me.btnViewResults.UseVisualStyleBackColor = True
        '
        'FormDynamicsIntegratorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.btnViewResults)
        Me.Controls.Add(Me.lblCurrent)
        Me.Controls.Add(Me.lblFinish)
        Me.Controls.Add(Me.lblStart)
        Me.Controls.Add(Me.TrackBar1)
        Me.Controls.Add(Me.btnStop)
        Me.Controls.Add(Me.cbScenario)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.btnRun)
        Me.DockAreas = CType(((((WeifenLuo.WinFormsUI.Docking.DockAreas.Float Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockLeft) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockRight) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockTop) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockBottom), WeifenLuo.WinFormsUI.Docking.DockAreas)
        Me.HideOnClose = True
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FormDynamicsIntegratorControl"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        CType(Me.TrackBar1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents btnRun As Button
    Friend WithEvents lblStart As Label
    Friend WithEvents lblFinish As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents cbScenario As ComboBox
    Friend WithEvents btnStop As Button
    Friend WithEvents TrackBar1 As TrackBar
    Friend WithEvents lblCurrent As Label
    Friend WithEvents btnViewResults As Button
End Class
