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
        Me.components = New System.ComponentModel.Container()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormDynamicsIntegratorControl))
        Me.lblStart = New System.Windows.Forms.Label()
        Me.lblFinish = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbScenario = New System.Windows.Forms.ComboBox()
        Me.lblCurrent = New System.Windows.Forms.Label()
        Me.btnViewResults = New System.Windows.Forms.Button()
        Me.btnRealtime = New System.Windows.Forms.Button()
        Me.btnStop = New System.Windows.Forms.Button()
        Me.btnRun = New System.Windows.Forms.Button()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        Me.ProgressBar1 = New System.Windows.Forms.ProgressBar()
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
        Me.cbScenario.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScenario.FormattingEnabled = True
        resources.ApplyResources(Me.cbScenario, "cbScenario")
        Me.cbScenario.Name = "cbScenario"
        '
        'lblCurrent
        '
        resources.ApplyResources(Me.lblCurrent, "lblCurrent")
        Me.lblCurrent.Name = "lblCurrent"
        '
        'btnViewResults
        '
        resources.ApplyResources(Me.btnViewResults, "btnViewResults")
        Me.btnViewResults.Name = "btnViewResults"
        Me.btnViewResults.UseVisualStyleBackColor = True
        '
        'btnRealtime
        '
        Me.btnRealtime.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_realtime
        resources.ApplyResources(Me.btnRealtime, "btnRealtime")
        Me.btnRealtime.Name = "btnRealtime"
        Me.ToolTip1.SetToolTip(Me.btnRealtime, resources.GetString("btnRealtime.ToolTip"))
        Me.btnRealtime.UseVisualStyleBackColor = True
        '
        'btnStop
        '
        Me.btnStop.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_stop
        resources.ApplyResources(Me.btnStop, "btnStop")
        Me.btnStop.Name = "btnStop"
        Me.ToolTip1.SetToolTip(Me.btnStop, resources.GetString("btnStop.ToolTip"))
        Me.btnStop.UseVisualStyleBackColor = True
        '
        'btnRun
        '
        Me.btnRun.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_play
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.Name = "btnRun"
        Me.ToolTip1.SetToolTip(Me.btnRun, resources.GetString("btnRun.ToolTip"))
        Me.btnRun.UseVisualStyleBackColor = True
        '
        'ProgressBar1
        '
        resources.ApplyResources(Me.ProgressBar1, "ProgressBar1")
        Me.ProgressBar1.Name = "ProgressBar1"
        Me.ProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous
        '
        'FormDynamicsIntegratorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.ProgressBar1)
        Me.Controls.Add(Me.btnRealtime)
        Me.Controls.Add(Me.btnViewResults)
        Me.Controls.Add(Me.lblCurrent)
        Me.Controls.Add(Me.lblFinish)
        Me.Controls.Add(Me.lblStart)
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
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub

    Friend WithEvents btnRun As Button
    Friend WithEvents lblStart As Label
    Friend WithEvents lblFinish As Label
    Friend WithEvents Label3 As Label
    Friend WithEvents cbScenario As ComboBox
    Friend WithEvents btnStop As Button
    Friend WithEvents lblCurrent As Label
    Friend WithEvents btnViewResults As Button
    Friend WithEvents btnRealtime As Button
    Friend WithEvents ToolTip1 As ToolTip
    Friend WithEvents ProgressBar1 As ProgressBar
End Class
