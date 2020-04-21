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
        Me.TrackBar1 = New System.Windows.Forms.TrackBar()
        Me.lblCurrent = New System.Windows.Forms.Label()
        Me.btnViewResults = New System.Windows.Forms.Button()
        Me.btnRealtime = New System.Windows.Forms.Button()
        Me.btnStop = New System.Windows.Forms.Button()
        Me.btnRun = New System.Windows.Forms.Button()
        Me.ToolTip1 = New System.Windows.Forms.ToolTip(Me.components)
        CType(Me.TrackBar1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'lblStart
        '
        resources.ApplyResources(Me.lblStart, "lblStart")
        Me.lblStart.Name = "lblStart"
        Me.ToolTip1.SetToolTip(Me.lblStart, resources.GetString("lblStart.ToolTip"))
        '
        'lblFinish
        '
        resources.ApplyResources(Me.lblFinish, "lblFinish")
        Me.lblFinish.Name = "lblFinish"
        Me.ToolTip1.SetToolTip(Me.lblFinish, resources.GetString("lblFinish.ToolTip"))
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        Me.ToolTip1.SetToolTip(Me.Label3, resources.GetString("Label3.ToolTip"))
        '
        'cbScenario
        '
        resources.ApplyResources(Me.cbScenario, "cbScenario")
        Me.cbScenario.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbScenario.FormattingEnabled = True
        Me.cbScenario.Name = "cbScenario"
        Me.ToolTip1.SetToolTip(Me.cbScenario, resources.GetString("cbScenario.ToolTip"))
        '
        'TrackBar1
        '
        resources.ApplyResources(Me.TrackBar1, "TrackBar1")
        Me.TrackBar1.Maximum = 100
        Me.TrackBar1.Name = "TrackBar1"
        Me.TrackBar1.TickStyle = System.Windows.Forms.TickStyle.Both
        Me.ToolTip1.SetToolTip(Me.TrackBar1, resources.GetString("TrackBar1.ToolTip"))
        '
        'lblCurrent
        '
        resources.ApplyResources(Me.lblCurrent, "lblCurrent")
        Me.lblCurrent.Name = "lblCurrent"
        Me.ToolTip1.SetToolTip(Me.lblCurrent, resources.GetString("lblCurrent.ToolTip"))
        '
        'btnViewResults
        '
        resources.ApplyResources(Me.btnViewResults, "btnViewResults")
        Me.btnViewResults.Name = "btnViewResults"
        Me.ToolTip1.SetToolTip(Me.btnViewResults, resources.GetString("btnViewResults.ToolTip"))
        Me.btnViewResults.UseVisualStyleBackColor = True
        '
        'btnRealtime
        '
        resources.ApplyResources(Me.btnRealtime, "btnRealtime")
        Me.btnRealtime.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_realtime
        Me.btnRealtime.Name = "btnRealtime"
        Me.ToolTip1.SetToolTip(Me.btnRealtime, resources.GetString("btnRealtime.ToolTip"))
        Me.btnRealtime.UseVisualStyleBackColor = True
        '
        'btnStop
        '
        resources.ApplyResources(Me.btnStop, "btnStop")
        Me.btnStop.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_stop
        Me.btnStop.Name = "btnStop"
        Me.ToolTip1.SetToolTip(Me.btnStop, resources.GetString("btnStop.ToolTip"))
        Me.btnStop.UseVisualStyleBackColor = True
        '
        'btnRun
        '
        resources.ApplyResources(Me.btnRun, "btnRun")
        Me.btnRun.BackgroundImage = Global.DWSIM.My.Resources.Resources.icons8_play
        Me.btnRun.Name = "btnRun"
        Me.ToolTip1.SetToolTip(Me.btnRun, resources.GetString("btnRun.ToolTip"))
        Me.btnRun.UseVisualStyleBackColor = True
        '
        'FormDynamicsIntegratorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.btnRealtime)
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
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
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
    Friend WithEvents btnRealtime As Button
    Friend WithEvents ToolTip1 As ToolTip
End Class
