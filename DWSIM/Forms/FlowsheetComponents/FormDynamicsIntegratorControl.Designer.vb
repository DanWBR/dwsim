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
        Me.btnStepBackwards = New System.Windows.Forms.Button()
        Me.btnStepForward = New System.Windows.Forms.Button()
        Me.ProgressBar1 = New System.Windows.Forms.ProgressBar()
        Me.btnLiveChart = New System.Windows.Forms.Button()
        Me.FlowLayoutPanel1 = New System.Windows.Forms.FlowLayoutPanel()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.FlowLayoutPanel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'lblStart
        '
        resources.ApplyResources(Me.lblStart, "lblStart")
        Me.lblStart.BackColor = System.Drawing.Color.Transparent
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
        'lblCurrent
        '
        resources.ApplyResources(Me.lblCurrent, "lblCurrent")
        Me.lblCurrent.BackColor = System.Drawing.Color.Transparent
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
        'btnStepBackwards
        '
        resources.ApplyResources(Me.btnStepBackwards, "btnStepBackwards")
        Me.btnStepBackwards.BackgroundImage = Global.DWSIM.My.Resources.Resources.backwards_48px
        Me.btnStepBackwards.Name = "btnStepBackwards"
        Me.ToolTip1.SetToolTip(Me.btnStepBackwards, resources.GetString("btnStepBackwards.ToolTip"))
        Me.btnStepBackwards.UseVisualStyleBackColor = True
        '
        'btnStepForward
        '
        resources.ApplyResources(Me.btnStepForward, "btnStepForward")
        Me.btnStepForward.BackgroundImage = Global.DWSIM.My.Resources.Resources.advance_48px
        Me.btnStepForward.Name = "btnStepForward"
        Me.ToolTip1.SetToolTip(Me.btnStepForward, resources.GetString("btnStepForward.ToolTip"))
        Me.btnStepForward.UseVisualStyleBackColor = True
        '
        'ProgressBar1
        '
        resources.ApplyResources(Me.ProgressBar1, "ProgressBar1")
        Me.ProgressBar1.Name = "ProgressBar1"
        Me.ProgressBar1.Style = System.Windows.Forms.ProgressBarStyle.Continuous
        Me.ToolTip1.SetToolTip(Me.ProgressBar1, resources.GetString("ProgressBar1.ToolTip"))
        '
        'btnLiveChart
        '
        resources.ApplyResources(Me.btnLiveChart, "btnLiveChart")
        Me.btnLiveChart.Name = "btnLiveChart"
        Me.ToolTip1.SetToolTip(Me.btnLiveChart, resources.GetString("btnLiveChart.ToolTip"))
        Me.btnLiveChart.UseVisualStyleBackColor = True
        '
        'FlowLayoutPanel1
        '
        resources.ApplyResources(Me.FlowLayoutPanel1, "FlowLayoutPanel1")
        Me.FlowLayoutPanel1.Controls.Add(Me.lblStart)
        Me.FlowLayoutPanel1.Controls.Add(Me.Label1)
        Me.FlowLayoutPanel1.Controls.Add(Me.lblCurrent)
        Me.FlowLayoutPanel1.Controls.Add(Me.Label2)
        Me.FlowLayoutPanel1.Controls.Add(Me.lblFinish)
        Me.FlowLayoutPanel1.Name = "FlowLayoutPanel1"
        Me.ToolTip1.SetToolTip(Me.FlowLayoutPanel1, resources.GetString("FlowLayoutPanel1.ToolTip"))
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        Me.ToolTip1.SetToolTip(Me.Label1, resources.GetString("Label1.ToolTip"))
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        Me.ToolTip1.SetToolTip(Me.Label2, resources.GetString("Label2.ToolTip"))
        '
        'FormDynamicsIntegratorControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.btnStepForward)
        Me.Controls.Add(Me.btnStepBackwards)
        Me.Controls.Add(Me.FlowLayoutPanel1)
        Me.Controls.Add(Me.btnLiveChart)
        Me.Controls.Add(Me.btnRealtime)
        Me.Controls.Add(Me.btnViewResults)
        Me.Controls.Add(Me.btnStop)
        Me.Controls.Add(Me.cbScenario)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.btnRun)
        Me.Controls.Add(Me.ProgressBar1)
        Me.DockAreas = CType(((((WeifenLuo.WinFormsUI.Docking.DockAreas.Float Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockLeft) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockRight) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockTop) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockBottom), WeifenLuo.WinFormsUI.Docking.DockAreas)
        Me.HideOnClose = True
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "FormDynamicsIntegratorControl"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.ToolTip1.SetToolTip(Me, resources.GetString("$this.ToolTip"))
        Me.FlowLayoutPanel1.ResumeLayout(False)
        Me.FlowLayoutPanel1.PerformLayout()
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
    Friend WithEvents btnLiveChart As Button
    Friend WithEvents FlowLayoutPanel1 As FlowLayoutPanel
    Friend WithEvents Label1 As Label
    Friend WithEvents Label2 As Label
    Friend WithEvents btnStepBackwards As Button
    Friend WithEvents btnStepForward As Button
End Class
