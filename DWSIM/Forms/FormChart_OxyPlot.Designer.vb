<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormChart_OxyPlot

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
        Me.PlotView1 = New OxyPlot.WindowsForms.PlotView()
        Me.SuspendLayout()
        '
        'PlotView1
        '
        Me.PlotView1.BackColor = System.Drawing.Color.White
        Me.PlotView1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PlotView1.Location = New System.Drawing.Point(0, 0)
        Me.PlotView1.Name = "PlotView1"
        Me.PlotView1.PanCursor = System.Windows.Forms.Cursors.Hand
        Me.PlotView1.Size = New System.Drawing.Size(800, 450)
        Me.PlotView1.TabIndex = 1
        Me.PlotView1.Text = "PlotView1"
        Me.PlotView1.ZoomHorizontalCursor = System.Windows.Forms.Cursors.SizeWE
        Me.PlotView1.ZoomRectangleCursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.PlotView1.ZoomVerticalCursor = System.Windows.Forms.Cursors.SizeNS
        '
        'FormChart_OxyPlot
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(800, 450)
        Me.Controls.Add(Me.PlotView1)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.HideOnClose = True
        Me.Name = "FormChart_OxyPlot"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        Me.Text = "FormChart_OxyPlot"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents PlotView1 As OxyPlot.WindowsForms.PlotView
End Class
