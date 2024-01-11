<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class TwoDimChartControl
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(TwoDimChartControl))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.PGrid1 = New Controls.PropertyGridEx.PropertyGridEx()
        Me.btnUpdate = New System.Windows.Forms.Button()
        Me.btnExportPNG = New System.Windows.Forms.Button()
        Me.btnExportSVG = New System.Windows.Forms.Button()
        Me.PlotView1 = New OxyPlot.WindowsForms.PlotView()
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SplitContainer1.Panel1.SuspendLayout()
        Me.SplitContainer1.Panel2.SuspendLayout()
        Me.SplitContainer1.SuspendLayout()
        Me.SuspendLayout()
        '
        'SplitContainer1
        '
        resources.ApplyResources(Me.SplitContainer1, "SplitContainer1")
        Me.SplitContainer1.Name = "SplitContainer1"
        '
        'SplitContainer1.Panel1
        '
        Me.SplitContainer1.Panel1.Controls.Add(Me.PGrid1)
        Me.SplitContainer1.Panel1.Controls.Add(Me.btnUpdate)
        Me.SplitContainer1.Panel1.Controls.Add(Me.btnExportPNG)
        Me.SplitContainer1.Panel1.Controls.Add(Me.btnExportSVG)
        '
        'SplitContainer1.Panel2
        '
        Me.SplitContainer1.Panel2.Controls.Add(Me.PlotView1)
        '
        'PGrid1
        '
        '
        '
        '
        Me.PGrid1.DocCommentDescription.AutoEllipsis = True
        Me.PGrid1.DocCommentDescription.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGrid1.DocCommentDescription.ImeMode = CType(resources.GetObject("PGrid1.DocCommentDescription.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGrid1.DocCommentDescription.Location = CType(resources.GetObject("PGrid1.DocCommentDescription.Location"), System.Drawing.Point)
        Me.PGrid1.DocCommentDescription.Name = ""
        Me.PGrid1.DocCommentDescription.Size = CType(resources.GetObject("PGrid1.DocCommentDescription.Size"), System.Drawing.Size)
        Me.PGrid1.DocCommentDescription.TabIndex = CType(resources.GetObject("PGrid1.DocCommentDescription.TabIndex"), Integer)
        Me.PGrid1.DocCommentImage = Nothing
        '
        '
        '
        Me.PGrid1.DocCommentTitle.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGrid1.DocCommentTitle.Font = CType(resources.GetObject("PGrid1.DocCommentTitle.Font"), System.Drawing.Font)
        Me.PGrid1.DocCommentTitle.ImeMode = CType(resources.GetObject("PGrid1.DocCommentTitle.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGrid1.DocCommentTitle.Location = CType(resources.GetObject("PGrid1.DocCommentTitle.Location"), System.Drawing.Point)
        Me.PGrid1.DocCommentTitle.Name = ""
        Me.PGrid1.DocCommentTitle.Size = CType(resources.GetObject("PGrid1.DocCommentTitle.Size"), System.Drawing.Size)
        Me.PGrid1.DocCommentTitle.TabIndex = CType(resources.GetObject("PGrid1.DocCommentTitle.TabIndex"), Integer)
        Me.PGrid1.DocCommentTitle.UseMnemonic = False
        resources.ApplyResources(Me.PGrid1, "PGrid1")
        Me.PGrid1.Name = "PGrid1"
        Me.PGrid1.ToolbarVisible = False
        '
        '
        '
        Me.PGrid1.ToolStrip.AccessibleName = resources.GetString("PGrid1.ToolStrip.AccessibleName")
        Me.PGrid1.ToolStrip.AccessibleRole = System.Windows.Forms.AccessibleRole.ToolBar
        Me.PGrid1.ToolStrip.AllowMerge = False
        Me.PGrid1.ToolStrip.AutoSize = CType(resources.GetObject("PGrid1.ToolStrip.AutoSize"), Boolean)
        Me.PGrid1.ToolStrip.CanOverflow = False
        Me.PGrid1.ToolStrip.Dock = CType(resources.GetObject("PGrid1.ToolStrip.Dock"), System.Windows.Forms.DockStyle)
        Me.PGrid1.ToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.PGrid1.ToolStrip.Location = CType(resources.GetObject("PGrid1.ToolStrip.Location"), System.Drawing.Point)
        Me.PGrid1.ToolStrip.Name = ""
        Me.PGrid1.ToolStrip.Padding = CType(resources.GetObject("PGrid1.ToolStrip.Padding"), System.Windows.Forms.Padding)
        Me.PGrid1.ToolStrip.Size = CType(resources.GetObject("PGrid1.ToolStrip.Size"), System.Drawing.Size)
        Me.PGrid1.ToolStrip.TabIndex = CType(resources.GetObject("PGrid1.ToolStrip.TabIndex"), Integer)
        Me.PGrid1.ToolStrip.TabStop = True
        Me.PGrid1.ToolStrip.Text = resources.GetString("PGrid1.ToolStrip.Text")
        Me.PGrid1.ToolStrip.Visible = CType(resources.GetObject("PGrid1.ToolStrip.Visible"), Boolean)
        '
        'btnUpdate
        '
        resources.ApplyResources(Me.btnUpdate, "btnUpdate")
        Me.btnUpdate.Name = "btnUpdate"
        Me.btnUpdate.UseVisualStyleBackColor = True
        '
        'btnExportPNG
        '
        resources.ApplyResources(Me.btnExportPNG, "btnExportPNG")
        Me.btnExportPNG.Name = "btnExportPNG"
        Me.btnExportPNG.UseVisualStyleBackColor = True
        '
        'btnExportSVG
        '
        resources.ApplyResources(Me.btnExportSVG, "btnExportSVG")
        Me.btnExportSVG.Name = "btnExportSVG"
        Me.btnExportSVG.UseVisualStyleBackColor = True
        '
        'PlotView1
        '
        Me.PlotView1.BackColor = System.Drawing.Color.White
        resources.ApplyResources(Me.PlotView1, "PlotView1")
        Me.PlotView1.Name = "PlotView1"
        Me.PlotView1.PanCursor = System.Windows.Forms.Cursors.Hand
        Me.PlotView1.ZoomHorizontalCursor = System.Windows.Forms.Cursors.SizeWE
        Me.PlotView1.ZoomRectangleCursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.PlotView1.ZoomVerticalCursor = System.Windows.Forms.Cursors.SizeNS
        '
        'TwoDimChartControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.SplitContainer1)
        Me.Name = "TwoDimChartControl"
        Me.SplitContainer1.Panel1.ResumeLayout(False)
        Me.SplitContainer1.Panel2.ResumeLayout(False)
        CType(Me.SplitContainer1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.SplitContainer1.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents SplitContainer1 As SplitContainer
    Friend WithEvents PlotView1 As OxyPlot.WindowsForms.PlotView
    Friend WithEvents PGrid1 As Controls.PropertyGridEx.PropertyGridEx
    Friend WithEvents btnUpdate As Button
    Friend WithEvents btnExportPNG As Button
    Friend WithEvents btnExportSVG As Button
End Class
