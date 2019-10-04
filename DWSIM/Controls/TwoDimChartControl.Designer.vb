<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class TwoDimChartControl
    Inherits System.Windows.Forms.UserControl

    'UserControl overrides dispose to clean up the component list.
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(TwoDimChartControl))
        Me.SplitContainer1 = New System.Windows.Forms.SplitContainer()
        Me.PlotView1 = New OxyPlot.WindowsForms.PlotView()
        Me.PGrid1 = New Controls.PropertyGridEx.PropertyGridEx()
        Me.btnUpdate = New System.Windows.Forms.Button()
        Me.btnExportPNG = New System.Windows.Forms.Button()
        Me.btnExportSVG = New System.Windows.Forms.Button()
        Me.SaveFileDialog1 = New System.Windows.Forms.SaveFileDialog()
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
        resources.ApplyResources(Me.SplitContainer1.Panel1, "SplitContainer1.Panel1")
        Me.SplitContainer1.Panel1.Controls.Add(Me.PlotView1)
        '
        'SplitContainer1.Panel2
        '
        resources.ApplyResources(Me.SplitContainer1.Panel2, "SplitContainer1.Panel2")
        Me.SplitContainer1.Panel2.Controls.Add(Me.PGrid1)
        Me.SplitContainer1.Panel2.Controls.Add(Me.btnUpdate)
        Me.SplitContainer1.Panel2.Controls.Add(Me.btnExportPNG)
        Me.SplitContainer1.Panel2.Controls.Add(Me.btnExportSVG)
        '
        'PlotView1
        '
        resources.ApplyResources(Me.PlotView1, "PlotView1")
        Me.PlotView1.BackColor = System.Drawing.Color.White
        Me.PlotView1.Name = "PlotView1"
        Me.PlotView1.PanCursor = System.Windows.Forms.Cursors.Hand
        Me.PlotView1.ZoomHorizontalCursor = System.Windows.Forms.Cursors.SizeWE
        Me.PlotView1.ZoomRectangleCursor = System.Windows.Forms.Cursors.SizeNWSE
        Me.PlotView1.ZoomVerticalCursor = System.Windows.Forms.Cursors.SizeNS
        '
        'PGrid1
        '
        resources.ApplyResources(Me.PGrid1, "PGrid1")
        '
        '
        '
        Me.PGrid1.DocCommentDescription.AccessibleDescription = resources.GetString("PGrid1.DocCommentDescription.AccessibleDescription")
        Me.PGrid1.DocCommentDescription.AccessibleName = resources.GetString("PGrid1.DocCommentDescription.AccessibleName")
        Me.PGrid1.DocCommentDescription.Anchor = CType(resources.GetObject("PGrid1.DocCommentDescription.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGrid1.DocCommentDescription.AutoEllipsis = True
        Me.PGrid1.DocCommentDescription.AutoSize = CType(resources.GetObject("PGrid1.DocCommentDescription.AutoSize"), Boolean)
        Me.PGrid1.DocCommentDescription.BackgroundImageLayout = CType(resources.GetObject("PGrid1.DocCommentDescription.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGrid1.DocCommentDescription.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGrid1.DocCommentDescription.Dock = CType(resources.GetObject("PGrid1.DocCommentDescription.Dock"), System.Windows.Forms.DockStyle)
        Me.PGrid1.DocCommentDescription.Font = CType(resources.GetObject("PGrid1.DocCommentDescription.Font"), System.Drawing.Font)
        Me.PGrid1.DocCommentDescription.ImageAlign = CType(resources.GetObject("PGrid1.DocCommentDescription.ImageAlign"), System.Drawing.ContentAlignment)
        Me.PGrid1.DocCommentDescription.ImageIndex = CType(resources.GetObject("PGrid1.DocCommentDescription.ImageIndex"), Integer)
        Me.PGrid1.DocCommentDescription.ImageKey = resources.GetString("PGrid1.DocCommentDescription.ImageKey")
        Me.PGrid1.DocCommentDescription.ImeMode = CType(resources.GetObject("PGrid1.DocCommentDescription.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGrid1.DocCommentDescription.Location = CType(resources.GetObject("PGrid1.DocCommentDescription.Location"), System.Drawing.Point)
        Me.PGrid1.DocCommentDescription.MaximumSize = CType(resources.GetObject("PGrid1.DocCommentDescription.MaximumSize"), System.Drawing.Size)
        Me.PGrid1.DocCommentDescription.Name = ""
        Me.PGrid1.DocCommentDescription.RightToLeft = CType(resources.GetObject("PGrid1.DocCommentDescription.RightToLeft"), System.Windows.Forms.RightToLeft)
        Me.PGrid1.DocCommentDescription.Size = CType(resources.GetObject("PGrid1.DocCommentDescription.Size"), System.Drawing.Size)
        Me.PGrid1.DocCommentDescription.TabIndex = CType(resources.GetObject("PGrid1.DocCommentDescription.TabIndex"), Integer)
        Me.PGrid1.DocCommentDescription.TextAlign = CType(resources.GetObject("PGrid1.DocCommentDescription.TextAlign"), System.Drawing.ContentAlignment)
        Me.PGrid1.DocCommentImage = Nothing
        '
        '
        '
        Me.PGrid1.DocCommentTitle.AccessibleDescription = resources.GetString("PGrid1.DocCommentTitle.AccessibleDescription")
        Me.PGrid1.DocCommentTitle.AccessibleName = resources.GetString("PGrid1.DocCommentTitle.AccessibleName")
        Me.PGrid1.DocCommentTitle.Anchor = CType(resources.GetObject("PGrid1.DocCommentTitle.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGrid1.DocCommentTitle.AutoSize = CType(resources.GetObject("PGrid1.DocCommentTitle.AutoSize"), Boolean)
        Me.PGrid1.DocCommentTitle.BackgroundImageLayout = CType(resources.GetObject("PGrid1.DocCommentTitle.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGrid1.DocCommentTitle.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGrid1.DocCommentTitle.Dock = CType(resources.GetObject("PGrid1.DocCommentTitle.Dock"), System.Windows.Forms.DockStyle)
        Me.PGrid1.DocCommentTitle.Font = CType(resources.GetObject("PGrid1.DocCommentTitle.Font"), System.Drawing.Font)
        Me.PGrid1.DocCommentTitle.ImageAlign = CType(resources.GetObject("PGrid1.DocCommentTitle.ImageAlign"), System.Drawing.ContentAlignment)
        Me.PGrid1.DocCommentTitle.ImageIndex = CType(resources.GetObject("PGrid1.DocCommentTitle.ImageIndex"), Integer)
        Me.PGrid1.DocCommentTitle.ImageKey = resources.GetString("PGrid1.DocCommentTitle.ImageKey")
        Me.PGrid1.DocCommentTitle.ImeMode = CType(resources.GetObject("PGrid1.DocCommentTitle.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGrid1.DocCommentTitle.Location = CType(resources.GetObject("PGrid1.DocCommentTitle.Location"), System.Drawing.Point)
        Me.PGrid1.DocCommentTitle.MaximumSize = CType(resources.GetObject("PGrid1.DocCommentTitle.MaximumSize"), System.Drawing.Size)
        Me.PGrid1.DocCommentTitle.Name = ""
        Me.PGrid1.DocCommentTitle.RightToLeft = CType(resources.GetObject("PGrid1.DocCommentTitle.RightToLeft"), System.Windows.Forms.RightToLeft)
        Me.PGrid1.DocCommentTitle.Size = CType(resources.GetObject("PGrid1.DocCommentTitle.Size"), System.Drawing.Size)
        Me.PGrid1.DocCommentTitle.TabIndex = CType(resources.GetObject("PGrid1.DocCommentTitle.TabIndex"), Integer)
        Me.PGrid1.DocCommentTitle.TextAlign = CType(resources.GetObject("PGrid1.DocCommentTitle.TextAlign"), System.Drawing.ContentAlignment)
        Me.PGrid1.DocCommentTitle.UseMnemonic = False
        Me.PGrid1.Name = "PGrid1"
        Me.PGrid1.ToolbarVisible = False
        '
        '
        '
        Me.PGrid1.ToolStrip.AccessibleDescription = resources.GetString("PGrid1.ToolStrip.AccessibleDescription")
        Me.PGrid1.ToolStrip.AccessibleName = resources.GetString("PGrid1.ToolStrip.AccessibleName")
        Me.PGrid1.ToolStrip.AccessibleRole = System.Windows.Forms.AccessibleRole.ToolBar
        Me.PGrid1.ToolStrip.AllowMerge = False
        Me.PGrid1.ToolStrip.Anchor = CType(resources.GetObject("PGrid1.ToolStrip.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGrid1.ToolStrip.AutoSize = CType(resources.GetObject("PGrid1.ToolStrip.AutoSize"), Boolean)
        Me.PGrid1.ToolStrip.BackgroundImage = CType(resources.GetObject("PGrid1.ToolStrip.BackgroundImage"), System.Drawing.Image)
        Me.PGrid1.ToolStrip.BackgroundImageLayout = CType(resources.GetObject("PGrid1.ToolStrip.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGrid1.ToolStrip.CanOverflow = False
        Me.PGrid1.ToolStrip.Dock = CType(resources.GetObject("PGrid1.ToolStrip.Dock"), System.Windows.Forms.DockStyle)
        Me.PGrid1.ToolStrip.Font = CType(resources.GetObject("PGrid1.ToolStrip.Font"), System.Drawing.Font)
        Me.PGrid1.ToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.PGrid1.ToolStrip.ImeMode = CType(resources.GetObject("PGrid1.ToolStrip.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGrid1.ToolStrip.Location = CType(resources.GetObject("PGrid1.ToolStrip.Location"), System.Drawing.Point)
        Me.PGrid1.ToolStrip.MaximumSize = CType(resources.GetObject("PGrid1.ToolStrip.MaximumSize"), System.Drawing.Size)
        Me.PGrid1.ToolStrip.Name = ""
        Me.PGrid1.ToolStrip.Padding = CType(resources.GetObject("PGrid1.ToolStrip.Padding"), System.Windows.Forms.Padding)
        Me.PGrid1.ToolStrip.RightToLeft = CType(resources.GetObject("PGrid1.ToolStrip.RightToLeft"), System.Windows.Forms.RightToLeft)
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
        'SaveFileDialog1
        '
        resources.ApplyResources(Me.SaveFileDialog1, "SaveFileDialog1")
        '
        'TwoDimChartControl
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
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
    Friend WithEvents SaveFileDialog1 As SaveFileDialog
End Class
