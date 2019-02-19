Imports DWSIM.Controls
Imports PropertyGridEx.PropertyGridEx

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormEditGraphicObject
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormEditGraphicObject))
        Me.ToolStrip1 = New System.Windows.Forms.ToolStrip()
        Me.ToolStripLabel1 = New System.Windows.Forms.ToolStripLabel()
        Me.tsbDockingLeft = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingLeftAutoHide = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingRightAutoHide = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingRight = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingTop = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingBottom = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingDocument = New System.Windows.Forms.ToolStripButton()
        Me.tsbDockingFloat = New System.Windows.Forms.ToolStripButton()
        Me.tsbClose = New System.Windows.Forms.ToolStripButton()
        Me.PGEx2 = New PropertyGridEx.PropertyGridEx()
        Me.ToolStrip1.SuspendLayout()
        Me.SuspendLayout()
        '
        'ToolStrip1
        '
        resources.ApplyResources(Me.ToolStrip1, "ToolStrip1")
        Me.ToolStrip1.BackColor = System.Drawing.Color.SteelBlue
        Me.ToolStrip1.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.ToolStrip1.Items.AddRange(New System.Windows.Forms.ToolStripItem() {Me.ToolStripLabel1, Me.tsbDockingLeft, Me.tsbDockingLeftAutoHide, Me.tsbDockingRightAutoHide, Me.tsbDockingRight, Me.tsbDockingTop, Me.tsbDockingBottom, Me.tsbDockingDocument, Me.tsbDockingFloat, Me.tsbClose})
        Me.ToolStrip1.Name = "ToolStrip1"
        Me.ToolStrip1.RenderMode = System.Windows.Forms.ToolStripRenderMode.System
        '
        'ToolStripLabel1
        '
        resources.ApplyResources(Me.ToolStripLabel1, "ToolStripLabel1")
        Me.ToolStripLabel1.ForeColor = System.Drawing.Color.White
        Me.ToolStripLabel1.Name = "ToolStripLabel1"
        '
        'tsbDockingLeft
        '
        resources.ApplyResources(Me.tsbDockingLeft, "tsbDockingLeft")
        Me.tsbDockingLeft.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingLeft.Image = Global.DWSIM.My.Resources.Resources.arrow_left1
        Me.tsbDockingLeft.Name = "tsbDockingLeft"
        '
        'tsbDockingLeftAutoHide
        '
        resources.ApplyResources(Me.tsbDockingLeftAutoHide, "tsbDockingLeftAutoHide")
        Me.tsbDockingLeftAutoHide.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingLeftAutoHide.Image = Global.DWSIM.My.Resources.Resources.rewind
        Me.tsbDockingLeftAutoHide.Name = "tsbDockingLeftAutoHide"
        '
        'tsbDockingRightAutoHide
        '
        resources.ApplyResources(Me.tsbDockingRightAutoHide, "tsbDockingRightAutoHide")
        Me.tsbDockingRightAutoHide.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingRightAutoHide.Image = Global.DWSIM.My.Resources.Resources.fast_forward
        Me.tsbDockingRightAutoHide.Name = "tsbDockingRightAutoHide"
        '
        'tsbDockingRight
        '
        resources.ApplyResources(Me.tsbDockingRight, "tsbDockingRight")
        Me.tsbDockingRight.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingRight.Image = Global.DWSIM.My.Resources.Resources.arrow_right
        Me.tsbDockingRight.Name = "tsbDockingRight"
        '
        'tsbDockingTop
        '
        resources.ApplyResources(Me.tsbDockingTop, "tsbDockingTop")
        Me.tsbDockingTop.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingTop.Image = Global.DWSIM.My.Resources.Resources.arrow_up1
        Me.tsbDockingTop.Name = "tsbDockingTop"
        '
        'tsbDockingBottom
        '
        resources.ApplyResources(Me.tsbDockingBottom, "tsbDockingBottom")
        Me.tsbDockingBottom.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingBottom.Image = Global.DWSIM.My.Resources.Resources.arrow_down1
        Me.tsbDockingBottom.Name = "tsbDockingBottom"
        '
        'tsbDockingDocument
        '
        resources.ApplyResources(Me.tsbDockingDocument, "tsbDockingDocument")
        Me.tsbDockingDocument.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingDocument.Image = Global.DWSIM.My.Resources.Resources.arrows_4_way
        Me.tsbDockingDocument.Name = "tsbDockingDocument"
        '
        'tsbDockingFloat
        '
        resources.ApplyResources(Me.tsbDockingFloat, "tsbDockingFloat")
        Me.tsbDockingFloat.DisplayStyle = System.Windows.Forms.ToolStripItemDisplayStyle.Image
        Me.tsbDockingFloat.Image = Global.DWSIM.My.Resources.Resources._new
        Me.tsbDockingFloat.Name = "tsbDockingFloat"
        '
        'tsbClose
        '
        resources.ApplyResources(Me.tsbClose, "tsbClose")
        Me.tsbClose.Alignment = System.Windows.Forms.ToolStripItemAlignment.Right
        Me.tsbClose.ForeColor = System.Drawing.Color.White
        Me.tsbClose.Image = Global.DWSIM.My.Resources.Resources.no
        Me.tsbClose.Name = "tsbClose"
        '
        'PGEx2
        '
        resources.ApplyResources(Me.PGEx2, "PGEx2")
        '
        '
        '
        Me.PGEx2.DocCommentDescription.AccessibleDescription = resources.GetString("PGEx2.DocCommentDescription.AccessibleDescription")
        Me.PGEx2.DocCommentDescription.AccessibleName = resources.GetString("PGEx2.DocCommentDescription.AccessibleName")
        Me.PGEx2.DocCommentDescription.Anchor = CType(resources.GetObject("PGEx2.DocCommentDescription.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGEx2.DocCommentDescription.AutoEllipsis = True
        Me.PGEx2.DocCommentDescription.AutoSize = CType(resources.GetObject("PGEx2.DocCommentDescription.AutoSize"), Boolean)
        Me.PGEx2.DocCommentDescription.BackColor = System.Drawing.Color.Transparent
        Me.PGEx2.DocCommentDescription.BackgroundImageLayout = CType(resources.GetObject("PGEx2.DocCommentDescription.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGEx2.DocCommentDescription.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGEx2.DocCommentDescription.Dock = CType(resources.GetObject("PGEx2.DocCommentDescription.Dock"), System.Windows.Forms.DockStyle)
        Me.PGEx2.DocCommentDescription.Font = CType(resources.GetObject("PGEx2.DocCommentDescription.Font"), System.Drawing.Font)
        Me.PGEx2.DocCommentDescription.ImageAlign = CType(resources.GetObject("PGEx2.DocCommentDescription.ImageAlign"), System.Drawing.ContentAlignment)
        Me.PGEx2.DocCommentDescription.ImageIndex = CType(resources.GetObject("PGEx2.DocCommentDescription.ImageIndex"), Integer)
        Me.PGEx2.DocCommentDescription.ImageKey = resources.GetString("PGEx2.DocCommentDescription.ImageKey")
        Me.PGEx2.DocCommentDescription.ImeMode = CType(resources.GetObject("PGEx2.DocCommentDescription.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGEx2.DocCommentDescription.Location = CType(resources.GetObject("PGEx2.DocCommentDescription.Location"), System.Drawing.Point)
        Me.PGEx2.DocCommentDescription.MaximumSize = CType(resources.GetObject("PGEx2.DocCommentDescription.MaximumSize"), System.Drawing.Size)
        Me.PGEx2.DocCommentDescription.Name = ""
        Me.PGEx2.DocCommentDescription.RightToLeft = CType(resources.GetObject("PGEx2.DocCommentDescription.RightToLeft"), System.Windows.Forms.RightToLeft)
        Me.PGEx2.DocCommentDescription.Size = CType(resources.GetObject("PGEx2.DocCommentDescription.Size"), System.Drawing.Size)
        Me.PGEx2.DocCommentDescription.TabIndex = CType(resources.GetObject("PGEx2.DocCommentDescription.TabIndex"), Integer)
        Me.PGEx2.DocCommentDescription.TextAlign = CType(resources.GetObject("PGEx2.DocCommentDescription.TextAlign"), System.Drawing.ContentAlignment)
        Me.PGEx2.DocCommentImage = Nothing
        '
        '
        '
        Me.PGEx2.DocCommentTitle.AccessibleDescription = resources.GetString("PGEx2.DocCommentTitle.AccessibleDescription")
        Me.PGEx2.DocCommentTitle.AccessibleName = resources.GetString("PGEx2.DocCommentTitle.AccessibleName")
        Me.PGEx2.DocCommentTitle.Anchor = CType(resources.GetObject("PGEx2.DocCommentTitle.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGEx2.DocCommentTitle.AutoSize = CType(resources.GetObject("PGEx2.DocCommentTitle.AutoSize"), Boolean)
        Me.PGEx2.DocCommentTitle.BackColor = System.Drawing.Color.Transparent
        Me.PGEx2.DocCommentTitle.BackgroundImageLayout = CType(resources.GetObject("PGEx2.DocCommentTitle.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGEx2.DocCommentTitle.Cursor = System.Windows.Forms.Cursors.Default
        Me.PGEx2.DocCommentTitle.Dock = CType(resources.GetObject("PGEx2.DocCommentTitle.Dock"), System.Windows.Forms.DockStyle)
        Me.PGEx2.DocCommentTitle.Font = CType(resources.GetObject("PGEx2.DocCommentTitle.Font"), System.Drawing.Font)
        Me.PGEx2.DocCommentTitle.ImageAlign = CType(resources.GetObject("PGEx2.DocCommentTitle.ImageAlign"), System.Drawing.ContentAlignment)
        Me.PGEx2.DocCommentTitle.ImageIndex = CType(resources.GetObject("PGEx2.DocCommentTitle.ImageIndex"), Integer)
        Me.PGEx2.DocCommentTitle.ImageKey = resources.GetString("PGEx2.DocCommentTitle.ImageKey")
        Me.PGEx2.DocCommentTitle.ImeMode = CType(resources.GetObject("PGEx2.DocCommentTitle.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGEx2.DocCommentTitle.Location = CType(resources.GetObject("PGEx2.DocCommentTitle.Location"), System.Drawing.Point)
        Me.PGEx2.DocCommentTitle.MaximumSize = CType(resources.GetObject("PGEx2.DocCommentTitle.MaximumSize"), System.Drawing.Size)
        Me.PGEx2.DocCommentTitle.Name = ""
        Me.PGEx2.DocCommentTitle.RightToLeft = CType(resources.GetObject("PGEx2.DocCommentTitle.RightToLeft"), System.Windows.Forms.RightToLeft)
        Me.PGEx2.DocCommentTitle.Size = CType(resources.GetObject("PGEx2.DocCommentTitle.Size"), System.Drawing.Size)
        Me.PGEx2.DocCommentTitle.TabIndex = CType(resources.GetObject("PGEx2.DocCommentTitle.TabIndex"), Integer)
        Me.PGEx2.DocCommentTitle.TextAlign = CType(resources.GetObject("PGEx2.DocCommentTitle.TextAlign"), System.Drawing.ContentAlignment)
        Me.PGEx2.DocCommentTitle.UseMnemonic = False
        Me.PGEx2.DrawFlatToolbar = True
        Me.PGEx2.Name = "PGEx2"
        Me.PGEx2.SelectedObject = CType(resources.GetObject("PGEx2.SelectedObject"), Object)
        Me.PGEx2.ShowCustomProperties = True
        Me.PGEx2.ToolbarVisible = False
        '
        '
        '
        Me.PGEx2.ToolStrip.AccessibleDescription = resources.GetString("PGEx2.ToolStrip.AccessibleDescription")
        Me.PGEx2.ToolStrip.AccessibleName = resources.GetString("PGEx2.ToolStrip.AccessibleName")
        Me.PGEx2.ToolStrip.AccessibleRole = System.Windows.Forms.AccessibleRole.ToolBar
        Me.PGEx2.ToolStrip.AllowMerge = False
        Me.PGEx2.ToolStrip.Anchor = CType(resources.GetObject("PGEx2.ToolStrip.Anchor"), System.Windows.Forms.AnchorStyles)
        Me.PGEx2.ToolStrip.AutoSize = CType(resources.GetObject("PGEx2.ToolStrip.AutoSize"), Boolean)
        Me.PGEx2.ToolStrip.BackgroundImage = CType(resources.GetObject("PGEx2.ToolStrip.BackgroundImage"), System.Drawing.Image)
        Me.PGEx2.ToolStrip.BackgroundImageLayout = CType(resources.GetObject("PGEx2.ToolStrip.BackgroundImageLayout"), System.Windows.Forms.ImageLayout)
        Me.PGEx2.ToolStrip.CanOverflow = False
        Me.PGEx2.ToolStrip.Dock = CType(resources.GetObject("PGEx2.ToolStrip.Dock"), System.Windows.Forms.DockStyle)
        Me.PGEx2.ToolStrip.Font = CType(resources.GetObject("PGEx2.ToolStrip.Font"), System.Drawing.Font)
        Me.PGEx2.ToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.PGEx2.ToolStrip.ImeMode = CType(resources.GetObject("PGEx2.ToolStrip.ImeMode"), System.Windows.Forms.ImeMode)
        Me.PGEx2.ToolStrip.Location = CType(resources.GetObject("PGEx2.ToolStrip.Location"), System.Drawing.Point)
        Me.PGEx2.ToolStrip.MaximumSize = CType(resources.GetObject("PGEx2.ToolStrip.MaximumSize"), System.Drawing.Size)
        Me.PGEx2.ToolStrip.Name = ""
        Me.PGEx2.ToolStrip.Padding = CType(resources.GetObject("PGEx2.ToolStrip.Padding"), System.Windows.Forms.Padding)
        Me.PGEx2.ToolStrip.RightToLeft = CType(resources.GetObject("PGEx2.ToolStrip.RightToLeft"), System.Windows.Forms.RightToLeft)
        Me.PGEx2.ToolStrip.Size = CType(resources.GetObject("PGEx2.ToolStrip.Size"), System.Drawing.Size)
        Me.PGEx2.ToolStrip.TabIndex = CType(resources.GetObject("PGEx2.ToolStrip.TabIndex"), Integer)
        Me.PGEx2.ToolStrip.TabStop = True
        Me.PGEx2.ToolStrip.Text = resources.GetString("PGEx2.ToolStrip.Text")
        Me.PGEx2.ToolStrip.Visible = CType(resources.GetObject("PGEx2.ToolStrip.Visible"), Boolean)
        '
        'FormEditGraphicObject
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.PGEx2)
        Me.Controls.Add(Me.ToolStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormEditGraphicObject"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        Me.ShowIcon = False
        Me.ToolStrip1.ResumeLayout(False)
        Me.ToolStrip1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents ToolStrip1 As System.Windows.Forms.ToolStrip
    Friend WithEvents ToolStripLabel1 As System.Windows.Forms.ToolStripLabel
    Friend WithEvents tsbDockingLeft As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingLeftAutoHide As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingRightAutoHide As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingRight As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingTop As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingBottom As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingDocument As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbDockingFloat As System.Windows.Forms.ToolStripButton
    Friend WithEvents tsbClose As System.Windows.Forms.ToolStripButton
    Public WithEvents PGEx2 As PropertyGridEx.PropertyGridEx
End Class
