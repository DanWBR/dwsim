<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormGroupedPPConfigWindows
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormGroupedPPConfigWindows))
        Me.TabControl1 = New System.Windows.Forms.TabControl()
        Me.TabPageBIPs = New System.Windows.Forms.TabPage()
        Me.TabPageFlash = New System.Windows.Forms.TabPage()
        Me.TabPageProps = New System.Windows.Forms.TabPage()
        Me.TabPageSolids = New System.Windows.Forms.TabPage()
        Me.CLBSolids = New System.Windows.Forms.CheckedListBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.TabPagePropOverr = New System.Windows.Forms.TabPage()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.TabControl1.SuspendLayout()
        Me.TabPageSolids.SuspendLayout()
        Me.TabPagePropOverr.SuspendLayout()
        Me.SuspendLayout()
        '
        'TabControl1
        '
        resources.ApplyResources(Me.TabControl1, "TabControl1")
        Me.TabControl1.Controls.Add(Me.TabPageBIPs)
        Me.TabControl1.Controls.Add(Me.TabPageFlash)
        Me.TabControl1.Controls.Add(Me.TabPageProps)
        Me.TabControl1.Controls.Add(Me.TabPageSolids)
        Me.TabControl1.Controls.Add(Me.TabPagePropOverr)
        Me.TabControl1.Name = "TabControl1"
        Me.TabControl1.SelectedIndex = 0
        '
        'TabPageBIPs
        '
        resources.ApplyResources(Me.TabPageBIPs, "TabPageBIPs")
        Me.TabPageBIPs.Name = "TabPageBIPs"
        Me.TabPageBIPs.UseVisualStyleBackColor = True
        '
        'TabPageFlash
        '
        resources.ApplyResources(Me.TabPageFlash, "TabPageFlash")
        Me.TabPageFlash.Name = "TabPageFlash"
        Me.TabPageFlash.UseVisualStyleBackColor = True
        '
        'TabPageProps
        '
        resources.ApplyResources(Me.TabPageProps, "TabPageProps")
        Me.TabPageProps.Name = "TabPageProps"
        Me.TabPageProps.UseVisualStyleBackColor = True
        '
        'TabPageSolids
        '
        resources.ApplyResources(Me.TabPageSolids, "TabPageSolids")
        Me.TabPageSolids.Controls.Add(Me.CLBSolids)
        Me.TabPageSolids.Controls.Add(Me.Label1)
        Me.TabPageSolids.Name = "TabPageSolids"
        Me.TabPageSolids.UseVisualStyleBackColor = True
        '
        'CLBSolids
        '
        resources.ApplyResources(Me.CLBSolids, "CLBSolids")
        Me.CLBSolids.BorderStyle = System.Windows.Forms.BorderStyle.None
        Me.CLBSolids.CheckOnClick = True
        Me.CLBSolids.FormattingEnabled = True
        Me.CLBSolids.MultiColumn = True
        Me.CLBSolids.Name = "CLBSolids"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'TabPagePropOverr
        '
        resources.ApplyResources(Me.TabPagePropOverr, "TabPagePropOverr")
        Me.TabPagePropOverr.Controls.Add(Me.Button1)
        Me.TabPagePropOverr.Name = "TabPagePropOverr"
        Me.TabPagePropOverr.UseVisualStyleBackColor = True
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'FormGroupedPPConfigWindows
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
Me.AutoScaleDimensions = New System.Drawing.SizeF(96, 96)
        Me.Controls.Add(Me.TabControl1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "FormGroupedPPConfigWindows"
        Me.TabControl1.ResumeLayout(False)
        Me.TabPageSolids.ResumeLayout(False)
        Me.TabPageSolids.PerformLayout()
        Me.TabPagePropOverr.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents TabPageSolids As TabPage
    Friend WithEvents CLBSolids As CheckedListBox
    Friend WithEvents Label1 As Label
    Public WithEvents TabControl1 As TabControl
    Public WithEvents TabPageBIPs As TabPage
    Public WithEvents TabPageProps As TabPage
    Public WithEvents TabPageFlash As TabPage
    Friend WithEvents TabPagePropOverr As TabPage
    Friend WithEvents Button1 As Button
End Class
