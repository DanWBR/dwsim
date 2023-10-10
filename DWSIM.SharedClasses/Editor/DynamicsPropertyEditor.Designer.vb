<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class DynamicsPropertyEditor

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(DynamicsPropertyEditor))
        Me.gbProps = New System.Windows.Forms.GroupBox()
        Me.PropertiesLayout = New System.Windows.Forms.TableLayoutPanel()
        Me.gbProps.SuspendLayout()
        Me.SuspendLayout()
        '
        'gbProps
        '
        Me.gbProps.Controls.Add(Me.PropertiesLayout)
        resources.ApplyResources(Me.gbProps, "gbProps")
        Me.gbProps.Name = "gbProps"
        Me.gbProps.TabStop = False
        '
        'PropertiesLayout
        '
        resources.ApplyResources(Me.PropertiesLayout, "PropertiesLayout")
        Me.PropertiesLayout.Name = "PropertiesLayout"
        '
        'DynamicsPropertyEditor
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.Controls.Add(Me.gbProps)
        Me.DockAreas = CType(((((WeifenLuo.WinFormsUI.Docking.DockAreas.Float Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockLeft) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockRight) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockTop) _
            Or WeifenLuo.WinFormsUI.Docking.DockAreas.DockBottom), WeifenLuo.WinFormsUI.Docking.DockAreas)
        Me.MaximizeBox = False
        Me.MinimizeBox = False
        Me.Name = "DynamicsPropertyEditor"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.gbProps.ResumeLayout(False)
        Me.ResumeLayout(False)

    End Sub
    Public WithEvents PropertiesLayout As TableLayoutPanel
    Public WithEvents gbProps As GroupBox
End Class
