<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class EditingForm_Pipe_UserDefinedU
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
        Me.grid1 = New unvell.ReoGrid.ReoGridControl()
        Me.SuspendLayout()
        '
        'grid1
        '
        Me.grid1.BackColor = System.Drawing.Color.White
        Me.grid1.ColumnHeaderContextMenuStrip = Nothing
        Me.grid1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.grid1.LeadHeaderContextMenuStrip = Nothing
        Me.grid1.Location = New System.Drawing.Point(0, 0)
        Me.grid1.Name = "grid1"
        Me.grid1.RowHeaderContextMenuStrip = Nothing
        Me.grid1.Script = Nothing
        Me.grid1.SheetTabContextMenuStrip = Nothing
        Me.grid1.SheetTabNewButtonVisible = False
        Me.grid1.SheetTabVisible = False
        Me.grid1.SheetTabWidth = 60
        Me.grid1.ShowScrollEndSpacing = True
        Me.grid1.Size = New System.Drawing.Size(415, 393)
        Me.grid1.TabIndex = 1
        Me.grid1.Text = "ReoGridControl1"
        '
        'EditingForm_Pipe_UserDefinedU
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(96.0!, 96.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Dpi
        Me.ClientSize = New System.Drawing.Size(415, 393)
        Me.Controls.Add(Me.grid1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.SizableToolWindow
        Me.Name = "EditingForm_Pipe_UserDefinedU"
        Me.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen
        Me.Text = "User-Defined Overall HTC Data"
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents grid1 As unvell.ReoGrid.ReoGridControl
End Class
