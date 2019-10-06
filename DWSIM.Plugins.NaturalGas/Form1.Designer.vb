Imports DWSIM.Controls

<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class Form1
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(Form1))
        Me.TableLayoutPanel1 = New System.Windows.Forms.TableLayoutPanel()
        Me.Panel3 = New System.Windows.Forms.Panel()
        Me.lblVapOnly = New System.Windows.Forms.Label()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.Panel2 = New System.Windows.Forms.Panel()
        Me.lblCalcd = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.lblStream = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.pg = New Controls.PropertyGridEx.PropertyGridEx()
        Me.TableLayoutPanel1.SuspendLayout()
        Me.Panel3.SuspendLayout()
        Me.Panel2.SuspendLayout()
        Me.Panel1.SuspendLayout()
        Me.SuspendLayout()
        '
        'TableLayoutPanel1
        '
        Me.TableLayoutPanel1.CellBorderStyle = System.Windows.Forms.TableLayoutPanelCellBorderStyle.[Single]
        Me.TableLayoutPanel1.ColumnCount = 1
        Me.TableLayoutPanel1.ColumnStyles.Add(New System.Windows.Forms.ColumnStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Controls.Add(Me.pg, 0, 3)
        Me.TableLayoutPanel1.Controls.Add(Me.Panel3, 0, 2)
        Me.TableLayoutPanel1.Controls.Add(Me.Panel2, 0, 1)
        Me.TableLayoutPanel1.Controls.Add(Me.Panel1, 0, 0)
        Me.TableLayoutPanel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.TableLayoutPanel1.Location = New System.Drawing.Point(0, 0)
        Me.TableLayoutPanel1.Name = "TableLayoutPanel1"
        Me.TableLayoutPanel1.RowCount = 4
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Absolute, 20.0!))
        Me.TableLayoutPanel1.RowStyles.Add(New System.Windows.Forms.RowStyle(System.Windows.Forms.SizeType.Percent, 100.0!))
        Me.TableLayoutPanel1.Size = New System.Drawing.Size(320, 382)
        Me.TableLayoutPanel1.TabIndex = 1
        '
        'Panel3
        '
        Me.Panel3.Controls.Add(Me.lblVapOnly)
        Me.Panel3.Controls.Add(Me.Label3)
        Me.Panel3.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel3.Location = New System.Drawing.Point(4, 46)
        Me.Panel3.Name = "Panel3"
        Me.Panel3.Size = New System.Drawing.Size(312, 14)
        Me.Panel3.TabIndex = 3
        '
        'lblVapOnly
        '
        Me.lblVapOnly.AutoSize = True
        Me.lblVapOnly.Dock = System.Windows.Forms.DockStyle.Left
        Me.lblVapOnly.Location = New System.Drawing.Point(121, 0)
        Me.lblVapOnly.Name = "lblVapOnly"
        Me.lblVapOnly.Size = New System.Drawing.Size(0, 13)
        Me.lblVapOnly.TabIndex = 3
        Me.lblVapOnly.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label3
        '
        Me.Label3.Dock = System.Windows.Forms.DockStyle.Left
        Me.Label3.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label3.Location = New System.Drawing.Point(0, 0)
        Me.Label3.Margin = New System.Windows.Forms.Padding(3)
        Me.Label3.Name = "Label3"
        Me.Label3.Size = New System.Drawing.Size(121, 14)
        Me.Label3.TabIndex = 1
        Me.Label3.Text = "Vapor Only"
        Me.Label3.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Panel2
        '
        Me.Panel2.Controls.Add(Me.lblCalcd)
        Me.Panel2.Controls.Add(Me.Label2)
        Me.Panel2.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel2.Location = New System.Drawing.Point(4, 25)
        Me.Panel2.Name = "Panel2"
        Me.Panel2.Size = New System.Drawing.Size(312, 14)
        Me.Panel2.TabIndex = 2
        '
        'lblCalcd
        '
        Me.lblCalcd.AutoSize = True
        Me.lblCalcd.Dock = System.Windows.Forms.DockStyle.Left
        Me.lblCalcd.Location = New System.Drawing.Point(121, 0)
        Me.lblCalcd.Margin = New System.Windows.Forms.Padding(0)
        Me.lblCalcd.Name = "lblCalcd"
        Me.lblCalcd.Size = New System.Drawing.Size(0, 13)
        Me.lblCalcd.TabIndex = 2
        Me.lblCalcd.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label2
        '
        Me.Label2.Dock = System.Windows.Forms.DockStyle.Left
        Me.Label2.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label2.Location = New System.Drawing.Point(0, 0)
        Me.Label2.Margin = New System.Windows.Forms.Padding(3)
        Me.Label2.Name = "Label2"
        Me.Label2.Size = New System.Drawing.Size(121, 14)
        Me.Label2.TabIndex = 1
        Me.Label2.Text = "Calculated"
        Me.Label2.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Panel1
        '
        Me.Panel1.Controls.Add(Me.lblStream)
        Me.Panel1.Controls.Add(Me.Label1)
        Me.Panel1.Dock = System.Windows.Forms.DockStyle.Fill
        Me.Panel1.Location = New System.Drawing.Point(4, 4)
        Me.Panel1.Name = "Panel1"
        Me.Panel1.Size = New System.Drawing.Size(312, 14)
        Me.Panel1.TabIndex = 1
        '
        'lblStream
        '
        Me.lblStream.AutoSize = True
        Me.lblStream.Dock = System.Windows.Forms.DockStyle.Left
        Me.lblStream.Location = New System.Drawing.Point(121, 0)
        Me.lblStream.Name = "lblStream"
        Me.lblStream.Size = New System.Drawing.Size(0, 13)
        Me.lblStream.TabIndex = 1
        Me.lblStream.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'Label1
        '
        Me.Label1.Dock = System.Windows.Forms.DockStyle.Left
        Me.Label1.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Label1.Location = New System.Drawing.Point(0, 0)
        Me.Label1.Margin = New System.Windows.Forms.Padding(3)
        Me.Label1.Name = "Label1"
        Me.Label1.Size = New System.Drawing.Size(121, 14)
        Me.Label1.TabIndex = 0
        Me.Label1.Text = "Material Stream"
        Me.Label1.TextAlign = System.Drawing.ContentAlignment.MiddleLeft
        '
        'pg
        '
        '
        '
        '
        Me.pg.DocCommentDescription.AutoEllipsis = True
        Me.pg.DocCommentDescription.Cursor = System.Windows.Forms.Cursors.Default
        Me.pg.DocCommentDescription.Location = New System.Drawing.Point(3, 18)
        Me.pg.DocCommentDescription.Name = ""
        Me.pg.DocCommentDescription.Size = New System.Drawing.Size(306, 37)
        Me.pg.DocCommentDescription.TabIndex = 1
        Me.pg.DocCommentImage = Nothing
        '
        '
        '
        Me.pg.DocCommentTitle.Cursor = System.Windows.Forms.Cursors.Default
        Me.pg.DocCommentTitle.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Bold)
        Me.pg.DocCommentTitle.Location = New System.Drawing.Point(3, 3)
        Me.pg.DocCommentTitle.Name = ""
        Me.pg.DocCommentTitle.Size = New System.Drawing.Size(306, 15)
        Me.pg.DocCommentTitle.TabIndex = 0
        Me.pg.DocCommentTitle.UseMnemonic = False
        Me.pg.Dock = System.Windows.Forms.DockStyle.Fill
        Me.pg.DrawFlatToolbar = True
        Me.pg.Location = New System.Drawing.Point(4, 67)
        Me.pg.Name = "pg"
        Me.pg.Size = New System.Drawing.Size(312, 311)
        Me.pg.TabIndex = 4
        Me.pg.ToolbarVisible = False
        '
        '
        '
        Me.pg.ToolStrip.AccessibleName = "ToolBar"
        Me.pg.ToolStrip.AccessibleRole = System.Windows.Forms.AccessibleRole.ToolBar
        Me.pg.ToolStrip.AllowMerge = False
        Me.pg.ToolStrip.AutoSize = False
        Me.pg.ToolStrip.CanOverflow = False
        Me.pg.ToolStrip.Dock = System.Windows.Forms.DockStyle.None
        Me.pg.ToolStrip.GripStyle = System.Windows.Forms.ToolStripGripStyle.Hidden
        Me.pg.ToolStrip.Location = New System.Drawing.Point(0, 1)
        Me.pg.ToolStrip.Name = ""
        Me.pg.ToolStrip.Padding = New System.Windows.Forms.Padding(2, 0, 1, 0)
        Me.pg.ToolStrip.Size = New System.Drawing.Size(312, 25)
        Me.pg.ToolStrip.TabIndex = 1
        Me.pg.ToolStrip.TabStop = True
        Me.pg.ToolStrip.Text = "PropertyGridToolBar"
        Me.pg.ToolStrip.Visible = False
        '
        'Form1
        '
        Me.AutoScaleDimensions = New System.Drawing.SizeF(6.0!, 13.0!)
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.ClientSize = New System.Drawing.Size(320, 382)
        Me.Controls.Add(Me.TableLayoutPanel1)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Icon = CType(resources.GetObject("$this.Icon"), System.Drawing.Icon)
        Me.Name = "Form1"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
        Me.TabText = "Natural Gas Properties"
        Me.Text = "Natural Gas Properties"
        Me.TableLayoutPanel1.ResumeLayout(False)
        Me.Panel3.ResumeLayout(False)
        Me.Panel3.PerformLayout()
        Me.Panel2.ResumeLayout(False)
        Me.Panel2.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        Me.Panel1.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Friend WithEvents TableLayoutPanel1 As System.Windows.Forms.TableLayoutPanel
    Friend WithEvents Panel3 As System.Windows.Forms.Panel
    Friend WithEvents Panel2 As System.Windows.Forms.Panel
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents lblVapOnly As System.Windows.Forms.Label
    Friend WithEvents lblCalcd As System.Windows.Forms.Label
    Friend WithEvents lblStream As System.Windows.Forms.Label
    Friend WithEvents pg As Controls.PropertyGridEx.PropertyGridEx

End Class
