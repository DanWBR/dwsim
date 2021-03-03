<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()>
Partial Class FormConfigPitzer
    Inherits FormConfigPropertyPackageBase

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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigPitzer))
        Me.FaTabStrip1 = New FarsiLibrary.Win.FATabStrip()
        Me.FaTabStripItem2 = New FarsiLibrary.Win.FATabStripItem()
        Me.GroupBox2 = New System.Windows.Forms.GroupBox()
        Me.Label3 = New System.Windows.Forms.Label()
        Me.tbTol = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbMaxIts = New System.Windows.Forms.TextBox()
        Me.cbReacSets = New System.Windows.Forms.ComboBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.FaTabStripItem1 = New FarsiLibrary.Win.FATabStripItem()
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.FaTabStrip1.SuspendLayout()
        Me.FaTabStripItem2.SuspendLayout()
        Me.GroupBox2.SuspendLayout()
        Me.SuspendLayout()
        '
        'FaTabStrip1
        '
        resources.ApplyResources(Me.FaTabStrip1, "FaTabStrip1")
        Me.FaTabStrip1.AlwaysShowClose = False
        Me.FaTabStrip1.AlwaysShowMenuGlyph = False
        Me.FaTabStrip1.Items.AddRange(New FarsiLibrary.Win.FATabStripItem() {Me.FaTabStripItem2, Me.FaTabStripItem1})
        Me.FaTabStrip1.Name = "FaTabStrip1"
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem2
        '
        'FaTabStripItem2
        '
        resources.ApplyResources(Me.FaTabStripItem2, "FaTabStripItem2")
        Me.FaTabStripItem2.CanClose = False
        Me.FaTabStripItem2.Controls.Add(Me.GroupBox2)
        Me.FaTabStripItem2.IsDrawn = True
        Me.FaTabStripItem2.Name = "FaTabStripItem2"
        Me.FaTabStripItem2.Selected = True
        '
        'GroupBox2
        '
        resources.ApplyResources(Me.GroupBox2, "GroupBox2")
        Me.GroupBox2.Controls.Add(Me.Label3)
        Me.GroupBox2.Controls.Add(Me.tbTol)
        Me.GroupBox2.Controls.Add(Me.Label2)
        Me.GroupBox2.Controls.Add(Me.tbMaxIts)
        Me.GroupBox2.Controls.Add(Me.cbReacSets)
        Me.GroupBox2.Controls.Add(Me.Label1)
        Me.GroupBox2.Name = "GroupBox2"
        Me.GroupBox2.TabStop = False
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'tbTol
        '
        resources.ApplyResources(Me.tbTol, "tbTol")
        Me.tbTol.Name = "tbTol"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'tbMaxIts
        '
        resources.ApplyResources(Me.tbMaxIts, "tbMaxIts")
        Me.tbMaxIts.Name = "tbMaxIts"
        '
        'cbReacSets
        '
        resources.ApplyResources(Me.cbReacSets, "cbReacSets")
        Me.cbReacSets.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbReacSets.FormattingEnabled = True
        Me.cbReacSets.Name = "cbReacSets"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'FaTabStripItem1
        '
        resources.ApplyResources(Me.FaTabStripItem1, "FaTabStripItem1")
        Me.FaTabStripItem1.CanClose = False
        Me.FaTabStripItem1.IsDrawn = True
        Me.FaTabStripItem1.Name = "FaTabStripItem1"
        '
        'FormConfigPitzer
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.FaTabStrip1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormConfigPitzer"
        Me.TopMost = True
        CType(Me.FaTabStrip1, System.ComponentModel.ISupportInitialize).EndInit()
        Me.FaTabStrip1.ResumeLayout(False)
        Me.FaTabStripItem2.ResumeLayout(False)
        Me.GroupBox2.ResumeLayout(False)
        Me.GroupBox2.PerformLayout()
        Me.ResumeLayout(False)

    End Sub
    Private WithEvents FaTabStrip1 As FarsiLibrary.Win.FATabStrip
    Private WithEvents FaTabStripItem1 As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents FaTabStripItem2 As FarsiLibrary.Win.FATabStripItem
    Friend WithEvents GroupBox2 As System.Windows.Forms.GroupBox
    Friend WithEvents cbReacSets As System.Windows.Forms.ComboBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents tbTol As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbMaxIts As System.Windows.Forms.TextBox
End Class
