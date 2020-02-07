<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormConfigureMasterTable
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
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormConfigureMasterTable))
        Me.Label1 = New System.Windows.Forms.Label()
        Me.cbObjectType = New System.Windows.Forms.ComboBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.lvProps = New System.Windows.Forms.ListView()
        Me.ColumnHeader2 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.Label3 = New System.Windows.Forms.Label()
        Me.cbOrderBy = New System.Windows.Forms.ComboBox()
        Me.lvObjects = New System.Windows.Forms.ListView()
        Me.ColumnHeader1 = CType(New System.Windows.Forms.ColumnHeader(), System.Windows.Forms.ColumnHeader)
        Me.Label4 = New System.Windows.Forms.Label()
        Me.TextBox1 = New System.Windows.Forms.TextBox()
        Me.btnOrderUp = New System.Windows.Forms.Button()
        Me.btnOrderDown = New System.Windows.Forms.Button()
        Me.lblOrder = New System.Windows.Forms.Label()
        Me.Button1 = New System.Windows.Forms.Button()
        Me.Button2 = New System.Windows.Forms.Button()
        Me.Button3 = New System.Windows.Forms.Button()
        Me.Button4 = New System.Windows.Forms.Button()
        Me.Button5 = New System.Windows.Forms.Button()
        Me.Label5 = New System.Windows.Forms.Label()
        Me.nupLines = New System.Windows.Forms.NumericUpDown()
        Me.Button6 = New System.Windows.Forms.Button()
        CType(Me.nupLines, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbObjectType
        '
        resources.ApplyResources(Me.cbObjectType, "cbObjectType")
        Me.cbObjectType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbObjectType.FormattingEnabled = True
        Me.cbObjectType.Name = "cbObjectType"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'lvProps
        '
        resources.ApplyResources(Me.lvProps, "lvProps")
        Me.lvProps.CheckBoxes = True
        Me.lvProps.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvProps.FullRowSelect = True
        Me.lvProps.HideSelection = False
        Me.lvProps.Name = "lvProps"
        Me.lvProps.UseCompatibleStateImageBehavior = False
        Me.lvProps.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader2
        '
        resources.ApplyResources(Me.ColumnHeader2, "ColumnHeader2")
        '
        'Label3
        '
        resources.ApplyResources(Me.Label3, "Label3")
        Me.Label3.Name = "Label3"
        '
        'cbOrderBy
        '
        resources.ApplyResources(Me.cbOrderBy, "cbOrderBy")
        Me.cbOrderBy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrderBy.FormattingEnabled = True
        Me.cbOrderBy.Name = "cbOrderBy"
        '
        'lvObjects
        '
        resources.ApplyResources(Me.lvObjects, "lvObjects")
        Me.lvObjects.CheckBoxes = True
        Me.lvObjects.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvObjects.FullRowSelect = True
        Me.lvObjects.HideSelection = False
        Me.lvObjects.MultiSelect = False
        Me.lvObjects.Name = "lvObjects"
        Me.lvObjects.ShowGroups = False
        Me.lvObjects.UseCompatibleStateImageBehavior = False
        Me.lvObjects.View = System.Windows.Forms.View.Details
        '
        'ColumnHeader1
        '
        resources.ApplyResources(Me.ColumnHeader1, "ColumnHeader1")
        '
        'Label4
        '
        resources.ApplyResources(Me.Label4, "Label4")
        Me.Label4.Name = "Label4"
        '
        'TextBox1
        '
        resources.ApplyResources(Me.TextBox1, "TextBox1")
        Me.TextBox1.Name = "TextBox1"
        '
        'btnOrderUp
        '
        resources.ApplyResources(Me.btnOrderUp, "btnOrderUp")
        Me.btnOrderUp.Name = "btnOrderUp"
        Me.btnOrderUp.UseVisualStyleBackColor = True
        '
        'btnOrderDown
        '
        resources.ApplyResources(Me.btnOrderDown, "btnOrderDown")
        Me.btnOrderDown.Name = "btnOrderDown"
        Me.btnOrderDown.UseVisualStyleBackColor = True
        '
        'lblOrder
        '
        resources.ApplyResources(Me.lblOrder, "lblOrder")
        Me.lblOrder.Name = "lblOrder"
        '
        'Button1
        '
        resources.ApplyResources(Me.Button1, "Button1")
        Me.Button1.Name = "Button1"
        Me.Button1.UseVisualStyleBackColor = True
        '
        'Button2
        '
        resources.ApplyResources(Me.Button2, "Button2")
        Me.Button2.Name = "Button2"
        Me.Button2.UseVisualStyleBackColor = True
        '
        'Button3
        '
        resources.ApplyResources(Me.Button3, "Button3")
        Me.Button3.Name = "Button3"
        Me.Button3.UseVisualStyleBackColor = True
        '
        'Button4
        '
        resources.ApplyResources(Me.Button4, "Button4")
        Me.Button4.Name = "Button4"
        Me.Button4.UseVisualStyleBackColor = True
        '
        'Button5
        '
        resources.ApplyResources(Me.Button5, "Button5")
        Me.Button5.Name = "Button5"
        Me.Button5.UseVisualStyleBackColor = True
        '
        'Label5
        '
        resources.ApplyResources(Me.Label5, "Label5")
        Me.Label5.Name = "Label5"
        '
        'nupLines
        '
        resources.ApplyResources(Me.nupLines, "nupLines")
        Me.nupLines.Maximum = New Decimal(New Integer() {10, 0, 0, 0})
        Me.nupLines.Minimum = New Decimal(New Integer() {1, 0, 0, 0})
        Me.nupLines.Name = "nupLines"
        Me.nupLines.Value = New Decimal(New Integer() {1, 0, 0, 0})
        '
        'Button6
        '
        resources.ApplyResources(Me.Button6, "Button6")
        Me.Button6.Name = "Button6"
        Me.Button6.UseVisualStyleBackColor = True
        '
        'FormConfigureMasterTable
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.Button6)
        Me.Controls.Add(Me.nupLines)
        Me.Controls.Add(Me.Label5)
        Me.Controls.Add(Me.Button4)
        Me.Controls.Add(Me.Button5)
        Me.Controls.Add(Me.Button3)
        Me.Controls.Add(Me.Button2)
        Me.Controls.Add(Me.Button1)
        Me.Controls.Add(Me.lblOrder)
        Me.Controls.Add(Me.btnOrderDown)
        Me.Controls.Add(Me.btnOrderUp)
        Me.Controls.Add(Me.TextBox1)
        Me.Controls.Add(Me.Label4)
        Me.Controls.Add(Me.lvObjects)
        Me.Controls.Add(Me.cbOrderBy)
        Me.Controls.Add(Me.Label3)
        Me.Controls.Add(Me.lvProps)
        Me.Controls.Add(Me.Label2)
        Me.Controls.Add(Me.cbObjectType)
        Me.Controls.Add(Me.Label1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormConfigureMasterTable"
        CType(Me.nupLines, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)
        Me.PerformLayout()

    End Sub
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents cbObjectType As System.Windows.Forms.ComboBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents lvProps As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader2 As System.Windows.Forms.ColumnHeader
    Friend WithEvents Label3 As System.Windows.Forms.Label
    Friend WithEvents cbOrderBy As System.Windows.Forms.ComboBox
    Friend WithEvents lvObjects As System.Windows.Forms.ListView
    Friend WithEvents ColumnHeader1 As System.Windows.Forms.ColumnHeader
    Friend WithEvents Label4 As System.Windows.Forms.Label
    Friend WithEvents TextBox1 As System.Windows.Forms.TextBox
    Friend WithEvents btnOrderUp As System.Windows.Forms.Button
    Friend WithEvents btnOrderDown As System.Windows.Forms.Button
    Friend WithEvents lblOrder As System.Windows.Forms.Label
    Friend WithEvents Button1 As Button
    Friend WithEvents Button2 As Button
    Friend WithEvents Button3 As Button
    Friend WithEvents Button4 As Button
    Friend WithEvents Button5 As Button
    Friend WithEvents Label5 As Label
    Friend WithEvents nupLines As NumericUpDown
    Friend WithEvents Button6 As Button
End Class
