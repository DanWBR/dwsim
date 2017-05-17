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
        Me.SuspendLayout()
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'cbObjectType
        '
        Me.cbObjectType.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbObjectType.FormattingEnabled = True
        resources.ApplyResources(Me.cbObjectType, "cbObjectType")
        Me.cbObjectType.Name = "cbObjectType"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'lvProps
        '
        Me.lvProps.CheckBoxes = True
        Me.lvProps.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader2})
        Me.lvProps.FullRowSelect = True
        resources.ApplyResources(Me.lvProps, "lvProps")
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
        Me.cbOrderBy.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.cbOrderBy.FormattingEnabled = True
        resources.ApplyResources(Me.cbOrderBy, "cbOrderBy")
        Me.cbOrderBy.Name = "cbOrderBy"
        '
        'lvObjects
        '
        Me.lvObjects.CheckBoxes = True
        Me.lvObjects.Columns.AddRange(New System.Windows.Forms.ColumnHeader() {Me.ColumnHeader1})
        Me.lvObjects.FullRowSelect = True
        Me.lvObjects.HideSelection = False
        resources.ApplyResources(Me.lvObjects, "lvObjects")
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
        'FormConfigureMasterTable
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
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
End Class
