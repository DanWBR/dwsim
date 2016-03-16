<Global.Microsoft.VisualBasic.CompilerServices.DesignerGenerated()> _
Partial Class FormDBManager
    Inherits System.Windows.Forms.Form

    'Das Formular überschreibt den Löschvorgang, um die Komponentenliste zu bereinigen.
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

    'Wird vom Windows Form-Designer benötigt.
    Private components As System.ComponentModel.IContainer

    'Hinweis: Die folgende Prozedur ist für den Windows Form-Designer erforderlich.
    'Das Bearbeiten ist mit dem Windows Form-Designer möglich.  
    'Das Bearbeiten mit dem Code-Editor ist nicht möglich.
    <System.Diagnostics.DebuggerStepThrough()> _
    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(FormDBManager))
        Me.GroupBox1 = New System.Windows.Forms.GroupBox()
        Me.CBDBName = New System.Windows.Forms.ComboBox()
        Me.LblDBPath = New System.Windows.Forms.Label()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.Panel1 = New System.Windows.Forms.Panel()
        Me.BtnEditComponent = New System.Windows.Forms.Button()
        Me.BtnDelComponent = New System.Windows.Forms.Button()
        Me.DGrComps = New System.Windows.Forms.DataGridView()
        Me.ID = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.CompName = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.StudyFile = New System.Windows.Forms.DataGridViewTextBoxColumn()
        Me.GroupBox1.SuspendLayout()
        Me.Panel1.SuspendLayout()
        CType(Me.DGrComps, System.ComponentModel.ISupportInitialize).BeginInit()
        Me.SuspendLayout()
        '
        'GroupBox1
        '
        resources.ApplyResources(Me.GroupBox1, "GroupBox1")
        Me.GroupBox1.Controls.Add(Me.CBDBName)
        Me.GroupBox1.Controls.Add(Me.LblDBPath)
        Me.GroupBox1.Controls.Add(Me.Label2)
        Me.GroupBox1.Controls.Add(Me.Label1)
        Me.GroupBox1.Name = "GroupBox1"
        Me.GroupBox1.TabStop = False
        '
        'CBDBName
        '
        resources.ApplyResources(Me.CBDBName, "CBDBName")
        Me.CBDBName.BackColor = System.Drawing.Color.White
        Me.CBDBName.DropDownHeight = 200
        Me.CBDBName.DropDownStyle = System.Windows.Forms.ComboBoxStyle.DropDownList
        Me.CBDBName.DropDownWidth = 130
        Me.CBDBName.FormattingEnabled = True
        Me.CBDBName.Name = "CBDBName"
        '
        'LblDBPath
        '
        resources.ApplyResources(Me.LblDBPath, "LblDBPath")
        Me.LblDBPath.Name = "LblDBPath"
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.Name = "Label2"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.Name = "Label1"
        '
        'Panel1
        '
        resources.ApplyResources(Me.Panel1, "Panel1")
        Me.Panel1.BackColor = System.Drawing.SystemColors.ControlLight
        Me.Panel1.Controls.Add(Me.BtnEditComponent)
        Me.Panel1.Controls.Add(Me.BtnDelComponent)
        Me.Panel1.Name = "Panel1"
        '
        'BtnEditComponent
        '
        resources.ApplyResources(Me.BtnEditComponent, "BtnEditComponent")
        Me.BtnEditComponent.Image = Global.DWSIM.My.Resources.Resources.wand
        Me.BtnEditComponent.Name = "BtnEditComponent"
        Me.BtnEditComponent.UseVisualStyleBackColor = True
        '
        'BtnDelComponent
        '
        resources.ApplyResources(Me.BtnDelComponent, "BtnDelComponent")
        Me.BtnDelComponent.Image = Global.DWSIM.My.Resources.Resources.delete1
        Me.BtnDelComponent.Name = "BtnDelComponent"
        Me.BtnDelComponent.UseVisualStyleBackColor = True
        '
        'DGrComps
        '
        resources.ApplyResources(Me.DGrComps, "DGrComps")
        Me.DGrComps.AllowUserToAddRows = False
        Me.DGrComps.AllowUserToDeleteRows = False
        Me.DGrComps.ColumnHeadersHeightSizeMode = System.Windows.Forms.DataGridViewColumnHeadersHeightSizeMode.AutoSize
        Me.DGrComps.Columns.AddRange(New System.Windows.Forms.DataGridViewColumn() {Me.ID, Me.CompName, Me.StudyFile})
        Me.DGrComps.Name = "DGrComps"
        Me.DGrComps.ReadOnly = True
        '
        'ID
        '
        resources.ApplyResources(Me.ID, "ID")
        Me.ID.Name = "ID"
        Me.ID.ReadOnly = True
        '
        'CompName
        '
        resources.ApplyResources(Me.CompName, "CompName")
        Me.CompName.Name = "CompName"
        Me.CompName.ReadOnly = True
        '
        'StudyFile
        '
        resources.ApplyResources(Me.StudyFile, "StudyFile")
        Me.StudyFile.Name = "StudyFile"
        Me.StudyFile.ReadOnly = True
        '
        'FormDBManager
        '
        resources.ApplyResources(Me, "$this")
        Me.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font
        Me.Controls.Add(Me.DGrComps)
        Me.Controls.Add(Me.Panel1)
        Me.Controls.Add(Me.GroupBox1)
        Me.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedToolWindow
        Me.Name = "FormDBManager"
        Me.ShowIcon = False
        Me.ShowInTaskbar = False
        Me.GroupBox1.ResumeLayout(False)
        Me.GroupBox1.PerformLayout()
        Me.Panel1.ResumeLayout(False)
        CType(Me.DGrComps, System.ComponentModel.ISupportInitialize).EndInit()
        Me.ResumeLayout(False)

    End Sub
    Friend WithEvents GroupBox1 As System.Windows.Forms.GroupBox
    Friend WithEvents LblDBPath As System.Windows.Forms.Label
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents Label1 As System.Windows.Forms.Label
    Friend WithEvents Panel1 As System.Windows.Forms.Panel
    Friend WithEvents DGrComps As System.Windows.Forms.DataGridView
    Friend WithEvents BtnDelComponent As System.Windows.Forms.Button
    Friend WithEvents BtnEditComponent As System.Windows.Forms.Button
    Public WithEvents CBDBName As System.Windows.Forms.ComboBox
    Friend WithEvents ID As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents CompName As System.Windows.Forms.DataGridViewTextBoxColumn
    Friend WithEvents StudyFile As System.Windows.Forms.DataGridViewTextBoxColumn
End Class
