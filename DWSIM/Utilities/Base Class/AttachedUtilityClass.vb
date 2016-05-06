Public Class AttachedUtilityClass
    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Implements Interfaces.IAttachedUtility

    Friend WithEvents PanelUtility As System.Windows.Forms.Panel
    Friend WithEvents PanelHeader As System.Windows.Forms.Panel

    Public Property InternalUtility As Interfaces.IAttachedUtility

    Public Property AttachedTo As Interfaces.ISimulationObject Implements Interfaces.IAttachedUtility.AttachedTo

    Public Property ID As Integer Implements Interfaces.IAttachedUtility.ID

    Public Property UtilityName As String Implements Interfaces.IAttachedUtility.Name

    Public Overridable Function GetPropertyUnits(pname As String) As String Implements Interfaces.IAttachedUtility.GetPropertyUnits
        Return InternalUtility.GetPropertyUnits(pname)
    End Function

    Public Overridable Function GetPropertyValue(pname As String) As Object Implements Interfaces.IAttachedUtility.GetPropertyValue
        Return InternalUtility.GetPropertyValue(pname)
    End Function

    Public Overridable Sub SetPropertyValue(pname As String, pvalue As Object) Implements Interfaces.IAttachedUtility.SetPropertyValue
        InternalUtility.SetPropertyValue(pname, pvalue)
    End Sub

    Public Overridable Function GetPropertyList() As List(Of String) Implements Interfaces.IAttachedUtility.GetPropertyList
        Return InternalUtility.GetPropertyList()
    End Function

    Public Overridable Sub UpdateResults() Implements Interfaces.IAttachedUtility.Update
        InternalUtility.Update()
    End Sub

    Private Sub AttachedUtilityClass_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        PanelUtility.Controls.Add(InternalUtility)
    End Sub

    Private Sub InitializeComponent()
        Me.PanelHeader = New System.Windows.Forms.Panel()
        Me.PanelUtility = New System.Windows.Forms.Panel()
        Me.SuspendLayout()
        '
        'PanelHeader
        '
        Me.PanelHeader.BackColor = System.Drawing.Color.SteelBlue
        Me.PanelHeader.Dock = System.Windows.Forms.DockStyle.Top
        Me.PanelHeader.Location = New System.Drawing.Point(0, 0)
        Me.PanelHeader.Name = "PanelHeader"
        Me.PanelHeader.Size = New System.Drawing.Size(617, 78)
        Me.PanelHeader.TabIndex = 0
        '
        'PanelUtility
        '
        Me.PanelUtility.Dock = System.Windows.Forms.DockStyle.Fill
        Me.PanelUtility.Location = New System.Drawing.Point(0, 78)
        Me.PanelUtility.Name = "PanelUtility"
        Me.PanelUtility.Size = New System.Drawing.Size(617, 279)
        Me.PanelUtility.TabIndex = 1
        '
        'AttachedUtilityClass
        '
        Me.ClientSize = New System.Drawing.Size(617, 357)
        Me.Controls.Add(Me.PanelUtility)
        Me.Controls.Add(Me.PanelHeader)
        Me.Font = New System.Drawing.Font("Microsoft Sans Serif", 8.25!, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, CType(0, Byte))
        Me.Name = "AttachedUtilityClass"
        Me.ResumeLayout(False)

    End Sub

End Class
