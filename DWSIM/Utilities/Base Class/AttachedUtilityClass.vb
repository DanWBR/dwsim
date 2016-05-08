Public Class AttachedUtilityClass

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Implements Interfaces.IAttachedUtility

    Friend WithEvents PanelUtility As System.Windows.Forms.Panel
    Friend WithEvents chkAutoUpdate As System.Windows.Forms.CheckBox
    Friend WithEvents tbAttachedTo As System.Windows.Forms.TextBox
    Friend WithEvents Label2 As System.Windows.Forms.Label
    Friend WithEvents tbName As System.Windows.Forms.TextBox
    Friend WithEvents Label1 As System.Windows.Forms.Label
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

        tbName.Text = UtilityName
        tbAttachedTo.Text = AttachedTo.GraphicObject.Tag
        InternalUtility.AttachedTo = AttachedTo

        Me.Size = DirectCast(InternalUtility, UserControl).Size

        If Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float Then
            Me.FloatPane.Size = DirectCast(InternalUtility, UserControl).Size
        End If

        PanelUtility.Controls.Add(InternalUtility)
        DirectCast(InternalUtility, UserControl).Dock = DockStyle.Fill

    End Sub

    Sub New()

        InitializeComponent()

    End Sub

    Private Sub InitializeComponent()
        Dim resources As System.ComponentModel.ComponentResourceManager = New System.ComponentModel.ComponentResourceManager(GetType(AttachedUtilityClass))
        Me.PanelHeader = New System.Windows.Forms.Panel()
        Me.chkAutoUpdate = New System.Windows.Forms.CheckBox()
        Me.tbAttachedTo = New System.Windows.Forms.TextBox()
        Me.Label2 = New System.Windows.Forms.Label()
        Me.tbName = New System.Windows.Forms.TextBox()
        Me.Label1 = New System.Windows.Forms.Label()
        Me.PanelUtility = New System.Windows.Forms.Panel()
        Me.PanelHeader.SuspendLayout()
        Me.SuspendLayout()
        '
        'PanelHeader
        '
        Me.PanelHeader.BackColor = System.Drawing.Color.SteelBlue
        Me.PanelHeader.Controls.Add(Me.chkAutoUpdate)
        Me.PanelHeader.Controls.Add(Me.tbAttachedTo)
        Me.PanelHeader.Controls.Add(Me.Label2)
        Me.PanelHeader.Controls.Add(Me.tbName)
        Me.PanelHeader.Controls.Add(Me.Label1)
        resources.ApplyResources(Me.PanelHeader, "PanelHeader")
        Me.PanelHeader.Name = "PanelHeader"
        '
        'chkAutoUpdate
        '
        resources.ApplyResources(Me.chkAutoUpdate, "chkAutoUpdate")
        Me.chkAutoUpdate.ForeColor = System.Drawing.Color.White
        Me.chkAutoUpdate.Name = "chkAutoUpdate"
        Me.chkAutoUpdate.UseVisualStyleBackColor = True
        '
        'tbAttachedTo
        '
        resources.ApplyResources(Me.tbAttachedTo, "tbAttachedTo")
        Me.tbAttachedTo.Name = "tbAttachedTo"
        Me.tbAttachedTo.ReadOnly = True
        '
        'Label2
        '
        resources.ApplyResources(Me.Label2, "Label2")
        Me.Label2.ForeColor = System.Drawing.Color.White
        Me.Label2.Name = "Label2"
        '
        'tbName
        '
        resources.ApplyResources(Me.tbName, "tbName")
        Me.tbName.Name = "tbName"
        '
        'Label1
        '
        resources.ApplyResources(Me.Label1, "Label1")
        Me.Label1.ForeColor = System.Drawing.Color.White
        Me.Label1.Name = "Label1"
        '
        'PanelUtility
        '
        resources.ApplyResources(Me.PanelUtility, "PanelUtility")
        Me.PanelUtility.Name = "PanelUtility"
        '
        'AttachedUtilityClass
        '
        resources.ApplyResources(Me, "$this")
        Me.Controls.Add(Me.PanelUtility)
        Me.Controls.Add(Me.PanelHeader)
        Me.Name = "AttachedUtilityClass"
        Me.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Float
        Me.ShowIcon = False
        Me.PanelHeader.ResumeLayout(False)
        Me.PanelHeader.PerformLayout()
        Me.ResumeLayout(False)

    End Sub

    Private Sub tbName_TextChanged(sender As Object, e As EventArgs) Handles tbName.TextChanged
        Me.UtilityName = tbName.Text
        Me.Text = tbName.Text
        Me.TabText = tbName.Text
    End Sub

    Public Function GetUtilityType() As Interfaces.Enums.FlowsheetUtility Implements Interfaces.IAttachedUtility.GetUtilityType
        Return InternalUtility.GetUtilityType
    End Function

End Class
