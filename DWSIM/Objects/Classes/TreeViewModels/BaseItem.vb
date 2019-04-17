Imports System.Drawing
Imports System.IO
Imports Aga.Controls.Tree

Public MustInherit Class BaseNodeItem

    Public Property ItemID As String

    Public Property ItemName As String

    Public Property ItemMemberInfo As Reflection.MemberInfo

    Public Property ItemOwnerObject As Object

    Public Property ItemObject As Object

    Public Property ItemType As String

    Public Property ItemValue As String

    Public Property ItemIcon As Image

    Public Property Parent As BaseNodeItem

    Public Property Owner As TypeBrowserModel

    Public Property CodeFile As String

    Public Overrides Function ToString() As String

        Return Me.ItemName

    End Function

    Public Sub New()

        ItemID = Guid.NewGuid().ToString

    End Sub

End Class

Public Class RootNodeItem

    Inherits BaseNodeItem

    Public Sub New(ByVal name As String, ByVal owner As ITreeModel)

        MyBase.New

        ItemName = name

        owner = owner

    End Sub

End Class

Public Class TreeNodeItem

    Inherits BaseNodeItem

    Public Sub New(ByVal name As String, ByVal parent As BaseNodeItem, ByVal owner As TypeBrowserModel)

        MyBase.New

        ItemName = name
        parent = parent
        owner = owner

    End Sub

End Class