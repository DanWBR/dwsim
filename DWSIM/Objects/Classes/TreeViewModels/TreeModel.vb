Imports System.ComponentModel
Imports System.IO
Imports Aga.Controls.Tree
Imports System.Linq

Public Class TypeBrowserModel

    Implements ITreeModel

    Public Event NodesChanged As EventHandler(Of TreeModelEventArgs) Implements ITreeModel.NodesChanged

    Public Event NodesInserted As EventHandler(Of TreeModelEventArgs) Implements ITreeModel.NodesInserted

    Public Event NodesRemoved As EventHandler(Of TreeModelEventArgs) Implements ITreeModel.NodesRemoved

    Public Event StructureChanged As EventHandler(Of TreePathEventArgs) Implements ITreeModel.StructureChanged

    Private _itemsToRead As New List(Of BaseNodeItem)

    Private _cache As Dictionary(Of String, List(Of BaseNodeItem)) = New Dictionary(Of String, List(Of BaseNodeItem))

    Private _rootobjs As List(Of Object)

    Private _rootnames As List(Of String)

    Public Sub New(rootobjs As List(Of Object), names As List(Of String))

        MyBase.New

        _rootobjs = rootobjs
        _rootnames = names

    End Sub

    Private Sub ReadItemProperties(item As BaseNodeItem)

        With item

            Select Case .ItemMemberInfo.MemberType
                Case Reflection.MemberTypes.Constructor
                    .ItemName += " ()"
                    .ItemValue = ""
                    .ItemType = "Constructor"
                    .ItemIcon = My.Resources.bullet_black
                Case Reflection.MemberTypes.Event
                    .ItemValue = ""
                    .ItemType = "Event"
                    .ItemIcon = My.Resources.bullet_orange
                Case Reflection.MemberTypes.Field
                    .ItemValue = .ItemObject?.ToString()
                    .ItemType = "Field"
                    .ItemIcon = My.Resources.bullet_blue
                Case Reflection.MemberTypes.Method
                    .ItemName += " ()"
                    .ItemValue = ""
                    .ItemType = "Method"
                    .ItemIcon = My.Resources.bullet_red
                Case Reflection.MemberTypes.NestedType
                    Try
                        .ItemValue = .ItemOwnerObject.GetType().GetNestedType(.ItemName).ToString
                    Catch ex As Exception
                    End Try
                    .ItemType = "Nested Type"
                    .ItemIcon = My.Resources.bullet_purple
                Case Reflection.MemberTypes.Property
                    .ItemValue = .ItemObject?.ToString()
                    .ItemType = "Property"
                    .ItemIcon = My.Resources.bullet_green
                Case Reflection.MemberTypes.Custom
                    .ItemValue = .ItemObject?.ToString()
                    .ItemType = .ItemMemberInfo.GetType().ToString
                    .ItemIcon = My.Resources.bullet_yellow
            End Select

        End With

    End Sub

    Public Function GetChildren(ByVal treePath As TreePath) As System.Collections.IEnumerable Implements ITreeModel.GetChildren

        Dim items As List(Of BaseNodeItem) = Nothing

        If treePath.IsEmpty Then

            If _cache.ContainsKey("ROOT") Then

                items = _cache("ROOT")

            Else

                items = New List(Of BaseNodeItem)

                _cache.Add("ROOT", items)

                Dim rootobjs As New List(Of RootNodeItem)

                Dim i As Integer = 0

                For Each var In _rootobjs
                    Dim ro As New RootNodeItem(_rootnames(i), Me)
                    With ro
                        .ItemObject = var
                        .ItemType = var.GetType.ToString
                        .ItemValue = var.ToString
                    End With
                    items.Add(ro)
                    i += 1
                Next

                items = items.OrderBy(Function(x) x.ItemName).ToList

            End If

        Else

            Dim parent As BaseNodeItem = CType(treePath.LastNode, BaseNodeItem)

            If (Not (parent) Is Nothing) Then

                If _cache.ContainsKey(parent.ItemID) Then

                    items = _cache(parent.ItemID)

                Else

                    items = New List(Of BaseNodeItem)

                    Try

                        If TryCast(parent.ItemObject, System.Dynamic.ExpandoObject) IsNot Nothing Then

                            Dim col = DirectCast(parent.ItemObject, System.Dynamic.ExpandoObject)

                            For Each item In col

                                If item.Value IsNot Nothing Then

                                    Dim nitem = New TreeNodeItem(item.Key, parent, Me)

                                    With nitem

                                        .ItemOwnerObject = parent.ItemObject
                                        .ItemMemberInfo = parent.ItemMemberInfo
                                        .ItemObject = item.Value
                                        .ItemValue = item.Value.ToString
                                        .ItemType = item.Value.GetType.ToString

                                    End With

                                    items.Add(nitem)

                                End If

                            Next

                            items = items.OrderBy(Function(x) x.ItemName).ToList

                        ElseIf TryCast(parent.ItemObject, IList) IsNot Nothing Then

                            Dim col = DirectCast(parent.ItemObject, IList)

                            For Each item In col

                                Dim nitem = New TreeNodeItem(item.GetType.Name, parent, Me)

                                With nitem

                                    .ItemOwnerObject = parent.ItemObject
                                    .ItemMemberInfo = parent.ItemMemberInfo
                                    .ItemValue = item.ToString
                                    .ItemObject = item
                                    .ItemType = item.GetType.ToString

                                End With

                                items.Add(nitem)

                            Next

                            items = items.OrderBy(Function(x) x.ItemName).ToList

                        ElseIf TryCast(parent.ItemObject, IDictionary) IsNot Nothing Then

                            Dim col = DirectCast(parent.ItemObject, IDictionary)

                            For Each item As DictionaryEntry In col

                                If item.Value IsNot Nothing Then

                                    Dim nitem = New TreeNodeItem(item.Value.GetType.Name, parent, Me)

                                    With nitem

                                        .ItemOwnerObject = parent.ItemObject
                                        .ItemMemberInfo = parent.ItemMemberInfo
                                        .ItemObject = item.Value
                                        .ItemValue = item.Key.ToString
                                        .ItemType = item.GetType.ToString

                                    End With

                                    items.Add(nitem)

                                End If

                            Next

                            items = items.OrderBy(Function(x) x.ItemName).ToList

                        Else

                            If TypeOf parent.ItemObject Is System.String OrElse
                                TypeOf parent.ItemObject Is System.Double OrElse
                                TypeOf parent.ItemObject Is System.Boolean OrElse
                                TypeOf parent.ItemObject Is System.Single OrElse
                                TypeOf parent.ItemObject Is System.Decimal OrElse
                                TypeOf parent.ItemObject Is System.Int32 Then
                                Exit Try
                            End If

                            Dim members = parent.ItemObject.GetType().GetMembers(Reflection.BindingFlags.Public Or Reflection.BindingFlags.Instance)

                            For Each str As Reflection.MemberInfo In members

                                If Not str.Name.StartsWith("get_") And Not str.Name.StartsWith("set_") And
                                        Not str.Name.EndsWith("_Click") And
                                        Not str.Name.StartsWith("add_") And Not str.Name.StartsWith("remove_") And
                                        Not str.DeclaringType().Assembly.FullName.Contains("System.Windows.Forms") Then

                                    Dim nitem = New TreeNodeItem(str.Name, parent, Me)

                                    With nitem

                                        .ItemOwnerObject = parent.ItemObject
                                        .ItemMemberInfo = str
                                        Select Case str.MemberType
                                            Case Reflection.MemberTypes.Field
                                                Try
                                                    .ItemObject = .ItemOwnerObject.GetType().GetField(.ItemName).GetValue(.ItemOwnerObject)
                                                Catch ex As Exception
                                                End Try
                                                If TryCast(.ItemObject, IDictionary) IsNot Nothing Then
                                                    Dim col = DirectCast(.ItemObject, IDictionary)
                                                    Dim l As New Dictionary(Of String, Object)
                                                    For Each item As DictionaryEntry In col
                                                        l.Add(item.Key, item.Value)
                                                    Next
                                                    .ItemObject = l
                                                ElseIf TryCast(.ItemObject, IList) IsNot Nothing Then
                                                    Dim col = DirectCast(.ItemObject, IList)
                                                    Dim l As New List(Of Object)
                                                    For Each item In col
                                                        l.Add(item)
                                                    Next
                                                    .ItemObject = l
                                                End If
                                            Case Reflection.MemberTypes.Property
                                                Try
                                                    .ItemObject = .ItemOwnerObject.GetType().GetProperty(.ItemName).GetValue(.ItemOwnerObject)
                                                Catch ex As Exception
                                                End Try
                                                If TryCast(.ItemObject, IDictionary) IsNot Nothing Then
                                                    Dim col = DirectCast(.ItemObject, IDictionary)
                                                    Dim l As New Dictionary(Of String, Object)
                                                    For Each item As DictionaryEntry In col
                                                        l.Add(item.Key, item.Value)
                                                    Next
                                                    .ItemObject = l
                                                ElseIf TryCast(.ItemObject, IList) IsNot Nothing Then
                                                    Dim col = DirectCast(.ItemObject, IList)
                                                    Dim l As New List(Of Object)
                                                    For Each item In col
                                                        l.Add(item)
                                                    Next
                                                    .ItemObject = l
                                                End If
                                        End Select

                                    End With

                                    ReadItemProperties(nitem)

                                    items.Add(nitem)

                                End If

                            Next

                            items = items.OrderBy(Function(x) x.ItemName).ToList

                        End If

                    Catch ex As IOException

                        Return Nothing

                    End Try

                    _cache.Add(parent.ItemID, items)

                    _itemsToRead.AddRange(items)

                End If

            End If

        End If

        Return items

    End Function

    Public Function IsLeaf(ByVal treePath As TreePath) As Boolean Implements ITreeModel.IsLeaf

        Dim ni = DirectCast(treePath.LastNode, BaseNodeItem)
        Dim mi = ni.ItemMemberInfo
        If Not mi Is Nothing Then
            Select Case mi.MemberType
                Case Reflection.MemberTypes.Field, Reflection.MemberTypes.Property
                    If ni.ItemObject Is Nothing Then
                        Return True
                    Else
                        Return False
                    End If
                Case Else
                    Return True
            End Select
        Else
            If TypeOf treePath.LastNode Is RootNodeItem Then
                Return False
            Else
                Return True
            End If
        End If

    End Function

    Private Function GetPath(ByVal item As BaseNodeItem) As TreePath

        If (item Is Nothing) Then

            Return TreePath.Empty

        Else

            Dim stack As Stack(Of Object) = New Stack(Of Object)

            While (Not (item) Is Nothing)

                stack.Push(item)
                item = item.Parent

            End While

            Return New TreePath(stack.ToArray)

        End If

    End Function

    Friend Sub OnNodesChanged(ByVal item As BaseNodeItem)

        Dim path As TreePath = GetPath(item.Parent)

        RaiseEvent NodesChanged(Me, New TreeModelEventArgs(path, New Object() {item}))

    End Sub

    Public Sub OnStructureChanged()

        RaiseEvent StructureChanged(Me, New TreePathEventArgs)

    End Sub

    Private Sub ProgressChanged(ByVal sender As Object, ByVal e As ProgressChangedEventArgs)

        OnNodesChanged(CType(e.UserState, BaseNodeItem))

    End Sub

End Class
