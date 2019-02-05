Imports System
Imports System.Drawing
Imports System.Collections
Imports System.Windows.Forms

Namespace Controls

    Public Class ListViewEx

        Inherits ListView

        Private Const REORDER As String = "Reorder"

        Private _allowRowReorder As Boolean = True

        Public Property AllowRowReorder As Boolean
            Get
                Return Me._allowRowReorder
            End Get
            Set
                Me._allowRowReorder = Value
                MyBase.AllowDrop = Value
            End Set
        End Property

        Public Shadows Property Sorting As SortOrder
            Get
                Return SortOrder.None
            End Get
            Set
                MyBase.Sorting = SortOrder.None
            End Set
        End Property

        Public Sub New()
            MyBase.New
            Me.AllowRowReorder = True
        End Sub

        Protected Overrides Sub OnDragDrop(ByVal e As DragEventArgs)
            MyBase.OnDragDrop(e)
            If Not Me.AllowRowReorder Then
                Return
            End If

            If (MyBase.SelectedItems.Count = 0) Then
                Return
            End If

            Dim cp As Point = MyBase.PointToClient(New Point(e.X, e.Y))
            Dim dragToItem As ListViewItem = MyBase.GetItemAt(cp.X, cp.Y)
            If (dragToItem Is Nothing) Then
                Return
            End If

            Dim dropIndex As Integer = dragToItem.Index
            If (dropIndex > MyBase.SelectedItems(0).Index) Then
                dropIndex = (dropIndex + 1)
            End If

            Dim insertItems As ArrayList = New ArrayList(MyBase.SelectedItems.Count)
            For Each item As ListViewItem In MyBase.SelectedItems
                insertItems.Add(item.Clone)
            Next
            Dim i As Integer = (insertItems.Count - 1)
            Do While (i >= 0)
                Dim insertItem As ListViewItem = CType(insertItems(i), ListViewItem)
                MyBase.Items.Insert(dropIndex, insertItem)
                i = (i - 1)
            Loop

            For Each removeItem As ListViewItem In MyBase.SelectedItems
                MyBase.Items.Remove(removeItem)
            Next
        End Sub

        Protected Overrides Sub OnDragOver(ByVal e As DragEventArgs)
            If Not Me.AllowRowReorder Then
                e.Effect = DragDropEffects.None
                Return
            End If

            If Not e.Data.GetDataPresent(DataFormats.Text) Then
                e.Effect = DragDropEffects.None
                Return
            End If

            Dim cp As Point = MyBase.PointToClient(New Point(e.X, e.Y))
            Dim hoverItem As ListViewItem = MyBase.GetItemAt(cp.X, cp.Y)
            If (hoverItem Is Nothing) Then
                e.Effect = DragDropEffects.None
                Return
            End If

            For Each moveItem As ListViewItem In MyBase.SelectedItems
                If (moveItem.Index = hoverItem.Index) Then
                    e.Effect = DragDropEffects.None
                    hoverItem.EnsureVisible()
                    Return
                End If

            Next
            MyBase.OnDragOver(e)
            Dim text As String = CType(e.Data.GetData(REORDER.GetType), String)
            If (text.CompareTo(REORDER) = 0) Then
                e.Effect = DragDropEffects.Move
                hoverItem.EnsureVisible()
            Else
                e.Effect = DragDropEffects.None
            End If

        End Sub

        Protected Overrides Sub OnDragEnter(ByVal e As DragEventArgs)
            MyBase.OnDragEnter(e)
            If Not Me.AllowRowReorder Then
                e.Effect = DragDropEffects.None
                Return
            End If

            If Not e.Data.GetDataPresent(DataFormats.Text) Then
                e.Effect = DragDropEffects.None
                Return
            End If

            MyBase.OnDragEnter(e)
            Dim text As String = CType(e.Data.GetData(REORDER.GetType), String)
            If (text.CompareTo(REORDER) = 0) Then
                e.Effect = DragDropEffects.Move
            Else
                e.Effect = DragDropEffects.None
            End If

        End Sub

        Protected Overrides Sub OnItemDrag(ByVal e As ItemDragEventArgs)
            MyBase.OnItemDrag(e)
            If Not Me.AllowRowReorder Then
                Return
            End If

            MyBase.DoDragDrop(REORDER, DragDropEffects.Move)
        End Sub
    End Class

End Namespace
