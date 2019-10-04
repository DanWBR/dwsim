Imports Controls
Imports DWSIM.Controls

Module Extensions

    <System.Runtime.CompilerServices.Extension>
    Public Function EnumerateAllItems(grid As PropertyGrid) As IEnumerable(Of GridItem)
        Dim start As GridItem = grid.SelectedGridItem
        While start.Parent IsNot Nothing
            start = start.Parent
        End While
        Dim list As New List(Of GridItem)
        For Each item As GridItem In start.EnumerateAllItems()
            list.Add(item)
        Next
        Return list
    End Function

    <System.Runtime.CompilerServices.Extension>
    Public Function EnumerateAllItems(item As GridItem) As IEnumerable(Of GridItem)
        Dim list As New List(Of GridItem)
        list.Add(item)
        For Each child As GridItem In item.GridItems
            For Each gc As GridItem In child.EnumerateAllItems()
                list.Add(gc)
            Next
        Next
        Return list
    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Sub ExpandGroup(propertyGrid As PropertyGridEx.PropertyGridEx, groupName As String)

        Dim root As GridItem = propertyGrid.SelectedGridItem
        'Get the parent
        While root.Parent IsNot Nothing
            root = root.Parent
        End While

        If root IsNot Nothing Then
            For Each g As GridItem In root.GridItems
                For Each g2 As GridItem In g.GridItems
                    If g2.Label = groupName Then
                        g2.Expanded = True
                        Exit For
                    End If
                Next
            Next
        End If

    End Sub

End Module
