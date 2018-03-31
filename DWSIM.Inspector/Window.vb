Imports Eto.Drawing
Imports Eto.Forms
Imports c = DWSIM.UI.Shared.Common

Public Class Window

    Public Shared Function GetInspectorWindow() As TableLayout

        'Layout

        Dim content As New TableLayout With {.Padding = New Padding(5, 5, 5, 5)}

        Dim InspectorLabel = New Label With {.Text = "Solution Inspector", .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim InspectorDescription = New Label With {.Text = "The Solution Inspector brings a human-readable, hierarchized view of all model calculations in a Flowsheet Solver run.", .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorDescription.Font = New Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize())

        content.Rows.Add(New TableRow(InspectorLabel))
        content.Rows.Add(New TableRow(InspectorDescription))
        content.BackgroundColor = New Color(0.051F, 0.447F, 0.651F)

        Dim SetsBox As New ListBox With {.Height = 100}

        Dim SetsLabel = New Label With {.Text = "Available Solution Sets", .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        SetsLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim leftcontainer, rightcontainer As New TableLayout

        leftcontainer.Rows.Add(New TableRow(SetsLabel))
        leftcontainer.Rows.Add(New TableRow(SetsBox))
        leftcontainer.Rows.Add(New Label With {.Text = "Items", .Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize()), .TextColor = Colors.White})
        leftcontainer.Padding = New Padding(5, 5, 5, 5)
        leftcontainer.Spacing = New Size(10, 10)
        leftcontainer.Width = 350

        Dim itemSelector As New TreeGridView()

        itemSelector.ShowHeader = False
        itemSelector.AllowMultipleSelection = False
        itemSelector.Columns.Add(New GridColumn With {.DataCell = New TextBoxCell(0)})

        leftcontainer.Rows.Add(itemSelector)

        rightcontainer.Rows.Add(New Label With {.Text = "Selected Item", .Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize()), .TextColor = Colors.White})
        rightcontainer.Padding = New Padding(5, 5, 5, 5)
        rightcontainer.Spacing = New Size(10, 10)

        Dim currentItemViewer As New WebView With {.BrowserContextMenuEnabled = True}

        rightcontainer.Rows.Add(New TableRow(currentItemViewer))

        content.Rows.Add(New TableLayout(New TableRow(leftcontainer, rightcontainer)))

        'Events

        Dim avsol As List(Of String) = Host.Items.Select(Of String)(Function(x) x.SolutionID).Distinct().ToList

        For Each item In avsol
            SetsBox.Items.Add(New ListItem() With {.Text = Date.FromBinary(item).ToString(), .Key = item})
        Next

        AddHandler SetsBox.SelectedIndexChanged,
            Sub(sender, e)

                Dim sitems = Host.Items.Where(Function(x) x.SolutionID = SetsBox.SelectedKey.ToString).ToList

                Dim tvc As New TreeGridItemCollection()

                For Each item In sitems.Where(Function(x) x.ParentID = -1)
                    Dim titem = New TreeGridItem() With {.Values = {item.Name}, .Tag = item.ID}
                    tvc.Add(titem)
                    Dim nesteditems = GetItems(item)
                    For Each item2 In nesteditems
                        Dim parent = GetAllTreeItems(tvc).Where(Function(x) DirectCast(x, TreeGridItem).Tag = item2.ParentID).FirstOrDefault
                        Dim titem2 = New TreeGridItem() With {.Values = {item2.Name}, .Tag = item2.ID}
                        If parent Is Nothing Then
                            tvc.Add(titem2)
                        Else
                            DirectCast(parent, TreeGridItem).Children.Add(titem2)
                        End If
                    Next
                Next

                itemSelector.DataStore = tvc

            End Sub

        AddHandler itemSelector.SelectedItemChanged,
            Sub(sender, e)

                If itemSelector.SelectedItem IsNot Nothing Then
                    Dim nesteditems = GetItems(Host.Items.ToList)
                    Dim sitem = nesteditems.Where(Function(x) x.ID = DirectCast(itemSelector.SelectedItem, TreeGridItem).Tag.ToString).FirstOrDefault
                    currentItemViewer.LoadHtml(sitem.GetHTML())
                End If

            End Sub

        Return content

    End Function

    Public Shared Function GetItems(ByVal list As List(Of InspectorItem)) As List(Of InspectorItem)
        Dim myItems As List(Of InspectorItem) = New List(Of InspectorItem)()
        For Each i As InspectorItem In list
            GetInspectorItems(i, myItems)
        Next

        Return myItems
    End Function

    Public Shared Function GetItems(ByVal iitem As InspectorItem) As List(Of InspectorItem)
        Dim myItems As List(Of InspectorItem) = New List(Of InspectorItem)()
        For Each i As InspectorItem In iitem.Items
            GetInspectorItems(i, myItems)
        Next

        Return myItems
    End Function

    Private Shared Sub GetInspectorItems(ByVal item As InspectorItem, ByVal items As List(Of InspectorItem))
        items.Add(item)
        For Each i As InspectorItem In item.Items
            GetInspectorItems(i, items)
        Next
    End Sub

    Public Shared Function GetAllTreeItems(ByVal iitem As TreeGridItemCollection) As List(Of TreeGridItem)
        Dim myItems As List(Of TreeGridItem) = New List(Of TreeGridItem)()
        For Each i As TreeGridItem In iitem
            GetTreeItems(i, myItems)
        Next
        Return myItems
    End Function

    Private Shared Sub GetTreeItems(ByVal item As TreeGridItem, ByVal items As List(Of TreeGridItem))
        items.Add(item)
        For Each i As TreeGridItem In item.Children
            GetTreeItems(i, items)
        Next
    End Sub

End Class
