Imports Eto.Drawing
Imports Eto.Forms
Imports c = DWSIM.UI.Shared.Common

Public Class Window

    Public Shared Function GetInspectorWindow() As TableLayout

        'Layout

        Dim content As New TableLayout With {.Padding = New Padding(5, 5, 5, 5)}

        Dim InspectorLabel = New Label With {.Text = "  " + "Solution Inspector", .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim InspectorDescription = New Label With {.Text = "  " + "The Solution Inspector brings a structured view of all model calculations.", .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorDescription.Font = New Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize())

        content.Rows.Add(New TableRow(InspectorLabel))
        content.Rows.Add(New TableRow(InspectorDescription))
        content.BackgroundColor = New Color(0.051F, 0.447F, 0.651F)

        Dim SetsBox As New ListBox

        content.Rows.Add(New TableRow(New GroupBox() With {.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize()), .TextColor = Colors.White, .Content = SetsBox, .Height = 100, .Text = "Available Solution Sets"}))

        Dim leftcontainer, rightcontainer As New TableLayout

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
            SetsBox.Items.Add(item)
        Next

        AddHandler SetsBox.SelectedIndexChanged,
            Sub(sender, e)

                Dim sitems = Host.Items.Where(Function(x) x.SolutionID = SetsBox.SelectedValue.ToString).ToList

                Dim tvc As New TreeGridItemCollection()

                For Each item In sitems
                    Dim titem = New TreeGridItem() With {.Values = {item.Name}, .Tag = item.ID}
                    tvc.Add(titem)
                Next

                itemSelector.DataStore = tvc

            End Sub

        AddHandler itemSelector.SelectedItemChanged,
            Sub(sender, e)

                If itemSelector.SelectedItem IsNot Nothing Then
                    Dim sitem = Host.Items.Where(Function(x) x.ID = DirectCast(itemSelector.SelectedItem, TreeGridItem).Tag.ToString).FirstOrDefault
                    currentItemViewer.LoadHtml(sitem.GetHTML())
                End If

            End Sub

        Return content

    End Function



End Class
