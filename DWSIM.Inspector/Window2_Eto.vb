Imports System.IO
Imports DWSIM.Interfaces
Imports Eto.Drawing
Imports Eto.Forms
Imports c = DWSIM.UI.Shared.Common

Public Class Window2_Eto

    Public Shared Function GetInspectorWindow(SelectedObject As ISimulationObject) As TableLayout

        'Layout

        Dim content As New TableLayout With {.Padding = New Padding(5, 5, 5, 5), .Spacing = New Size(10, 10)}

        Dim InspectorLabel = New Label With {.Text = "  Solution Inspector", .Font = SystemFonts.Bold(), .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorLabel.Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize())

        Dim InspectorDescription = New Label With {.Text = "  The Solution Inspector brings a human-readable, hierarchized view of all model calculations in a Flowsheet Solver run.", .VerticalAlignment = VerticalAlignment.Bottom, .TextColor = Colors.White, .Height = 20}
        InspectorDescription.Font = New Font(SystemFont.Default, DWSIM.UI.Shared.Common.GetEditorFontSize())

        content.Rows.Add(New TableRow(InspectorLabel))
        content.Rows.Add(New TableRow(InspectorDescription))
        content.BackgroundColor = New Color(0.051F, 0.447F, 0.651F)

        Dim leftcontainer, rightcontainer As New TableLayout

        leftcontainer.Rows.Add(New Label With {.VerticalAlignment = VerticalAlignment.Center, .Text = "Reports", .Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize()), .TextColor = Colors.White})
        leftcontainer.Padding = New Padding(5, 5, 5, 5)
        leftcontainer.Spacing = New Size(10, 10)
        leftcontainer.Width = 350

        Dim itemSelector As New TreeGridView()

        itemSelector.ShowHeader = False
        itemSelector.AllowMultipleSelection = False
        itemSelector.Columns.Add(New GridColumn With {.DataCell = New TextBoxCell(0)})

        leftcontainer.Rows.Add(itemSelector)

        rightcontainer.Padding = New Padding(5, 5, 5, 5)
        rightcontainer.Spacing = New Size(10, 10)

        Dim currentItemViewer As New WebView With {.BrowserContextMenuEnabled = True}


        Dim lblTools As New Label() With {.VerticalAlignment = VerticalAlignment.Center, .Text = "Selected Report", .Font = New Font(SystemFont.Bold, DWSIM.UI.Shared.Common.GetEditorFontSize()), .TextColor = Colors.White}
        Dim btnExportHTML As New Button() With {.Text = "Export to HTML File"}

        AddHandler btnExportHTML.Click, Sub()
                                            Dim Dialog As New SaveFileDialog()
                                            Dialog.Title = "Export HTML Page"
                                            Dialog.Filters.Add(New FileFilter("HTML File", {".htm"}))
                                            Dialog.CurrentFilterIndex = 0
                                            If Dialog.ShowDialog(content.ParentWindow) = DialogResult.Ok Then
                                                Dim nesteditems = GetItems(Host.Items.ToList)
                                                Dim sitem = nesteditems.Where(Function(x) x.ID = DirectCast(itemSelector.SelectedItem, TreeGridItem).Tag.ToString).FirstOrDefault
                                                File.WriteAllText(Dialog.FileName, sitem.GetHTML)
                                            End If
                                        End Sub

        Dim l1 As New TableLayout(New TableRow(lblTools, Nothing, btnExportHTML))
        l1.Spacing = New Size(10, 10)

        rightcontainer.Rows.Add(New TableRow(l1))

        rightcontainer.Rows.Add(New TableRow(currentItemViewer))

        rightcontainer.Rows(1).ScaleHeight = True

        Dim splitterpanel As New Splitter()

        splitterpanel.Panel1 = leftcontainer
        splitterpanel.Panel2 = rightcontainer
        splitterpanel.Orientation = Orientation.Horizontal
        splitterpanel.SplitterWidth = 1

        content.Rows.Add(splitterpanel)

        'Events

        Dim avsol As List(Of String) = Host.Items.Select(Of String)(Function(x) x.SolutionID).Distinct().ToList

        Dim f As New Eto.Forms.Form
        Dim loadingtext As Label
        Dim progressSpinner As ProgressBar
        Dim btnCancel As Button

        With f

            .Topmost = True
            .Title = "Loading Inspector Reports"

            progressSpinner = New ProgressBar With {.Width = 350, .Height = 20, .Enabled = True}
            loadingtext = New Label With {.Text = "Loading reports..."}
            loadingtext.VerticalAlignment = VerticalAlignment.Center
            loadingtext.TextAlignment = TextAlignment.Center
            btnCancel = New Button With {.Text = "Cancel"}

            Dim row1 = New TableLayout
            row1.Rows.Add(New TableRow(New TableCell() {Nothing, progressSpinner, Nothing}))
            Dim row3 = New TableLayout
            row3.Rows.Add(New TableRow(New TableCell() {loadingtext, Nothing, btnCancel}))
            Dim Container = New TableLayout With {.Spacing = New Size(5, 5), .Padding = New Padding(25, 10, 25, 10)}
            Container.Rows.Add(row1)
            Container.Rows.Add(row3)
            Container.Rows.Add(Nothing)

            .Content = Container

            .WindowStyle = Eto.Forms.WindowStyle.Default

            .ShowInTaskbar = False

            .Maximizable = False
            .Minimizable = False

            Dim w = 400
            Dim h = 80

            Dim center = Screen.PrimaryScreen.WorkingArea.Center
            center.X -= w / 2
            center.Y -= h / 2

            .Location = New Point(center)

            .ClientSize = New Size(w, h)

        End With

        'content.Enabled = False

        f.Show()

        Dim sitems = Host.Items.Where(Function(x) x.Name.Contains(SelectedObject.GraphicObject.Tag))

        Dim tvc As New TreeGridItemCollection()

        Dim ct As New Threading.CancellationTokenSource

        AddHandler btnCancel.Click, Sub()
                                        ct.Cancel()
                                    End Sub

        Dim allitems As New List(Of InspectorItem)

        Task.Factory.StartNew(Sub()
                                  Application.Instance.Invoke(Sub()
                                                                  allitems = GetItems(Host.Items.Where(Function(x) x.Name.Contains(SelectedObject.GraphicObject.Tag)).ToList)
                                                              End Sub)
                                  Dim i As Integer = 1
                                  For Each item In sitems.Where(Function(x) x.ParentID = -1)
                                      Dim timetaken = item.TimeTaken.TotalMilliseconds.ToString("N0") + " ms"
                                      If timetaken = "0 ms" Then timetaken = (item.TimeTaken.TotalMilliseconds * 1000).ToString("N0") + " µs"
                                      If timetaken = "0 µs" Then timetaken = (item.TimeTaken.TotalMilliseconds * 1000000).ToString("N0") + " ns"
                                      Dim titem = New TreeGridItem() With {.Values = {item.Name + " (" + timetaken + ")"}, .Tag = item.ID}
                                      tvc.Add(titem)
                                      Application.Instance.Invoke(Sub()
                                                                      loadingtext.Text = String.Format("Loading reports... ({0}/{1})", i, allitems.Count)
                                                                      progressSpinner.Value = i / allitems.Count * 100
                                                                      content.ParentWindow.Invalidate()
                                                                  End Sub)
                                      i += 1
                                      Dim nesteditems = GetItems(item)
                                      For Each item2 In nesteditems
                                          Dim parent = GetAllTreeItems(tvc).Where(Function(x) DirectCast(x, TreeGridItem).Tag = item2.ParentID).FirstOrDefault
                                          Dim timetaken2 = item2.TimeTaken.TotalMilliseconds.ToString("N0") + " ms"
                                          If timetaken2 = "0 ms" Then timetaken2 = (item2.TimeTaken.TotalMilliseconds * 1000).ToString("N0") + " µs"
                                          If timetaken2 = "0 µs" Then timetaken2 = (item2.TimeTaken.TotalMilliseconds * 1000000).ToString("N0") + " ns"
                                          Dim titem2 = New TreeGridItem() With {.Values = {item2.Name + " (" + timetaken2 + ")"}, .Tag = item2.ID}
                                          If parent Is Nothing Then
                                              tvc.Add(titem2)
                                          Else
                                              DirectCast(parent, TreeGridItem).Children.Add(titem2)
                                          End If
                                          Application.Instance.Invoke(Sub()
                                                                          loadingtext.Text = String.Format("Loading reports... ({0}/{1})", i, allitems.Count)
                                                                          progressSpinner.Value = i / allitems.Count * 100
                                                                          content.ParentWindow.Invalidate()
                                                                      End Sub)
                                          i += 1
                                          If ct.IsCancellationRequested Then Throw New TaskCanceledException()
                                      Next
                                  Next
                              End Sub, ct.Token).ContinueWith(Sub()
                                                                  Application.Instance.Invoke(Sub()
                                                                                                  itemSelector.DataStore = tvc
                                                                                                  'content.Enabled = True
                                                                                                  f.Close()
                                                                                                  itemSelector.SelectedRow = 0
                                                                                              End Sub)
                                                              End Sub)


        AddHandler itemSelector.SelectedItemChanged,
            Sub(sender, e)

                If itemSelector.SelectedItem IsNot Nothing Then
                    Dim nesteditems = GetItems(Host.Items.ToList)
                    Dim sitem = nesteditems.Where(Function(x) x.ID = DirectCast(itemSelector.SelectedItem, TreeGridItem).Tag.ToString).FirstOrDefault
                    If Not sitem Is Nothing Then
                        currentItemViewer.LoadHtml(sitem.GetHTML())
                    Else
                        MessageBox.Show("Selected report not found.")
                    End If
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
