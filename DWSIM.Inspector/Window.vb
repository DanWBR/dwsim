﻿Imports System.Windows.Forms
Imports DWSIM.ExtensionMethods
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class Window
    Private Sub Window_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Viewer.EnsureCoreWebView2Async()

        SetupInspectorWindow()

    End Sub

    Public Sub SetupInspectorWindow()

        lblDisabled.Visible = Not GlobalSettings.Settings.InspectorEnabled

        'Events

        Dim avsol As List(Of String) = Host.Items.Select(Of String)(Function(x) x.SolutionID).Distinct().ToList

        For Each item In avsol
            If item <> "" Then SetsBox.Items.Add(Date.FromBinary(item).ToString("O"))
        Next

        AddHandler SetsBox.SelectedIndexChanged,
            Sub(sender, e)

                Dim f As New Loading

                With f

                    .Label1.Text = "Loading reports..."
                    .ProgressBar1.Value = 0

                End With

                Dim sitems = Host.Items.Where(Function(x) x.SolutionID = Date.Parse(SetsBox.SelectedItem.ToString).ToBinary).ToList

                f.Show()

                Me.Enabled = False

                Dim tvc As New List(Of TreeNode)

                Dim ct As New Threading.CancellationTokenSource

                AddHandler f.btnCancel.Click, Sub()
                                                  ct.Cancel()
                                              End Sub

                Dim allitems As New List(Of InspectorItem)

                Task.Factory.StartNew(Sub()
                                          Me.UIThread(Sub()
                                                          allitems = GetItems(Host.Items.Where(Function(x) x.SolutionID = Date.Parse(SetsBox.SelectedItem.ToString).ToBinary).ToList)
                                                      End Sub)
                                          Dim i As Integer = 1
                                          For Each item In sitems.Where(Function(x) x.ParentID = -1)
                                              Dim timetaken = item.TimeTaken.TotalMilliseconds.ToString("N0") + " ms"
                                              If timetaken = "0 ms" Then timetaken = (item.TimeTaken.TotalMilliseconds * 1000).ToString("N0") + " µs"
                                              If timetaken = "0 µs" Then timetaken = (item.TimeTaken.TotalMilliseconds * 1000000).ToString("N0") + " ns"
                                              Dim titem = New TreeNode() With {.Text = item.Name + " (" + timetaken + ")", .Tag = item.ID}
                                              tvc.Add(titem)
                                              Me.UIThread(Sub()
                                                              f.Label1.Text = String.Format("Loading reports... ({0}/{1})", i, allitems.Count)
                                                              f.ProgressBar1.Value = CDbl(i / allitems.Count) * 100
                                                              i += 1
                                                          End Sub)
                                              Dim nesteditems = GetItems(item)
                                              For Each item2 In nesteditems
                                                  Dim parent = GetAllTreeItems(tvc).Where(Function(x) DirectCast(x, TreeNode).Tag = item2.ParentID).FirstOrDefault
                                                  Dim timetaken2 = item2.TimeTaken.TotalMilliseconds.ToString("N0") + " ms"
                                                  If timetaken2 = "0 ms" Then timetaken2 = (item2.TimeTaken.TotalMilliseconds * 1000).ToString("N0") + " µs"
                                                  If timetaken2 = "0 µs" Then timetaken2 = (item2.TimeTaken.TotalMilliseconds * 1000000).ToString("N0") + " ns"
                                                  Dim titem2 = New TreeNode() With {.Text = item2.Name + " (" + timetaken2 + ")", .Tag = item2.ID}
                                                  If parent Is Nothing Then
                                                      tvc.Add(titem2)
                                                  Else
                                                      DirectCast(parent, TreeNode).Nodes.Add(titem2)
                                                  End If
                                                  Me.UIThread(Sub()
                                                                  f.Label1.Text = String.Format("Loading reports... ({0}/{1})", i, allitems.Count)
                                                                  f.ProgressBar1.Value = CDbl(i / allitems.Count) * 100
                                                                  i += 1
                                                              End Sub)
                                                  If ct.IsCancellationRequested Then Throw New TaskCanceledException()
                                              Next
                                          Next
                                      End Sub, ct.Token).ContinueWith(Sub()
                                                                          Me.UIThread(Sub()
                                                                                          itemSelector.Nodes.AddRange(tvc.ToArray)
                                                                                          Me.Enabled = True
                                                                                          f.Close()
                                                                                      End Sub)
                                                                      End Sub)


            End Sub

        AddHandler itemSelector.AfterSelect,
            Sub(sender, e)

                If itemSelector.SelectedNode IsNot Nothing Then
                    Dim nesteditems = GetItems(Host.Items.ToList)
                    Dim sitem = nesteditems.Where(Function(x) x.ID = DirectCast(itemSelector.SelectedNode, TreeNode).Tag.ToString).FirstOrDefault
                    If Not sitem Is Nothing Then
                        Viewer.NavigateToString(sitem.GetHTML())
                    Else
                        MessageBox.Show("Selected report not found.")
                    End If
                End If

            End Sub

    End Sub

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

    Public Shared Function GetAllTreeItems(ByVal iitem As List(Of TreeNode)) As List(Of TreeNode)
        Dim myItems As List(Of TreeNode) = New List(Of TreeNode)()
        For Each i As TreeNode In iitem
            GetTreeItems(i, myItems)
        Next
        Return myItems
    End Function

    Private Shared Sub GetTreeItems(ByVal item As TreeNode, ByVal items As List(Of TreeNode))
        items.Add(item)
        For Each i As TreeNode In item.Nodes
            GetTreeItems(i, items)
        Next
    End Sub

    Private Async Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("HTML File", "*.html")})

        If handler IsNot Nothing Then
            Try
                Dim html As String
                html = Await Viewer.ExecuteScriptAsync("document.documentElement.outerHTML;")
                html = System.Text.RegularExpressions.Regex.Unescape(html)
                html = html.Remove(0, 1)
                html = html.Remove(html.Length - 1, 1)
                Using stream As New IO.MemoryStream()
                    Using writer As New IO.StreamWriter(stream) With {.AutoFlush = True}
                        writer.Write(html)
                        handler.Write(stream)
                    End Using
                End Using
            Catch ex As Exception
                MessageBox.Show(ex.Message,
                                "Error",
                                MessageBoxButtons.OK,
                                MessageBoxIcon.Error)
            End Try
        End If

    End Sub

End Class