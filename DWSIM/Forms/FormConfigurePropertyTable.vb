Imports System.Linq
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables

Public Class FormConfigurePropertyTable

    Public Table As TableGraphic

    Private loaded As Boolean = False

    Private MasterList As New List(Of ListViewItem)

    Private Sub FormSelectProperties_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        TextBox1.Text = Table.HeaderText

        lvObjects.Items.Clear()
        For Each obj In Table.Flowsheet.SimulationObjects.Values
            lvObjects.Items.Add(obj.GraphicObject.Tag).Tag = obj.Name
        Next

        lvObjects.Sorting = SortOrder.Ascending
        lvObjects.Sort()

        loaded = True

    End Sub

    Private Sub lvObjects_ItemSelectionChanged(sender As Object, e As ListViewItemSelectionChangedEventArgs) Handles lvObjects.ItemSelectionChanged

        If e.IsSelected Then
            MasterList.Clear()
            For Each item In Table.Flowsheet.SimulationObjects(e.Item.Tag).GetProperties(PropertyType.ALL)
                Dim lvi = New ListViewItem(Table.Flowsheet.GetTranslatedString(item))
                lvi.Tag = item
                If Table.VisibleProperties.ContainsKey(lvObjects.SelectedItems(0).Tag) Then
                    If Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Contains(item) Then
                        lvi.Checked = True
                    End If
                End If
                MasterList.Add(lvi)
            Next
        End If

        FilterList("")

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs)
        Me.Close()
    End Sub

    Private Sub lvProps_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles lvProps.ItemChecked

        If loaded Then
            If Not Table.VisibleProperties.ContainsKey(lvObjects.SelectedItems(0).Tag) Then
                Table.VisibleProperties.Add(lvObjects.SelectedItems(0).Tag, New List(Of String))
            End If

            If e.Item.Checked Then
                If Not Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Contains(e.Item.Tag) Then
                    Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Add(e.Item.Tag)
                End If
            Else
                If Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Contains(e.Item.Tag) Then
                    Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Remove(e.Item.Tag)
                End If
            End If
        End If

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        If loaded Then Table.HeaderText = TextBox1.Text
    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles Button1.Click
        For Each obj In lvProps.Items
            DirectCast(obj, ListViewItem).Checked = True
        Next
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        For Each obj In lvProps.Items
            DirectCast(obj, ListViewItem).Checked = False
        Next
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Dim props() As String = Nothing

        props = Table.Flowsheet.SimulationObjects(lvObjects.SelectedItems(0).Tag).GetDefaultProperties
        For Each lvi As ListViewItem In lvProps.Items
            lvi.Checked = False
        Next
        For Each lvi As ListViewItem In lvProps.Items
            If props.Contains(lvi.Tag) Then lvi.Checked = True
        Next
    End Sub

    Private Sub FormConfigurePropertyTable_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        ColumnHeader1.Width = lvObjects.Width - 15
        ColumnHeader2.Width = lvProps.Width - 15
        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub tbSearch_TextChanged(sender As Object, e As EventArgs) Handles tbSearch.TextChanged

        FilterList(tbSearch.Text)

    End Sub

    Public Sub FilterList(searchterms As String)

        lvProps.Items.Clear()

        For Each item In MasterList.Where(Function(lvi)
                                              Dim words = lvi.Text.ToLower().Trim().Split(" ").ToList()
                                              Dim terms = searchterms.ToLower().Trim().Split(" ").ToList()
                                              Return terms.All(Function(w) lvi.Text.ToLower().Contains(w))
                                          End Function)
            lvProps.Items.Add(item)
        Next

    End Sub

End Class