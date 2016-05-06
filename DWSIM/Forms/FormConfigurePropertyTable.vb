Public Class FormConfigurePropertyTable

    Public Table As GraphicObjects.TableGraphic

    Private loaded As Boolean = False

    Private Sub FormSelectProperties_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        TextBox1.Text = Table.HeaderText

        lvObjects.Items.Clear()
        For Each obj In Table.Flowsheet.SimulationObjects.Values
            lvObjects.Items.Add(obj.GraphicObject.Tag).Tag = obj.Name
        Next

        loaded = True

    End Sub

    Private Sub lvObjects_ItemSelectionChanged(sender As Object, e As ListViewItemSelectionChangedEventArgs) Handles lvObjects.ItemSelectionChanged
        If e.IsSelected Then
            lvProps.Items.Clear()
            For Each item In Table.Flowsheet.SimulationObjects(e.Item.Tag).GetProperties(PropertyType.ALL)
                Dim lvi = New ListViewItem(DWSIM.App.GetPropertyName(item))
                lvi.Tag = item
                If Table.VisibleProperties.ContainsKey(lvObjects.SelectedItems(0).Tag) Then
                    If Table.VisibleProperties(lvObjects.SelectedItems(0).Tag).Contains(item) Then
                        lvi.Checked = True
                    End If
                End If
                lvProps.Items.Add(lvi)
            Next
        End If
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
End Class