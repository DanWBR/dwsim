Public Class FormConfigureMasterTable

    Public Table As GraphicObjects.MasterTableGraphic

    Private Loaded As Boolean = False

    Private Sub FormConfigureMasterTable_FormClosed(sender As Object, e As FormClosedEventArgs) Handles Me.FormClosed
        Table.Update()
    End Sub

    Private Sub FormConfigureMasterTable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        cbObjectType.Items.AddRange([Enum].GetNames(Table.ObjectType.GetType))
        cbOrderBy.Items.AddRange(Table.SortableItems)

        cbObjectType.SelectedItem = Table.ObjectFamily.ToString
        cbOrderBy.SelectedItem = Table.SortBy

        TextBox1.Text = Table.HeaderText

        Populate()

        Loaded = True

    End Sub

    Sub Populate()

        lvObjects.Items.Clear()
        For Each obj In Table.Flowsheet.SimulationObjects.Values
            If obj.GraphicObject.ObjectType = Table.ObjectFamily Then
                If Not Table.ObjectList.ContainsKey(obj.GraphicObject.Tag) Then Table.ObjectList.Add(obj.GraphicObject.Tag, False)
                Dim lvi As New ListViewItem(obj.GraphicObject.Tag)
                lvi.Tag = "Object|" & obj.Name
                If Table.ObjectList.ContainsKey(obj.GraphicObject.Tag) Then
                    lvi.Checked = Table.ObjectList(obj.GraphicObject.Tag)
                End If
                lvObjects.Items.Add(lvi)
            End If
        Next

        Dim props() As String = Nothing

        lvProps.Items.Clear()
        If Table.ObjectList.Count > 0 Then
            For Each s As String In Table.ObjectList.Keys
                props = Table.Flowsheet.GetFlowsheetSimulationObject(s).GetProperties(PropertyType.ALL)
                Exit For
            Next
            For Each p As String In props
                If Not Table.PropertyList.ContainsKey(p) Then Table.PropertyList.Add(p, False)
                Dim lvi As New ListViewItem(DWSIM.App.GetPropertyName(p))
                lvi.Tag = p
                If Table.PropertyList.ContainsKey(p) Then
                    lvi.Checked = Table.PropertyList(p)
                End If
                lvProps.Items.Add(lvi)
            Next
        End If

    End Sub

    Private Sub cbObjectType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbObjectType.SelectedIndexChanged

        If Loaded Then
            Table.ObjectFamily = [Enum].Parse(Table.ObjectType.GetType, cbObjectType.SelectedItem.ToString)
            Table.ObjectList.Clear()
            Table.SortedList.Clear()
            Table.PropertyList.Clear()
            Populate()
        End If

    End Sub


    Private Sub cbOrderBy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOrderBy.SelectedIndexChanged

        If Loaded Then
            Table.SortBy = cbOrderBy.SelectedItem.ToString
            Populate()
        End If

    End Sub

    Private Sub lvObjects_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles lvObjects.ItemChecked

        If Loaded Then
            If Table.ObjectList.ContainsKey(e.Item.Text) Then
                Table.ObjectList(e.Item.Text) = e.Item.Checked
            End If
        End If

    End Sub

    Private Sub lvProps_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles lvProps.ItemChecked

        If Loaded Then
            If Table.PropertyList.ContainsKey(e.Item.Tag) Then
                Table.PropertyList(e.Item.Tag) = e.Item.Checked
            End If
        End If

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        Table.HeaderText = TextBox1.Text
    End Sub
End Class