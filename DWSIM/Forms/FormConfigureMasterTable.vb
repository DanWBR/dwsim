Imports System.Linq
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables

Public Class FormConfigureMasterTable

    Public Table As MasterTableGraphic

    Private Loaded As Boolean = False

    Private Sub FormConfigureMasterTable_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Dim list As New List(Of String)
        For Each lvi As ListViewItem In Me.lvObjects.Items
            list.Add(lvi.Text)
        Next

        Table.SortedList = list

    End Sub

    Private Sub FormConfigureMasterTable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        cbObjectType.Items.AddRange([Enum].GetNames(Table.ObjectType.GetType))
        cbOrderBy.Items.AddRange(Table.SortableItems)

        cbObjectType.SelectedItem = Table.ObjectFamily.ToString
        cbOrderBy.SelectedItem = Table.SortBy

        If Table.SortBy = "Custom" Then
            lblOrder.Enabled = True
            btnOrderDown.Enabled = True
            btnOrderUp.Enabled = True
        Else
            lblOrder.Enabled = False
            btnOrderDown.Enabled = False
            btnOrderUp.Enabled = False
        End If

        TextBox1.Text = Table.HeaderText

        nupLines.Value = Table.NumberOfLines

        Populate()

        Loaded = True

    End Sub

    Sub Populate()

        lvObjects.Items.Clear()

        If Table.SortBy = "Custom" Then
            For Each item In Table.SortedList
                If Table.Flowsheet.GetFlowsheetSimulationObject(item) IsNot Nothing Then
                    Dim lvi As New ListViewItem(item)
                    lvi.Checked = True
                    lvObjects.Items.Add(lvi)
                End If
            Next
            For Each obj In Table.Flowsheet.SimulationObjects.Values
                If obj.GraphicObject.ObjectType = Table.ObjectFamily And Not Table.SortedList.Contains(obj.GraphicObject.Tag) Then
                    If Not Table.ObjectList.ContainsKey(obj.GraphicObject.Tag) Then Table.ObjectList.Add(obj.GraphicObject.Tag, False)
                    Dim lvi As New ListViewItem(obj.GraphicObject.Tag)
                    lvi.Tag = "Object|" & obj.Name
                    lvObjects.Items.Add(lvi)
                End If
            Next
        Else
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
        End If

        Dim props() As String = Nothing

        lvProps.Items.Clear()
        If Table.ObjectList.Count > 0 Then
            For Each s As String In Table.ObjectList.Keys
                props = Table.Flowsheet.GetFlowsheetSimulationObject(s).GetProperties(PropertyType.ALL)
                Exit For
            Next
            For Each p As String In props
                If Not Table.PropertyList.ContainsKey(p) Then Table.PropertyList.Add(p, False)
                Dim lvi As New ListViewItem(Table.Flowsheet.GetTranslatedString(p))
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
            If Table.SortBy = "Custom" Then
                lblOrder.Enabled = True
                btnOrderDown.Enabled = True
                btnOrderUp.Enabled = True
            Else
                lblOrder.Enabled = False
                btnOrderDown.Enabled = False
                btnOrderUp.Enabled = False
            End If
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

    Private Sub btnOrderUp_Click(sender As Object, e As EventArgs) Handles btnOrderUp.Click
        Dim index As Integer = 0
        If Me.lvObjects.SelectedItems.Count > 0 Then
            index = Me.lvObjects.SelectedItems(0).Index
            If index <> 0 Then
                Dim lvi As ListViewItem = Me.lvObjects.SelectedItems(0).Clone
                Me.lvObjects.SelectedItems(0).Remove()
                Me.lvObjects.Items.Insert(index - 1, lvi)
                Me.lvObjects.Items(index - 1).Selected = True
            End If
        End If
    End Sub

    Private Sub btnOrderDown_Click(sender As Object, e As EventArgs) Handles btnOrderDown.Click
        Dim index As Integer = 0
        If Me.lvObjects.SelectedItems.Count > 0 Then
            index = Me.lvObjects.SelectedItems(0).Index
            If index <> Me.lvObjects.Items.Count - 1 Then
                Dim lvi As ListViewItem = Me.lvObjects.SelectedItems(0).Clone
                Me.lvObjects.SelectedItems(0).Remove()
                Me.lvObjects.Items.Insert(index + 1, lvi)
                Me.lvObjects.Items(index + 1).Selected = True
            End If
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        For Each obj In lvObjects.Items
            DirectCast(obj, ListViewItem).Checked = True
        Next
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        For Each obj In lvProps.Items
            DirectCast(obj, ListViewItem).Checked = True
        Next
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim props() As String = Nothing

        If Table.ObjectList.Count > 0 Then
            For Each s As String In Table.ObjectList.Keys
                props = Table.Flowsheet.GetFlowsheetSimulationObject(s).GetDefaultProperties
                Exit For
            Next
            For Each lvi As ListViewItem In lvProps.Items
                lvi.Checked = False
            Next
            For Each lvi As ListViewItem In lvProps.Items
                If props.Contains(lvi.Tag) Then lvi.Checked = True
            Next
        End If
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        For Each obj In lvObjects.Items
            DirectCast(obj, ListViewItem).Checked = False
        Next
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        For Each obj In lvProps.Items
            DirectCast(obj, ListViewItem).Checked = False
        Next
    End Sub

    Private Sub nupLines_ValueChanged(sender As Object, e As EventArgs) Handles nupLines.ValueChanged
        If Loaded Then Table.NumberOfLines = nupLines.Value
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        cbObjectType.SelectedItem = "MaterialStream"

        Button1_Click(sender, e)

        Dim props() As String = Nothing

        If Table.ObjectList.Count > 0 Then
            For Each s As String In Table.ObjectList.Keys
                props = Table.Flowsheet.GetFlowsheetSimulationObject(s).GetProcessFlowsheetProperties
                Exit For
            Next
            For Each lvi As ListViewItem In lvProps.Items
                lvi.Checked = False
            Next
            For Each lvi As ListViewItem In lvProps.Items
                If props.Contains(lvi.Tag) Then lvi.Checked = True
            Next
        End If

    End Sub

End Class