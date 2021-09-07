Public Class FormCustomCalcOrder

    Public ItemList As List(Of String)
    Public NewItemList As New List(Of String)

    Public Flowsheet As IFlowsheet

    Private Sub FormCustomCalcOrder_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.AutoScaleMode = AutoScaleMode.Font

        For Each control As Control In Me.Controls
            control.Font = Drawing.SystemFonts.MessageBoxFont
        Next

        ListView1.Items.Clear()

        For Each item In ItemList
            ListView1.Items.Add(New ListViewItem(Flowsheet.SimulationObjects(item).GraphicObject.Tag) With {.Tag = item})
        Next

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        NewItemList = ItemList

        Me.Close()

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        NewItemList.Clear()

        For Each item As ListViewItem In ListView1.Items
            NewItemList.Add(item.Tag)
        Next

        Me.Close()

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click
        Dim index As Integer = 0
        If Me.ListView1.SelectedItems.Count > 0 Then
            index = Me.ListView1.SelectedItems(0).Index
            If index <> 0 Then
                Dim lvi As ListViewItem = Me.ListView1.SelectedItems(0).Clone
                Me.ListView1.SelectedItems(0).Remove()
                Me.ListView1.Items.Insert(index - 1, lvi)
                Me.ListView1.Items(index - 1).Selected = True
            End If
        End If
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim index As Integer = 0
        If Me.ListView1.SelectedItems.Count > 0 Then
            index = Me.ListView1.SelectedItems(0).Index
            If index <> Me.ListView1.Items.Count - 1 Then
                Dim lvi As ListViewItem = Me.ListView1.SelectedItems(0).Clone
                Me.ListView1.SelectedItems(0).Remove()
                Me.ListView1.Items.Insert(index + 1, lvi)
                Me.ListView1.Items(index + 1).Selected = True
            End If
        End If
    End Sub

End Class