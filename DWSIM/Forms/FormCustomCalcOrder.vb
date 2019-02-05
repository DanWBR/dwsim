Public Class FormCustomCalcOrder

    Public ItemList As List(Of String)
    Public NewItemList As New List(Of String)

    Public Flowsheet As FormFlowsheet

    Private Sub FormCustomCalcOrder_Load(sender As Object, e As EventArgs) Handles MyBase.Load

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

End Class