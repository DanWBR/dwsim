Imports System.Reflection

Public Class FormConfig

    Private Sub FormConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        lblVersion.Text = "Version " + Assembly.GetExecutingAssembly().GetName().Version.ToString()
        Dim mapper As New CompoundMapper
        DataGridView1.Rows.Clear()
        For Each item In mapper.Maps.Values
            DataGridView1.Rows.Add(New Object() {item.Name, item.Formula})
        Next
        DataGridView1.Sort(Column1, ComponentModel.ListSortDirection.Ascending)
    End Sub
End Class