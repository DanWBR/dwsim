Imports DWSIM.Interfaces

Public Class GHGCompositionsEditor

    Public Flowsheet As IFlowsheet

    Private Loaded As Boolean = False

    Private GHGComp As IGHGComposition

    Private Sub GHGCompositionsEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Public Sub PopulateList()

        PanelData.Visible = False

        ListView1.Items.Clear()
        For Each ghg In Flowsheet.GHGEmissionCompositions
            ListView1.Items.Add(New ListViewItem(ghg.Value.Name) With {.Tag = ghg.Key})
        Next

    End Sub

    Private Sub ListView1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles ListView1.SelectedIndexChanged
        If ListView1.SelectedIndices.Count > 0 Then
            PanelData.Visible = True
            GHGComp = Flowsheet.GHGEmissionCompositions(ListView1.SelectedItems(0).Tag)
        Else
            PanelData.Visible = False
            GHGComp = Nothing
        End If
    End Sub

    Public Sub PopulateData()



    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles btnAdd.Click

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnRemove.Click

    End Sub

End Class
