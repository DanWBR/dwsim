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
            PopulateData()
        Else
            PanelData.Visible = False
            GHGComp = Nothing
        End If
    End Sub

    Public Sub PopulateData()

        Loaded = False

        tbName.Text = GHGComp.Name
        tbCH4.Text = (GHGComp.Methane * 100.0).ToString()
        tbCO2.Text = (GHGComp.CarbonDioxide * 100.0).ToString()
        tbN2O.Text = (GHGComp.NitrousOxide * 100.0).ToString()
        tbH2O.Text = (GHGComp.Water * 100.0).ToString()

        Loaded = True

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles btnAdd.Click
        Dim ghgcomp As New GHGEmissionComposition With {.ID = Guid.NewGuid().ToString(), .Name = "GHGComposition" + (Flowsheet.GHGEmissionCompositions.Count + 1).ToString()}
        Flowsheet.GHGEmissionCompositions.Add(ghgcomp.ID, ghgcomp)
        ListView1.Items.Add(New ListViewItem(ghgcomp.Name) With {.Tag = ghgcomp.ID})
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnRemove.Click
        If ListView1.SelectedIndices.Count > 0 Then
            If MessageBox.Show(DWSIM.App.GetLocalString("ConfirmOperation"),
                                          DWSIM.App.GetLocalString("Ateno2"),
                                          MessageBoxButtons.YesNo,
                                          MessageBoxIcon.Question) = DialogResult.Yes Then

                Dim id = ListView1.SelectedItems(0).Tag
                GHGComp = Nothing
                Flowsheet.GHGEmissionCompositions.Remove(id)
                ListView1.Items.Remove(ListView1.SelectedItems(0))

            End If
        End If
    End Sub

    Private Sub tbName_TextChanged(sender As Object, e As EventArgs) Handles tbName.TextChanged
        If Loaded And GHGComp IsNot Nothing Then
            GHGComp.Name = tbName.Text
            ListView1.SelectedItems(0).Text = tbName.Text
        End If
    End Sub

    Private Sub tbCH4_TextChanged(sender As Object, e As EventArgs) Handles tbCH4.TextChanged
        If Loaded And GHGComp IsNot Nothing Then
            If tbCH4.Text.IsValidDouble() Then
                GHGComp.Methane = tbCH4.Text.ToDoubleFromCurrent() / 100.0
                tbCH4.ForeColor = Color.Blue
            Else
                tbCH4.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub tbCO2_TextChanged(sender As Object, e As EventArgs) Handles tbCO2.TextChanged
        If Loaded And GHGComp IsNot Nothing Then
            If tbCO2.Text.IsValidDouble() Then
                GHGComp.CarbonDioxide = tbCO2.Text.ToDoubleFromCurrent() / 100.0
                tbCO2.ForeColor = Color.Blue
            Else
                tbCO2.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub tbN2O_TextChanged(sender As Object, e As EventArgs) Handles tbN2O.TextChanged
        If Loaded And GHGComp IsNot Nothing Then
            If tbN2O.Text.IsValidDouble() Then
                GHGComp.NitrousOxide = tbN2O.Text.ToDoubleFromCurrent() / 100.0
                tbN2O.ForeColor = Color.Blue
            Else
                tbN2O.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub tbH2O_TextChanged(sender As Object, e As EventArgs) Handles tbH2O.TextChanged
        If Loaded And GHGComp IsNot Nothing Then
            If tbH2O.Text.IsValidDouble() Then
                GHGComp.Water = tbH2O.Text.ToDoubleFromCurrent() / 100.0
                tbH2O.ForeColor = Color.Blue
            Else
                tbH2O.ForeColor = Color.Red
            End If
        End If
    End Sub

End Class
