Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary.SepOps
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations
Imports DWSIM.DWSIM.SimulationObjects

Public Class UIReboiledSideStripperEditorForm

    Dim dc As DistillationColumn
    Dim form As FormFlowsheet
    Dim cb As Object
    Dim loaded As Boolean = False

    Dim tpl As DWSIM.SimulationObjects.UnitOperations.Auxiliary.DGVCBSelectors.Templates
    Dim cvt As SystemsOfUnits.Converter

    Private Sub UIReboiledSideStripperEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        cvt = New SystemsOfUnits.Converter()

        form = My.Application.ActiveSimulation
        dc = form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
        tpl = New DWSIM.SimulationObjects.UnitOperations.Auxiliary.DGVCBSelectors.Templates(form, dc)

        Dim i As Integer = 0

        Dim sgt1 As New DataGridViewComboBoxCell
        With sgt1.Items
            i = 0
            For Each st As Stage In dc.Stages
                .Add((i + 1).ToString)
                i += 1
            Next
        End With

        'selectors
        dgv1.Columns(3).CellTemplate = sgt1.Clone
        dgv1.Columns(4).CellTemplate = sgt1.Clone
        dgv1.Columns(7).CellTemplate = tpl.GetMaterialStreamOutSelector

        With Me.dgv1.Rows
            .Clear()
            For Each rss As ReboiledSideStripper In dc.RebSStrCol.Collection.Values
                If dc.MaterialStreams.ContainsKey(rss.ProductStreamID) Then
                    '.Add(New Object() {.Count + 1, rss.Name, rss.Stages.Count, rss.FromStage, rss.ToStage, SystemsOfUnits.Converter.ConvertFromSI(form.Options.SelectedUnitSystem.molarflow, rss.ProductRate), rss.BoilUpRatio, dc.MaterialStreams(rss.ProductStreamID).Tag, rss.ID})
                Else
                    .Add(New Object() {.Count + 1, rss.Name, rss.Stages.Count, rss.FromStage, rss.ToStage, rss.BoilUpRatio, SystemsOfUnits.Converter.ConvertFromSI(form.Options.SelectedUnitSystem.molarflow, rss.ProductRate), "", rss.ID})
                End If
            Next
        End With

        dgv1.Columns(6).HeaderText += " (" & form.Options.SelectedUnitSystem.molarflow & ")"

        loaded = True


    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        If dgv1.SelectedCells.Count > 0 Then
            If Not dgv1.SelectedCells(0).ReadOnly Then
                dgv1.SelectedCells(0).Value = cb
            End If
        End If
    End Sub

    Private Sub ToolStripButton8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton8.Click
        If dgv1.SelectedCells.Count > 0 Then
            If Not dgv1.SelectedCells(0).ReadOnly Then
                dgv1.SelectedCells(0).Value = Nothing
            End If
        End If
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        With Me.dgv1.Rows
            Dim id As String = Guid.NewGuid.ToString
            .Add(New Object() {.Count + 1, "", 5, "", "", 0.75, "", 0, id})
            dc.RebSStrCol.Collection.Add(id, New ReboiledSideStripper(id, 5))
            Dim msid As String = Guid.NewGuid.ToString
            'dc.MaterialStreams.Add(msid.ToString, New StreamInformation(msid.ToString, "0", "", "", StreamInformation.Type.Material, StreamInformation.Behavior.SideOpLiquidProduct, StreamInformation.Phase.L))
        End With
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        With Me.dgv1.Rows
            dc.RebSStrCol.Collection.Remove(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(5).Value.ToString)
            .RemoveAt(dgv1.SelectedCells(0).RowIndex)
            dc.MaterialStreams.Remove(dc.RebSStrCol.Collection(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(5).Value.ToString).ProductStreamID)
        End With
    End Sub

    Private Sub dgv1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs)
        e.Cancel = True
    End Sub

    Private Sub dgv1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs)
        If loaded Then
            Dim id As String = dgv1.Rows(e.RowIndex).Cells(8).Value
            Dim value As Object = dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Select Case e.ColumnIndex
                Case 1
                    dc.RebSStrCol.Collection(id).Name = value
                Case 2
                    dc.RebSStrCol.Collection(id).NumberOfStages = value
                Case 3
                    dc.RebSStrCol.Collection(id).FromStage = value
                Case 4
                    dc.RebSStrCol.Collection(id).ToStage = value
                Case 5
                    dc.RebSStrCol.Collection(id).BoilUpRatio = value
                Case 6
                    dc.RebSStrCol.Collection(id).ProductRate = SystemsOfUnits.Converter.ConvertToSI(form.Options.SelectedUnitSystem.molarflow, value)
                Case 7
                    Dim msid As String = dc.RebSStrCol.Collection(id).ProductStreamID
                    If dc.MaterialStreams.ContainsKey(msid) Then
                        'dc.MaterialStreams(msid).Name = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams(msid).ID = dc.MaterialStreams(msid).Name.ToString
                    Else
                        Dim id2 = Guid.NewGuid.ToString
                        msid = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams.Add(id2.ToString, New StreamInformation(id2.ToString, "0", FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name, FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Tag, StreamInformation.Type.Material, StreamInformation.Behavior.SideOpLiquidProduct, StreamInformation.Phase.L))
                        dc.RebSStrCol.Collection(id).ProductStreamID = id2
                    End If
            End Select
        End If
    End Sub

End Class