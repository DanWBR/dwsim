'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary.SepOps
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations
Imports DWSIM.DWSIM.SimulationObjects

Public Class UISteamedSideStripperEditorForm

    Dim dc As DistillationColumn
    Dim form As FormFlowsheet
    Dim cb As Object
    Dim loaded As Boolean = False

    Dim tpl As DWSIM.SimulationObjects.UnitOperations.Auxiliary.DGVCBSelectors.Templates
    Dim cvt As SystemsOfUnits.Converter

    Private Sub UISteamedSideStripperEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

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
        dgv1.Columns(6).CellTemplate = tpl.GetMaterialStreamOutSelector
        dgv1.Columns(7).CellTemplate = tpl.GetMaterialStreamInSelector

        With Me.dgv1.Rows
            .Clear()
            For Each sss As SteamedSideStripper In dc.StmSStrCol.Collection.Values
                If dc.MaterialStreams.ContainsKey(sss.ProductStreamID) And dc.MaterialStreams.ContainsKey(sss.SteamStreamID) Then
                    '.Add(New Object() {.Count + 1, sss.Name, sss.Stages.Count, sss.FromStage, sss.ToStage, SystemsOfUnits.Converter.ConvertFromSI(form.Options.SelectedUnitSystem.molarflow, sss.ProductRate), dc.MaterialStreams(sss.ProductStreamID).Tag, dc.MaterialStreams(sss.SteamStreamID).Tag, sss.ID})
                Else
                    .Add(New Object() {.Count + 1, sss.Name, sss.Stages.Count, sss.FromStage, sss.ToStage, SystemsOfUnits.Converter.ConvertFromSI(form.Options.SelectedUnitSystem.molarflow, sss.ProductRate), "", "", sss.ID})
                End If
            Next
        End With

        dgv1.Columns(5).HeaderText += " (" & form.Options.SelectedUnitSystem.molarflow & ")"

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
            .Add(New Object() {.Count + 1, "", 5, "", "", "", "", 0, id})
            dc.StmSStrCol.Collection.Add(id, New SteamedSideStripper(id, 5))
            Dim msid As String = Guid.NewGuid.ToString
            'dc.MaterialStreams.Add(msid.ToString, New StreamInformation(msid.ToString, "0", "", "", StreamInformation.Type.Material, StreamInformation.Behavior.SideOpLiquidProduct, StreamInformation.Phase.L))
            Dim msid2 As String = Guid.NewGuid.ToString
            'dc.MaterialStreams.Add(msid2.ToString, New StreamInformation(msid2.ToString, "0", "", "", StreamInformation.Type.Material, StreamInformation.Behavior.Steam, StreamInformation.Phase.V))
        End With
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        With Me.dgv1.Rows
            dc.MaterialStreams.Remove(dc.StmSStrCol.Collection(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(8).Value.ToString).ProductStreamID)
            dc.MaterialStreams.Remove(dc.StmSStrCol.Collection(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(8).Value.ToString).SteamStreamID)
            dc.StmSStrCol.Collection.Remove(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(8).Value.ToString)
            .RemoveAt(dgv1.SelectedCells(0).RowIndex)
        End With
    End Sub

    Private Sub dgv1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgv1.DataError
        e.Cancel = True
    End Sub

    Private Sub dgv1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv1.CellValueChanged
        If loaded Then
            Dim id As String = dgv1.Rows(e.RowIndex).Cells(8).Value
            Dim value As Object = dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Select Case e.ColumnIndex
                Case 1
                    dc.StmSStrCol.Collection(id).Name = value
                Case 2
                    dc.StmSStrCol.Collection(id).NumberOfStages = value
                Case 3
                    dc.StmSStrCol.Collection(id).FromStage = value
                Case 4
                    dc.StmSStrCol.Collection(id).ToStage = value
                Case 5
                    dc.StmSStrCol.Collection(id).ProductRate = SystemsOfUnits.Converter.ConvertToSI(form.Options.SelectedUnitSystem.molarflow, value)
                Case 6
                    Dim msid As String = dc.StmSStrCol.Collection(id).ProductStreamID
                    If dc.MaterialStreams.ContainsKey(msid) Then
                        'dc.MaterialStreams(msid).Tag = value
                        'dc.MaterialStreams(msid).Name = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams(msid).ID = dc.MaterialStreams(msid).Name.ToString
                    Else
                        Dim id2 = Guid.NewGuid.ToString
                        msid = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams.Add(id2.ToString, New StreamInformation(id2.ToString, "0", FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name, FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Tag, StreamInformation.Type.Material, StreamInformation.Behavior.SideOpLiquidProduct, StreamInformation.Phase.L))
                        dc.StmSStrCol.Collection(id).ProductStreamID = id2
                    End If
                Case 7
                    Dim msid As String = dc.StmSStrCol.Collection(id).SteamStreamID
                    If dc.MaterialStreams.ContainsKey(msid) Then
                        'dc.MaterialStreams(msid).Tag = value
                        'dc.MaterialStreams(msid).Name = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams(msid).ID = dc.MaterialStreams(msid).Name.ToString
                    Else
                        Dim id2 = Guid.NewGuid.ToString
                        msid = FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name
                        'dc.MaterialStreams.Add(id2.ToString, New StreamInformation(id2.ToString, "", FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Name, FormFlowsheet.SearchSurfaceObjectsByTag(value, form.FormSurface.FlowsheetDesignSurface).Tag, StreamInformation.Type.Material, StreamInformation.Behavior.Steam, StreamInformation.Phase.L))
                        dc.StmSStrCol.Collection(id).SteamStreamID = id2
                    End If

            End Select
        End If
    End Sub

End Class