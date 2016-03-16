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

Imports DWSIM.DWSIM.SimulationObjects.UnitOps.Auxiliary.SepOps
Imports DWSIM.DWSIM.SimulationObjects.UnitOps

Public Class UIPumpAroundsEditorForm

    Dim dc As DistillationColumn
    Dim form As FormFlowsheet

    Dim cb As Object

    Dim loaded As Boolean = False
    Dim cvt As DWSIM.SystemsOfUnits.Converter

    Private Sub UIPumpAroundsEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        cvt = New DWSIM.SystemsOfUnits.Converter()

        form = My.Application.ActiveSimulation
        dc = form.Collections.ObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

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
        dgv1.Columns(2).CellTemplate = sgt1.Clone
        dgv1.Columns(3).CellTemplate = sgt1.Clone

        With Me.dgv1.Rows
            .Clear()
            For Each pa As PumpAround In dc.PArCol.Collection.Values
                .Add(New Object() {.Count + 1, pa.Name, pa.FromStage, pa.ToStage, Converter.ConvertFromSI(form.Options.SelectedUnitSystem.deltaP, pa.DeltaP), pa.ID})
            Next
        End With

        dgv1.Columns(4).HeaderText += " (" & form.Options.SelectedUnitSystem.deltaP & ")"

        loaded = True

    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        If dgv1.SelectedCells.Count > 0 Then
            cb = dgv1.SelectedCells(0).Value
        End If
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
            .Add(New Object() {dgv1.Rows.Count + 1, "", "", "", 0, id})
            dc.PArCol.Collection.Add(id, New PumpAround(id))
        End With
    End Sub

    Private Sub dgv1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv1.CellValueChanged
        If loaded Then
            Dim id As String = dgv1.Rows(e.RowIndex).Cells(5).Value
            Dim value As Object = dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Select Case e.ColumnIndex
                Case 1
                    dc.PArCol.Collection(id).Name = value
                Case 2
                    dc.PArCol.Collection(id).FromStage = value
                Case 3
                    dc.PArCol.Collection(id).ToStage = value
                Case 4
                    dc.PArCol.Collection(id).DeltaP = Converter.ConvertToSI(form.Options.SelectedUnitSystem.deltaP, value)
            End Select
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        With Me.dgv1.Rows
            dc.PArCol.Collection.Remove(dgv1.Rows(dgv1.SelectedCells(0).RowIndex).Cells(5).Value.ToString)
            .RemoveAt(dgv1.SelectedCells(0).RowIndex)
        End With
    End Sub

    Private Sub dgv1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgv1.DataError
        e.Cancel = True
    End Sub
End Class
