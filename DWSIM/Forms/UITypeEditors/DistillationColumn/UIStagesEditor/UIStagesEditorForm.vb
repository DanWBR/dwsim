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

Public Class UIStagesEditorForm

    Dim dc As Column
    Dim form As FormFlowsheet

    Dim cb As Object

    Dim loaded As Boolean = False

    Dim cvt As DWSIM.SystemsOfUnits.Converter

    Private Sub UIStagesEditorForm_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

    End Sub

    Private Sub UIStagesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        cvt = New DWSIM.SystemsOfUnits.Converter()

        Dim i As Integer = 0

        form = My.Application.ActiveSimulation
        dc = form.Collections.FlowsheetObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

        With Me.dgv1.Rows
            .Clear()
            i = 0
            Do
                .Add(New Object() {i, dc.Stages(i).Name, Converter.ConvertFromSI(form.Options.SelectedUnitSystem.pressure, dc.Stages(i).P), dc.Stages(i).Efficiency})
                i = i + 1
            Loop Until i = dc.Stages.Count
            Select Case dc.ColumnType
                Case Column.ColType.DistillationColumn
                    .Item(0).ReadOnly = True
                    For Each c As DataGridViewCell In .Item(0).Cells
                        c.Style.BackColor = Color.WhiteSmoke
                    Next
                    .Item(.Count - 1).ReadOnly = True
                    For Each c As DataGridViewCell In .Item(.Count - 1).Cells
                        c.Style.BackColor = Color.WhiteSmoke
                    Next
                Case Column.ColType.AbsorptionColumn
                Case Column.ColType.ReboiledAbsorber
                    .Item(.Count - 1).ReadOnly = True
                    For Each c As DataGridViewCell In .Item(.Count - 1).Cells
                        c.Style.BackColor = Color.WhiteSmoke
                    Next
                Case Column.ColType.RefluxedAbsorber
                    .Item(0).ReadOnly = True
                    For Each c As DataGridViewCell In .Item(0).Cells
                        c.Style.BackColor = Color.WhiteSmoke
                    Next
            End Select
        End With

        Me.dgv1.Columns(0).ReadOnly = True

        For Each r As DataGridViewRow In dgv1.Rows
            r.Cells(0).Style.BackColor = Color.WhiteSmoke
        Next

        dgv1.Columns(2).HeaderText += " (" & form.Options.SelectedUnitSystem.pressure & ")"

        loaded = True

    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        If dgv1.SelectedCells.Count > 0 Then
            cb = dgv1.SelectedCells(0).Value
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        If dgv1.SelectedCells.Count > 0 Then
            If Not dgv1.SelectedCells(0).ReadOnly Then
                dgv1.SelectedCells(0).Value = cb
            End If
        End If
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        If dgv1.SelectedCells.Count > 0 Then
            If Not dgv1.SelectedCells(0).ReadOnly Then
                dgv1.SelectedCells(0).Value = Nothing
            End If
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        For Each r As DataGridViewRow In dgv1.Rows
            If Not r.Cells(3).ReadOnly Then
                r.Cells(3).Value = 1.0#
            End If
        Next
    End Sub

    Private Sub dgv1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgv1.CellValueChanged
        If loaded Then
            If e.ColumnIndex = 2 Then
                'pressure
                dc.Stages(e.RowIndex).P = Converter.ConvertToSI(form.Options.SelectedUnitSystem.pressure, dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value)
            ElseIf e.ColumnIndex = 3 Then
                'efficiency
                dc.Stages(e.RowIndex).Efficiency = dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            ElseIf e.ColumnIndex = 1 Then
                'name
                dc.Stages(e.RowIndex).Name = dgv1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            End If
        End If
    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        For Each r As DataGridViewRow In dgv1.Rows
            If Not r.Cells(1).ReadOnly Then
                r.Cells(2).Value = Format((dgv1.Rows(dgv1.Rows.Count - 1).Cells(2).Value - dgv1.Rows(0).Cells(2).Value) * (r.Index - dgv1.Rows(0).Index) / (dgv1.Rows(dgv1.Rows.Count - 1).Index - dgv1.Rows(0).Index) + dgv1.Rows(0).Cells(2).Value, form.Options.NumberFormat)
            End If
        Next
    End Sub

End Class