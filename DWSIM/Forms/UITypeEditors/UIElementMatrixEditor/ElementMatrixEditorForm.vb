'    Copyright 2010 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DWSIM.SimulationObjects.Reactors

Public Class ElementMatrixEditorForm

    Dim loaded As Boolean = False
    Dim gr As Reactor_Gibbs
    Dim form As FormFlowsheet

    Public elmat(,) As Double

    Private Sub ElementMatrixEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        form = My.Application.ActiveSimulation
        gr = form.Collections.ObjectCollection(form.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

        Dim i, j, e_, c As Integer

        With Me.grid
            .AllowUserToAddRows = False
            .Rows.Clear()
            .Columns.Clear()
            .Columns.Add("el", "Element")
            For Each s As String In gr.ComponentIDs
                .Columns.Add(s, DWSIM.App.GetComponentName(s))
            Next
            For Each s As String In gr.Elements
                .Rows.Add()
            Next
            c = gr.ComponentIDs.Count - 1
            e_ = gr.Elements.Length - 1
            For i = 0 To e_
                .Rows(i).Cells(0).Value = gr.Elements(i)
                For j = 0 To c
                    Try
                        .Rows(i).Cells(j + 1).Value = elmat(i, j)
                    Catch ex As Exception

                    End Try
                Next
            Next
        End With


    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        gr.CreateElementMatrix()

        Dim i, j, e_, c As Integer

        With Me.grid
            .AllowUserToAddRows = False
            .Rows.Clear()
            .Columns.Clear()
            .Columns.Add("el", "Elements")
            For Each s As String In gr.ComponentIDs
                .Columns.Add(s, DWSIM.App.GetComponentName(s))
            Next
            For Each s As String In gr.Elements
                .Rows.Add()
            Next
            c = gr.ComponentIDs.Count - 1
            e_ = gr.Elements.Length - 1
            For i = 0 To e_
                .Rows(i).Cells(0).Value = gr.Elements(i)
                For j = 0 To c
                    .Rows(i).Cells(j + 1).Value = gr.ElementMatrix(i, j)
                Next
            Next
        End With

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click

        ReDim elmat(Me.grid.Rows.Count - 1, Me.grid.Columns.Count - 2)
        ReDim gr.TotalElements(Me.grid.Rows.Count - 1)
        Dim sum_e As Double
        Dim elements As New ArrayList
        For Each r As DataGridViewRow In Me.grid.Rows
            sum_e = 0
            elements.Add(r.Cells(0).Value)
            For Each c As DataGridViewColumn In Me.grid.Columns
                If c.Index > 0 Then
                    elmat(r.Index, c.Index - 1) = grid.Rows(r.Index).Cells(c.Index).Value
                End If
            Next
        Next
        gr.Elements = elements.ToArray(Type.GetType("System.String"))

    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        grid.Rows.Add()
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        If Not grid.SelectedRows.Count = 0 Then
            Me.grid.Rows.RemoveAt(Me.grid.SelectedRows(0).Index)
        End If
    End Sub
End Class