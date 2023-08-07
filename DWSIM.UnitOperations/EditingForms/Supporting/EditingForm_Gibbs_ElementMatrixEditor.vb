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

Imports System.Drawing

Public Class EditingForm_Gibbs_ElementMatrixEditor

    Inherits UserControl

    Dim loaded As Boolean = False
    Public gr As Reactors.Reactor_Gibbs
    Dim form As IFlowsheet

    Public elmat(,) As Double

    Public Sub ElementMatrixEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()

        End Using

        form = gr.FlowSheet

        elmat = gr.ElementMatrix

        Dim i, j, e_, c As Integer

        With Me.grid
            .AllowUserToAddRows = False
            .Rows.Clear()
            .Columns.Clear()
            .Columns.Add("el", "Element")
            For Each s As String In gr.ComponentIDs
                .Columns.Add(s, s)
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

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)



    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)



    End Sub


    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        grid.Rows.Add()
    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        If Not grid.SelectedRows.Count = 0 Then
            grid.Rows.RemoveAt(grid.SelectedRows(0).Index)
        ElseIf grid.SelectedCells.Count > 0 Then
            grid.Rows.RemoveAt(grid.SelectedCells(0).RowIndex)
        End If
    End Sub

    Public Sub CreateMatrix()
        gr.CreateElementMatrix()

        Dim i, j, e_, c As Integer

        With Me.grid
            .AllowUserToAddRows = False
            .Rows.Clear()
            .Columns.Clear()
            .Columns.Add("el", "Element")
            For Each s As String In gr.ComponentIDs
                .Columns.Add(s, s)
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

    Private Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        CreateMatrix()
    End Sub

    Public Sub SaveMatrix()
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
        gr.ElementMatrix = elmat
    End Sub

    Private Sub ToolStripButton3_Click(sender As Object, e As EventArgs) Handles ToolStripButton3.Click
        gr.FlowSheet.RegisterSnapshot(Enums.SnapshotType.ObjectData, gr)
        SaveMatrix()
    End Sub

End Class