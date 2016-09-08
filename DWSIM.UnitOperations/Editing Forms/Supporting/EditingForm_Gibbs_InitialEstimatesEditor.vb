'    Copyright 2012 Daniel Wagner O. de Medeiros
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

Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class EditingForm_Gibbs_InitialEstimatesEditor

    Inherits UserControl

    Dim loaded As Boolean = False
    Public gr As Reactors.Reactor_Gibbs
    Public form As IFlowsheet
    Public inlet, outletv, outletl As Thermodynamics.Streams.MaterialStream
    Dim su As SharedClasses.SystemsOfUnits.Units
    Dim nf As String = ""

    Public ie As List(Of Double)

    Public Sub GibbsInitialEstimatesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        form = gr.FlowSheet

        nf = form.FlowsheetOptions.NumberFormat
        su = form.FlowsheetOptions.SelectedUnitSystem

        ie = gr.InitialEstimates

        If Not Me.grid.Columns(1).HeaderText.Contains("(") Then Me.grid.Columns(1).HeaderText += " (" & su.molarflow & ")"

        If ie.Count <> gr.ComponentIDs.Count Then
            ie = New List(Of Double)
            For Each s As String In gr.ComponentIDs
                ie.Add(0.0#)
            Next
        End If

        Dim i As Integer = 0
        Me.grid.Rows.Clear()
        For Each s As String In gr.ComponentIDs
            Me.grid.Rows.Add(New Object() {s, Format(cv.ConvertFromSI(su.molarflow, ie(i)), nf)})
            i += 1
        Next

        gr.InitialEstimates = ie

        loaded = True

    End Sub

    Private Sub grid_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles grid.CellValueChanged
        If loaded Then
            ie(e.RowIndex) = cv.ConvertToSI(su.molarflow, grid.Rows(e.RowIndex).Cells(e.ColumnIndex).Value)
            gr.InitialEstimates = ie
        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        For Each r As DataGridViewRow In grid.Rows
            Dim s As String = gr.ComponentIDs(r.Index)
            r.Cells(1).Value = Format(cv.ConvertFromSI(su.molarflow, gr.GetInletMaterialStream(0).Phases(0).Compounds(s).MolarFlow.GetValueOrDefault), nf)
        Next
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        For Each r As DataGridViewRow In grid.Rows
            Dim s As String = gr.ComponentIDs(r.Index)
            r.Cells(1).Value = Format(cv.ConvertFromSI(su.molarflow, gr.GetOutletMaterialStream(0).Phases(0).Compounds(s).MolarFlow.GetValueOrDefault), nf)
        Next
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        For Each r As DataGridViewRow In grid.Rows
            Dim s As String = gr.ComponentIDs(r.Index)
            r.Cells(1).Value = Format(cv.ConvertFromSI(su.molarflow, gr.GetOutletMaterialStream(1).Phases(0).Compounds(s).MolarFlow.GetValueOrDefault), nf)
        Next
    End Sub

End Class