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

Imports DWSIM.UnitOperations.UnitOperations
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.IO
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports System.Drawing
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class EditingForm_Column_InitialEstimates

    Inherits UserControl

    Public dc As Column
    Dim form As IFlowsheet

    Dim cb1, cb2, cb3 As Object
    Public _ie As InitialEstimates
    Public pathsep As Char

    Dim _ies As InitialEstimates

    Dim loaded As Boolean = False
    Dim su As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub UIInitialEstimatesEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ChangeDefaultFont(Me)

        pathsep = Path.DirectorySeparatorChar

        form = dc.FlowSheet
        nf = form.FlowsheetOptions.NumberFormat
        su = form.FlowsheetOptions.SelectedUnitSystem

        dgvv.Columns(1).HeaderText += " (" & form.FlowsheetOptions.SelectedUnitSystem.temperature & ")"
        dgvv.Columns(2).HeaderText += " (" & form.FlowsheetOptions.SelectedUnitSystem.molarflow & ")"
        dgvv.Columns(3).HeaderText += " (" & form.FlowsheetOptions.SelectedUnitSystem.molarflow & ")"

        dgvv.Rows.Clear()
        Dim i As Integer = 0
        Dim count As Integer = dc.Stages.Count
        For Each st As Stage In dc.Stages
            dgvv.Rows.Add(New Object() {dc.Stages(i).Name, Format(cv.ConvertFromSI(su.temperature, dc.InitialEstimates.StageTemps(i).Value), nf), Format(cv.ConvertFromSI(su.molarflow, dc.InitialEstimates.VapMolarFlows(i).Value), nf), Format(cv.ConvertFromSI(su.molarflow, dc.InitialEstimates.LiqMolarFlows(i).Value), nf)})
            dgvv.Rows(dgvv.Rows.Count - 1).HeaderCell.Value = i.ToString()
            i += 1
        Next

        Dim j As Integer = 0
        For Each cp As Thermodynamics.BaseClasses.ConstantProperties In form.SelectedCompounds.Values
            dgvcl.Columns.Add(cp.Name, (cp.Name))
            dgvcv.Columns.Add(cp.Name, (cp.Name))
            j = j + 1
        Next

        If dc.InitialEstimates.LiqCompositions(0).Keys.Except(form.SelectedCompounds.Keys).Count > 0 Then
            dc.InitialEstimates = dc.RebuildEstimates()
        ElseIf dc.InitialEstimates.LiqCompositions.Count = 0 Then
            dc.InitialEstimates = dc.RebuildEstimates()
        Else
            If form.SelectedCompounds.Count <> dc.InitialEstimates.LiqCompositions(0).Count Then
                dc.InitialEstimates = dc.RebuildEstimates()
            End If
        End If

        i = 0
        Dim ob(CInt(j.ToString)) As Object
        For Each st As Stage In dc.Stages
            j = 1
            ob(0) = dc.Stages(i).Name
            For Each cp As Thermodynamics.BaseClasses.ConstantProperties In form.SelectedCompounds.Values
                ob(j) = Format(dc.InitialEstimates.LiqCompositions(i)(cp.Name).Value, nf)
                j = j + 1
            Next
            dgvcl.Rows.Add(ob)
            dgvcl.Rows(dgvcl.Rows.Count - 1).HeaderCell.Value = i.ToString()
            i += 1
        Next

        i = 0
        For Each st As Stage In dc.Stages
            j = 1
            ob(0) = dc.Stages(i).Name
            For Each cp As Thermodynamics.BaseClasses.ConstantProperties In form.SelectedCompounds.Values
                ob(j) = Format(dc.InitialEstimates.VapCompositions(i)(cp.Name).Value, nf)
                j = j + 1
            Next
            dgvcv.Rows.Add(ob)
            dgvcv.Rows(dgvcv.Rows.Count - 1).HeaderCell.Value = i.ToString()
            i += 1
        Next

        ToolStripButton29.Checked = dc.AutoUpdateInitialEstimates

        loaded = True

    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        If dgvv.SelectedCells.Count > 0 Then
            cb1 = dgvv.SelectedCells(0).Value
        End If
    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        If dgvv.SelectedCells.Count > 0 Then
            If Not dgvv.SelectedCells(0).ReadOnly Then
                dgvv.SelectedCells(0).Value = cb1
            End If
        End If
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        For Each c As DataGridViewCell In dgvv.SelectedCells
            If c.ColumnIndex <> 0 Then c.Value = Nothing
        Next
    End Sub

    Private Sub ToolStripButton6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton6.Click
        If dgvv.SelectedCells.Count > 0 Then
            dgvv.SelectedCells(0).Tag = "LOCKED"
            dgvv.SelectedCells(0).Style.BackColor = Color.WhiteSmoke
        End If
    End Sub

    Private Sub ToolStripButton7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton7.Click
        If dgvv.SelectedCells.Count > 0 Then
            dgvv.SelectedCells(0).Tag = ""
            dgvv.SelectedCells(0).Style.BackColor = Color.White
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton3.Click
        If dgvcl.SelectedCells.Count > 0 Then
            cb2 = dgvcl.SelectedCells(0).Value
        End If
    End Sub

    Private Sub ToolStripButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton4.Click
        If dgvcl.SelectedCells.Count > 0 Then
            If Not dgvcl.SelectedCells(0).ReadOnly Then
                dgvcl.SelectedCells(0).Value = cb2
            End If
        End If
    End Sub

    Private Sub ToolStripButton8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton8.Click
        For Each c As DataGridViewCell In dgvcl.SelectedCells
            If c.ColumnIndex <> 0 Then c.Value = Nothing
        Next
    End Sub

    Private Sub ToolStripButton9_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton9.Click
        If dgvcl.SelectedCells.Count > 0 Then
            dgvcl.SelectedCells(0).Tag = "LOCKED"
            dgvcl.SelectedCells(0).Style.BackColor = Color.WhiteSmoke
        End If
    End Sub

    Private Sub ToolStripButton10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton10.Click
        If dgvcl.SelectedCells.Count > 0 Then
            dgvcl.SelectedCells(0).Tag = ""
            dgvcl.SelectedCells(0).Style.BackColor = Color.White
        End If
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Initial Estimates File", "*.dwcdi")})

        If handler IsNot Nothing Then
            Try
                Dim jsondata = handler.ReadAllText()
                _ies = Newtonsoft.Json.JsonConvert.DeserializeObject(Of InitialEstimates)(jsondata)
                Me.TextBox1.Text = handler.FullPath
            Catch ex As Exception
                MessageBox.Show(ex.Message)
            End Try
        End If

    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Initial Estimates File", "*.dwcdi")})

        If handler IsNot Nothing Then
            Try
                Dim data = Newtonsoft.Json.JsonConvert.SerializeObject(dc.InitialEstimates, Newtonsoft.Json.Formatting.Indented)
                Using stream As New IO.MemoryStream()
                    Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                        writer.Write(data)
                        handler.Write(stream)
                    End Using
                End Using
                Me.TextBox1.Text = handler.FullPath
            Catch ex As Exception
                MessageBox.Show(ex.Message)
            End Try
        End If

    End Sub

    Private Sub dgvv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvv.CellValueChanged
        If loaded Then
            Dim value As Object = dgvv.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim count As Integer = dc.Stages.Count
            Select Case e.ColumnIndex
                Case 1
                    dc.InitialEstimates.StageTemps(e.RowIndex).Value = cv.ConvertToSI(su.temperature, value)
                Case 2
                    dc.InitialEstimates.VapMolarFlows(e.RowIndex).Value = cv.ConvertToSI(su.molarflow, value)
                Case 3
                    dc.InitialEstimates.LiqMolarFlows(e.RowIndex).Value = cv.ConvertToSI(su.molarflow, value)
            End Select
        End If
    End Sub

    Private Sub dgvcl_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles dgvcl.CellValidating
        If e.ColumnIndex > 0 Then dgvcl.ValidateCellForDouble(e)
    End Sub

    Private Sub dgvc_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvcl.CellValueChanged
        If loaded Then
            Dim value As Object = dgvcl.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim count As Integer = dc.Stages.Count
            Dim colname As String = dgvcl.Columns(e.ColumnIndex).Name
            dc.InitialEstimates.LiqCompositions(e.RowIndex)(colname).Value = value
        End If
    End Sub

    Private Sub dgvc_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgvcl.DataError
        e.Cancel = True
    End Sub

    Private Sub dgvv_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles dgvv.DataError
        e.Cancel = True
    End Sub

    Private Sub ToolStripButton21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton21.Click
        If dgvcv.SelectedCells.Count > 0 Then
            cb3 = dgvcv.SelectedCells(0).Value
        End If
    End Sub

    Private Sub ToolStripButton22_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton22.Click
        If dgvcv.SelectedCells.Count > 0 Then
            If Not dgvcv.SelectedCells(0).ReadOnly Then
                dgvcv.SelectedCells(0).Value = cb3
            End If
        End If
    End Sub

    Private Sub ToolStripButton23_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton23.Click
        For Each c As DataGridViewCell In dgvcv.SelectedCells
            If c.ColumnIndex <> 0 Then c.Value = Nothing
        Next
    End Sub

    Private Sub ToolStripButton24_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton24.Click
        If dgvcv.SelectedCells.Count > 0 Then
            dgvcv.SelectedCells(0).Tag = "LOCKED"
            dgvcv.SelectedCells(0).Style.BackColor = Color.WhiteSmoke
        End If
    End Sub

    Private Sub ToolStripButton25_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton25.Click
        If dgvcv.SelectedCells.Count > 0 Then
            dgvcv.SelectedCells(0).Tag = ""
            dgvcv.SelectedCells(0).Style.BackColor = Color.White
        End If
    End Sub

    Private Sub dgvcv_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles dgvcv.CellValidating
        If e.ColumnIndex > 0 Then dgvcv.ValidateCellForDouble(e)
    End Sub

    Private Sub dgvcv_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvcv.CellValueChanged
        If loaded Then
            Dim value As Object = dgvcv.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim count As Integer = dc.Stages.Count
            Dim colname As String = dgvcv.Columns(e.ColumnIndex).Name
            dc.InitialEstimates.VapCompositions(e.RowIndex)(colname).Value = value
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim i, j As Integer
        For i = 0 To dc.NumberOfStages - 1
            dgvv.Rows(i).Cells(1).Value = Format(cv.ConvertFromSI(su.temperature, dc.Tf(i)), nf)
            dgvv.Rows(i).Cells(2).Value = Format(cv.ConvertFromSI(su.molarflow, dc.Vf(i)), nf)
            dgvv.Rows(i).Cells(3).Value = Format(cv.ConvertFromSI(su.molarflow, dc.Lf(i)), nf)
            For j = 0 To dc.compids.Count - 1
                dgvcl.Rows(i).Cells(j + 1).Value = Format(dc.xf(i)(j), nf)
                dgvcv.Rows(i).Cells(j + 1).Value = Format(dc.yf(i)(j), nf)
            Next
        Next
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click

        loaded = False

        dc.InitialEstimates = _ies
        dc.UpdateEditForm()

        loaded = True

    End Sub

    Private Sub ToolStripButton28_Click(sender As Object, e As EventArgs) Handles ToolStripButton28.Click

        Dim i, n As Integer, px, py As New List(Of Double)

        n = dgvv.Rows.Count - 1

        For i = 0 To n
            If dgvv.Rows(i).Cells(1).Value IsNot Nothing Then
                If Double.TryParse(dgvv.Rows(i).Cells(1).Value.ToString, New Double) Then
                    px.Add(Double.Parse(i))
                    py.Add(Double.Parse(dgvv.Rows(i).Cells(1).Value))
                End If
            End If
        Next

        For Each c As DataGridViewCell In Me.dgvv.SelectedCells
            If c.Value Is Nothing And c.ColumnIndex = 1 Then c.Value = MathNet.Numerics.Interpolate.Linear(px, py).Interpolate(c.RowIndex)
        Next

    End Sub

    Private Sub ToolStripButton27_Click(sender As Object, e As EventArgs) Handles ToolStripButton27.Click

        Dim i, n As Integer, px, py As New List(Of Double)

        n = dgvv.Rows.Count - 1

        For i = 0 To n
            If dgvv.Rows(i).Cells(2).Value IsNot Nothing Then
                If Double.TryParse(dgvv.Rows(i).Cells(2).Value.ToString, New Double) Then
                    px.Add(Double.Parse(i))
                    py.Add(Double.Parse(dgvv.Rows(i).Cells(2).Value))
                End If
            End If
        Next

        For Each c As DataGridViewCell In Me.dgvv.SelectedCells
            If c.Value Is Nothing And c.ColumnIndex = 2 Then c.Value = MathNet.Numerics.Interpolate.Linear(px, py).Interpolate(c.RowIndex)
        Next

    End Sub

    Private Sub ToolStripButton29_CheckedChanged(sender As Object, e As EventArgs) Handles ToolStripButton29.CheckedChanged
        If loaded Then
            dc.AutoUpdateInitialEstimates = ToolStripButton29.Checked
        End If
    End Sub

    Private Sub dgvv_KeyDown(sender As Object, e As KeyEventArgs) Handles dgvv.KeyDown, dgvcl.KeyDown, dgvcv.KeyDown
        If e.KeyCode = Keys.V And e.Modifiers = Keys.Control Then
            PasteData(sender)
        End If
    End Sub

    Private Sub ToolStripButton26_Click(sender As Object, e As EventArgs) Handles ToolStripButton26.Click

        Dim i, n As Integer, px, py As New List(Of Double)

        n = dgvv.Rows.Count - 1

        For i = 0 To n
            If dgvv.Rows(i).Cells(3).Value IsNot Nothing Then
                If Double.TryParse(dgvv.Rows(i).Cells(3).Value.ToString, New Double) Then
                    px.Add(Double.Parse(i))
                    py.Add(Double.Parse(dgvv.Rows(i).Cells(3).Value))
                End If
            End If
        Next

        For Each c As DataGridViewCell In Me.dgvv.SelectedCells
            If c.Value Is Nothing And c.ColumnIndex = 3 Then c.Value = MathNet.Numerics.Interpolate.Linear(px, py).Interpolate(c.RowIndex)
        Next

    End Sub

    Private Sub dgvv_CellValidating(sender As Object, e As DataGridViewCellValidatingEventArgs) Handles dgvv.CellValidating
        If e.ColumnIndex > 0 Then dgvv.ValidateCellForDouble(e)
    End Sub
End Class