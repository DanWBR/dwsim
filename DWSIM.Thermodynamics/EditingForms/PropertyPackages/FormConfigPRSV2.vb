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

Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.IO

Public Class FormConfigPRSV2

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormUNIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Loaded = False

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Me.KryptonDataGridView2.DataSource = Nothing

        Me.FaTabStripItem2.Visible = True

        Me.KryptonDataGridView2.Rows.Clear()

        Dim nf As String = "0.0000"

        If TypeOf _pp Is PRSV2PropertyPackage Then
            Dim ppu As PRSV2PropertyPackage = _pp
            For Each cp As ConstantProperties In _comps.Values
gt0:            If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name.ToLower) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name.ToLower).ContainsKey(cp2.Name.ToLower) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name.ToLower) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name.ToLower).ContainsKey(cp.Name.ToLower) Then
                                        ppu.m_pr.InteractionParameters(cp.Name.ToLower).Add(cp2.Name.ToLower, New PropertyPackages.Auxiliary.PRSV2_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kij
                                        Dim a21 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kji
                                        KryptonDataGridView2.Rows.Add(New Object() {(cp2.Name), (cp.Name), Format(a12, nf), Format(a21, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name.ToLower
                                            .Cells(1).Tag = cp2.Name.ToLower
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kij
                                Dim a21 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kji
                                KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf), Format(a21, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name.ToLower
                                    .Cells(1).Tag = cp2.Name.ToLower
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name.ToLower, New Dictionary(Of String, PropertyPackages.Auxiliary.PRSV2_IPData))
                    GoTo gt0
                End If
            Next

            dgvu1.Rows.Clear()

            For Each cp As ConstantProperties In _comps.Values
gt1:            If ppu.m_pr._data.ContainsKey(cp.Name.ToLower) Then
                    Dim kappa1 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa1
                    Dim kappa2 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa2
                    Dim kappa3 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa3
                    dgvu1.Rows.Add(New Object() {(cp.Name), kappa1, kappa2, kappa3})
                Else
                    ppu.m_pr._data.Add(cp.Name.ToLower, New PropertyPackages.Auxiliary.PRSV2Param)
                    GoTo gt1
                End If
            Next
        Else

            Dim ppu As PRSV2VLPropertyPackage = _pp
            For Each cp As ConstantProperties In _comps.Values
gt0a:           If ppu.m_pr.InteractionParameters.ContainsKey(cp.Name.ToLower) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If cp.Name <> cp2.Name Then
                            If Not ppu.m_pr.InteractionParameters(cp.Name.ToLower).ContainsKey(cp2.Name.ToLower) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name.ToLower) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name.ToLower).ContainsKey(cp.Name.ToLower) Then
                                        ppu.m_pr.InteractionParameters(cp.Name.ToLower).Add(cp2.Name.ToLower, New PropertyPackages.Auxiliary.PRSV2_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kij
                                        Dim a21 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kji
                                        KryptonDataGridView2.Rows.Add(New Object() {(cp2.Name), (cp.Name), Format(a12, nf), Format(a21, nf)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name.ToLower
                                            .Cells(1).Tag = cp2.Name.ToLower
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kij
                                Dim a21 As Double = ppu.m_pr.InteractionParameters(cp.Name.ToLower)(cp2.Name.ToLower).kji
                                KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf), Format(a21, nf)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name.ToLower
                                    .Cells(1).Tag = cp2.Name.ToLower
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_pr.InteractionParameters.Add(cp.Name.ToLower, New Dictionary(Of String, PropertyPackages.Auxiliary.PRSV2_IPData))
                    GoTo gt0a
                End If
            Next

            dgvu1.Rows.Clear()

            For Each cp As ConstantProperties In _comps.Values
gt2:            If ppu.m_pr._data.ContainsKey(cp.Name.ToLower) Then
                    Dim kappa1 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa1
                    Dim kappa2 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa2
                    Dim kappa3 As Double = ppu.m_pr._data(cp.Name.ToLower).kappa3
                    dgvu1.Rows.Add(New Object() {(cp.Name), kappa1, kappa2, kappa3})
                Else
                    ppu.m_pr._data.Add(cp.Name.ToLower, New PropertyPackages.Auxiliary.PRSV2Param)
                    GoTo gt2
                End If
            Next
        End If

        Loaded = True

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Private Sub KryptonDataGridView2_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs)
        If e.FormattedValue <> Nothing Then
            If Double.TryParse(e.FormattedValue, New Double) = False Then
                MessageBox.Show(Calculator.GetLocalString("Ovalorinseridoinvlid"), Calculator.GetLocalString("Parmetroinvlido"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                e.Cancel = True
            End If
        End If
    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            If TypeOf _pp Is PRSV2PropertyPackage Then
                Dim ppu As PRSV2PropertyPackage = _pp
                Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Value.ToString.ToLower
                Select Case e.ColumnIndex
                    Case 1
                        ppu.m_pr._data(id1).kappa1 = value
                    Case 2
                        ppu.m_pr._data(id1).kappa2 = value
                    Case 3
                        ppu.m_pr._data(id1).kappa3 = value
                End Select
            Else
                Dim ppu As PRSV2VLPropertyPackage = _pp
                Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Value.ToString.ToLower
                Select Case e.ColumnIndex
                    Case 1
                        ppu.m_pr._data(id1).kappa1 = value
                    Case 2
                        ppu.m_pr._data(id1).kappa2 = value
                    Case 3
                        ppu.m_pr._data(id1).kappa3 = value
                End Select
            End If
        End If
    End Sub

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim oldvalue As Double, tp As String = ""
            If TypeOf _pp Is PRSV2PropertyPackage Then
                Dim ppu As PRSV2PropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim id1 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString.ToLower
                Dim id2 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString.ToLower
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = value
                        tp = "PRSV2_KIJ"
                    Case 3
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kji
                        ppu.m_pr.InteractionParameters(id1)(id2).kji = value
                        tp = "PRSV2_KJI"
                End Select
                If Not _form Is Nothing Then
                    _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                       .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, tp, oldvalue, value),
                                                                       .OldValue = oldvalue, .NewValue = CDbl(value), .ObjID = id1, .ObjID2 = id2,
                                                                       .Tag = _pp, .PropertyName = tp})
                End If
            Else
                Dim ppu As PRSV2VLPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                Dim id1 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString.ToLower
                Dim id2 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString.ToLower
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        ppu.m_pr.InteractionParameters(id1)(id2).kij = value
                        tp = "PRSV2VL_KIJ"
                    Case 3
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kji
                        ppu.m_pr.InteractionParameters(id1)(id2).kji = value
                        tp = "PRSV2VL_KJI"
                End Select
                If Not _form Is Nothing Then
                    _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                       .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, tp, oldvalue, value),
                                                                       .OldValue = oldvalue, .NewValue = CDbl(value), .ObjID = id1, .ObjID2 = id2,
                                                                       .Tag = _pp, .PropertyName = tp})
                End If
            End If
        End If
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()
        Dim BIPs As List(Of PropertyPackages.Auxiliary.PRSV2_IPData)

        Dim openedFile As IVirtualFile = filePickerForm.ShowOpenDialog(New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON file", "*.json")})

        If openedFile IsNot Nothing Then

            Try

                BIPs = Newtonsoft.Json.JsonConvert.DeserializeObject(Of List(Of PropertyPackages.Auxiliary.PRSV2_IPData))(openedFile.ReadAllText())

                If MessageBox.Show("Interaction Parameters loaded successfully. Proceed with overwriting current values?",
                                   "Warning", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then

                    For Each row As DataGridViewRow In KryptonDataGridView2.Rows

                        Dim c1 = row.Cells(0).Value
                        Dim c2 = row.Cells(1).Value

                        Dim bip1 = BIPs.Where(Function(b) b.id1 = c1 And b.id2 = c2).FirstOrDefault()
                        Dim bip2 = BIPs.Where(Function(b) b.id2 = c2 And b.id1 = c1).FirstOrDefault()

                        If bip1 IsNot Nothing Then
                            row.Cells(2).Value = bip1.kij
                            row.Cells(3).Value = bip1.kji
                        End If

                        If bip2 IsNot Nothing Then
                            row.Cells(2).Value = bip2.kij
                            row.Cells(3).Value = bip2.kji
                        End If

                    Next

                End If

            Catch ex As Exception

                MessageBox.Show("Error: " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        End If

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click

        Dim BIPs As New List(Of PropertyPackages.Auxiliary.PRSV2_IPData)

        For Each row As DataGridViewRow In KryptonDataGridView2.Rows
            BIPs.Add(New PropertyPackages.Auxiliary.PRSV2_IPData With {.id1 = row.Cells(0).Value, .id2 = row.Cells(1).Value,
                     .kij = row.Cells(2).Value.ToString().ToDoubleFromCurrent(), .kji = row.Cells(3).Value.ToString().ToDoubleFromCurrent()})
        Next

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("JSON File", "*.json")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                    Try
                        Dim jsondata = Newtonsoft.Json.JsonConvert.SerializeObject(BIPs, Newtonsoft.Json.Formatting.Indented)
                        writer.Write(jsondata)
                        handler.Write(stream)
                        MessageBox.Show("File saved successfully.", "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)
                    Catch ex As Exception
                        MessageBox.Show("Error saving file: " + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End Using
            End Using
        End If

    End Sub

End Class