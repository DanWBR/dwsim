'    Copyright 2008-2019 Daniel Wagner O. de Medeiros
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

Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports System.IO
Imports DWSIM.Thermodynamics
Imports System.Windows.Forms
Imports DWSIM
Imports DWSIM.Thermodynamics.AdvancedEOS

Public Class FormConfig

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False

    Private Sub FormConfigPR_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        Me.KryptonDataGridView2.DataSource = Nothing
    End Sub

    Private Sub FormConfigPR_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Me.KryptonDataGridView2.DataSource = Nothing

        Me.FaTabStripItem2.Visible = True

        Me.FaTabStrip1.SelectedTab = FaTabStripItem2

        Me.KryptonDataGridView2.Rows.Clear()

        Dim ci = System.Globalization.CultureInfo.InvariantCulture
        Dim nf As String = "G6"

        If TypeOf _pp Is SoaveRedlichKwongAdvancedPropertyPackage Then

            Dim ppu As SoaveRedlichKwongAdvancedPropertyPackage = _pp

            For Each cp As ConstantProperties In _comps.Values
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.Name <> cp2.Name Then
                        Dim pair1 = cp.Name + "/" + cp2.Name
                        'Dim pair2 = New Tuple(Of String, String)(cp2.Name, cp.Name)
                        If ppu.KijExpressions.ContainsKey(pair1) Then
                            Dim exp As String = ppu.KijExpressions(pair1)
                            KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, exp})
                            With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                .Cells(0).Tag = cp.Name
                                .Cells(1).Tag = cp2.Name
                            End With
                            'ElseIf ppu.KijExpressions.ContainsKey(pair2) Then
                            '    If Not ppu.KijExpressions.ContainsKey(pair1) Then
                            '        Dim exp As String = ppu.KijExpressions(pair2)
                            '        KryptonDataGridView2.Rows.Add(New Object() {cp2.Name, cp.Name, exp})
                            '        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                            '            .Cells(0).Tag = cp2.Name
                            '            .Cells(1).Tag = cp.Name
                            '        End With
                            '    End If
                        Else
                            If Not ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                                ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, Auxiliary.PR_IPData)())
                            End If
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, a12.ToString(nf, ci)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, a12.ToString(nf, ci)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    End If
                Next
            Next

        ElseIf TypeOf _pp Is PengRobinson1978AdvancedPropertyPackage Then

            Dim ppu As PengRobinson1978AdvancedPropertyPackage = _pp

            For Each cp As ConstantProperties In _comps.Values
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.Name <> cp2.Name Then
                        Dim pair1 = cp.Name + "/" + cp2.Name
                        'Dim pair2 = New Tuple(Of String, String)(cp2.Name, cp.Name)
                        If ppu.KijExpressions.ContainsKey(pair1) Then
                            Dim exp As String = ppu.KijExpressions(pair1)
                            KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, exp})
                            With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                .Cells(0).Tag = cp.Name
                                .Cells(1).Tag = cp2.Name
                            End With
                            'ElseIf ppu.KijExpressions.ContainsKey(pair2) Then
                            '    If Not ppu.KijExpressions.ContainsKey(pair1) Then
                            '        Dim exp As String = ppu.KijExpressions(pair2)
                            '        KryptonDataGridView2.Rows.Add(New Object() {cp2.Name, cp.Name, exp})
                            '        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                            '            .Cells(0).Tag = cp2.Name
                            '            .Cells(1).Tag = cp.Name
                            '        End With
                            '    End If
                        Else
                            If Not ppu.m_pr.InteractionParameters.ContainsKey(cp.Name) Then
                                ppu.m_pr.InteractionParameters.Add(cp.Name, New Dictionary(Of String, Auxiliary.PR_IPData)())
                            End If
                            If Not ppu.m_pr.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                                'check if collection has id2 as primary id
                                If ppu.m_pr.InteractionParameters.ContainsKey(cp2.Name) Then
                                    If Not ppu.m_pr.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                        ppu.m_pr.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.PR_IPData)
                                        Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                        KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, a12.ToString(nf, ci)})
                                        With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                            .Cells(0).Tag = cp.Name
                                            .Cells(1).Tag = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.Name)(cp2.Name).kij
                                KryptonDataGridView2.Rows.Add(New Object() {cp.Name, cp2.Name, a12.ToString(nf, ci)})
                                With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                    .Cells(0).Tag = cp.Name
                                    .Cells(1).Tag = cp2.Name
                                End With
                            End If
                        End If
                    End If
                Next
            Next

        End If

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True

    End Sub

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim oldvalue, newvalue As Object
            Dim id1 As String
            Dim id2 As String
            If TypeOf _pp Is SoaveRedlichKwongAdvancedPropertyPackage Then
                Dim ppu As SoaveRedlichKwongAdvancedPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        newvalue = value
                        Dim pair1 = id1 + "/" + id2
                        Dim pair2 = id2 + "/" + id1
                        If ppu.KijExpressions.ContainsKey(pair1) Then
                            oldvalue = ppu.KijExpressions(pair1)
                            ppu.KijExpressions(pair1) = newvalue
                        ElseIf ppu.KijExpressions.ContainsKey(pair2) Then
                            oldvalue = ppu.KijExpressions(pair2)
                            ppu.KijExpressions(pair2) = newvalue
                        Else
                            oldvalue = 0.0
                            ppu.KijExpressions.Add(pair1, newvalue)
                        End If
                End Select
                If Not _form Is Nothing Then
                    _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                       .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, "PR_IP", oldvalue, newvalue),
                                                                       .OldValue = oldvalue, .NewValue = newvalue, .ObjID = id1, .ObjID2 = id2,
                                                                       .Tag = _pp, .PropertyName = "PR_IP"})
                End If
            ElseIf TypeOf _pp Is PengRobinson1978AdvancedPropertyPackage Then
                Dim ppu As PengRobinson1978AdvancedPropertyPackage = _pp
                Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
                id1 = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
                id2 = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
                Select Case e.ColumnIndex
                    Case 2
                        oldvalue = ppu.m_pr.InteractionParameters(id1)(id2).kij
                        newvalue = value
                        Dim pair1 = id1 + "/" + id2
                        Dim pair2 = id2 + "/" + id1
                        If ppu.KijExpressions.ContainsKey(pair1) Then
                            ppu.KijExpressions(pair1) = newvalue
                        ElseIf ppu.KijExpressions.ContainsKey(pair2) Then
                            ppu.KijExpressions(pair2) = newvalue
                        Else
                            ppu.KijExpressions.Add(pair1, newvalue)
                        End If
                End Select
                If Not _form Is Nothing Then
                    _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                       .Name = String.Format(_pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, "PR_IP", oldvalue, newvalue),
                                                                       .OldValue = oldvalue, .NewValue = newvalue, .ObjID = id1, .ObjID2 = id2,
                                                                       .Tag = _pp, .PropertyName = "PR_IP"})
                End If
            End If
        End If

    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click

        For Each r2 As DataGridViewCell In Me.KryptonDataGridView2.SelectedCells

            Dim r = Me.KryptonDataGridView2.Rows(r2.RowIndex)

            Dim id1 As String = r.Cells(0).Tag.ToString
            Dim id2 As String = r.Cells(1).Tag.ToString

            Dim comp1, comp2 As ConstantProperties
            comp1 = _comps(id1)
            comp2 = _comps(id2)

            Dim Vc1 As Double = comp1.Critical_Volume
            Dim Vc2 As Double = comp2.Critical_Volume

            Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

            r.Cells(2).Value = tmp

        Next

    End Sub

End Class
