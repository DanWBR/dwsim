﻿'    Copyright 2008 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU Lesser General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU Lesser General Public License for more details.
'
'    You should have received a copy of the GNU Lesser General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.


Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms

Public Class FormConfigLKP

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False

    Private Sub FormConfigPR_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing
        Me.KryptonDataGridView2.DataSource = Nothing
    End Sub

    Private Sub FormConfigPR_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Me.KryptonDataGridView2.DataSource = Nothing

        If _pp.ComponentName.ToString.Contains("Raoult") Or _
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Vapor")) Or _
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Chao-Seader")) Or _
           _pp.ComponentName.ToString.Contains(Calculator.GetLocalString("Grayson-Streed")) Then
            Me.FaTabStripItem2.Visible = False
            Me.FaTabStripItem1.Selected = True
            Exit Sub
        Else
            Me.FaTabStripItem2.Visible = True
            Me.FaTabStripItem1.Selected = True
        End If
        Me.FaTabStrip1.SelectedItem = Me.FaTabStripItem1

        Me.KryptonDataGridView2.Rows.Clear()

        Dim ppu As New PropertyPackages.LKPPropertyPackage
        ppu = _pp

        Dim nf As String = "0.####"

        For Each cp As ConstantProperties In _comps.Values
gt1:        If ppu.m_lk.InteractionParameters.ContainsKey(cp.Name) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.Name <> cp2.Name Then
                        If Not ppu.m_lk.InteractionParameters(cp.Name).ContainsKey(cp2.Name) Then
                            'check if collection has id2 as primary id
                            If ppu.m_lk.InteractionParameters.ContainsKey(cp2.Name) Then
                                If Not ppu.m_lk.InteractionParameters(cp2.Name).ContainsKey(cp.Name) Then
                                    ppu.m_lk.InteractionParameters(cp.Name).Add(cp2.Name, New PropertyPackages.Auxiliary.LKP_IPData)
                                    Dim a12 As Double = ppu.m_lk.InteractionParameters(cp.Name)(cp2.Name).kij
                                    KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf)})
                                    With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                        .Cells(0).Tag = cp.Name
                                        .Cells(1).Tag = cp2.Name
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_lk.InteractionParameters(cp.Name)(cp2.Name).kij
                            KryptonDataGridView2.Rows.Add(New Object() {(cp.Name), (cp2.Name), Format(a12, nf)})
                            With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                .Cells(0).Tag = cp.Name
                                .Cells(1).Tag = cp2.Name
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_lk.InteractionParameters.Add(cp.Name, New Dictionary(Of String, PropertyPackages.Auxiliary.LKP_IPData))
                GoTo gt1
            End If
        Next

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Not Me.KryptonDataGridView2.SelectedCells(0) Is Nothing Then
            If Me.KryptonDataGridView2.SelectedCells(0).RowIndex <> Me.KryptonDataGridView2.SelectedCells(0).ColumnIndex Then
                Dim Vc1 As Double = _comps(Me.KryptonDataGridView2.Rows(Me.KryptonDataGridView2.SelectedCells(0).RowIndex).Tag).Critical_Volume
                Dim Vc2 As Double = _comps(Me.KryptonDataGridView2.Columns(Me.KryptonDataGridView2.SelectedCells(0).ColumnIndex).Tag).Critical_Volume

                Dim tmp As Double = 1 - 8 * (Vc1 * Vc2) ^ 0.5 / ((Vc1 ^ (1 / 3) + Vc2 ^ (1 / 3)) ^ 3)

                Me.KryptonDataGridView2.SelectedCells(0).Value = tmp

            End If
        End If
    End Sub

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim oldvalue As Double
            Dim ppu As Object = _pp
            ppu = _pp
            Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    oldvalue = ppu.m_lk.InteractionParameters(id1)(id2).kij
                    ppu.m_lk.InteractionParameters(id1)(id2).kij = CDbl(value)
            End Select
            If Not _form Is Nothing Then
                _form.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = Interfaces.Enums.UndoRedoActionType.PropertyPackagePropertyChanged,
                                                                  .Name = _pp.Flowsheet.GetTranslatedString("UndoRedo_PropertyPackagePropertyChanged"),
                                                                    .OldValue = oldvalue, .NewValue = CDbl(value), .ObjID = id1, .ObjID2 = id2,
                                                                                 .Tag = _pp, .PropertyName = "LK_IP"})
            End If
        End If
    End Sub

    Private Sub Button3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button3.Click
        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex
        Me.KryptonDataGridView2.Rows(row).Cells(2).Value = EstimateBinaryIP(4)
    End Sub

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button1.Click
        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex
        Me.KryptonDataGridView2.Rows(row).Cells(2).Value = EstimateBinaryIP(3)
    End Sub

    Private Sub Button2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button2.Click
        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex
        Me.KryptonDataGridView2.Rows(row).Cells(2).Value = EstimateBinaryIP(2)
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex
        Me.KryptonDataGridView2.Rows(row).Cells(2).Value = EstimateBinaryIP(1)
    End Sub

    Function EstimateBinaryIP(ByVal eqtype As Integer) As Double

        Dim row As Integer = Me.KryptonDataGridView2.SelectedCells(0).RowIndex

        Dim id1 As String = Me.KryptonDataGridView2.Rows(row).Cells(0).Tag.ToString
        Dim id2 As String = Me.KryptonDataGridView2.Rows(row).Cells(1).Tag.ToString

        Dim comp1, comp2 As ConstantProperties
        comp1 = _comps(id1)
        comp2 = _comps(id2)

        Dim Vc1 As Double = comp1.Critical_Volume
        Dim Vc2 As Double = comp2.Critical_Volume
        Dim Tc1 As Double = comp1.Critical_Temperature
        Dim Tc2 As Double = comp2.Critical_Temperature


        Dim x, y As Double

        x = 1 / (Tc1 * Vc1 / (Tc2 * Vc2))

        Select Case eqtype
            Case 1
                y = -0.0008419 * x ^ 2 + 0.046687 * x + 0.90024
            Case 2
                y = -0.001 * x ^ 2 + 0.0477 * x + 0.9051
            Case 3
                y = -0.00005 * x ^ 2 + 0.0215 * x + 0.8848
            Case 4
                y = -0.00008 * x ^ 2 + 0.0227 * x + 1.0776
        End Select

        Return y

    End Function

End Class