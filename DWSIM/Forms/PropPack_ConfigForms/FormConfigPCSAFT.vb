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


Imports DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports DWSIM.DWSIM.Flowsheet

Public Class FormConfigPCSAFT

    Inherits FormConfigBase

    Public Loaded As Boolean = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormUNIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Loaded = False

        Me.Text = DWSIM.App.GetLocalString("ConfigurarPacotedePropriedades") & _pp.Tag & ")"

        With Me.KryptonDataGridView1.Rows
            .Clear()
            For Each kvp As KeyValuePair(Of String, Double) In _pp.Parameters
                .Add(New Object() {kvp.Key, DWSIM.App.GetLocalString(kvp.Key), kvp.Value})
            Next
        End With

        Me.KryptonDataGridView2.DataSource = Nothing

        Me.FaTabStripItem2.Visible = True

        Me.KryptonDataGridView2.Rows.Clear()

        Dim ppu As PropertyPackages.PCSAFTPropertyPackage = _pp

        Dim nf As String = "0.0000"

        For Each cp As ConstantProperties In _comps.Values
gt0:        If ppu.m_pr.InteractionParameters.ContainsKey(cp.CAS_Number) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If cp.CAS_Number <> cp2.CAS_Number Then
                        If Not ppu.m_pr.InteractionParameters(cp.CAS_Number).ContainsKey(cp2.CAS_Number) Then
                            'check if collection has id2 as primary id
                            If ppu.m_pr.InteractionParameters.ContainsKey(cp2.CAS_Number) Then
                                If Not ppu.m_pr.InteractionParameters(cp2.CAS_Number).ContainsKey(cp.CAS_Number) Then
                                    ppu.m_pr.InteractionParameters(cp.CAS_Number).Add(cp2.CAS_Number, New Thermodynamics.PropertyPackages.Auxiliary.PCSIP)
                                    Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.CAS_Number)(cp2.CAS_Number).kij
                                    KryptonDataGridView2.Rows.Add(New Object() {DWSIM.App.GetComponentName(cp.Name), DWSIM.App.GetComponentName(cp2.Name), Format(a12, nf)})
                                    With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                        .Cells(0).Tag = cp.CAS_Number
                                        .Cells(1).Tag = cp2.CAS_Number
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_pr.InteractionParameters(cp.CAS_Number)(cp2.CAS_Number).kij
                            KryptonDataGridView2.Rows.Add(New Object() {DWSIM.App.GetComponentName(cp.Name), DWSIM.App.GetComponentName(cp2.Name), Format(a12, nf)})
                            With KryptonDataGridView2.Rows(KryptonDataGridView2.Rows.Count - 1)
                                .Cells(0).Tag = cp.CAS_Number
                                .Cells(1).Tag = cp2.CAS_Number
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_pr.InteractionParameters.Add(cp.CAS_Number, New Dictionary(Of String, Thermodynamics.PropertyPackages.Auxiliary.PCSIP))
                GoTo gt0
            End If
        Next

        dgvu1.Rows.Clear()

        For Each cp As ConstantProperties In _comps.Values
gt1:        If ppu.m_pr.Data.ContainsKey(cp.CAS_Number) Then
                Dim mw As Double = ppu.m_pr.Data(cp.CAS_Number).mw
                Dim m As Double = ppu.m_pr.Data(cp.CAS_Number).m
                Dim sigma As Double = ppu.m_pr.Data(cp.CAS_Number).sigma
                Dim epsilon As Double = ppu.m_pr.Data(cp.CAS_Number).epsilon
                dgvu1.Rows.Add(New Object() {DWSIM.App.GetComponentName(cp.Name), cp.CAS_Number, mw, m, sigma, epsilon})
            Else
                ppu.m_pr.Data.Add(cp.CAS_Number, New Thermodynamics.PropertyPackages.Auxiliary.PCSParam)
                GoTo gt1
            End If
        Next

        Loaded = True

    End Sub

    Private Sub KryptonDataGridView1_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellEndEdit

        Dim oldvalue = _pp.Parameters(Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(0).Value)
        Dim newvalue = Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(2).Value
        Dim parid As String = Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(0).Value
        Dim parname As String = Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(1).Value

        _pp.Parameters(parid) = newvalue
        If Not _form Is Nothing Then
            _form.AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PropertyPackagePropertyChanged,
                                                               .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_PropertyPackagePropertyChanged"), _pp.Tag, parname, oldvalue, newvalue),
                                                               .OldValue = oldvalue, .NewValue = newvalue, .Tag = _pp, .ObjID = parid, .PropertyName = "PARAM"})
        End If

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Private Sub KryptonDataGridView2_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs)
        If e.FormattedValue <> Nothing Then
            If Double.TryParse(e.FormattedValue, New Double) = False Then
                MessageBox.Show(DWSIM.App.GetLocalString("Ovalorinseridoinvlid"), DWSIM.App.GetLocalString("Parmetroinvlido"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                e.Cancel = True
            End If
        End If
    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            Dim ppu As PropertyPackages.PCSAFTPropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(1).Value.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_pr.Data(id1).mw = value
                Case 3
                    ppu.m_pr.Data(id1).m = value
                Case 4
                    ppu.m_pr.Data(id1).sigma = value
                Case 5
                    ppu.m_pr.Data(id1).epsilon = value
            End Select
        End If
    End Sub

    Private Sub KryptonDataGridView2_CellValueChanged(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView2.CellValueChanged
        If Loaded Then
            Dim ppu As PropertyPackages.PCSAFTPropertyPackage = _pp
            Dim value As Object = KryptonDataGridView2.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = KryptonDataGridView2.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_pr.InteractionParameters(id1)(id2).kij = value
            End Select
        End If
    End Sub

End Class