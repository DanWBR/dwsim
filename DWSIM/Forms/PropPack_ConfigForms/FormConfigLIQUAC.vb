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


Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports System.IO
Imports System.Linq

Public Class FormConfigLIQUAC

    Inherits FormConfigBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormLIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Loaded = False

        Me.Text = DWSIM.App.GetLocalString("ConfigurarPacotedePropriedades") & _pp.Tag & ")"

        With Me.KryptonDataGridView1.Rows
            .Clear()
            For Each kvp As KeyValuePair(Of String, Double) In _pp.Parameters
                .Add(New Object() {kvp.Key, DWSIM.App.GetLocalString(kvp.Key), kvp.Value})
            Next
        End With

        Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp

        Dim nf As String = "0.####"

        dgvu1.Rows.Clear()

        Dim id1, id2 As String

        For Each cp As ConstantProperties In _comps.Values
            If Not cp.IsIon Then
                id1 = cp.Name
            Else
                id1 = cp.Formula
            End If
gt1:        If ppu.m_uni.InteractionParameters.ContainsKey(id1) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If Not cp2.IsIon Then
                        id2 = cp2.Name
                    Else
                        id2 = cp2.Formula
                    End If
                    If id1 <> id2 Then
                        If Not ppu.m_uni.InteractionParameters(id1).ContainsKey(id2) Then
                            'check if collection has id2 as primary id
                            If ppu.m_uni.InteractionParameters.ContainsKey(id2) Then
                                If Not ppu.m_uni.InteractionParameters(id2).ContainsKey(id1) Then
                                    ppu.m_uni.InteractionParameters(id1).Add(id2, New DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LIQUAC2_IPData)
                                    Dim a12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).A12
                                    Dim a21 As Double = ppu.m_uni.InteractionParameters(id1)(id2).A21
                                    Dim b12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).B12
                                    Dim c12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).C12
                                    dgvu1.Rows.Add(New Object() {DWSIM.App.GetComponentName(id1), DWSIM.App.GetComponentName(id2), Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(c12, nf)})
                                    With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                        .Cells(0).Tag = id1
                                        .Cells(1).Tag = id2
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).A12
                            Dim a21 As Double = ppu.m_uni.InteractionParameters(id1)(id2).A21
                            Dim b12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).B12
                            Dim c12 As Double = ppu.m_uni.InteractionParameters(id1)(id2).C12
                            dgvu1.Rows.Add(New Object() {DWSIM.App.GetComponentName(id1), DWSIM.App.GetComponentName(id2), Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(c12, nf)})
                            With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                .Cells(0).Tag = id1
                                .Cells(1).Tag = id2
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_uni.InteractionParameters.Add(id1, New Dictionary(Of String, DWSIM.SimulationObjects.PropertyPackages.Auxiliary.LIQUAC2_IPData))
                GoTo gt1
            End If
        Next

        Me.cbReacSets.Items.Clear()
        For Each rset As ReactionSet In Me._form.Options.ReactionSets.Values
            cbReacSets.Items.Add(rset.Name)
        Next

        Me.tbMaxIts.Text = ppu.ElectrolyteFlash.MaximumIterations
        Me.tbTol.Text = ppu.ElectrolyteFlash.Tolerance

        Me.chkCalcChemEq.Checked = ppu.ElectrolyteFlash.CalculateChemicalEquilibria

        Try
            Dim reacsetname As String = (From rset As ReactionSet In _form.Options.ReactionSets.Values Select rset Where rset.ID = ppu.ElectrolyteFlash.ReactionSet).FirstOrDefault.Name
            Me.cbReacSets.SelectedItem = reacsetname
        Catch ex As Exception

        End Try

        Loaded = True

    End Sub

    Private Sub KryptonDataGridView1_CellEndEdit(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellEndEdit

        _pp.Parameters(Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(0).Value) = Me.KryptonDataGridView1.Rows(e.RowIndex).Cells(2).Value

    End Sub

    Private Sub KryptonDataGridView1_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs) Handles KryptonDataGridView1.CellValidating

        'If Me.Loaded Then
        '    If e.ColumnIndex = 1 Then
        '        If Double.TryParse(e.FormattedValue, New Integer) = False Then
        '            Me.KryptonDataGridView1.Rows(e.RowIndex).ErrorText = _
        '                DWSIM.App.GetLocalString("Ovalorinseridoinvlid")
        '            e.Cancel = True
        '        ElseIf CDbl(e.FormattedValue) < 0 Then
        '            Me.KryptonDataGridView1.Rows(e.RowIndex).ErrorText = _
        '                DWSIM.App.GetLocalString("Ovalorinseridoinvlid")
        '            e.Cancel = True
        '        End If
        '    End If
        'End If

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_uni.InteractionParameters(id1)(id2).A12 = value
                Case 3
                    ppu.m_uni.InteractionParameters(id1)(id2).A21 = value
                Case 4
                    ppu.m_uni.InteractionParameters(id1)(id2).B12 = value
                Case 5
                    ppu.m_uni.InteractionParameters(id1)(id2).C12 = value
            End Select
        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Button4.Click
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "uniquacip.dat")
    End Sub

    Private Sub TextBox1_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbMaxIts.TextChanged
        If Loaded Then
            Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp
            If Integer.TryParse(tbMaxIts.Text, New Integer) Then
                tbMaxIts.ForeColor = Color.Blue
                ppu.ElectrolyteFlash.MaximumIterations = tbMaxIts.Text
            Else
                tbMaxIts.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub tbTol_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbTol.TextChanged
        If Loaded Then
            Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp
            If Double.TryParse(tbTol.Text, New Double) Then
                tbTol.ForeColor = Color.Blue
                ppu.ElectrolyteFlash.Tolerance = tbTol.Text
            Else
                tbTol.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub chkCalcChemEq_CheckedChanged(sender As System.Object, e As System.EventArgs) Handles chkCalcChemEq.CheckedChanged
        If Loaded Then
            Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp
            ppu.ElectrolyteFlash.CalculateChemicalEquilibria = chkCalcChemEq.Checked
        End If
    End Sub

    Private Sub cbReacSets_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbReacSets.SelectedIndexChanged
        If Loaded Then
            Dim ppu As DWSIM.SimulationObjects.PropertyPackages.LIQUAC2PropertyPackage = _pp
            Dim reacsetID As String = (From rset As ReactionSet In _form.Options.ReactionSets.Values Select rset Where rset.Name = cbReacSets.SelectedItem.ToString).FirstOrDefault.ID
            ppu.ElectrolyteFlash.ReactionSet = reacsetID
        End If
    End Sub

End Class