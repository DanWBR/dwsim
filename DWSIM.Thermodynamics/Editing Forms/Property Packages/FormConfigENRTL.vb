'    Copyright 2019 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports System.IO
Imports System.Linq
Imports System.Drawing

Public Class FormConfigENRTL

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormLIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Loaded = False

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Me.Text += " (" & _pp.Tag & ")"

        Dim ppu As PropertyPackages.ElectrolyteNRTLPropertyPackage = _pp

        Dim nf As String = "0.####"

        dgvu1.Rows.Clear()

        Dim id1, id2 As String

        For Each cp As ConstantProperties In _comps.Values
            If Not cp.IsIon Then
                id1 = cp.Formula
                If id1 = "HOH" Then id1 = "H2O"
                If id1 = "OCO" Then id1 = "CO2"
                If id1 = "HSH" Then id1 = "H2S"
                If id1 = "HOCH2CH2NH2" Then id1 = "RNH2" 'MEA
                If id1 = "(HOCH2CH2)2NH" Then id1 = "R2NH" 'DEA
                If id1 = "HOCH2CH2NH3+" Then id1 = "RNH3+" 'Protonated MEA
                If id1 = "(HOCH2CH2)2NH2+" Then id1 = "R2NH2+" 'Protonated DEA
gt1:            If ppu.m_enrtl.InteractionParameters.ContainsKey(id1) Then
                    For Each cp2 As ConstantProperties In _comps.Values
                        If Not cp2.IsIon And cp.Name <> cp2.Name Then
                            id2 = cp2.Formula
                            If id2 = "HOH" Then id2 = "H2O"
                            If id2 = "OCO" Then id2 = "CO2"
                            If id2 = "HSH" Then id2 = "H2S"
                            If id2 = "HOCH2CH2NH2" Then id2 = "RNH2" 'MEA
                            If id2 = "(HOCH2CH2)2NH" Then id2 = "R2NH" 'DEA
                            If id2 = "HOCH2CH2NH3+" Then id2 = "RNH3+" 'Protonated MEA
                            If id2 = "(HOCH2CH2)2NH2+" Then id2 = "R2NH2+" 'Protonated DEA
                            If Not ppu.m_enrtl.InteractionParameters(id1).ContainsKey(id2) Then
                                'check if collection has id2 as primary id
                                If ppu.m_enrtl.InteractionParameters.ContainsKey(id2) Then
                                    If Not ppu.m_enrtl.InteractionParameters(id2).ContainsKey(id1) Then
                                        ppu.m_enrtl.InteractionParameters(id1).Add(id2, New PropertyPackages.Auxiliary.ElectrolyteNRTL_IPData)
                                        Dim A12 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).A12
                                        Dim A21 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).A21
                                        Dim B12 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).B12
                                        Dim B21 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).B21
                                        dgvu1.Rows.Add(New Object() {(id1), (id2), Format(A12, nf), Format(A21, nf), Format(B12, nf), Format(B21, nf)})
                                        With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                            .Cells(0).Tag = id1
                                            .Cells(1).Tag = id2
                                            .Cells(0).ToolTipText = cp.Name
                                            .Cells(1).ToolTipText = cp2.Name
                                        End With
                                    End If
                                End If
                            Else
                                Dim A12 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).A12
                                Dim A21 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).A21
                                Dim B12 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).B12
                                Dim B21 As Double = ppu.m_enrtl.InteractionParameters(id1)(id2).B21
                                dgvu1.Rows.Add(New Object() {(id1), (id2), Format(A12, nf), Format(A21, nf), Format(B12, nf), Format(B21, nf)})
                                With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                    .Cells(0).Tag = id1
                                    .Cells(1).Tag = id2
                                    .Cells(0).ToolTipText = cp.Name
                                    .Cells(1).ToolTipText = cp2.Name
                                End With
                            End If
                        End If
                    Next
                Else
                    ppu.m_enrtl.InteractionParameters.Add(id1, New Dictionary(Of String, PropertyPackages.Auxiliary.ElectrolyteNRTL_IPData))
                    GoTo gt1
                End If
            End If
        Next

        Me.cbReacSets.Items.Clear()
        For Each rset As ReactionSet In Me._form.ReactionSets.Values
            cbReacSets.Items.Add(rset.Name)
        Next

        Me.tbMaxIts.Text = ppu.MaxIterations
        Me.tbTol.Text = ppu.Tolerance

        Try
            Dim reacsetname As String = (From rset As ReactionSet In _form.ReactionSets.Values Select rset Where rset.ID = ppu.ReactionSet).FirstOrDefault.Name
            Me.cbReacSets.SelectedItem = reacsetname
        Catch ex As Exception

        End Try

        Loaded = True

    End Sub

    Private Sub KryptonDataGridView1_CellValidating(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellValidatingEventArgs)

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            Dim ppu As PropertyPackages.ElectrolyteNRTLPropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_enrtl.InteractionParameters(id1)(id2).A12 = value
                Case 3
                    ppu.m_enrtl.InteractionParameters(id1)(id2).A21 = value
                Case 4
                    ppu.m_enrtl.InteractionParameters(id1)(id2).B12 = value
                Case 5
                    ppu.m_enrtl.InteractionParameters(id1)(id2).B21 = value
            End Select
        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "ExUNIQUAC_uij.txt")
    End Sub

    Private Sub TextBox1_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbMaxIts.TextChanged
        If Loaded Then
            Dim ppu As PropertyPackages.ElectrolyteNRTLPropertyPackage = _pp
            If Integer.TryParse(tbMaxIts.Text, New Integer) Then
                tbMaxIts.ForeColor = Color.Blue
                ppu.MaxIterations = tbMaxIts.Text
            Else
                tbMaxIts.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub tbTol_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbTol.TextChanged
        If Loaded Then
            Dim ppu As PropertyPackages.ElectrolyteNRTLPropertyPackage = _pp
            If Double.TryParse(tbTol.Text, New Double) Then
                tbTol.ForeColor = Color.Blue
                ppu.Tolerance = tbTol.Text
            Else
                tbTol.ForeColor = Color.Red
            End If
        End If
    End Sub

    Private Sub cbReacSets_SelectedIndexChanged(sender As System.Object, e As System.EventArgs) Handles cbReacSets.SelectedIndexChanged
        If Loaded Then
            Dim ppu As PropertyPackages.ElectrolyteNRTLPropertyPackage = _pp
            Dim reacsetID As String = (From rset As ReactionSet In _form.ReactionSets.Values Select rset Where rset.Name = cbReacSets.SelectedItem.ToString).FirstOrDefault.ID
            ppu.ReactionSet = reacsetID
        End If
    End Sub

End Class