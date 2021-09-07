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

Imports System.Drawing
Imports System.IO
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms

Public Class FormConfigLIQUAC

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormLIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Loaded = False

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Dim ppu As LIQUAC2PropertyPackage = _pp

        Dim nf As String = "0.####"

        dgvu1.Rows.Clear()

        Dim id1, id2 As String

        For Each cp As ConstantProperties In _comps.Values
            If Not cp.IsIon Then
                id1 = cp.Name
            Else
                id1 = cp.Formula
            End If
gt1:        If ppu.m_liquac.InteractionParameters.ContainsKey(id1) Then
                For Each cp2 As ConstantProperties In _comps.Values
                    If Not cp2.IsIon Then
                        id2 = cp2.Name
                    Else
                        id2 = cp2.Formula
                    End If
                    If id1 <> id2 Then
                        If Not ppu.m_liquac.InteractionParameters(id1).ContainsKey(id2) Then
                            'check if collection has id2 as primary id
                            If ppu.m_liquac.InteractionParameters.ContainsKey(id2) Then
                                If Not ppu.m_liquac.InteractionParameters(id2).ContainsKey(id1) Then
                                    ppu.m_liquac.InteractionParameters(id1).Add(id2, New Auxiliary.LIQUAC2_IPData)
                                    Dim a12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).A12
                                    Dim a21 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).A21
                                    Dim b12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).B12
                                    Dim c12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).C12
                                    dgvu1.Rows.Add(New Object() {id1, id2, Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(c12, nf)})
                                    With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                        .Cells(0).Tag = id1
                                        .Cells(1).Tag = id2
                                    End With
                                End If
                            End If
                        Else
                            Dim a12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).A12
                            Dim a21 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).A21
                            Dim b12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).B12
                            Dim c12 As Double = ppu.m_liquac.InteractionParameters(id1)(id2).C12
                            dgvu1.Rows.Add(New Object() {id1, id2, Format(a12, nf), Format(a21, nf), Format(b12, nf), Format(c12, nf)})
                            With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                .Cells(0).Tag = id1
                                .Cells(1).Tag = id2
                            End With
                        End If
                    End If
                Next
            Else
                ppu.m_liquac.InteractionParameters.Add(id1, New Dictionary(Of String, Auxiliary.LIQUAC2_IPData))
                GoTo gt1
            End If
        Next

        Me.cbReacSets.Items.Clear()
        For Each rset As ReactionSet In Me._form.Options.ReactionSets.Values
            cbReacSets.Items.Add(rset.Name)
        Next

        Me.tbMaxIts.Text = ppu.MaxIterations
        Me.tbTol.Text = ppu.Tolerance

        Try
            Dim reacsetname As String = (From rset As ReactionSet In _pp.Flowsheet.ReactionSets.Values Select rset Where rset.ID = ppu.ReactionSet).FirstOrDefault.Name
            Me.cbReacSets.SelectedItem = reacsetname
        Catch ex As Exception

        End Try

        Loaded = True

    End Sub

    Private Sub FormConfigPR_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Loaded = True
    End Sub

    Public Sub RefreshIPTable()

    End Sub

    Private Sub dgvu1_CellValueChanged(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles dgvu1.CellValueChanged
        If Loaded Then
            Dim ppu As LIQUAC2PropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_liquac.InteractionParameters(id1)(id2).A12 = value
                Case 3
                    ppu.m_liquac.InteractionParameters(id1)(id2).A21 = value
                Case 4
                    ppu.m_liquac.InteractionParameters(id1)(id2).B12 = value
                Case 5
                    ppu.m_liquac.InteractionParameters(id1)(id2).C12 = value
            End Select
        End If
    End Sub

    Private Sub TextBox1_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbMaxIts.TextChanged
        If Loaded Then
            Dim ppu As LIQUAC2PropertyPackage = _pp
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
            Dim ppu As LIQUAC2PropertyPackage = _pp
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
            Dim ppu As LIQUAC2PropertyPackage = _pp
            Dim reacsetID As String = (From rset As ReactionSet In _form.ReactionSets.Values Select rset Where rset.Name = cbReacSets.SelectedItem.ToString).FirstOrDefault.ID
            ppu.ReactionSet = reacsetID
        End If
    End Sub

End Class