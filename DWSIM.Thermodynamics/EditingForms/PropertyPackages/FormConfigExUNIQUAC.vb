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
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms
Imports System.IO
Imports System.Linq
Imports System.Drawing

Public Class FormConfigExUNIQUAC

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormLIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Loaded = False

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Dim ppu As PropertyPackages.ExUNIQUACPropertyPackage = _pp

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
                    If Not ppu.m_uni.InteractionParameters(id1).ContainsKey(id2) Then
                        'check if collection has id2 as primary id
                        If ppu.m_uni.InteractionParameters.ContainsKey(id2) Then
                            If Not ppu.m_uni.InteractionParameters(id2).ContainsKey(id1) Then
                                ppu.m_uni.InteractionParameters(id1).Add(id2, New PropertyPackages.Auxiliary.ExUNIQUAC_IPData)
                                Dim uij0 As Double = ppu.m_uni.InteractionParameters(id1)(id2).uij0
                                Dim uijT As Double = ppu.m_uni.InteractionParameters(id1)(id2).uijT
                                dgvu1.Rows.Add(New Object() {(id1), (id2), Format(uij0, nf), Format(uijT, nf)})
                                With dgvu1.Rows(dgvu1.Rows.Count - 1)
                                    .Cells(0).Tag = id1
                                    .Cells(1).Tag = id2
                                End With
                            End If
                        End If
                    Else
                        Dim uij0 As Double = ppu.m_uni.InteractionParameters(id1)(id2).uij0
                        Dim uijT As Double = ppu.m_uni.InteractionParameters(id1)(id2).uijT
                        dgvu1.Rows.Add(New Object() {(id1), (id2), Format(uij0, nf), Format(uijT, nf)})
                        With dgvu1.Rows(dgvu1.Rows.Count - 1)
                            .Cells(0).Tag = id1
                            .Cells(1).Tag = id2
                        End With
                    End If
                Next
            Else
                ppu.m_uni.InteractionParameters.Add(id1, New Dictionary(Of String, PropertyPackages.Auxiliary.ExUNIQUAC_IPData))
                GoTo gt1
            End If
        Next

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
            Dim ppu As PropertyPackages.ExUNIQUACPropertyPackage = _pp
            Dim value As Object = dgvu1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value
            Dim id1 As String = dgvu1.Rows(e.RowIndex).Cells(0).Tag.ToString
            Dim id2 As String = dgvu1.Rows(e.RowIndex).Cells(1).Tag.ToString
            Select Case e.ColumnIndex
                Case 2
                    ppu.m_uni.InteractionParameters(id1)(id2).uij0 = value
                Case 3
                    ppu.m_uni.InteractionParameters(id1)(id2).uijT = value
            End Select
        End If
    End Sub

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "ExUNIQUAC_uij.txt")
    End Sub

End Class