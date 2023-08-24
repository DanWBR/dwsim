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

Public Class FormConfigDH

    Inherits FormConfigPropertyPackageBase

    Public Loaded = False
    Public param As System.Collections.Specialized.StringDictionary

    Private Sub ConfigFormLIQUAC_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Loaded = False

        FaTabStripItem1.Controls.Add(New PropertyPackageSettingsEditingControl(_pp) With {.Dock = DockStyle.Fill})

        Me.Text += " (" & _pp.Tag & ") [" + _pp.ComponentName + "]"

        Dim ppu As PropertyPackages.ElectrolyteBasePropertyPackage = _pp

        Dim nf As String = "0.####"

        Dim id1 As String

        For Each cp As ConstantProperties In _comps.Values
            If Not cp.IsIon Then
                id1 = cp.Name
            Else
                id1 = cp.Formula
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

    Private Sub Button4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        Process.Start(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "data" & Path.DirectorySeparatorChar & "ExUNIQUAC_uij.txt")
    End Sub

    Private Sub TextBox1_TextChanged(sender As System.Object, e As System.EventArgs) Handles tbMaxIts.TextChanged
        If Loaded Then
            Dim ppu As PropertyPackages.ElectrolyteBasePropertyPackage = _pp
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
            Dim ppu As PropertyPackages.ElectrolyteBasePropertyPackage = _pp
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
            Dim ppu As PropertyPackages.ElectrolyteBasePropertyPackage = _pp
            Dim reacsetID As String = (From rset As ReactionSet In _form.ReactionSets.Values Select rset Where rset.Name = cbReacSets.SelectedItem.ToString).FirstOrDefault.ID
            ppu.ReactionSet = reacsetID
        End If
    End Sub

End Class