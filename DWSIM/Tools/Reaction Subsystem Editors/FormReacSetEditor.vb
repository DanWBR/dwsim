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


Public Class FormReacSetEditor

    Inherits System.Windows.Forms.Form

    Public mode As String = "Add"
    Public rs As ReactionSet
    Public fc As FormFlowsheet
    Public loaded As Boolean = False

    Private Sub FormReacSetEditor_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        tbName.Focus()
        tbName.ScrollToCaret()

        fc = My.Application.ActiveSimulation

        Select Case mode
            Case "Add"
                rs = New ReactionSet()
            Case "Edit"
        End Select

        For Each rxnbase As ReactionSetBase In rs.Reactions.Values
            With Me.KryptonDataGridView1.Rows
                .Add(New Object() {fc.Options.Reactions(rxnbase.ReactionID).Name, fc.Options.Reactions(rxnbase.ReactionID).ReactionType.ToString, fc.Options.Reactions(rxnbase.ReactionID).Equation, rxnbase.IsActive, rxnbase.Rank, rxnbase.ReactionID})
            End With
        Next

        Me.tbName.Text = rs.Name
        Me.KryptonTextBox1.Text = rs.Description

        Dim add As Boolean = True

        tsddAdd.DropDownItems.Clear()

        For Each rxn As Reaction In fc.Options.Reactions.Values
            add = True
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(5).Value = rxn.ID Then add = False
                Exit For
            Next
            If add = True Then
                Dim tsmi As New ToolStripMenuItem(rxn.Name & " (" & rxn.ReactionType.ToString() & ")") With {.Tag = rxn.ID}
                AddHandler tsmi.Click, AddressOf ReactionItem_click
                tsddAdd.DropDownItems.Add(tsmi)
            End If
        Next

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub ReactionItem_click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim myLink As ToolStripMenuItem = CType(sender, ToolStripMenuItem)

        Dim rxn As Reaction = fc.Options.Reactions(myLink.Tag)
        For Each row1 As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If row1.Cells(5).Value = rxn.ID Then
                MessageBox.Show(DWSIM.App.GetLocalString("ReacaoJaAdicionada"), DWSIM.App.GetLocalString("Erro"))
                Exit Sub
            End If
        Next
        With Me.KryptonDataGridView1.Rows
            .Add(New Object() {rxn.Name, rxn.ReactionType, rxn.Equation, True, 0, rxn.ID})
        End With

    End Sub


    Private Sub KryptonButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton3.Click

        fc.RegisterSnapshot(SnapshotType.ReactionSubsystem)

        rs.Description = Me.KryptonTextBox1.Text
        rs.Name = Me.tbName.Text
        With rs.Reactions
            .Clear()
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                .Add(row.Cells(5).Value, New ReactionSetBase(row.Cells(5).Value, row.Cells(4).Value, row.Cells(3).Value))
            Next
        End With

        'add or edit reaction set
        Select Case mode
            Case "Add"
                rs.ID = Guid.NewGuid().ToString
                fc.Options.ReactionSets.Add(rs.ID, rs)
            Case "Edit"
                fc.Options.ReactionSets(rs.ID) = rs
        End Select

        Me.Close()

    End Sub

    Private Sub KryptonButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton2.Click
        Me.Close()
    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles tsbRemove.Click
        If Not Me.KryptonDataGridView1.SelectedRows(0) Is Nothing Then
            Dim res As MsgBoxResult = MessageBox.Show(DWSIM.App.GetLocalString("RemoverReacaodoConjuntoPergunta"), DWSIM.App.GetLocalString("Pergunta"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
            If res = MsgBoxResult.Yes Then Me.KryptonDataGridView1.Rows.Remove(Me.KryptonDataGridView1.SelectedRows(0))
        End If
    End Sub

    Private Sub KryptonLabel2_Click(sender As Object, e As EventArgs) Handles KryptonLabel2.Click

    End Sub
End Class