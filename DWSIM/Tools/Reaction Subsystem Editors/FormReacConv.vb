'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports Ciloci.Flee
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.ExtensionMethods

Public Class FormReacConv

    Inherits System.Windows.Forms.Form

    Public mode As String = "Add"
    Public rc As Reaction
    Public fc As FormFlowsheet
    Public su As SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String
    Public loaded As Boolean = False

    Private Sub FormReacConv_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        fc = My.Application.ActiveSimulation
        su = fc.Options.SelectedUnitSystem
        nf = fc.Options.NumberFormat

        'populate datagrid
        For Each subs As ConstantProperties In fc.Options.SelectedComponents.Values
            With Me.KryptonDataGridView1
                .Rows.Add(New Object() {subs.Name, Format(subs.Molar_Weight, nf), Format(subs.IG_Enthalpy_of_Formation_25C, nf), False, False, 0, subs.Name})
            End With
        Next

        Select Case mode
            Case "Add"
                rc = New Reaction
                Me.Text = DWSIM.App.GetLocalString("AdicionarNovaReacaodeConversao")
            Case "Edit"
                Me.Text = DWSIM.App.GetLocalString("EditarReacaodeConversao")
        End Select

        For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If rc.Components.ContainsKey(row.Cells(6).Value) Then
                row.Cells(3).Value = True
                row.Cells(4).Value = rc.Components(row.Cells(6).Value).IsBaseReactant
                row.Cells(5).Value = rc.Components(row.Cells(6).Value).StoichCoeff
            End If
        Next

        Me.tbName.Text = rc.Name
        Me.tbDesc.Text = rc.Description
        Me.tbCompBase.Text = rc.BaseReactant
        Me.tbEquation.Text = rc.Equation
        Me.tbExp.Text = rc.Expression
        Me.tbReacHeat.Text = rc.ReactionHeat
        Me.tbStoich.Text = rc.StoichBalance

        Select Case rc.ReactionPhase
            Case PhaseName.Vapor
                Me.tbPhase.SelectedIndex = 0
            Case PhaseName.Liquid
                Me.tbPhase.SelectedIndex = 1
            Case PhaseName.Mixture
                Me.tbPhase.SelectedIndex = 2
        End Select

        loaded = True

        Select Case mode
            Case "Edit"
                Call Me.KryptonDataGridView1_CellValidated(sender, New DataGridViewCellEventArgs(3, 0))
        End Select

        'tbName.CanFocus
        tbName.Focus()
        tbName.ScrollToCaret()

    End Sub

    Private Sub KryptonDataGridView1_CellValidated(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellValueChanged

        If e.ColumnIndex = 4 Then
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value = True Then
                    rc.BaseReactant = row.Cells(6).Value
                    Me.tbCompBase.Text = rc.BaseReactant
                    Exit For
                End If
            Next
        End If

        If loaded = True Then

            Dim hp, hr, bp, br, brsc As Double

            Dim eq As String = ""
            'build reaction equation
            'scan for reactants
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(5).Value IsNot Nothing AndAlso row.Cells(5).Value.ToString.IsValidDouble Then
                    If row.Cells(5).Value < 0 And row.Cells(3).Value = True Then
                        If row.Cells(5).Value = -1 Then
                            eq += fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                        Else
                            If row.Cells(5).Value = 1 Then
                                eq += fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                            Else
                                eq += Math.Abs(row.Cells(5).Value.ToString.ToDoubleFromCurrent) & fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                            End If
                        End If
                        hr += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(6).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(6).Value).Molar_Weight
                        br += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(6).Value).Molar_Weight
                    End If
                Else
                    eq += "[ERROR]" & fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                End If
            Next
            If eq.Length >= 2 Then eq = eq.Remove(eq.Length - 2, 2)
            eq += "--> "
            'scan for products
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(5).Value IsNot Nothing AndAlso row.Cells(5).Value.ToString.IsValidDouble Then
                    If row.Cells(5).Value > 0 And row.Cells(3).Value = True Then
                        If row.Cells(5).Value = 1 Then
                            eq += fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                        Else
                            eq += Math.Abs(row.Cells(5).Value.ToString.ToDoubleFromCurrent) & fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                        End If
                        hp += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(6).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(6).Value).Molar_Weight
                        bp += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(6).Value).Molar_Weight
                    End If
                Else
                    eq += "[ERROR]" & fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                End If
            Next
            eq = eq.Remove(eq.Length - 2, 2)

            Me.tbEquation.Text = eq

            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value = True Then
                    If row.Cells(5).Value IsNot Nothing And row.Cells(5).Value.ToString.IsValidDouble Then
                        brsc = Math.Abs(Convert.ToDouble(row.Cells(5).Value))
                        Exit For
                    Else
                        brsc = 1.0
                    End If
                End If
            Next
            Me.tbReacHeat.Text = Format((hp - hr) / brsc, nf)
            rc.ReactionHeat = (hp - hr) / brsc

            If Math.Abs(bp - br) < 0.01 Then
                Me.tbStoich.Text = "OK"

            Else
                Me.tbStoich.Text = Format(bp - br, nf)

            End If

        End If

    End Sub

    Private Sub KryptonButton4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton4.Click

        If Me.tbStoich.Text <> "OK" Then
            MessageBox.Show(DWSIM.App.GetLocalString("VerifiqueEstequiometria"), DWSIM.App.GetLocalString("ErroAoAdicionarReacao"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        Else
            'proceed with expression evaluation

            '// Define the context of our expression
            'ExpressionContext context = new ExpressionContext();
            '// Import all members of the Math type into the default namespace
            'context.Imports.ImportStaticMembers(typeof(Math));
            rc.ExpContext = New Ciloci.Flee.ExpressionContext
            With rc.ExpContext
                .Imports.AddType(GetType(System.Math))
            End With
            With rc
                .ExpContext.Variables.Add("T", Convert.ToDouble(300))
                Try
                    .ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    .Expr = .ExpContext.CompileGeneric(Of Double)(Me.tbExp.Text)
                    .Expression = Me.tbExp.Text
                Catch ex As ExpressionCompileException
                    Select Case ex.Reason
                        Case CompileExceptionReason.SyntaxError
                            MessageBox.Show(DWSIM.App.GetLocalString("Erronasintaxedaexpre"))
                        Case CompileExceptionReason.UndefinedName
                            MessageBox.Show(DWSIM.App.GetLocalString("ErronaexpressoVerifi"))
                        Case Else
                            MessageBox.Show(ex.ToString)
                    End Select
                End Try
            End With

            'Components and stoichiometry
            rc._Components.Clear()
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc._Components.Add(row.Cells(6).Value, New ReactionStoichBase(row.Cells(6).Value, row.Cells(5).Value, row.Cells(4).Value, 0, 0))
                End If
            Next

            'phase and other settings
            Select Case Me.tbPhase.SelectedIndex
                Case 0
                    rc.ReactionPhase = PhaseName.Vapor
                Case 1
                    rc.ReactionPhase = PhaseName.Liquid
                Case 2
                    rc.ReactionPhase = PhaseName.Mixture
            End Select
            'rc.ReactionHeat = Me.tbReacHeat.Text
            rc.Description = Me.tbDesc.Text
            rc.Name = Me.tbName.Text
            rc.Equation = Me.tbEquation.Text
            rc.StoichBalance = 0
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value = True Then
                    rc.BaseReactant = row.Cells(6).Value
                    Exit For
                End If
            Next

            'add or edit reaction
            Select Case mode
                Case "Add"
                    rc.ID = Guid.NewGuid.ToString
                    rc.ReactionType = ReactionType.Conversion
                    fc.Options.Reactions.Add(rc.ID, rc)
                    fc.Options.ReactionSets("DefaultSet").Reactions.Add(rc.ID, New ReactionSetBase(rc.ID, 0, True))
                Case "Edit"
                    fc.Options.Reactions(rc.ID) = rc
            End Select

            Me.Close()

        End If

    End Sub

    Private Sub KryptonButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles KryptonButton3.Click
        Me.Close()
    End Sub

    Private Sub KryptonButton2_Click(sender As Object, e As EventArgs) Handles KryptonButton2.Click

        Dim sum As Double = 0.0
        For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If row.Cells(3).Value = True AndAlso (row.Cells(5).Value Is Nothing OrElse row.Cells(5).Value.ToString.ToDoubleFromCurrent = 0.0) Then
                If tbStoich.Text.IsValidDouble Then
                    row.Cells(5).Value = (-tbStoich.Text.ToDoubleFromCurrent / fc.Options.SelectedComponents(row.Cells(6).Value).Molar_Weight).ToString(nf)
                    Exit For
                End If
            End If
        Next

    End Sub

End Class