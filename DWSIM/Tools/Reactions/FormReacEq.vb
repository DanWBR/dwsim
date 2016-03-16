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


Imports Ciloci.Flee
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses

Public Class FormReacEq

    Inherits System.Windows.Forms.Form

    Public mode As String = "Add"
    Public rc As Reaction
    Public fc As FormFlowsheet
    Public su As DWSIM.SystemsOfUnits.Units
    Public cv As New DWSIM.SystemsOfUnits.Converter
    Public nf As String
    Public loaded As Boolean = False

    Private Sub FormReacEq_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.cbBase.Enabled = True

        tbName.Focus()
        tbName.ScrollToCaret()

        fc = My.Application.ActiveSimulation
        su = fc.Options.SelectedUnitSystem
        nf = fc.Options.NumberFormat

        'populate datagrid
        For Each subs As ConstantProperties In fc.Options.SelectedComponents.Values
            With Me.KryptonDataGridView1
                .Rows.Add(New Object() {DWSIM.App.GetComponentName(subs.Name), Format(subs.Molar_Weight, nf), False, False, 0, subs.Name})
            End With
        Next

        Select Case mode
            Case "Add"
                rc = New Reaction
                Me.Text = DWSIM.App.GetLocalString("AdicionarNovaReacaodeEquilibrio")
            Case "Edit"
                Me.Text = DWSIM.App.GetLocalString("EditarReacaodeEquilibrio")
        End Select

        For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If rc.Components.ContainsKey(row.Cells(5).Value) Then
                row.Cells(2).Value = True
                row.Cells(3).Value = rc.Components(row.Cells(5).Value).IsBaseReactant
                row.Cells(4).Value = rc.Components(row.Cells(5).Value).StoichCoeff
            End If
        Next

        Me.tbName.Text = rc.Name
        Me.tbDesc.Text = rc.Description
        Me.cbBase.Text = rc.BaseReactant
        Me.tbEquation.Text = rc.Equation
        Me.tbExp.Text = rc.Expression
        Me.tbReacHeat.Text = rc.ReactionHeat
        Me.tbStoich.Text = rc.StoichBalance
        Me.tbApproach.Text = rc.Approach
        Me.tbTmin.Text = rc.Tmin
        Me.tbTmax.Text = rc.Tmax
        Me.tbKeqConstant.Text = rc.ConstantKeqValue

        Select Case rc.ReactionPhase
            Case PhaseName.Vapor
                Me.tbPhase.SelectedIndex = 0
            Case PhaseName.Liquid
                Me.tbPhase.SelectedIndex = 1
        End Select

        Select Case rc.ReactionBasis
            Case ReactionBasis.Activity
                Me.cbBase.SelectedIndex = 0
            Case ReactionBasis.Fugacity
                Me.cbBase.SelectedIndex = 1
            Case ReactionBasis.MolarConc
                Me.cbBase.SelectedIndex = 2
            Case ReactionBasis.MassConc
                Me.cbBase.SelectedIndex = 3
            Case ReactionBasis.MolarFrac
                Me.cbBase.SelectedIndex = 4
            Case ReactionBasis.MolarConc
                Me.cbBase.SelectedIndex = 5
            Case ReactionBasis.PartialPress
                Me.cbBase.SelectedIndex = 6
        End Select

        Select Case rc.KExprType
            Case Reaction.KOpt.Expression
                Me.rbKeqfT.Checked = True
            Case Reaction.KOpt.Gibbs
                Me.rbKeqGibbs.Checked = True
            Case Reaction.KOpt.Constant
                Me.rbKeqConstant.Checked = True
        End Select

        loaded = True

        Select Case mode
            Case "Edit"
                Call Me.KryptonDataGridView1_CellValidated(sender, New DataGridViewCellEventArgs(3, 0))
        End Select

    End Sub

    Private Sub KryptonContextMenuItem_Click(ByVal sender As Object, ByVal e As System.EventArgs)

        'Dim kmi As ComponentFactory.Krypton.Toolkit.KryptonContextMenuItem = CType(sender, ComponentFactory.Krypton.Toolkit.KryptonContextMenuItem)

        'Dim sp As Integer = kmi.Text.IndexOf("(")

        'Dim substr As String = kmi.Text.Substring(sp + 1, kmi.Text.Length - sp - 2)

        'tbExp.Text += substr
        'tbExp.SelectionStart = tbExp.Text.Length
        'tbExp.ScrollToCaret()

    End Sub

    Private Sub KryptonDataGridView1_CellValidated(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellValueChanged

        If e.ColumnIndex = 3 Then
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc.BaseReactant = row.Cells(5).Value
                    Me.cbBase.Text = Global.DWSIM.DWSIM.App.GetComponentName(rc.BaseReactant)
                    Exit For
                End If
            Next
        End If

        If loaded = True Then

            Dim hp, hr, bp, br, brsc, gp, gr As Double

            Dim eq As String = ""
            'build reaction equation
            'scan for reactants
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value < 0 And row.Cells(2).Value = True Then
                    If row.Cells(4).Value = -1 Then
                        eq += fc.Options.SelectedComponents(row.Cells(5).Value).Formula & " + "
                    Else
                        eq += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) & fc.Options.SelectedComponents(row.Cells(5).Value).Formula & " + "
                    End If
                    hr += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                    br += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                    gr += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                End If
            Next
            If eq.Length >= 2 Then eq = eq.Remove(eq.Length - 2, 2)
            eq += "<--> "
            'scan for products
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value > 0 And row.Cells(2).Value = True Then
                    If row.Cells(4).Value = 1 Then
                        eq += fc.Options.SelectedComponents(row.Cells(5).Value).Formula & " + "
                    Else
                        eq += Math.Abs(Convert.ToInt32(row.Cells(4).Value)) & fc.Options.SelectedComponents(row.Cells(5).Value).Formula & " + "
                    End If
                    hp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                    bp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                    gp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(5).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(5).Value).Molar_Weight
                End If
            Next
            eq = eq.Remove(eq.Length - 2, 2)

            Me.tbEquation.Text = eq

            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    brsc = Math.Abs(Convert.ToDouble(row.Cells(4).Value))
                    Exit For
                End If
            Next
            Me.tbReacHeat.Text = Format((hp - hr) / brsc, nf)
            rc.ReactionHeat = (hp - hr) / brsc
            Me.tbDelG.Text = Format((gp - gr) / brsc, nf)
            rc.ReactionGibbsEnergy = (gp - gr) / brsc

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
            If Me.rbKeqfT.Checked Then
                'proceed with expression evaluation
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
                        Exit Sub
                    End Try
                End With
            End If

            'Components and stoichiometry
            rc.Components.Clear()
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(2).Value = True Then
                    rc.Components.Add(row.Cells(5).Value, New ReactionStoichBase(row.Cells(5).Value, row.Cells(4).Value, row.Cells(3).Value, 0, 0))
                End If
            Next

            'Keq option
            If Me.rbKeqConstant.Checked Then rc.KExprType = Reaction.KOpt.Constant
            If Me.rbKeqfT.Checked Then rc.KExprType = Reaction.KOpt.Expression
            If Me.rbKeqGibbs.Checked Then rc.KExprType = Reaction.KOpt.Gibbs

            'phase and other settings
            Select Case Me.tbPhase.SelectedIndex
                Case 0
                    rc.ReactionPhase = PhaseName.Vapor
                Case 1
                    rc.ReactionPhase = PhaseName.Liquid
            End Select

            Select Case Me.cbBase.SelectedIndex
                Case 0
                    rc.ReactionBasis = ReactionBasis.Activity
                Case 1
                    rc.ReactionBasis = ReactionBasis.Fugacity
                Case 2
                    rc.ReactionBasis = ReactionBasis.MolarConc
                Case 3
                    rc.ReactionBasis = ReactionBasis.MassConc
                Case 4
                    rc.ReactionBasis = ReactionBasis.MolarFrac
                Case 5
                    rc.ReactionBasis = ReactionBasis.MolarConc
                Case 6
                    rc.ReactionBasis = ReactionBasis.PartialPress
            End Select

            'rc.ReactionHeat = Me.tbReacHeat.Text
            rc.Description = Me.tbDesc.Text
            rc.Name = Me.tbName.Text
            rc.Equation = Me.tbEquation.Text
            rc.StoichBalance = 0
            rc.Approach = Me.tbApproach.Text
            rc.Tmin = Me.tbTmin.Text
            rc.Tmax = Me.tbTmax.Text
            rc.ConstantKeqValue = Me.tbKeqConstant.Text

            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc.BaseReactant = row.Cells(5).Value
                    Exit For
                End If
            Next

            'add or edit reaction
            Select Case mode
                Case "Add"
                    rc.ID = Guid.NewGuid.ToString
                    rc.ReactionType = ReactionType.Equilibrium
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

End Class