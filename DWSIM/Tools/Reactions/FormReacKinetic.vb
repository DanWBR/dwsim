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
Imports DWSIM.Interfaces.Enums

Public Class FormReacKinetic

    Inherits System.Windows.Forms.Form

    Public mode As String = "Add"
    Public rc As Reaction
    Public fc As FormFlowsheet
    Public su As DWSIM.SystemsOfUnits.Units
    Public cv As New DWSIM.SystemsOfUnits.Converter
    Public nf As String
    Public loaded As Boolean = False

    Private Sub FormReacKinetic_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Me.cbBase.Enabled = True

        tbName.Focus()
        tbName.ScrollToCaret()

        fc = My.Application.ActiveSimulation
        su = fc.Options.SelectedUnitSystem
        nf = fc.Options.NumberFormat

        'populate datagrid
        For Each subs As ConstantProperties In fc.Options.SelectedComponents.Values
            With Me.KryptonDataGridView1
                .Rows.Add(New Object() {DWSIM.App.GetComponentName(subs.Name), Format(subs.Molar_Weight, nf), False, False, 0, 0, 0, subs.Name})
            End With
        Next

        Select Case mode
            Case "Add"
                rc = New Reaction
                Me.Text = DWSIM.App.GetLocalString("AdicionarNovaReacaoCinetica")
            Case "Edit"
                Me.Text = DWSIM.App.GetLocalString("EditarReacaoCinetica")
        End Select

        For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If rc.Components.ContainsKey(row.Cells(7).Value) Then
                row.Cells(2).Value = True
                row.Cells(3).Value = rc.Components(row.Cells(7).Value).IsBaseReactant
                row.Cells(4).Value = rc.Components(row.Cells(7).Value).StoichCoeff
                row.Cells(5).Value = rc.Components(row.Cells(7).Value).DirectOrder
                row.Cells(6).Value = rc.Components(row.Cells(7).Value).ReverseOrder
            End If
        Next

        Me.tbName.Text = rc.Name
        Me.tbDesc.Text = rc.Description
        Me.tbCompBase.Text = rc.BaseReactant
        Me.tbEquation.Text = rc.Equation
        Me.tbReacHeat.Text = rc.ReactionHeat
        Me.tbStoich.Text = rc.StoichBalance
        Me.tbTmin.Text = rc.Tmin
        Me.tbTmax.Text = rc.Tmax
        Me.tbFwdA.Text = rc.A_Forward
        Me.tbFwdE.Text = rc.E_Forward
        Me.tbRevA.Text = rc.A_Reverse
        Me.tbRevE.Text = rc.E_Reverse

        Select Case rc.ReactionPhase
            Case PhaseName.Vapor
                Me.tbPhase.SelectedIndex = 0
            Case PhaseName.Liquid
                Me.tbPhase.SelectedIndex = 1
            Case PhaseName.Mixture
                Me.tbPhase.SelectedIndex = 2
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

        'Me.cbConcUnit.Items.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3", "kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
        If rc.ConcUnit = "" Then
            Me.cbConcUnit.SelectedIndex = 0
        Else
            Me.cbConcUnit.SelectedItem = rc.ConcUnit
        End If
        Me.cbVelUnit.Items.AddRange(New String() {"kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol/[ft3.h]"})
        If rc.VelUnit = "" Then
            Me.cbConcUnit.SelectedIndex = 0
        Else
            Me.cbVelUnit.SelectedItem = rc.VelUnit
        End If

        loaded = True

        Select Case mode
            Case "Edit"
                Call Me.KryptonDataGridView1_CellValidated(sender, New DataGridViewCellEventArgs(3, 0))
        End Select

    End Sub

    Private Sub KryptonDataGridView1_CellValidated(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellValueChanged

        If e.ColumnIndex = 3 Then
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc.BaseReactant = row.Cells(7).Value
                    Me.tbCompBase.Text = Global.DWSIM.DWSIM.App.GetComponentName(rc.BaseReactant)
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
                        eq += fc.Options.SelectedComponents(row.Cells(7).Value).Formula & " + "
                    Else
                        eq += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) & fc.Options.SelectedComponents(row.Cells(7).Value).Formula & " + "
                    End If
                    hr += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
                    br += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
                    gr += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
                End If
            Next
            If eq.Length >= 2 Then eq = eq.Remove(eq.Length - 2, 2)
            eq += "<--> "
            'scan for products
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value > 0 And row.Cells(2).Value = True Then
                    If row.Cells(4).Value = 1 Then
                        eq += fc.Options.SelectedComponents(row.Cells(7).Value).Formula & " + "
                    Else
                        eq += Math.Abs(Convert.ToInt32(row.Cells(4).Value)) & fc.Options.SelectedComponents(row.Cells(7).Value).Formula & " + "
                    End If
                    hp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
                    bp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
                    gp += Math.Abs(Convert.ToDouble(row.Cells(4).Value)) * fc.Options.SelectedComponents(row.Cells(7).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(7).Value).Molar_Weight
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
            'Components, stoichiometry and reaction orders
            rc.Components.Clear()
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(2).Value = True Then
                    rc.Components.Add(row.Cells(7).Value, New ReactionStoichBase(row.Cells(7).Value, row.Cells(4).Value, row.Cells(3).Value, row.Cells(5).Value, row.Cells(6).Value))
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
            rc.Tmin = Me.tbTmin.Text
            rc.Tmax = Me.tbTmax.Text

            rc.A_Forward = Me.tbFwdA.Text
            rc.A_Reverse = Me.tbRevA.Text
            rc.E_Forward = Me.tbFwdE.Text
            rc.E_Reverse = Me.tbRevE.Text

            rc.ConcUnit = Me.cbConcUnit.SelectedItem.ToString
            rc.VelUnit = Me.cbVelUnit.SelectedItem

            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc.BaseReactant = row.Cells(7).Value
                    Exit For
                End If
            Next

            'add or edit reaction
            Select Case mode
                Case "Add"
                    rc.ID = Guid.NewGuid.ToString
                    rc.ReactionType = ReactionType.Kinetic
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

    Private Sub cbBase_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbBase.SelectedIndexChanged

        Me.cbConcUnit.Items.Clear()

        Select Case cbBase.SelectedIndex
            Case 0, 4, 5
                'atividade, frac molar, frac massica
                Me.cbConcUnit.Items.AddRange(New String() {""})
            Case 1, 6
                'fugacidade, press parcial
                Me.cbConcUnit.Items.AddRange(New Object() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
            Case 2
                'conc molar
                Me.cbConcUnit.Items.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"})
            Case 3
                'conc massica
                Me.cbConcUnit.Items.AddRange(New String() {"kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
            Case 4
        End Select

        Me.cbConcUnit.SelectedIndex = 0

    End Sub

End Class