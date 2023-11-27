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
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums

Public Class FormReacKinetic

    Inherits System.Windows.Forms.Form

    Public mode As String = "Add"
    Public rc As Reaction
    Public fc As FormFlowsheet
    Public su As SystemsOfUnits.Units
    Public cv As New SystemsOfUnits.Converter
    Public nf As String
    Public loaded As Boolean = False

    Private Sub FormReacKinetic_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Me.cbBase.Enabled = True

        tbName.Focus()
        tbName.ScrollToCaret()

        su = fc.Options.SelectedUnitSystem
        nf = fc.Options.NumberFormat

        'populate datagrid
        For Each subs As ConstantProperties In fc.Options.SelectedComponents.Values
            With Me.KryptonDataGridView1
                .Rows.Add(New Object() {(subs.Name), Format(subs.Molar_Weight, nf), Format(subs.IG_Enthalpy_of_Formation_25C, nf), False, False, 0, 0, 0, subs.Name, 0})
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
            If rc.Components.ContainsKey(row.Cells(8).Value) Then
                row.Cells(3).Value = True
                row.Cells(4).Value = rc.Components(row.Cells(8).Value).IsBaseReactant
                row.Cells(5).Value = rc.Components(row.Cells(8).Value).StoichCoeff
                row.Cells(6).Value = rc.Components(row.Cells(8).Value).DirectOrder
                row.Cells(7).Value = rc.Components(row.Cells(8).Value).ReverseOrder
            End If
        Next

        Me.tbName.Text = rc.Name
        Me.tbDesc.Text = rc.Description
        Me.tbCompBase.Text = rc.BaseReactant
        Me.tbEquation.Text = rc.Equation
        Me.tbReacHeat.Text = rc.ReactionHeat.ToString(nf)
        Me.tbStoich.Text = rc.StoichBalance.ToString(nf)
        Me.tbTmin.Text = rc.Tmin
        Me.tbTmax.Text = rc.Tmax
        Me.tbFwdA.Text = rc.A_Forward.ToString(nf)
        Me.tbFwdE.Text = rc.E_Forward.ToString(nf)
        Me.tbRevA.Text = rc.A_Reverse.ToString(nf)
        Me.tbRevE.Text = rc.E_Reverse.ToString(nf)

        If rc.ReactionKinFwdType = ReactionKineticType.Arrhenius Then
            rdFwdArr.Checked = True
            rdFwdUD.Checked = False
        Else
            rdFwdArr.Checked = False
            rdFwdUD.Checked = True
        End If

        If rc.ReactionKinRevType = ReactionKineticType.Arrhenius Then
            rdRevArr.Checked = True
            rdRevUD.Checked = False
        Else
            rdRevArr.Checked = False
            rdRevUD.Checked = True
        End If

        tbFwdUE.Text = rc.ReactionKinFwdExpression
        tbRevUE.Text = rc.ReactionKinRevExpression

        tbPhase.SelectedIndex = rc.ReactionPhase

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
            Case ReactionBasis.MassFrac
                Me.cbBase.SelectedIndex = 5
            Case ReactionBasis.PartialPress
                Me.cbBase.SelectedIndex = 6
        End Select

        cbScripts.Items.Clear()
        cbScripts.Items.Add("")
        For Each script In fc.Scripts
            cbScripts.Items.Add(script.Value.Title)
        Next

        If cbScripts.Items.Contains(rc.ScriptTitle) Then cbScripts.SelectedItem = rc.ScriptTitle Else cbScripts.SelectedIndex = 0

        If rc.ReactionKinetics = ReactionKinetics.Expression Then
            rbBasicKin.Checked = True
        Else
            rbAdvKin.Checked = True
        End If

        gbKinetics.Enabled = rbBasicKin.Checked
        Label2.Enabled = rbAdvKin.Checked
        cbScripts.Enabled = rbAdvKin.Checked
        btnScriptHelp.Enabled = rbAdvKin.Checked

        Me.cbConcUnit.Items.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3", "kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
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

        cbReverseEUnits.Items.Clear()
        cbReverseEUnits.Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_enthalpy).ToArray())
        Try
            cbReverseEUnits.SelectedItem = rc.E_Reverse_Unit
        Catch ex As Exception
            cbReverseEUnits.SelectedItem = "J/mol"
        End Try

        cbForwardEUnits.Items.Clear()
        cbForwardEUnits.Items.AddRange(su.GetUnitSet(UnitOfMeasure.molar_enthalpy).ToArray())
        Try
            cbForwardEUnits.SelectedItem = rc.E_Forward_Unit
        Catch ex As Exception
            cbForwardEUnits.SelectedItem = "J/mol"
        End Try

        loaded = True

        Select Case mode
            Case "Edit"
                Call Me.KryptonDataGridView1_CellValidated(sender, New DataGridViewCellEventArgs(3, 0))
        End Select

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Private Sub KryptonDataGridView1_CellValidated(ByVal sender As Object, ByVal e As DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellValueChanged

        If e.ColumnIndex = 4 Then
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value = True Then
                    rc.BaseReactant = row.Cells(8).Value
                    Me.tbCompBase.Text = (rc.BaseReactant)
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
                If row.Cells(5).Value IsNot Nothing AndAlso row.Cells(5).Value.ToString.IsValidDouble Then
                    If row.Cells(5).Value < 0 And row.Cells(3).Value = True Then
                        If row.Cells(5).Value = -1 Then
                            eq += fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                        Else
                            If row.Cells(5).Value = 1 Then
                                eq += fc.Options.SelectedComponents(row.Cells(6).Value).Formula & " + "
                            Else
                                eq += Math.Abs(row.Cells(5).Value.ToString.ToDoubleFromCurrent) & fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                            End If
                        End If
                        hr += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                        br += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                        gr += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                    End If
                Else
                    eq += "[ERROR]" & fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                End If
            Next
            If eq.Length >= 2 Then eq = eq.Remove(eq.Length - 2, 2)
            eq += "<--> "
            'scan for products
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(5).Value IsNot Nothing AndAlso row.Cells(5).Value.ToString.IsValidDouble Then
                    If row.Cells(5).Value > 0 And row.Cells(3).Value = True Then
                        If row.Cells(5).Value = 1 Then
                            eq += fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                        Else
                            If row.Cells(5).Value = 1 Then
                                eq += fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                            Else
                                eq += Math.Abs(row.Cells(5).Value.ToString.ToDoubleFromCurrent) & fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
                            End If
                        End If
                        hp += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).IG_Enthalpy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                        bp += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                        gp += Math.Abs(Convert.ToDouble(row.Cells(5).Value)) * fc.Options.SelectedComponents(row.Cells(8).Value).IG_Gibbs_Energy_of_Formation_25C * fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight
                    End If
                Else
                    eq += "[ERROR]" & fc.Options.SelectedComponents(row.Cells(8).Value).Formula & " + "
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

            fc.RegisterSnapshot(SnapshotType.ReactionSubsystem)

            'Components, stoichiometry and reaction orders
            rc._Components.Clear()
            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(3).Value = True Then
                    rc._Components.Add(row.Cells(8).Value, New ReactionStoichBase(row.Cells(8).Value, row.Cells(5).Value, row.Cells(4).Value, row.Cells(6).Value, row.Cells(7).Value))
                End If
            Next

            rc.ReactionPhase = tbPhase.SelectedIndex

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
                    rc.ReactionBasis = ReactionBasis.MassFrac
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

            rc.E_Forward_Unit = cbForwardEUnits.SelectedItem.ToString
            rc.E_Reverse_Unit = cbReverseEUnits.SelectedItem.ToString

            rc.ReactionKinFwdExpression = tbFwdUE.Text
            rc.ReactionKinRevExpression = tbRevUE.Text

            If rdFwdArr.Checked Then
                rc.ReactionKinFwdType = ReactionKineticType.Arrhenius
            Else
                rc.ReactionKinFwdType = ReactionKineticType.UserDefined
            End If

            If rdRevArr.Checked Then
                rc.ReactionKinRevType = ReactionKineticType.Arrhenius
            Else
                rc.ReactionKinRevType = ReactionKineticType.UserDefined
            End If

            rc.ConcUnit = Me.cbConcUnit.SelectedItem.ToString
            rc.VelUnit = Me.cbVelUnit.SelectedItem

            For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
                If row.Cells(4).Value = True Then
                    rc.BaseReactant = row.Cells(8).Value
                    If rc.Components(rc.BaseReactant).StoichCoeff >= 0 Then
                        MessageBox.Show(fc.GetTranslatedString1("ReactantAsBaseComp"), fc.GetTranslatedString1("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If
                    Exit For
                End If
            Next

            If rbAdvKin.Checked Then
                rc.ReactionKinetics = ReactionKinetics.PythonScript
            Else
                rc.ReactionKinetics = ReactionKinetics.Expression
            End If

            rc.ScriptTitle = cbScripts.SelectedItem.ToString()

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

    Private Sub KryptonButton2_Click(sender As Object, e As EventArgs) Handles KryptonButton2.Click

        Dim sum As Double = 0.0
        For Each row As DataGridViewRow In Me.KryptonDataGridView1.Rows
            If row.Cells(3).Value = True AndAlso (row.Cells(5).Value Is Nothing OrElse row.Cells(5).Value.ToString.ToDoubleFromCurrent = 0.0) Then
                If tbStoich.Text.IsValidDouble Then
                    row.Cells(5).Value = (-tbStoich.Text.ToDoubleFromCurrent / fc.Options.SelectedComponents(row.Cells(8).Value).Molar_Weight).ToString(nf)
                    Exit For
                End If
            End If
        Next

    End Sub

    Private Sub rdFwdArr_CheckedChanged(sender As Object, e As EventArgs) Handles rdFwdArr.CheckedChanged

        tbFwdA.Enabled = rdFwdArr.Checked
        tbFwdE.Enabled = rdFwdArr.Checked
        tbFwdUE.Enabled = Not rdFwdArr.Checked

    End Sub

    Private Sub rdRevArr_CheckedChanged(sender As Object, e As EventArgs) Handles rdRevArr.CheckedChanged

        tbRevA.Enabled = rdRevArr.Checked
        tbRevE.Enabled = rdRevArr.Checked
        tbRevUE.Enabled = Not rdRevArr.Checked

        Column7.Visible = rdRevArr.Checked
        Column7.Visible = rdRevArr.Checked

    End Sub

    Private Sub rbBasicKin_CheckedChanged(sender As Object, e As EventArgs) Handles rbBasicKin.CheckedChanged, rbAdvKin.CheckedChanged

        gbKinetics.Enabled = rbBasicKin.Checked
        Label2.Enabled = rbAdvKin.Checked
        cbScripts.Enabled = rbAdvKin.Checked
        btnScriptHelp.Enabled = rbAdvKin.Checked

        Column7.Visible = rbBasicKin.Checked
        Column8.Visible = rbBasicKin.Checked

    End Sub

    Private Sub btnScriptHelp_Click(sender As Object, e As EventArgs) Handles btnScriptHelp.Click

        MessageBox.Show("Create a Python Script using the Script Manager which returns the reaction rate 'r' in the currently selected units. " + vbCrLf +
"You can get the compound amounts through the R1, R2, ..., Rn, P1, P2, ..., Pn and N1, N2, ... Nn variables or use the 'Amounts' dictionary to get the amount by the compound's name." + vbCrLf +
"You can also use the current temperature 'T' in Kelvin and pressure 'P' in Pa.", "Advanced Kinetics", MessageBoxButtons.OK, MessageBoxIcon.Information)

    End Sub

    Private Sub KryptonDataGridView1_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellContentClick
        If e.ColumnIndex = 3 Or e.ColumnIndex = 4 Then
            If e.ColumnIndex = 4 Then
                Dim state As Boolean = Convert.ToBoolean((CType(KryptonDataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex), DataGridViewCheckBoxCell)).EditingCellFormattedValue)
                Dim intDemarcationSetID As Integer = Convert.ToInt32((CType(KryptonDataGridView1.Rows(e.RowIndex).Cells(9), DataGridViewTextBoxCell)).EditedFormattedValue)
                If state Then
                    For Each dgvr As DataGridViewRow In KryptonDataGridView1.Rows
                        If Convert.ToInt32(dgvr.Cells(4).Value) <> intDemarcationSetID Then
                            DirectCast(dgvr.Cells(e.ColumnIndex), DataGridViewCheckBoxCell).Value = False
                        End If
                    Next
                End If
            End If
            KryptonDataGridView1.CommitEdit(DataGridViewDataErrorContexts.Commit)
        End If
    End Sub

    Private Sub KryptonDataGridView1_CellContentDoubleClick(sender As Object, e As DataGridViewCellEventArgs) Handles KryptonDataGridView1.CellContentDoubleClick
        If e.ColumnIndex = 3 Or e.ColumnIndex = 4 Then
            If e.ColumnIndex = 4 Then
                Dim state As Boolean = Convert.ToBoolean((CType(KryptonDataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex), DataGridViewCheckBoxCell)).EditingCellFormattedValue)
                Dim intDemarcationSetID As Integer = Convert.ToInt32((CType(KryptonDataGridView1.Rows(e.RowIndex).Cells(9), DataGridViewTextBoxCell)).EditedFormattedValue)
                If state Then
                    For Each dgvr As DataGridViewRow In KryptonDataGridView1.Rows
                        If Convert.ToInt32(dgvr.Cells(4).Value) <> intDemarcationSetID Then
                            DirectCast(dgvr.Cells(e.ColumnIndex), DataGridViewCheckBoxCell).Value = False
                        End If
                    Next
                End If
            End If
            KryptonDataGridView1.CommitEdit(DataGridViewDataErrorContexts.Commit)
        End If
    End Sub

End Class