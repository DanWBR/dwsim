'    Copyright 2014 Daniel Wagner O. de Medeiros
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
Imports System.IO
Imports DWSIM.FlowsheetSolver
Imports DWSIM.Thermodynamics.PropertyPackages

Public Class FormSimulWizard

    Private FrmChild As FormFlowsheet
    Dim loaded As Boolean = False

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Public switch As Boolean = False

    Dim ACSC1 As AutoCompleteStringCollection

    Private Sub FormConfigWizard_Load(sender As Object, e As System.EventArgs) Handles Me.Load

        Me.ListViewPP.View = View.List

        Me.StepWizardControl1.FinishButtonText = DWSIM.App.GetLocalString("FinishText")
        Me.StepWizardControl1.CancelButtonText = DWSIM.App.GetLocalString("CancelText")
        Me.StepWizardControl1.NextButtonText = DWSIM.App.GetLocalString("NextText") & " >"

        Init()

    End Sub

    Sub Init(Optional ByVal reset As Boolean = False)

        Dim pathsep As Char = Path.DirectorySeparatorChar

        FrmChild = My.Application.ActiveSimulation

        Dim comp As BaseClasses.ConstantProperties
        If Not loaded Or reset Then

            ACSC1 = New AutoCompleteStringCollection

            Me.ListViewA.Items.Clear()
            For Each comp In Me.FrmChild.Options.SelectedComponents.Values
                Me.ListViewA.Items.Add(comp.Name, comp.Name, 0).Tag = comp.Name
            Next
            For Each comp In Me.FrmChild.Options.NotSelectedComponents.Values
                Dim idx As Integer = Me.AddCompToGrid(comp)
                If Not idx = -1 Then
                    For Each c As DataGridViewCell In Me.ogc1.Rows(idx).Cells
                        If comp.OriginalDB <> "Electrolytes" Then
                            If comp.Acentric_Factor = 0.0# Or comp.Critical_Compressibility = 0.0# Then
                                c.Style.ForeColor = Color.Red
                                c.ToolTipText = DWSIM.App.GetLocalString("CompMissingData")
                            End If
                        End If
                    Next
                End If
            Next

            'Me.TextBox1.AutoCompleteCustomSource = ACSC1

            'property packages
            Me.ListViewPP.Items.Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FormMain.PropertyPackages.Values
                Select Case pp2.PackageType
                    Case PropertyPackages.PackageType.EOS
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("EOS")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.ActivityCoefficient
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("ACT")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.ChaoSeader
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("CS")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.VaporPressure
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("VAP")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.Miscelaneous
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("MISC")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.CorrespondingStates
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("CST")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                    Case PropertyPackages.PackageType.CAPEOPEN
                        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                            .Group = Me.ListViewPP.Groups("CAP")
                            .ToolTipText = pp2.ComponentDescription
                        End With
                End Select
            Next

        Else

            For Each r As DataGridViewRow In ogc1.Rows
                If FrmChild.Options.NotSelectedComponents.ContainsKey(r.Cells(0).Value) Then
                    comp = FrmChild.Options.NotSelectedComponents(r.Cells(0).Value)
                    For Each c As DataGridViewCell In r.Cells
                        If comp.Acentric_Factor = 0.0# Or comp.Critical_Compressibility = 0.0# Then
                            c.Style.ForeColor = Color.Red
                            c.ToolTipText = DWSIM.App.GetLocalString("CompMissingData")
                        End If
                    Next
                End If
            Next

        End If

        With Me.dgvpp.Rows
            .Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FrmChild.Options.PropertyPackages.Values
                .Add(New Object() {pp2.UniqueID, pp2.Tag, pp2.ComponentName})
            Next
        End With

        Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
        FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
        Me.ComboBox2.Items.Clear()
        Me.ComboBox2.Items.AddRange(array1)

        ComboBox2.SelectedIndex = 0

        Me.loaded = True

    End Sub

    Public Function AddCompToGrid(ByRef comp As BaseClasses.ConstantProperties) As Integer

        'If Not initialized Then
        '    Me.Visible = False
        '    Me.Show()
        '    Me.Visible = False
        'End If

        Dim contains As Boolean = False
        Dim index As Integer = -1
        For Each r As DataGridViewRow In ogc1.Rows
            If r.Cells(0).Value = comp.Name Then
                contains = True
                index = r.Index
            End If
        Next

        Dim translatedname As String = ""

        If Not contains Then
            Try
                Dim r As New DataGridViewRow
                translatedname = comp.Name
                r.CreateCells(ogc1, New Object() {comp.Name, translatedname, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                ogc1.Rows.Add(r)
                Return ogc1.Rows.Count - 1
            Catch ex As Exception
                Console.WriteLine(ex.ToString)
                Return -1
            Finally
                ACSC1.Add(translatedname)
                ACSC1.Add(comp.CAS_Number)
                ACSC1.Add(comp.Formula)
                'Me.TextBox1.AutoCompleteCustomSource = ACSC1
            End Try
        Else
            Return index
        End If

    End Function

    Private Sub LinkLabelPropertyMethods_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabelPropertyMethods.LinkClicked
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Property_Methods_and_Correlation_Profiles")
    End Sub

    Private Sub LinkLabel1_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs)
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Excel_Add-In_for_Thermodynamic_Calculations#Flash_Algorithms_and_Results_Validation")
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.Enter Then
            Call Button7_Click(sender, e)
            Me.TextBox1.Text = ""
        End If
    End Sub

    Private Sub TextBox1_TextChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextBox1.TextChanged

        ogc1.ClearSelection()

        Dim needselecting As Boolean = True

        For Each r As DataGridViewRow In ogc1.Rows
            If Not r.Cells(1).Value Is Nothing Then
                If r.Cells(1).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Or
                   r.Cells(2).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Or
                   r.Cells(4).Value.ToString.ToLower.Contains(Me.TextBox1.Text.ToLower) Then
                    r.Visible = True
                    If r.Cells(1).Value.ToString.ToLower.Equals(Me.TextBox1.Text.ToLower) Or
                                       r.Cells(2).Value.ToString.ToLower.Equals(Me.TextBox1.Text.ToLower) Or
                                       r.Cells(4).Value.ToString.ToLower.Equals(Me.TextBox1.Text.ToLower) Then
                        r.Selected = True
                        needselecting = False
                    End If
                Else
                    r.Visible = False
                End If
            End If
        Next
        If ogc1.Rows.GetFirstRow(DataGridViewElementStates.Visible) >= 0 And needselecting Then
            ogc1.Rows(ogc1.Rows.GetFirstRow(DataGridViewElementStates.Visible)).Selected = True
        End If
        If TextBox1.Text = "" Then
            ogc1.FirstDisplayedScrollingRowIndex = 0
            For Each r As DataGridViewRow In ogc1.Rows
                r.Selected = False
                r.Visible = True
            Next
        End If
    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs) Handles Button7.Click
        If Me.ogc1.SelectedRows.Count > 0 Then
            Me.AddCompToSimulation(Me.ogc1.SelectedRows(0).Index)
        End If
    End Sub

    Sub AddCompToSimulation(ByVal index As Integer)
        ' TODO Add code to check that index is within range. If it is out of range, don't do anything.
        If Me.loaded Then
            If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(ogc1.Rows(index).Cells(0).Value) Then
                Dim tmpcomp As New BaseClasses.ConstantProperties
                tmpcomp = Me.FrmChild.Options.NotSelectedComponents(ogc1.Rows(index).Cells(0).Value)

                Me.FrmChild.Options.SelectedComponents.Add(tmpcomp.Name, tmpcomp)
                Me.FrmChild.Options.NotSelectedComponents.Remove(tmpcomp.Name)
                Dim ms As Streams.MaterialStream

                Dim proplist As New ArrayList
                For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                    For Each phase As BaseClasses.Phase In ms.Phases.Values
                        phase.Compounds.Add(tmpcomp.Name, New BaseClasses.Compound(tmpcomp.Name, ""))
                        phase.Compounds(tmpcomp.Name).ConstantProperties = tmpcomp
                    Next
                Next

                Me.ListViewA.Items.Add(tmpcomp.Name, tmpcomp.Name & " (" & tmpcomp.OriginalDB & ")", 0).Tag = tmpcomp.Name
                Me.ogc1.Rows.RemoveAt(index)
            End If
        End If
        UpdateKeyCompounds()
    End Sub

    Private Sub Button10_Click(sender As System.Object, e As System.EventArgs) Handles Button10.Click
        If Me.ListViewA.SelectedItems.Count > 0 Then
            For Each lvi As ListViewItem In Me.ListViewA.SelectedItems
                Me.RemoveCompFromSimulation(lvi.Tag)
            Next
        End If
    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        Dim tmpcomp As New BaseClasses.ConstantProperties
        Dim nm As String = compid
        tmpcomp = Me.FrmChild.Options.SelectedComponents(nm)
        Me.FrmChild.Options.SelectedComponents.Remove(tmpcomp.Name)
        Me.ListViewA.Items.RemoveByKey(tmpcomp.Name)
        Me.FrmChild.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
        Me.AddCompToGrid(tmpcomp)
        Dim ms As Streams.MaterialStream
        Dim proplist As New ArrayList

        For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
            For Each phase As BaseClasses.Phase In ms.Phases.Values
                phase.Compounds.Remove(tmpcomp.Name)
            Next
        Next
        UpdateKeyCompounds()
    End Sub

    Private Sub Button11_Click(sender As System.Object, e As System.EventArgs) Handles Button11.Click
        For Each lvi As ListViewItem In Me.ListViewA.Items
            Me.RemoveCompFromSimulation(lvi.Tag)
        Next
    End Sub

    Private Sub Button6_Click(sender As System.Object, e As System.EventArgs)

    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        Dim pp As PropertyPackages.PropertyPackage
        pp = FormMain.PropertyPackages(ListViewPP.SelectedItems(0).Text).Clone
        With pp
            pp.Tag = pp.ComponentName + " (" + (FrmChild.PropertyPackages.Count + 1).ToString() + ")"
            pp.UniqueID = "PP-" & Guid.NewGuid.ToString
            pp.Flowsheet = FrmChild
        End With
        FrmChild.Options.PropertyPackages.Add(pp.UniqueID, pp)
        Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName, "..."})
        Me.dgvpp.Rows(Me.dgvpp.Rows.Count - 1).Selected = True
        If TypeOf pp Is PropertyPackages.CAPEOPENPropertyPackage Then
            pp.DisplayEditingForm()
        End If
    End Sub

    Private Sub ComboBox2_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ComboBox2.SelectedIndexChanged

        FrmChild.Options.SelectedUnitSystem = FormMain.AvailableUnitSystems.Item(ComboBox2.SelectedItem.ToString)
        Dim su As SystemsOfUnits.Units = FrmChild.Options.SelectedUnitSystem

        With Me.DataGridView1.Rows
            .Clear()
            .Add(New Object() {DWSIM.App.GetLocalString("Temperatura"), su.temperature, DWSIM.App.GetLocalString("Presso"), su.pressure})
            .Add(New Object() {DWSIM.App.GetLocalString("Vazomssica"), su.massflow, DWSIM.App.GetLocalString("Vazomolar"), su.molarflow})
            .Add(New Object() {DWSIM.App.GetLocalString("Vazovolumtrica"), su.volumetricFlow, DWSIM.App.GetLocalString("EntalpiaEspecfica"), su.enthalpy})
            .Add(New Object() {DWSIM.App.GetLocalString("EntropiaEspecfica"), su.entropy, DWSIM.App.GetLocalString("Massamolar"), su.molecularWeight})
            .Add(New Object() {DWSIM.App.GetLocalString("Massaespecfica"), su.density, DWSIM.App.GetLocalString("Tensosuperficial"), su.surfaceTension})
            .Add(New Object() {DWSIM.App.GetLocalString("CapacidadeCalorfica"), su.heatCapacityCp, DWSIM.App.GetLocalString("Condutividadetrmica"), su.thermalConductivity})
            .Add(New Object() {DWSIM.App.GetLocalString("Viscosidadecinemtica"), su.cinematic_viscosity, DWSIM.App.GetLocalString("Viscosidadedinmica"), su.viscosity})
            .Add(New Object() {DWSIM.App.GetLocalString("DeltaT2"), su.deltaT, DWSIM.App.GetLocalString("DeltaP"), su.deltaP})
            .Add(New Object() {DWSIM.App.GetLocalString("ComprimentoHead"), su.head, DWSIM.App.GetLocalString("FluxodeEnergyFlow"), su.heatflow})
            .Add(New Object() {DWSIM.App.GetLocalString("Tempo"), su.time, DWSIM.App.GetLocalString("Volume"), su.volume})
            .Add(New Object() {DWSIM.App.GetLocalString("VolumeMolar"), su.molar_volume, DWSIM.App.GetLocalString("rea"), su.area})
            .Add(New Object() {DWSIM.App.GetLocalString("DimetroEspessura"), su.diameter, DWSIM.App.GetLocalString("Fora"), su.force})
            .Add(New Object() {DWSIM.App.GetLocalString("Aceleracao"), su.accel, DWSIM.App.GetLocalString("CoefdeTransfdeCalor"), su.heat_transf_coeff})
            .Add(New Object() {DWSIM.App.GetLocalString("ConcMolar"), su.molar_conc, DWSIM.App.GetLocalString("ConcMssica"), su.mass_conc})
            .Add(New Object() {DWSIM.App.GetLocalString("TaxadeReao"), su.reac_rate, DWSIM.App.GetLocalString("VolEspecfico"), su.spec_vol})
            .Add(New Object() {DWSIM.App.GetLocalString("MolarEnthalpy"), su.molar_enthalpy, DWSIM.App.GetLocalString("MolarEntropy"), su.molar_entropy})
            .Add(New Object() {DWSIM.App.GetLocalString("Velocity"), su.velocity, DWSIM.App.GetLocalString("HXFoulingFactor"), su.foulingfactor})
            .Add(New Object() {DWSIM.App.GetLocalString("FilterSpecificCakeResistance"), su.cakeresistance, DWSIM.App.GetLocalString("FilterMediumResistance"), su.mediumresistance})
            .Add(New String() {DWSIM.App.GetLocalString("IsothermalCompressibility"), su.compressibility, DWSIM.App.GetLocalString("JouleThomsonCoefficient"), su.jouleThomsonCoefficient})
        End With

    End Sub

    Private Sub ListViewPP_DoubleClick(sender As Object, e As EventArgs) Handles ListViewPP.DoubleClick
        If ListViewPP.SelectedItems.Count = 1 Then
            Button8.PerformClick()
        End If
    End Sub

    Private Sub ListViewPP_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ListViewPP.SelectedIndexChanged
        If Me.ListViewPP.SelectedItems.Count > 0 Then
            Me.Button8.Enabled = True
        Else
            Me.Button8.Enabled = False
        End If
    End Sub

    Private Sub DataGridView1_DataError(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewDataErrorEventArgs) Handles DataGridView1.DataError

    End Sub

    Private Sub ogc1_CellDoubleClick(sender As Object, e As System.Windows.Forms.DataGridViewCellEventArgs) Handles ogc1.CellDoubleClick
        If e.RowIndex > -1 Then
            AddCompToSimulation(e.RowIndex)
        End If
    End Sub

    Sub UpdateKeyCompounds()

        Try
            Dim i As Integer = 0
            Dim sel As New ArrayList
            Dim lvi2 As ListViewItem
            For Each lvi2 In Me.ListViewA.Items
                If Not lvi2 Is Nothing Then sel.Add(lvi2.Tag)
            Next
        Catch ex As Exception

        End Try

    End Sub

    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click

        switch = True

        Me.Close()

    End Sub

    Private Sub WizardPage2_Commit(sender As System.Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage2.Commit
        If ListViewA.Items.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("Adicionesubstnciassi"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            e.Cancel = True
        End If
    End Sub

    Private Sub WizardPage3_Commit(sender As System.Object, e As AeroWizard.WizardPageConfirmEventArgs) Handles WizardPage3.Commit
        If dgvpp.Rows.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("NoexistemPacotesdePr"), DWSIM.App.GetLocalString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            e.Cancel = True
        End If
    End Sub

    Private Sub dgvpp_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles dgvpp.CellContentClick
        If e.ColumnIndex = 3 Then
            Dim ppid As String = ""
            ppid = dgvpp.SelectedRows(0).Cells(0).Value
            Dim pp As PropertyPackages.PropertyPackage = FrmChild.Options.PropertyPackages(ppid)
            If pp.IsConfigurable Then
                pp.DisplayEditingForm()
            Else
                MessageBox.Show(DWSIM.App.GetLocalString("NonConfigurablePP"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            End If
        End If
    End Sub

    Private Sub btnInfoLeft_Click(sender As Object, e As EventArgs) Handles btnInfoLeft.Click
        Dim f As New FormPureComp() With {.Flowsheet = FrmChild, .Added = False, .MyCompound = Me.FrmChild.Options.NotSelectedComponents(ogc1.SelectedRows(0).Cells(0).Value)}
        f.ShowDialog(Me)
    End Sub

    Private Sub btnInfoRight_Click(sender As Object, e As EventArgs) Handles btnInfoRight.Click
        Dim f As New FormPureComp() With {.Flowsheet = FrmChild, .Added = True, .MyCompound = Me.FrmChild.Options.SelectedComponents(Me.ListViewA.SelectedItems(0).Tag)}
        f.ShowDialog(Me)
    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Property_Package_Selection")
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        If Me.OpenFileDialog1.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            For Each fn In Me.OpenFileDialog1.FileNames
                Try
                    Dim comp = Newtonsoft.Json.JsonConvert.DeserializeObject(Of BaseClasses.ConstantProperties)(File.ReadAllText(fn))
                    If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                        Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                        Dim ms As Streams.MaterialStream
                        Dim proplist As New ArrayList
                        For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                            For Each phase As BaseClasses.Phase In ms.Phases.Values
                                phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                                phase.Compounds(comp.Name).ConstantProperties = comp
                            Next
                        Next
                        Me.ListViewA.Items.Add(comp.Name, comp.Name & " (" & comp.OriginalDB & ")", 0).Tag = comp.Name
                    Else
                        MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Next
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    Me.ListViewA.Items.Add(comp.Name, comp.Name & " (" & comp.OriginalDB & ")", 0).Tag = comp.Name
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim f As New FormImportCompoundChEDLThermo
        If f.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    Me.ListViewA.Items.Add(comp.Name, comp.Name & " (" & comp.OriginalDB & ")", 0).Tag = comp.Name
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub
End Class
