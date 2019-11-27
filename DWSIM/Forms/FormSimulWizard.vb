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
Imports System.Linq
Imports DWSIM.Thermodynamics.PropertyPackages

Public Class FormSimulWizard

    Private FrmChild As FormFlowsheet
    Dim loaded As Boolean = False

    Private prevsort As System.ComponentModel.ListSortDirection = System.ComponentModel.ListSortDirection.Ascending
    Private prevcol As Integer = 1

    Public switch As Boolean = False

    Dim ACSC1 As AutoCompleteStringCollection

    Private StillTyping As Boolean = False

    Private Sub FormConfigWizard_Load(sender As Object, e As System.EventArgs) Handles Me.Load

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

            colAdd.ValueType = True.GetType()
            colAdd.FalseValue = False
            colAdd.TrueValue = True
            colAdd.IndeterminateValue = False

            ACSC1 = New AutoCompleteStringCollection

            For Each comp In Me.FrmChild.Options.SelectedComponents.Values
                ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
            Next
            For Each comp In Me.FrmChild.Options.NotSelectedComponents.Values
                ogc1.Rows.Add(New Object() {comp.Name, False, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                'For Each c As DataGridViewCell In Me.ogc1.Rows(idx).Cells
                '    If comp.OriginalDB <> "Electrolytes" Then
                '        If comp.Acentric_Factor = 0.0# Or comp.Critical_Compressibility = 0.0# Then
                '            c.Style.ForeColor = Color.Red
                '            c.ToolTipText = DWSIM.App.GetLocalString("CompMissingData")
                '        End If
                '    End If
                'Next
            Next

            'Me.TextBox1.AutoCompleteCustomSource = ACSC1

            'property packages
            Me.ListViewPP.Items.Clear()
            For Each pp2 As PropertyPackages.PropertyPackage In FormMain.PropertyPackages.Values.OrderBy(Function(x) x.ComponentName)
                Me.ListViewPP.Items.Add(pp2.ComponentName)
                'Select Case pp2.PackageType
                '    Case PropertyPackages.PackageType.EOS
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("EOS")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.ActivityCoefficient
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("ACT")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.ChaoSeader
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("CS")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.VaporPressure
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("VAP")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.Miscelaneous
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("MISC")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.CorrespondingStates
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("CST")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                '    Case PropertyPackages.PackageType.CAPEOPEN
                '        With Me.ListViewPP.Items.Add(pp2.ComponentName)
                '            .Group = Me.ListViewPP.Groups("CAP")
                '            .ToolTipText = pp2.ComponentDescription
                '        End With
                'End Select
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

    Private Sub LinkLabelPropertyMethods_LinkClicked(sender As System.Object, e As System.Windows.Forms.LinkLabelLinkClickedEventArgs) Handles LinkLabelPropertyMethods.LinkClicked
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Property_Methods_and_Correlation_Profiles")
    End Sub

    Private Sub TextBox1_KeyDown(sender As Object, e As System.Windows.Forms.KeyEventArgs) Handles TextBox1.KeyDown
        If e.KeyCode = Keys.Enter Then
            If DWSIM.App.IsRunningOnMono Then
                If Me.ogc1.SelectedCells.Count > 0 Then
                    Me.ogc1.Rows(Me.ogc1.SelectedCells(0).RowIndex).Cells(1).Value = Not Me.ogc1.Rows(Me.ogc1.SelectedCells(0).RowIndex).Cells(1).Value
                End If
            Else
                If Me.ogc1.SelectedRows.Count > 0 Then
                    Me.ogc1.SelectedRows(0).Cells(1).Value = Not Me.ogc1.SelectedRows(0).Cells(1).Value
                End If
            End If
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
            For Each r As DataGridViewRow In ogc1.Rows
                r.Selected = False
                r.Visible = True
            Next
            ogc1.FirstDisplayedScrollingRowIndex = 0
            ogc1.Sort(colAdd, System.ComponentModel.ListSortDirection.Descending)
        Else
            If ogc1.SelectedRows.Count > 0 Then
                ogc1.FirstDisplayedScrollingRowIndex = ogc1.SelectedRows(0).Index
            End If
        End If


    End Sub

    Private Sub Button7_Click(sender As System.Object, e As System.EventArgs)
        If Me.ogc1.SelectedRows.Count > 0 Then
            Me.AddCompToSimulation(Me.ogc1.SelectedRows(0).Index)
        End If
    End Sub

    Sub AddCompToSimulation(ByVal compid As String)
        ' TODO Add code to check that index is within range. If it is out of range, don't do anything.
        If Me.loaded Then
            If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(compid) Then
                Dim tmpcomp As New BaseClasses.ConstantProperties
                tmpcomp = Me.FrmChild.Options.NotSelectedComponents(compid)

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
            End If
        End If
    End Sub

    Sub RemoveCompFromSimulation(ByVal compid As String)

        Dim tmpcomp As New BaseClasses.ConstantProperties
        Dim nm As String = compid
        tmpcomp = Me.FrmChild.Options.SelectedComponents(nm)
        Me.FrmChild.Options.SelectedComponents.Remove(tmpcomp.Name)
        Me.FrmChild.Options.NotSelectedComponents.Add(tmpcomp.Name, tmpcomp)
        Dim ms As Streams.MaterialStream
        Dim proplist As New ArrayList

        For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
            For Each phase As BaseClasses.Phase In ms.Phases.Values
                phase.Compounds.Remove(tmpcomp.Name)
            Next
        Next
    End Sub

    Private Sub Button8_Click(sender As System.Object, e As System.EventArgs) Handles Button8.Click
        Dim pp As PropertyPackages.PropertyPackage
        pp = FormMain.PropertyPackages(ListViewPP.SelectedItems(0)).Clone
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

        If ComboBox2.SelectedIndex <= 2 Then
            Me.DataGridView1.Columns(1).ReadOnly = True
            Me.DataGridView1.Columns(3).ReadOnly = True
        Else
            Me.DataGridView1.Columns(1).ReadOnly = False
            Me.DataGridView1.Columns(3).ReadOnly = False
        End If

        '.Add(New Object() {DWSIM.App.GetLocalString("Temperatura")})
        With DirectCast(Me.DataGridView1.Rows.Item(0).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"K", "R", "C", "F"})
            .Style.Tag = 1
            .Value = su.temperature
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Presso")})
        With DirectCast(Me.DataGridView1.Rows.Item(0).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"Pa", "atm", "kgf/cm2", "kgf/cm2g", "lbf/ft2", "kPa", "kPag", "bar", "barg", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi", "psig"})
            .Style.Tag = 2
            .Value = su.pressure
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazomssica")})
        With DirectCast(Me.DataGridView1.Rows.Item(1).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"})
            .Style.Tag = 3
            .Value = su.massflow
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazomolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(1).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})
            .Value = su.molarflow
            .Style.Tag = 4
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Vazovolumtrica")})
        With DirectCast(Me.DataGridView1.Rows.Item(2).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m3/s", "ft3/s", "cm3/s", "m3/h", "m3/d", "bbl/h", "bbl/d", "ft3/min", "ft3/d", "gal[UK]/h", "gal[UK]/s", "gal[US]/h", "gal[US]/min", "L/h", "L/min", "L/s"})
            .Value = su.volumetricFlow
            .Style.Tag = 5
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("EntalpiaEspecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(2).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kJ/kg", "cal/g", "BTU/lbm", "kcal/kg"})
            .Value = su.enthalpy
            .Style.Tag = 6
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("EntropiaEspecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(3).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
            .Value = su.entropy
            .Style.Tag = 7
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Massamolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(3).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kg/kmol", "g/mol", "lbm/lbmol"})
            .Value = su.molecularWeight
            .Style.Tag = 8
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Massaespecfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(4).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kg/m3", "g/cm3", "lbm/ft3"})
            .Value = su.density
            .Style.Tag = 10
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Tensosuperficial")})
        With DirectCast(Me.DataGridView1.Rows.Item(4).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"N/m", "dyn/cm", "lbf/in"})
            .Value = su.surfaceTension
            .Style.Tag = 9
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("CapacidadeCalorfica")})
        With DirectCast(Me.DataGridView1.Rows.Item(5).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kJ/[kg.K]", "cal/[g.C]", "BTU/[lbm.R]"})
            .Value = su.heatCapacityCp
            .Style.Tag = 11
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Condutividadetrmica")})
        With DirectCast(Me.DataGridView1.Rows.Item(5).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"W/[m.K]", "cal/[cm.s.C]", "BTU/[ft.h.R]"})
            .Value = su.thermalConductivity
            .Style.Tag = 12
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Viscosidadecinemtica")})
        With DirectCast(Me.DataGridView1.Rows.Item(6).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m2/s", "cSt", "ft2/s", "mm2/s"})
            .Value = su.cinematic_viscosity
            .Style.Tag = 13
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ViscosidadeDinmica1")})
        With DirectCast(Me.DataGridView1.Rows.Item(6).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kg/[m.s]", "Pa.s", "cP", "lbm/[ft.h]"})
            .Value = su.viscosity
            .Style.Tag = 14
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DeltaP")})
        With DirectCast(Me.DataGridView1.Rows.Item(7).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"Pa", "atm", "lbf/ft2", "kgf/cm2", "kPa", "bar", "ftH2O", "inH2O", "inHg", "mbar", "mH2O", "mmH2O", "mmHg", "MPa", "psi"})
            .Value = su.deltaP
            .Style.Tag = 15
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DeltaT2")})
        With DirectCast(Me.DataGridView1.Rows.Item(7).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"C.", "K.", "F.", "R."})
            .Value = su.deltaT
            .Style.Tag = 16
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ComprimentoHead")})
        With DirectCast(Me.DataGridView1.Rows.Item(8).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m", "ft", "cm"})
            .Value = su.head
            .Style.Tag = 17
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FluxodeEnergyFlow")})
        With DirectCast(Me.DataGridView1.Rows.Item(8).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"})
            .Value = su.heatflow
            .Style.Tag = 18
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Tempo")})
        With DirectCast(Me.DataGridView1.Rows.Item(9).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"s", "min.", "h"})
            .Value = su.time
            .Style.Tag = 19
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Volume")})
        With DirectCast(Me.DataGridView1.Rows.Item(9).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m3", "cm3", "L", "ft3"})
            .Value = su.volume
            .Style.Tag = 20
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("VolumeMolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(10).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {DWSIM.App.GetLocalString("m3kmol"), "cm3/mmol", "ft3/lbmol"})
            .Value = su.molar_volume
            .Style.Tag = 21
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("rea")})
        With DirectCast(Me.DataGridView1.Rows.Item(10).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m2", "cm2", "ft2"})
            .Value = su.area
            .Style.Tag = 22
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("DimetroEspessura")})
        With DirectCast(Me.DataGridView1.Rows.Item(11).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m", "mm", "cm", "in", "ft"})
            .Value = su.diameter
            .Style.Tag = 23
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Fora")})
        With DirectCast(Me.DataGridView1.Rows.Item(11).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {DWSIM.App.GetLocalString("N"), "dyn", "kgf", "lbf"})
            .Value = su.force
            .Style.Tag = 24
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("CoefdeTransfdeCalor")})
        With DirectCast(Me.DataGridView1.Rows.Item(12).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"W/[m2.K]", "cal/[cm2.s.C]", "BTU/[ft2.h.R]"})
            .Value = su.heat_transf_coeff
            .Style.Tag = 25
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Aceleracao")})
        With DirectCast(Me.DataGridView1.Rows.Item(12).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m/s2", "cm/s2", "ft/s2"})
            .Value = su.accel
            .Style.Tag = 26
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ConcentraoMolar")})
        With DirectCast(Me.DataGridView1.Rows.Item(13).Cells(1), DataGridViewComboBoxCell)
            .Items.AddRange(New String() {"kmol/m3", "mol/m3", "mol/L", "mol/cm3", "mol/mL", "lbmol/ft3"})
            .Value = su.molar_conc
            .Style.Tag = 28
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("ConcentraoMssica")})
        With DirectCast(Me.DataGridView1.Rows.Item(13).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kg/m3", "g/L", "g/cm3", "g/mL", "lbm/ft3"})
            .Value = su.mass_conc
            .Style.Tag = 29
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("TaxadeReao")})
        With DirectCast(Me.DataGridView1.Rows.Item(14).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kmol/[m3.s]", "kmol/[m3.min.]", "kmol/[m3.h]", "mol/[m3.s]", "mol/[m3.min.]", "mol/[m3.h]", "mol/[L.s]", "mol/[L.min.]", "mol/[L.h]", "mol/[cm3.s]", "mol/[cm3.min.]", "mol/[cm3.h]", "lbmol.[ft3.h]"})
            .Value = su.reac_rate
            .Style.Tag = 30
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("VolumeEspecfico")})
        With DirectCast(Me.DataGridView1.Rows.Item(14).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m3/kg", "cm3/g", "ft3/lbm"})
            .Value = su.spec_vol
            .Style.Tag = 27
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("MolarEnthalpy")})
        With DirectCast(Me.DataGridView1.Rows.Item(15).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kJ/kmol", "cal/mol", "BTU/lbmol"})
            .Value = su.molar_enthalpy
            .Style.Tag = 31
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("MolarEntropy")})
        With DirectCast(Me.DataGridView1.Rows.Item(15).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"kJ/[kmol.K]", "cal/[mol.C]", "BTU/[lbmol.R]"})
            .Value = su.molar_entropy
            .Style.Tag = 32
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("Velocity")})
        With DirectCast(Me.DataGridView1.Rows.Item(16).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m/s", "cm/s", "mm/s", "km/h", "ft/h", "ft/min", "ft/s", "in/s"})
            .Value = su.velocity
            .Style.Tag = 33
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("HXFoulingFactor")})
        With DirectCast(Me.DataGridView1.Rows.Item(16).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"K.m2/W", "C.cm2.s/cal", "ft2.h.F/BTU"})
            .Value = su.foulingfactor
            .Style.Tag = 34
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FilterSpecificCakeResistance")})
        With DirectCast(Me.DataGridView1.Rows.Item(17).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m/kg", "ft/lbm", "cm/g"})
            .Value = su.cakeresistance
            .Style.Tag = 35
        End With

        '.Add(New Object() {DWSIM.App.GetLocalString("FilterMediumResistance")})
        With DirectCast(Me.DataGridView1.Rows.Item(17).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(New String() {"m-1", "cm-1", "ft-1"})
            .Value = su.mediumresistance
            .Style.Tag = 36
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(18).Cells(1), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.compressibility).ToArray)
            .Value = su.compressibility
            .Style.Tag = 37
        End With

        With DirectCast(Me.DataGridView1.Rows.Item(18).Cells(3), DataGridViewComboBoxCell)
            .Items.Clear()
            .Items.AddRange(su.GetUnitSet(UnitOfMeasure.jouleThomsonCoefficient).ToArray)
            .Value = su.jouleThomsonCoefficient
            .Style.Tag = 38
        End With

    End Sub

    Private Sub DataGridView1_CellValueChanged1(ByVal sender As Object, ByVal e As System.Windows.Forms.DataGridViewCellEventArgs) Handles DataGridView1.CellValueChanged

        If loaded Then

            Dim cell As DataGridViewCell = Me.DataGridView1.Rows(e.RowIndex).Cells(e.ColumnIndex)
            Dim oldvalue As String = ""
            Dim member As String = ""

            Dim su As SystemsOfUnits.Units = FrmChild.Options.SelectedUnitSystem

            Select Case cell.Style.Tag
                Case 1
                    member = "spmp_temperature"
                    oldvalue = su.temperature
                    su.temperature = cell.Value
                Case 2
                    member = "spmp_pressure"
                    oldvalue = su.pressure
                    su.pressure = cell.Value
                Case 3
                    member = "spmp_massflow"
                    oldvalue = su.massflow
                    su.massflow = cell.Value
                Case 4
                    member = "spmp_molarflow"
                    oldvalue = su.molarflow
                    su.molarflow = cell.Value
                Case 5
                    member = "spmp_volumetricFlow"
                    oldvalue = su.volumetricFlow
                    su.volumetricFlow = cell.Value
                Case 6
                    member = "spmp_enthalpy"
                    oldvalue = su.enthalpy
                    su.enthalpy = cell.Value
                Case 7
                    member = "spmp_entropy"
                    oldvalue = su.entropy
                    su.entropy = cell.Value
                Case 8
                    member = "spmp_molecularWeight"
                    oldvalue = su.molecularWeight
                    su.molecularWeight = cell.Value
                Case 9
                    member = "tdp_surfaceTension"
                    oldvalue = su.surfaceTension
                    su.surfaceTension = cell.Value
                Case 10
                    member = "spmp_density"
                    oldvalue = su.density
                    su.density = cell.Value
                Case 11
                    member = "spmp_heatCapacityCp"
                    oldvalue = su.heatCapacityCp
                    su.heatCapacityCp = cell.Value
                Case 12
                    member = "spmp_thermalConductivity"
                    oldvalue = su.thermalConductivity
                    su.thermalConductivity = cell.Value
                Case 13
                    member = "spmp_cinematic_viscosity"
                    oldvalue = su.cinematic_viscosity
                    su.cinematic_viscosity = cell.Value
                Case 14
                    member = "spmp_viscosity"
                    oldvalue = su.viscosity
                    su.viscosity = cell.Value
                Case 15
                    member = "spmp_deltaP"
                    oldvalue = su.deltaP
                    su.deltaP = cell.Value
                Case 16
                    member = "spmp_deltaT"
                    oldvalue = su.deltaT
                    su.deltaT = cell.Value
                Case 17
                    member = "spmp_head"
                    oldvalue = su.head
                    su.head = cell.Value
                Case 18
                    member = "spmp_heatflow"
                    oldvalue = su.heatflow
                    su.heatflow = cell.Value
                Case 19
                    member = "time"
                    oldvalue = su.time
                    su.time = cell.Value
                Case 20
                    member = "volume"
                    oldvalue = su.volume
                    su.volume = cell.Value
                Case 21
                    member = "molar_volume"
                    oldvalue = su.molar_volume
                    su.molar_volume = cell.Value
                Case 22
                    member = "area"
                    oldvalue = su.area
                    su.area = cell.Value
                Case 23
                    member = "diameter"
                    oldvalue = su.diameter
                    su.diameter = cell.Value
                Case 24
                    member = "force"
                    oldvalue = su.force
                    su.force = cell.Value
                Case 25
                    member = "heat_transf_coeff"
                    oldvalue = su.heat_transf_coeff
                    su.heat_transf_coeff = cell.Value
                Case 26
                    member = "accel"
                    oldvalue = su.accel
                    su.accel = cell.Value
                Case 27
                    member = "spec_vol"
                    oldvalue = su.spec_vol
                    su.spec_vol = cell.Value
                Case 28
                    member = "molar_conc"
                    oldvalue = su.molar_conc
                    su.molar_conc = cell.Value
                Case 29
                    member = "mass_conc"
                    oldvalue = su.mass_conc
                    su.mass_conc = cell.Value
                Case 30
                    member = "reac_rate"
                    oldvalue = su.reac_rate
                    su.reac_rate = cell.Value
                Case 31
                    member = "molar_enthalpy"
                    oldvalue = su.molar_enthalpy
                    su.molar_enthalpy = cell.Value
                Case 32
                    member = "molar_entropy"
                    oldvalue = su.molar_entropy
                    su.molar_entropy = cell.Value
                Case 33
                    member = "velocity"
                    oldvalue = su.velocity
                    su.velocity = cell.Value
                Case 34
                    member = "foulingfactor"
                    oldvalue = su.foulingfactor
                    su.foulingfactor = cell.Value
                Case 35
                    member = "cakeresistance"
                    oldvalue = su.cakeresistance
                    su.cakeresistance = cell.Value
                Case 36
                    member = "mediumresistance"
                    oldvalue = su.mediumresistance
                    su.mediumresistance = cell.Value
                Case 37
                    member = "compressibility"
                    oldvalue = su.compressibility
                    su.compressibility = cell.Value
                Case 38
                    member = "jouleThomsonCoefficient"
                    oldvalue = su.jouleThomsonCoefficient
                    su.jouleThomsonCoefficient = cell.Value
            End Select

        End If

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
            ogc1.Rows(e.RowIndex).Cells(1).Value = Not ogc1.Rows(e.RowIndex).Cells(1).Value
        End If
    End Sub

    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click

        switch = True

        Me.Close()

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

        If ogc1.SelectedRows.Count > 0 Then
            Dim f As New FormPureComp() With {.Flowsheet = FrmChild, .Added = False, .MyCompound = Me.FrmChild.AvailableCompounds(ogc1.SelectedRows(0).Cells(0).Value)}
            f.ShowDialog(Me)
        Else
            MessageBox.Show(DWSIM.App.GetLocalString("Error! No component selected!"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        End If

    End Sub

    Private Sub LinkLabel2_LinkClicked(sender As Object, e As LinkLabelLinkClickedEventArgs) Handles LinkLabel2.LinkClicked
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Property_Package_Selection")
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs)
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
                        ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    Else
                        MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Next
        End If
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs)

        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    If Not Me.FrmChild.AvailableCompounds.ContainsKey(comp.Name) Then Me.FrmChild.AvailableCompounds.Add(comp.Name, comp)
                    Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub btnCloneUnits_Click(sender As Object, e As EventArgs) Handles btnCloneUnits.Click

        Dim newsu = New SystemsOfUnits.Units

        newsu = Newtonsoft.Json.JsonConvert.DeserializeObject(Of SharedClasses.SystemsOfUnits.Units)(Newtonsoft.Json.JsonConvert.SerializeObject(FrmChild.Options.SelectedUnitSystem))
        newsu.Name = newsu.Name + "_" + (FormMain.AvailableUnitSystems.Count + 1).ToString

        If Not My.Application.UserUnitSystems.ContainsKey(newsu.Name) Then
            My.Application.UserUnitSystems.Add(newsu.Name, newsu)
            FormMain.AvailableUnitSystems.Add(newsu.Name, newsu)
            ComboBox2.Items.Add(newsu.Name)
            ComboBox2.SelectedIndex = ComboBox2.Items.Count - 1
        End If

    End Sub

    Private Sub btnCreateNewUnits_Click(sender As Object, e As EventArgs) Handles btnCreateNewUnits.Click
        Dim frmUnit As New FormUnitGen
        frmUnit.Wizard = True
        If frmUnit.ShowDialog(Me) = DialogResult.Yes Then
            ComboBox2.Items.Add(frmUnit.SuName)
            ComboBox2.SelectedIndex = ComboBox2.Items.Count - 1
        End If
    End Sub

    Private Sub ogc1_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles ogc1.CellValueChanged

        If loaded Then

            If ((e.ColumnIndex = colAdd.Index) AndAlso (e.RowIndex <> -1)) Then

                If ogc1.Rows(e.RowIndex).Cells(e.ColumnIndex).Value = True Then
                    'add
                    AddCompToSimulation(ogc1.Rows(e.RowIndex).Cells(0).Value)
                Else
                    'remove
                    RemoveCompFromSimulation(ogc1.Rows(e.RowIndex).Cells(0).Value)
                End If

            End If

        End If

    End Sub

    Private Sub ogc1_OnCellMouseUp(ByVal sender As Object, ByVal e As DataGridViewCellMouseEventArgs) Handles ogc1.CellMouseUp

        If ((e.ColumnIndex = colAdd.Index) AndAlso (e.RowIndex <> -1)) Then
            ogc1.EndEdit()
        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click

        cmsAddComps.Show(Button4, Button4.Width, 0)

    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem1.Click
        Dim f As New FormImportCompoundOnline
        If f.ShowDialog(Me) = Windows.Forms.DialogResult.OK Then
            Try
                Dim comp = f.BaseCompound
                If Not Me.FrmChild.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    If Not Me.FrmChild.AvailableCompounds.ContainsKey(comp.Name) Then Me.FrmChild.AvailableCompounds.Add(comp.Name, comp)
                    Me.FrmChild.Options.SelectedComponents.Add(comp.Name, comp)
                    Dim ms As Streams.MaterialStream
                    Dim proplist As New ArrayList
                    For Each ms In FrmChild.Collections.FlowsheetObjectCollection.Values
                        For Each phase As BaseClasses.Phase In ms.Phases.Values
                            phase.Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                            phase.Compounds(comp.Name).ConstantProperties = comp
                        Next
                    Next
                    ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                Else
                    MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End If
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If
    End Sub

    Private Sub ToolStripMenuItem2_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem2.Click
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
                        ogc1.Rows.Add(New Object() {comp.Name, True, comp.Name, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                    Else
                        MessageBox.Show(DWSIM.App.GetLocalString("CompoundExists"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End If
                Catch ex As Exception
                    MessageBox.Show(DWSIM.App.GetLocalString("Erro") + ex.Message.ToString, "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            Next
        End If
    End Sub

    Private Sub ToolStripMenuItem3_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem3.Click

        AddPRPropPack()

        Dim frmb As New FormPCBulk
        frmb.frmwizard = Me
        frmb.ShowDialog(Me)

    End Sub

    Private Sub ToolStripMenuItem4_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem4.Click

        AddPRPropPack()

        Dim frmdc As New DCCharacterizationWizard
        frmdc.frmwizard = Me
        frmdc.ShowDialog(Me)

    End Sub

    Sub AddPRPropPack()

        If FrmChild.Options.PropertyPackages.Count = 0 Then

            Dim pp As New PropertyPackages.PengRobinsonPropertyPackage
            With pp
                pp.Tag = pp.ComponentName + " (" + (FrmChild.PropertyPackages.Count + 1).ToString() + ")"
                pp.UniqueID = "PP-" & Guid.NewGuid.ToString
                pp.Flowsheet = FrmChild
            End With
            FrmChild.Options.PropertyPackages.Add(pp.UniqueID, pp)

            Me.dgvpp.Rows.Add(New Object() {pp.UniqueID, pp.Tag, pp.ComponentName, "..."})
            Me.dgvpp.Rows(Me.dgvpp.Rows.Count - 1).Selected = True

        End If

    End Sub

    Public Function AddCompToGrid(ByVal comp As BaseClasses.ConstantProperties) As Integer

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
                r.CreateCells(ogc1, New Object() {comp.Name, True, translatedname, comp.CAS_Number, DWSIM.App.GetComponentType(comp), comp.Formula, comp.OriginalDB, comp.IsCOOLPROPSupported})
                ogc1.Rows.Add(r)
                Return ogc1.Rows.Count - 1
            Catch ex As Exception
                Console.WriteLine(ex.ToString)
                Return -1
            End Try
        Else
            Return index
        End If

    End Function

    Private Sub CriarAPartirDeEstruturaUNIFACToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CriarAPartirDeEstruturaUNIFACToolStripMenuItem.Click

        If MessageBox.Show(DWSIM.App.GetLocalString("CreateFromUNIFACWarning"), DWSIM.App.GetLocalString("Information"), MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then

            Close()

            Dim NewMDIChild As New FormCompoundCreator()
            'Set the Parent Form of the Child window.
            NewMDIChild.MdiParent = FrmChild.MdiParent
            'Display the new form.
            NewMDIChild.Text = "CompCreator" & FormMain.m_childcount
            NewMDIChild.Show()
            FormMain.m_childcount += 1

        End If

    End Sub

End Class
